-module(dgiot_iotdb_dump).

-export([export/2, import/2]).

-define(PAGE_SIZE, 100000).
-define(DEVICE_PAGE_SIZE, 1000).

export(ChannelId, #{<<"deviceid">> := DeviceId} = Body) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            case dgiot_data:get({ProductId, <<"IOTDB">>}) of
                ChannelId -> export_device(ChannelId, ProductId, DeviceId, Body, []);
                _ -> {error, {product_not_bound_to_iotdb, ProductId}}
            end;
        _ -> {ok, []}
    end;
export(ChannelId, #{<<"sessionToken">> := SessionToken}) ->
    export_session_devices(ChannelId, SessionToken, 0, []).

export_session_devices(ChannelId, SessionToken, Skip, Acc) ->
    Query = #{<<"keys">> => [<<"objectId">>, <<"product">>],
        <<"limit">> => ?DEVICE_PAGE_SIZE, <<"skip">> => Skip},
    case dgiot_parsex:query_object(<<"Device">>, Query,
            [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Devices}} ->
            case export_devices(ChannelId, Devices, Acc) of
                {ok, NewAcc} when length(Devices) =:= ?DEVICE_PAGE_SIZE ->
                    export_session_devices(ChannelId, SessionToken, Skip + length(Devices), NewAcc);
                Result -> Result
            end;
        {error, Reason} -> {error, Reason};
        Other -> {error, {invalid_device_page, Other}}
    end.

export_devices(_ChannelId, [], Acc) ->
    {ok, Acc};
export_devices(ChannelId, [#{<<"objectId">> := DeviceId,
        <<"product">> := #{<<"objectId">> := ProductId}} | Rest], Acc) ->
    case dgiot_data:get({ProductId, <<"IOTDB">>}) of
        ChannelId ->
            case export_device(ChannelId, ProductId, DeviceId, #{}, Acc) of
                {ok, NewAcc} -> export_devices(ChannelId, Rest, NewAcc);
                Error -> Error
            end;
        _ -> export_devices(ChannelId, Rest, Acc)
    end;
export_devices(ChannelId, [_ | Rest], Acc) ->
    export_devices(ChannelId, Rest, Acc).

export_device(ChannelId, ProductId, DeviceId, Query, Acc) ->
    PageSize = page_size(maps:get(<<"limit">>, Query, ?PAGE_SIZE)),
    PageQuery = maps:remove(<<"skip">>, Query),
    case export_device_rows(ChannelId, ProductId, DeviceId, PageQuery, PageSize, []) of
        {ok, []} -> {ok, Acc};
        {ok, Rows} ->
            Name = <<ProductId/binary, "/", DeviceId/binary, ".json">>,
            Json = dgiot_json:encode(#{<<"results">> => Rows}),
            {ok, Acc ++ [{binary_to_list(Name), unicode:characters_to_binary(Json)}]};
        Error -> Error
    end.

export_device_rows(ChannelId, ProductId, DeviceId, Query, PageSize, AccRev) ->
    PageQuery = Query#{<<"limit">> => PageSize, <<"order">> => <<"createdat">>},
    case dgiot_device_iotdb:get_history_data(ChannelId, ProductId, DeviceId, PageQuery) of
        {_Names, {ok, #{<<"results">> := Rows}}} when length(Rows) < PageSize ->
            {ok, lists:reverse(lists:reverse(Rows, AccRev))};
        {_Names, {ok, #{<<"results">> := Rows}}} when length(Rows) =:= PageSize ->
            continue_export_page(ChannelId, ProductId, DeviceId, Query, PageSize, Rows, AccRev);
        {_Names, {ok, _}} -> {ok, lists:reverse(AccRev)};
        {_Names, {error, Reason}} -> {error, Reason};
        Other -> {error, {invalid_query_response, Other}}
    end.

continue_export_page(ChannelId, ProductId, DeviceId, Query, PageSize, Rows, AccRev) ->
    case lists:last(Rows) of
        #{<<"createdat">> := LastTime} ->
            NextStart = dgiot_iotdb_field:timestamp_ms(LastTime) + 1,
            NewQuery = next_page_query(Query, NextStart),
            export_device_rows(ChannelId, ProductId, DeviceId, NewQuery, PageSize,
                lists:reverse(Rows, AccRev));
        _ ->
            {error, missing_timestamp_for_export_pagination}
    end.

next_page_query(Query, StartTime) ->
    Query0 = maps:remove(<<"skip">>, Query),
    Query1 = Query0#{<<"starttime">> => StartTime},
    case decode_where(maps:get(<<"where">>, Query0, undefined)) of
        Where when is_map(Where) ->
            TimeWhere = maps:get(<<"createdat">>, Where, maps:get(<<"time">>, Where, #{})),
            Query2 = preserve_time_upper_bound(Query1, TimeWhere),
            Query2#{<<"where">> => maps:without([<<"createdat">>, <<"time">>], Where)};
        _ -> Query1
    end.

preserve_time_upper_bound(Query, Operators) when is_map(Operators) ->
    EndValue = case maps:find(<<"$lte">>, Operators) of
        {ok, Value} -> Value;
        error ->
            case maps:find(<<"$lt">>, Operators) of
                {ok, Value} -> dgiot_iotdb_field:timestamp_ms(Value) - 1;
                error -> undefined
            end
    end,
    case EndValue of
        undefined -> Query;
        _ ->
            EndTime = dgiot_iotdb_field:timestamp_ms(EndValue),
            ExistingEnd = maps:get(<<"endtime">>, Query, EndTime),
            Query#{<<"endtime">> => min(dgiot_iotdb_field:timestamp_ms(ExistingEnd), EndTime)}
    end;
preserve_time_upper_bound(Query, _Operators) ->
    Query.

decode_where(Where) when is_binary(Where) ->
    try dgiot_json:decode(Where, [return_maps]) of
        Decoded when is_map(Decoded) -> Decoded;
        _ -> undefined
    catch _:_ -> undefined end;
decode_where(Where) ->
    Where.

page_size(Limit) when is_integer(Limit), Limit > 0 -> min(Limit, ?PAGE_SIZE);
page_size(_) -> ?PAGE_SIZE.

import(ChannelId, Files) ->
    import_files(ChannelId, Files).

import_files(_ChannelId, []) ->
    ok;
import_files(ChannelId, [{Name, Contents} | Rest]) ->
    case {decode_file(Contents), file_identity(Name)} of
        {{ok, #{<<"results">> := Rows}}, {ok, ProductId, DeviceId}} when is_list(Rows) ->
            case import_device(ChannelId, ProductId, DeviceId, Rows) of
                ok -> import_files(ChannelId, Rest);
                Error -> Error
            end;
        _ -> {error, {invalid_backup_entry, Name}}
    end.

import_device(ChannelId, ProductId, DeviceId, Rows) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            case dgiot_data:get({ProductId, <<"IOTDB">>}) of
                ChannelId -> import_rows(ChannelId, ProductId, DevAddr, Rows);
                _ -> {error, {product_not_bound_to_iotdb, ProductId}}
            end;
        _ -> {error, {device_not_found, DeviceId}}
    end.

import_rows(_ChannelId, _ProductId, _DevAddr, []) ->
    ok;
import_rows(ChannelId, ProductId, DevAddr, Rows) ->
    {Batch, Rest} = lists:split(min(500, length(Rows)), Rows),
    case dgiot_iotdb:batch(ChannelId, [{ProductId, DevAddr, Row} || Row <- Batch]) of
        {error, Reason} -> {error, Reason};
        _ -> import_rows(ChannelId, ProductId, DevAddr, Rest)
    end.

decode_file(Contents) ->
    try jsx:decode(Contents, [{labels, binary}, return_maps]) of
        Data when is_map(Data) -> {ok, Data}
    catch
        _:_ -> error
    end.

file_identity(Name) ->
    case binary:split(dgiot_utils:to_binary(Name), <<"/">>, [global, trim]) of
        [ProductId, Filename] ->
            case binary:split(Filename, <<".">>, [global, trim]) of
                [DeviceId, <<"json">>] -> {ok, ProductId, DeviceId};
                _ -> error
            end;
        _ -> error
    end.
