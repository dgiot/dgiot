-module(dgiot_device_iotdb).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_device/include/dgiot_device.hrl").

-export([get_device/3, get_device/4, get_device/5]).
-export([get_history_data/4, get_realtime_data/4, get_gps_track/4]).
-export([get_aggregate_functions/2, format_aggregate_results/2]).

get_device(ProductId, DevAddr, Query) ->
    case dgiot_data:get({ProductId, <<"IOTDB">>}) of
        not_find ->
            {error, <<"not find channel">>};
        Channel ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            get_device(Channel, ProductId, DeviceId, DevAddr, Query)
    end.

get_device(Channel, ProductId, DevAddr, Query) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    get_device(Channel, ProductId, DeviceId, DevAddr, Query).

get_device(Channel, ProductId, DeviceId, _DevAddr, Query) ->
    get_realtime_data(Channel, ProductId, DeviceId, Query).

get_history_data(Channel, ProductId, DeviceIds, Query) ->
    Names = get_names(ProductId, maps:get(<<"keys">>, Query, <<"*">>)),
    case query_devices(Channel, ProductId, device_ids(DeviceIds), Query) of
        {ok, Rows} ->
            Direction = case maps:get(<<"order">>, Query, <<>> ) of
                <<"-createdat">> -> desc;
                <<"-time">> -> desc;
                _ -> asc
            end,
            {Names, {ok, #{<<"results">> => merge_rows(Rows, Direction)}}};
        {error, Reason} ->
            {Names, {error, Reason}}
    end.

get_realtime_data(Channel, ProductId, DeviceIds, Query) ->
    LatestQuery = Query#{<<"order">> => <<"-createdat">>, <<"limit">> => 1},
    case query_devices(Channel, ProductId, device_ids(DeviceIds), LatestQuery) of
        {ok, Rows} ->
            {ok, #{<<"results">> => merge_rows(Rows, desc)}};
        {error, Reason} ->
            {error, Reason}
    end.

get_gps_track(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"function">>, <<"functions">>, <<"interval">>],
        Args#{<<"keys">> => [<<"latitude">>, <<"longitude">>]}),
    {_Names, Result} = get_history_data(Channel, ProductId, DeviceId, Query),
    case Result of
        {ok, #{<<"results">> := Results}} ->
            NewResults = lists:filtermap(fun format_gps_point/1, Results),
            {ok, #{<<"results">> => NewResults}};
        {_Names, {error, _Reason} = Error} -> Error;
        Error -> Error
    end.

get_aggregate_functions(ProductId, Keys) ->
    Selected = selected_keys(ProductId, Keys),
    Functions = case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := Thing}} ->
            Properties = maps:get(<<"properties">>, Thing, []) ++ maps:get(<<"tags">>, Thing, []),
            maps:from_list([
                {Identifier, aggregate_function(Property)}
                || #{<<"identifier">> := Identifier} = Property <- Properties,
                   lists:member(Identifier, Selected),
                   dgiot_iotdb_field:get_field(Property) =/= pass
            ]);
        _ ->
            #{}
    end,
    {Selected, Functions}.

format_aggregate_results(Results, Functions) ->
    [maps:fold(fun
        (<<"createdat">>, Value, Acc) -> Acc#{<<"createdat">> => Value};
        (Identifier, Value, Acc) ->
            Function = maps:get(Identifier, Functions, <<"last">>),
            Prefix = case Function of
                <<"sum">> -> <<"sum">>;
                _ -> <<"last">>
            end,
            Key = <<Prefix/binary, "(", Identifier/binary, ")">>,
            Acc#{Key => Value}
    end, #{}, Result) || Result <- Results].

query_devices(_Channel, _ProductId, [], _Query) ->
    {ok, []};
query_devices(Channel, ProductId, [DeviceId | DeviceIds], Query) ->
    case query_device(Channel, ProductId, DeviceId, Query) of
        {ok, FirstRows} ->
            query_devices(Channel, ProductId, DeviceIds, Query, FirstRows);
        Error ->
            Error
    end.

query_devices(_Channel, _ProductId, [], _Query, Rows) ->
    {ok, Rows};
query_devices(Channel, ProductId, [DeviceId | DeviceIds], Query, Rows) ->
    case query_device(Channel, ProductId, DeviceId, Query) of
        {ok, NewRows} ->
            query_devices(Channel, ProductId, DeviceIds, Query, Rows ++ NewRows);
        Error ->
            Error
    end.

query_device(Channel, ProductId, DeviceId, Query) ->
    Path = dgiot_iotdb:get_device_path(Channel, ProductId, DeviceId),
    case dgiot_iotdb:query_object(Channel, ProductId, Path, Query) of
        {ok, #{<<"results">> := Results}} -> {ok, Results};
        {ok, Result} -> {ok, maps:get(<<"results">>, Result, [])};
        Error -> Error
    end.

device_ids(DeviceIds) when is_list(DeviceIds) -> DeviceIds;
device_ids(DeviceId) -> [DeviceId].

selected_keys(ProductId, <<"*">>) ->
    dgiot_product:get_keys(ProductId);
selected_keys(_ProductId, Keys) when is_list(Keys) -> Keys;
selected_keys(_ProductId, Keys) when is_binary(Keys) ->
    re:split(Keys, <<",">>, [trim]);
selected_keys(_ProductId, _) -> [].

aggregate_function(#{<<"isaccumulate">> := Value}) when Value =:= true; Value =:= 1 -> <<"sum">>;
aggregate_function(_) -> <<"last">>.

get_names(ProductId, Keys) ->
    [case dgiot_product:get_product_identifier(ProductId, Key) of
        #{<<"name">> := Name} -> Name;
        _ -> Key
    end || Key <- selected_keys(ProductId, Keys)].

merge_rows(Rows, Direction) ->
    ByTime = lists:foldl(fun
        (#{<<"createdat">> := Time} = Row, Acc) ->
            Acc#{Time => maps:merge(maps:get(Time, Acc, #{}), Row)};
        (Row, Acc) ->
            Acc#{make_ref() => Row}
    end, #{}, Rows),
    Values = maps:values(ByTime),
    Sort = case Direction of
        desc -> fun(A, B) -> row_time(A) > row_time(B) end;
        _ -> fun(A, B) -> row_time(A) < row_time(B) end
    end,
    lists:sort(Sort, Values).

row_time(#{<<"createdat">> := Time}) -> Time;
row_time(_) -> 0.

format_gps_point(#{<<"latitude">> := Latitude, <<"longitude">> := Longitude} = Row)
        when Latitude =/= null, Longitude =/= null ->
    MapType = dgiot_utils:to_binary(application:get_env(dgiot_device, map_type, "baidu")),
    case dgiot_gps:fromwgs84(#{<<"longitude">> => Longitude, <<"latitude">> => Latitude}, MapType) of
        #{<<"longitude">> := MapLongitude, <<"latitude">> := MapLatitude} ->
            {true, Row#{<<"lat">> => MapLatitude, <<"lng">> => MapLongitude}};
        _ -> false
    end;
format_gps_point(_) ->
    false.
