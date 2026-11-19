-module(dgiot_iotdb_handler).
-behavior(dgiot_rest).
-dgiot_rest(all).
-include("dgiot_iotdb.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([swagger_iotdb/0, handle/4]).

swagger_iotdb() ->
    [dgiot_http_server:bind(<<"/swagger_iotdb.json">>, ?MODULE, [], priv)].

handle(OperationId, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationId, Args, Context, Req) of
        {ErrType, Reason} when ErrType =:= 'EXIT'; ErrType =:= error ->
            ?LOG(info, "IoTDB request failed: ~p, ~p, ~p", [OperationId, Args, Reason]),
            Error = case is_binary(Reason) of
                true -> Reason;
                false -> list_to_binary(io_lib:format("~p", [Reason]))
            end,
            {500, Headers, #{<<"error">> => Error}};
        ok -> {200, Headers, #{}, Req};
        {ok, Result} -> {200, Headers, Result, Req};
        {Status, Result} -> {Status, Headers, Result, Req};
        {Status, NewHeaders, Result} -> {Status, maps:merge(Headers, NewHeaders), Result, Req};
        {Status, NewHeaders, Result, NewReq} -> {Status, maps:merge(Headers, NewHeaders), Result, NewReq}
    end.

do_request(get_iotdb_product_pid, #{<<"pid">> := ProductId} = Args, Context, _Req) ->
    SessionToken = maps:get(<<"sessionToken">>, Context, <<>>),
    Query0 = maps:without([<<"pid">>], Args),
    case decode_product_where(Query0) of
        {ok, Query} ->
            dgiot_product_iotdb:do_channel(ProductId, SessionToken, fun(_ChannelId) ->
                case dgiot_product_iotdb:get_product(ProductId, Query) of
                    {ok, Data} -> {ok, Data};
                    {error, Reason} -> {400, Reason}
                end
            end);
        {error, Reason} -> {400, Reason}
    end;

do_request(get_iotdb_device_deviceid, #{<<"deviceid">> := DeviceId} = Args, Context, _Req) ->
    with_device_channel(DeviceId, maps:get(<<"sessionToken">>, Context, <<>>), fun(Channel, ProductId) ->
        dgiot_product_iotdb:get_product_data(Channel, ProductId, DeviceId, Args)
    end);

do_request(get_iotdb_echart_deviceid, #{<<"deviceid">> := DeviceId, <<"style">> := Style} = Args, Context, _Req) ->
    with_device(DeviceId, maps:get(<<"sessionToken">>, Context, <<>>), fun(Channel, ProductId, Device) ->
        DevAddr = maps:get(<<"devaddr">>, Device, <<>>),
        case Style of
            <<"amis_table">> -> dgiot_device_echart:get_data_by_month(Channel, ProductId, DeviceId, Args);
            <<"echart_category">> -> dgiot_device_echart:get_data_by_echart_category(Channel, ProductId, DeviceId, Args);
            _ -> dgiot_device_echart:get_echart_data(Channel, ProductId, DeviceId, DevAddr, Args)
        end
    end);

do_request(get_iotdb_devicecard_deviceid, #{<<"deviceid">> := DeviceId} = Args, Context, _Req) ->
    case dgiot_data:get({last_data, DeviceId}) of
        not_find ->
            SessionToken = maps:get(<<"sessionToken">>, Context, <<>>),
            with_device(DeviceId, SessionToken, fun(Channel, ProductId, Device) ->
                DevAddr = maps:get(<<"devaddr">>, Device, <<>>),
                dgiot_mqtt:subscribe_route_key(
                    [<<"$dg/user/realtimecard/", DeviceId/binary, "/#">>],
                    <<"realtimecard">>, SessionToken),
                dgiot_device_card:get_devcard(Channel, ProductId, DeviceId, DevAddr, Args)
            end);
        Data -> {ok, #{<<"data">> => Data}}
    end;

do_request(get_iotdb_gps_track_deviceid, #{<<"deviceid">> := DeviceId,
        <<"starttime">> := Start, <<"endtime">> := End} = Args, Context, _Req) ->
    SessionToken = maps:get(<<"sessionToken">>, Context, <<>>),
    Query = Args#{<<"starttime">> => dgiot_iotdb_field:timestamp_ms(Start),
        <<"endtime">> => dgiot_iotdb_field:timestamp_ms(End), <<"interval">> => <<"8m">>},
    case DeviceId of
        <<"all">> -> get_all_gps_tracks(SessionToken, Query);
        _ -> with_device_channel(DeviceId, SessionToken, fun(Channel, ProductId) ->
            dgiot_device_iotdb:get_gps_track(Channel, ProductId, DeviceId, Query)
        end)
    end;

do_request(post_iotdb_save, #{<<"productid">> := ProductId, <<"devaddr">> := DevAddr,
        <<"data">> := Data}, _Context, _Req) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    ensure_device(ProductId, DeviceId, DevAddr, Data),
    case dgiot_iotdb_adapter:save(ProductId, DevAddr, Data) of
        {error, Reason} -> {400, Reason};
        _ -> {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}}
    end;

do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

with_device_channel(DeviceId, SessionToken, Fun) ->
    with_device(DeviceId, SessionToken, fun(Channel, ProductId, _Device) ->
        Fun(Channel, ProductId)
    end).

with_device(DeviceId, SessionToken, Fun) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"objectId">> := DeviceId,
                <<"product">> := #{<<"objectId">> := ProductId}} = Device} ->
            dgiot_product_iotdb:do_channel(ProductId, SessionToken, fun(Channel) ->
                Fun(Channel, ProductId, Device)
            end);
        _ -> {error, <<"not find device">>}
    end.

decode_product_where(Query) ->
    case maps:find(<<"where">>, Query) of
        error -> {ok, Query};
        {ok, Where} when is_map(Where) -> {ok, Query#{<<"where">> => Where}};
        {ok, Where} when is_binary(Where) ->
            case jsx:is_json(Where) of
                true -> {ok, Query#{<<"where">> => jsx:decode(Where, [{labels, binary}, return_maps])}};
                false -> {error, <<"where is not json">>}
            end;
        _ -> {error, <<"where is not json">>}
    end.

get_all_gps_tracks(SessionToken, Query) ->
    get_all_gps_tracks(SessionToken, Query, 0, []).

get_all_gps_tracks(SessionToken, Query, Skip, AccRev) ->
    case dgiot_parsex:query_object(<<"Device">>,
            #{<<"keys">> => [<<"objectId">>, <<"name">>, <<"product">>],
                <<"limit">> => 1000, <<"skip">> => Skip},
            [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Devices}} ->
            Tracks = lists:filtermap(fun
                (#{<<"objectId">> := DeviceId, <<"name">> := Name,
                        <<"product">> := #{<<"objectId">> := ProductId}}) ->
                    case dgiot_product_iotdb:do_channel(ProductId, SessionToken, fun(Channel) ->
                        dgiot_device_iotdb:get_gps_track(Channel, ProductId, DeviceId, Query)
                    end) of
                        {ok, #{<<"results">> := Results}} ->
                            {true, #{<<"objectId">> => DeviceId, <<"name">> => Name, <<"lineList">> => Results}};
                        _ -> false
                    end;
                (_) -> false
            end, Devices),
            NewAccRev = lists:reverse(Tracks, AccRev),
            case length(Devices) of
                1000 -> get_all_gps_tracks(SessionToken, Query, Skip + 1000, NewAccRev);
                _ -> {ok, #{<<"results">> => lists:reverse(NewAccRev)}}
            end;
        {error, Reason} -> {error, Reason}
    end.

ensure_device(ProductId, DeviceId, DevAddr, Data) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, _} -> ok;
        _ ->
            case dgiot_parsex:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"ACL">> := ACL, <<"devType">> := DevType}} ->
                    dgiot_device:create_device(#{
                        <<"devaddr">> => DevAddr,
                        <<"name">> => DevAddr,
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"ACL">> => ACL,
                        <<"status">> => <<"ONLINE">>,
                        <<"brand">> => DevType,
                        <<"devModel">> => DevType,
                        <<"basedata">> => Data
                    });
                _ -> ok
            end
    end.
