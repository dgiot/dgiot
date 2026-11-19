-module(dgiot_iotdb_adapter).
-include("dgiot_iotdb.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([save/1, save/2, save/3, do_save/2, do_save/4, save_sql/2, save_sql/3]).

save_sql(ProductId, DevAddr, Sql) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(DevAddr)),
    dgiot_device:online(DeviceId),
    do_channel(ProductId, fun(ChannelId) -> do_save_sql(ChannelId, Sql) end).

save_sql(ProductId, Sql) ->
    do_channel(ProductId, fun(ChannelId) -> do_save_sql(ChannelId, Sql) end).

save(ProductId, DevAddr, Data) when is_map(Data) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(DevAddr)),
    dgiot_device:online(DeviceId),
    do_channel(ProductId, fun(ChannelId) -> do_save(ChannelId, ProductId, DevAddr, Data) end).

save(ProductId, Data) when is_map(Data) ->
    do_channel(ProductId, fun(ChannelId) -> do_save(ChannelId, Data#{<<"productId">> => ProductId}) end).

save(#{<<"product">> := ProductId} = Message) ->
    case format(Message) of
        ignore -> ignore;
        {ok, Data} -> save(ProductId, Data);
        {error, Reason} ->
            ?LOG(warning, "~p format error ~p", [Message, Reason]),
            {error, Reason}
    end;
save(_Message) ->
    {error, invalid_message}.

do_save(ChannelId, ProductId, DevAddr, Data) ->
    dgiot_channelx:do_message(?TYPE, ChannelId, {data, ProductId, DevAddr, Data, #{}}, 30000).

do_save(ChannelId, Data) ->
    dgiot_channelx:do_message(?TYPE, ChannelId, {data, Data, #{}}, 30000).

do_save_sql(ChannelId, Sql) ->
    dgiot_channelx:do_message(?TYPE, ChannelId, {sql, Sql}, 30000).

do_channel(ProductId, Fun) ->
    case dgiot_iotdb:get_channel(ProductId) of
        {ok, ChannelId} -> Fun(ChannelId);
        {error, not_find_iotdb} ->
            ?LOG(warning, "~p not find IoTDB channel", [ProductId]),
            {error, not_find_iotdb};
        Error -> Error
    end.

format(#{<<"devaddr">> := DevAddr, <<"dtuaddr">> := DtuAddr,
        <<"thing">> := Things} = Message) when is_list(Things) ->
    Data = format_thing(Things, #{}),
    case map_size(Data) of
        0 -> ignore;
        _ -> {ok, Data#{<<"dtuaddr">> => DtuAddr,
            <<"productId">> => maps:get(<<"product">>, Message),
            <<"devaddr">> => DevAddr}}
    end;
format(_Message) ->
    {error, not_match}.

format_thing([], Acc) -> Acc;
format_thing([#{<<"value">> := Value, <<"identifier">> := Identifier} | Rest], Acc) ->
    format_thing(Rest, Acc#{dgiot_utils:to_binary(Identifier) => Value});
format_thing([_ | Rest], Acc) ->
    format_thing(Rest, Acc).
