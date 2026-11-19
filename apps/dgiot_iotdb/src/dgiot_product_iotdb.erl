-module(dgiot_product_iotdb).
-include("dgiot_iotdb.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_device/include/dgiot_device.hrl").

-export([get_product/2, get_products/2, get_channel/1, do_channel/3, get_product_data/4]).

get_product(ProductId, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find -> {error, not_find_iotdb};
        ChannelId ->
            Limit = positive_integer(maps:get(<<"limit">>, Query, 10), 10),
            Skip = nonnegative_integer(maps:get(<<"skip">>, Query, 0), 0),
            DeviceLimit = min(Limit + Skip, 100000),
            DeviceQuery = (maps:without([<<"skip">>, <<"deviceid">>, <<"pid">>], Query))#{
                <<"limit">> => DeviceLimit},
            case query_product_devices(ChannelId, ProductId, DeviceQuery, 0, []) of
                {ok, Rows} ->
                    Direction = case maps:get(<<"order">>, Query, <<>>) of
                        <<"-createdat">> -> desc;
                        <<"-time">> -> desc;
                        _ -> asc
                    end,
                    Sorted = lists:sort(fun(A, B) -> compare_rows(A, B, Direction) end, Rows),
                    Page = lists:sublist(drop_rows(Sorted, Skip), Limit),
                    {ok, #{<<"results">> => Page, <<"rows">> => length(Page), <<"code">> => 200}};
                Error -> Error
            end
    end.

get_products(ProductId, ChannelId) ->
    ProductIds =
        case dgiot_bridge:get_product_info(ProductId) of
            {ok, #{<<"nodeType">> := 2}} ->
                get_sub_products(ProductId);
            _ -> [ProductId]
        end,
    lists:foreach(fun(Id) -> dgiot_data:insert({Id, ?TYPE}, ChannelId) end, ProductIds),
    ProductIds.

get_channel(Session) ->
    Body = #{
        <<"keys">> => <<"objectId">>,
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{<<"cType">> => ?TYPE, <<"isEnable">> => true}
    },
    SessionKey = {?TYPE, Session},
    case dgiot_data:get(?DGIOT_CHANNEL_SESSION, SessionKey) of
        not_find ->
            case dgiot_parsex:query_object(<<"Channel">>, Body,
                    [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
                {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} ->
                    dgiot_data:insert(?DGIOT_CHANNEL_SESSION, SessionKey, ChannelId),
                    {ok, ChannelId};
                {ok, #{<<"results">> := []}} -> {error, <<"not find channel">>};
                {error, Reason} -> {error, Reason}
            end;
        ChannelId -> {ok, ChannelId}
    end.

do_channel(ProductId, Session, Fun) when is_function(Fun, 1) ->
    Body = #{
        <<"keys">> => <<"objectId">>,
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"cType">> => ?TYPE,
            <<"isEnable">> => true,
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"Product">>,
                <<"objectId">> => ProductId
            }
        }
    },
    case dgiot_parsex:query_object(<<"Channel">>, Body,
            [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} -> Fun(ChannelId);
        {ok, #{<<"results">> := []}} -> {404, <<"not find channel">>};
        {error, Reason} -> {error, Reason}
    end.

get_product_data(Channel, ProductId, DeviceId, Args) ->
    Query = maps:without([<<"productid">>, <<"deviceid">>, <<"productId">>, <<"deviceId">>], Args),
    Device = dgiot_iotdb:get_device_path(Channel, ProductId, DeviceId),
    case dgiot_iotdb:query_object(Channel, ProductId, Device, Query) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {400, Reason}
    end.

query_product_devices(ChannelId, ProductId, Query, Skip, AccRev) ->
    DeviceQuery = #{<<"keys">> => [<<"objectId">>, <<"devaddr">>],
        <<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 1000,
        <<"skip">> => Skip},
    case dgiot_parsex:query_object(<<"Device">>, DeviceQuery) of
        {ok, #{<<"results">> := Devices}} ->
            case query_product_device_page(ChannelId, ProductId, Devices, Query, AccRev) of
                {ok, NewAccRev} when length(Devices) =:= 1000 ->
                    query_product_devices(ChannelId, ProductId, Query, Skip + 1000, NewAccRev);
                {ok, NewAccRev} -> {ok, lists:reverse(NewAccRev)};
                Error -> Error
            end;
        {error, Reason} -> {error, Reason};
        Other -> {error, {invalid_device_page, Other}}
    end.

query_product_device_page(_ChannelId, _ProductId, [], _Query, AccRev) ->
    {ok, AccRev};
query_product_device_page(ChannelId, ProductId,
        [#{<<"objectId">> := DeviceId} = Device | Rest], Query, AccRev) ->
    case dgiot_device_iotdb:get_history_data(ChannelId, ProductId, DeviceId, Query) of
        {_Names, {ok, #{<<"results">> := Results}}} ->
            DevAddr = maps:get(<<"devaddr">>, Device, <<>>),
            Tagged = [Row#{<<"deviceid">> => DeviceId, <<"devaddr">> => DevAddr} || Row <- Results],
            query_product_device_page(ChannelId, ProductId, Rest, Query, lists:reverse(Tagged, AccRev));
        {_Names, {error, Reason}} -> {error, Reason}
    end;
query_product_device_page(ChannelId, ProductId, [_ | Rest], Query, AccRev) ->
    query_product_device_page(ChannelId, ProductId, Rest, Query, AccRev).

compare_rows(A, B, desc) -> row_timestamp(A) > row_timestamp(B);
compare_rows(A, B, asc) -> row_timestamp(A) < row_timestamp(B).

row_timestamp(#{<<"createdat">> := Timestamp}) -> Timestamp;
row_timestamp(_) -> 0.

drop_rows(Rows, 0) -> Rows;
drop_rows([_ | Rest], Count) when Count > 0 -> drop_rows(Rest, Count - 1);
drop_rows([], _Count) -> [].

positive_integer(Value, _Default) when is_integer(Value), Value > 0 -> Value;
positive_integer(_Value, Default) -> Default.

nonnegative_integer(Value, _Default) when is_integer(Value), Value >= 0 -> Value;
nonnegative_integer(_Value, Default) -> Default.

get_sub_products(ProductId) ->
    case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1,
            <<"where">> => #{<<"product">> => ProductId}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ParentId} | _]}} ->
            case dgiot_parsex:query_object(<<"Device">>, #{<<"limit">> => 1000,
                    <<"keys">> => [<<"product">>], <<"where">> => #{<<"parentId">> => ParentId}}) of
                {ok, #{<<"results">> := Results}} ->
                    ChildIds = [ChildId || #{<<"product">> := #{<<"objectId">> := ChildId}} <- Results],
                    lists:usort([ProductId | ChildIds]);
                _ -> [ProductId]
            end;
        _ -> [ProductId]
    end.
