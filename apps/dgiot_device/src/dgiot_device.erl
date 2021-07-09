%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_device).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TIMEOUT, 60000).
-dgiot_data("ets").
-export([init_ets/0]).
-export([create_device/1, create_device/2, get_sub_device/1, get_sub_device/2, get/2]).
-export([save/1, save/2, save/3, lookup/1, lookup/2, delete/1, delete/2, save_prod/2, lookup_prod/1]).
-export([encode/1, decode/3]).

init_ets() ->
    dgiot_data:init(?DGIOT_PRODUCT),
    ok.

save(Device) ->
    DeviceId = maps:get(<<"objectId">>, Device),
    #{<<"objectId">> := ProductId} = maps:get(<<"product">>, Device),
    #{<<"latitude">> := Latitude, <<"longitude">> := Logitude} = maps:get(<<"location">>, Device),
    UpdatedAt = maps:get(<<"updatedAt">>, Device),
    Product = binary_to_atom(ProductId),
    Acl =
        lists:foldl(fun(X, Acc) ->
            Acc ++ [binary_to_atom(X)]
                    end, [], maps:keys(maps:get(<<"ACL">>, Device))),
    dgiot_mnesia:insert(DeviceId, {[UpdatedAt, {Latitude, Logitude}, Product, Acl], node()}).

save(ProductId, DevAddr) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case lookup(DeviceId) of
        {ok, {[_Ts, Location, Product, Acl], Node}} ->
            dgiot_mnesia:insert(DeviceId, {[dgiot_datetime:now_ms(), Location, Product, Acl], Node});
        _ -> pass
    end.

save(DeviceId, Latitude, Logitude) ->
    case lookup(DeviceId) of
        {ok, {[_Ts, _, Product, Acl], Node}} ->
            dgiot_mnesia:insert(DeviceId, {[dgiot_datetime:now_ms(), {Latitude, Logitude}, Product, Acl], Node});
        _ -> pass
    end.

lookup(DeviceId) ->
    case dgiot_mnesia:lookup(DeviceId) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {ok, [{mnesia, _K, V}]} ->
            {ok, V}
    end.

lookup(ProductId, DevAddr) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_mnesia:lookup(DeviceId) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [{mnesia, _K, V}]} ->
            {Device, _} = V,
            {ok, Device}
    end.

delete(DeviceId) ->
    dgiot_mnesia:delete(DeviceId).

delete(ProductId, DevAddr) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    dgiot_mnesia:delete(DeviceId).

%% 存储产品
%%-define(SMART_PROD, mnesia_smartprod).
%%-record(dgiot_prod, {
%%    key,      % [ProductId], [产品ID]
%%    product   % 产品基本数据,map类型
%%}).
save_prod(ProductId, Product) ->
    case dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product) of
        true -> ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%    #{<<"accessMode">> => <<"r">>,
%%        <<"dataForm">> => #{
%%            <<"address">> => <<"00000000">>,
%%            <<"byteorder">> => <<"big">>,
%%            <<"collection">> => <<"%s/1000">>,
%%            <<"control">> => <<"%s">>,
%%            <<"offset">> => 0,
%%            <<"protocol">> => <<"DLT645">>,
%%            <<"quantity">> => <<"528590">>,
%%            <<"rate">> => 1,
%%            <<"strategy">> => <<"10">>
%%        },
%%        <<"dataType">> => #{
%%            <<"specs">> => #{
%%                <<"max">> => 100000000,
%%                <<"min">> => 0,
%%                <<"step">> => 1,
%%                <<"unit">> => <<"kWÂ·h">>},
%%            <<"type">> => <<"int">>},
%%        <<"identifier">> =>
%%        <<"ActiveTotalEnergy">>,
%%        <<"name">> => <<230,156,137,229,138,159,230,128,187,231,148,181,232,131,189>>,
%%        <<"required">> => true},
lookup_prod(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, ProductId) of
        not_find ->
            not_find;
        Value ->
            {ok, Value}
    end.

get_sub_device(DtuAddr) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

get_sub_device(DtuAddr, SessionToken) ->
    Query = #{<<"keys">> => [<<"route">>, <<"devaddr">>, <<"product">>],
        <<"where">> => #{<<"route.", DtuAddr/binary>> => #{<<"$regex">> => <<".+">>}},
        <<"order">> => <<"devaddr">>, <<"limit">> => 1000,
        <<"include">> => <<"product">>},
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} -> [];
        {ok, #{<<"results">> := List}} -> List;
        _ -> []
    end.

create_device(#{
    <<"status">> := Status,
    <<"brand">> := Brand,
    <<"devModel">> := DevModel,
    <<"name">> := Name,
    <<"devaddr">> := DevAddr,
    <<"product">> := ProductId
} = Device, SessionToken) ->
    #{<<"objectId">> := DeviceId} =
        dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {ok, Result} = dgiot_parse:update_object(<<"Device">>, ObjectId,
                #{
                    <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                    <<"status">> => Status
                },
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            {ok, Result#{<<"objectId">> => ObjectId}};
        _R ->
            {{Y, M, D}, {_, _, _}} = dgiot_datetime:local_time(),
            Batch_name = dgiot_utils:to_list(Y) ++ dgiot_utils:to_list(M) ++ dgiot_utils:to_list(D),
            NewDevice = Device#{
                <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                },
                <<"detail">> => #{
                    <<"desc">> => Name,
                    <<"brand">> => Brand,
                    <<"devModel">> => DevModel,
                    <<"batchId">> => #{
                        <<"batch_name">> => dgiot_utils:to_binary(Batch_name),
                        <<"createdtime">> => dgiot_datetime:now_secs()
                    }
                }
            },
            dgiot_parse:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice),
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}])
    end.

create_device(#{
    <<"status">> := Status,
    <<"brand">> := Brand,
    <<"devModel">> := DevModel,
    <<"name">> := Name,
    <<"devaddr">> := DevAddr,
    <<"product">> := ProductId} = Device) ->
    #{<<"objectId">> := DeviceId} = dgiot_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Result} ->
            Body = #{
                <<"ip">> => maps:get(<<"ip">>, Device, <<"">>),
                <<"status">> => Status},
            dgiot_parse:update_object(<<"Device">>, DeviceId, Body),
            {ok, Result};
        _R ->
            {{Y, M, D}, {_, _, _}} = dgiot_datetime:local_time(),
            Batch_name = dgiot_utils:to_list(Y) ++ dgiot_utils:to_list(M) ++ dgiot_utils:to_list(D),
            NewDevice = Device#{
                <<"location">> => maps:get(<<"location">>, Device, #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441}),
                <<"isEnable">> => maps:get(<<"isEnable">>, Device, true),
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => ProductId
                },
                <<"detail">> => #{
                    <<"desc">> => Name,
                    <<"brand">> => Brand,
                    <<"devModel">> => DevModel,
                    <<"batchId">> => #{
                        <<"batch_name">> => dgiot_utils:to_binary(Batch_name),
                        <<"createdtime">> => dgiot_datetime:now_secs()
                    }
                }
            },
            ?LOG(info, "~p", [NewDevice]),
            R = dgiot_parse:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice)),
            ?LOG(info, "~p", [R]),
            R
    end.

get(ProductId, DevAddr) ->
    Keys = [<<"objectId">>, <<"status">>, <<"isEnable">>],
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, Device} ->
            case maps:get(<<"isEnable">>, Device, false) of
                false ->
                    {error, forbiden};
                true ->
                    {ok, maps:with(Keys, Device)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

encode(Frame) when is_map(Frame) ->
    case maps:get(<<"data">>, Frame, no) of
        no ->
            dgiot_protocol:encode(Frame);
        Data ->
            HxData = dgiot_utils:binary_to_hex(Data),
            case re:run(HxData, <<"0000180080E1">>) of
                nomatch ->
                    dgiot_protocol:encode(Frame);
                _ ->
                    {ignore, Frame}
            end
    end;
encode(Data) when is_binary(Data) ->
    {ok, Data}.

decode([], _, _State) ->
    {error, not_decode};
decode([MsgType | Other], Bin, State) ->
    case dgiot_protocol:decode(MsgType, Bin, State) of
        {ok, Rest, Messages} ->
            {ok, Rest, Messages};
        {error, _} ->
            %?LOG(error,"decode:~p, not this protocol:~p", [Bin, MsgType]),
            decode(Other, Bin, State)
    end.
