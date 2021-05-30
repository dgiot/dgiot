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

-module(dgiot_shadow).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/dgiot_mnesia.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([save/3, info/2, lookup/2, lookup/3, query/2, page/4, page/5]).
-export([save_hub/4, lookup_hub/2, save_prod/2, lookup_prod/1]).
-export([start/3, start/4, register/4, online/3, offline/3, update/3, do_child/4, do_device/3]).
-export([test/2]).
-define(TIMEOUT, 60000).
-export([create_device/1, create_device/2, get_sub_device/1, get_sub_device/2, save_gateway/5, lookup_gateway/2]).

%% 根据设备地址启动设备实例
start(ProductId, DevAddr, Args) ->
    start(node(), ProductId, DevAddr, Args).
start(Node, ProductId, DevAddr, Args) when Node == node() ->
    dgiot_shadow_sup:start_shadow(ProductId, DevAddr, Args);
start(Node, ProductId, DevAddr, Args) ->
    case rpc:call(Node, ?MODULE, start, [Node, ProductId, DevAddr, Args]) of
        {Err, Reason} when Err == badrpc; Err == error ->
            {error, Reason};
        {ok, Pid} ->
            {ok, Pid}
    end.

%% 设备注册
register(ProductId, DevAddr, ChannelId, Device) ->
    case lookup(ProductId, DevAddr) of
        {error, not_find} ->
            case dgiot_product:add_device(ProductId, Device#{<<"isEnable">> => true, <<"devaddr">> => DevAddr}) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Pid} ->
                    gen_server:call(Pid, {register, #{<<"channel">> => ChannelId}}, ?TIMEOUT)
            end;
        {ok, Node, #dgiot_device{basedata = OldDevice}} when Node == node() ->
            F =
                fun(Key, Value, Acc) ->
                    case maps:get(Key, OldDevice, no) of
                        Value -> Acc;
                        _ -> Acc#{Key => Value}
                    end
                end,
            NewDevice = maps:fold(F, #{}, Device),
            case maps:size(NewDevice) == 0 of
                true ->
                    online(ProductId, DevAddr, #{<<"channel">> => ChannelId});
                false ->
                    online(ProductId, DevAddr, #{<<"channel">> => ChannelId, <<"basedata">> => NewDevice})
            end;
        {ok, Node, _} ->
            case rpc:call(Node, ?MODULE, register, [Node, ProductId, DevAddr, Device]) of
                {badrpc, Reason} ->
                    {error, Reason};
                Res ->
                    Res
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 设备数据更新
update(ProductId, DevAddr, Device) ->
    call(ProductId, DevAddr, {update, Device}).

%% 设备上线
online(ProductId, DevAddr, Message) ->
    call(ProductId, DevAddr, {online, Message}).

%% 设备离线
offline(ProductId, DevAddr, Message) ->
    call(ProductId, DevAddr, {offline, Message}).

%% 存储父设备与子设备之间的路由
%%-record(dgiot_hub, {
%%    key,      % [vcaddr,pn], [网关设备地址,子设备子网地址]
%%    hub    % [ProductId,DevAddr], [产品ID, 设备地址]
%%}).
save_hub(VcAddr, Pn, ProductId, DevAddr) ->
    Key = [VcAddr, Pn],
    HubData = #dgiot_hub{
        key = Key,
        subdev = [ProductId, DevAddr]
    },
    case dgiot_mnesia:insert(?SMART_HUB, HubData) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

lookup_hub(ProductId, DevAddr) ->
    case dgiot_mnesia:lookup(?SMART_HUB, [ProductId, DevAddr]) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [{dgiot_hub, _K, V}]} ->
            V
    end.

save_gateway(ProductId, DevAddr, DtuProductId, VcAddr, Pn) ->
    Key = [ProductId, DevAddr],
    GateWayData = #dgiot_gateway{
        key = Key,
        gatway = [DtuProductId, VcAddr, Pn]
    },
    case dgiot_mnesia:insert(?SMART_GATEWAY, GateWayData) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

lookup_gateway(ProductId, DevAddr) ->
    case dgiot_mnesia:lookup(?SMART_GATEWAY, [ProductId, DevAddr]) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [{dgiot_gateway, _K, V}]} ->
            V
    end.

%% 存储产品
%%-define(SMART_PROD, mnesia_smartprod).
%%-record(dgiot_prod, {
%%    key,      % [ProductId], [产品ID]
%%    product   % 产品基本数据,map类型
%%}).
save_prod(ProductId, Product) ->
    Key = [ProductId],
    ProdData = #dgiot_prod{
        key = Key,
        product = Product
    },
    case dgiot_mnesia:insert(?SMART_PROD, ProdData) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
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
    case dgiot_mnesia:lookup(?SMART_PROD, [ProductId]) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [{dgiot_prod, _K, V}]} ->
            {ok, V}
    end.

%% 存储设备
save(ProductId, DevAddr, BaseData) ->
    Key = [ProductId, DevAddr],
    DevData = #dgiot_device{
        key = Key,
        basedata = BaseData
    },
    case dgiot_mnesia:insert(?SMART_DEV, DevData) of
        {atomic, ok} ->
            Route = #dgiot_route{
                key = Key,
                node = node()
            },
            case dgiot_mnesia:insert(?DGIOT_ROUTE, Route) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    {error, Reason}
            end;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% 设备查找
info(ProductId, DevAddr) ->
    case lookup(ProductId, DevAddr) of
        {ok, Node, Device} ->
            DevInfo = format_device(Device),
            Status = dgiot_shadow_worker:get_status(ProductId, DevAddr),
            {ok, DevInfo#{
                <<"node">> => Node,
                <<"status">> => Status
            }};
        {error, Reason} ->
            {error, Reason}
    end.


lookup(ProductId, DevAddr) ->
    case dgiot_mnesia:lookup(?DGIOT_ROUTE, [ProductId, DevAddr]) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [#dgiot_route{node = Node}]} ->
            lookup(Node, ProductId, DevAddr)
    end.
lookup(Node, ProductId, DevAddr) when Node == node() ->
    case dgiot_mnesia:lookup(?SMART_DEV, [ProductId, DevAddr]) of
        {atomic, []} ->
            {error, not_find};
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [Device]} ->
            {ok, Node, Device}
    end;
lookup(Node, ProductId, DevAddr) ->
    case rpc:call(Node, ?MODULE, lookup, [Node, ProductId, DevAddr]) of
        {badrpc, Reason} ->
            {error, Reason};
        Res ->
            Res
    end.

do_child(ProductId, DevAddr, ShoartAddr, Fun) ->
    case dgiot_shadow_worker:get_child(ProductId, DevAddr, ShoartAddr) of
        {ok, ProductId, Pid} ->
            case dgiot_product:local(ProductId) of
                {ok, Product} ->
                    Fun(Product, Pid);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

do_device(ProductId, DevAddr, Fun) ->
    case lookup(ProductId, DevAddr) of
        {ok, Node, _Device} ->
            Result =
                case Node == node() of
                    true ->
                        dgiot_product:local(ProductId);
                    false ->
                        case rpc:call(Node, dgiot_product, local_cache, [ProductId]) of
                            {badrpc, Reason} ->
                                {error, Reason};
                            Res ->
                                Res
                        end
                end,
            case Result of
                {ok, Product} ->
                    case dgiot_shadow_worker:is_online(ProductId, DevAddr) of
                        false ->
                            {error, not_alive};
                        Pid ->
                            Fun(Product, Pid)
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.


query(ProductId, DevAddr) ->
    Keys = [<<"objectId">>, <<"status">>, <<"isEnable">>],
    Where = #{
        <<"limit">> => 1,
        <<"keys">> => Keys,
        <<"order">> => <<"-updatedAt">>,
        <<"where">> => #{
            <<"devaddr">> => DevAddr,
            <<"product">> => #{
                <<"className">> => <<"Product">>,
                <<"__type">> => <<"Pointer">>,
                <<"objectId">> => ProductId
            }
        }
    },
    case dgiot_parse:query_object(<<"Device">>, Where) of
        {ok, #{<<"results">> := []}} ->
            {error, {device, not_find}};
        {ok, #{<<"results">> := [Device]}} ->
            case maps:get(<<"isEnable">>, Device, false) of
                false ->
                    {error, forbiden};
                true ->
                    {ok, maps:with(Keys, Device)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.



page(PageNo, PageSize, Filter, RowFun) ->
    page(PageNo, PageSize, Filter, RowFun, none).
page(PageNo, PageSize, Filter, RowFun, Order) ->
    NewRowFun =
        fun(#dgiot_route{node = Node, key = [DevType, DevAddr]}) ->
            case lookup(Node, DevType, DevAddr) of
                {ok, DevInfo} ->
                    RowFun(DevInfo);
                {error, _Reason} ->
                    []
            end
        end,
    NewFilter =
        fun(#dgiot_route{node = Node, status = Status, key = [DevType, DevAddr]}) ->
            Filter(#{
                <<"node">> => Node,
                <<"stataus">> => Status,
                <<"devType">> => DevType,
                <<"devaddr">> => DevAddr
            })
        end,
    dgiot_mnesia:page(?DGIOT_ROUTE, PageNo, PageSize, NewFilter, NewRowFun, Order).


%% ======================================================

format_device(#dgiot_device{key = [ProductId, DevAddr], basedata = BaseData}) ->
    BaseData#{
        <<"productId">> => ProductId,
        <<"devaddr">> => DevAddr
    }.

call(ProductId, DevAddr, Msg) ->
    case dgiot_mnesia:lookup(?DGIOT_ROUTE, [ProductId, DevAddr]) of
        {atomic, []} ->
            % 如果设备未在其它节点找到，则在当前节点运行
            % @todo 是否有策略？
            case dgiot_product:add_device(ProductId, DevAddr) of
                {ok, Pid} ->
                    gen_server:call(Pid, Msg, ?TIMEOUT);
                {error, Reason} ->
                    {error, Reason}
            end;
        {aborted, Reason} ->
            {error, Reason};
        {atomic, [#dgiot_route{node = Node}]} ->
            call(Node, ProductId, DevAddr, Msg)
    end.
call(Node, ProductId, DevAddr, Msg) when Node == node() ->
    case dgiot_shadow_worker:is_online(ProductId, DevAddr) of
        false ->
            case dgiot_product:add_device(ProductId, DevAddr) of
                {ok, Pid} ->
                    gen_server:call(Pid, Msg, ?TIMEOUT);
                {error, Reason} ->
                    {error, Reason}
            end;
        Pid ->
            gen_server:call(Pid, Msg, ?TIMEOUT)
    end;
call(Node, ProductId, DevAddr, Msg) ->
    case rpc:call(Node, ?MODULE, call, [Node, ProductId, DevAddr, Msg]) of
        {badrpc, Reason} ->
            {error, Reason};
        Res ->
            Res
    end.

test(ProductId, Count) ->
    dgiot_product:load(ProductId),
    do_reg(ProductId, 1, Count).


do_reg(ProductId, I, Max) when I =< Max ->
    DevAddr = list_to_binary(lists:concat([binary_to_list(ProductId), I])),
    Device = #{
        <<"ip">> => <<"127.0.0.1">>
    },
    case register(ProductId, DevAddr, <<"TestChannel">>, Device) of
        ok -> ok;
        {error, Reason} -> ?LOG(info,"~p, ~p, ~p", [ProductId, DevAddr, Reason])
    end,
    do_reg(ProductId, I + 1, Max);
do_reg(_, _, _) ->
    ok.

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
            ?LOG(info,"~p",[NewDevice]),
            R = dgiot_parse:create_object(<<"Device">>, maps:without([<<"brand">>, <<"devModel">>], NewDevice)),
            ?LOG(info,"~p",[R]),
            R
    end.
