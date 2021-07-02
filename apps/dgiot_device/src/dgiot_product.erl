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

-module(dgiot_product).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"shadow">>).
-define(CHANNEL, <<"product">>).
-define(CACHE, binary_to_atom(<<?TYPE/binary, ?CHANNEL/binary>>, utf8)).
-define(CONFIG(Key, Default), dgiot:get_env(dgiot_device, Key, Default)).
-record(state, {}).

-export([start/0, load/1, init/3, handle_event/3, handle_message/2, stop/3]).
-export([add_device/2, local/1, save/1, get/1, synchronize_device/1]).
-export([add_handler/4, do_handler/3, del_handler/1]).
-export([update_config/2, parse_frame/3, to_frame/2]).

-export([create_product/2]).


start() ->
    dgiot_data:init(?MODULE),
    dgiot_channelx:add(?TYPE, ?CHANNEL, ?MODULE, #{}).

%% 载入产品
load(ProductId) ->
    case dgiot_channelx:call(?TYPE, ?CHANNEL, {load, ProductId, true}) of
        {ok, Product} = Result ->
            dgiot_device:save_prod(ProductId, Product),
            Result;
        Error -> Error
    end.

%% 通道初始化
init(?TYPE, _ChannelId, _ChannelArgs) ->
    {ok, #state{}}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info,"channel ~p, ~p", [EventId, Event]),
    ok.

handle_message({load, ProductId, IsLoadDevice}, State) ->
    Reply =
        case dgiot_product:get(ProductId) of
            {ok, Product} ->
                {ok, Product1} = dgiot_product:save(Product),
                case IsLoadDevice of
                    true ->
                        case dgiot_product:synchronize_device(ProductId) of
                            ok ->
                                {ok, Product1};
                            {error, Why} ->
                                {error, Why}
                        end;
                    false ->
                        {ok, Product1}
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    {reply, Reply, State};

handle_message(_Message, State) ->
    %?LOG(info,"channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _) ->
    ?LOG(error,"channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% 注册数据处理通道
add_handler([], _, _, _) -> ok;
add_handler([ProductId | ProductIds], ChannelId, EndPoint, Action) ->
    ok = add_handler(ProductId, ChannelId, EndPoint, Action),
    add_handler(ProductIds, ChannelId, EndPoint, Action);
add_handler(ProductId, ChannelId, EndPoint, Action) ->
    dgiot_data:insert(?MODULE, {ChannelId, ProductId, EndPoint}, Action),
    ok.

%% 删除处理通道
del_handler(ChannelId) ->
    dgiot_data:match_delete(?MODULE, {{ChannelId, '_', '_'}, '_'}).

do_handler(ProductId, EndPoint, Message) ->
    case dgiot_data:match(?MODULE, {{'$1', ProductId, EndPoint}, '$2'}) of
        {ok, Actions} ->
            excute_handler(ProductId, Actions, Message);
        {error, empty} ->
            ok
    end.

%% 同步设备
synchronize_device(ProductId) ->
    Query = #{
        <<"where">> => #{
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"Product">>,
                <<"objectId">> => ProductId
            }
        }
    },
    case local(ProductId) of
        {ok, Product} ->
            case synchronize_device(Query, fun(Page) -> load_device(Product, Page) end) of
                ok ->
                    ok;
                {error, Why} ->
                    {error, Why}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


save(Product) ->
    Product1 = format_product(Product),
    #{<<"productId">> := ProductId} = Product1,
    dgiot_data:insert(?MODULE, ProductId, Product1),
    ?LOG(debug,"product ~p", [Product1]),
    {ok, Product1}.

local(ProductId) ->
    case dgiot_data:lookup(?DGIOT_PRODUCT, ProductId) of
        {ok, Product} ->
            {ok, Product};
        {error, not_find} ->
            {error, not_find}
    end.

get(ProductId) ->
    Keys = [<<"nodeType">>, <<"thing">>, <<"dynamicReg">>, <<"topics">>, <<"ACL">>],
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"results">> := Product}} ->
            {ok, maps:with(Keys,Product)};
        {error, Reason} ->
            {error, Reason}
    end.


% 1.启动设备进程必需产品要先load过，否则禁止启动
% 保证产品与设备在同一节点上
% 2.dynamicReg ：load 为读库启动的
%   dynamicReg ：true 为动态注册
add_device(ProductId, Device) when is_map(Device) ->
    case local(ProductId) of
        {error, not_find} ->
            {error, {product, not_find}};
        {ok, Product} ->
            add_device(Product, Device)
    end;
add_device(ProductId, DevAddr) ->
    case local(ProductId) of
        {error, not_find} ->
            {error, {product, not_find}};
        {ok, Product} ->
            case dgiot_device:get(ProductId, DevAddr) of
                {ok, Device} ->
                    add_device(Product, Device);
                {error, Reason} ->
                    {error, Reason}
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
format_product(#{<<"objectId">> := ProductId} = Product) ->
    Thing = maps:get(<<"thing">>, Product, #{}),
    Props = maps:get(<<"properties">>, Thing, []),
    Keys = [<<"ACL">>, <<"status">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>],
    Map = maps:with(Keys, Product),
    Map#{
        <<"productId">> => ProductId,
        <<"topics">> => maps:get(<<"topics">>, Product, []),
        <<"thing">> => Thing#{
            <<"properties">> => Props
        }
    }.

%% =============根据产品载入设备============
synchronize_device(Query, Fun) ->
    MaxTotal = ?CONFIG(device_limit, 1000000),
    PageSize = ?CONFIG(page_size, 200),
    Keys = [<<"devaddr">>, <<"isEnable">>, <<"route">>, <<"status">>],
    dgiot_parse_loader:start(<<"Device">>, Query#{<<"keys">> => Keys}, 1, PageSize, MaxTotal, Fun).

load_device(_, []) -> ok;
load_device(#{<<"productId">> := ProductId} = Product, [Device | Devices]) ->
    case add_device(Product, Device) of
        {ok, Pid} ->
            ?LOG(debug,"ProductId:~p, DevAddr:~p -> ~p", [ProductId, Device, Pid]);
        {error, Reason} ->
            ?LOG(debug,"ProductId:~p, DevAddr:~p -> ~p", [ProductId, Device, Reason])
    end,
    load_device(Product, Devices).

excute_handler(_, [], _) ->
    ok;
excute_handler(ProductId, [[ChannelId, {channel, Type}] | Actions], Message) ->
    Context = #{},
    dgiot_channelx:do_message(Type, ChannelId, {data, Message, Context}, 30000),
    excute_handler(ProductId, Actions, Message);
excute_handler(ProductId, [[ChannelId, {Mod, Fun, Args}] | Actions], Message) ->
    case catch apply(Mod, Fun, [ProductId, ChannelId, Message | Args]) of
        {'EXIT', Reason} ->
            ?LOG(error,"do handler error, ~p ~p", [ProductId, Reason]);
        _ ->
            ok
    end,
    excute_handler(ProductId, Actions, Message);
excute_handler(ProductId, [[ChannelId, Fun] | Actions], Message) when is_function(Fun) ->
    case catch Fun(ProductId, ChannelId, Message) of
        {'EXIT', Reason} ->
            ?LOG(error,"do handler error, ~p ~p", [ProductId, Reason]);
        _ ->
            ok
    end,
    excute_handler(ProductId, Actions, Message).

update_config(#{<<"config">> := Config, <<"objectId">> := ProductId} = Product, SessionToken) when is_map(Config) ->
    case dgiot_parse:query_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := #{<<"objectId">> := ObjectId, <<"config">> := OldConfig, <<"thing">> := Thing}}} ->
            ControlList = get_control(Config, OldConfig),
            NewThing = update_thing(#{<<"config">> => Config#{<<"components">> => ControlList}, <<"thing">> => Thing}),
            {ok, R1} = dgiot_parse:update_object(<<"Product">>, ObjectId,
                #{<<"config">> => Config#{<<"components">> => ControlList}, <<"thing">> => NewThing},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            {update, R1#{<<"objectId">> => ObjectId}};
        {ok, #{<<"results">> := #{<<"objectId">> := ObjectId, <<"config">> := OldConfig}}} ->
            {ok, R} = dgiot_parse:update_object(<<"Product">>, ObjectId,
                #{<<"config">> => Config#{<<"components">> => get_control(Config, OldConfig)}},
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]),
            {update, R#{<<"objectId">> => ObjectId}};
        _ ->
            ?LOG(info,"Product ~p", [Product]),
            create_product(Product, SessionToken)
    end;

update_config(_Other, _SessionToken) ->
    #{}.

get_control(Config, OldConfig) ->
    lists:foldl(fun(X, Acc) ->
        case maps:with([<<"address">>, <<"name">>, <<"wumoxing">>, <<"style">>, <<"action">>, <<"dataBind">>], X) of
            #{<<"address">> := Addr, <<"name">> := Name, <<"wumoxing">> := Wumoxing, <<"style">> := Style,
                <<"action">> := Action, <<"dataBind">> := DataBind} ->
                lists:foldl(fun(Y, Acc1) ->
                    case Y of
                        #{<<"name">> := Name, <<"address">> := Addr} ->
                            Acc1 ++ [Y#{
                                <<"wumoxing">> => Wumoxing,
                                <<"style">> => Style,
                                <<"action">> => Action,
                                <<"dataBind">> => DataBind
                            }];
                        _ -> Acc1 ++ [Y]
                    end
                            end, [], Acc);
            _ -> Acc
        end
                end, maps:get(<<"components">>, Config, []), maps:get(<<"components">>, OldConfig, [])).

update_thing(#{<<"config">> := Config, <<"thing">> := Thing}) when is_map(Config); is_map(Thing) ->
    NewPropes =
        lists:foldl(
            fun(X, Acc) ->
                case maps:with([<<"address">>, <<"identifier">>, <<"wumoxing">>], X) of
                    #{<<"address">> := Addr, <<"identifier">> := Quantity, <<"wumoxing">> := #{<<"identifier">> := Identifier}} ->
%%                    ?LOG(info,"Addr ~p ,Quantity ~p ,Identifier ~p", [Addr, Quantity, Identifier]),
                        lists:foldl(fun(Y, Acc1) ->
                            case Y of
                                #{<<"identifier">> := Identifier} ->
                                    Acc1 ++ [Y#{<<"dataForm">> => #{
                                        <<"address">> => Addr,
                                        <<"quantity">> => Quantity}}];
                                _ -> Acc1 ++ [Y]
                            end
                                    end, [], Acc);
                    _ -> Acc
                end
            end, maps:get(<<"properties">>, Thing, []), maps:get(<<"components">>, Config, [])),
    #{<<"properties">> => NewPropes};

update_thing(_Other) ->
    #{}.

create_product(#{<<"name">> := ProductName, <<"devType">> := DevType,
    <<"category">> := Category} = Product, SessionToken) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{
        <<"name">> => ProductName,
        <<"devType">> => DevType,
        <<"category">> => Category
    }},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
            dgiot_parse:update_object(<<"Product">>, ObjectId, Product,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        _ ->
            case dgiot_auth:get_session(SessionToken) of
                #{<<"roles">> := Roles} = _User ->
                    [#{<<"name">> := Role} | _] = maps:values(Roles),
                    dgiot_parse:create_object(<<"Product">>,
                        Product#{
                            <<"ACL">> => #{<<"role:", Role/binary>> => #{
                                <<"read">> => true,
                                <<"write">> => true}
                            },
                            <<"productSecret">> => dgiot_utils:random()},
                        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                Err -> {400, Err}
            end
    end.


parse_frame(ProductId, Bin, Opts) ->
    apply(binary_to_atom(ProductId, utf8), parse_frame, [Bin, Opts]).

to_frame(ProductId, Msg) ->
    apply(binary_to_atom(ProductId, utf8), to_frame, [Msg]).

