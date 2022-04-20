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

-export([start/0, load/0, load/1, init/3, handle_event/3, handle_message/2, stop/3]).
-export([add_device/2, local/1, save/1, get/1, synchronize_device/1]).
-export([add_handler/4, do_handler/3, del_handler/1]).
-export([update_config/2, parse_frame/3, to_frame/2, delete/1, save_prod/2, lookup_prod/1]).

-export([create_product/2]).

start() ->
    dgiot_data:init(?MODULE),
    dgiot_channelx:add(?TYPE, ?CHANNEL, ?MODULE, #{}).

%% 载入产品
load(ProductId) ->
    case dgiot_channelx:call(?TYPE, ?CHANNEL, {load, ProductId, true}) of
        {ok, Product} = Result ->
            dgiot_product:save_prod(ProductId, Product),
            Result;
        Error -> Error
    end.

%% 通道初始化
init(?TYPE, _ChannelId, _ChannelArgs) ->
    {ok, #state{}}.

%% 通道消息处理,注意：进程池调用
handle_event(EventId, Event, _State) ->
    ?LOG(info, "channel ~p, ~p", [EventId, Event]),
    ok.

handle_message({load, ProductId, IsLoadDevice}, State) ->
    Reply =
        case dgiot_product:get(ProductId) of
            {ok, Product} ->
                case dgiot_product:save(Product) of
                    {ok, Product1} ->
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
                    _ ->
                        pass
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    {reply, Reply, State};

handle_message(_Message, State) ->
    %?LOG(info,"channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _) ->
    ?LOG(error, "channel stop ~p,~p", [ChannelType, ChannelId]),
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

load() ->
    Success = fun(Page) ->
        lists:map(fun(Product) ->
            dgiot_product:save(Product)
                  end, Page)
              end,
    Query = #{
        <<"where">> => #{}
    },
    dgiot_parse_loader:start(<<"Product">>, Query, 0, 100, 1000000, Success).


%% 存储产品
%%-define(SMART_PROD, mnesia_smartprod).
%%-record(dgiot_prod, {
%%    key,      % [ProductId], [产品ID]
%%    product   % 产品基本数据,map类型
%%}).
save_prod(ProductId, #{<<"thing">> := _thing} = Product) ->
    dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product),
    ?LOG(debug, "product ~p", [Product]),
    {ok, Product};

save_prod(_ProductId, _Product) ->
    pass.

lookup_prod(ProductId) ->
    case dgiot_data:get(?DGIOT_PRODUCT, ProductId) of
        not_find ->
            not_find;
        Value ->
            {ok, Value}
    end.

save(#{<<"thing">> := _thing} = Product) ->
    Product1 = format_product(Product),
    #{<<"productId">> := ProductId} = Product1,
    dgiot_data:insert(?DGIOT_PRODUCT, ProductId, Product1),
    ?LOG(debug, "product ~p", [Product1]),
    {ok, Product1};

save(_Product) ->
    ?LOG(debug, "product error ~p", [_Product]),
    pass.

local(ProductId) ->
    case dgiot_data:lookup(?DGIOT_PRODUCT, ProductId) of
        {ok, Product} ->
            {ok, Product};
        {error, not_find} ->
            {error, not_find}
    end.

delete(ProductId) ->
    dgiot_data:delete(?DGIOT_PRODUCT, ProductId).

get(ProductId) ->
    Keys = [<<"ACL">>, <<"status">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>, <<"productSecret">>],
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            {ok, maps:with(Keys, Product)};
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
    Keys = [<<"ACL">>, <<"status">>, <<"nodeType">>, <<"dynamicReg">>, <<"topics">>, <<"productSecret">>],
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
            ?LOG(debug, "ProductId:~p, DevAddr:~p -> ~p", [ProductId, Device, Pid]);
        {error, Reason} ->
            ?LOG(debug, "ProductId:~p, DevAddr:~p -> ~p", [ProductId, Device, Reason])
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
            ?LOG(error, "do handler error, ~p ~p", [ProductId, Reason]);
        _ ->
            ok
    end,
    excute_handler(ProductId, Actions, Message);
excute_handler(ProductId, [[ChannelId, Fun] | Actions], Message) when is_function(Fun) ->
    case catch Fun(ProductId, ChannelId, Message) of
        {'EXIT', Reason} ->
            ?LOG(error, "do handler error, ~p ~p", [ProductId, Reason]);
        _ ->
            ok
    end,
    excute_handler(ProductId, Actions, Message).

update_config(Product, SessionToken) ->
    create_product(Product, SessionToken).


create_product(#{<<"name">> := ProductName, <<"devType">> := DevType, <<"category">> := #{
    <<"objectId">> := CategoryId, <<"__type">> := <<"Pointer">>, <<"className">> := <<"Category">>}} = Product, SessionToken) ->
    ProductId = dgiot_parse_id:get_productid(CategoryId, DevType, ProductName),
    case dgiot_parse:get_object(<<"Product">>, ProductId, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            dgiot_parse:update_object(<<"Product">>, ObjectId, Product,
                [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
        _ ->
            ACL = maps:get(<<"ACL">>, Product, #{}),
            case dgiot_auth:get_session(SessionToken) of
                #{<<"roles">> := Roles} = _User ->
                    [#{<<"name">> := Role} | _] = maps:values(Roles),
                    CreateProductArgs = Product#{
                        <<"ACL">> => ACL#{
                            <<"role:", Role/binary>> => #{
                                <<"read">> => true,
                                <<"write">> => true
                            }
                        },
                        <<"productSecret">> => dgiot_utils:random()},
                    io:format("CreateProductArgs ~p~n", [CreateProductArgs]),
                    dgiot_parse:create_object(<<"Product">>,
                        CreateProductArgs, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]);
                Err ->
                    {400, Err}
            end
    end.

parse_frame(ProductId, Bin, Opts) ->
    apply(binary_to_atom(ProductId, utf8), parse_frame, [Bin, Opts]).

to_frame(ProductId, Msg) ->
    apply(binary_to_atom(ProductId, utf8), to_frame, [Msg]).


