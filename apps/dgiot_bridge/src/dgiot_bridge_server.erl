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

-module(dgiot_bridge_server).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-author("kenneth").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({deliver, _, Msg}, State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case catch jsx:decode(Payload, [{labels, binary}, return_maps]) of
        {'EXIT', Reason} ->
            ?LOG(error, "Payload:~p, error:~p", [Payload, Reason]);
        Message ->
            case re:run(Topic, <<"[^//]+">>, [{capture, all, binary}, global]) of
                {match, [[<<"dashboard_task">>], [_DashboardId]]} ->
                    supervisor:start_child(dashboard_task, [Message]);
                {match, [[<<"channel">>], [ChannelId]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId});
                {match, [[<<"channel">>], [ChannelId], [ProductId]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId, <<"productId">> => ProductId});
                {match, [[<<"channel">>], [ChannelId], [ProductId], [Devaddr]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId, <<"productId">> => ProductId, <<"devaddr">> => Devaddr});
                _ ->
                    ?LOG(error, "~p, Payload:~p", [Topic, Payload])
            end
    end,
    {noreply, State};

handle_info({start_channel, Module, #{<<"objectId">> := ChannelId, <<"ACL">> := Acl, <<"type">> := Type, <<"cType">> := CType, <<"product">> := Products, <<"config">> := Cfg}}, State) ->
    ChannelType = list_to_binary(string:uppercase(binary_to_list(CType))),
    Behaviour = binary_to_atom(list_to_binary(io_lib:format("~s", [Module])), utf8),
    dgiot_data:insert(?DGIOT_BRIDGE, {ChannelId, acl}, Acl),
    case start_channel(Type, ChannelType, ChannelId, Products, Cfg#{<<"behaviour">> => Behaviour}) of
        ok ->
            do_handle(#{<<"channelId">> => ChannelId, <<"enable">> => true});
        {error, Reason} ->
            ?LOG(error, "Channel[~p] Start Fail, ~p", [ChannelId, Reason])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG(error, "_Info ~p, State:~p", [_Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_channel(_Type, CType, ChannelId, Products, Cfg) ->
    case maps:get(<<"Node">>, Cfg, <<"all">>) of
        <<"all">> ->
            do_channel(_Type, CType, ChannelId, Products, Cfg);
        Nodes ->
            List = binary:split(Nodes, <<";">>, [global]),
            LocalNode = dgiot_utils:to_binary(node()),
            case lists:member(LocalNode, List) of
                true ->
                    do_channel(_Type, CType, ChannelId, Products, Cfg);
                _ ->
                    {error, {LocalNode, notstart}}
            end
    end.

%% type : 1 采集通道 type : 2 资源通道
do_channel(_Type, CType, ChannelId, Products, Cfg) ->
    case dgiot_bridge:get_behaviour(CType) of
        {error, not_find} ->
            {error, {CType, unknow}};
        {ok, Mod} ->
            dgiot_mqtt:subscribe(<<"dashboard_task/#">>),
            case erlang:module_loaded(Mod) of
                true ->
                    case erlang:function_exported(Mod, start, 2) of
                        true ->
                            ProductIds = do_product(ChannelId, Products),
                            dgiot_data:insert(?DGIOT_BRIDGE, {ChannelId, productIds}, {CType, ProductIds}),
                            case Mod:start(ChannelId, Cfg#{<<"product">> => Products}) of
                                {ok, _} ->
                                    dgiot_mqtt:subscribe(<<"channel/", ChannelId/binary, "/#">>),
                                    dgiot_bridge:send_log(ChannelId, "Channel ~s is Install Protocol ~s", [ChannelId, jsx:encode(ProductIds)]),
                                    ok;
                                {error, Reason} ->
                                    dgiot_data:delete(?DGIOT_BRIDGE, {ChannelId, productIds}),
                                    {error, Reason}
                            end;
                        false ->
                            {error, {Mod, start_error}}
                    end;
                false ->
                    {error, {Mod, start_error}}
            end
    end.

%% 停止通道
do_handle(#{<<"channelId">> := ChannelId, <<"enable">> := false}) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, CType, _ProductIds} ->
            dgiot_channelx:delete(CType, ChannelId),
            dgiot_data:delete(?DGIOT_BRIDGE, {ChannelId, productIds}),
            ?LOG(info, "Channel[~s,~p] offline!", [CType, ChannelId]),
            sysnc_product_channel(ChannelId, <<"disable">>),
            dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"status">> => <<"OFFLINE">>});
        _ ->
            ok
    end;

%% 启用通道
do_handle(#{<<"channelId">> := ChannelId, <<"enable">> := true}) ->
    case dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"status">> => <<"ONLINE">>}) of
        {ok, _} ->
            sysnc_product_channel(ChannelId, <<"enable">>);
        {error, Reason} ->
            ?LOG(error, "Channel[~p] Start Fail, ~p", [ChannelId, Reason])
    end;

%% 更新关联产品 { "action": "update" }
do_handle(#{<<"channelId">> := ChannelId, <<"action">> := <<"update">>}) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, Type, _} ->
            sysnc_product_channel(ChannelId, <<"update">>),
            dgiot_bridge_loader:load_channel([#{<<"objectId">> => ChannelId}],
                fun(#{<<"product">> := Products}) ->
                    ProductIds = do_product(ChannelId, Products),
                    dgiot_data:insert(?DGIOT_BRIDGE, {ChannelId, productIds}, {Type, ProductIds}),
                    dgiot_bridge:send_log(ChannelId, "Channel ~s is Install Protocol ~s", [ChannelId, jsx:encode(ProductIds)])
                end);
        _ -> pass
    end;


%% 开启日志, cfg 里面可以加过滤
do_handle(#{<<"channelId">> := ChannelId, <<"action">> := <<"start_logger">>} = Filter) ->
    NewFilter = maps:without([<<"channelId">>, <<"action">>], Filter),
    {ok, _Type, ProductIds} = dgiot_bridge:get_products(ChannelId),
    Fmt = "Channel[~s] is Running, Products:~s, Log is ~s",
    Args = [ChannelId, jsx:encode(ProductIds), true],
    dgiot_data:insert(?DGIOT_BRIDGE, {ChannelId, log}, NewFilter#{
        <<"time">> => dgiot_datetime:nowstamp()
    }),
    case Filter of
        #{<<"devaddr">> := Addr, <<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, Addr, "Channel[~s] is Running, ProductId:~s, devaddr:~s, Log is ~s", [ChannelId, ProductId, Addr, true]);
        #{<<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, "Channel[~s] is Running, ProductId:~s, Log is ~s", [ChannelId, ProductId, true]);
        _ ->
            dgiot_bridge:send_log(ChannelId, Fmt, Args)
    end;

%% 关闭日志
do_handle(#{<<"channelId">> := ChannelId, <<"action">> := <<"stop_logger">>} = Filter) ->
    Fmt = "Channel[~s] Log is Close",
    Args = [ChannelId],
    case Filter of
        #{<<"devaddr">> := Addr, <<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, Addr, Fmt, Args);
        #{<<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, Fmt, Args);
        _ ->
            dgiot_bridge:send_log(ChannelId, Fmt, Args)
    end,
    dgiot_data:delete(?DGIOT_BRIDGE, {ChannelId, log}).

do_product(ChannelId, Products) ->
    do_product(ChannelId, Products, []).

do_product(_, [], Acc) -> Acc;
do_product(ChannelId, [{ProductId, Product} | Products], Acc) ->
    case maps:get(<<"code">>, maps:get(<<"decoder">>, Product, #{}), no) of
        Code when is_binary(Code), byte_size(Code) > 0 ->
            case dgiot_decoder:install(ProductId, Code) of
                {ok, Mod} ->
                    ?LOG(info, "Product:~s install protocol success, Module:~p", [ProductId, Mod]);
                {error, Reason} ->
                    ?LOG(error, "Product:~s install protocol error, Reason:~p", [ProductId, Reason])
            end;
        _ ->
            ok
    end,
    do_product(ChannelId, Products, [ProductId | Acc]).

%%保存产品的采集协议通道
sysnc_product_channel(ChannelId, Action) ->
    ProductIds =
        case dgiot_bridge:get_products(ChannelId) of
            {error, not_find} ->
                [];
            {ok, _Type, ProductIds1} ->
                ProductIds1
        end,
    case dgiot_data:get(?DGIOT_BRIDGE, {ChannelId, type}) of
        {?PROTOCOL_CHL, CType} ->
            lists:map(fun(ProductId) ->
                PropList =
                    case dgiot_data:get(?DGIOT_PRODUCT_CHANNEL, ProductId) of
                        not_find -> [];
                        List -> List
                    end,
                case Action of
                    <<"update">> ->
                        dgiot_data:insert(?DGIOT_PRODUCT_CHANNEL, ProductId, dgiot_utils:unique_1(PropList ++ [{ChannelId, CType}]));
                    <<"enable">> ->
                        dgiot_data:insert(?DGIOT_PRODUCT_CHANNEL, ProductId, dgiot_utils:unique_1(PropList ++ [{ChannelId, CType}]));
                    _ ->
                        NewPropList = proplists:delete(ChannelId, PropList),
                        dgiot_data:insert(?DGIOT_PRODUCT_CHANNEL, ProductId, NewPropList)
                end
                      end, ProductIds);
        _ -> pass
    end.
