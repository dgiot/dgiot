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
                {match, [[<<"channel">>], [ChannelId]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId});
                {match, [[<<"channel">>], [ChannelId], [ProductId]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId, <<"productId">> => ProductId});
                {match, [[<<"channel">>], [ChannelId], [ProductId], [DeviceId]]} ->
                    do_handle(Message#{<<"channelId">> => ChannelId, <<"productId">> => ProductId, <<"devaddr">> => DeviceId});
                _ ->
                    ?LOG(error, "~p, Payload:~p", [Topic, Payload])
            end
    end,
    {noreply, State};

handle_info({start_channel, Module, #{<<"objectId">> := ChannelId, <<"type">> := Type, <<"cType">> := CType, <<"product">> := Products, <<"config">> := Cfg}}, State) ->
    ChannelType = list_to_binary(string:uppercase(binary_to_list(CType))),
    Behaviour = binary_to_atom(list_to_binary(io_lib:format("~s", [Module])), utf8),
    case do_channel(Type, ChannelType, ChannelId, Products, Cfg#{<<"behaviour">> => Behaviour}) of
        ok ->
            do_handle(#{<<"channelId">> => ChannelId, <<"enable">> => true});
        {error, Reason} ->
            ?LOG(error, "Channel[~p] Start Fail, ~p", [ChannelId, Reason])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% type : 1 采集通道 type : 2 资源通道
do_channel(_Type, CType, ChannelId, Products, Cfg) ->
    case dgiot_bridge:get_behaviour(CType) of
        {error, not_find} ->
            {error, {CType, unknow}};
        {ok, Mod} ->
            case erlang:function_exported(Mod, start, 2) of
                true ->
                    ProductIds = do_product(ChannelId, Products),
                    dgiot_data:insert(?ETS, {ChannelId, productIds}, {CType, ProductIds}),
                    case Mod:start(ChannelId, Cfg#{<<"product">> => Products}) of
                        {ok, _} ->
                            dgiot_utils:subscribe(<<"channel/", ChannelId/binary, "/#">>),
                            dgiot_bridge:send_log(ChannelId, "Channel ~s is Install Protocol ~s", [ChannelId, jsx:encode(ProductIds)]),
                            ok;
                        {error, Reason} ->
                            dgiot_data:delete(?ETS, {ChannelId, productIds}),
                            {error, Reason}
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
            dgiot_data:delete(?ETS, {ChannelId, productIds}),
            ?LOG(info, "Channel[~s,~p] offline!", [CType, ChannelId]),
            dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"status">> => <<"OFFLINE">>});
        _ ->
            ok
    end;

%% 启用通道
do_handle(#{<<"channelId">> := ChannelId, <<"enable">> := true}) ->
    case dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"status">> => <<"ONLINE">>}) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG(error, "Channel[~p] Start Fail, ~p", [ChannelId, Reason])
    end;

%% 更新关联产品 { "action": "update" }
do_handle(#{<<"channelId">> := ChannelId, <<"action">> := <<"update">>}) ->
    {ok, Type, _} = dgiot_bridge:get_products(ChannelId),
    dgiot_bridge_loader:load_channel([#{<<"objectId">> => ChannelId}],
        fun(#{<<"product">> := Products}) ->
            ProductIds = do_product(ChannelId, Products),
            dgiot_data:insert(?ETS, {ChannelId, productIds}, {Type, ProductIds}),
            dgiot_bridge:send_log(ChannelId, "Channel ~s is Install Protocol ~s", [ChannelId, jsx:encode(ProductIds)])
        end);

%% 开启日志, cfg 里面可以加过滤
do_handle(#{<<"channelId">> := ChannelId, <<"action">> := <<"start_logger">>} = Filter) ->
    NewFilter = maps:without([<<"channelId">>, <<"action">>], Filter),
    {ok, _Type, ProductIds} = dgiot_bridge:get_products(ChannelId),
    Fmt = "Channel[~s] is Running, Products:~s, Log is ~s",
    Args = [ChannelId, jsx:encode(ProductIds), true],
    dgiot_logger:info(Fmt, Args),
    dgiot_data:insert(?ETS, {ChannelId, log}, NewFilter#{
        <<"time">> => dgiot_datetime:nowstamp()
    }),
    case Filter of
        #{<<"devaddr">> := Addr, <<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, Addr, Fmt, Args);
        #{<<"productId">> := ProductId} ->
            dgiot_bridge:send_log(ChannelId, ProductId, Fmt, Args);
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
    dgiot_data:delete(?ETS, {ChannelId, log}).



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
    %% 提前载入设备
    dgiot_product:load(ProductId),
    do_product(ChannelId, Products, [ProductId | Acc]).
