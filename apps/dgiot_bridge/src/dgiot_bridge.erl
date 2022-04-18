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

-module(dgiot_bridge).
-author("kenneth").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").

-dgiot_data("ets").
-export([init_ets/0]).

%% API
-export([start/0, start_channel/2, register_channel/2, get_behaviour/1, start_channel/3, do_global_message/1]).
-export([get_product_info/1, get_products/1, get_acl/1, apply_channel/5, apply_product/3, parse_frame/3, to_frame/3]).
-export([get_data/2, send_log/3, send_log/4, send_log/5]).
-export([get_all_channel/0, control_channel/2, list/0]).

init_ets() ->
    dgiot_data:init(?DGIOT_BRIDGE),
    dgiot_data:init(?DGIOT_RUlES),
    register_all_channel(),
    dgiot_hook:add(<<"global/dgiot">>, fun ?MODULE:do_global_message/1),
    proc_lib:spawn_link(
        fun() ->
            timer:sleep(10000),
            dgiot_bridge:start()
        end).

start() ->
    load_channel().

start_channel(Name, Filter) ->
    dgiot_bridge_loader:start(Name, Filter,
        fun(Module, Channel) ->
            dgiot_bridge_server ! {start_channel, Module, Channel}
        end).

start_channel(Name, Mod, Where) ->
    start_channel(Name, #{<<"mod">> => Mod, <<"where">> => Where}).


register_channel(CType, Mod) ->
    CType1 = list_to_binary(string:uppercase(dgiot_utils:to_list(CType))),
    dgiot_data:insert(?DGIOT_BRIDGE, {CType1, behaviour}, Mod).

get_behaviour(CType) ->
    CType1 = list_to_binary(string:uppercase(dgiot_utils:to_list(CType))),
    dgiot_data:lookup(?DGIOT_BRIDGE, {CType1, behaviour}).

parse_frame(ProductId, Bin, State) ->
    apply_product(ProductId, parse_frame, [Bin, State]).

to_frame(ProductId, Msg, State) ->
    apply_product(ProductId, to_frame, [Msg, State]).


%% 对产品编码器依次调用
apply_channel(_ChannelId, [], _Fun, _Args, Env) ->
    {ok, Env};
apply_channel(ChannelId, [ProductId | ProductIds], Fun, Args, Env) ->
    case apply_channel(ChannelId, ProductId, Fun, Args, Env) of
        {ok, NewEnv} ->
            apply_channel(ChannelId, ProductIds, Fun, Args, NewEnv);
        {reply, ProductId, Reply, NewEnv} ->
            {reply, ProductId, Reply, NewEnv};
        {stop, Reason, NewEnv} ->
            {stop, Reason, NewEnv}
    end;
apply_channel(ChannelId, ProductId, Fun, Args, Env) ->
    case apply_product(ProductId, Fun, Args ++ [Env#{
        <<"channelId">> => ChannelId,
        <<"productId">> => ProductId
    }]) of
        ok ->
            {ok, Env};
        ignore ->
            {ok, Env};
        {error, function_not_exported} ->
            {ok, Env};
        {ok, NewEnv} ->
            {ok, maps:without([<<"channelId">>, <<"productId">>], NewEnv)};
        {reply, Reply} ->
            {reply, ProductId, Reply, Env};
        {reply, Reply, NewEnv} ->
            {reply, ProductId, Reply, maps:without([<<"channelId">>, <<"productId">>], NewEnv)};
        {stop, Reason} ->
            {stop, Reason, Env};
        {stop, Reason, NewEnv} ->
            {stop, Reason, maps:without([<<"channelId">>, <<"productId">>], NewEnv)}
    end.


apply_product(ProductId, Fun, Args) ->
    Mod = binary_to_atom(ProductId, utf8),
    case erlang:module_loaded(Mod) of
        true ->
            case erlang:function_exported(Mod, Fun, length(Args)) of
                true ->
                    case catch apply(Mod, Fun, Args) of
                        {'EXIT', Reason} ->
                            {error, Reason};
                        Result ->
                            Result
                    end;
                false ->
                    {error, function_not_exported}
            end;
        false ->
            {error, function_not_exported}
    end.

get_product_info(ProductId) ->
    dgiot_product:local(ProductId).


get_products(ChannelId) ->
    case ets:info(?DGIOT_BRIDGE) of
        undefined ->
            application:start(dgiot_bridge);
        _ -> pass
    end,
    case dgiot_data:lookup(?DGIOT_BRIDGE, {ChannelId, productIds}) of
        {error, not_find} ->
            {error, not_find};
        {ok, {Type, ProductIds}} ->
            {ok, Type, ProductIds}
    end.


get_acl(ChannelId) ->
    case ets:info(?DGIOT_BRIDGE) of
        undefined ->
            application:start(dgiot_bridge);
        _ -> pass
    end,
    case dgiot_data:lookup(?DGIOT_BRIDGE, {ChannelId, acl}) of
        {error, not_find} ->
            {error, not_find};
        {ok, ACL} ->
            {ok, ACL}
    end.

send_log(ChannelId, ProductId, DevAddr, Fmt, Args) ->
    is_send_log(ChannelId, ProductId, DevAddr,
        fun() ->
            Topic = <<"$dg/channel/", ChannelId/binary, "/", ProductId/binary, "/", DevAddr/binary>>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).

send_log(ChannelId, ProductId, Fmt, Args) ->
    is_send_log(ChannelId, ProductId, undefined,
        fun() ->
            Topic = <<"$dg/channel/", ChannelId/binary, "/", ProductId/binary>>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).

send_log(ChannelId, Fmt, Args) ->
    is_send_log(ChannelId, undefined, undefined,
        fun() ->
            Topic = <<"$dg/channel/", ChannelId/binary, "/channelid">>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).


get_data(ProductId, DevAddr) ->
    dgiot_data:match(?DGIOT_BRIDGE, {{ProductId, DevAddr, '_'}, '$1'}).


%%====================================================================
%% Internal functions
%%====================================================================

is_send_log(ChannelId, ProductId, DevAddr, Fun) ->
    Now = dgiot_datetime:nowstamp(),
    case dgiot_data:lookup(?DGIOT_BRIDGE, {ChannelId, log}) of
        {ok, #{<<"time">> := Time} = Filter} when Now =< Time + 10 * 60 ->
            case ProductId =/= undefined andalso maps:get(<<"productId">>, Filter, undefined) of
                ProductId1 when ProductId1 == false; ProductId1 == undefined; ProductId1 == ProductId ->
                    case DevAddr =/= undefined andalso maps:get(<<"devaddr">>, Filter, undefined) of
                        Devaddr1 when Devaddr1 == false;  Devaddr1 == undefined; Devaddr1 == DevAddr ->
                            Fun();
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        {ok, _} ->
            dgiot_data:delete(?DGIOT_BRIDGE, {ChannelId, log}),
            ok;
        {error, not_find} ->
            ok
    end.

load_channel() ->
    case application:get_env(dgiot_bridge, filters) of
        {ok, Filters} when length(Filters) > 0  ->
            lists:foreach(
                fun(Data) ->
                    Json = list_to_binary(Data),
                    case jsx:is_json(Json) andalso jsx:decode(Json, [{labels, binary}, return_maps]) of
                        false ->
                            ?LOG(error, "~p is not json.", [Json]);
                        Filter ->
                            ?LOG(error, "Filter: ~p", [Filter]),
                            start_channel(dgiot_bridge, Filter)
                    end
                end, Filters);
        _Other ->
            ?LOG(error, "_Other:~p", [_Other]),
            ok
    end.

do_global_message([Msg]) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    application:ensure_started(dgiot_bridge),
    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
        #{<<"channelId">> := ChannelId, <<"enable">> := true} ->
            start_channel(dgiot_bridge, #{
                <<"mod">> => <<"dgiot_bridge_frame">>,
                <<"where">> => #{
                    <<"objectId">> => ChannelId
                }
            });
        _ ->
            ok
    end.

register_all_channel() ->
    lists:map(fun({_, Channel_type}) ->
        Mod = maps:get(mod, Channel_type),
        CType = maps:get(cType, Channel_type),
        register_channel(CType, Mod)
              end, list()).

get_all_channel() ->
    lists:foldl(fun({_, Channel_type}, Acc) ->
        App = maps:get(app, Channel_type),
        Mod = maps:get(mod, Channel_type),
        CType = maps:get(cType, Channel_type),
        Attributes = Mod:module_info(attributes),
        [format_channel(App, CType, Channel_type, Attributes) | Acc]
                end, [], list()).

format_channel(App, CType, Channel_type, Attributes) ->
    [Params] = proplists:get_value(params, Attributes, [#{}]),
    Channel_type#{
        cType => CType,
        app => App,
        params => maps:merge(#{
            <<"Node">> => #{
                order => 99,
                type => string,
                required => false,
                default => <<"all"/utf8>>,
                title => #{
                    en => <<"Node">>,
                    zh => <<"节点"/utf8>>
                },
                description => #{
                    en => <<"Node start">>,
                    zh => <<"指定节点启动"/utf8>>
                }
            },
            <<"Size">> => #{
                order => 100,
                type => integer,
                required => false,
                default => 1,
                title => #{
                    en => <<"Size">>,
                    zh => <<"池子大小"/utf8>>
                },
                description => #{
                    en => <<"Pool Size">>,
                    zh => <<"进程池子数量"/utf8>>
                }
            },
            <<"MaxOverFlow">> => #{
                order => 101,
                type => integer,
                required => false,
                default => 10,
                title => #{
                    en => <<"MaxOverFlow">>,
                    zh => <<"最大溢出"/utf8>>
                },
                description => #{
                    en => <<"MaxOverFlow">>,
                    zh => <<"最大溢出"/utf8>>
                }
            },
            <<"ico">> => #{
                order => 102,
                type => string,
                required => false,
                default => <<"http://dgiot-1253666439.cos.ap-shanghai-fsi.myqcloud.com/logo/logo.png">>,
                title => #{
                    en => <<"channel ICO">>,
                    zh => <<"通道ICO"/utf8>>
                },
                description => #{
                    en => <<"channel ICO">>,
                    zh => <<"通道ICO"/utf8>>
                }
            }
        }, Params)
    }.

list() ->
    Fun =
        fun({App, Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) == false of
                true ->
                    Acc;
                false ->
                    case [Channel || {channel_type, [Channel]} <- Mod:module_info(attributes)] of
                        [] ->
                            Acc;
                        [Channel | _] ->
                            [{maps:get(priority, Channel, 255), Channel#{app => App, mod => Mod, vsn => Vsn}}] ++ Acc
                    end
            end
        end,
    lists:sort(dgiot_plugin:check_module(Fun, [])).

control_channel(ChannelId, Action) ->
    IsEnable =
        case Action of
            <<"disable">> ->
                Topic = <<"channel/", ChannelId/binary>>,
                Payload = jsx:encode(#{<<"enable">> => false}),
                dgiot_mqtt:publish(ChannelId, Topic, Payload),
                false;
            <<"enable">> ->
                Topic = <<"global/dgiot">>,
                Payload = jsx:encode(#{<<"channelId">> => ChannelId, <<"enable">> => true}),
                dgiot_mqtt:publish(ChannelId, Topic, Payload),
                true;
            <<"update">> ->
                Topic = <<"channel/", ChannelId/binary>>,
                Payload = jsx:encode(#{<<"channelId">> => ChannelId, <<"action">> => <<"update">>}),
                dgiot_mqtt:publish(ChannelId, Topic, Payload),
                true;
            <<"start_logger">> ->
                Topic = <<"channel/", ChannelId/binary>>,
                Payload = jsx:encode(#{<<"channelId">> => ChannelId, <<"action">> => <<"start_logger">>}),
                dgiot_mqtt:publish(ChannelId, Topic, Payload),
                true;
            <<"stop_logger">> ->
                Topic = <<"channel/", ChannelId/binary>>,
                Payload = jsx:encode(#{<<"channelId">> => ChannelId, <<"action">> => <<"stop_logger">>}),
                dgiot_mqtt:publish(ChannelId, Topic, Payload),
                true
        end,
    Fun =
        fun() ->
            case dgiot_parse:get_object(<<"Channel">>, ChannelId) of
                {ok, #{<<"status">> := Status}} ->
                    case IsEnable of
                        true -> Status == <<"ONLINE">>;
                        false -> Status == <<"OFFLINE">>
                    end;
                _ ->
                    false
            end
        end,
    case dgiot_parse:update_object(<<"Channel">>, ChannelId, #{<<"isEnable">> => IsEnable}) of
        {ok, Update} ->
            case wait_request(30000, Fun) of
                false -> {500, #{}};
                true -> {ok, Update}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


wait_request(Time, _) when Time =< 0 ->
    false;
wait_request(Time, Fun) ->
    Sec = min(Time, 1000),
    receive
    after Sec ->
        case Fun() of
            false ->
                wait_request(Time - Sec, Fun);
            Result ->
                Result
        end
    end.
