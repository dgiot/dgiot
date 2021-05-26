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

%% API
-export([start/0, start_channel/2, register_channel/2, get_behaviour/1, start_channel/3, do_global_message/1]).
-export([get_product_info/1, get_products/1, apply_channel/5, apply_product/3, parse_frame/3, to_frame/3]).
-export([get_data/2, send_log/3, send_log/4, send_log/5]).
-export([get_all_channel/0, control_channel/2]).


start() ->
    dgiot_data:init(?ETS),
    register_all_channel(),
    load_channel(),
    dgiot_hook:add(<<"global/dgiot">>, fun ?MODULE:do_global_message/1).

start_channel(Name, Filter) ->
    dgiot_bridge_loader:start(Name, Filter,
        fun(Module, Channel) -> dgiot_bridge_server ! {start_channel, Module, Channel}
        end).

start_channel(Name, Mod, Where) ->
    start_channel(Name, #{<<"mod">> => Mod, <<"where">> => Where}).


register_channel(CType, Mod) ->
    CType1 = list_to_binary(string:uppercase(binary_to_list(CType))),
    dgiot_data:insert(?ETS, {CType1, behaviour}, Mod).

get_behaviour(CType) ->
    CType1 = list_to_binary(string:uppercase(binary_to_list(CType))),
    dgiot_data:lookup(?ETS, {CType1, behaviour}).

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
    end.


get_product_info(ProductId) ->
    dgiot_product:local(ProductId).


get_products(ChannelId) ->
    case ets:info(?ETS) of
        undefined ->
            application:start(dgiot_bridge);
        _ -> pass
    end,
    case dgiot_data:lookup(?ETS, {ChannelId, productIds}) of
        {error, not_find} ->
            {error, not_find};
        {ok, {Type, ProductIds}} ->
            {ok, Type, ProductIds}
    end.


send_log(ChannelId, ProductId, DevAddr, Fmt, Args) ->
    is_send_log(ChannelId, ProductId, DevAddr,
        fun() ->
            Topic = <<"log/channel/", ChannelId/binary, "/", ProductId/binary, "/", DevAddr/binary>>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).

send_log(ChannelId, ProductId, Fmt, Args) ->
    is_send_log(ChannelId, ProductId, undefined,
        fun() ->
            Topic = <<"log/channel/", ChannelId/binary, "/", ProductId/binary>>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).

send_log(ChannelId, Fmt, Args) ->
    is_send_log(ChannelId, undefined, undefined,
        fun() ->
            Topic = <<"log/channel/", ChannelId/binary>>,
            Payload = io_lib:format("[~s]~p " ++ Fmt, [node(), self() | Args]),
            dgiot_mqtt:publish(ChannelId, Topic, unicode:characters_to_binary(Payload))
        end).


get_data(ProductId, DevAddr) ->
    dgiot_data:match(?ETS, {{ProductId, DevAddr, '_'}, '$1'}).


%%====================================================================
%% Internal functions
%%====================================================================

is_send_log(ChannelId, ProductId, DevAddr, Fun) ->
    Now = dgiot_datetime:nowstamp(),
    case dgiot_data:lookup(?ETS, {ChannelId, log}) of
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
            dgiot_data:delete(?ETS, {ChannelId, log}),
            ok;
        {error, not_find} ->
            ok
    end.

load_channel() ->
    case application:get_env(dgiot_bridge, filters) of
        {ok, Filters} when length(Filters) > 0 ->
            lists:foreach(
                fun(Data) ->
                    Json = list_to_binary(Data),
                    case jsx:is_json(Json) andalso jsx:decode(Json, [{labels, binary}, return_maps]) of
                        false ->
                            ?LOG(error,"~p is not json.", [Json]);
                        Filter ->
                            start_channel(dgiot_bridge, Filter)
                    end
                end, Filters);
        _ ->
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
    Fun =
        fun(_App, {Type, Mod}, Acc) ->
            register_channel(Type, Mod),
            [Type | Acc]
        end,
    search_channel(Fun, []).

get_all_channel() ->
    Fun =
        fun(App, {Type, Mod}, Acc) ->
            Attributes = Mod:module_info(attributes),
            case proplists:get_value(channel_type, Attributes) of
                undefined ->
                    Acc;
                [Channel] ->
                    [format_channel(App, Type, Channel, Attributes) | Acc]
            end
        end,
    search_channel(Fun, []).

format_channel(App, Type, Channel, Attributes) ->
    [Params] = proplists:get_value(params, Attributes, [#{}]),
    Channel#{
        cType => Type,
        app => App,
        params => Params#{
            <<"Size">> => #{
                order => 100,
                type => integer,
                required => false,
                default => 5,
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
        }
    }.

search_channel(Check, Acc0) ->
    Fun =
        fun({App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) == false of
                true ->
                    Acc;
                false ->
                    case [{Type, Mod} || {channel, [Type]} <- Mod:module_info(attributes)] of
                        [] ->
                            Acc;
                        Mods ->
                            lists:foldl(
                                fun(Mod1, Acc1) ->
                                    Check(App, Mod1, Acc1)
                                end, Acc, Mods)
                    end
            end
        end,
    dgiot_plugin:check_module(Fun, Acc0).

control_channel(ChannelId, Action) ->
    IsEnable =
        case Action of
            <<"disable">> ->
                Topic = <<"channel/", ChannelId/binary>>,
                Payload =  jsx:encode(#{<<"enable">> => false}),
                dgiot_mqtt:publish(ChannelId,Topic,Payload),
                false;
            <<"enable">> ->
                Topic = <<"global/dgiot">>,
                Payload =  jsx:encode(#{<<"channelId">> => ChannelId, <<"enable">> =>true}),
                dgiot_mqtt:publish(ChannelId,Topic,Payload),
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
