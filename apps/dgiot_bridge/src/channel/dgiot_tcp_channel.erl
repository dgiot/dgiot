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

-module(dgiot_tcp_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(TYPE, <<"TCP">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {id, mod, product, env = #{}, buff_size = 1024000}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).



%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"TCP采集通道"/utf8>>
    },
    description => #{
        zh => <<"TCP采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 19110,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"buff_size">> => #{
        order => 2,
        type => integer,
        required => false,
        default => 1024000,
        title => #{
            zh => <<"缓存限定"/utf8>>
        },
        description => #{
            zh => <<"设置缓存限定"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).


%% 通道初始化
init(?TYPE, ChannelId, #{<<"port">> := Port, <<"behaviour">> := Mod} = Args) ->
    State = #state{
        id = ChannelId,
        mod = Mod,
        buff_size = maps:get(<<"buff_size">>, Args, 1024000)
    },
    {ok, State, dgiot_tcp_server:child_spec(?MODULE, Port, State)}.


handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info,"Channel ~p", [Event]),
    {ok, State}.

handle_message(Message, State) ->
    ?LOG(info,"Channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning,"Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.


%% =======================

%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            NewTcpState = TCPState#tcp{
                log = log_fun(ChannelId)
            },
            do_product(init, [ChannelId], NewTcpState#tcp{
                state = State#state{
                    env = #{},
                    product = ProductIds
                }
            });
        {error, not_find} ->
            {stop, not_find_channel}
    end.


handle_info({deliver, _, Msg}, TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case jsx:is_json(Payload) of
        true ->
            case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                #{<<"cmd">> := <<"send">>} = Cmd ->
                    handle_info(Cmd, TCPState);
                Info ->
                    handle_info({mqtt, Topic, Info}, TCPState)
            end;
        false ->
            handle_info({mqtt, Topic, Payload}, TCPState)
    end;

%% 对于TCP转一道，向下发的命令
handle_info(#{<<"cmd">> := <<"send">>} = Cmd, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    case do_product(to_frame, [maps:without([<<"cmd">>], Cmd)], TCPState) of
        {ok, NewTCPState} ->
            {noreply, NewTCPState};
        {reply, ProductId, Payload, NewTCPState} ->
            case dgiot_tcp_server:send(TCPState, Payload) of
                ok ->
                    ok;
                {error, Reason} ->
                    dgiot_bridge:send_log(ChannelId, ProductId, "Send Fail, ~p, CMD:~p", [Cmd, Reason])
            end,
            {noreply, NewTCPState}
    end;

handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, mod = Module, product = Product}} = TCPState) ->
    case decode(Buff, Product, TCPState) of
        {ok, [], NewTCPState} ->
            {noreply, NewTCPState};
        {ok, Frames, #tcp{state = #state{product = ProductId}} = NewTCPState} ->
            dgiot_bridge:send_log(ChannelId, ProductId, "~s", [jsx:encode(Frames)]),
            Module:do(ChannelId, ProductId, Frames),
            handle_frames(Frames, NewTCPState);
        {stop, Reason} ->
            {stop, Reason, TCPState}
    end;

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(Info, TCPState) ->
    case do_product(handle_info, [Info], TCPState) of
        {ok, NewTCPState} ->
            {noreply, NewTCPState};
        {stop, Reason, NewTCPState} ->
            {stop, Reason, NewTCPState}
    end.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.


%% =======================

log_fun(ChannelId) ->
    fun(Type, Buff) ->
        Data =
            case Type of
                <<"ERROR">> -> Buff;
                _ -> <<<<Y>> || <<X:4>> <= Buff, Y <- integer_to_list(X, 16)>>
            end,
        dgiot_bridge:send_log(ChannelId, "~s", [<<Type/binary, " ", Data/binary>>])
    end.

send_fun(TCPState) ->
    fun(Payload) ->
        dgiot_tcp_server:send(TCPState, Payload)
    end.

%% 如果是多个产品，先用报文依次解析，只要有一个能解开，则认为此连接为此产品的，
%% 否则一直解析下去，如果一个产品都没有解析成功，则缓存到下一次解析，直到缓存
%% 如果超过了BuffSize，TCP断开，
decode(Payload, [], _TCPState) when byte_size(Payload) > ?MAX_BUFF_SIZE ->
    {stop, buff_size_limit};
decode(Payload, [], TCPState) ->
    {ok, [], TCPState#tcp{buff = Payload}};
decode(Payload, [ProductId | Products], #tcp{state = #state{env = Env} = State} = TCPState) ->
    case dgiot_bridge:parse_frame(ProductId, Payload, Env) of
        {error, function_not_exported} ->
            decode(Payload, Products, TCPState);
        {error, Reason} ->
            {stop, Reason};
        {Rest, Messages, NewEnv} when length(Messages) > 0 ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{env = NewEnv, product = ProductId}}};
        {Rest, Messages} when length(Messages) > 0 ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{product = ProductId}}};
        _ ->
            decode(Payload, Products, TCPState)
    end;

decode(Payload, ProductId, #tcp{state = #state{env = Env} = State} = TCPState) ->
    case dgiot_bridge:parse_frame(ProductId, Payload, Env) of
        {error, Reason} ->
            {stop, Reason};
        {Rest, Messages} ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{product = ProductId}}};
        {Rest, Messages, NewEnv} ->
            {ok, Messages, TCPState#tcp{buff = Rest, state = State#state{env = NewEnv, product = ProductId}}}
    end.

do_product(Fun, Args, #tcp{state = #state{product = ProductIds, id = ChannelId, env = Env}} = TCPState) ->
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, Fun, Args, Env#{<<"send">> => send_fun(TCPState)}) of
        {ok, NewEnv} ->
            {ok, update_state(NewEnv, TCPState)};
        {stop, Reason, NewEnv} ->
            {stop, Reason, update_state(NewEnv, TCPState)};
        {reply, ProductId, Reply, NewEnv} ->
            {reply, ProductId, Reply, update_state(NewEnv, TCPState)}
    end.

handle_frames([], TCPState) ->
    {noreply, TCPState};
handle_frames([Frame | Frames], TCPState) ->
    case do_product(handle_info, [{message, Frame}], TCPState) of
        {ok, NewTCPState} ->
            handle_frames(Frames, NewTCPState);
        {stop, Reason, NewTCPState} ->
            {stop, Reason, NewTCPState}
    end.

update_state(Env, #tcp{state = State} = TCPState) ->
    TCPState#tcp{state = State#state{env = maps:without([<<"send">>], Env)}}.
