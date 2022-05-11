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

-module(dgiot_tcp_worker).
-author("kenneth").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TCP">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {
    id,
    buff_size = 1024000,
    devaddr = <<>>,
    heartcount = 0,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    env = #{},
    dtutype = <<>>,
    module = dgiot_tcp,
    productid = <<>>
}).


%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

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
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"thing">>, ProductId, DevAddr, <<"tcp">>, <<"hex">>] ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            dgiot_device:save_log(DeviceId, Payload, ['tcp_send']),
            dgiot_tcp_server:send(TCPState, dgiot_utils:hex_to_binary(dgiot_utils:trim_string(Payload))),
            {noreply, TCPState};
        _ ->
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
            end
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

handle_info({tcp, Buff}, #tcp{state = #state{product = Products, module = Module} = State} = TCPState) ->
    lists:map(fun(ProductId) ->
        dgiot_hook:run_hook({tcp, Module, ProductId}, {Buff, #{}, State#state{productid = ProductId}})
              end, Products),
    {noreply, TCPState};

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


do_product(Fun, Args, #tcp{state = #state{product = ProductIds, id = ChannelId, env = Env}} = TCPState) ->
    case dgiot_bridge:apply_channel(ChannelId, ProductIds, Fun, Args, Env#{<<"send">> => send_fun(TCPState)}) of
        {ok, NewEnv} ->
            {ok, update_state(NewEnv, TCPState)};
        {stop, Reason, NewEnv} ->
            {stop, Reason, update_state(NewEnv, TCPState)};
        {reply, ProductId, Reply, NewEnv} ->
            {reply, ProductId, Reply, update_state(NewEnv, TCPState)}
    end.

update_state(Env, #tcp{state = State} = TCPState) ->
    TCPState#tcp{state = State#state{env = maps:without([<<"send">>], Env)}}.

