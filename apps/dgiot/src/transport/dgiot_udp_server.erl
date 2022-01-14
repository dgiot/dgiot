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

-module(dgiot_udp_server).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(MAX_MESSAGE_ID, 65535). % 16-bit number

%% API
-export([start_link/5, child_spec/3, send/2]).

%% gen_server callbacks
-export([init/5, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, esockd_send/3, esockd_send_ok/3]).
-define(VERSION, 1).
-define(SERVER, ?MODULE).
-record(coap_message, {type, method, id, token = <<>>, options = [], payload = <<>>}).

%%-record(state, {mod, sock, chid, tokens, msgid_token, trans, nextmid, options, conn_state, active_n, incoming_bytes = 0, rate_limit, limit_timer, child = #udp{}}).
-record(state, {sock, chid, responder, options, mod, incoming_bytes = 0, child = #udp{}}).
%%-record(state, {sock, chid, tokens, msgid_token, trans, nextmid, responder, options}).
-define(SOCKOPTS, [binary, {reuseaddr, true}]).

child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).

child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
    {ok, DefActiveN, DefRateLimit, UDPOpts} = dgiot_transport:get_opts(udp, Port),
    ActiveN = proplists:get_value(active_n, Opts, DefActiveN),
    RateLimit = proplists:get_value(rate_limit, Opts, DefRateLimit),
    Opts1 = lists:foldl(fun(Key, Acc) -> proplists:delete(Key, Acc) end, Opts, [active_n, rate_limit]),
    NewOpts = [{active_n, ActiveN}, {rate_limit, RateLimit}] ++ Opts1,
    MFArgs = {?MODULE, start_link, [Mod, NewOpts, State]},
    esockd:udp_child_spec(Name, Port, UDPOpts, MFArgs).

%% udp
start_link(Socket = {udp, _SockPid, _Sock}, Sock, Mod, Opts, State) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Mod, Socket, Sock, Opts, State])};
%% dtls
start_link(esockd_transport, RawSock, Mod, Opts, State) ->
    Socket = {esockd_transport, RawSock},
    case esockd_transport:peername(RawSock) of
        {ok, Peername} ->
            {ok, proc_lib:spawn_link(?MODULE, init, [Mod, Socket, Peername, Opts, State])};
        R = {error, _} -> R
    end.

init(Mod, Socket, Sock, Opts, State) ->
    process_flag(trap_exit, true),
    case esockd_wait(Socket) of
        {ok, NSocket} ->
            ChildState = #udp{socket = Socket, sock = Sock, register = false, transport = esockd_transport, state = State},
            case Mod:init(ChildState) of
                {ok, NewChildState} ->
                    GState = #state{
                        sock = NSocket,
                        chid = Sock,
                        options = Opts,
                        mod = Mod,
                        child = NewChildState
                    },
                    dgiot_metrics:inc(dgiot_bridge, <<"udp_server">>, 1),
                    gen_server:enter_loop(?MODULE, [], GState);
                {error, Reason} ->
                    _ = esockd_close(Socket),
                    exit_on_sock_error(Reason)
            end;
        {error, Reason} ->
            _ = esockd_close(Socket),
            exit_on_sock_error(Reason)
    end.

handle_call(Request, From, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_call(Request, From, ChildState) of
        {reply, Reply, NewChildState} ->
            {reply, Reply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

handle_cast(Msg, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_cast(Msg, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

handle_info({datagram, _SockPid, Data}, State) ->
    handle_info({udp, _SockPid, Data}, State);


handle_info({ssl, _RawSock, Data}, State) ->
    handle_info({ssl, _RawSock, Data}, State);

%% add register function
handle_info({udp, _SockPid, Data}, #state{mod = Mod, child = #udp{register = false, buff = Buff, socket = Sock} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    write_log(ChildState#udp.log, <<"RECV">>, NewBin),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#udp{buff = <<>>},
    case Mod:handle_info({udp, <<Buff/binary, NewBin/binary>>}, NewChildState) of
        {noreply, #udp{register = true, clientid = ClientId, buff = Buff, socket = Sock} = NewChild} ->
            dgiot_cm:register_channel(ClientId, self(), #{conn_mod => Mod}),
            Ip = dgiot_utils:get_ip(Sock),
            Port = dgiot_utils:get_port(Sock),
            dgiot_cm:insert_channel_info(ClientId, #{ip => Ip, port => Port, online => dgiot_datetime:now_microsecs()}, [{udp_recv, 1}]),
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

handle_info({udp, Sock, Data}, #state{mod = Mod, child = #udp{buff = Buff, socket = Sock} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    write_log(ChildState#udp.log, <<"RECV">>, NewBin),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#udp{buff = <<>>},
    case NewChildState of
        #udp{clientid = CliendId, register = true} ->
            dgiot_device:online(CliendId),
            dgiot_tracer:check_trace(CliendId, CliendId, dgiot_utils:binary_to_hex(Binary), ?MODULE, ?LINE);
        _ ->
            pass
    end,
    case Mod:handle_info({udp, <<Buff/binary, NewBin/binary>>}, NewChildState) of
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

handle_info({shutdown, Reason}, #state{child = #udp{clientid = CliendId, register = true} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#udp.state]),
    dgiot_cm:unregister_channel(CliendId),
    dgiot_device:offline(CliendId),
    write_log(ChildState#udp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, normal, State#state{child = ChildState#udp{socket = undefined}}};

handle_info({shutdown, Reason}, #state{child = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#udp.state]),
    write_log(ChildState#udp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, normal, State#state{child = ChildState#udp{socket = undefined}}};


handle_info({udp_error, _Sock, Reason}, #state{child = ChildState} = State) ->
    ?LOG(error, "udp_error, ~p, ~p~n", [Reason, ChildState#udp.state]),
    write_log(ChildState#udp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, {shutdown, Reason}, State};

handle_info({udp_closed, Sock}, #state{mod = Mod, child = #udp{socket = Sock} = ChildState} = State) ->
    write_log(ChildState#udp.log, <<"ERROR">>, <<"udp_closed">>),
    ?LOG(error, "udp_closed ~p", [ChildState#udp.state]),
    case Mod:handle_info(udp_closed, ChildState) of
        {noreply, NewChild} ->
            {stop, normal, State#state{child = NewChild#udp{socket = undefined}}};
        {stop, _Reason, NewChild} ->
            {stop, normal, State#state{child = NewChild#udp{socket = undefined}}}
    end;

handle_info(Info, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end;

handle_info({timeout, _TrId, _Event}, State) ->
    {noreply, State, hibernate};

handle_info({request_complete, #coap_message{token = _Token, id = _Id}}, State) ->
    {noreply, State, hibernate};

handle_info({'EXIT', Resp, Reason}, State = #state{responder = Resp}) ->
    logger:info("channel received exit from responder: ~p, reason: ~p", [Resp, Reason]),
    {stop, Reason, State};

handle_info({'EXIT', _Pid, _Reason}, State = #state{}) ->
    logger:error("channel received exit from stranger: ~p, reason: ~p", [_Pid, _Reason]),
    {noreply, State, hibernate};

handle_info(Info, State) ->
    logger:warning("unexpected massage ~p~n", [Info]),
    {noreply, State, hibernate}.

terminate(Reason, #state{mod = Mod, child = #udp{clientid = CliendId, register = true} = ChildState}) ->
    dgiot_cm:unregister_channel(CliendId),
    dgiot_metrics:dec(dgiot_bridge, <<"udp_server">>, 1),
    Mod:terminate(Reason, ChildState);

terminate(Reason, #state{mod = Mod, child = ChildState}) ->
    dgiot_metrics:dec(dgiot_bridge, <<"udp_server">>, 1),
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#state{child = NewChildState}}.

%%--------------------------------------------------------------------
%% Handle datagram
%%--------------------------------------------------------------------



%%%===================================================================
%%% Internal functions
%%%===================================================================

send(#udp{clientid = CliendId, register = true, transport = Transport, socket = Socket}, Payload) ->
    dgiot_tracer:check_trace(CliendId, CliendId, dgiot_utils:binary_to_hex(Payload), ?MODULE, ?LINE),
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_send">>, 1),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end;

send(#udp{transport = Transport, socket = Socket}, Payload) ->
    dgiot_metrics:inc(dgiot_bridge, <<"udp_server_send">>, 1),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end.

%%--------------------------------------------------------------------
%% Wrapped codes for esockd udp/dtls

-spec exit_on_sock_error(_) -> no_return().
exit_on_sock_error(Reason) when Reason =:= einval;
    Reason =:= enotconn;
    Reason =:= closed ->
    erlang:exit(normal);
exit_on_sock_error(timeout) ->
    erlang:exit({shutdown, ssl_upgrade_timeout});
exit_on_sock_error(Reason) ->
    erlang:exit({shutdown, Reason}).

esockd_wait(Socket = {udp, _SockPid, _Sock}) ->
    {ok, Socket};
esockd_wait({esockd_transport, Sock}) ->
    case esockd_transport:wait(Sock) of
        {ok, NSock} -> {ok, {esockd_transport, NSock}};
        R = {error, _} -> R
    end.

esockd_send_ok(Socket, Dest, Data) ->
    _ = esockd_send(Socket, Dest, Data),
    ok.

esockd_send({udp, _SockPid, Sock}, {Ip, Port}, Data) ->
    gen_udp:send(Sock, Ip, Port, Data);
esockd_send({esockd_transport, Sock}, {_Ip, _Port}, Data) ->
    esockd_transport:async_send(Sock, Data).

esockd_close({udp, _SockPid, Sock}) ->
    gen_udp:close(Sock);
esockd_close({esockd_transport, Sock}) ->
    esockd_transport:fast_close(Sock).


write_log(file, Type, Buff) ->
    [Pid] = io_lib:format("~p", [self()]),
    Date = dgiot_datetime:format("YYYY-MM-DD"),
    Path = <<"log/tcp_server/", Date/binary, ".txt">>,
    filelib:ensure_dir(Path),
    Time = dgiot_datetime:format("HH:NN:SS " ++ Pid),
    Data = case Type of
               <<"ERROR">> -> Buff;
               _ -> <<<<Y>> || <<X:4>> <= Buff, Y <- integer_to_list(X, 16)>>
           end,
    file:write_file(Path, <<Time/binary, " ", Type/binary, " ", Data/binary, "\r\n">>, [append]),
    ok;
write_log({Mod, Fun}, Type, Buff) ->
    catch apply(Mod, Fun, [Type, Buff]);
write_log(Fun, Type, Buff) when is_function(Fun) ->
    catch Fun(Type, Buff);
write_log(_, _, _) ->
    ok.
