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

-module(dgiot_tcp_server).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start_link/5, child_spec/3, child_spec/4, send/2]).

%% gen_server callbacks
-export([init/5, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mod, conn_state, active_n, incoming_bytes = 0, rate_limit, limit_timer, child = #tcp{}}).


child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).


child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
    {ok, DefActiveN, DefRateLimit, TCPOpts} = dgiot_transport:get_opts(tcp, Port),
    ActiveN = proplists:get_value(active_n, Opts, DefActiveN),
    RateLimit = proplists:get_value(rate_limit, Opts, DefRateLimit),
    Opts1 = lists:foldl(fun(Key, Acc) -> proplists:delete(Key, Acc) end, Opts, [active_n, rate_limit]),
    NewOpts = [{active_n, ActiveN}, {rate_limit, RateLimit}] ++ Opts1,
    MFArgs = {?MODULE, start_link, [Mod, NewOpts, State]},
    esockd:child_spec(Name, Port, TCPOpts, MFArgs).

start_link(Transport, Sock, Mod, Opts, State) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Mod, Transport, Opts, Sock, State])}.

init(Mod, Transport, Opts, Sock0, State) ->
    case Transport:wait(Sock0) of
        {ok, Sock} ->
            ChildState = #tcp{socket = Sock, register = false, transport = Transport, state = State},
            case Mod:init(ChildState) of
                {ok, NewChildState} ->
                    GState = #state{
                        mod = Mod,
                        conn_state = running,
                        active_n = proplists:get_value(active_n, Opts, 8),
                        rate_limit = rate_limit(proplists:get_value(rate_limit, Opts)),
                        child = NewChildState
                    },
                    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server">>, 1),
                    ok = activate_socket(GState),
                    gen_server:enter_loop(?MODULE, [], GState);
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
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

handle_info(activate_socket, State) ->
    NewState = State#state{limit_timer = undefined, conn_state = running},
    ok = activate_socket(NewState),
    {noreply, NewState, hibernate};

handle_info({tcp_passive, _Sock}, State) ->
    NState = ensure_rate_limit(State),
    ok = activate_socket(NState),
    {noreply, NState};

%% add register function
handle_info({tcp, Sock, Data}, #state{mod = Mod, child = #tcp{register = false, buff = Buff, socket = Sock} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    write_log(ChildState#tcp.log, <<"RECV">>, NewBin),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#tcp{buff = <<>>},
    case Mod:handle_info({tcp, <<Buff/binary, NewBin/binary>>}, NewChildState) of
        {noreply, #tcp{register = true, clientid = ClientId, buff = Buff, socket = Sock} = NewChild} ->
            dgiot_cm:register_channel(ClientId, self(), #{conn_mod => Mod}),
            Ip = dgiot_utils:get_ip(Sock),
            Port = dgiot_utils:get_port(Sock),
            dgiot_cm:insert_channel_info(ClientId, #{ip => Ip, port => Port, online => dgiot_datetime:now_microsecs()}, [{tcp_recv, 1}]),
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

handle_info({tcp, Sock, Data}, #state{mod = Mod, child = #tcp{buff = Buff, socket = Sock} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    write_log(ChildState#tcp.log, <<"RECV">>, NewBin),
    ?LOG(info, "ChildState ~p", [ChildState]),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#tcp{buff = <<>>},
    case NewChildState of
        #tcp{clientid = CliendId, register = true} ->
            dgiot_tracer:check_trace(CliendId, CliendId, Binary, ?MODULE, ?LINE);
        _ ->
            pass
    end,
    case Mod:handle_info({tcp, <<Buff/binary, NewBin/binary>>}, NewChildState) of
        {noreply, NewChild} ->
            {noreply, State#state{child = NewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            {stop, Reason, State#state{child = NewChild}}
    end;

handle_info({shutdown, Reason}, #state{child = #tcp{clientid = CliendId, register = true} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    dgiot_cm:unregister_channel(CliendId),
    write_log(ChildState#tcp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, normal, State#state{child = ChildState#tcp{socket = undefined}}};

handle_info({shutdown, Reason}, #state{child = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    write_log(ChildState#tcp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, normal, State#state{child = ChildState#tcp{socket = undefined}}};


handle_info({tcp_error, _Sock, Reason}, #state{child = ChildState} = State) ->
    ?LOG(error, "tcp_error, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    write_log(ChildState#tcp.log, <<"ERROR">>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, {shutdown, Reason}, State};

handle_info({tcp_closed, Sock}, #state{mod = Mod, child = #tcp{socket = Sock} = ChildState} = State) ->
    write_log(ChildState#tcp.log, <<"ERROR">>, <<"tcp_closed">>),
    ?LOG(error, "tcp_closed ~p", [ChildState#tcp.state]),
    case Mod:handle_info(tcp_closed, ChildState) of
        {noreply, NewChild} ->
            {stop, normal, State#state{child = NewChild#tcp{socket = undefined}}};
        {stop, _Reason, NewChild} ->
            {stop, normal, State#state{child = NewChild#tcp{socket = undefined}}}
    end;

handle_info(Info, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            {noreply, State#state{child = NewChildState}, hibernate};
        {stop, Reason, NewChildState} ->
            {stop, Reason, State#state{child = NewChildState}}
    end.

terminate(Reason, #state{mod = Mod, child = #tcp{clientid = CliendId, register = true} = ChildState}) ->
    dgiot_cm:unregister_channel(CliendId),
    dgiot_metrics:dec(dgiot_bridge, <<"tcp_server">>, 1),
    Mod:terminate(Reason, ChildState);

terminate(Reason, #state{mod = Mod, child = ChildState}) ->
    dgiot_metrics:dec(dgiot_bridge, <<"tcp_server">>, 1),
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    {ok, State#state{child = NewChildState}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(#tcp{clientid = CliendId, register = true, transport = Transport, socket = Socket, log = Log}, Payload) ->
    dgiot_tracer:check_trace(CliendId, CliendId, Payload, ?MODULE, ?LINE),
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_send">>, 1),
    write_log(Log, <<"SEND">>, Payload),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end;

send(#tcp{transport = Transport, socket = Socket, log = Log}, Payload) ->
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_send">>, 1),
    write_log(Log, <<"SEND">>, Payload),
    case Socket == undefined of
        true ->
            {error, disconnected};
        false ->
            Transport:send(Socket, Payload)
    end.


rate_limit({Rate, Burst}) ->
    esockd_rate_limit:new(Rate, Burst).

activate_socket(#state{conn_state = blocked}) ->
    ok;
activate_socket(#state{child = #tcp{transport = Transport, socket = Socket}, active_n = N}) ->
    TrueOrN =
        case Transport:is_ssl(Socket) of
            true -> true; %% Cannot set '{active, N}' for SSL:(
            false -> N
        end,
    case Transport:setopts(Socket, [{active, TrueOrN}]) of
        ok -> ok;
        {error, Reason} ->
            self() ! {shutdown, Reason},
            ok
    end.

ensure_rate_limit(State) ->
    case esockd_rate_limit:check(State#state.incoming_bytes, State#state.rate_limit) of
        {0, RateLimit} ->
            State#state{incoming_bytes = 0, rate_limit = RateLimit};
        {Pause, RateLimit} ->
            %?LOG(info,"[~p] ensure_rate_limit :~p", [Pause, ensure_rate_limit]),
            TRef = erlang:send_after(Pause, self(), activate_socket),
            State#state{conn_state = blocked, incoming_bytes = 0, rate_limit = RateLimit, limit_timer = TRef}
    end.


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
