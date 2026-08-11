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
-define(PRINT(Format, Args), io:format(Format, Args)).   %% 添加调试打印宏

-record(state, {mod, conn_state, active_n, incoming_bytes = 0, rate_limit, limit_timer, child = #tcp{}}).

%% 强制转换为二进制，并记录错误
ensure_binary(undefined) -> 
    ?LOG(info, "ensure_binary received undefined from ~p", [self()]),
    <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Other) -> 
    ?LOG(info, "ensure_binary received non-binary: ~p from ~p", [Other, self()]),
    <<>>.

child_spec(Mod, Port, State) ->
    child_spec(Mod, Port, State, []).

child_spec(Mod, Port, State, Opts) ->
    Name = Mod,
    ok = esockd:start(),
    case dgiot_transport:get_opts(tcp, Port) of
        {ok, DefActiveN, DefRateLimit, TCPOpts} ->
            ActiveN = proplists:get_value(active_n, Opts, DefActiveN),
            RateLimit = proplists:get_value(rate_limit, Opts, DefRateLimit),
            Opts1 = lists:foldl(fun(Key, Acc) -> proplists:delete(Key, Acc) end, Opts, [active_n, rate_limit]),
            NewOpts = [{active_n, ActiveN}, {rate_limit, RateLimit}] ++ Opts1,
            MFArgs = {?MODULE, start_link, [Mod, NewOpts, State]},
            esockd:child_spec(Name, Port, TCPOpts, MFArgs);
        _ ->
            []
    end.

start_link(Transport, Sock, Mod, Opts, State) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Mod, Transport, Opts, Sock, State])}.

init(Mod, Transport, Opts, Sock0, State) ->
    case Transport:wait(Sock0) of
        {ok, Sock} ->
            dgiot_metrics:inc(dgiot, <<"tcp_online">>, 1),
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
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {reply, Reply, State#state{child = SafeNewChild}, hibernate};
        {stop, Reason, NewChildState} ->
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {stop, Reason, State#state{child = SafeNewChild}}
    end.

handle_cast(Msg, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_cast(Msg, ChildState) of
        {noreply, NewChildState} ->
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {noreply, State#state{child = SafeNewChild}, hibernate};
        {stop, Reason, NewChildState} ->
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {stop, Reason, State#state{child = SafeNewChild}}
    end.

handle_info(activate_socket, State) ->
    NewState = State#state{limit_timer = undefined, conn_state = running},
    ok = activate_socket(NewState),
    {noreply, NewState, hibernate};

handle_info({tcp_passive, _Sock}, State) ->
    NState = ensure_rate_limit(State),
    ok = activate_socket(NState),
    {noreply, NState};

%% add register function (first data)
handle_info({tcp, Sock, Data}, #state{mod = Mod, child = #tcp{clientid = Clientid, register = false, buff = Buff, socket = Sock} = ChildState} = State) ->
   
    dgiot_metrics:inc(dgiot, <<"tcp_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    DTUIP = case dgiot_utils:get_ip(Sock) of
        <<"">> -> <<"unknown_ip">>;
        IPAddr -> IPAddr
    end,
    % 确保 Clientid 是二进制，否则可能引起 write_log 内部问题，但 write_log 会处理
    SafeClientid = ensure_binary(Clientid),
    write_log(ChildState#tcp.log, <<" RECV ", DTUIP/binary, " ", SafeClientid/binary>>, NewBin),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#tcp{buff = <<>>},
    SafeBuff = ensure_binary(Buff),
    SafeNewBin = ensure_binary(NewBin),
    Merged = <<SafeBuff/binary, SafeNewBin/binary>>,
    SafeMerged = ensure_binary(Merged),
    case Mod:handle_info({tcp, SafeMerged}, NewChildState) of
        {noreply, #tcp{register = true, clientid = ClientId, buff = _NewBuff, socket = Sock} = NewChild} ->
            dgiot_cm:register_channel(ClientId, self(), #{conn_mod => Mod}),
            % 安全获取IP地址和端口，处理可能的socket错误
            Ip = case dgiot_utils:get_ip(Sock) of
                <<"">> -> <<"unknown_ip">>;
                IPAddr1 -> IPAddr1
            end,
            Port = case dgiot_utils:get_port(Sock) of
                0 -> 0;
                P -> P
            end,
            %% 打印TCP连接信息
            ?LOG(info, "TCP新连接: ClientId=~ts, IP=~ts, Port=~p, Socket=~p, Module=~p",
                  [ClientId, Ip, Port, Sock, Mod]),
            dgiot_cm:insert_channel_info(ClientId, #{ip => Ip, port => Port, online => dgiot_datetime:now_microsecs()}, [{tcp_recv, 1}]),
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {noreply, State#state{child = SafeNewChild, incoming_bytes = Cnt}, hibernate};
        {noreply, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {noreply, State#state{child = SafeNewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {stop, Reason, State#state{child = SafeNewChild}}
    end;

%% handle_info for registered devices (后续数据)
handle_info({tcp, Sock, Data}, #state{mod = Mod, child = #tcp{clientid = Clientid, buff = Buff, socket = Sock} = ChildState} = State) ->
    dgiot_metrics:inc(dgiot, <<"tcp_recv">>, 1),
    Binary = iolist_to_binary(Data),
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    DTUIP = case dgiot_utils:get_ip(Sock) of
        <<"">> -> <<"unknown_ip">>;
        IPAddr2 -> IPAddr2
    end,
    SafeClientid = ensure_binary(Clientid),
    write_log(ChildState#tcp.log, <<"RECV ", DTUIP/binary, " ", SafeClientid/binary>>, NewBin),
    Cnt = byte_size(NewBin),
    NewChildState = ChildState#tcp{buff = <<>>},
    case NewChildState of
        #tcp{clientid = CliendId, register = true} ->
            dgiot_device:online(CliendId),
            dgiot_tracer:check_trace(CliendId, CliendId, dgiot_utils:binary_to_hex(Binary), ?MODULE, ?LINE);
        _ -> pass
    end,
    SafeBuff = ensure_binary(Buff),
    SafeNewBin = ensure_binary(NewBin),
    Merged = <<SafeBuff/binary, SafeNewBin/binary>>,
    SafeMerged = ensure_binary(Merged),
    Result = Mod:handle_info({tcp, SafeMerged}, NewChildState),
    case Result of
        {noreply, #tcp{buff = _NewBuff} = NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {noreply, State#state{child = SafeNewChild, incoming_bytes = Cnt}, hibernate};
        {noreply, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {noreply, State#state{child = SafeNewChild, incoming_bytes = Cnt}, hibernate};
        {stop, Reason, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {stop, Reason, State#state{child = SafeNewChild}}
    end;

handle_info({shutdown, Reason}, #state{child = #tcp{clientid = CliendId, socket = Sock, register = true} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    case CliendId of
        undefined -> ok;
        _ ->
            dgiot_cm:unregister_channel(CliendId),
            dgiot_device:offline(CliendId)
    end,
    DTUIP = case dgiot_utils:get_ip(Sock) of
        <<"">> -> <<"unknown_ip">>;
        IPAddr3 -> IPAddr3
    end,
    case CliendId of
        undefined -> ok;
        _ -> write_log(ChildState#tcp.log, <<"ERROR ", DTUIP/binary, " ", CliendId/binary>>, list_to_binary(io_lib:format("~w", [Reason])))
    end,
    {stop, normal, State#state{child = ChildState#tcp{socket = undefined}}};

handle_info({shutdown, Reason}, #state{child = #tcp{clientid = Clientid, socket = Sock} = ChildState} = State) ->
    ?LOG(error, "shutdown, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    DTUIP = case dgiot_utils:get_ip(Sock) of
        <<"">> -> <<"unknown_ip">>;
        IPAddr4 -> IPAddr4
    end,
    write_log(ChildState#tcp.log, <<"ERROR ", DTUIP/binary, " ", Clientid/binary>>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, normal, State#state{child = ChildState#tcp{socket = undefined}}};

handle_info({tcp_error, _Sock, Reason}, #state{child = #tcp{clientid = Clientid, socket = Sock} = ChildState} = State) ->
    ?LOG(error, "tcp_error, ~p, ~p~n", [Reason, ChildState#tcp.state]),
    DTUIP = case dgiot_utils:get_ip(Sock) of
        <<"">> -> <<"unknown_ip">>;
        IPAddr5 -> IPAddr5
    end,
    write_log(ChildState#tcp.log, <<"ERROR ", DTUIP/binary, " ", Clientid/binary>>, list_to_binary(io_lib:format("~w", [Reason]))),
    {stop, {shutdown, Reason}, State};

handle_info({tcp_closed, Sock}, #state{mod = Mod, child = #tcp{clientid = _Clientid, socket = Sock} = ChildState} = State) ->
    % DTUIP = case dgiot_utils:get_ip(Sock) of
    %     <<"">> -> <<"unknown_ip">>;
    %     IPAddr6 -> IPAddr6
    % end,
    % SafeClientid = ensure_binary(Clientid),
    % write_log(ChildState#tcp.log, <<"ERROR ", DTUIP/binary, " ", SafeClientid/binary>>, <<"tcp_closed">>),
    dgiot_metrics:dec(dgiot, <<"tcp_online">>, 1),
    case Mod:handle_info(tcp_closed, ChildState) of
        {noreply, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {stop, normal, State#state{child = SafeNewChild#tcp{socket = undefined}}};
        {stop, _Reason, NewChild} ->
            SafeNewChild = NewChild#tcp{buff = ensure_binary(NewChild#tcp.buff)},
            {stop, normal, State#state{child = SafeNewChild#tcp{socket = undefined}}}
    end;

handle_info(Info, #state{mod = Mod, child = ChildState} = State) ->
    case Mod:handle_info(Info, ChildState) of
        {noreply, NewChildState} ->
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {noreply, State#state{child = SafeNewChild}, hibernate};
        {stop, Reason, NewChildState} ->
            SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
            {stop, Reason, State#state{child = SafeNewChild}}
    end.

terminate(Reason, #state{mod = Mod, child = #tcp{clientid = CliendId, register = true} = ChildState}) ->
    case CliendId of
        undefined -> ok;
        _ -> dgiot_cm:unregister_channel(CliendId)
    end,
    dgiot_metrics:dec(dgiot_bridge, <<"tcp_server">>, 1),
    Mod:terminate(Reason, ChildState);
terminate(Reason, #state{mod = Mod, child = ChildState}) ->
    dgiot_metrics:dec(dgiot_bridge, <<"tcp_server">>, 1),
    Mod:terminate(Reason, ChildState).

code_change(OldVsn, #state{mod = Mod, child = ChildState} = State, Extra) ->
    {ok, NewChildState} = Mod:code_change(OldVsn, ChildState, Extra),
    SafeNewChild = NewChildState#tcp{buff = ensure_binary(NewChildState#tcp.buff)},
    {ok, State#state{child = SafeNewChild}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(#tcp{clientid = CliendId, register = true, transport = Transport, socket = Socket} = ChildState, Payload) ->
    SafePayload = ensure_binary(Payload),
    case SafePayload of
        <<>> -> ?LOG(error, "send called with empty/undefined payload from ~p, clientid=~s", [self(), CliendId]);
        _ -> ok
    end,
    dgiot_tracer:check_trace(CliendId, CliendId, dgiot_utils:binary_to_hex(SafePayload), ?MODULE, ?LINE),
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_send">>, 1),
    case Socket == undefined of
        true -> {error, disconnected};
        false ->
            DTUIP = case dgiot_utils:get_ip(Socket) of
                <<"">> -> <<"unknown_ip">>;
                IPAddr7 -> IPAddr7
            end,
            write_log(ChildState#tcp.log, <<"send ", DTUIP/binary, " ", CliendId/binary>>, SafePayload),
            Transport:send(Socket, SafePayload)
    end;

send(#tcp{clientid = Clientid, transport = Transport, socket = Socket} = ChildState, Payload) ->
    SafePayload = ensure_binary(Payload),
    case SafePayload of
        <<>> -> ?LOG(error, "send called with empty/undefined payload from ~p, clientid=~s", [self(), Clientid]);
        _ -> ok
    end,
    dgiot_metrics:inc(dgiot_bridge, <<"tcp_server_send">>, 1),
    case Socket == undefined of
        true -> {error, disconnected};
        false ->
            % 安全获取IP地址，处理可能的socket错误
            DTUIP = case dgiot_utils:get_ip(Socket) of
                <<"">> -> <<"unknown_ip">>;
                IPAddr8 -> IPAddr8
            end,
            write_log(ChildState#tcp.log, <<"send ", DTUIP/binary, " ", Clientid/binary>>, Payload),
            Transport:send(Socket, SafePayload)
    end.

rate_limit({Rate, Burst}) ->
    esockd_rate_limit:new(Rate, Burst).

activate_socket(#state{conn_state = blocked}) -> ok;
activate_socket(#state{child = #tcp{transport = Transport, socket = Socket}, active_n = N}) ->
    TrueOrN = case Transport:is_ssl(Socket) of true -> true; false -> N end,
    case Transport:setopts(Socket, [{active, TrueOrN}]) of
        ok -> ok;
        {error, Reason} -> self() ! {shutdown, Reason}, ok
    end.

ensure_rate_limit(State) ->
    case esockd_rate_limit:check(State#state.incoming_bytes, State#state.rate_limit) of
        {0, RateLimit} -> State#state{incoming_bytes = 0, rate_limit = RateLimit};
        {Pause, RateLimit} ->
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
write_log({Mod, Fun}, Type, Buff) -> catch apply(Mod, Fun, [Type, Buff]);
write_log(Fun, Type, Buff) when is_function(Fun) -> catch Fun(Type, Buff);
write_log(_, _, _) -> ok.