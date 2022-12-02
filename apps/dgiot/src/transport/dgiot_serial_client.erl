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
-module(dgiot_serial_client).
-behaviour(gen_server).
-dgiot_data("ets").
-define(SERIAL, dgiot_serial_ets).
-export([
    init_ets/0,
    open/1, open/2,
    close/1,

    getfd/1,

    read/2,
    write/2,
    send/2,

    controlling_process/2
]).

-export([start_link/2]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    % Original termios attributes
    oattr,
    port,
    % PID of controlling process
    pid,
    % serial dev file descriptor
    fd,
    % device name
    dev,
    speed,
    % interval (Unit: millisecond)
    interval = 50 :: integer(), %% b2400
    %% Timestamp (Unit: millisecond) 收包时间
    timestamp = 0 :: integer(),
    %% Message from
    data = <<>> :: binary(),
    %% 累计收包数
    package_recv_count = 0 :: integer(),
    %% 累计发包数
    package_send_count = 0 :: integer()
}).

init_ets() ->
    dgiot_data:init(?SERIAL).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
open(Dev) ->
    open(Dev, []).

open(Dev, Opt) ->
    start_link(Dev, Opt).

close(Ref) when is_pid(Ref) ->
    catch gen_server:call(Ref, close, infinity),
    ok;

close(Ref) ->
    serctl:close(Ref),
    ok.

getfd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, fd, infinity).

read(FD, Len) when is_integer(Len) ->
    serctl:read(FD, Len).

write(Serialport, Data) ->
    case dgiot_data:get(?SERIAL, Serialport) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    gen_server:call(Pid, {write, Data}, infinity);
                false ->
                    pass
            end;
        _ ->
            pass
    end.

send(Ref, Data) when is_pid(Ref) ->
    gen_server:call(Ref, {send, Data}, infinity).

% FIXME: race condition: events can be delivered out of order
controlling_process(Ref, Pid) when is_pid(Ref), is_pid(Pid) ->
    gen_server:call(Ref, {controlling_process, Pid}, infinity),
    flush_events(Ref, Pid).

start_link(Dev, Opt) ->
    ParentPid = self(),
    gen_server:start_link(?MODULE, [ParentPid, Dev, Opt], []).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([ParentPid, Serialport, Opt]) ->
%%    io:format("~s ~p Serialport = ~p.~n", [?FILE, ?LINE, Serialport]),
    dgiot_data:insert(?SERIAL, Serialport, self()),
    process_flag(trap_exit, true),
    BSpeed = proplists:get_value(speed, Opt, b9600),
    Flow = proplists:get_value(flow, Opt, true),
    PortOpt = proplists:get_value(port_options, Opt, [stream, binary]),

    Dev = <<"/dev/", Serialport/binary>>,
    {ok, FD} = serctl:open(Dev),

    {ok, Orig} = serctl:tcgetattr(FD),

    Mode =
        case proplists:get_value(mode, Opt, raw) of
            raw -> serctl:mode(raw);
            none -> Orig
        end,
    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        Mode,
        [
            fun(N) -> serctl:flow(N, Flow) end,
            fun(N) -> serctl:ispeed(N, BSpeed) end,
            fun(N) -> serctl:ospeed(N, BSpeed) end
        ]
    ),
    <<"b", Speed/binary>> = dgiot_utils:to_binary(BSpeed),

    ok = serctl:tcsetattr(FD, tcsanow, Termios),
    ParentPid ! {serial_open, #{<<"pid">> => ParentPid, <<"fd">> => FD}},
    {ok, #state{
        oattr = Orig,
        speed = dgiot_utils:to_int(Speed),
        port = set_active(FD, PortOpt),
        pid = ParentPid,
        fd = FD,
        dev = Dev
    }}.

%%
%% retrieve/modify gen_server state
%%
handle_call(devname, _From, #state{dev = Dev} = State) ->
    {reply, Dev, State};
handle_call(fd, _From, #state{fd = FD} = State) ->
    {reply, FD, State};
handle_call({send, Data}, _From, #state{port = Port} = State) ->
    Reply =
        try erlang:port_command(Port, Data) of
            true -> ok
        catch
            error:Error -> {error, Error}
        end,
    {reply, Reply, State};
handle_call({write, Data}, _From, #state{fd = FD, package_send_count = Package_send_count} = State) ->
    try serctl:write(FD, Data) of
        ok ->
            {reply, ok, State#state{package_send_count = Package_send_count + 1}}
    catch
        error:Error ->
            {reply, {error, Error}, State}
    end;

handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call({controlling_process, Pid}, {Owner, _}, #state{pid = Owner} = State) ->
    link(Pid),
    unlink(Owner),
    {reply, ok, State#state{pid = Pid}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% {active, true} mode
handle_info({Port, {data, FirstData}}, #state{port = Port, speed = Speed, data = <<>>} = State) when Speed < 9600 ->
    Ms = dgiot_datetime:now_ms(),
    erlang:send_after(100, self(), timeout),
    {noreply, State#state{timestamp = Ms, data = iolist_to_binary([FirstData])}};

handle_info({Port, {data, Data}}, #state{port = Port, speed = Speed, data = OldData} = State) when Speed < 9600 ->
    Ms = dgiot_datetime:now_ms(),
    {noreply, State#state{timestamp = Ms, data = iolist_to_binary([OldData | Data])}};

handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    {noreply, send(Pid, Data, State)};

handle_info(timeout, #state{pid = ParentPid, data = NowData, timestamp = Timestamp} = State) ->
    Ms = dgiot_datetime:now_ms(),
    case Ms - Timestamp of
        Interval when Interval > 15 ->
            {noreply, send(ParentPid, NowData, State)};
        _ ->
            erlang:send_after(100, self(), timeout),
            {noreply, State}
    end;

% port has closed
handle_info({'EXIT', Port, _Reason}, #state{port = Port} = State) ->
    {stop, shutdown, State};
% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = FD, port = Port, oattr = Orig}) ->
    catch erlang:port_close(Port),
    _ = serctl:tcsetattr(FD, tcsanow, Orig),
    _ = serctl:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_active(Res, Opt) ->
    FD = serctl:getfd(Res),
    erlang:open_port({fd, FD, FD}, Opt).

flush_events(Ref, Pid) ->
    receive
        {serial, Ref, _} = Event ->
            Pid ! Event,
            flush_events(Ref, Pid)
    after 0 -> ok
    end.

send(ParentPid, Data, #state{package_recv_count = Recv_count} = State) ->
    ParentPid ! {serial_data, self(), Data},
    State#state{data = <<>>, package_recv_count = Recv_count + 1}.
