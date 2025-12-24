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
-include("dgiot_edge.hrl").
-export([
    init_ets/0,
    open/2,
    close/1,

    getfd/1,

    read/2,
    write/3,
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

init_ets() ->
    dgiot_data:init(?SERIAL).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
%% Opt {"baudrate":"b2400","checkbit":"EVEN","connectiontype":"tcp","databit":"8","flowcontrol":"NONE","ip":"127.0.0.1","port":61888,"stopbit":"1"}
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

write(ParentPid, Serialport, Data) ->
    case dgiot_data:get(?SERIAL, Serialport) of
        {Pid, _} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pubtopic = <<"$dg/user/edge/", Serialport/binary>>,
                    NewData = dgiot_edge:convert_data(dgiot_data:get({send_messagetype, Serialport}), Data),
                    BinParentPid = dgiot_utils:to_binary(ParentPid),
                    dgiot_mqtt:publish(self(), Pubtopic, <<BinParentPid/binary, "SEND ", NewData/binary>>),
                    gen_server:call(Pid, {write, ParentPid, Data}, infinity);
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
    case os:type() of
        {win32, _} ->
            pass;
        _ ->
            ParentPid = self(),
            gen_server:start_link(?MODULE, [ParentPid, Dev, Opt], [])
    end.

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([ParentPid, Serialport, Opt]) ->
%%    io:format("~s ~p Serialport = ~p.~n", [?FILE, ?LINE, Serialport]),
    case dgiot_data:get(?SERIAL, Serialport) of
        {Pid, FD} when is_pid(Pid) ->
            case serctl:tcgetattr(FD) of
                {ok, Orig} ->
                    dgiot_data:insert(?SERIAL, Serialport, {self(), FD}),
                    ParentPid ! {serial_open, #{<<"pid">> => ParentPid, <<"fd">> => FD}},
                    Dev = <<"/dev/", Serialport/binary>>,
                    PortOpt = maps:get(<<"port_options">>, Opt, [stream, binary]),
                    <<"b", Speed/binary>> = maps:get(<<"baudrate">>, Opt, <<"b9600">>),
                    {ok, #state{
                        oattr = Orig,
                        serialport = Serialport,
                        speed = dgiot_utils:to_int(Speed),
                        port = set_active(FD, PortOpt),
                        pid = ParentPid,
                        fd = FD,
                        dev = Dev,
                        env = Opt
                    }};
                _ ->
                    init(open, [ParentPid, Serialport, Opt])
            end;
        _ ->
            init(open, [ParentPid, Serialport, Opt])
    end.

init(open, [ParentPid, Serialport, Opt]) ->
    process_flag(trap_exit, true),
    BSpeed = dgiot_utils:to_atom(maps:get(<<"baudrate">>, Opt, b9600)),
    Flow = dgiot_utils:to_atom(maps:get(<<"flow">>, Opt, false)),
    PortOpt = maps:get(<<"port_options">>, Opt, [stream, binary]),
    Dev = <<"/dev/", Serialport/binary>>,
%%    io:format("~s ~p Dev = ~p.~n", [?FILE, ?LINE, Dev]),
    case serctl:open(Dev) of
        {ok, FD} ->
            dgiot_data:insert(?SERIAL, Serialport, {self(), FD}),
            {ok, Orig} = serctl:tcgetattr(FD),
            Defult_mode =
                case maps:get(<<"mode">>, Opt, raw) of
                    raw -> defult_mode();
                    none -> Orig
                end,
            Termios = lists:foldl(
                fun(Fun, Acc) -> Fun(Acc) end,
                Defult_mode,
                [
                    fun(N) -> serctl:flow(N, Flow) end,
                    fun(N) -> serctl:ispeed(N, BSpeed) end,
                    fun(N) -> serctl:ospeed(N, BSpeed) end
                ]
            ),
            <<"b", Speed/binary>> = dgiot_utils:to_binary(BSpeed),
            NewTermios =
                maps:fold(fun(K, V, Acc) ->
                    setflag(Acc, #{K => V})
                          end, Termios, Opt),
            ok = serctl:tcsetattr(FD, tcsanow, NewTermios),
            ParentPid ! {serial_open, #{<<"pid">> => ParentPid, <<"fd">> => FD}},
            {ok, #state{
                oattr = Orig,
                serialport = Serialport,
                speed = dgiot_utils:to_int(Speed),
                port = set_active(FD, PortOpt),
                pid = ParentPid,
                fd = FD,
                dev = Dev,
                env = Opt
            }};
        _Error ->
            ParentPid ! {serial_open_error, #{<<"pid">> => ParentPid}},
            Pubtopic = <<"$dg/user/edge/", Serialport/binary>>,
            dgiot_mqtt:publish(self(), Pubtopic, <<" OPEN FAILURE">>),
            {ok, #state{
                pid = ParentPid,
                dev = Dev,
                env = Opt
            }}
    end.

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
handle_call({write, ParentPid, Data}, _From, #state{fd = FD, serialport = Serialport} = State) ->
%%    io:format("~s ~p FD = ~p, Data = ~p.~n", [?FILE, ?LINE, FD, dgiot_utils:binary_to_hex(Data)]),
    try serctl:write(FD, Data) of
        ok ->
            Size = size(Data),
            dgiot_metrics:inc(dgiot_edge, <<"edge_send">>, 1),
            dgiot_metrics:inc(dgiot_edge, <<"edge_send_bytes">>, Size),
            dgiot_metrics:inc(dgiot_edge, <<Serialport/binary, "_send">>, 1),
            dgiot_metrics:inc(dgiot_edge, <<Serialport/binary, "_send_bytes">>, Size),
            {reply, ok, State#state{pid = ParentPid}}
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
handle_info({Port, {data, FirstData}}, #state{port = Port, data = <<>>} = State) ->
    {noreply, State#state{timestamp = dgiot_datetime:now_ms(), data = iolist_to_binary([FirstData]), ref = erlang:send_after(200, self(), timeout)}};

handle_info({Port, {data, NewData}}, #state{pid = ParentPid, port = Port, speed = Speed, data = LastData, timestamp = Timestamp, ref = Ref} = State) ->
    Ms = dgiot_datetime:now_ms(),
    Interval = get_interval(size(NewData), dgiot_utils:to_int(Speed)),
%%    io:format("~s ~p Speed ~p Interval ~p size ~p  (Ms - Timestamp) = ~p.~n", [?FILE, ?LINE,Speed, Interval, size(NewData), Ms - Timestamp]),
    case (Ms - Timestamp) > Interval of
        true ->
%%             取消超时定时器
            case Ref of
                undefined ->
                    pass;
                _ -> erlang:cancel_timer(Ref)
            end,
%%            io:format("~s ~p size ~p LastData = ~p.~n", [?FILE, ?LINE, size(LastData), LastData]),
            {noreply, send(ParentPid, NewData, LastData, State#state{ref = erlang:send_after(200, self(), timeout)})};
        _ ->
            {noreply, State#state{timestamp = Ms, data = iolist_to_binary([LastData | NewData])}}
    end;

handle_info({Port, {data, NewData}}, #state{port = Port, data = LastData, pid = Pid} = State) ->
    {noreply, send(Pid, NewData, LastData, State)};

handle_info(timeout, #state{pid = ParentPid, data = LastData, speed = Speed, timestamp = Timestamp} = State) ->
    Ms = dgiot_datetime:now_ms(),
    Interval = get_interval(size(LastData), dgiot_utils:to_int(Speed)),
    case (Ms - Timestamp) > Interval of
        true ->
            {noreply, send(ParentPid, <<>>, LastData, State)};
        _ ->
            {noreply, State#state{ref = erlang:send_after(200, self(), timeout)}}
    end;

% port has closed
handle_info({'EXIT', Port, _Reason}, #state{port = Port} = State) ->
    {stop, shutdown, State};
% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = undefined, port = undefined}) ->
    ok;

terminate(_Reason, #state{fd = FD, port = Port, oattr = Orig}) ->
    catch erlang:port_close(Port),
    _ = serctl:tcsetattr(FD, tcsanow, Orig),
    _ = serctl:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 如果波特率为9600，发送一个位需要的时间为
%% 1/9600s=0.0001042s=0.1042ms,
%% 这里按数据位为8位，停止位为2位，加起来就是10位，10个位发送所需的时间为：
%% 0.1042*10ms = 1.042ms,
%% 1/9600 * 1000 * 10
get_interval(Size, 600) ->
    Size * 16 + 4;
get_interval(Size, 1200) ->
    Size * 8 + 1;
get_interval(Size, 2400) ->
    Size * 4 + 2;
get_interval(Size, 4800) ->
    Size * 2 + 3;
get_interval(Size, 9600) ->
    Size * 1 + 5;
get_interval(Size, _) ->
    Size.

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

%% {dgiot_data:get({package_write_count,<<"usb7">>}), dgiot_data:get({package_recv_count,<<"usb5">>})}.
send(ParentPid, NewData, LastData, #state{serialport = Serialport, env = #{<<"serialdelay">> := Serialdelay}} = State) ->
    timer:sleep(Serialdelay),
    ParentPid ! {serial_data, self(), LastData},
    Size = size(LastData),
    dgiot_metrics:inc(dgiot_edge, <<"edge_recv">>, 1),
    dgiot_metrics:inc(dgiot_edge, <<"edge_recv_bytes">>, Size),
    dgiot_metrics:inc(dgiot_edge, <<Serialport/binary, "_recv">>, 1),
    dgiot_metrics:inc(dgiot_edge, <<Serialport/binary, "_recv_bytes">>, Size),
    Pubtopic = <<"$dg/user/edge/", Serialport/binary>>,
    NewLastData = dgiot_edge:convert_data(dgiot_data:get({recv_messagetype, Serialport}), LastData),
    BinParentPid = dgiot_utils:to_binary(ParentPid),
    dgiot_mqtt:publish(self(), Pubtopic, <<BinParentPid/binary, "RECV ", NewLastData/binary>>),
    State#state{data = NewData, timestamp = dgiot_datetime:now_ms()}.

setflag(Termios, #{<<"checkbit">> := <<"EVEN">>}) ->
    serctl:setflag(Termios, [{cflag, [{parenb, true}]}]);

setflag(Termios, #{<<"checkbit">> := <<"ODD">>}) ->
    serctl:setflag(Termios, [{cflag, [{parodd, true}]}]);

setflag(Termios, #{<<"databit">> := <<"5">>}) ->
    serctl:setflag(Termios, [{cflag, [{cs5, true}]}]);

setflag(Termios, #{<<"databit">> := <<"6">>}) ->
    serctl:setflag(Termios, [{cflag, [{cs6, true}]}]);

setflag(Termios, #{<<"databit">> := <<"7">>}) ->
    serctl:setflag(Termios, [{cflag, [{cs7, true}]}]);

setflag(Termios, #{<<"databit">> := <<"8">>}) ->
    serctl:setflag(Termios, [{cflag, [{cs8, true}]}]);

setflag(Termios, #{<<"hupcl">> := Flag}) ->
    serctl:setflag(Termios, [{cflag, [{hupcl, Flag}]}]);

setflag(Termios, #{<<"ignbrk">> := Flag}) ->
    serctl:setflag(Termios, [{iflag, [{ignbrk, Flag}]}]);

setflag(Termios, #{<<"ignpar">> := Flag}) ->
    serctl:setflag(Termios, [{iflag, [{ignpar, Flag}]}]);


setflag(Termios, _) ->
%%    io:format("~s ~p Termios = ~p ~n", [?FILE, ?LINE, Termios]),
    Termios.


defult_mode() ->
    #termios{
        cc = lists:foldl(
            fun({Offset, Val}, Bin) ->
                serctl:offset(Bin, {Offset + 1, Val})
            end,
            % zero'ed bytes
            <<0:(serctl:constant(nccs) * 8)>>,
            [
                % Minimum number of characters
                {serctl:constant(vmin), 1},
                % Timeout in deciseconds
                {serctl:constant(vtime), 5},
                {serctl:constant(vintr), 3},
                {serctl:constant(vreprint), 18},
                {serctl:constant(vdiscard), 15},
                {serctl:constant(vquit), 28},
                {serctl:constant(verase), 127},
                {serctl:constant(vkill), 21},
                {serctl:constant(veof), 4},
                {serctl:constant(vstart), 17},
                {serctl:constant(vstop), 19},
                {serctl:constant(vsusp), 26},
                {serctl:constant(vwerase), 23},
                {serctl:constant(vlnext), 22}
            ]
        ),

        % ignore (discard) parity errors
        iflag = serctl:constant(ignpar),

        cflag =
        serctl:constant(cs8) bor
            serctl:constant(clocal) bor
            serctl:constant(crtscts) bor
            serctl:constant(cread)
    }.
