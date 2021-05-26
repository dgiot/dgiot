-module(observer_cli_port).

-include("observer_cli.hrl").

-export([start/2]).

-spec start(pid(), view_opts()) -> no_return.
start(Port, Opts) ->
    #view_opts{port = RefreshMs} = Opts,
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(Port, RefreshMs, ?INIT_TIME_REF)
    end),
    manager(RenderPid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(RenderPid, Opts) ->
    case parse_cmd(Opts, RenderPid) of
        quit ->
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            manager(RenderPid, Opts#view_opts{port = NewInterval});
        ViewAction ->
            erlang:send(RenderPid, ViewAction),
            manager(RenderPid, Opts)
    end.

render_worker(Port, Interval, TimeRef) ->
    PortInfo = recon:port_info(Port),
    Meta = proplists:get_value(meta, PortInfo),
    case lists:member(undefined, Meta) of
        true ->
            output_die_view(Port, Interval),
            next_draw_view(TimeRef, Interval, Port);
        false ->
            Id = proplists:get_value(id, Meta),
            Name = proplists:get_value(name, Meta),
            OsPid = proplists:get_value(os_pid, Meta),

            Signals = proplists:get_value(signals, PortInfo),
            Link = proplists:get_value(links, Signals),
            Monitors = proplists:get_value(monitors, Signals),
            Connected = proplists:get_value(connected, Signals),

            IO = proplists:get_value(io, PortInfo),
            Input = proplists:get_value(input, IO),
            Output = proplists:get_value(output, IO),

            MemoryUsed = proplists:get_value(memory_used, PortInfo),
            Memory = proplists:get_value(memory, MemoryUsed),
            QueueSize = proplists:get_value(queue_size, MemoryUsed),
            Menu = render_menu(info, Interval),

            Line1 = render_port_info(
                Port,
                Id,
                Name,
                OsPid,
                Input,
                Output,
                Memory,
                QueueSize,
                Connected
            ),
            Line2 = render_link_monitor(Link, Monitors),
            Line3 = render_type_line(proplists:get_value(type, PortInfo)),
            LastLine = render_last_line(),

            ?output([?CURSOR_TOP, Menu, Line1, Line2, Line3, LastLine]),
            next_draw_view(TimeRef, Interval, Port)
    end.

next_draw_view(TimeRef, Interval, Port) ->
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    next_draw_view_2(NewTimeRef, Interval, Port).

next_draw_view_2(TimeRef, Interval, Port) ->
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(Port, NewInterval, TimeRef);
        _ ->
            ?output(?CLEAR),
            render_worker(Port, Interval, TimeRef)
    end.

render_port_info(
    Port,
    Id,
    Name,
    OsPid,
    Input,
    Output,
    Memory,
    QueueSize,
    Connected
) ->
    QueueSizeColor =
        case QueueSize > 0 of
            true -> ?RED;
            false -> ?GREEN
        end,
    Title =
        ?render([
            ?GRAY_BG,
            ?W("Attr", 18),
            ?W("Value", 20),
            ?W("Attr", 18),
            ?W("Value", 20),
            ?W("Attr", 19),
            ?W("Value", 21)
        ]),
    Rows =
        ?render([
            ?W("port", 18),
            ?W(Port, 20),
            ?W("id", 18),
            ?W(Id, 20),
            ?W("name", 19),
            ?W(Name, 21),
            ?NEW_LINE,
            ?W("queue_size", 18),
            ?W2(QueueSizeColor, QueueSize, 21),
            ?W(" input", 19),
            ?W({byte, Input}, 20),
            ?W("output", 19),
            ?W({byte, Output}, 21),
            ?NEW_LINE,
            ?W("connected", 18),
            ?W(Connected, 20),
            ?W("memory", 18),
            ?W({byte, Memory}, 20),
            ?W("os_pid", 19),
            ?W(OsPid, 21)
        ]),
    [Title, Rows].

render_link_monitor(Link, Monitors) ->
    LinkStr = [
        begin
            observer_cli_lib:to_list(P)
        end
        || P <- lists:sublist(Link, 30)
    ],
    MonitorsStr = [
        begin
            case P of
                {process, Pid} ->
                    observer_cli_lib:to_list(Pid);
                {RegName, Node} ->
                    observer_cli_lib:to_list(RegName) ++ "/" ++ observer_cli_lib:to_list(Node)
            end
        end
        || P <- lists:sublist(Monitors, 30)
    ],
    LinkInfo = "Links(" ++ erlang:integer_to_list(erlang:length(Link)) ++ ")",
    MonitorInfo = "Monitors(" ++ erlang:integer_to_list(erlang:length(Monitors)) ++ ")",
    ?render([
        ?W(LinkInfo, 18),
        ?W(LinkStr, 110),
        ?NEW_LINE,
        ?W2(?UNDERLINE, MonitorInfo, 19),
        ?W2(?UNDERLINE, MonitorsStr, 111)
    ]).

render_type_line(List) ->
    PeerName =
        case lists:keyfind(peername, 1, List) of
            {_, Peer} -> addr_to_str(Peer);
            false -> "undefined"
        end,
    SockName =
        case lists:keyfind(sockname, 1, List) of
            {_, Sock} -> addr_to_str(Sock);
            false -> "undefined"
        end,
    Line1 =
        ?render([
            ?UNDERLINE,
            ?W("            " ++ SockName ++ "(sockname)", 55),
            ?W("<=============>", 15),
            ?W("            " ++ PeerName ++ "(peername)", 55)
        ]),
    Line2 =
        case lists:keyfind(statistics, 1, List) of
            {_, Stats} -> [Line1, render_stats(Stats)];
            false -> Line1
        end,
    case lists:keyfind(options, 1, List) of
        {_, Opts} -> Line2 ++ [render_opts(Opts)];
        false -> Line2
    end.

render_stats(Stats) ->
    RecvOct = proplists:get_value(recv_oct, Stats),
    RecvCnt = proplists:get_value(recv_cnt, Stats),
    RecvMax = proplists:get_value(recv_max, Stats),
    RecvAvg = proplists:get_value(recv_avg, Stats),
    RecvDvi = proplists:get_value(recv_dvi, Stats),
    SendOct = proplists:get_value(send_oct, Stats),
    SendCnt = proplists:get_value(send_cnt, Stats),
    SendMax = proplists:get_value(send_max, Stats),
    SendAvg = proplists:get_value(send_avg, Stats),
    SendPend = proplists:get_value(send_pend, Stats),
    ?render([
        ?W("recv_cnt", 9),
        ?W(RecvCnt, 12),
        ?W("recv_oct", 8),
        ?W({byte, RecvOct}, 12),
        ?W("recv_max", 9),
        ?W({byte, RecvMax}, 12),
        ?W("recv_avg", 9),
        ?W({byte, RecvAvg}, 12),
        ?W("recv_dvi", 9),
        ?W({byte, RecvDvi}, 12),
        ?NEW_LINE,
        ?W("send_cnt", 9),
        ?W(SendCnt, 12),
        ?W("send_oct", 8),
        ?W({byte, SendOct}, 12),
        ?W("send_max", 9),
        ?W({byte, SendMax}, 12),
        ?W("send_avg", 9),
        ?W({byte, SendAvg}, 12),
        ?W("send_pend", 9),
        ?W(SendPend, 12)
    ]).

render_opts(Opts) ->
    Active = proplists:get_value(active, Opts),
    Broadcast = proplists:get_value(broadcast, Opts),
    Buffer = proplists:get_value(buffer, Opts),
    DelaySend = proplists:get_value(delay_send, Opts),
    DontRoute = proplists:get_value(dontroute, Opts),

    ExitOnClose = proplists:get_value(exit_on_close, Opts),
    Header = proplists:get_value(header, Opts),
    HighWatermark = proplists:get_value(high_watermark, Opts),
    KeepAlive = proplists:get_value(keepalive, Opts),
    Linger = io_lib:format("~p", [proplists:get_value(linger, Opts)]),

    LowWatermark = proplists:get_value(low_watermark, Opts),
    Mode = proplists:get_value(mode, Opts),
    NoDelay = proplists:get_value(nodelay, Opts),
    Packet = proplists:get_value(packet, Opts),
    PacketSize = proplists:get_value(packet_size, Opts),

    Priority = proplists:get_value(priority, Opts),
    RecBuf = proplists:get_value(recbuf, Opts),
    ReuseAddr = proplists:get_value(reuseaddr, Opts),
    SendTimeout = proplists:get_value(send_timeout, Opts),
    SndBuf = proplists:get_value(sndbuf, Opts),
    Title =
        ?render([
            ?GRAY_BG,
            ?W("Option", 9),
            ?W("Value", 6),
            ?W("Option", 14),
            ?W("Value", 12),
            ?W("Option", 9),
            ?W("Value", 12),
            ?W("Option", 13),
            ?W("Value", 8),
            ?W("Option", 9),
            ?W("Value", 12)
        ]),
    Rows =
        ?render([
            ?W("mode", 9),
            ?W(Mode, 6),
            ?W("recbuf", 14),
            ?W({byte, RecBuf}, 12),
            ?W("sndbuf", 9),
            ?W({byte, SndBuf}, 12),
            ?W("delay_send", 13),
            ?W(DelaySend, 8),
            ?W("dontroute", 9),
            ?W(DontRoute, 12),
            ?NEW_LINE,
            ?W("reuseaddr", 9),
            ?W(ReuseAddr, 6),
            ?W("packet_size", 14),
            ?W({byte, PacketSize}, 12),
            ?W("buffer", 9),
            ?W({byte, Buffer}, 12),
            ?W("exit_on_close", 13),
            ?W(ExitOnClose, 8),
            ?W("priority", 9),
            ?W(Priority, 12),
            ?NEW_LINE,
            ?W("active", 9),
            ?W(Active, 6),
            ?W("low_watermark", 14),
            ?W({byte, LowWatermark}, 12),
            ?W("header", 9),
            ?W(Header, 12),
            ?W("keepalive", 13),
            ?W(KeepAlive, 8),
            ?W("linger", 9),
            ?W(Linger, 12),
            ?NEW_LINE,
            ?W("nodelay", 9),
            ?W(NoDelay, 6),
            ?W("high_watermark", 14),
            ?W({byte, HighWatermark}, 12),
            ?W("broadcast", 9),
            ?W(Broadcast, 12),
            ?W("send_timeout", 13),
            ?W(SendTimeout, 8),
            ?W("packet", 9),
            ?W(Packet, 12)
        ]),
    [Title, Rows].

render_last_line() ->
    io_lib:format("|\e[7mq(quit) ~124.124s\e[0m|~n", [" "]).

render_menu(Type, Interval) ->
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Title = get_menu_title(Type),
    UpTime = observer_cli_lib:uptime(),
    TitleWidth = ?COLUMN + 41 - erlang:length(UpTime),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

get_menu_title(Type) ->
    [Home, Net, Port] = get_menu_title2(Type),
    [Home, "|", Net, "|", Port].

get_menu_title2(info) ->
    [?UNSELECT("Home(H)"), ?UNSELECT("Network(N)"), ?SELECT("Port Info(P)")].

parse_cmd(ViewOpts, Pid) ->
    case observer_cli_lib:to_list(io:get_line("")) of
        "q\n" ->
            quit;
        "Q\n" ->
            quit;
        "P\n" ->
            info_view;
        "H\n" ->
            erlang:exit(Pid, stop),
            observer_cli:start(ViewOpts);
        "N\n" ->
            erlang:exit(Pid, stop),
            observer_cli_inet:start(ViewOpts);
        Number ->
            observer_cli_lib:parse_integer(Number)
    end.

output_die_view(Port, Interval) ->
    Menu = render_menu(info, Interval),
    Line = io_lib:format("\e[31mPort(~p) has already die.\e[0m~n", [Port]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

addr_to_str({Addr, Port}) ->
    AddrList = [
        begin
            erlang:integer_to_list(A)
        end
        || A <- erlang:tuple_to_list(Addr)
    ],
    string:join(AddrList, ".") ++ ":" ++ erlang:integer_to_list(Port).
