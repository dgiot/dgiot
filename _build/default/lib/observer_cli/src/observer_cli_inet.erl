%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_inet).

-include("observer_cli.hrl").

%% API
-export([start/1]).
-export([clean/1]).

-define(LAST_LINE,
    "q(quit) ic(inet_count) iw(inet_window) rc(recv_cnt) ro(recv_oct)"
    " sc(send_cnt) so(send_oct) cnt oct 9(port 9 info) pd/pu(page:down/up)"
).

-spec start(view_opts()) -> no_return.
start(#view_opts{inet = InetOpt, auto_row = AutoRow} = ViewOpts) ->
    StorePid = observer_cli_store:start(),
    RenderPid = spawn_link(
        fun() ->
            ?output(?CLEAR),
            {{input, In}, {output, Out}} = erlang:statistics(io),
            render_worker(StorePid, InetOpt, ?INIT_TIME_REF, 0, {In, Out}, AutoRow)
        end
    ),
    manager(StorePid, RenderPid, ViewOpts).

-spec clean(list()) -> ok.
clean(Pids) -> observer_cli_lib:exit_processes(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(StorePid, RenderPid, ViewOpts = #view_opts{inet = InetOpts}) ->
    #inet{cur_page = CurPage, pages = Pages} = InetOpts,
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [RenderPid, StorePid]) of
        quit ->
            observer_cli_lib:exit_processes([StorePid]),
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            NewInet = InetOpts#inet{interval = NewInterval},
            manager(StorePid, RenderPid, ViewOpts#view_opts{inet = NewInet});
        Func when Func =:= inet_count; Func =:= inet_window ->
            clean([StorePid, RenderPid]),
            start(ViewOpts#view_opts{inet = InetOpts#inet{func = Func}});
        Type when
            Type =:= recv_cnt;
            Type =:= recv_oct;
            Type =:= send_cnt;
            Type =:= send_oct;
            Type =:= cnt;
            Type =:= oct
        ->
            clean([StorePid, RenderPid]),
            start(ViewOpts#view_opts{inet = InetOpts#inet{type = Type}});
        {jump, NewPos} ->
            NewPages = observer_cli_lib:update_page_pos(CurPage, NewPos, Pages),
            NewInetOpts = InetOpts#inet{pages = NewPages},
            start_port_view(StorePid, RenderPid, ViewOpts#view_opts{inet = NewInetOpts}, false);
        jump ->
            start_port_view(StorePid, RenderPid, ViewOpts, true);
        page_down_top_n ->
            NewPage = max(CurPage + 1, 1),
            NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
            clean([StorePid, RenderPid]),
            start(ViewOpts#view_opts{inet = InetOpts#inet{cur_page = NewPage, pages = NewPages}});
        page_up_top_n ->
            NewPage = max(CurPage - 1, 1),
            NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
            clean([StorePid, RenderPid]),
            start(ViewOpts#view_opts{inet = InetOpts#inet{cur_page = NewPage, pages = NewPages}});
        _ ->
            manager(StorePid, RenderPid, ViewOpts)
    end.

render_worker(StorePid, InetOpt, LastTimeRef, Count, LastIO, AutoRow) ->
    #inet{func = Function, type = Type, interval = Interval, cur_page = CurPage} = InetOpt,
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Row = erlang:max(TerminalRow - 5, 0),
    Text = get_menu_str(Function, Type, Interval, Row),
    Menu = observer_cli_lib:render_menu(inet, Text),
    TopLen = Row * CurPage,
    InetList = inet_info(Function, Type, TopLen, Interval, Count),
    {IORows, NewIO} = render_io_rows(LastIO),
    {PortList, InetRows} = render_inet_rows(InetList, TopLen, InetOpt),
    LastLine = observer_cli_lib:render_last_line(?LAST_LINE),
    ?output([?CURSOR_TOP, Menu, IORows, InetRows, LastLine]),
    observer_cli_store:update(StorePid, Row, PortList),
    NewInterval =
        case Function of
            inet_count -> Interval;
            inet_window -> 10
        end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, NewInterval),
    receive
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(
                StorePid,
                InetOpt#inet{interval = NewInterval},
                TimeRef,
                Count + 1,
                NewIO,
                AutoRow
            );
        quit ->
            quit;
        _ ->
            render_worker(StorePid, InetOpt, TimeRef, Count + 1, NewIO, AutoRow)
    end.

render_io_rows({LastIn, LastOut}) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {
        ?render([
            ?YELLOW,
            ?W("Byte Input", 14),
            ?W({byte, In - LastIn}, 12),
            ?W("Byte Output", 13),
            ?W({byte, Out - LastOut}, 12),
            ?W("Total Input", 15),
            ?W({byte, In}, 17),
            ?W("Total Output", 15),
            ?W({byte, Out}, 17)
        ]),
        {In, Out}
    }.

render_inet_rows([], Rows, #inet{func = inet_count, type = Type}) ->
    {[], io_lib:format("\e[32;1mGet nothing for recon:inet_count(~p, ~p)\e[0m~n", [Type, Rows])};
render_inet_rows([], Rows, #inet{func = inet_window, type = Type, interval = Interval}) ->
    {[],
        io_lib:format("\e[32;1mGet nothing for recon:inet_window(~p, ~p, ~p)\e[0m~n", [
            Type,
            Rows,
            Interval
        ])};
render_inet_rows(InetList, Num, #inet{
    type = Type,
    pages = Pages,
    cur_page = Page
}) when Type =:= cnt orelse Type =:= oct ->
    {Unit, RecvType, SendType} = trans_type(Type),
    Title = title(Type, RecvType, SendType),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(InetList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Port, Value, [{_, Recv}, {_, Send}]} = Item,
        {memory_used, MemoryUsed} = recon:port_info(Port, memory_used),
        {io, IO} = recon:port_info(Port, io),
        Input = proplists:get_value(input, IO),
        Output = proplists:get_value(output, IO),
        QueueSize = proplists:get_value(queue_size, MemoryUsed),
        Memory = proplists:get_value(memory, MemoryUsed),
        IP = get_remote_ip(Port),
        {ValueFormat, RecvFormat, SendFormat} = trans_format(Unit, Value, Recv, Send),
        R = [
            ?W(Pos, 2),
            ?W(Port, 16),
            ValueFormat,
            RecvFormat,
            SendFormat,
            ?W({byte, Output}, 12),
            ?W({byte, Input}, 12),
            ?W(QueueSize, 6),
            ?W({byte, Memory}, 12),
            ?W(IP, 19)
        ],
        Rows = add_choose_color(ChoosePos, Pos, R),
        {[?render(Rows) | Acc], [{Pos, Port} | Acc1], Pos + 1}
    end,
    {Rows, PortList, _} = lists:foldl(
        FormatFunc,
        {[], [], Start},
        lists:sublist(InetList, Start, Num)
    ),
    {PortList, [Title | lists:reverse(Rows)]};
render_inet_rows(InetList, Num, #inet{type = Type, pages = Pages, cur_page = Page}) ->
    {Unit, Type1, Type2} = trans_type(Type),
    Title = title(Type, Type1, Type2),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(InetList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Port, Value, _} = Item,
        {memory_used, MemoryUsed} = recon:port_info(Port, memory_used),
        {io, IO} = recon:port_info(Port, io),
        Input = proplists:get_value(input, IO),
        Output = proplists:get_value(output, IO),
        QueueSize = proplists:get_value(queue_size, MemoryUsed),
        Memory = proplists:get_value(memory, MemoryUsed),
        IP = get_remote_ip(Port),
        Packet1 = getstat(Port, erlang:list_to_existing_atom(Type1)),
        AllPacket =
            case is_integer(Packet1) of
                true -> Value + Packet1;
                false -> Value
            end,
        {ValueFormat, Packet1Format, AllFormat} = trans_format(Unit, Value, Packet1, AllPacket),
        R = [
            ?W(Pos, 2),
            ?W(Port, 16),
            ValueFormat,
            Packet1Format,
            AllFormat,
            ?W({byte, Input}, 12),
            ?W({byte, Output}, 12),
            ?W(QueueSize, 6),
            ?W({byte, Memory}, 12),
            ?W(IP, 19)
        ],
        Rows = add_choose_color(ChoosePos, Pos, R),
        {[?render(Rows) | Acc], [{Pos, Port} | Acc1], Pos + 1}
    end,
    {Rows, PortList, _} = lists:foldl(
        FormatFunc,
        {[], [], Start},
        lists:sublist(InetList, Start, Num)
    ),
    {PortList, [Title | lists:reverse(Rows)]}.

get_menu_str(inet_count, Type, Interval, Rows) ->
    io_lib:format("recon:inet_count(~p, ~w) Interval:~wms", [Type, Rows, Interval]);
get_menu_str(inet_window, Type, Interval, Rows) ->
    io_lib:format("recon:inet_window(~p, ~w, ~w) Interval:~wms", [Type, Rows, Interval, Interval]).

inet_info(inet_count, Type, Num, _, _) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, _, 0) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, Ms, _) -> recon:inet_window(Type, Num, Ms).

getstat(Port, Attr) ->
    case inet:getstat(Port, [Attr]) of
        {ok, [{_, Value}]} -> Value;
        {error, Err} -> inet:format_error(Err)
    end.

start_port_view(StorePid, RenderPid, Opts = #view_opts{inet = InetOpt}, AutoJump) ->
    #inet{cur_page = CurPage, pages = Pages} = InetOpt,
    {_, CurPos} = lists:keyfind(CurPage, 1, Pages),
    case observer_cli_store:lookup_pos(StorePid, CurPos) of
        {CurPos, ChoosePort} ->
            clean([StorePid, RenderPid]),
            observer_cli_port:start(ChoosePort, Opts);
        {_, ChoosePort} when AutoJump ->
            clean([StorePid, RenderPid]),
            observer_cli_port:start(ChoosePort, Opts);
        _ ->
            manager(StorePid, RenderPid, Opts)
    end.

trans_type(cnt) ->
    {number, "recv_cnt", "send_cnt"};
trans_type(oct) ->
    {byte, "recv_oct", "send_oct"};
trans_type(send_cnt) ->
    {number, "recv_cnt", "cnt"};
trans_type(recv_cnt) ->
    {number, "send_cnt", "cnt"};
trans_type(send_oct) ->
    {byte, "recv_oct", "oct"};
trans_type(recv_oct) ->
    {byte, "send_oct", "oct"}.

trans_format(byte, Val, Val1, Val2) ->
    {
        ?W({byte, Val}, 10),
        ?W({byte, Val1}, 10),
        ?W({byte, Val2}, 10)
    };
trans_format(number, Val, Val1, Val2) ->
    {
        ?W(Val, 10),
        ?W(Val1, 10),
        ?W(Val2, 10)
    }.

title(Type, Type1, Type2) ->
    ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("NO", 2),
        ?W("Port", 16),
        ?W(erlang:atom_to_list(Type), 10),
        ?W(Type1, 10),
        ?W(Type2, 10),
        ?W("output", 12),
        ?W("input", 12),
        ?W("queuesize", 6),
        ?W("memory", 12),
        ?W("Peername(ip:port)", 19)
    ]).

get_remote_ip(P) ->
    case inet:peername(P) of
        {ok, {Addr, Port}} ->
            AddrList = [
                begin
                    erlang:integer_to_list(A)
                end
                || A <- erlang:tuple_to_list(Addr)
            ],
            string:join(AddrList, ".") ++ ":" ++ erlang:integer_to_list(Port);
        {error, Err} ->
            inet:format_error(Err)
    end.

add_choose_color(ChoosePos, ChoosePos, R) -> [?CHOOSE_BG | R];
add_choose_color(_ChoosePos, _CurPos, R) -> R.
