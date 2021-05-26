%%% @author zhongwen <zhongwencool@gmail.com>

-module(observer_cli).

-include("observer_cli.hrl").

%% API
-export([start/0]).
-export([start/1]).
-export([start/2]).
-export([start_plugin/0]).
-export([clean/1]).

%% cpu >= this value will be highlight
-define(CPU_ALARM_THRESHOLD, 0.8).
%% port or process reach max_limit * 0.85 will be highlight
-define(COUNT_ALARM_THRESHOLD, 0.85).
-define(LAST_LINE,
    "q(quit) p(pause) r/rr(reduction) m/mm(mem)"
    "b/bb(binary mem) t/tt(total heap size) mq/mmq(msg queue) 9(proc 9 info) F/B(page forward/back)"
).

-spec start() -> no_return | {badrpc, term()}.
start() -> start(#view_opts{}).

-spec start(Node) -> no_return | {badrpc, term()} when Node :: atom() | non_neg_integer().
start(Node) when Node =:= node() ->
    start(#view_opts{});
start(Node) when is_atom(Node) ->
    rpc_start(Node, ?DEFAULT_INTERVAL);
start(#view_opts{home = Home} = Opts) ->
    erlang:process_flag(trap_exit, true),
    AutoRow = check_auto_row(),
    #home{scheduler_usage = SchUsage} = Home,
    StorePid = observer_cli_store:start(),
    LastSchWallFlag = set_scheduler_wall_time(true, SchUsage),
    PsCmd = io_lib:format("ps -o pcpu,pmem ~s", [os:getpid()]),
    RenderPid = spawn_link(fun() -> render_worker(PsCmd, StorePid, Home, AutoRow) end),
    manager(StorePid, RenderPid, Opts#view_opts{auto_row = AutoRow}, LastSchWallFlag);
start(Interval) when is_integer(Interval), Interval >= ?MIN_INTERVAL ->
    start(#view_opts{
        home = #home{interval = Interval},
        ets = #ets{interval = Interval},
        sys = #system{interval = Interval},
        db = #db{interval = Interval},
        help = #help{interval = Interval},
        inet = #inet{interval = Interval},
        process = #process{interval = Interval},
        port = Interval
    }).

-spec start(Node, Cookies | Options) -> no_return | {badrpc, term()} when
    Node :: atom(),
    Cookies :: atom(),
    Options :: proplists:proplist().
start(Node, _Cookie) when Node =:= node() ->
    start(#view_opts{});
start(Node, Cookie) when is_atom(Node) andalso is_atom(Cookie) ->
    start(Node, [{cookie, Cookie}]);
start(Node, Options) when is_atom(Node) andalso is_list(Options) ->
    case proplists:get_value(cookie, Options) of
        undefined -> ok;
        Cookie -> erlang:set_cookie(Node, Cookie)
    end,
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    rpc_start(Node, Interval).

-spec start_plugin() -> no_return.
start_plugin() ->
    erlang:process_flag(trap_exit, true),
    application:ensure_all_started(observer_cli),
    observer_cli_plugin:start(#view_opts{}).

-spec clean(list()) -> boolean().
clean([RenderPid, StorePid, SchWallFlag, SchUsage]) ->
    observer_cli_lib:exit_processes([RenderPid, StorePid]),
    set_scheduler_wall_time(SchWallFlag, SchUsage).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpc_start(Node, Interval) ->
    case net_kernel:hidden_connect_node(Node) of
        true ->
            rpc:call(Node, ?MODULE, start, [Interval]);
        false ->
            Msg = <<"Node(~p) refuse to be connected, make sure cookie is valid~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown};
        ignored ->
            Msg = <<"Ignored by node(~p), local node is not alive!~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown}
    end.

manager(StorePid, RenderPid, Opts, LastSchWallFlag) ->
    #view_opts{home = Home = #home{cur_page = CurPage, pages = Pages, scheduler_usage = SchUsage}} =
        Opts,
    Resource = [RenderPid, StorePid, LastSchWallFlag, SchUsage],
    case observer_cli_lib:parse_cmd(Opts, ?MODULE, Resource) of
        quit ->
            erlang:unlink(RenderPid),
            erlang:send(RenderPid, quit),
            set_scheduler_wall_time(LastSchWallFlag, SchUsage),
            observer_cli_lib:exit_processes([StorePid]),
            quit;
        pause_or_resume ->
            erlang:send(RenderPid, pause_or_resume),
            manager(StorePid, RenderPid, Opts, LastSchWallFlag);
        {new_interval, NewInterval} ->
            clean(Resource),
            start(Opts#view_opts{home = Home#home{interval = NewInterval}});
        scheduler_usage ->
            NewSchUsage =
                case SchUsage of
                    ?DISABLE -> ?ENABLE;
                    ?ENABLE -> ?DISABLE
                end,
            clean(Resource),
            start(Opts#view_opts{home = Home#home{scheduler_usage = NewSchUsage}});
        {jump, NewPos} ->
            NewPages = observer_cli_lib:update_page_pos(CurPage, NewPos, Pages),
            NewOpts = Opts#view_opts{home = Home#home{pages = NewPages}},
            start_process_view(StorePid, RenderPid, NewOpts, LastSchWallFlag, false);
        jump ->
            start_process_view(StorePid, RenderPid, Opts, LastSchWallFlag, true);
        {func, Func, Type} ->
            clean(Resource),
            start(Opts#view_opts{home = Home#home{func = Func, type = Type}});
        page_down_top_n ->
            NewPage = max(CurPage + 1, 1),
            NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
            clean(Resource),
            start(Opts#view_opts{home = Home#home{cur_page = NewPage, pages = NewPages}});
        page_up_top_n ->
            NewPage = max(CurPage - 1, 1),
            NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
            clean(Resource),
            start(Opts#view_opts{home = Home#home{cur_page = NewPage, pages = NewPages}});
        _ ->
            manager(StorePid, RenderPid, Opts, LastSchWallFlag)
    end.

render_worker(PsCmd, Manager, Home = #home{scheduler_usage = SchUsage}, AutoRow) ->
    ?output(?CLEAR),
    StableInfo = get_stable_system_info(),
    LastStats = get_incremental_stats(SchUsage),
    redraw_running(PsCmd, Manager, Home, StableInfo, LastStats, erlang:make_ref(), AutoRow, true).

%% pause status waiting to be resume
redraw_pause(PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow) ->
    notify_pause_status(),
    erlang:cancel_timer(LastTimeRef),
    #home{func = Func, type = Type} = Home,
    receive
        quit ->
            quit;
        {Func, Type} ->
            redraw_running(
                PsCmd,
                StorePid,
                Home,
                StableInfo,
                LastStats,
                LastTimeRef,
                AutoRow,
                false
            );
        pause_or_resume ->
            ?output(?CLEAR),
            redraw_running(PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow, true)
    end.

%% running status
redraw_running(PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow, IsFirstTime) ->
    #home{
        interval = Interval,
        func = Func,
        type = Type,
        pages = RankPos,
        cur_page = CurPage,
        scheduler_usage = SchUsage
    } = Home,
    erlang:cancel_timer(LastTimeRef),
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    {Diffs, Schedulers, NewStats} = node_stats(LastStats, SchUsage),
    {CPURow, CPULine} = render_scheduler_usage(Schedulers),
    ProcessRows = max(TerminalRow - 14 - CPURow, 0),
    TopLen = ProcessRows * CurPage,
    TopList = get_top_n(Func, Type, Interval, TopLen, IsFirstTime),
    Text = get_refresh_prompt(Func, Type, Interval, TopLen),
    MenuLine = observer_cli_lib:render_menu(home, Text),
    SystemLine = render_system_line(PsCmd, element(1, StableInfo)),
    MemLine = render_memory_process_line(Diffs, element(2, StableInfo), Interval),
    {TopNList, RankLine} = render_top_n_view(Type, TopList, ProcessRows, RankPos, CurPage),
    LastLine = observer_cli_lib:render_last_line(?LAST_LINE),
    ?output([?CURSOR_TOP, MenuLine, SystemLine, MemLine, CPULine, RankLine, LastLine]),

    observer_cli_store:update(StorePid, ProcessRows, TopNList),
    TimeRef = refresh_next_time(Func, Type, Interval),
    receive
        quit ->
            quit;
        pause_or_resume ->
            redraw_pause(PsCmd, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow);
        {Func, Type} ->
            redraw_running(PsCmd, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow, false)
    end.

render_system_line(PsCmd, StableInfo) ->
    [Version, SysVersion, ProcLimit, PortLimit, EtsLimit] = StableInfo,
    ActiveTask = erlang:statistics(total_active_tasks),
    {ContextSwitch, _} = erlang:statistics(context_switches),
    AtomStatus = get_atom_status(),
    Reductions = erlang:statistics(reductions),
    {PortWarning, ProcWarning, PortCount, ProcCount} =
        get_port_proc_info(PortLimit, ProcLimit),
    [_, CmdValue | _] = string:split(os:cmd(PsCmd), "\n", all),

    [CpuPsV, MemPsV] =
        case lists:filter(fun(Y) -> Y =/= [] end, string:split(CmdValue, " ", all)) of
            [] -> ["--", "--"];
            [V1, V2] -> [V1, V2]
        end,
    Title = ?render([
        ?W(SysVersion, 136),
        ?NEW_LINE,
        ?GRAY_BG,
        ?W("System", 10),
        ?W("Count/Limit", 21),
        ?W("System", 25),
        ?W("Status", 21),
        ?W("Stat Info", 20),
        ?W("Size", 24)
    ]),
    Row1 = ?render([
        ?W("Proc Count", 10),
        ?W2(ProcWarning, ProcCount, 22),
        ?W(" Version", 26),
        ?W(Version, 21),
        ?W("Active Task", 20),
        ?W(ActiveTask, 24),
        ?NEW_LINE,
        ?W("Port Count", 10),
        ?W2(PortWarning, PortCount, 22),
        ?W(" ps -o pcpu", 26),
        ?W([CpuPsV, "%"], 21),
        ?W("Context Switch", 20),
        ?W(ContextSwitch, 24)
    ]),
    {Reds, AddReds} = Reductions,
    Row2 =
        case AtomStatus of
            {ok, AtomLimit, AtomCount} ->
                {AtomWarning, Atom} = format_atom_info(AtomLimit, AtomCount),
                ?render([
                    ?UNDERLINE,
                    ?W("Atom Count", 10),
                    ?W2(AtomWarning, Atom, 22),
                    ?W(" ps -o pmem", 26),
                    ?W([MemPsV, "%"], 21),
                    ?W("Reds(Total/SinceLastCall)", 20),
                    ?W([integer_to_list(Reds), "/", integer_to_list(AddReds)], 24)
                ]);
            {error, unsupported} ->
                ?render([
                    ?UNDERLINE,
                    ?W("Ets Limit", 10),
                    ?W(EtsLimit, 21),
                    ?W(" ps -o pmem", 25),
                    ?W([MemPsV, "%"], 21),
                    ?W("Reductions", 20),
                    ?W(Reductions, 24)
                ])
        end,
    [Title, Row1, Row2].

render_memory_process_line(MemSum, PortParallelism, Interval) ->
    RunQ = erlang:statistics(run_queue),
    Mem = erlang:memory(),
    TotalMem = proplists:get_value(total, Mem),
    ProcMem = proplists:get_value(processes_used, Mem),
    CodeMem = proplists:get_value(code, Mem),
    AtomMem = proplists:get_value(atom_used, Mem),
    BinMem = proplists:get_value(binary, Mem),
    EtsMem = proplists:get_value(ets, Mem),
    {
        BytesIn,
        BytesOut,
        GcCount,
        GcWordsReclaimed
    } = MemSum,

    {Queue, LogKey} =
        case whereis(error_logger) of
            undefined ->
                {erlang:integer_to_list(RunQ), "RunQueue"};
            Pid ->
                {_, Q} = process_info(Pid, message_queue_len),
                {[erlang:integer_to_list(RunQ), "/", erlang:integer_to_list(Q)],
                    "RunQueue/ErrorLoggerQueue"}
        end,
    ProcMemPercent = observer_cli_lib:to_percent(ProcMem / TotalMem),
    AtomMemPercent = observer_cli_lib:to_percent(AtomMem / TotalMem),
    BinMemPercent = observer_cli_lib:to_percent(BinMem / TotalMem),
    CodeMemPercent = observer_cli_lib:to_percent(CodeMem / TotalMem),
    EtsMemPercent = observer_cli_lib:to_percent(EtsMem / TotalMem),

    Title = ?render([
        ?GRAY_BG,
        ?W("Mem Type", 10),
        ?W("Size", 21),
        ?W("Mem Type", 25),
        ?W("Size", 21),
        ?W(["IO/GC:(", integer_to_binary(Interval), "ms)"], 20),
        ?W("Total/Increments", 24)
    ]),
    Row = ?render([
        ?W("Total", 10),
        ?W({byte, TotalMem}, 12),
        ?W("100.0%", 6),
        ?W("Binary", 25),
        ?W({byte, BinMem}, 12),
        ?W(BinMemPercent, 6),
        ?W("IO Output", 20),
        ?W(BytesOut, 24),
        ?NEW_LINE,
        ?W("Process", 10),
        ?W({byte, ProcMem}, 12),
        ?W(ProcMemPercent, 6),
        ?W("Code", 25),
        ?W({byte, CodeMem}, 12),
        ?W(CodeMemPercent, 6),
        ?W("IO Input", 20),
        ?W(BytesIn, 24),
        ?NEW_LINE,
        ?W("Atom", 10),
        ?W({byte, AtomMem}, 12),
        ?W(AtomMemPercent, 6),
        ?W("Port Parallelism (+spp)", 25),
        ?W(PortParallelism, 21),
        ?W("Gc Count", 20),
        ?W(GcCount, 24),
        ?NEW_LINE,
        ?W("Ets", 10),
        ?W({byte, EtsMem}, 12),
        ?W(EtsMemPercent, 6),
        ?W(LogKey, 25),
        ?W(Queue, 21),
        ?W("Gc Words Reclaimed", 20),
        ?W(GcWordsReclaimed, 24)
    ]),
    [Title, Row].

render_scheduler_usage(undefined) ->
    {0, []};
render_scheduler_usage(SchedulerUsage) ->
    SchedulerNum = erlang:length(SchedulerUsage),
    render_scheduler_usage(SchedulerUsage, SchedulerNum).

%% < 8 core split 2 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum < 8 ->
    Column =
        case SchedulerNum rem 2 =:= 0 of
            true -> SchedulerNum div 2;
            false -> (SchedulerNum div 2) + 1
        end,
    CPU = [
        begin
            Seq2 = transform_seq(Seq1, Column, SchedulerNum),
            Percent1 = proplists:get_value(Seq1, SchedulerUsage, 0.0),
            Percent2 = proplists:get_value(Seq2, SchedulerUsage, 0.0),
            CPU1 = observer_cli_lib:to_percent(Percent1),
            CPU2 = observer_cli_lib:to_percent(Percent2),
            Process1 = lists:duplicate(trunc(Percent1 * 57), "|"),
            Process2 = lists:duplicate(trunc(Percent2 * 57), "|"),
            IsLastLine = Seq1 =:= Column,
            Format = process_bar_format_style([Percent1, Percent2], IsLastLine),
            io_lib:format(Format, [
                Seq1,
                Process1,
                CPU1,
                Seq2,
                Process2,
                CPU2
            ])
        end
        || Seq1 <- lists:seq(1, Column)
    ],
    {Column, CPU};
%% 100 >= scheduler >= 8 split 4 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum =< 100 ->
    Column =
        case SchedulerNum rem 4 =:= 0 of
            true -> SchedulerNum div 4;
            false -> (SchedulerNum div 4) + 1
        end,
    CPU = [
        begin
            Seq2 = transform_seq(Seq1, Column, SchedulerNum),
            Seq3 = transform_seq(Seq2, Column, SchedulerNum),
            Seq4 = transform_seq(Seq3, Column, SchedulerNum),
            Percent1 = proplists:get_value(Seq1, SchedulerUsage, 0.0),
            Percent2 = proplists:get_value(Seq2, SchedulerUsage, 0.0),
            Percent3 = proplists:get_value(Seq3, SchedulerUsage, 0.0),
            Percent4 = proplists:get_value(Seq4, SchedulerUsage, 0.0),
            CPU1 = observer_cli_lib:to_percent(Percent1),
            CPU2 = observer_cli_lib:to_percent(Percent2),
            CPU3 = observer_cli_lib:to_percent(Percent3),
            CPU4 = observer_cli_lib:to_percent(Percent4),
            Process1 = lists:duplicate(trunc(Percent1 * 22), "|"),
            Process2 = lists:duplicate(trunc(Percent2 * 22), "|"),
            Process3 = lists:duplicate(trunc(Percent3 * 22), "|"),
            Process4 = lists:duplicate(trunc(Percent4 * 23), "|"),
            IsLastLine = Seq1 =:= Column,
            Format = process_bar_format_style([Percent1, Percent2, Percent3, Percent4], IsLastLine),
            io_lib:format(Format, [
                Seq1,
                Process1,
                CPU1,
                Seq2,
                Process2,
                CPU2,
                Seq3,
                Process3,
                CPU3,
                Seq4,
                Process4,
                CPU4
            ])
        end
        || Seq1 <- lists:seq(1, Column)
    ],
    {Column, CPU};
%% scheduler > 100 don't show process bar.
render_scheduler_usage(SchedulerUsage, SchedulerNum) ->
    Column =
        case SchedulerNum rem 10 =:= 0 of
            true -> SchedulerNum div 10;
            false -> (SchedulerNum div 10) + 1
        end,
    CPU = [
        begin
            Seq2 = transform_seq(Seq1, Column, SchedulerNum),
            Seq3 = transform_seq(Seq2, Column, SchedulerNum),
            Seq4 = transform_seq(Seq3, Column, SchedulerNum),
            Seq5 = transform_seq(Seq4, Column, SchedulerNum),
            Seq6 = transform_seq(Seq5, Column, SchedulerNum),
            Seq7 = transform_seq(Seq6, Column, SchedulerNum),
            Seq8 = transform_seq(Seq7, Column, SchedulerNum),
            Seq9 = transform_seq(Seq8, Column, SchedulerNum),
            Seq10 = transform_seq(Seq9, Column, SchedulerNum),
            Percent1 = proplists:get_value(Seq1, SchedulerUsage),
            Percent2 = proplists:get_value(Seq2, SchedulerUsage),
            Percent3 = proplists:get_value(Seq3, SchedulerUsage),
            Percent4 = proplists:get_value(Seq4, SchedulerUsage),
            Percent5 = proplists:get_value(Seq5, SchedulerUsage),
            Percent6 = proplists:get_value(Seq6, SchedulerUsage),
            Percent7 = proplists:get_value(Seq7, SchedulerUsage),
            Percent8 = proplists:get_value(Seq8, SchedulerUsage),
            Percent9 = proplists:get_value(Seq9, SchedulerUsage),
            Percent10 = proplists:get_value(Seq10, SchedulerUsage),
            CPU1 = observer_cli_lib:to_percent(Percent1),
            CPU2 = observer_cli_lib:to_percent(Percent2),
            CPU3 = observer_cli_lib:to_percent(Percent3),
            CPU4 = observer_cli_lib:to_percent(Percent4),
            CPU5 = observer_cli_lib:to_percent(Percent5),
            CPU6 = observer_cli_lib:to_percent(Percent6),
            CPU7 = observer_cli_lib:to_percent(Percent7),
            CPU8 = observer_cli_lib:to_percent(Percent8),
            CPU9 = observer_cli_lib:to_percent(Percent9),
            CPU10 = observer_cli_lib:to_percent(Percent10),
            IsLastLine = Seq1 =:= Column,
            Percents = [
                Percent1,
                Percent2,
                Percent3,
                Percent4,
                Percent5,
                Percent6,
                Percent7,
                Percent8,
                Percent9,
                Percent10
            ],
            Format = process_bar_format_style(Percents, IsLastLine),
            io_lib:format(Format, [
                Seq1,
                CPU1,
                Seq2,
                CPU2,
                Seq3,
                CPU3,
                Seq4,
                CPU4,
                Seq5,
                CPU5,
                Seq6,
                CPU6,
                Seq7,
                CPU7,
                Seq8,
                CPU8,
                Seq9,
                CPU9,
                Seq10,
                CPU10
            ])
        end
        || Seq1 <- lists:seq(1, Column)
    ],
    {Column, CPU}.

transform_seq(Seq, Column, Total) ->
    Num = Seq + Column,
    case Num > Total of
        true -> 1000;
        false -> Num
    end.

render_top_n_view(memory, MemoryList, Num, Pages, Page) ->
    Title = ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, "     Memory", 14),
        ?W(?GRAY_BG, "Name or Initial Call", 38),
        ?W(?GRAY_BG, "           Reductions", 21),
        ?W(?GRAY_BG, " MsgQueue", 10),
        ?W(?GRAY_BG, "Current Function", 32)
    ]),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(MemoryList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Pid, MemVal, CurFun, NameOrCall} = get_top_n_info(Item),
        {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
        Format = get_memory_format(ChoosePos, Pos),
        R = io_lib:format(
            Format,
            [
                Pos,
                erlang:pid_to_list(Pid),
                observer_cli_lib:to_byte(MemVal),
                NameOrCall,
                observer_cli_lib:to_list(Reductions),
                observer_cli_lib:to_list(MsgQueueLen),
                CurFun
            ]
        ),
        {[R | Acc], [{Pos, Pid} | Acc1], Pos + 1}
    end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(MemoryList, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]};
render_top_n_view(binary_memory, MemoryList, Num, Pages, Page) ->
    Title = ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, "  BinMemory", 14),
        ?W(?GRAY_BG, "Name or Initial Call", 38),
        ?W(?GRAY_BG, "           Reductions", 21),
        ?W(?GRAY_BG, " MsgQueue", 10),
        ?W(?GRAY_BG, "Current Function", 32)
    ]),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(MemoryList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Pid, MemVal, CurFun, NameOrCall} = get_top_n_info(Item),
        {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
        Format = get_memory_format(ChoosePos, Pos),
        R = io_lib:format(
            Format,
            [
                Pos,
                pid_to_list(Pid),
                observer_cli_lib:to_byte(MemVal),
                NameOrCall,
                observer_cli_lib:to_list(Reductions),
                observer_cli_lib:to_list(MsgQueueLen),
                CurFun
            ]
        ),
        {[R | Acc], [{Pos, Pid} | Acc1], Pos + 1}
    end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(MemoryList, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]};
render_top_n_view(reductions, ReductionList, Num, Pages, Page) ->
    Title = ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, "     Reductions", 21),
        ?W(?GRAY_BG, "Name or Initial Call", 38),
        ?W(?GRAY_BG, "      Memory", 13),
        ?W(?GRAY_BG, " MsgQueue", 10),
        ?W(?GRAY_BG, "Current Function", 33)
    ]),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(ReductionList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Pid, Reductions, CurFun, NameOrCall} = get_top_n_info(Item),
        {Memory, MsgQueueLen} = get_pid_info(Pid, [memory, message_queue_len]),
        Format = get_reduction_format(ChoosePos, Pos),
        R = io_lib:format(
            Format,
            [
                Pos,
                pid_to_list(Pid),
                observer_cli_lib:to_list(Reductions),
                NameOrCall,
                observer_cli_lib:to_byte(Memory),
                observer_cli_lib:to_list(MsgQueueLen),
                CurFun
            ]
        ),
        {[R | Acc], [{Pos, Pid} | Acc1], Pos + 1}
    end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(ReductionList, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]};
render_top_n_view(total_heap_size, HeapList, Num, Pages, Page) ->
    Title = ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, " TotalHeapSize", 14),
        ?W(?GRAY_BG, "Name or Initial Call", 38),
        ?W(?GRAY_BG, "           Reductions", 21),
        ?W(?GRAY_BG, " MsgQueue", 10),
        ?W(?GRAY_BG, "Current Function", 32)
    ]),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(HeapList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Pid, HeapSize, CurFun, NameOrCall} = get_top_n_info(Item),
        {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
        Format = get_memory_format(ChoosePos, Pos),
        R = io_lib:format(
            Format,
            [
                Pos,
                pid_to_list(Pid),
                observer_cli_lib:to_byte(HeapSize),
                NameOrCall,
                observer_cli_lib:to_list(Reductions),
                observer_cli_lib:to_list(MsgQueueLen),
                CurFun
            ]
        ),
        {[R | Acc], [{Pos, Pid} | Acc1], Pos + 1}
    end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(HeapList, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]};
render_top_n_view(message_queue_len, MQLenList, Num, Pages, Page) ->
    Title = ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, " MsgQueue", 11),
        ?W(?GRAY_BG, "Name or Initial Call", 37),
        ?W(?GRAY_BG, "      Memory", 13),
        ?W(?GRAY_BG, " Reductions", 21),
        ?W(?GRAY_BG, "Current Function", 33)
    ]),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(MQLenList)),
    FormatFunc = fun(Item, {Acc, Acc1, Pos}) ->
        {Pid, MQLen, CurFun, NameOrCall} = get_top_n_info(Item),
        {Reductions, Memory} = get_pid_info(Pid, [reductions, memory]),
        Format = get_message_queue_format(ChoosePos, Pos),
        R = io_lib:format(
            Format,
            [
                Pos,
                pid_to_list(Pid),
                observer_cli_lib:to_list(MQLen),
                NameOrCall,
                observer_cli_lib:to_byte(Memory),
                observer_cli_lib:to_list(Reductions),
                CurFun
            ]
        ),
        {[R | Acc], [{Pos, Pid} | Acc1], Pos + 1}
    end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(MQLenList, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]}.

top_n_rows(FormatFunc, Start, List) ->
    {Row, PidList, _} = lists:foldl(FormatFunc, {[], [], Start}, List),
    {Row, PidList}.

notify_pause_status() ->
    ?output("\e[31;1m PAUSE  INPUT (p, r/rr, b/bb, h/hh, m/mm) to resume or q to quit \e[0m~n").

get_memory_format(Pos, Pos) ->
    "|\e[42m~-3.3w|~-12.12s|~13.13s |~-38.38s|~21.21s| ~-9.9s|~-33.33s\e[49m|~n";
get_memory_format(_Pos, _RankPos) ->
    "|~-3.3w|~-12.12s|~13.13s |~-38.38s|~21.21s| ~-9.9s|~-33.33s|~n".

get_reduction_format(Pos, Pos) ->
    "|\e[42m~-3.3w|~-12.12s|~-21.21s|~-38.38s|~13.13s| ~-9.9s|~-34.34s\e[49m|~n";
get_reduction_format(_Pos, _RankPos) ->
    "|~-3.3w|~-12.12s|~-21.21s|~-38.38s|~13.13s| ~-9.9s|~-34.34s|~n".

get_message_queue_format(Pos, Pos) ->
    "|\e[42m~-3.3w|~-12.12s|~-11.11s|~-37.37s|~13.13s| ~-20.20s|~-34.34s\e[49m|~n";
get_message_queue_format(_Pos, _RankPos) ->
    "|~-3.3w|~-12.12s|~-11.11s|~-37.37s|~13.13s| ~-20.20s|~-34.34s|~n".

refresh_next_time(proc_count, Type, Interval) ->
    erlang:send_after(Interval, self(), {proc_count, Type});
refresh_next_time(proc_window, Type, _Interval) ->
    erlang:send_after(10, self(), {proc_window, Type}).

get_current_initial_call(Call) ->
    {_, CurFun} = lists:keyfind(current_function, 1, Call),
    {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
    {observer_cli_lib:mfa_to_list(CurFun), InitialCall}.

get_port_proc_info(PortLimit, ProcLimit) ->
    ProcCount = erlang:system_info(process_count),
    PortCount = erlang:system_info(port_count),
    PortCountStr = [integer_to_list(PortCount), "/", integer_to_list(PortLimit)],
    ProcCountStr = [integer_to_list(ProcCount), "/", integer_to_list(ProcLimit)],
    PortWarning =
        case PortCount > PortLimit * ?COUNT_ALARM_THRESHOLD of
            true -> ?RED;
            false -> <<"">>
        end,
    ProcWarning =
        case ProcCount > ProcLimit * ?COUNT_ALARM_THRESHOLD of
            true -> ?RED;
            false -> <<"">>
        end,
    {PortWarning, ProcWarning, PortCountStr, ProcCountStr}.

format_atom_info(AtomLimit, AtomCount) ->
    Atom = [integer_to_list(AtomCount), "/", integer_to_list(AtomLimit)],
    case AtomCount > AtomLimit * ?COUNT_ALARM_THRESHOLD of
        true -> {?RED, Atom};
        false -> {<<"">>, Atom}
    end.

warning_color(Percent) when Percent >= ?CPU_ALARM_THRESHOLD -> ?RED;
warning_color(_Percent) -> ?GREEN.

process_bar_format_style(Percents, IsLastLine) ->
    Format =
        case
            [
                begin
                    warning_color(P)
                end
                || P <- Percents
            ]
        of
            [W1, W2] ->
                <<"|", W1/binary, "|~2..0w ~-57.57s~s", W2/binary,
                    " |~2..0w ~-57.57s ~s \e[0m|~n">>;
            [W1, W2, W3, W4] ->
                <<"|", W1/binary, "|~-2.2w ~-22.22s ~s", W2/binary, " |~-2.2w ~-22.22s ~s",
                    W3/binary, " |~-2.2w ~-22.22s ~s", W4/binary, " |~-2.2w ~-23.23s ~s \e[0m|~n">>;
            [W1, W2, W3, W4, W5, W6, W7, W8, W9, W10] ->
                <<"|", W1/binary, " | ~-3.3w ~s", W2/binary, " | ~-3.3w ~s", W3/binary,
                    " | ~-3.3w ~s", W4/binary, " | ~-3.3w ~s", W5/binary, " | ~-3.3w ~s", W6/binary,
                    " |=====| ~-3.3w ~s", W7/binary, " | ~-3.3w ~s", W8/binary, " | ~-3.3w ~s",
                    W9/binary, " | ~-3.3w ~s", W10/binary, " | ~-3.3w ~s \e[0m|~n">>
        end,
    case IsLastLine of
        true -> <<?UNDERLINE/binary, Format/binary>>;
        false -> Format
    end.

get_top_n_info(Item) ->
    {Pid, Val, Call = [IsName | _]} = Item,
    {CurFun, InitialCall} = get_current_initial_call(Call),
    NameOrCall = display_name_or_initial_call(IsName, InitialCall, Pid),
    {Pid, Val, CurFun, NameOrCall}.

display_name_or_initial_call(IsName, _Call, _Pid) when is_atom(IsName) ->
    atom_to_list(IsName);
display_name_or_initial_call(_IsName, {proc_lib, init_p, 5}, Pid) ->
    %% translate gen_xxx behavior
    observer_cli_lib:mfa_to_list(proc_lib:translate_initial_call(Pid));
display_name_or_initial_call(_IsName, Call, _Pid) ->
    observer_cli_lib:mfa_to_list(Call).

get_refresh_prompt(proc_count, Type, Interval, Rows) ->
    io_lib:format("recon:proc_count(~p, ~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_prompt(proc_window, Type, Interval, Rows) ->
    io_lib:format("recon:proc_window(~p, ~w, ~w) Interval:~wms", [Type, Rows, Interval, Interval]).

get_stable_system_info() ->
    OtpRelease = erlang:system_info(otp_release),
    SysVersion = erlang:system_info(system_version) -- "\n",
    {[
            OtpRelease,
            SysVersion,
            erlang:system_info(process_limit),
            erlang:system_info(port_limit),
            erlang:system_info(ets_limit)
        ],
        erlang:system_info(port_parallelism)}.

get_atom_status() ->
    try erlang:system_info(atom_limit) of
        Limit ->
            Count = erlang:system_info(atom_count),
            {ok, Limit, Count}
    catch
        _:badarg -> {error, unsupported}
    end.

get_pid_info(Pid, Keys) ->
    case recon:info(Pid, Keys) of
        undefined -> {"die", "die"};
        [{_, Val1}, {_, Val2}] -> {Val1, Val2}
    end.

get_top_n(proc_window, Type, Interval, Rows, IsFirstTime) when not IsFirstTime ->
    recon:proc_window(Type, Rows, Interval);
get_top_n(_Func, Type, _Interval, Rows, _FirstTime) ->
    recon:proc_count(Type, Rows).

connect_error(Prompt, Node) ->
    Prop = <<?RED/binary, Prompt/binary, ?RESET/binary>>,
    ?output(Prop, [Node]).

start_process_view(StorePid, RenderPid, Opts = #view_opts{home = Home}, LastSchWallFlag, AutoJump) ->
    #home{cur_page = CurPage, pages = Pages, scheduler_usage = SchUsage} = Home,
    {_, CurPos} = lists:keyfind(CurPage, 1, Pages),
    case observer_cli_store:lookup_pos(StorePid, CurPos) of
        {CurPos, ChoosePid} ->
            clean([RenderPid, StorePid, LastSchWallFlag, SchUsage]),
            observer_cli_process:start(ChoosePid, Opts);
        {_, ChoosePid} when AutoJump ->
            clean([RenderPid, StorePid, LastSchWallFlag, SchUsage]),
            observer_cli_process:start(ChoosePid, Opts);
        _ ->
            manager(StorePid, RenderPid, Opts, LastSchWallFlag)
    end.

set_scheduler_wall_time(_Flag, ?DISABLE) -> false;
set_scheduler_wall_time(Flag, ?ENABLE) -> erlang:system_flag(scheduler_wall_time, Flag).

check_auto_row() ->
    case io:rows() of
        {ok, _} -> true;
        {error, _} -> false
    end.

node_stats({LastIn, LastOut, LastGCs, LastWords, LastScheduleWall}, SchUsage) ->
    New = {In, Out, GCs, Words, ScheduleWall} = get_incremental_stats(SchUsage),
    BytesInDiff = In - LastIn,
    BytesOutDiff = Out - LastOut,
    GCCountDiff = GCs - LastGCs,
    GCWordsDiff = Words - LastWords,
    {
        {
            [observer_cli_lib:to_byte(In), "/", observer_cli_lib:to_byte(BytesInDiff)],
            [observer_cli_lib:to_byte(Out), "/", observer_cli_lib:to_byte(BytesOutDiff)],
            [integer_to_list(GCs), "/", integer_to_list(GCCountDiff)],
            [integer_to_list(Words), "/", integer_to_list(GCWordsDiff)]
        },
        recon_lib:scheduler_usage_diff(LastScheduleWall, ScheduleWall),
        New
    }.

get_incremental_stats(SchUsage) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    ScheduleWall =
        case SchUsage of
            ?ENABLE -> erlang:statistics(scheduler_wall_time);
            ?DISABLE -> undefined
        end,
    {In, Out, GCs, Words, ScheduleWall}.
