-module(observer_cli_process).

-include("observer_cli.hrl").

-export([start/2]).

%% lists:foldl(fun(_X, Acc) -> queue:in('NaN', Acc) end, queue:new(), lists:seq(1, 5))
-define(INIT_QUEUE, {['NaN', 'NaN', 'NaN', 'NaN'], ['NaN']}).

-spec start(pid(), view_opts()) -> no_return.
start(Pid, Opts) ->
    #view_opts{process = #process{interval = RefreshMs}} = Opts,
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(info, RefreshMs, Pid, ?INIT_TIME_REF, ?INIT_QUEUE, ?INIT_QUEUE)
    end),
    manager(RenderPid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(RenderPid, #view_opts{process = ProcOpts} = Opts) ->
    case parse_cmd(Opts, RenderPid) of
        quit ->
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            manager(RenderPid, Opts#view_opts{process = ProcOpts#process{interval = NewInterval}});
        ViewAction ->
            erlang:send(RenderPid, ViewAction),
            manager(RenderPid, Opts)
    end.

render_worker(info, Interval, Pid, TimeRef, RedQ, MemQ) ->
    ProcessInfo = recon:info(Pid),
    Meta = proplists:get_value(meta, ProcessInfo),
    case Meta of
        undefined ->
            output_die_view(Pid, Interval),
            next_draw_view(info, TimeRef, Interval, Pid, RedQ, MemQ);
        _ ->
            RegisteredName = proplists:get_value(registered_name, Meta),
            GroupLeader = proplists:get_value(group_leader, Meta),
            Status = proplists:get_value(status, Meta),

            Signals = proplists:get_value(signals, ProcessInfo),
            Link = proplists:get_value(links, Signals),
            Monitors = proplists:get_value(monitors, Signals),
            MonitoredBy = proplists:get_value(monitored_by, Signals),
            TrapExit = proplists:get_value(trap_exit, Signals),

            Location = proplists:get_value(location, ProcessInfo),
            InitialCall = proplists:get_value(initial_call, Location),

            MemoryUsed = proplists:get_value(memory_used, ProcessInfo),
            Memory = proplists:get_value(memory, MemoryUsed),
            MessageQueueLen = proplists:get_value(message_queue_len, MemoryUsed),
            HeapSize = proplists:get_value(heap_size, MemoryUsed),
            TotalHeapSize = proplists:get_value(total_heap_size, MemoryUsed),
            GarbageCollection = proplists:get_value(garbage_collection, MemoryUsed),

            Work = proplists:get_value(work, ProcessInfo),
            Reductions = proplists:get_value(reductions, Work),

            Menu = render_menu(info, Interval),

            Line1 = render_process_info(
                Pid,
                RegisteredName,
                GroupLeader,
                Status,
                TrapExit,
                InitialCall,
                MessageQueueLen,
                HeapSize,
                TotalHeapSize,
                GarbageCollection
            ),

            Line2 = render_link_monitor(Link, Monitors, MonitoredBy),

            {NewRedQ, NewMemQ, Line3} = render_reduction_memory(Reductions, Memory, RedQ, MemQ),

            LastLine = render_last_line(),

            ?output([?CURSOR_TOP, Menu, Line1, Line2, Line3, LastLine]),
            next_draw_view(info, TimeRef, Interval, Pid, NewRedQ, NewMemQ)
    end;
render_worker(message, Interval, Pid, TimeRef, RedQ, MemQ) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            Line =
                if
                    Len =:= 0 ->
                        "\e[32;1mNo messages were found.\e[0m\n";
                    Len > 10000 ->
                        io_lib:format("\e[31mToo many message(~w)!\e[0m~n", [Len]);
                    true ->
                        {messages, Messages} = recon:info(Pid, messages),
                        [
                            io_lib:format("~p Message Len:~p~n", [Pid, Len]),
                            truncate_str(Messages)
                        ]
                end,
            Menu = render_menu(message, Interval),
            LastLine = render_last_line(),
            ?output([?CURSOR_TOP, Menu, Line, LastLine]),
            next_draw_view(message, TimeRef, Interval, Pid, RedQ, MemQ);
        undefined ->
            render_worker(info, Interval, Pid, ?INIT_TIME_REF, ?INIT_QUEUE, ?INIT_QUEUE)
    end;
render_worker(dict, Interval, Pid, TimeRef, RedQ, MemQ) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, List} ->
            Len = erlang:length(List),
            Line1 = io_lib:format(
                "erlang:process_info(~p, dictionary). dictionary_len:~p       ~n",
                [Pid, Len]
            ),
            Line2 =
                case Len of
                    0 -> "\e[32;1mNo dictionary was found\e[0m\n";
                    _ -> truncate_str(List)
                end,
            Menu = render_menu(dict, Interval),
            LastLine = render_last_line(),
            ?output([?CURSOR_TOP, Menu, Line1, Line2, LastLine]),
            next_draw_view(dict, TimeRef, Interval, Pid, RedQ, MemQ);
        undefined ->
            render_worker(info, Interval, Pid, ?INIT_TIME_REF, ?INIT_QUEUE, ?INIT_QUEUE)
    end;
render_worker(stack, Interval, Pid, TimeRef, RedQ, MemQ) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, StackTrace} ->
            Menu = render_menu(stack, Interval),
            Prompt = io_lib:format("erlang:process_info(~p, current_stacktrace).      ~n", [Pid]),
            LastLine = render_last_line(),
            {_, Line} =
                lists:foldr(
                    fun({Mod, Func, Arity, Location}, {Nth, Acc}) ->
                        Mfa = observer_cli_lib:mfa_to_list({Mod, Func, Arity}),
                        File = proplists:get_value(file, Location, "undefined"),
                        Line = proplists:get_value(line, Location, 0),
                        FileLine = File ++ ":" ++ erlang:integer_to_list(Line),
                        case Nth =:= 1 of
                            false -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62), ?NEW_LINE | Acc]};
                            true -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62) | Acc]}
                        end
                    end,
                    {1, []},
                    lists:sublist(StackTrace, 30)
                ),
            ?output([?CURSOR_TOP, Menu, Prompt, ?render(Line), LastLine]),
            next_draw_view(stack, TimeRef, Interval, Pid, RedQ, MemQ);
        undefined ->
            render_worker(info, Interval, Pid, ?INIT_TIME_REF, ?INIT_QUEUE, ?INIT_QUEUE)
    end;
render_worker(state, Interval, Pid, TimeRef, RedQ, MemQ) ->
    case render_state(Pid, Interval) of
        ok -> next_draw_view(state, TimeRef, Interval, Pid, RedQ, MemQ);
        error -> next_draw_view_2(state, TimeRef, Interval, Pid, RedQ, MemQ)
    end.

next_draw_view(Status, TimeRef, Interval, Pid, NewRedQ, NewMemQ) ->
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    next_draw_view_2(Status, NewTimeRef, Interval, Pid, NewRedQ, NewMemQ).

next_draw_view_2(Status, TimeRef, Interval, Pid, NewRedQ, NewMemQ) ->
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(Status, NewInterval, Pid, TimeRef, NewRedQ, NewMemQ);
        info_view ->
            ?output(?CLEAR),
            render_worker(info, Interval, Pid, TimeRef, NewRedQ, NewMemQ);
        message_view ->
            ?output(?CLEAR),
            render_worker(message, Interval, Pid, TimeRef, NewRedQ, NewMemQ);
        dict_view ->
            ?output(?CLEAR),
            render_worker(dict, Interval, Pid, TimeRef, NewRedQ, NewMemQ);
        stack_view ->
            ?output(?CLEAR),
            render_worker(stack, Interval, Pid, TimeRef, NewRedQ, NewMemQ);
        state_view ->
            ?output(?CLEAR),
            render_worker(state, Interval, Pid, TimeRef, NewRedQ, NewMemQ);
        _Msg ->
            render_worker(Status, Interval, Pid, TimeRef, NewRedQ, NewMemQ)
    end.

render_process_info(
    Pid,
    RegisteredName,
    GroupLeader,
    Status,
    TrapExit,
    InitialCall,
    MessageQueueLen,
    HeapSize,
    TotalHeapSize,
    GarbageCollection
) ->
    MinBinVHeapSize = proplists:get_value(min_bin_vheap_size, GarbageCollection),
    MinHeapSize = proplists:get_value(min_heap_size, GarbageCollection),
    FullSweepAfter = proplists:get_value(fullsweep_after, GarbageCollection),
    MinorGcs = integer_to_list(proplists:get_value(minor_gcs, GarbageCollection)),

    InitialCallStr = observer_cli_lib:mfa_to_list(InitialCall),
    GroupLeaderStr = erlang:pid_to_list(GroupLeader),
    PidStr = erlang:pid_to_list(Pid),
    Name =
        case RegisteredName of
            "" -> PidStr;
            _ -> PidStr ++ "/" ++ erlang:atom_to_list(RegisteredName)
        end,
    MessageQueueLenStr = erlang:integer_to_list(MessageQueueLen),
    MessageQueueLenColor =
        case MessageQueueLen > 0 of
            true -> ?RED;
            false -> ?GREEN
        end,

    [
        ?render([
            ?GRAY_BG,
            ?W("Meta", 16),
            ?W("Value", 42),
            ?W("Memory Used", 16),
            ?W("Value", 12),
            ?W("Garbage Collection", 18),
            ?W("Value", 12)
        ]),
        ?render([
            ?W("registered_name", 16),
            ?W(Name, 42),
            ?W("msg_queue_len", 16),
            ?W2(MessageQueueLenColor, MessageQueueLenStr, 13),
            ?W(" min_bin_vheap_size", 19),
            ?W({byte, MinBinVHeapSize}, 12),
            ?NEW_LINE,
            ?W("initial_call", 16),
            ?W(InitialCallStr, 42),
            ?W("heap_size", 16),
            ?W({byte, HeapSize}, 12),
            ?W("min_heap_size", 18),
            ?W({byte, MinHeapSize}, 12),
            ?NEW_LINE,
            ?W("group_leader", 16),
            ?W(GroupLeaderStr, 42),
            ?W("total_heap_size", 16),
            ?W({byte, TotalHeapSize}, 12),
            ?W("fullsweep_after", 18),
            ?W({byte, FullSweepAfter}, 12),
            ?NEW_LINE,
            ?W("status", 16),
            ?W(Status, 42),
            ?W("trap_exit", 16),
            ?W(TrapExit, 12),
            ?W("minor_gcs", 18),
            ?W(MinorGcs, 12)
        ])
    ].

render_link_monitor(Link, Monitors, MonitoredBy) ->
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
    MonitoredByStr = [
        begin
            observer_cli_lib:to_list(P)
        end
        || P <- lists:sublist(MonitoredBy, 30)
    ],
    LinkInfo = "Links(" ++ erlang:integer_to_list(erlang:length(Link)) ++ ")",
    MonitorInfo = "Monitors(" ++ erlang:integer_to_list(erlang:length(Monitors)) ++ ")",
    MonitoredByInfo = "MonitoredBy(" ++ erlang:integer_to_list(erlang:length(MonitoredBy)) ++ ")",
    ?render([
        ?W(LinkInfo, 16),
        ?W(LinkStr, 112),
        ?NEW_LINE,
        ?W(MonitorInfo, 16),
        ?W(MonitorsStr, 112),
        ?NEW_LINE,
        ?W(MonitoredByInfo, 16),
        ?W(MonitoredByStr, 112)
    ]).

render_reduction_memory(Reduction, Memory, ReductionQ, MemoryQ) ->
    {NewRed, NewMem} =
        case queue:len(ReductionQ) >= 20 of
            true ->
                RestRed = queue:tail(ReductionQ),
                RestMem = queue:tail(MemoryQ),
                {queue:in(Reduction, RestRed), queue:in(Memory, RestMem)};
            false ->
                {queue:in(Reduction, ReductionQ), queue:in(Memory, MemoryQ)}
        end,
    View = [
        io_lib:format("|Reductions: ~120.120s|~n", [get_chart_format(NewRed)]),
        io_lib:format("|Memorys: ~123.123s|~n", [get_chart_format(NewMem)])
    ],
    {NewRed, NewMem, View}.

render_last_line() ->
    io_lib:format("|\e[7mq(quit) ~124.124s\e[0m|~n", [" "]).

get_chart_format(Queue) ->
    List = queue:to_list(Queue),
    chart_format(List, "").

chart_format([_R], Lines) ->
    Lines;
chart_format([R, R | RestRed], Lines) ->
    chart_format([R | RestRed], Lines ++ observer_cli_lib:to_list(R) ++ "->");
chart_format([R1, R2 | RestRed], Lines) when R1 > R2 ->
    chart_format([R2 | RestRed], Lines ++ observer_cli_lib:to_list(R1) ++ "->");
chart_format([R1, R2 | RestRed], Lines) when R1 < R2 ->
    chart_format([R2 | RestRed], Lines ++ observer_cli_lib:to_list(R1) ++ "->").

render_menu(Type, Interval) ->
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Title = get_menu_title(Type),
    UpTime = observer_cli_lib:uptime(),
    TitleWidth = ?COLUMN + 104 - erlang:length(UpTime),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

get_menu_title(Type) ->
    [Home, Process, Messages, Dict, Stack, State] = get_menu_title2(Type),
    [Home, "|", Process, "|", Messages, "|", Dict, "|", Stack, "|", State, "|"].

get_menu_title2(info) ->
    [
        ?UNSELECT("Home(H)"),
        ?SELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(message) ->
    [
        ?UNSELECT("Home(H)"),
        ?UNSELECT("Process Info(P)"),
        ?SELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(dict) ->
    [
        ?UNSELECT("Home(H)"),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?SELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(stack) ->
    [
        ?UNSELECT("Home(H)"),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?SELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(state) ->
    [
        ?UNSELECT("Home(H)"),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?SELECT("State(S)")
    ].

parse_cmd(ViewOpts, Pid) ->
    case observer_cli_lib:to_list(io:get_line("")) of
        "q\n" ->
            quit;
        "Q\n" ->
            quit;
        "P\n" ->
            info_view;
        "M\n" ->
            message_view;
        "D\n" ->
            dict_view;
        "C\n" ->
            stack_view;
        "S\n" ->
            state_view;
        "H\n" ->
            erlang:exit(Pid, stop),
            observer_cli:start(ViewOpts);
        Number ->
            observer_cli_lib:parse_integer(Number)
    end.

render_state(Pid, Interval) ->
    Menu = render_menu(state, Interval),
    PromptRes = io_lib:format("recon:get_state(~p, 2500).                            ~n", [Pid]),
    PromptBefore = io_lib:format("\e[32;1mWaiting recon:get_state(~p, 2500) return...\e[0m~n", [Pid]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, PromptBefore]),
    try
        State = recon:get_state(Pid, 2500),
        Line = truncate_str(State),
        ?output([?CURSOR_TOP, Menu, PromptRes, Line, LastLine]),
        ok
    catch
        _Err:_Reason ->
            Error =
                "Information could not be retrieved, system messages may not be handled by this process.\n",
            ?output([?CURSOR_TOP, Menu, PromptRes, Error, LastLine]),
            error
    end.

output_die_view(Pid, Interval) ->
    Menu = render_menu(info, Interval),
    Line = io_lib:format("\e[31mProcess(~p) has already die.\e[0m~n", [Pid]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

truncate_str(Input) ->
    lists:sublist(lists:flatten(io_lib:format("~W", [Input, 50 * 30])), 140 * 30) ++ "\n".
