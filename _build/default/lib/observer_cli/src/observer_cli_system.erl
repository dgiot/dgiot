%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").

%% API
-export([start/1]).
-export([clean/1]).

-define(UTIL_ALLOCATORS, [
    binary_alloc,
    driver_alloc,
    eheap_alloc,
    ets_alloc,
    fix_alloc,
    ll_alloc,
    sl_alloc,
    std_alloc,
    temp_alloc
]).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system
%%  List Memory Allocators: std, ll, eheap, ets, fix, binary, driver.
-spec start(ViewOpts) -> no_return when ViewOpts :: view_opts().
start(#view_opts{sys = #system{interval = Interval}} = ViewOpts) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        Cmd = io_lib:format("ps -o pcpu,pmem,rss,vsz ~s", [os:getpid()]),
        render_worker(Cmd, Interval, ?INIT_TIME_REF)
    end),
    manager(Pid, ViewOpts).

-spec clean(list()) -> ok.
clean(Pids) -> observer_cli_lib:exit_processes(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, #view_opts{sys = AllocatorOpts} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [Pid]) of
        quit ->
            erlang:send(Pid, quit);
        {new_interval, NewInterval} ->
            erlang:send(Pid, {new_interval, NewInterval}),
            NewAllocate = AllocatorOpts#system{interval = NewInterval},
            manager(Pid, ViewOpts#view_opts{sys = NewAllocate});
        _ ->
            manager(Pid, ViewOpts)
    end.

render_worker(Cmd, Interval, LastTimeRef) ->
    CacheHitInfo = recon_alloc:cache_hit_rates(),
    AverageBlockCurs = recon_alloc:average_block_sizes(current),
    AverageBlockMaxes = recon_alloc:average_block_sizes(max),
    SbcsToMbcsCurs = observer_cli_lib:sbcs_to_mbcs(
        ?UTIL_ALLOCATORS,
        recon_alloc:sbcs_to_mbcs(current)
    ),
    SbcsToMbcsMaxs = observer_cli_lib:sbcs_to_mbcs(?UTIL_ALLOCATORS, recon_alloc:sbcs_to_mbcs(max)),
    Sys = render_sys_info(Cmd),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(allocator, Text),
    BlockView = render_block_size_info(
        AverageBlockCurs,
        AverageBlockMaxes,
        SbcsToMbcsCurs,
        SbcsToMbcsMaxs
    ),
    HitView = render_cache_hit_rates(CacheHitInfo, erlang:length(CacheHitInfo)),
    LastLine = observer_cli_lib:render_last_line("q(quit)"),
    ?output([?CURSOR_TOP, Menu, Sys, BlockView, HitView, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> render_worker(Cmd, NewInterval, NextTimeRef);
        redraw -> render_worker(Cmd, Interval, NextTimeRef)
    end.

render_cache_hit_rates(CacheHitInfo, Len) when Len =< 8 ->
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("Instance", 8),
        ?W("Hits", 10),
        ?W("Calls", 11),
        ?W("Hit Rate", 98)
    ]),
    View = [
        begin
            [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value(
                {instance, Seq},
                CacheHitInfo
            ),
            HitRateStr = observer_cli_lib:to_percent(HitRate),
            SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
            TrueHitRate =
                case Hit == 0 andalso Call == 0 of
                    true -> 0;
                    false -> HitRate
                end,
            Process = lists:duplicate(trunc(TrueHitRate * 91), "|"),
            ?render([
                ?W(SeqStr, 8),
                ?W(Hit, 10),
                ?W(Call, 11),
                ?W(Process, 89),
                ?W(HitRateStr, 6)
            ])
        end
        || Seq <- lists:seq(0, Len - 1)
    ],
    [Title | View];
render_cache_hit_rates(CacheHitInfo, Len) ->
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        "IN|",
        ?W(" Hits/Calls", 20),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", 20),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", 20),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", 19),
        ?W("HitRate", 6)
    ]),
    Num = Len div 4,
    Rows = [
        begin
            Seq2 = Seq1 + Num,
            Seq3 = Seq2 + Num,
            Seq4 = Seq3 + Num,
            {SeqStr1, Hit1, Call1, HitRateStr1} = get_cachehit_info(Seq1, CacheHitInfo),
            {SeqStr2, Hit2, Call2, HitRateStr2} = get_cachehit_info(Seq2, CacheHitInfo),
            {SeqStr3, Hit3, Call3, HitRateStr3} = get_cachehit_info(Seq3, CacheHitInfo),
            {SeqStr4, Hit4, Call4, HitRateStr4} = get_cachehit_info(Seq4, CacheHitInfo),
            ?render([
                SeqStr1,
                ?W([Hit1, "/", Call1], 19),
                ?W(HitRateStr1, 6),
                SeqStr2,
                ?W([Hit2, "/", Call2], 19),
                ?W(HitRateStr2, 6),
                SeqStr3,
                ?W([Hit3, "/", Call3], 19),
                ?W(HitRateStr3, 6),
                SeqStr4,
                ?W([Hit4, "/", Call4], 18),
                ?W(HitRateStr4, 6)
            ])
        end
        || Seq1 <- lists:seq(1, Num)
    ],
    [Title | Rows].

render_block_size_info(AverageBlockCurs, AverageBlockMaxes, SbcsToMbcsCurs, SbcsToMbcsMaxs) ->
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("Allocator Type", 16),
        ?W("Current Mbcs", 16),
        ?W("Max Mbcs", 16),
        ?W("Current Sbcs", 16),
        ?W("Max Sbcs", 16),
        ?W("Current SbcsToMbcs", 19),
        ?W("Max SbcsToMbcs", 19)
    ]),
    View = [
        begin
            [Type, CMC, MMC, CSC, MSBC, CSTM, MSTM] =
                get_alloc(
                    AllocKey,
                    AverageBlockCurs,
                    AverageBlockMaxes,
                    SbcsToMbcsCurs,
                    SbcsToMbcsMaxs
                ),
            ?render([
                ?W(Type, 16),
                ?W(CMC, 16),
                ?W(MMC, 16),
                ?W(CSC, 16),
                ?W(MSBC, 16),
                ?W(CSTM, 19),
                ?W(MSTM, 19)
            ])
        end
        || AllocKey <- ?UTIL_ALLOCATORS
    ],
    [Title | View].

get_alloc(Key, Curs, Maxes, STMCurs, STMMaxes) ->
    CurRes = proplists:get_value(Key, Curs),
    MaxRes = proplists:get_value(Key, Maxes),
    CurMbcs = proplists:get_value(mbcs, CurRes),
    CurSbcs = proplists:get_value(sbcs, CurRes),
    MaxMbcs = proplists:get_value(mbcs, MaxRes),
    MaxSbcs = proplists:get_value(sbcs, MaxRes),
    [
        atom_to_list(Key),
        observer_cli_lib:to_byte(CurMbcs),
        observer_cli_lib:to_byte(MaxMbcs),
        observer_cli_lib:to_byte(CurSbcs),
        observer_cli_lib:to_byte(MaxSbcs),
        proplists:get_value(Key, STMCurs),
        proplists:get_value(Key, STMMaxes)
    ].

render_sys_info(Cmd) ->
    SysInfo = sys_info(Cmd),
    {Info, Stat} = info_fields(),
    SystemAndCPU = fill_info(Info, SysInfo),
    MemAndStatistics = fill_info(Stat, SysInfo),
    System = proplists:get_value("System and Architecture", SystemAndCPU),
    CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
    {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
    {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
    render_sys_info(System, CPU, Memory, Statistics).

render_sys_info(System, CPU, Memory, Statistics) ->
    Title = ?render([
        ?GRAY_BG,
        ?UNDERLINE,
        ?W("System/Architecture", 22),
        ?W("State", 8),
        ?W("CPU's and Threads", 23),
        ?W("State", 7),
        ?W("Memory Usage", 11),
        ?W("State", 22),
        ?W("Statistics", 11),
        ?W("State", 11)
    ]),
    NewSystem = [
        begin
            {Key, Value}
        end
        || {Key, Value} <- System,
           Key =/= "Compiled for" andalso Key =/= "smp Support"
    ],
    [{_, TotalMem} | _R] = Memory,
    {bytes, TotalMemInt} = TotalMem,
    Row = [
        begin
            {SysKey, SysVal} = lists:nth(Pos, NewSystem),
            {CpuKey, CpuVal} = lists:nth(Pos, CPU),
            {MemKey, MemVal} = lists:nth(Pos, Memory),
            {StatisticsKey, StatisticsVal} = lists:nth(Pos, Statistics),

            {bytes, MemValInt} = MemVal,
            Percent = observer_cli_lib:to_percent(MemValInt / TotalMemInt),
            ?render([
                ?W(SysKey, 22),
                ?W(to_list(SysVal), 8),
                ?W(CpuKey, 23),
                ?W(to_list(CpuVal), 7),
                ?W(MemKey, 11),
                ?W(observer_cli_lib:to_byte(MemValInt), 13),
                ?W(Percent, 6),
                ?W(StatisticsKey, 11),
                ?W(to_list(StatisticsVal), 11)
            ])
        end
        || Pos <- lists:seq(1, 6)
    ],
    Compile = ?render([
        ?UNDERLINE,
        ?W("compiled for", 22),
        ?W(to_list(proplists:get_value("Compiled for", System)), 111)
    ]),
    [Title | (Row ++ [Compile])].

sys_info(Cmd) ->
    MemInfo =
        try erlang:memory() of
            Mem -> Mem
        catch
            _:_ -> []
        end,

    SchedulersOnline = erlang:system_info(schedulers_online),
    SchedulersAvailable =
        case erlang:system_info(multi_scheduling) of
            enabled -> SchedulersOnline;
            _ -> 1
        end,
    [_, CmdValue | _] = string:split(os:cmd(Cmd), "\n", all),
    [CpuPsV, MemPsV, RssPsV, VszPsV] =
        case lists:filter(fun(Y) -> Y =/= [] end, string:split(CmdValue, " ", all)) of
            [] -> ["--", "--", "--", "--"];
            [V1, V2, V3, V4] -> [V1, V2, V3, V4]
        end,

    {{_, Input}, {_, Output}} = erlang:statistics(io),
    [
        {ps_cpu, CpuPsV ++ "%"},
        {ps_mem, MemPsV ++ "%"},
        {ps_rss, list_to_integer(RssPsV) * 1024},
        {ps_vsz, list_to_integer(VszPsV) * 1024},
        {io_input, Input},
        {io_output, Output},

        {logical_processors, erlang:system_info(logical_processors)},
        {logical_processors_online, erlang:system_info(logical_processors_online)},
        {logical_processors_available, erlang:system_info(logical_processors_available)},
        {schedulers, erlang:system_info(schedulers)},
        {schedulers_online, SchedulersOnline},
        {schedulers_available, SchedulersAvailable},

        {otp_release, erlang:system_info(otp_release)},
        {version, erlang:system_info(version)},
        {system_architecture, erlang:system_info(system_architecture)},
        {kernel_poll, erlang:system_info(kernel_poll)},
        {smp_support, erlang:system_info(smp_support)},
        {threads, erlang:system_info(threads)},
        {thread_pool_size, erlang:system_info(thread_pool_size)},
        {wordsize_internal, erlang:system_info({wordsize, internal})},
        {wordsize_external, erlang:system_info({wordsize, external})},
        {alloc_info, alloc_info()}
        | MemInfo
    ].

alloc_info() ->
    Alloc = erlang:system_info(alloc_util_allocators),
    try erlang:system_info({allocator_sizes, Alloc}) of
        Allocators -> Allocators
    catch
        _:_ -> []
    end.

fill_info([{dynamic, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        {Str, Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, SubStructure} | Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([{Str, Attrib, SubStructure} | Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([], _) ->
    [].

to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) -> observer_cli_lib:to_byte(Val);
to_list(Val) -> Val.

info_fields() ->
    Info = [
        {"System and Architecture", [
            {"System Version", otp_release},
            {"Erts Version", version},
            {"Compiled for", system_architecture},
            {"Emulator Wordsize", wordsize_external},
            {"Process Wordsize", wordsize_internal},
            {"Smp Support", smp_support},
            {"Thread Support", threads},
            {"Async thread pool size", thread_pool_size}
        ]},
        {"CPU's and Threads", [
            {"Logical CPU's", logical_processors},
            {"Online Logical CPU's", logical_processors_online},
            {"Available Logical CPU's", logical_processors_available},
            {"Schedulers", schedulers},
            {"Online schedulers", schedulers_online},
            {"Available schedulers", schedulers_available}
        ]}
    ],
    Stat = [
        {"Memory Usage", right, [
            {"Total", {bytes, total}},
            {"Processes", {bytes, processes}},
            {"Atoms", {bytes, atom}},
            {"Binaries", {bytes, binary}},
            {"Code", {bytes, code}},
            {"Ets", {bytes, ets}}
        ]},
        {"Statistics", right, [
            {"ps -o pcpu", ps_cpu},
            {"ps -o pmem", ps_mem},
            {"ps -o rss", {bytes, ps_rss}},
            {"ps -o vsz", {bytes, ps_vsz}},
            {"Total IOIn", {bytes, io_input}},
            {"Total IOOut", {bytes, io_output}}
        ]}
    ],
    {Info, Stat}.

get_cachehit_info(Seq, CacheHitInfo) ->
    [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value(
        {instance, Seq},
        CacheHitInfo
    ),
    SeqStr = lists:flatten(io_lib:format("\e[92m~2..0w| \e[0m", [Seq])),
    HitRateStr = observer_cli_lib:to_percent(HitRate),
    {SeqStr, integer_to_list(Hit), integer_to_list(Call), HitRateStr}.
