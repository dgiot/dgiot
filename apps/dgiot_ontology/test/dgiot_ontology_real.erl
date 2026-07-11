%% 真实负载测试: gen_statem + 守卫 + ETS + 规则
-module(dgiot_ontology_real).
-behaviour(gen_statem).
-export([run/0]).
-export([init/1,callback_mode/0,handle_event/4,terminate/3]).

-define(TAB, real_shadow_tab).

run() ->
    io:format("=== 真实负载测试 (gen_statem + ETS + 守卫评估 + 状态转换) ===~n"),
    ets:new(?TAB, [named_table, public, set]),
    {ok, Pid} = gen_statem:start_link(?MODULE, latency, []),

    NormalCount = 10000,
    WarnCount = 500,
    CritCount = 100,
    EsdCount = 50,
    Total = NormalCount + WarnCount + CritCount + EsdCount,

    %% 生成混合测试数据: 97%正常 + 2%预警 + 0.8%严重 + 0.2%紧急
    Tests = lists:flatten([
        [{normal, rand:uniform() * 4.0} || _ <- lists:seq(1, NormalCount)],
        [{warn,   4.5 + rand:uniform() * 2.5} || _ <- lists:seq(1, WarnCount)],
        [{crit,   7.2 + rand:uniform() * 3.8} || _ <- lists:seq(1, CritCount)],
        [{esd,    11.2 + rand:uniform() * 5.0} || _ <- lists:seq(1, EsdCount)]
    ]),
    Shuffled = [V || {_, V} <- lists:sort([{rand:uniform(), T} || T <- Tests])],

    Samples = [begin
        V = lists:nth(I, Shuffled),
        _ = case I rem 5 of
            0 -> ets:insert(?TAB, {k, V});
            _ -> ets:lookup(?TAB, k)
        end,
        S = erlang:system_time(microsecond),
        gen_statem:call(Pid, {check, V}, 1000),
        erlang:system_time(microsecond) - S
    end || I <- lists:seq(1, Total)],

    gen_statem:stop(Pid),
    ets:delete(?TAB),
    Sorted = lists:sort(Samples),
    io:format("样本数: ~w~n", [Total]),
    io:format("p50=~w us  p95=~w us  p99=~w us  p999=~w us  max=~w us~n",
        [lists:nth(round(Total*0.50), Sorted),
         lists:nth(round(Total*0.95), Sorted),
         lists:nth(round(Total*0.99), Sorted),
         lists:nth(round(Total*0.999), Sorted),
         lists:last(Sorted)]),
    io:format("平均 = ~.1f us~n", [lists:sum(Samples) / length(Samples)]),
    Avg = lists:sum(Samples) / length(Samples),
    io:format("Throughput = ~w ops/sec (avg ~.1f us)~n", [round(1000000.0 / Avg), Avg]),
    ok.

%% --- gen_statem 真实影子 ---

init(_) -> {ok, normal, #{count => 0}}.

callback_mode() -> handle_event_function.

%% 正常: V<4.5 → 无动作 (97%)
handle_event({call, From}, {check, V}, normal, Data) when V =< 4.5 ->
    NewCnt = maps:get(count, Data, 0) + 1,
    {keep_state, Data#{count => NewCnt}, [{reply, From, ok}]};

%% 预警: 4.5<V=<7.1 → alarm (2%)
handle_event({call, From}, {check, V}, normal, Data) when V > 4.5, V =< 7.1 ->
    NewCnt1 = maps:get(count, Data, 0) + 1,
    {next_state, warning, Data#{count => NewCnt1, v => V}, [{reply, From, {warn, <<"振动超标">>}}]};

handle_event({call, From}, {check, V}, warning, Data) when V > 7.1, V =< 11.1 ->
    NewCnt2 = maps:get(count, Data, 0) + 1,
    {next_state, critical, Data#{count => NewCnt2, v => V}, [{reply, From, {critical, <<"即将停机">>}}]};

handle_event({call, From}, {check, V}, critical, Data) when V > 11.1 ->
    NewCnt3 = maps:get(count, Data, 0) + 1,
    {next_state, critical, Data#{count => NewCnt3, v => V}, [{reply, From, {esd, <<"紧急停机">>}}]};

%% 恢复: V<4.5
handle_event({call, From}, {check, V}, _State, Data) when V =< 4.5 ->
    NewCnt4 = maps:get(count, Data, 0) + 1,
    {next_state, normal, Data#{count => NewCnt4}, [{reply, From, ok}]};

handle_event({call, From}, {check, _V}, State, Data) ->
    {keep_state, Data, [{reply, From, {State, no_change}}]}.

terminate(_, _, _) -> ok.
