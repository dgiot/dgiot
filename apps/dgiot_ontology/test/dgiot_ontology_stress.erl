%% 三层本体压力测试: 独立运行·不依赖 registry
-module(dgiot_ontology_stress).
-behaviour(gen_statem).
-export([run/0, run/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

%% gen_statem callback (用于端到端延迟测试)
init(_) -> {ok, idle, #{}}.
callback_mode() -> handle_event_function.
handle_event({call, From}, {ping, _}, idle, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};
handle_event(_, _, _, _) -> keep_state_and_data.
terminate(_, _, _) -> ok.

-define(DEVICE_COUNT, 1000).
-define(SENSOR_HZ, 100).
-define(ITERATIONS, 10000).

run() -> run(#{devices => ?DEVICE_COUNT, iterations => ?ITERATIONS, sensor_hz => ?SENSOR_HZ}).

run(Opts) ->
    io:format("=========================================================~n"),
    io:format("  DG-IoT 三层本体压力测试 (纯 Erlang ~s)~n",
              [erlang:system_info(otp_release)]),
    io:format("=========================================================~n"),
    DevCnt = maps:get(devices, Opts, ?DEVICE_COUNT),

    io:format("~n[1/5] 并发进程启动...~n"),
    T1 = shadow_concurrency(DevCnt),
    io:format("  ~B 进程 in ~.2f ms | avg ~.2f μs/proc~n",
              [DevCnt, T1/1000, T1/DevCnt]),

    io:format("~n[2/5] 消息吞吐...~n"),
    T2 = message_throughput(DevCnt),
    Msgs = DevCnt * 100,
    io:format("  ~B msgs in ~.2f ms | ~.1fK msg/s~n",
              [Msgs, T2/1000, round(Msgs/(T2/1000))/1000]),

    io:format("~n[3/5] gen_statem 端到端延迟...~n"),
    T3 = end_to_end_latency(500),
    io:format("  p50=~w us  p95=~w us  p99=~w us~n",
              [element(1,T3), element(2,T3), element(3,T3)]),

    io:format("~n[4/5] ETS 并发读写...~n"),
    N = maps:get(iterations, Opts, ?ITERATIONS),
    T4 = ets_read_write(N),
    io:format("  ~B ops in ~.2f ms | ~.1fK ops/s~n",
              [N*2, T4/1000, round(N*2/(T4/1000))/1000]),

    io:format("~n[5/5] 进程消息路由...~n"),
    T5 = message_routing(10000),
    io:format("  ~B msgs routed in ~.2f ms | ~.1fK msg/s~n",
              [10000, T5/1000, round(10000/(T5/1000))/1000]),

    io:format("~n=========================================================~n"),
    io:format("  ~B 设备 · msg/s 测试完成~n", [DevCnt]),
    io:format("=========================================================~n").

%% 1. 进程启动速度
shadow_concurrency(N) ->
    Parent = self(),
    Start = erlang:system_time(microsecond),
    Pids = [spawn(fun() -> receive stop -> Parent ! {done, I} end end)
            || I <- lists:seq(1, N)],
    timer:sleep(10),
    [P ! stop || P <- Pids],
    [receive {done, _} -> ok end || _ <- lists:seq(1, N)],
    erlang:system_time(microsecond) - Start.

%% 2. 消息吞吐
message_throughput(N) ->
    Parent = self(),
    Workers = [spawn_link(fun() -> worker(Parent) end) || _ <- lists:seq(1, N)],
    Start = erlang:system_time(microsecond),
    [W ! {data, #{v => 0.5 + rand:uniform()*2}} || W <- Workers, _ <- lists:seq(1, 100)],
    [W ! stop || W <- Workers],
    [receive {done, _} -> ok end || _ <- Workers],
    erlang:system_time(microsecond) - Start.

worker(Parent) ->
    receive
        {data, _} -> worker(Parent);
        stop -> Parent ! {done, self()}
    end.

%% 3. gen_statem 同步调用延迟
end_to_end_latency(N) ->
    {ok, Pid} = gen_statem:start_link(?MODULE, latency, []),
    Samples = [begin
        S = erlang:system_time(microsecond),
        gen_statem:call(Pid, {ping, V}, 1000),
        erlang:system_time(microsecond) - S
    end || _ <- lists:seq(1, N), V <- [0.1 + rand:uniform()*8]],
    gen_statem:stop(Pid),
    Sorted = lists:sort(Samples),
    {lists:nth(round(N*0.5), Sorted),
     lists:nth(round(N*0.95), Sorted),
     lists:nth(round(N*0.99), Sorted)}.

%% 4. ETS 并发读写
ets_read_write(N) ->
    Tab = ets:new(stress, [public, set]),
    ets:insert(Tab, {k, 0}),
    Start = erlang:system_time(microsecond),
    Pids = [spawn(fun() ->
        [begin ets:lookup(Tab, k), ets:update_counter(Tab, k, 1) end
         || _ <- lists:seq(1, N div 10)]
    end) || _ <- lists:seq(1, 10)],
    [receive after infinity -> ok end || P <- Pids, monitor(process, P), receive {'DOWN', _, process, P, _} -> ok end],
    erlang:system_time(microsecond) - Start.

%% 5. 进程间消息路由
message_routing(N) ->
    Parent = self(),
    Router = spawn(fun() -> router(Parent) end),
    Start = erlang:system_time(microsecond),
    [Router ! {msg, I} || I <- lists:seq(1, N)],
    Router ! {done, self()},
    receive {routed, N} -> ok end,
    erlang:system_time(microsecond) - Start.

router(Parent) ->
    receive
        {msg, _} -> router(Parent);
        {done, From} -> From ! {routed, 0}
    end.
