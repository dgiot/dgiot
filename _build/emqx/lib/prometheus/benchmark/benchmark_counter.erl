-module(benchmark_counter).

-export([
         run/0
        ]).
-compile(inline).

-define(COUNT, 3 * 1000*1000).
-define(TIMEOUT, 100*1000).
-define(MILLION, 1000*1000).
-define(MILLIONS(C), C * ?MILLION).

setup() ->
  prometheus:start(),
  prometheus_counter:declare([{name, test_counter},
                              {help, ""}]).

run() ->
  io:format("Setting up environment~n"),
  setup(),
  io:format("Finished~n"),
  run_single_counter(),
  run_multi_counter(),
  run_semimulti_counter().

run_single_counter() ->

  prometheus_counter:reset(test_counter),

  run_test(1, ?MILLIONS(1), ?MILLIONS(1)),
  run_test(5, ?MILLIONS(1), ?MILLIONS(6)),
  run_test(10, ?MILLIONS(1), ?MILLIONS(16)),
  run_test(100, ?MILLIONS(1), ?MILLIONS(116)),

  io:format("Finished~n").

run_test(Pids, Count, ExpectedValue) ->
  io:format("~s~n"
            "  Pids: ~p~n"
            "  Iterations: ~p~n"
            "  Expected Value: ~p~n"
            "  Duration (sec): --",
            [color:blueb("Running single counter benchmark:"),
             Pids, Count, ExpectedValue]),
  Time = run_int(Pids, Count),
  Value = case prometheus_counter:value(test_counter) of
            ExpectedValue -> color:greenb(io_lib:format("~p", [ExpectedValue]));
            CValue -> color:redb(io_lib:format("~p", [CValue]))
          end,
  io:format("  Counter value: ~s~n", [Value]),
  {Time, prometheus_counter:value(test_counter)}.

run_int(PCount, Count) ->
  StartTime = erlang:monotonic_time(),
  Mgr = self(),
  Printer = spawn_link(fun() -> printer_loop(StartTime, PCount * Count) end),
  Pids = [spawn_link(fun() -> loop(Mgr, Printer, Count, test_counter) end) || _ <- lists:seq(1, PCount)],
  collect(Printer, Pids).

run_multi_counter() ->
  run_multi_test(1, ?MILLIONS(1), ?MILLIONS(1)),
  run_multi_test(5, ?MILLIONS(1), ?MILLIONS(5)),
  run_multi_test(10, ?MILLIONS(1), ?MILLIONS(10)),
  run_multi_test(100, ?MILLIONS(1), ?MILLIONS(100)),

  io:format("Finished~n").

multi_counter_name(N) ->
  list_to_binary(io_lib:format("counter_~p", [N])).

init_multi_counter(N) ->
  prometheus_counter:declare([{name, multi_counter_name(N)},
                              {help, ""}]),
  prometheus_counter:reset(multi_counter_name(N)),
  multi_counter_name(N).

run_multi_test(Pids, Count, ExpectedValue) ->
  io:format("~s~n"
            "  Pids (counters): ~p~n"
            "  Iterations: ~p~n"
            "  Expected sum: ~p~n"
            "  Duration (sec): --",
            [color:blueb("Running multi counter benchmark:"),
             Pids, Count, ExpectedValue]),

  [init_multi_counter(N) || N <- lists:seq(1, Pids)],

  Time = run_int_multi(Pids, Count),
  Sum = lists:sum([prometheus_counter:value(multi_counter_name(N)) || N <- lists:seq(1, Pids)]),
  Value = case Sum of
            ExpectedValue -> color:greenb(io_lib:format("~p", [ExpectedValue]));
            CValue -> color:redb(io_lib:format("~p", [CValue]))
          end,
  io:format("  Counters sum: ~s~n", [Value]),
  {Time, Sum}.

run_int_multi(PCount, Count) ->
  StartTime = erlang:monotonic_time(),
  Mgr = self(),
  Printer = spawn_link(fun() -> printer_loop(StartTime, PCount * Count) end),
  Pids = [spawn_link(fun() -> loop(Mgr, Printer,  Count, multi_counter_name(N)) end) || N <- lists:seq(1, PCount)],
  collect(Printer, Pids).

run_semimulti_counter() ->
  run_semimulti_test(1, ?MILLIONS(1), ?MILLIONS(1)),
  run_semimulti_test(5, ?MILLIONS(1), ?MILLIONS(5)),
  run_semimulti_test(10, ?MILLIONS(1), ?MILLIONS(10)),
  run_semimulti_test(100, ?MILLIONS(1), ?MILLIONS(100)),

  io:format("Finished~n").

run_semimulti_test(Pids, Count, ExpectedValue) ->
  Counters = [init_multi_counter(floor(N, 2)) || N <- lists:seq(1, Pids)],

  io:format("~s~n"
            "  Pids (counters): ~p(~p)~n"
            "  Iterations: ~p~n"
            "  Expected sum: ~p~n"
            "  Duration (sec): --",
            [color:blueb("Running semimulti counter benchmark:"),
             Pids, length(lists:usort(Counters)), Count, ExpectedValue]),


  Time = run_int_semimulti(Pids, Counters, Count),
  Sum = lists:sum([prometheus_counter:value(Counter) || Counter <- lists:usort(Counters)]),
  Value = case Sum of
            ExpectedValue -> color:greenb(io_lib:format("~p", [ExpectedValue]));
            CValue -> color:redb(io_lib:format("~p", [CValue]))
          end,
  io:format("  Counters sum: ~s~n", [Value]),
  {Time, Sum}.

run_int_semimulti(PCount, Counters, Count) ->
  StartTime = erlang:monotonic_time(),
  Mgr = self(),
  Printer = spawn_link(fun() -> printer_loop(StartTime, PCount * Count) end),
  Pids = [spawn_link(fun() -> loop(Mgr, Printer, Count, Counter) end) || Counter <- Counters],
  collect(Printer, Pids).


collect(Printer, []) ->
  EndTime = erlang:monotonic_time(),
  Printer ! {stop, self()},
  receive
    _ ->
      ok
  after 5000 ->
      ok
  end,
  EndTime;
collect(Printer, [Pid | Pids]) ->
  receive
    {ok, Pid} ->
      collect(Printer, Pids)
  after ?TIMEOUT ->
      {error, timeout}
  end.

flush(Flushes) ->
  receive
    ten -> flush(Flushes + 1)
  after 0 ->
      Flushes
  end.

printer_loop(StartTime, TotalCount) ->
  printer_loop(StartTime, TotalCount, 0).

printer_loop(StartTime, TotalCount, IncCount) ->
  receive
    ten ->
      Flushes = flush(1),
      print_duration(StartTime, TotalCount, IncCount + 250000 * Flushes),
      printer_loop(StartTime, TotalCount, IncCount + 250000 * Flushes);
    {stop, Mgr} ->
      io:format("\n"),
      Mgr ! printer_stopped
  after ?TIMEOUT ->
      {error, timeout}
  end.

print_duration(StartTime, TotalCount, IncCount) ->
  CurrentTime = erlang:monotonic_time(),
  TimeDiff = erlang:convert_time_unit(CurrentTime - StartTime, native,  milli_seconds) / 1000,
  io:format("\r  Duration (sec): ~p (~.1f%)", [TimeDiff, (IncCount/TotalCount)*100]).

loop(Mgr, Printer, 0, _Name) ->
  Printer ! ten,
  Mgr ! {ok, self()};
loop(Mgr, Printer, N, Name) ->
  case N of
    %% 100000 ->
    %%   Printer ! ten;
    250000 ->
      Printer ! ten;
    %% 300000 ->
    %%   Printer ! ten;
    %% 400000 ->
    %%   Printer ! ten;
    500000 ->
      Printer ! ten;
    %% 600000 ->
    %%   Printer ! ten;
    750000 ->
      Printer ! ten;
    %% 800000 ->
    %%   Printer ! ten;
    %% 900000 ->
    %%   Printer ! ten;
    _ -> ok
  end,
  prometheus_counter:inc(Name, 1),
  loop(Mgr, Printer, N-1, Name).

floor(X,Y) ->
  trunc((X - (X rem Y)) / Y).
