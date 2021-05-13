%% File    : benchmarks.erl
%% Purpose : Benchmarks runner for luerl
%% Use: erlc -I ../src benchmark.erl && erl -pa ../ebin -s benchmark run -s init stop -noshell
%% Or: make

-module(benchmarks).
-export([run/0, run/1]).
-export([benchmarks/1, do_benchmark/2, do_iteration/3]).

-define(DEFAULT_ITER, 1000).

run() ->
  Files = filelib:wildcard("suites/*.lua"),
  [run(File) || File <- Files],
  ok.

run(File) ->
  Lua0 = luerl:init(),
  {ok, Form, Lua1} = luerl:loadfile(File, Lua0),
  {_Resp, Lua2} = luerl:do(Form, Lua1),
  report_file(File),
  [do_benchmark(Benchmark, Lua2) || Benchmark <- benchmarks(Lua2)],
  {ok, Lua2}.

do_benchmark(Benchmark, Lua) ->
  Iter = num_iterations(Lua),
  report_benchmark(Benchmark),
  {Time, _Resp} = timer:tc(?MODULE, do_iteration,
                           [Iter, Benchmark, Lua]),
  report_time(Time),
  ok.

do_iteration(0, _Benchmark, _Lua) -> ok;
do_iteration(Iter, Benchmark, Lua) ->
  luerl:call_method1([<<"bench">>, Benchmark], [], Lua),
  do_iteration(Iter - 1, Benchmark, Lua).

num_iterations(Lua) ->
  case luerl:eval("return NUM_ITERATIONS", Lua) of
    {ok, [Iter]} when is_number(Iter) -> round(Iter);
    _any -> ?DEFAULT_ITER
  end.

benchmarks(Lua0) ->
  {ok, Chunk, Lua1} = luerl:loadfile("util/extract_bench_keys.lua", Lua0),
  {ok, [Benchmarks]}  = luerl:eval(Chunk, Lua1),
  [Key || {_Index, Key} <- Benchmarks].

report_file(File) ->
  io:format("~n~s ms~n", [string:left(File, 26)]).

report_benchmark(Benchmark) ->
  io:format("  ~s",
    [string:left(binary_to_list(Benchmark), 24, $.)]).

report_time(Time) ->
  io:format(" ~p~n", [Time / 1000]).


