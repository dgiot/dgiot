%% @private
-module(prometheus_test_instrumenter).

-behaviour(prometheus_instrumenter).

-export([setup_instrumenter/0]).

setup_instrumenter() ->
  ets:new(prometheus_instrumenter_tests, [set, named_table, public]),
  ok.
