-module(prometheus_instrumenter_tests).

-include_lib("eunit/include/eunit.hrl").

instrumenter_setup_test() ->
  prometheus:start(),
  ?assertNotMatch(undefined, ets:info(prometheus_instrumenter_tests)),
  application:set_env(prometheus, instrumenters, [qwe]),
  try
    ?assertMatch([qwe], prometheus_instrumenter:enabled_instrumenters())
  after
    application:unset_env(prometheus, instrumenters)
  end,
  ?assertMatch([prometheus_test_instrumenter],
               prometheus_instrumenter:enabled_instrumenters()).
