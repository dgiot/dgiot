-module(prometheus_collector_tests).

-include_lib("eunit/include/eunit.hrl").

collector_setup_test() ->
  prometheus:start(),
  application:set_env(prometheus, collectors, [qwe]),
  try
    ?assertMatch([qwe], prometheus_collector:enabled_collectors())
  after
    application:unset_env(prometheus, collectors)
  end,
  ?assertMatch([prometheus_vm_system_info_collector,
                prometheus_vm_statistics_collector,
                prometheus_vm_memory_collector,
                prometheus_summary,
                prometheus_mnesia_collector,
                prometheus_histogram,
                prometheus_gauge,
                prometheus_counter], prometheus_collector:enabled_collectors()).

collector_deprecations_test() ->
  ok = prometheus_collector:register(prometheus_registry_tests),
  ?assertEqual(true,
               prometheus_registry:collector_registeredp(prometheus_registry_tests)),
  prometheus_collector:deregister(prometheus_registry_tests),
  ?assertEqual(false,
               prometheus_registry:collector_registeredp(prometheus_registry_tests)).
