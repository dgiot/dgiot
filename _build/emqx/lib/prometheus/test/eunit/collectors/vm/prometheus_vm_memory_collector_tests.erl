-module(prometheus_vm_memory_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_default_metrics/1,
    fun test_all_metrics/1,
    fun test_custom_metrics/1]}.

test_default_metrics(_) ->
  prometheus_registry:register_collector(prometheus_vm_memory_collector),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dets_tables")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_tables")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes_total")),
   ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes_total"))
  ].

test_all_metrics(_) ->
  try
    application:set_env(prometheus, vm_memory_collector_metrics, [
                                                                  atom_bytes_total,
                                                                  bytes_total,
                                                                  dets_tables,
                                                                  ets_tables,
                                                                  processes_bytes_total,
                                                                  system_bytes_total
                                                                 ]),
    prometheus_registry:register_collector(prometheus_vm_memory_collector),
    Metrics = prometheus_text_format:format(),
    [
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes_total")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes_total")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dets_tables")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_tables")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes_total")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes_total"))
    ]

  after
    application:unset_env(prometheus, vm_memory_collector_metrics)
  end.

test_custom_metrics(_) ->
  try
    application:set_env(prometheus, vm_memory_collector_metrics, [
                                                                  atom_bytes_total,
                                                                  dets_tables
                                                                 ]),
    prometheus_registry:register_collector(prometheus_vm_memory_collector),
    Metrics = prometheus_text_format:format(),
    [
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes_total")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_bytes_total")),
     ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dets_tables")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_ets_tables")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_processes_bytes_total")),
     ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_system_bytes_total"))
    ]

  after
    application:unset_env(prometheus, vm_memory_collector_metrics)
  end.
