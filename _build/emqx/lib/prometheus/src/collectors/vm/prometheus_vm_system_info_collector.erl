%% @doc
%% Collects Erlang VM metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#system_info-1">
%%   erlang:system_info/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_vm_ets_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of ETS tables allowed.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors configured in the system.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors_available'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors
%%     available to the Erlang runtime system.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors_online'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors online on the system.
%%   </li>
%%   <li>
%%     `erlang_vm_port_count'<br/>
%%     Type: gauge.<br/>
%%     The number of ports currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_port_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing ports at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_process_count'<br/>
%%     Type: gauge.<br/>
%%     The number of processes currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_process_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing processes
%%     at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang_vm_schedulers_online'<br/>
%%     Type: gauge.<br/>
%%     The number of schedulers online.
%%   </li>
%%   <li>
%%     `erlang_vm_smp_support'<br/>
%%     Type: boolean.<br/>
%%     1 if the emulator has been compiled with SMP support, otherwise 0.
%%   </li>
%%   <li>
%%     `erlang_vm_threads'<br/>
%%     Type: boolean.<br/>
%%     1 if the emulator has been compiled with thread support, otherwise 0.
%%   </li>
%%   <li>
%%     `erlang_vm_thread_pool_size'<br/>
%%     Type: gauge.<br/>
%%     The number of async threads in the async thread pool
%%     used for asynchronous driver calls.
%%   </li>
%%   <li>
%%     `erlang_vm_time_correction'<br/>
%%     Type: boolean.<br/>
%%     1 if time correction is enabled, otherwise 0.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_system_info_collector_metrics' key of `prometheus' app environment.
%%
%% Options are the same as Item parameter values for
%% <a href="http://erlang.org/doc/man/erlang.html#system_info-1">
%%   erlang:system_info/1
%% </a>:
%% <ul>
%%   <li>
%%     `ets_limit' for `erlang_vm_ets_limit'.
%%   </li>
%%   <li>
%%     `logical_processors' for `erlang_vm_logical_processors'.
%%   </li>
%%   <li>
%%     `logical_processors_available' for
%%     `erlang_vm_logical_processors_available'.
%%   </li>
%%   <li>
%%     `logical_processors_online' for `erlang_vm_logical_processors_online'.
%%   </li>
%%   <li>
%%     `port_count' for `erlang_vm_port_count'.
%%   </li>
%%   <li>
%%     `port_limit' for `erlang_vm_port_limit'.
%%   </li>
%%   <li>
%%     `process_count' for `erlang_vm_process_count'.
%%   </li>
%%   <li>
%%     `process_limit' for `erlang_vm_process_limit'.
%%   </li>
%%   <li>
%%     `schedulers' for `erlang_vm_schedulers'.
%%   </li>
%%   <li>
%%     `schedulers_online' for `erlang_vm_schedulers_online'.
%%   </li>
%%   <li>
%%     `smp_support' for `erlang_vm_smp_support'.
%%   </li>
%%   <li>
%%     `threads' for `erlang_threads'.
%%   </li>
%%   <li>
%%     `thread_pool_size' for `erlang_vm_thread_pool_size'.
%%   </li>
%%   <li>
%%     `time_correction' for `erlang_vm_time_correction'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end
-module(prometheus_vm_system_info_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   untyped_metric/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(ETS_LIMIT, erlang_vm_ets_limit).
-define(LOGICAL_PROCESSORS, erlang_vm_logical_processors).
-define(LOGICAL_PROCESSORS_AVAILABLE, erlang_vm_logical_processors_available).
-define(LOGICAL_PROCESSORS_ONLINE, erlang_vm_logical_processors_online).
-define(PORT_COUNT, erlang_vm_port_count).
-define(PORT_LIMIT, erlang_vm_port_limit).
-define(PROCESS_COUNT, erlang_vm_process_count).
-define(PROCESS_LIMIT, erlang_vm_process_limit).
-define(SCHEDULERS, erlang_vm_schedulers).
-define(SCHEDULERS_ONLINE, erlang_vm_schedulers_online).
-define(SMP_SUPPORT, erlang_vm_smp_support).
-define(THREADS, erlang_vm_threads).
-define(THREAD_POOL_SIZE, erlang_vm_thread_pool_size).
-define(TIME_CORRECTION, erlang_vm_time_correction).

-define(PROMETHEUS_VM_SYSTEM_INFO, [
                                    ets_limit,
                                    logical_processors,
                                    logical_processors_available,
                                    logical_processors_online,
                                    port_count,
                                    port_limit,
                                    process_count,
                                    process_limit,
                                    schedulers,
                                    schedulers_online,
                                    smp_support,
                                    threads,
                                    thread_pool_size,
                                    time_correction
                                   ]).

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
%% @private
collect_mf(_Registry, Callback) ->
  [call_if_system_info_exists(MFName,
                              fun(Value) ->
                                  add_metric_family(MFName, Value, Callback)
                              end)
   || MFName <- enabled_system_info_metrics()],
  ok.

add_metric_family(ets_limit, Value, Callback) ->
  Callback(create_gauge(?ETS_LIMIT,
                        "The maximum number of ETS tables allowed.",
                        Value));
add_metric_family(logical_processors, Value, Callback) ->
  Callback(create_gauge(?LOGICAL_PROCESSORS,
                        "The detected number of logical processors "
                        "configured in the system.",
                        Value));
add_metric_family(logical_processors_available, Value, Callback) ->
  Callback(create_gauge(?LOGICAL_PROCESSORS_AVAILABLE,
                        "The detected number of logical processors "
                        "available to the Erlang runtime system.",
                        Value));
add_metric_family(logical_processors_online, Value, Callback) ->
  Callback(create_gauge(?LOGICAL_PROCESSORS_ONLINE,
                        "The detected number of logical processors "
                        "online on the system.",
                        Value));
add_metric_family(port_count, Value, Callback) ->
  Callback(create_gauge(?PORT_COUNT,
                        "The number of ports currently existing "
                        "at the local node.",
                        Value));
add_metric_family(port_limit, Value, Callback) ->
  Callback(create_gauge(?PORT_LIMIT,
                        "The maximum number of simultaneously existing ports "
                        "at the local node.",
                        Value));
add_metric_family(process_count, Value, Callback) ->
  Callback(create_gauge(?PROCESS_COUNT,
                        "The number of processes currently existing "
                        "at the local node.",
                        Value));
add_metric_family(process_limit, Value, Callback) ->
  Callback(create_gauge(?PROCESS_LIMIT,
                        "The maximum number of simultaneously existing "
                        "processes at the local node.",
                        Value));
add_metric_family(schedulers, Value, Callback) ->
  Callback(create_gauge(?SCHEDULERS,
                        "The number of scheduler threads used by the emulator.",
                        Value));
add_metric_family(schedulers_online, Value, Callback) ->
  Callback(create_gauge(?SCHEDULERS_ONLINE,
                        "The number of schedulers online.",
                        Value));
add_metric_family(smp_support, Value, Callback) ->
  Callback(create_boolean(?SMP_SUPPORT,
                          "1 if the emulator has been compiled with SMP "
                          "support, otherwise 0.",
                          Value));
add_metric_family(threads, Value, Callback) ->
  Callback(create_boolean(?THREADS,
                          "1 if the emulator has been compiled with thread "
                          "support, otherwise 0.",
                          Value));
add_metric_family(thread_pool_size, Value, Callback) ->
  Callback(create_gauge(?THREAD_POOL_SIZE,
                        "The number of async threads in the async thread pool "
                        "used for asynchronous driver calls.",
                        Value));
add_metric_family(time_correction, Value, Callback) ->
  Callback(create_boolean(?TIME_CORRECTION,
                          "1 if time correction is enabled, otherwise 0.",
                          Value)).

%% @private
collect_metrics(?ETS_LIMIT, Value) ->
  gauge_metric(Value);
collect_metrics(?LOGICAL_PROCESSORS, Value) ->
  gauge_metric(Value);
collect_metrics(?LOGICAL_PROCESSORS_AVAILABLE, Value) ->
  gauge_metric(Value);
collect_metrics(?LOGICAL_PROCESSORS_ONLINE, Value) ->
  gauge_metric(Value);
collect_metrics(?PORT_COUNT, Value) ->
  gauge_metric(Value);
collect_metrics(?PORT_LIMIT, Value) ->
  gauge_metric(Value);
collect_metrics(?PROCESS_COUNT, Value) ->
  gauge_metric(Value);
collect_metrics(?PROCESS_LIMIT, Value) ->
  gauge_metric(Value);
collect_metrics(?SCHEDULERS, Value) ->
  gauge_metric(Value);
collect_metrics(?SCHEDULERS_ONLINE, Value) ->
  gauge_metric(Value);
collect_metrics(?SMP_SUPPORT, Value) ->
  boolean_metric(Value);
collect_metrics(?THREADS, Value) ->
  boolean_metric(Value);
collect_metrics(?THREAD_POOL_SIZE, Value) ->
  gauge_metric(Value);
collect_metrics(?TIME_CORRECTION, Value) ->
  boolean_metric(Value).

%%====================================================================
%% Private Parts
%%====================================================================

call_if_system_info_exists(SysInfoItem, Fun) ->
  try
    case erlang:system_info(SysInfoItem) of
      unknown -> Fun(undefined);
      Value -> Fun(Value)
    end
  catch
    error:badarg -> undefined
  end.

enabled_system_info_metrics() ->
  application:get_env(prometheus, vm_system_info_collector_metrics,
                      ?PROMETHEUS_VM_SYSTEM_INFO).

%% create_counter(Name, Help, Data) ->
%%   create_mf(Name, Help, counter, ?MODULE, Data).

create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).

create_boolean(Name, Help, Data) ->
  create_mf(Name, Help, untyped, ?MODULE, Data).

boolean_metric(Value) ->
  case Value of
    true ->
      untyped_metric(1);
    _ ->
      untyped_metric(0)
  end.
