%% @doc
%% Collects Erlang VM metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#statistics-1">
%%   erlang:statistics/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_vm_statistics_bytes_output_total'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes output to ports.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_bytes_received_total'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes received through ports.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_context_switches'<br/>
%%     Type: counter.<br/>
%%     The total number of context switches since the system started.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_garbage_collection_number_of_gcs'<br/>
%%     Type: counter.<br/>
%%     The total number of garbage collections since the system started.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_garbage_collection_words_reclaimed'<br/>
%%     Type: counter.<br/>
%%     The total number of words reclaimed by GC since the system started.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_garbage_collection_bytes_reclaimed'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes reclaimed by GC since the system started.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_reductions_total'<br/>
%%     Type: counter.<br/>
%%     Total reductions count.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_run_queues_length_total'<br/>
%%     Type: gauge.<br/>
%%     The total length of the run-queues. That is, the number of
%%     processes and ports that are ready to run on all available run-queues.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_runtime_milliseconds'<br/>
%%     Type: counter.<br/>
%%     The sum of the runtime for all threads in the Erlang runtime system.
%%   </li>
%%   <li>
%%     `erlang_vm_statistics_wallclock_time_milliseconds'<br/>
%%     Type: counter.<br/>
%%     Can be used in the same manner as
%%     `erlang_vm_statistics_runtime_milliseconds', except that real time is
%%     measured as opposed to runtime or CPU time.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_statistics_collector_metrics' key of `prometheus' app environment.
%%
%% Options are the same as Item parameter values for
%% <a href="http://erlang.org/doc/man/erlang.html#statistics-1">
%%   erlang:statistics/1
%% </a>:
%% <ul>
%%   <li>
%%     `context_switches' for `erlang_vm_statistics_context_switches';
%%   </li>
%%   <li>
%%     `garbage_collection'
%%      for `erlang_vm_statistics_garbage_collection_number_of_gcs',
%%      `erlang_vm_statistics_garbage_collection_bytes_reclaimed', and
%%      `erlang_vm_statistics_garbage_collection_words_reclaimed';
%%   </li>
%%   <li>
%%     `io' for `erlang_vm_statistics_bytes_output_total' and
%%     `erlang_vm_statistics_bytes_received_total';
%%   </li>
%%   <li>
%%     `reductions' for `erlang_vm_statistics_reductions_total';
%%   </li>
%%   <li>
%%     `run_queue' for `erlang_vm_statistics_run_queues_length_total';
%%   </li>
%%   <li>
%%     `runtime' for `erlang_vm_statistics_runtime_milliseconds';
%%   </li>
%%   <li>
%%     `wall_clock' for `erlang_vm_statistics_wallclock_time_milliseconds'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end

-module(prometheus_vm_statistics_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(BYTES_OUTPUT, erlang_vm_statistics_bytes_output_total).
-define(BYTES_RECEIVED, erlang_vm_statistics_bytes_received_total).
-define(CONTEXT_SWITCHES, erlang_vm_statistics_context_switches).
-define(GC_NUM_GCS, erlang_vm_statistics_garbage_collection_number_of_gcs).
-define(GC_WORDS_RECLAIMED,
        erlang_vm_statistics_garbage_collection_words_reclaimed).
-define(GC_BYTES_RECLAIMED,
        erlang_vm_statistics_garbage_collection_bytes_reclaimed).
-define(REDUCTIONS, erlang_vm_statistics_reductions_total).
-define(RUN_QUEUES_LENGTH, erlang_vm_statistics_run_queues_length_total).
-define(RUNTIME_MS, erlang_vm_statistics_runtime_milliseconds).
-define(WALLCLOCK_TIME_MS, erlang_vm_statistics_wallclock_time_milliseconds).

-define(PROMETHEUS_VM_STATISTICS, [
                                   context_switches,
                                   garbage_collection,
                                   io,
                                   reductions,
                                   run_queue,
                                   runtime,
                                   wall_clock
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
  [call_if_statistics_exists(MFName,
                             fun(Stat) ->
                                 add_metric_family(MFName, Stat, Callback)
                             end)
   || MFName <- enabled_statistics_metrics()],
  ok.

add_metric_family(context_switches, Stat, Callback) ->
  do_add_metric_family(?CONTEXT_SWITCHES, Stat, Callback,
                       "Total number of context switches "
                       "since the system started");
add_metric_family(garbage_collection, Stat, Callback) ->
  do_add_metric_family(?GC_NUM_GCS, Stat, Callback,
                       "Garbage collection: number of GCs"),
  do_add_metric_family(?GC_WORDS_RECLAIMED, Stat, Callback,
                       "Garbage collection: words reclaimed"),
  do_add_metric_family(?GC_BYTES_RECLAIMED, Stat, Callback,
                       "Garbage collection: bytes reclaimed");
add_metric_family(io, Stat, Callback) ->
  do_add_metric_family(?BYTES_RECEIVED, Stat, Callback,
                       "Total number of bytes received through ports"),
  do_add_metric_family(?BYTES_OUTPUT, Stat, Callback,
                       "Total number of bytes output to ports");
add_metric_family(reductions, Stat, Callback) ->
  do_add_metric_family(?REDUCTIONS, Stat, Callback, "Total reductions");
add_metric_family(run_queue, Stat, Callback) ->
  do_add_metric_family(?RUN_QUEUES_LENGTH, Stat, Callback,
                       "Total length of the run-queues");
add_metric_family(runtime, Stat, Callback) ->
  do_add_metric_family(?RUNTIME_MS, Stat, Callback,
                       "The sum of the runtime for all threads "
                       "in the Erlang runtime system. "
                       "Can be greater than wall clock time");
add_metric_family(wall_clock, Stat, Callback) ->
  do_add_metric_family(
    ?WALLCLOCK_TIME_MS,
    Stat, Callback,
    "Information about wall clock. "
    "Same as erlang_vm_statistics_runtime_milliseconds "
    "except that real time is measured").

%% @private
collect_metrics(?BYTES_OUTPUT, {_, {output, Output}}) ->
  counter_metric(Output);
collect_metrics(?BYTES_RECEIVED, {{input, Input}, _}) ->
  counter_metric(Input);
collect_metrics(?CONTEXT_SWITCHES, {Stat, _}) ->
  counter_metric(Stat);
collect_metrics(?GC_NUM_GCS, {NumberOfGCs, _, _}) ->
  counter_metric(NumberOfGCs);
collect_metrics(?GC_WORDS_RECLAIMED, {_, WordsReclaimed, _}) ->
  counter_metric(WordsReclaimed);
collect_metrics(?GC_BYTES_RECLAIMED, {_, WordsReclaimed, _}) ->
  counter_metric(WordsReclaimed * erlang:system_info(wordsize));
collect_metrics(?REDUCTIONS, {ReductionsTotal, _}) ->
  counter_metric(ReductionsTotal);
collect_metrics(?RUN_QUEUES_LENGTH, Total) ->
  gauge_metric(Total);
collect_metrics(?RUNTIME_MS, {Runtime, _}) ->
  counter_metric(Runtime);
collect_metrics(?WALLCLOCK_TIME_MS, {WallclockTime, _}) ->
  counter_metric(WallclockTime).

%%====================================================================
%% Private Parts
%%====================================================================

call_if_statistics_exists(StatItem, Fun) ->
  try
    Stat = erlang:statistics(StatItem),
    Fun(Stat)
  catch
    error:badarg -> undefined
  end.

enabled_statistics_metrics() ->
  application:get_env(prometheus, vm_statistics_collector_metrics,
                      ?PROMETHEUS_VM_STATISTICS).

do_add_metric_family(?RUN_QUEUES_LENGTH, Stat, Callback, Help) ->
  Callback(create_gauge(?RUN_QUEUES_LENGTH, Help, Stat));
do_add_metric_family(Name, Stat, Callback, Help) ->
  Callback(create_counter(Name, Help, Stat)).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).
