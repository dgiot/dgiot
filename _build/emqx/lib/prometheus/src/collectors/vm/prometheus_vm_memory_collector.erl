%% @doc
%% Collects information about memory dynamically allocated
%% by the Erlang emulator using
%% <a href="http://erlang.org/doc/man/erlang.html#memory-0">
%%   erlang:memory/0
%% </a>, also provides basic (D)ETS statistics.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_vm_memory_atom_bytes_total{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for atoms.
%%     This memory is part of the memory presented as system memory.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_bytes_total{kind="system|processes"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated.
%%     This is the same as the sum of the memory size for processes and system.
%%   </li>
%%   <li>
%%     `erlang_vm_dets_tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM DETS Tables count.
%%   </li>
%%   <li>
%%     `erlang_vm_ets_tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM ETS Tables count.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_processes_bytes_total{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the Erlang processes.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_system_bytes_total{usage="atom|binary|code|ets|other"}'
%%     <br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the emulator
%%     that is not directly related to any Erlang process.
%%     Memory presented as processes is not included in this memory.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_memory_collector_metrics' key of `prometheus' app environment.
%%
%% Available options:
%% <ul>
%%   <li>
%%     `atom_bytes_total' for `erlang_vm_memory_atom_bytes_total'.
%%   </li>
%%   <li>
%%     `bytes_total' for `erlang_vm_memory_bytes_total'.
%%   </li>
%%   <li>
%%     `dets_tables' for `erlang_vm_dets_tables'.
%%   </li>
%%   <li>
%%     `ets_tables' for `erlang_vm_ets_tables'.
%%   </li>
%%   <li>
%%     `processes_bytes_total' for `erlang_vm_memory_processes_bytes_total'.
%%   </li>
%%   <li>
%%     `system_bytes_total' for `erlang_vm_memory_system_bytes_total'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end

-module(prometheus_vm_memory_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================


-define(ATOM_BYTES_TOTAL, erlang_vm_memory_atom_bytes_total).
-define(BYTES_TOTAL, erlang_vm_memory_bytes_total).
-define(DETS_TABLES, erlang_vm_dets_tables).
-define(ETS_TABLES, erlang_vm_ets_tables).
-define(PROCESSES_BYTES_TOTAL, erlang_vm_memory_processes_bytes_total).
-define(SYSTEM_BYTES_TOTAL, erlang_vm_memory_system_bytes_total).

-define(PROMETHEUS_VM_MEMORY, [
                               atom_bytes_total,
                               bytes_total,
                               dets_tables,
                               ets_tables,
                               processes_bytes_total,
                               system_bytes_total
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
  Memory = erlang:memory(),
  [add_metric_family(MFName, Memory, Callback)
   || MFName <- enabled_memory_metrics()],
  ok.


add_metric_family(atom_bytes_total, Memory, Callback) ->
  Callback(create_gauge(?ATOM_BYTES_TOTAL,
                        "The total amount of memory currently allocated "
                        "for atoms. This memory is part of the memory "
                        "presented as system memory.",
                        Memory));
add_metric_family(bytes_total, Memory, Callback) ->
  Callback(create_gauge(?BYTES_TOTAL,
                        "The total amount of memory currently allocated. "
                        "This is the same as the sum of the memory size "
                        "for processes and system.",
                        Memory));
add_metric_family(dets_tables, Memory, Callback) ->
  Callback(create_gauge(?DETS_TABLES,
                        "Erlang VM DETS Tables count",
                        Memory));
add_metric_family(ets_tables, Memory, Callback) ->
  Callback(create_gauge(?ETS_TABLES,
                        "Erlang VM ETS Tables count",
                        Memory));
add_metric_family(processes_bytes_total, Memory, Callback) ->
  Callback(create_gauge(?PROCESSES_BYTES_TOTAL,
                        "The total amount of memory currently allocated "
                        "for the Erlang processes.",
                        Memory));
add_metric_family(system_bytes_total, Memory, Callback) ->
  Callback(create_gauge(?SYSTEM_BYTES_TOTAL,
                        "The total amount of memory currently allocated "
                        "for the emulator that is not directly related "
                        "to any Erlang process. Memory presented as processes "
                        "is not included in this memory.",
                        Memory)).

%% @private
collect_metrics(?ATOM_BYTES_TOTAL, Memory) ->
  gauge_metrics([
                 {[{usage, used}], proplists:get_value(atom_used, Memory)},
                 {[{usage, free}],
                  proplists:get_value(atom, Memory)
                  - proplists:get_value(atom_used, Memory)}
                ]);
collect_metrics(?BYTES_TOTAL, Memory) ->
  gauge_metrics([
                 {[{kind, system}], proplists:get_value(system,  Memory)},
                 {[{kind, processes}], proplists:get_value(processes, Memory)}
                ]);
collect_metrics(?DETS_TABLES, _MFData) ->
  gauge_metric(length(dets:all()));
collect_metrics(?ETS_TABLES, _MFData) ->
  gauge_metric(length(ets:all()));
collect_metrics(?PROCESSES_BYTES_TOTAL, Memory) ->
  gauge_metrics([
                 {[{usage, used}], proplists:get_value(processes_used, Memory)},
                 {[{usage, free}],
                  proplists:get_value(processes, Memory)
                  - proplists:get_value(processes_used, Memory)}
                ]);
collect_metrics(?SYSTEM_BYTES_TOTAL, Memory) ->
  gauge_metrics([
                 {[{usage, atom}], proplists:get_value(atom, Memory)},
                 {[{usage, binary}], proplists:get_value(binary, Memory)},
                 {[{usage, code}], proplists:get_value(code, Memory)},
                 {[{usage, ets}], proplists:get_value(ets, Memory)},
                 {[{usage, other}], memory_other(Memory)}
                ]).

%%====================================================================
%% Private Parts
%%====================================================================

memory_other(Memory) ->
  proplists:get_value(system, Memory)
    - proplists:get_value(atom, Memory)
    - proplists:get_value(binary, Memory)
    - proplists:get_value(code, Memory)
    - proplists:get_value(ets, Memory).

enabled_memory_metrics() ->
  application:get_env(prometheus, vm_memory_collector_metrics,
                      ?PROMETHEUS_VM_MEMORY).

-spec create_gauge(Name, Help, Data) -> prometheus_model:'MetricFamily'() when
    Name :: atom(),
    Help :: string(),
    Data :: prometheus_collector:data().
create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).
