

# Module prometheus_vm_system_info_collector #
* [Description](#description)

Collects Erlang VM metrics using
[
erlang:system_info/1
](http://erlang.org/doc/man/erlang.md#system_info-1).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##


### <a name="Exported_metrics">Exported metrics</a> ###


* `erlang_vm_ets_limit`<br />
Type: gauge.<br />
The maximum number of ETS tables allowed.

* `erlang_vm_logical_processors`<br />
Type: gauge.<br />
The detected number of logical processors configured in the system.

* `erlang_vm_logical_processors_available`<br />
Type: gauge.<br />
The detected number of logical processors
available to the Erlang runtime system.

* `erlang_vm_logical_processors_online`<br />
Type: gauge.<br />
The detected number of logical processors online on the system.

* `erlang_vm_port_count`<br />
Type: gauge.<br />
The number of ports currently existing at the local node.

* `erlang_vm_port_limit`<br />
Type: gauge.<br />
The maximum number of simultaneously existing ports at the local node.

* `erlang_vm_process_count`<br />
Type: gauge.<br />
The number of processes currently existing at the local node.

* `erlang_vm_process_limit`<br />
Type: gauge.<br />
The maximum number of simultaneously existing processes
at the local node.

* `erlang_vm_schedulers`<br />
Type: gauge.<br />
The number of scheduler threads used by the emulator.

* `erlang_vm_schedulers_online`<br />
Type: gauge.<br />
The number of schedulers online.

* `erlang_vm_smp_support`<br />
Type: boolean.<br />
1 if the emulator has been compiled with SMP support, otherwise 0.

* `erlang_vm_threads`<br />
Type: boolean.<br />
1 if the emulator has been compiled with thread support, otherwise 0.

* `erlang_vm_thread_pool_size`<br />
Type: gauge.<br />
The number of async threads in the async thread pool
used for asynchronous driver calls.

* `erlang_vm_time_correction`<br />
Type: boolean.<br />
1 if time correction is enabled, otherwise 0.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`vm_system_info_collector_metrics` key of `prometheus` app environment.

Options are the same as Item parameter values for
[
erlang:system_info/1
](http://erlang.org/doc/man/erlang.md#system_info-1):

* `ets_limit` for `erlang_vm_ets_limit`.

* `logical_processors` for `erlang_vm_logical_processors`.

* `logical_processors_available` for
`erlang_vm_logical_processors_available`.

* `logical_processors_online` for `erlang_vm_logical_processors_online`.

* `port_count` for `erlang_vm_port_count`.

* `port_limit` for `erlang_vm_port_limit`.

* `process_count` for `erlang_vm_process_count`.

* `process_limit` for `erlang_vm_process_limit`.

* `schedulers` for `erlang_vm_schedulers`.

* `schedulers_online` for `erlang_vm_schedulers_online`.

* `smp_support` for `erlang_vm_smp_support`.

* `threads` for `erlang_threads`.

* `thread_pool_size` for `erlang_vm_thread_pool_size`.

* `time_correction` for `erlang_vm_time_correction`.


By default all metrics are enabled.