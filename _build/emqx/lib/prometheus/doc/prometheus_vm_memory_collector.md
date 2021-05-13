

# Module prometheus_vm_memory_collector #
* [Description](#description)

Collects information about memory dynamically allocated
by the Erlang emulator using
[
erlang:memory/0
](http://erlang.org/doc/man/erlang.md#memory-0), also provides basic (D)ETS statistics.

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##


### <a name="Exported_metrics">Exported metrics</a> ###


* `erlang_vm_memory_atom_bytes_total{usage="free|used"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated for atoms.
This memory is part of the memory presented as system memory.

* `erlang_vm_memory_bytes_total{kind="system|processes"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated.
This is the same as the sum of the memory size for processes and system.

* `erlang_vm_dets_tables`<br />
Type: gauge.<br />
Erlang VM DETS Tables count.

* `erlang_vm_ets_tables`<br />
Type: gauge.<br />
Erlang VM ETS Tables count.

* `erlang_vm_memory_processes_bytes_total{usage="free|used"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated for the Erlang processes.

* `erlang_vm_memory_system_bytes_total{usage="atom|binary|code|ets|other"}`
<br />
Type: gauge.<br />
The total amount of memory currently allocated for the emulator
that is not directly related to any Erlang process.
Memory presented as processes is not included in this memory.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`vm_memory_collector_metrics` key of `prometheus` app environment.

Available options:

* `atom_bytes_total` for `erlang_vm_memory_atom_bytes_total`.

* `bytes_total` for `erlang_vm_memory_bytes_total`.

* `dets_tables` for `erlang_vm_dets_tables`.

* `ets_tables` for `erlang_vm_ets_tables`.

* `processes_bytes_total` for `erlang_vm_memory_processes_bytes_total`.

* `system_bytes_total` for `erlang_vm_memory_system_bytes_total`.


By default all metrics are enabled.