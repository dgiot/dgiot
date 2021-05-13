

# Module prometheus_collector #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A collector for a set of metrics.

__This module defines the `prometheus_collector` behaviour.__<br /> Required callback functions: `collect_mf/2`, `collect_metrics/2`, `deregister_cleanup/1`.

<a name="description"></a>

## Description ##

Normal users should use [`prometheus_gauge`](prometheus_gauge.md),
[`prometheus_counter`](prometheus_counter.md), [`prometheus_summary`](prometheus_summary.md)
and [`prometheus_histogram`](prometheus_histogram.md).

Implementing `:prometheus_collector` behaviour is for advanced uses
such as proxying metrics from another monitoring system.
It is it the responsibility of the implementer to ensure produced metrics
are valid.

You will be working with Prometheus
data model directly (see [`prometheus_model_helpers`](prometheus_model_helpers.md)).

Callbacks:
- `collect_mf(Registry, Callback)` - called by exporters and formats.
Should call `Callback` for each `MetricFamily` of this collector;
- `collect_metrics(Name, Data)` - called by `MetricFamily` constructor.
Should return Metric list for each MetricFamily identified by `Name`.
`Data` is a term associated with MetricFamily by collect_mf.
- `deregister_cleanup(Registry)` - called when collector unregistered by
`Registry`. If collector is stateful you can put cleanup code here.

Example (simplified `prometheus_vm_memory_collector`):

```erlang

  -module(prometheus_vm_memory_collector).
  -export([deregister_cleanup/1,
           collect_mf/2,
           collect_metrics/2]).
  -behaviour(prometheus_collector).
  %%====================================================================
  %% Collector API
  %%====================================================================
  deregister_cleanup(_) -> ok.
  collect_mf(_Registry, Callback) ->
    Memory = erlang:memory(),
    Callback(create_gauge(erlang_vm_bytes_total,
                          "The total amount of memory currently allocated. "
                          "This is the same as the sum of the memory size "
                          "for processes and system.",
                          Memory)),
    ok.
  collect_metrics(erlang_vm_bytes_total, Memory) ->
    prometheus_model_helpers:gauge_metrics(
      [
        {[{kind, system}], proplists:get_value(system,  Memory)},
        {[{kind, processes}], proplists:get_value(processes, Memory)}
      ]).
  %%====================================================================
  %% Private Parts
  %%====================================================================
  create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
```

<a name="types"></a>

## Data Types ##




### <a name="type-collect_mf_callback">collect_mf_callback()</a> ###


<pre><code>
collect_mf_callback() = fun((<a href="prometheus_model.md#type-MetricFamily">prometheus_model:'MetricFamily'()</a>) -&gt; any())
</code></pre>




### <a name="type-collector">collector()</a> ###


<pre><code>
collector() = atom()
</code></pre>




### <a name="type-data">data()</a> ###


<pre><code>
data() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_mf-3">collect_mf/3</a></td><td>Calls <code>Callback</code> for each MetricFamily of this collector.</td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#deregister-2"><tt>deregister(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister-2">deregister/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#register-2"><tt>register(Collector, default)</tt></a>.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td>(<em>Deprecated</em>.) </td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_mf-3"></a>

### collect_mf/3 ###

<pre><code>
collect_mf(Registry, Collector, Callback) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Callback = <a href="#type-collect_mf_callback">collect_mf_callback()</a></code></li></ul>

Calls `Callback` for each MetricFamily of this collector.

<a name="deregister-1"></a>

### deregister/1 ###

`deregister(Collector) -> any()`

Equivalent to [`deregister(Collector, default)`](#deregister-2).

__This function is deprecated:__ Please use [`prometheus_registry:deregister_collector/1`](prometheus_registry.md#deregister_collector-1)

<a name="deregister-2"></a>

### deregister/2 ###

<pre><code>
deregister(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

__This function is deprecated:__ Please use [`prometheus_registry:deregister_collector/2`](prometheus_registry.md#deregister_collector-2)

<a name="register-1"></a>

### register/1 ###

`register(Collector) -> any()`

Equivalent to [`register(Collector, default)`](#register-2).

__This function is deprecated:__ Please use [`prometheus_registry:register_collector/1`](prometheus_registry.md#register_collector-1)

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Collector, Registry) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="#type-collector">collector()</a></code></li><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li></ul>

__This function is deprecated:__ Please use [`prometheus_registry:register_collector/2`](prometheus_registry.md#register_collector-2)

