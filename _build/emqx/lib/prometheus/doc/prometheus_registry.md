

# Module prometheus_registry #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A registry of Collectors.

<a name="description"></a>

## Description ##

The majority of users should use the `default`, rather than their own.

Creating a registry other than the default is primarily useful for
unit tests, or pushing a subset of metrics to the
[Pushgateway](https://github.com/prometheus/pushgateway) from
batch jobs.
<a name="types"></a>

## Data Types ##




### <a name="type-collect_callback">collect_callback()</a> ###


<pre><code>
collect_callback() = fun((<a href="#type-registry">registry()</a>, <a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>) -&gt; any())
</code></pre>




### <a name="type-registry">registry()</a> ###


<pre><code>
registry() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#clear-0">clear/0</a></td><td>Equivalent to <a href="#clear-1"><tt>clear(default)</tt></a>.</td></tr><tr><td valign="top"><a href="#clear-1">clear/1</a></td><td>Unregisters all collectors.</td></tr><tr><td valign="top"><a href="#collect-2">collect/2</a></td><td>
Calls <code>Callback</code> for each collector with two arguments:
<code>Registry</code> and <code>Collector</code>.</td></tr><tr><td valign="top"><a href="#collector_registeredp-1">collector_registeredp/1</a></td><td>Equivalent to <a href="#collector_registeredp-2"><tt>collector_registeredp(default, Collector)</tt></a>.</td></tr><tr><td valign="top"><a href="#collector_registeredp-2">collector_registeredp/2</a></td><td>Checks whether <code>Collector</code> is registered.</td></tr><tr><td valign="top"><a href="#collectors-1">collectors/1</a></td><td>
Returns collectors registered in <code>Registry</code>.</td></tr><tr><td valign="top"><a href="#deregister_collector-1">deregister_collector/1</a></td><td>Equivalent to <a href="#deregister_collector-2"><tt>deregister_collector(default, Collector)</tt></a>.</td></tr><tr><td valign="top"><a href="#deregister_collector-2">deregister_collector/2</a></td><td>Unregisters a collector.</td></tr><tr><td valign="top"><a href="#register_collector-1">register_collector/1</a></td><td>Equivalent to <a href="#register_collector-2"><tt>register_collector(default, Collector)</tt></a>.</td></tr><tr><td valign="top"><a href="#register_collector-2">register_collector/2</a></td><td>Register a collector.</td></tr><tr><td valign="top"><a href="#register_collectors-1">register_collectors/1</a></td><td>Equivalent to <a href="#register_collectors-2"><tt>register_collectors(default, Collectors)</tt></a>.</td></tr><tr><td valign="top"><a href="#register_collectors-2">register_collectors/2</a></td><td>Registers collectors list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="clear-0"></a>

### clear/0 ###

<pre><code>
clear() -&gt; ok
</code></pre>
<br />

Equivalent to [`clear(default)`](#clear-1).

<a name="clear-1"></a>

### clear/1 ###

<pre><code>
clear(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; ok
</code></pre>
<br />

Unregisters all collectors.

<a name="collect-2"></a>

### collect/2 ###

<pre><code>
collect(Registry, Callback) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li><li><code>Callback = <a href="#type-collect_callback">collect_callback()</a></code></li></ul>

Calls `Callback` for each collector with two arguments:
`Registry` and `Collector`.

<a name="collector_registeredp-1"></a>

### collector_registeredp/1 ###

<pre><code>
collector_registeredp(Collector) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Collector = <a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a></code></li></ul>

Equivalent to [`collector_registeredp(default, Collector)`](#collector_registeredp-2).

<a name="collector_registeredp-2"></a>

### collector_registeredp/2 ###

<pre><code>
collector_registeredp(Registry, Collector) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Registry = <a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a></code></li><li><code>Collector = <a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a></code></li></ul>

Checks whether `Collector` is registered.

<a name="collectors-1"></a>

### collectors/1 ###

<pre><code>
collectors(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; [Collector::<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>]
</code></pre>
<br />

Returns collectors registered in `Registry`.

<a name="deregister_collector-1"></a>

### deregister_collector/1 ###

<pre><code>
deregister_collector(Collector::<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`deregister_collector(default, Collector)`](#deregister_collector-2).

<a name="deregister_collector-2"></a>

### deregister_collector/2 ###

<pre><code>
deregister_collector(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>, Collector::<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>) -&gt; ok
</code></pre>
<br />

Unregisters a collector.

<a name="register_collector-1"></a>

### register_collector/1 ###

<pre><code>
register_collector(Collector::<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>) -&gt; ok
</code></pre>
<br />

Equivalent to [`register_collector(default, Collector)`](#register_collector-2).

<a name="register_collector-2"></a>

### register_collector/2 ###

<pre><code>
register_collector(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>, Collector::<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>) -&gt; ok
</code></pre>
<br />

Register a collector.

<a name="register_collectors-1"></a>

### register_collectors/1 ###

<pre><code>
register_collectors(Collectors::[<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>]) -&gt; ok
</code></pre>
<br />

Equivalent to [`register_collectors(default, Collectors)`](#register_collectors-2).

<a name="register_collectors-2"></a>

### register_collectors/2 ###

<pre><code>
register_collectors(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>, Collectors::[<a href="prometheus_collector.md#type-collector">prometheus_collector:collector()</a>]) -&gt; ok
</code></pre>
<br />

Registers collectors list.

