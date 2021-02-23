

# Module prometheus_protobuf_format #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Serializes Prometheus registry using
[protocol buffer format](http://bit.ly/2cxSuJP).

__Behaviours:__ [`prometheus_format`](prometheus_format.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#content_type-0">content_type/0</a></td><td>
Returns content type of the protocol buffer format.</td></tr><tr><td valign="top"><a href="#format-0">format/0</a></td><td>
Formats <code>default</code> registry using protocol buffer format.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>
Formats <code>Registry</code> using protocol buffer format.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="content_type-0"></a>

### content_type/0 ###

<pre><code>
content_type() -&gt; binary()
</code></pre>
<br />

Returns content type of the protocol buffer format.

<a name="format-0"></a>

### format/0 ###

<pre><code>
format() -&gt; binary()
</code></pre>
<br />

Equivalent to [`format(default)`](#format-1).

Formats `default` registry using protocol buffer format.

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; binary()
</code></pre>
<br />

Formats `Registry` using protocol buffer format.

