

# Module prometheus_text_format #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

__Behaviours:__ [`prometheus_format`](prometheus_format.md).

<a name="description"></a>

## Description ##

Serializes Prometheus registry using the latest
[text format](http://bit.ly/2cxSuJP).

Example output:

```

    # TYPE http_request_duration_milliseconds histogram
    # HELP http_request_duration_milliseconds Http Request execution time
    http_request_duration_milliseconds_bucket{method="post",le="100"} 0
    http_request_duration_milliseconds_bucket{method="post",le="300"} 1
    http_request_duration_milliseconds_bucket{method="post",le="500"} 3
    http_request_duration_milliseconds_bucket{method="post",le="750"} 4
    http_request_duration_milliseconds_bucket{method="post",le="1000"} 5
    http_request_duration_milliseconds_bucket{method="post",le="+Inf"} 6
    http_request_duration_milliseconds_count{method="post"} 6
    http_request_duration_milliseconds_sum{method="post"} 4350
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#content_type-0">content_type/0</a></td><td>
Returns content type of the latest [text format](http://bit.ly/2cxSuJP).</td></tr><tr><td valign="top"><a href="#format-0">format/0</a></td><td>
Formats <code>default</code> registry using the latest text format.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>
Formats <code>Registry</code> using the latest text format.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="content_type-0"></a>

### content_type/0 ###

<pre><code>
content_type() -&gt; binary()
</code></pre>
<br />

Returns content type of the latest [text format](http://bit.ly/2cxSuJP).

<a name="format-0"></a>

### format/0 ###

<pre><code>
format() -&gt; binary()
</code></pre>
<br />

Equivalent to [`format(default)`](#format-1).

Formats `default` registry using the latest text format.

<a name="format-1"></a>

### format/1 ###

<pre><code>
format(Registry::<a href="prometheus_registry.md#type-registry">prometheus_registry:registry()</a>) -&gt; binary()
</code></pre>
<br />

Formats `Registry` using the latest text format.

