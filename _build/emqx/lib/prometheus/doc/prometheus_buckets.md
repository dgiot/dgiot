

# Module prometheus_buckets #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-bucket_bound">bucket_bound()</a> ###


<pre><code>
bucket_bound() = number() | infinity
</code></pre>




### <a name="type-buckets">buckets()</a> ###


<pre><code>
buckets() = [<a href="#type-bucket_bound">bucket_bound()</a>, ...]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default-0">default/0</a></td><td>
Default histogram buckets.</td></tr><tr><td valign="top"><a href="#exponential-3">exponential/3</a></td><td>
Creates <code>Count</code> buckets, where the lowest bucket has an
upper bound of <code>Start</code> and each following bucket's upper bound is <code>Factor</code>
times the previous bucket's upper bound.</td></tr><tr><td valign="top"><a href="#linear-3">linear/3</a></td><td>
Creates <code>Count</code> buckets, each <code>Width</code> wide, where the lowest
bucket has an upper bound of <code>Start</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default-0"></a>

### default/0 ###

<pre><code>
default() -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Default histogram buckets.

```erlang

  1> prometheus_buckets:default().
  [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
```

Please note these buckets are floats and represent seconds so you'll
have to use [`prometheus_histogram:dobserve/3`](prometheus_histogram.md#dobserve-3) or
configure `duration_unit` as `seconds`.

<a name="exponential-3"></a>

### exponential/3 ###

<pre><code>
exponential(Start::number(), Factor::number(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Creates `Count` buckets, where the lowest bucket has an
upper bound of `Start` and each following bucket's upper bound is `Factor`
times the previous bucket's upper bound. The returned list is meant to be
used for the `buckets` key of histogram constructors options.

```erlang

  3> prometheus_buckets:exponential(100, 1.2, 3).
  [100, 120, 144]
```

The function raises `{invalid_value, Value, Message}` error if `Count`
isn't positive, if `Start` isn't positive, or if `Factor` is less than or
equals to 1.

<a name="linear-3"></a>

### linear/3 ###

<pre><code>
linear(Start::number(), Step::number(), Count::pos_integer()) -&gt; <a href="#type-buckets">buckets()</a>
</code></pre>
<br />

Creates `Count` buckets, each `Width` wide, where the lowest
bucket has an upper bound of `Start`. The returned list is meant to be
used for the `buckets` key of histogram constructors options.

```erlang

  2> prometheus_buckets:linear(10, 5, 6).
  [10, 15, 20, 25, 30, 35]
```

The function raises `{invalid_value, Value, Message}` error if `Count`
is zero or negative.

