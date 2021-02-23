

# Module prometheus_summary #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Summary metric, to track the size of events.

__Behaviours:__ [`gen_server`](gen_server.md), [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="description"></a>

## Description ##

Example use cases for Summaries:
- Response latency;
- Request size;
- Response size.

Example:

```erlang

  -module(my_proxy_instrumenter).
  setup() ->
    prometheus_summary:declare([{name, request_size_bytes},
                                {help, "Request size in bytes."}]),
    prometheus_summary:declare([{name, response_size_bytes},
                                {help, "Response size in bytes."}]).
  observe_request(Size) ->
    prometheus_summary:observe(request_size_bytes, Size).
  observe_response(Size) ->
    prometheus_summary:observe(response_size_bytes, Size).
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td>Creates a summary using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#dobserve-2">dobserve/2</a></td><td>Equivalent to <a href="#dobserve-4"><tt>dobserve(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dobserve-3">dobserve/3</a></td><td>Equivalent to <a href="#dobserve-4"><tt>dobserve(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dobserve-4">dobserve/4</a></td><td>Observes the given <code>Value</code>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a summary using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#observe-2">observe/2</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-3">observe/3</a></td><td>Equivalent to <a href="#observe-4"><tt>observe(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe-4">observe/4</a></td><td>Observes the given <code>Value</code>.</td></tr><tr><td valign="top"><a href="#observe_duration-2">observe_duration/2</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, [], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-3">observe_duration/3</a></td><td>Equivalent to <a href="#observe_duration-4"><tt>observe_duration(default, Name, LabelValues, Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#observe_duration-4">observe_duration/4</a></td><td>Tracks the amount of time spent executing <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>Removes summary series identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td>Resets the value of the summary identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td>Returns the value of the summary identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

Creates a summary using `Spec`.
If a summary with the same `Spec` exists returns `false`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Soec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />
Raises `{invalid_value_error, Value, MessagE}` error if `duration_unit` is
unknown or doesn't match metric name.<br />

<a name="declare-2"></a>

### declare/2 ###

`declare(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`declare/1`](#declare-1) with registry
key instead.

<a name="dobserve-2"></a>

### dobserve/2 ###

`dobserve(Name, Value) -> any()`

Equivalent to [`dobserve(default, Name, [], Value)`](#dobserve-4).

<a name="dobserve-3"></a>

### dobserve/3 ###

`dobserve(Name, LabelValues, Value) -> any()`

Equivalent to [`dobserve(default, Name, LabelValues, Value)`](#dobserve-4).

<a name="dobserve-4"></a>

### dobserve/4 ###

`dobserve(Registry, Name, LabelValues, Value) -> any()`

Observes the given `Value`.
If `Value` happened to be a float number even one time(!) you
shouldn't use [`observe/4`](#observe-4) after dobserve.

Raises `{invalid_value, Value, Message}` if `Value`
isn't a number.<br />
Raises `{unknown_metric, Registry, Name}` error if summary with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

Creates a summary using `Spec`.

Raises `{missing_metric_spec_key, Key, Spec}` error if required `Soec` key
is missing.<br />
Raises `{invalid_metric_name, Name, Message}` error if metric `Name`
is invalid.<br />
Raises `{invalid_metric_help, Help, Message}` error if metric `Help`
is invalid.<br />
Raises `{invalid_metric_labels, Labels, Message}` error if `Labels`
isn't a list.<br />
Raises `{invalid_label_name, Name, Message}` error if `Name` isn't a valid
label name.<br />
Raises `{invalid_value_error, Value, Message}` error if `duration_unit` is
unknown or doesn't match metric name.<br />
Raises `{mf_already_exists, {Registry, Name}, Message}` error if a summary
with the same `Spec` already exists.

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`new/1`](#new-1) with registry
key instead.

<a name="observe-2"></a>

### observe/2 ###

`observe(Name, Value) -> any()`

Equivalent to [`observe(default, Name, [], Value)`](#observe-4).

<a name="observe-3"></a>

### observe/3 ###

`observe(Name, LabelValues, Value) -> any()`

Equivalent to [`observe(default, Name, LabelValues, Value)`](#observe-4).

<a name="observe-4"></a>

### observe/4 ###

`observe(Registry, Name, LabelValues, Value) -> any()`

Observes the given `Value`.

Raises `{invalid_value, Value, Message}` if `Value`
isn't an integer.<br />
Raises `{unknown_metric, Registry, Name}` error if summary with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="observe_duration-2"></a>

### observe_duration/2 ###

`observe_duration(Name, Fun) -> any()`

Equivalent to [`observe_duration(default, Name, [], Fun)`](#observe_duration-4).

<a name="observe_duration-3"></a>

### observe_duration/3 ###

`observe_duration(Name, LabelValues, Fun) -> any()`

Equivalent to [`observe_duration(default, Name, LabelValues, Fun)`](#observe_duration-4).

<a name="observe_duration-4"></a>

### observe_duration/4 ###

`observe_duration(Registry, Name, LabelValues, Fun) -> any()`

Tracks the amount of time spent executing `Fun`.

Raises `{unknown_metric, Registry, Name}` error if summary with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.
Raises `{invalid_value, Value, Message}` if `Fun`
isn't a function.<br />

<a name="remove-1"></a>

### remove/1 ###

`remove(Name) -> any()`

Equivalent to [`remove(default, Name, [])`](#remove-3).

<a name="remove-2"></a>

### remove/2 ###

`remove(Name, LabelValues) -> any()`

Equivalent to [`remove(default, Name, LabelValues)`](#remove-3).

<a name="remove-3"></a>

### remove/3 ###

`remove(Registry, Name, LabelValues) -> any()`

Removes summary series identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if summary with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="reset-1"></a>

### reset/1 ###

`reset(Name) -> any()`

Equivalent to [`reset(default, Name, [])`](#reset-3).

<a name="reset-2"></a>

### reset/2 ###

`reset(Name, LabelValues) -> any()`

Equivalent to [`reset(default, Name, LabelValues)`](#reset-3).

<a name="reset-3"></a>

### reset/3 ###

`reset(Registry, Name, LabelValues) -> any()`

Resets the value of the summary identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if summary with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="value-1"></a>

### value/1 ###

`value(Name) -> any()`

Equivalent to [`value(default, Name, [])`](#value-3).

<a name="value-2"></a>

### value/2 ###

`value(Name, LabelValues) -> any()`

Equivalent to [`value(default, Name, LabelValues)`](#value-3).

<a name="value-3"></a>

### value/3 ###

`value(Registry, Name, LabelValues) -> any()`

Returns the value of the summary identified by `Registry`, `Name`
and `LabelValues`. If there is no summary for `LabelValues`,
returns `undefined`.

If duration unit set, sum will be converted to the duration unit.
[Read more here.](prometheus_time.md)

Raises `{unknown_metric, Registry, Name}` error if summary named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

