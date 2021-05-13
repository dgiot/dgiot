

# Module prometheus_gauge #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Gauge metric, to report instantaneous values.

__Behaviours:__ [`gen_server`](gen_server.md), [`prometheus_collector`](prometheus_collector.md), [`prometheus_metric`](prometheus_metric.md).

<a name="description"></a>

## Description ##

Gauge is a metric that represents a single numerical value that can
arbitrarily go up and down.

A Gauge is typically used for measured values like temperatures or current
memory usage, but also "counts" that can go up and down, like the number of
running processes.

Example use cases for Gauges:

* Inprogress requests

* Number of items in a queue

* Free memory

* Total memory

* Temperature


Example:

```erlang

  -module(my_pool_instrumenter).
  -export([setup/0,
           set_size/1]).
  setup() ->
    prometheus_gauge:declare([{name, my_pool_size},
                              {help, "Pool size."}]),
    prometheus_gauge:declare([{name, my_pool_checked_out},
                              {help, "Number of checked out sockets"}]).
  set_size(Size) ->
    prometheus_gauge:set(my_pool_size, Size)
  track_checked_out_sockets(CheckoutFun) ->
    prometheus_gauge:track_inprogress(my_pool_checked_out, CheckoutFun)..
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ddec-1">ddec/1</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, [], -1)</tt></a>.</td></tr><tr><td valign="top"><a href="#ddec-2">ddec/2</a></td><td>If the second argument is a list, equivalent to
<a href="#dinc-4"><tt>dinc(default, Name, LabelValues, -1)</tt></a>
otherwise equivalent to
<a href="#dinc-4"><tt>dinc(default, Name, [], -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#ddec-3">ddec/3</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, LabelValues, -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#ddec-4">ddec/4</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, LabelValues, -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dec-1">dec/1</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, [], -1)</tt></a>.</td></tr><tr><td valign="top"><a href="#dec-2">dec/2</a></td><td>If the second argument is a list, equivalent to
<a href="#inc-4"><tt>inc(default, Name, LabelValues, -1)</tt></a>
otherwise equivalent to
<a href="#inc-4"><tt>inc(default, Name, [], -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dec-3">dec/3</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, LabelValues, -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dec-4">dec/4</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(Registry, Name, LabelValues, -1 * Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#declare-1">declare/1</a></td><td>Creates a gauge using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#declare-2">declare/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#dinc-1">dinc/1</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, [], 1)</tt></a>.</td></tr><tr><td valign="top"><a href="#dinc-2">dinc/2</a></td><td>If the second argument is a list, equivalent to
<a href="#dinc-4"><tt>dinc(default, Name, LabelValues, 1)</tt></a>
otherwise equivalent to
<a href="#dinc-4"><tt>dinc(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dinc-3">dinc/3</a></td><td>Equivalent to <a href="#dinc-4"><tt>dinc(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#dinc-4">dinc/4</a></td><td>Increments the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> by <code>Value</code>.</td></tr><tr><td valign="top"><a href="#inc-1">inc/1</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, [], 1)</tt></a>.</td></tr><tr><td valign="top"><a href="#inc-2">inc/2</a></td><td>If the second argument is a list, equivalent to
<a href="#inc-4"><tt>inc(default, Name, LabelValues, 1)</tt></a>
otherwise equivalent to
<a href="#inc-4"><tt>inc(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#inc-3">inc/3</a></td><td>Equivalent to <a href="#inc-4"><tt>inc(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#inc-4">inc/4</a></td><td>Increments the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> by <code>Value</code>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a gauge using <code>Spec</code>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Equivalent to <a href="#remove-3"><tt>remove(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>Removes gauge series identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-2">reset/2</a></td><td>Equivalent to <a href="#reset-3"><tt>reset(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td>Resets the value of the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Equivalent to <a href="#set-4"><tt>set(default, Name, [], Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Equivalent to <a href="#set-4"><tt>set(default, Name, LabelValues, Value)</tt></a>.</td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Sets the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> to <code>Value</code>.</td></tr><tr><td valign="top"><a href="#set_duration-2">set_duration/2</a></td><td>Equivalent to <a href="#set_duration-4"><tt>set_duration(default, Name, [], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_duration-3">set_duration/3</a></td><td>Equivalent to <a href="#set_duration-4"><tt>set_duration(default, Name, LabelValues, Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_duration-4">set_duration/4</a></td><td>Sets the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> to the the amount of time spent executing <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#set_to_current_time-1">set_to_current_time/1</a></td><td>Equivalent to <a href="#set_to_current_time-3"><tt>set_to_current_time(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#set_to_current_time-2">set_to_current_time/2</a></td><td>Equivalent to <a href="#set_to_current_time-3"><tt>set_to_current_time(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#set_to_current_time-3">set_to_current_time/3</a></td><td>Sets the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> to the current unixtime.</td></tr><tr><td valign="top"><a href="#track_inprogress-2">track_inprogress/2</a></td><td>Equivalent to <a href="#track_inprogress-4"><tt>track_inprogress(default, Name, [], Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#track_inprogress-3">track_inprogress/3</a></td><td>Equivalent to <a href="#track_inprogress-4"><tt>track_inprogress(default, Name, LabelValues, Fun)</tt></a>.</td></tr><tr><td valign="top"><a href="#track_inprogress-4">track_inprogress/4</a></td><td>Sets the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code> to the number of currently executing <code>Fun</code>s.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#value-2">value/2</a></td><td>Equivalent to <a href="#value-3"><tt>value(default, Name, LabelValues)</tt></a>.</td></tr><tr><td valign="top"><a href="#value-3">value/3</a></td><td>Returns the value of the gauge identified by <code>Registry</code>, <code>Name</code>
and <code>LabelValues</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ddec-1"></a>

### ddec/1 ###

`ddec(Name) -> any()`

Equivalent to [`dinc(default, Name, [], -1)`](#dinc-4).

<a name="ddec-2"></a>

### ddec/2 ###

`ddec(Name, LabelValues) -> any()`

If the second argument is a list, equivalent to
[`dinc(default, Name, LabelValues, -1)`](#dinc-4)
otherwise equivalent to
[`dinc(default, Name, [], -1 * Value)`](#dinc-4).

<a name="ddec-3"></a>

### ddec/3 ###

`ddec(Name, LabelValues, Value) -> any()`

Equivalent to [`dinc(default, Name, LabelValues, -1 * Value)`](#dinc-4).

<a name="ddec-4"></a>

### ddec/4 ###

`ddec(Registry, Name, LabelValues, Value) -> any()`

Equivalent to [`dinc(default, Name, LabelValues, -1 * Value)`](#dinc-4).

<a name="dec-1"></a>

### dec/1 ###

`dec(Name) -> any()`

Equivalent to [`inc(default, Name, [], -1)`](#inc-4).

<a name="dec-2"></a>

### dec/2 ###

`dec(Name, LabelValues) -> any()`

If the second argument is a list, equivalent to
[`inc(default, Name, LabelValues, -1)`](#inc-4)
otherwise equivalent to
[`inc(default, Name, [], -1 * Value)`](#inc-4).

<a name="dec-3"></a>

### dec/3 ###

`dec(Name, LabelValues, Value) -> any()`

Equivalent to [`inc(default, Name, LabelValues, -1 * Value)`](#inc-4).

<a name="dec-4"></a>

### dec/4 ###

`dec(Registry, Name, LabelValues, Value) -> any()`

Equivalent to [`inc(Registry, Name, LabelValues, -1 * Value)`](#inc-4).

<a name="declare-1"></a>

### declare/1 ###

`declare(Spec) -> any()`

Creates a gauge using `Spec`.
If a gauge with the same `Spec` exists returns `false`.

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

<a name="dinc-1"></a>

### dinc/1 ###

`dinc(Name) -> any()`

Equivalent to [`dinc(default, Name, [], 1)`](#dinc-4).

<a name="dinc-2"></a>

### dinc/2 ###

`dinc(Name, LabelValues) -> any()`

If the second argument is a list, equivalent to
[`dinc(default, Name, LabelValues, 1)`](#dinc-4)
otherwise equivalent to
[`dinc(default, Name, [], Value)`](#dinc-4).

<a name="dinc-3"></a>

### dinc/3 ###

`dinc(Name, LabelValues, Value) -> any()`

Equivalent to [`dinc(default, Name, LabelValues, Value)`](#dinc-4).

<a name="dinc-4"></a>

### dinc/4 ###

`dinc(Registry, Name, LabelValues, Value) -> any()`

Increments the gauge identified by `Registry`, `Name`
and `LabelValues` by `Value`.
If `Value` happened to be a float number even one time(!) you
shouldn't use [`inc/4`](#inc-4) after dinc.

Raises `{invalid_value, Value, Message}` if `Value`
isn't a number.<br />
Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="inc-1"></a>

### inc/1 ###

`inc(Name) -> any()`

Equivalent to [`inc(default, Name, [], 1)`](#inc-4).

<a name="inc-2"></a>

### inc/2 ###

`inc(Name, LabelValues) -> any()`

If the second argument is a list, equivalent to
[`inc(default, Name, LabelValues, 1)`](#inc-4)
otherwise equivalent to
[`inc(default, Name, [], Value)`](#inc-4).

<a name="inc-3"></a>

### inc/3 ###

`inc(Name, LabelValues, Value) -> any()`

Equivalent to [`inc(default, Name, LabelValues, Value)`](#inc-4).

<a name="inc-4"></a>

### inc/4 ###

`inc(Registry, Name, LabelValues, Value) -> any()`

Increments the gauge identified by `Registry`, `Name`
and `LabelValues` by `Value`.

Raises `{invalid_value, Value, Message}` if `Value`
isn't an integer.<br />
Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="new-1"></a>

### new/1 ###

`new(Spec) -> any()`

Creates a gauge using `Spec`.

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
Raises `{mf_already_exists, {Registry, Name}, Message}` error if a gauge
with the same `Spec` already exists.

<a name="new-2"></a>

### new/2 ###

`new(Spec, Registry) -> any()`

__This function is deprecated:__ Please use [`new/1`](#new-1) with registry
key instead.

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

Removes gauge series identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
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

Resets the value of the gauge identified by `Registry`, `Name`
and `LabelValues`.

Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="set-2"></a>

### set/2 ###

`set(Name, Value) -> any()`

Equivalent to [`set(default, Name, [], Value)`](#set-4).

<a name="set-3"></a>

### set/3 ###

`set(Name, LabelValues, Value) -> any()`

Equivalent to [`set(default, Name, LabelValues, Value)`](#set-4).

<a name="set-4"></a>

### set/4 ###

`set(Registry, Name, LabelValues, Value) -> any()`

Sets the gauge identified by `Registry`, `Name`
and `LabelValues` to `Value`.

Raises `{invalid_value, Value, Message}` if `Value`
isn't a number or `undefined`.<br />
Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="set_duration-2"></a>

### set_duration/2 ###

`set_duration(Name, Fun) -> any()`

Equivalent to [`set_duration(default, Name, [], Fun)`](#set_duration-4).

<a name="set_duration-3"></a>

### set_duration/3 ###

`set_duration(Name, LabelValues, Fun) -> any()`

Equivalent to [`set_duration(default, Name, LabelValues, Fun)`](#set_duration-4).

<a name="set_duration-4"></a>

### set_duration/4 ###

`set_duration(Registry, Name, LabelValues, Fun) -> any()`

Sets the gauge identified by `Registry`, `Name`
and `LabelValues` to the the amount of time spent executing `Fun`.

Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.
Raises `{invalid_value, Value, Message}` if `Fun`
isn't a function.<br />

<a name="set_to_current_time-1"></a>

### set_to_current_time/1 ###

`set_to_current_time(Name) -> any()`

Equivalent to [`set_to_current_time(default, Name, [])`](#set_to_current_time-3).

<a name="set_to_current_time-2"></a>

### set_to_current_time/2 ###

`set_to_current_time(Name, LabelValues) -> any()`

Equivalent to [`set_to_current_time(default, Name, LabelValues)`](#set_to_current_time-3).

<a name="set_to_current_time-3"></a>

### set_to_current_time/3 ###

`set_to_current_time(Registry, Name, LabelValues) -> any()`

Sets the gauge identified by `Registry`, `Name`
and `LabelValues` to the current unixtime.

Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

<a name="track_inprogress-2"></a>

### track_inprogress/2 ###

`track_inprogress(Name, Fun) -> any()`

Equivalent to [`track_inprogress(default, Name, [], Fun)`](#track_inprogress-4).

<a name="track_inprogress-3"></a>

### track_inprogress/3 ###

`track_inprogress(Name, LabelValues, Fun) -> any()`

Equivalent to [`track_inprogress(default, Name, LabelValues, Fun)`](#track_inprogress-4).

<a name="track_inprogress-4"></a>

### track_inprogress/4 ###

`track_inprogress(Registry, Name, LabelValues, Fun) -> any()`

Sets the gauge identified by `Registry`, `Name`
and `LabelValues` to the number of currently executing `Fun`s.

Raises `{unknown_metric, Registry, Name}` error if gauge with named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.
Raises `{invalid_value, Value, Message}` if `Fun`
isn't a function.<br />

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

Returns the value of the gauge identified by `Registry`, `Name`
and `LabelValues`. If there is no gauge for `LabelValues`,
returns `undefined`.

If duration unit set, value will be converted to the duration unit.
[Read more here.](prometheus_time.md)

Raises `{unknown_metric, Registry, Name}` error if gauge named `Name`
can't be found in `Registry`.<br />
Raises `{invalid_metric_arity, Present, Expected}` error if labels count
mismatch.

