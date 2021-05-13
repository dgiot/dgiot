

# Module gproc_ps #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Gproc Publish/Subscribe patterns
This module implements a few convenient functions for publish/subscribe.

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="description"></a>

## Description ##

Publish/subscribe with Gproc relies entirely on gproc properties and counters.
This makes for a very concise implementation, as the monitoring of subscribers and
removal of subscriptions comes for free with Gproc.

Using this module instead of rolling your own (which is easy enough) brings the
benefit of consistency, in tracing and debugging.
The implementation can also serve to illustrate how to use gproc properties and
counters to good effect.

<a name="types"></a>

## Data Types ##




### <a name="type-event">event()</a> ###


<pre><code>
event() = any()
</code></pre>




### <a name="type-msg">msg()</a> ###


<pre><code>
msg() = any()
</code></pre>




### <a name="type-scope">scope()</a> ###


<pre><code>
scope() = l | g
</code></pre>




### <a name="type-status">status()</a> ###


<pre><code>
status() = 1 | 0
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change_cond-3">change_cond/3</a></td><td>Change the condition specification of an existing subscription.</td></tr><tr><td valign="top"><a href="#create_single-2">create_single/2</a></td><td>Creates a single-shot subscription entry for Event.</td></tr><tr><td valign="top"><a href="#delete_single-2">delete_single/2</a></td><td>Deletes the single-shot subscription for Event.</td></tr><tr><td valign="top"><a href="#disable_single-2">disable_single/2</a></td><td>Disables the single-shot subscription for Event.</td></tr><tr><td valign="top"><a href="#enable_single-2">enable_single/2</a></td><td>Enables the single-shot subscription for Event.</td></tr><tr><td valign="top"><a href="#list_singles-2">list_singles/2</a></td><td>Lists all single-shot subscribers of Event, together with their status.</td></tr><tr><td valign="top"><a href="#list_subs-2">list_subs/2</a></td><td>List the pids of all processes subscribing to <code>Event</code></td></tr><tr><td valign="top"><a href="#notify_single_if_true-4">notify_single_if_true/4</a></td><td>Create/enable a single subscription for event; notify at once if F() -> true.</td></tr><tr><td valign="top"><a href="#publish-3">publish/3</a></td><td>Publish the message <code>Msg</code> to all subscribers of <code>Event</code></td></tr><tr><td valign="top"><a href="#publish_cond-3">publish_cond/3</a></td><td>Publishes the message <code>Msg</code> to conditional subscribers of <code>Event</code></td></tr><tr><td valign="top"><a href="#subscribe-2">subscribe/2</a></td><td>Subscribe to events of type <code>Event</code></td></tr><tr><td valign="top"><a href="#subscribe_cond-3">subscribe_cond/3</a></td><td>Subscribe conditionally to events of type <code>Event</code></td></tr><tr><td valign="top"><a href="#tell_singles-3">tell_singles/3</a></td><td>Publish <code>Msg</code> to all single-shot subscribers of <code>Event</code></td></tr><tr><td valign="top"><a href="#unsubscribe-2">unsubscribe/2</a></td><td>Remove subscribtion created using <code>subscribe(Scope, Event)</code></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="change_cond-3"></a>

### change_cond/3 ###

<pre><code>
change_cond(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, Spec::undefined | <a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; true
</code></pre>
<br />

Change the condition specification of an existing subscription.

This function atomically changes the condition spec of an existing
subscription (see [`subscribe_cond/3`](#subscribe_cond-3)). An exception is raised if
the subscription doesn't already exist.

Note that this function can also be used to change a conditional subscription
to an unconditional one (by setting `Spec = undefined`), or a 'normal'
subscription to a conditional one.

<a name="create_single-2"></a>

### create_single/2 ###

<pre><code>
create_single(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; true
</code></pre>
<br />

Creates a single-shot subscription entry for Event

Single-shot subscriptions behave similarly to the `{active,once}` property of sockets.
Once a message has been published, the subscription is disabled, and no more messages
will be delivered to the subscriber unless the subscription is re-enabled using
`enable_single/2`.

The function creates a gproc counter entry, `{c,Scope,{gproc_ps_event,Event}}`, which
will have either of the values `0` (disabled) or `1` (enabled). Initially, the value
is `1`, meaning the subscription is enabled.

Counters are used in this case, since they can be atomically updated by both the
subscriber (owner) and publisher. The publisher sets the counter value to `0` as soon
as it has delivered a message.

<a name="delete_single-2"></a>

### delete_single/2 ###

<pre><code>
delete_single(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; true
</code></pre>
<br />

Deletes the single-shot subscription for Event

This function deletes the counter entry representing the single-shot description.
An exception will be raised if there is no such subscription.

<a name="disable_single-2"></a>

### disable_single/2 ###

<pre><code>
disable_single(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; integer()
</code></pre>
<br />

Disables the single-shot subscription for Event

This function changes the value of the corresponding gproc counter to `0` (disabled).

The subscription remains (e.g. for debugging purposes), but with a 'disabled' status.
This function is insensitive to concurrency, using 'wrapping' ets counter update ops.
This guarantees that the counter will have either the value 1 or 0, depending on which
update happened last.

The return value indicates the previous status.

<a name="enable_single-2"></a>

### enable_single/2 ###

<pre><code>
enable_single(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; integer()
</code></pre>
<br />

Enables the single-shot subscription for Event

This function changes the value of the corresponding gproc counter to `1` (enabled).

After enabling, the subscriber will receive the next message published for `Event`,
after which the subscription is automatically disabled.

This function is insensitive to concurrency, using 'wrapping' ets counter update ops.
This guarantees that the counter will have either the value 1 or 0, depending on which
update happened last.

The return value indicates the previous status.

<a name="list_singles-2"></a>

### list_singles/2 ###

<pre><code>
list_singles(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; [{pid(), <a href="#type-status">status()</a>}]
</code></pre>
<br />

Lists all single-shot subscribers of Event, together with their status

<a name="list_subs-2"></a>

### list_subs/2 ###

<pre><code>
list_subs(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; [pid()]
</code></pre>
<br />

List the pids of all processes subscribing to `Event`

This function uses `gproc:select/2` to find all properties indicating a subscription.

<a name="notify_single_if_true-4"></a>

### notify_single_if_true/4 ###

<pre><code>
notify_single_if_true(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, F::fun(() -&gt; boolean()), Msg::<a href="#type-msg">msg()</a>) -&gt; ok
</code></pre>
<br />

Create/enable a single subscription for event; notify at once if F() -> true

This function is a convenience function, wrapping a single-shot pub/sub around a
user-provided boolean test. `Msg` should be what the publisher will send later, if the
immediate test returns `false`.

<a name="publish-3"></a>

### publish/3 ###

<pre><code>
publish(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, Msg::<a href="#type-msg">msg()</a>) -&gt; ok
</code></pre>
<br />

Publish the message `Msg` to all subscribers of `Event`

The message delivered to each subscriber will be of the form:

`{gproc_ps_event, Event, Msg}`

The function uses `gproc:send/2` to send a message to all processes which have a
property `{p,Scope,{gproc_ps_event,Event}}`.

<a name="publish_cond-3"></a>

### publish_cond/3 ###

<pre><code>
publish_cond(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, Msg::<a href="#type-msg">msg()</a>) -&gt; <a href="#type-msg">msg()</a>
</code></pre>
<br />

Publishes the message `Msg` to conditional subscribers of `Event`

The message will be delivered to each subscriber provided their respective
condition tests succeed.

__See also:__ [subscribe_cond/3](#subscribe_cond-3).

<a name="subscribe-2"></a>

### subscribe/2 ###

<pre><code>
subscribe(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; true
</code></pre>
<br />

Subscribe to events of type `Event`

Any messages published with `gproc_ps:publish(Scope, Event, Msg)` will be
delivered to the current process, along with all other subscribers.

This function creates a property, `{p,Scope,{gproc_ps_event,Event}}`, which
can be searched and displayed for debugging purposes.

Note that, as with [`gproc:reg/1`](gproc.md#reg-1), this function will raise an
exception if you try to subscribe to the same event twice from the same
process.

<a name="subscribe_cond-3"></a>

### subscribe_cond/3 ###

<pre><code>
subscribe_cond(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, Spec::undefined | <a href="ets.md#type-match_spec">ets:match_spec()</a>) -&gt; true
</code></pre>
<br />

Subscribe conditionally to events of type `Event`

This function is similar to [`subscribe/2`](#subscribe-2), but adds a condition
in the form of a match specification.

The condition is tested by the [`publish_cond/3`](#publish_cond-3) function
and a message is delivered only if the condition is true. Specifically,
the test is:

`ets:match_spec_run([Msg], ets:match_spec_compile(Cond)) == [true]`

In other words, if the match_spec returns true for a message, that message
is sent to the subscriber. For any other result from the match_spec, the
message is not sent. `Cond == undefined` means that all messages will be
delivered (that is, `publish_cond/3` will treat 'normal' subscribers just
like [`publish/3`](#publish-3) does, except that `publish/3` strictly speaking
ignores the Value part of the property completely, whereas `publish_cond/3`
expects it to be either undefined or a valid match spec).

This means that `Cond=undefined` and `Cond=[{'_',[],[true]}]` are
equivalent.

Note that, as with [`gproc:reg/1`](gproc.md#reg-1), this function will raise an
exception if you try to subscribe to the same event twice from the same
process.

<a name="tell_singles-3"></a>

### tell_singles/3 ###

<pre><code>
tell_singles(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>, Msg::<a href="#type-msg">msg()</a>) -&gt; [pid()]
</code></pre>
<br />

Publish `Msg` to all single-shot subscribers of `Event`

The subscriber status of each active subscriber is changed to `0` (disabled) before
delivering the message. This reduces the risk that two different processes will be able
to both deliver a message before disabling the subscribers. This could happen if the
context switch happens just after the select operation (finding the active subscribers)
and before the process is able to update the counters. In this case, it is possible
that more than one can be delivered.

The way to prevent this from happening is to ensure that only one process publishes
for `Event`.

<a name="unsubscribe-2"></a>

### unsubscribe/2 ###

<pre><code>
unsubscribe(Scope::<a href="#type-scope">scope()</a>, Event::<a href="#type-event">event()</a>) -&gt; true
</code></pre>
<br />

Remove subscribtion created using `subscribe(Scope, Event)`

This removes the property created through `subscribe/2`.

