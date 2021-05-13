

# Module gproc_monitor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module implements a notification system for gproc names
When a process subscribes to notifications for a given name, a message
will be sent each time that name is registered.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Ulf Wiger ([`ulf.wiger@feuerlabs.com`](mailto:ulf.wiger@feuerlabs.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td>
Subscribe to registration events for a certain name.</td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>
Unsubscribe from registration events for a certain name.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>
<br />

Starts the server

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

Subscribe to registration events for a certain name

The subscribing process will receive a `{gproc_monitor, Name, Pid}` message
whenever a process registers under the given name, and a
`{gproc_monitor, Name, undefined}` message when the name is unregistered,
either explicitly, or because the registered process dies.

When the subscription is first ordered, one of the above messages will be
sent immediately, indicating the current status of the name.

<a name="unsubscribe-1"></a>

### unsubscribe/1 ###

<pre><code>
unsubscribe(Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

Unsubscribe from registration events for a certain name

This function is the reverse of subscribe/1. It removes the subscription.

