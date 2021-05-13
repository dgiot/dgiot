

# Module gproc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Extended process registry
This module implements an extended process registry.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="description"></a>

## Description ##

For a detailed description, see
[erlang07-wiger.pdf](erlang07-wiger.pdf).

__NOTE:__ The functions in the Gproc API expect the Gproc application
to be running.


## Tuning Gproc performance ##

Gproc relies on a central server and an ordered-set ets table.
Effort is made to perform as much work as possible in the client without
sacrificing consistency. A few things can be tuned by setting the following
application environment variables in the top application of `gproc`
(usually `gproc`):

* `{ets_options, list()}` - Currently, the options `{write_concurrency, F}`
and `{read_concurrency, F}` are allowed. The default is
`[{write_concurrency, true}, {read_concurrency, true}]`
* `{server_options, list()}` - These will be passed as spawn options when
starting the `gproc` and `gproc_dist` servers. Default is `[]`. It is
likely that `{priority, high | max}` and/or increasing `min_heap_size`
will improve performance.

<a name="types"></a>

## Data Types ##




### <a name="type-context">context()</a> ###


<pre><code>
context() = {<a href="#type-scope">scope()</a>, <a href="#type-type">type()</a>} | <a href="#type-type">type()</a>
</code></pre>




### <a name="type-ctr_incr">ctr_incr()</a> ###


<pre><code>
ctr_incr() = integer()
</code></pre>




### <a name="type-ctr_setval">ctr_setval()</a> ###


<pre><code>
ctr_setval() = integer()
</code></pre>




### <a name="type-ctr_thr">ctr_thr()</a> ###


<pre><code>
ctr_thr() = integer()
</code></pre>




### <a name="type-ctr_update">ctr_update()</a> ###


<pre><code>
ctr_update() = <a href="#type-ctr_incr">ctr_incr()</a> | {<a href="#type-ctr_incr">ctr_incr()</a>, <a href="#type-ctr_thr">ctr_thr()</a>, <a href="#type-ctr_setval">ctr_setval()</a>}
</code></pre>




### <a name="type-headpat">headpat()</a> ###


<pre><code>
headpat() = {<a href="#type-keypat">keypat()</a>, <a href="#type-pidpat">pidpat()</a>, any()}
</code></pre>




### <a name="type-increment">increment()</a> ###


<pre><code>
increment() = <a href="#type-ctr_incr">ctr_incr()</a> | <a href="#type-ctr_update">ctr_update()</a> | [<a href="#type-ctr_update">ctr_update()</a>]
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = {<a href="#type-type">type()</a>, <a href="#type-scope">scope()</a>, any()}
</code></pre>




### <a name="type-keypat">keypat()</a> ###


<pre><code>
keypat() = {<a href="#type-sel_type">sel_type()</a> | <a href="#type-sel_var">sel_var()</a>, l | g | <a href="#type-sel_var">sel_var()</a>, any()}
</code></pre>




### <a name="type-monitor_type">monitor_type()</a> ###


<pre><code>
monitor_type() = info | standby | follow
</code></pre>




### <a name="type-pidpat">pidpat()</a> ###


<pre><code>
pidpat() = pid() | <a href="#type-sel_var">sel_var()</a>
</code></pre>




### <a name="type-reg_id">reg_id()</a> ###


<pre><code>
reg_id() = {<a href="#type-type">type()</a>, <a href="#type-scope">scope()</a>, any()}
</code></pre>




### <a name="type-scope">scope()</a> ###


<pre><code>
scope() = l | g
</code></pre>




### <a name="type-sel_context">sel_context()</a> ###


<pre><code>
sel_context() = {<a href="#type-scope">scope()</a>, <a href="#type-type">type()</a>} | <a href="#type-type">type()</a>
</code></pre>




### <a name="type-sel_pattern">sel_pattern()</a> ###


<pre><code>
sel_pattern() = [{<a href="#type-headpat">headpat()</a>, list(), list()}]
</code></pre>




### <a name="type-sel_scope">sel_scope()</a> ###


<pre><code>
sel_scope() = scope | all | global | local
</code></pre>




### <a name="type-sel_type">sel_type()</a> ###


<pre><code>
sel_type() = <a href="#type-type">type()</a> | names | props | counters | aggr_counters | resources | resource_counters
</code></pre>




### <a name="type-sel_var">sel_var()</a> ###


<pre><code>
sel_var() = '_' | atom()
</code></pre>




### <a name="type-type">type()</a> ###


<pre><code>
type() = n | p | c | a | r | rc
</code></pre>




### <a name="type-unique_id">unique_id()</a> ###


<pre><code>
unique_id() = {n | a, <a href="#type-scope">scope()</a>, any()}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_global_aggr_counter-1">add_global_aggr_counter/1</a></td><td>Registers a global (unique) aggregated counter.</td></tr><tr><td valign="top"><a href="#add_global_counter-2">add_global_counter/2</a></td><td>Registers a global (non-unique) counter.</td></tr><tr><td valign="top"><a href="#add_global_name-1">add_global_name/1</a></td><td>Registers a global (unique) name.</td></tr><tr><td valign="top"><a href="#add_global_property-2">add_global_property/2</a></td><td>Registers a global (non-unique) property.</td></tr><tr><td valign="top"><a href="#add_local_aggr_counter-1">add_local_aggr_counter/1</a></td><td>Registers a local (unique) aggregated counter.</td></tr><tr><td valign="top"><a href="#add_local_counter-2">add_local_counter/2</a></td><td>Registers a local (non-unique) counter.</td></tr><tr><td valign="top"><a href="#add_local_name-1">add_local_name/1</a></td><td>Registers a local (unique) name.</td></tr><tr><td valign="top"><a href="#add_local_property-2">add_local_property/2</a></td><td>Registers a local (non-unique) property.</td></tr><tr><td valign="top"><a href="#add_shared_local_counter-2">add_shared_local_counter/2</a></td><td>Registers a local shared (unique) counter.</td></tr><tr><td valign="top"><a href="#audit_process-1">audit_process/1</a></td><td></td></tr><tr><td valign="top"><a href="#await-1">await/1</a></td><td>Equivalent to <a href="#await-2"><tt>await(Key, infinity)</tt></a>.</td></tr><tr><td valign="top"><a href="#await-2">await/2</a></td><td>Wait for a name or aggregated counter to be registered.</td></tr><tr><td valign="top"><a href="#await-3">await/3</a></td><td>Wait for a name or aggregated counter to be registered on <code>Node</code>.</td></tr><tr><td valign="top"><a href="#bcast-2">bcast/2</a></td><td>Equivalent to <a href="#bcast-3"><tt>bcast(nodes(), Key, Msg)</tt></a>.</td></tr><tr><td valign="top"><a href="#bcast-3">bcast/3</a></td><td>Sends a message to processes corresponding to Key on Nodes.</td></tr><tr><td valign="top"><a href="#cancel_wait-2">cancel_wait/2</a></td><td>Cancels a previous call to nb_wait/1.</td></tr><tr><td valign="top"><a href="#cancel_wait-3">cancel_wait/3</a></td><td>Cancels a previous call to nb_wait/2.</td></tr><tr><td valign="top"><a href="#cancel_wait_or_monitor-1">cancel_wait_or_monitor/1</a></td><td></td></tr><tr><td valign="top"><a href="#default-1">default/1</a></td><td></td></tr><tr><td valign="top"><a href="#demonitor-2">demonitor/2</a></td><td>Remove a monitor on a registered name
This function is the reverse of monitor/1.</td></tr><tr><td valign="top"><a href="#ensure_reg-1">ensure_reg/1</a></td><td>Equivalent to <a href="#ensure_reg-3"><tt>ensure_reg(Key, default(Key), [])</tt></a>.</td></tr><tr><td valign="top"><a href="#ensure_reg-2">ensure_reg/2</a></td><td>Equivalent to <a href="#ensure_reg-3"><tt>ensure_reg(Key, Value, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#ensure_reg-3">ensure_reg/3</a></td><td>Registers a new name or property unless such and entry (by key) has
already been registered by the current process.</td></tr><tr><td valign="top"><a href="#ensure_reg_other-2">ensure_reg_other/2</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_reg_other-3">ensure_reg_other/3</a></td><td>Equivalent to <a href="#ensure_reg_other-4"><tt>ensure_reg_other(Key, Pid, Value, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#ensure_reg_other-4">ensure_reg_other/4</a></td><td>Register or update name or property to another process.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td>Behaves as ets:first(Tab) for a given type of registration.</td></tr><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td>Get attribute value of <code>Attr</code> associated with <code>Key</code> for most likely Pid.</td></tr><tr><td valign="top"><a href="#get_attribute-3">get_attribute/3</a></td><td>Get the attribute value of <code>Attr</code> associated with <code>Key</code> for process Pid.</td></tr><tr><td valign="top"><a href="#get_attribute_shared-2">get_attribute_shared/2</a></td><td>Get the attribute value of <code>Attr</code> associated with the shared <code>Key</code>.</td></tr><tr><td valign="top"><a href="#get_attributes-1">get_attributes/1</a></td><td>Get attributes associated with registration.</td></tr><tr><td valign="top"><a href="#get_attributes-2">get_attributes/2</a></td><td>Returns the list of attributes associated with the registration.</td></tr><tr><td valign="top"><a href="#get_env-3">get_env/3</a></td><td>Equivalent to <a href="#get_env-4"><tt>get_env(Scope, App, Key, [app_env])</tt></a>.</td></tr><tr><td valign="top"><a href="#get_env-4">get_env/4</a></td><td>Read an environment value, potentially cached as a <code>gproc_env</code> property.</td></tr><tr><td valign="top"><a href="#get_set_env-3">get_set_env/3</a></td><td>Equivalent to <a href="#get_set_env-4"><tt>get_set_env(Scope, App, Key, [app_env])</tt></a>.</td></tr><tr><td valign="top"><a href="#get_set_env-4">get_set_env/4</a></td><td>Fetch and cache an environment value, if not already cached.</td></tr><tr><td valign="top"><a href="#get_value-1">get_value/1</a></td><td>Reads the value stored with a key registered to the current process.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>Reads the value stored with a key registered to the process Pid.</td></tr><tr><td valign="top"><a href="#get_value_shared-1">get_value_shared/1</a></td><td>Reads the value stored with a shared key.</td></tr><tr><td valign="top"><a href="#give_away-2">give_away/2</a></td><td>Atomically transfers the key <code>From</code> to the process identified by <code>To</code>.</td></tr><tr><td valign="top"><a href="#goodbye-0">goodbye/0</a></td><td>Unregister all items of the calling process and inform gproc
to forget about the calling process.</td></tr><tr><td valign="top"><a href="#i-0">i/0</a></td><td>Similar to the built-in shell command <code>i()</code> but inserts information
about names and properties registered in Gproc, where applicable.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Similar to <code>process_info(Pid)</code> but with additional gproc info.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Similar to process_info(Pid, Item), but with additional gproc info.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td>Behaves as ets:last(Tab) for a given type of registration.</td></tr><tr><td valign="top"><a href="#lookup_global_aggr_counter-1">lookup_global_aggr_counter/1</a></td><td>Lookup a global (unique) aggregated counter and returns its value.</td></tr><tr><td valign="top"><a href="#lookup_global_counters-1">lookup_global_counters/1</a></td><td>Look up all global (non-unique) instances of a given Counter.</td></tr><tr><td valign="top"><a href="#lookup_global_name-1">lookup_global_name/1</a></td><td>Lookup a global unique name.</td></tr><tr><td valign="top"><a href="#lookup_global_properties-1">lookup_global_properties/1</a></td><td>Look up all global (non-unique) instances of a given Property.</td></tr><tr><td valign="top"><a href="#lookup_local_aggr_counter-1">lookup_local_aggr_counter/1</a></td><td>Lookup a local (unique) aggregated counter and returns its value.</td></tr><tr><td valign="top"><a href="#lookup_local_counters-1">lookup_local_counters/1</a></td><td>Look up all local (non-unique) instances of a given Counter.</td></tr><tr><td valign="top"><a href="#lookup_local_name-1">lookup_local_name/1</a></td><td>Lookup a local unique name.</td></tr><tr><td valign="top"><a href="#lookup_local_properties-1">lookup_local_properties/1</a></td><td>Look up all local (non-unique) instances of a given Property.</td></tr><tr><td valign="top"><a href="#lookup_pid-1">lookup_pid/1</a></td><td>Lookup the Pid stored with a key.</td></tr><tr><td valign="top"><a href="#lookup_pids-1">lookup_pids/1</a></td><td>Returns a list of pids with the published key Key.</td></tr><tr><td valign="top"><a href="#lookup_value-1">lookup_value/1</a></td><td>Lookup the value stored with a key.</td></tr><tr><td valign="top"><a href="#lookup_values-1">lookup_values/1</a></td><td>Retrieve the <code>{Pid,Value}</code> pairs corresponding to Key.</td></tr><tr><td valign="top"><a href="#monitor-1">monitor/1</a></td><td>Equivalent to <a href="#monitor-2"><tt>monitor(Key, info)</tt></a>.</td></tr><tr><td valign="top"><a href="#monitor-2">monitor/2</a></td><td>monitor a registered name
<code>monitor(Key, info)</code> works much like erlang:monitor(process, Pid), but monitors
a unique name registered via gproc.</td></tr><tr><td valign="top"><a href="#mreg-3">mreg/3</a></td><td>Register multiple {Key,Value} pairs of a given type and scope.</td></tr><tr><td valign="top"><a href="#munreg-3">munreg/3</a></td><td>Unregister multiple Key items of a given type and scope.</td></tr><tr><td valign="top"><a href="#nb_wait-1">nb_wait/1</a></td><td>Wait for a name or aggregated counter to be registered.</td></tr><tr><td valign="top"><a href="#nb_wait-2">nb_wait/2</a></td><td>Wait for a name or aggregated counter to be registered on <code>Node</code>.</td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td>Behaves as ets:next(Tab,Key) for a given type of registration.</td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td>Behaves as ets:prev(Tab,Key) for a given type of registration.</td></tr><tr><td valign="top"><a href="#reg-1">reg/1</a></td><td>Equivalent to <a href="#reg-3"><tt>reg(Key, default(Key), [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reg-2">reg/2</a></td><td>Register a name or property for the current process.</td></tr><tr><td valign="top"><a href="#reg-3">reg/3</a></td><td>Register a name or property for the current process
<code>Attrs</code> (default: <code>[]</code>) can be inspected using <a href="#get_attribute-2"><code>get_attribute/2</code></a>.</td></tr><tr><td valign="top"><a href="#reg_or_locate-1">reg_or_locate/1</a></td><td>Equivalent to <a href="#reg_or_locate-2"><tt>reg_or_locate(Key, default(Key))</tt></a>.</td></tr><tr><td valign="top"><a href="#reg_or_locate-2">reg_or_locate/2</a></td><td>Try registering a unique name, or return existing registration.</td></tr><tr><td valign="top"><a href="#reg_or_locate-3">reg_or_locate/3</a></td><td>Spawn a process with a registered name, or return existing registration.</td></tr><tr><td valign="top"><a href="#reg_other-2">reg_other/2</a></td><td>Equivalent to <a href="#reg_other-4"><tt>reg_other(Key, Pid, default(Key), [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reg_other-3">reg_other/3</a></td><td>Equivalent to <a href="#reg_other-4"><tt>reg_other(Key, Pid, Value, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#reg_other-4">reg_other/4</a></td><td>Register name or property to another process.</td></tr><tr><td valign="top"><a href="#reg_shared-1">reg_shared/1</a></td><td>Register a resource, but don't tie it to a particular process.</td></tr><tr><td valign="top"><a href="#reg_shared-2">reg_shared/2</a></td><td>Register a resource, but don't tie it to a particular process.</td></tr><tr><td valign="top"><a href="#reg_shared-3">reg_shared/3</a></td><td></td></tr><tr><td valign="top"><a href="#register_name-2">register_name/2</a></td><td>Behaviour support callback.</td></tr><tr><td valign="top"><a href="#reset_counter-1">reset_counter/1</a></td><td>Reads and resets a counter in a "thread-safe" way.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td>Perform a select operation on the process registry.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td>Perform a select operation with limited context on the process registry.</td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td>Like <a href="#select-2"><code>select/2</code></a> but returns Limit objects at a time.</td></tr><tr><td valign="top"><a href="#select_count-1">select_count/1</a></td><td>Equivalent to <a href="#select_count-2"><tt>select_count(all, Pat)</tt></a>.</td></tr><tr><td valign="top"><a href="#select_count-2">select_count/2</a></td><td>Perform a select_count operation on the process registry.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Sends a message to the process, or processes, corresponding to Key.</td></tr><tr><td valign="top"><a href="#set_attributes-2">set_attributes/2</a></td><td>Add/modify <code>{Key, Value}</code> attributes associated with a registration.</td></tr><tr><td valign="top"><a href="#set_attributes_shared-2">set_attributes_shared/2</a></td><td>Add/modify <code>{Key, Value}</code> attributes associated with a shared registration.</td></tr><tr><td valign="top"><a href="#set_env-5">set_env/5</a></td><td>Updates the cached value as well as underlying environment.</td></tr><tr><td valign="top"><a href="#set_value-2">set_value/2</a></td><td>Sets the value of the registration given by Key.</td></tr><tr><td valign="top"><a href="#set_value_shared-2">set_value_shared/2</a></td><td>Sets the value of the shared registration given by Key.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the gproc server.</td></tr><tr><td valign="top"><a href="#table-0">table/0</a></td><td>Equivalent to <a href="#table-1"><tt>table({all, all})</tt></a>.</td></tr><tr><td valign="top"><a href="#table-1">table/1</a></td><td>Equivalent to <a href="#table-2"><tt>table(Context, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#table-2">table/2</a></td><td>QLC table generator for the gproc registry.</td></tr><tr><td valign="top"><a href="#unreg-1">unreg/1</a></td><td>Unregister a name or property.</td></tr><tr><td valign="top"><a href="#unreg_other-2">unreg_other/2</a></td><td>Unregister a name registered to another process.</td></tr><tr><td valign="top"><a href="#unreg_shared-1">unreg_shared/1</a></td><td>Unregister a shared resource.</td></tr><tr><td valign="top"><a href="#unregister_name-1">unregister_name/1</a></td><td>Equivalent to <tt>unreg / 1</tt>.</td></tr><tr><td valign="top"><a href="#update_counter-2">update_counter/2</a></td><td>Updates the counter registered as Key for the current process.</td></tr><tr><td valign="top"><a href="#update_counter-3">update_counter/3</a></td><td></td></tr><tr><td valign="top"><a href="#update_counters-2">update_counters/2</a></td><td>Update a list of counters.</td></tr><tr><td valign="top"><a href="#update_shared_counter-2">update_shared_counter/2</a></td><td>Updates the shared counter registered as Key.</td></tr><tr><td valign="top"><a href="#where-1">where/1</a></td><td>Returns the pid registered as Key.</td></tr><tr><td valign="top"><a href="#whereis_name-1">whereis_name/1</a></td><td>Equivalent to <tt>where / 1</tt>.</td></tr><tr><td valign="top"><a href="#wide_await-3">wide_await/3</a></td><td>Wait for a local name to be registered on any of <code>Nodes</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_global_aggr_counter-1"></a>

### add_global_aggr_counter/1 ###

`add_global_aggr_counter(Name) -> any()`

Equivalent to [`reg({a, g, Name})`](#reg-1).

Registers a global (unique) aggregated counter.

<a name="add_global_counter-2"></a>

### add_global_counter/2 ###

`add_global_counter(Name, Initial) -> any()`

Registers a global (non-unique) counter. @equiv reg({c,g,Name},Value)

<a name="add_global_name-1"></a>

### add_global_name/1 ###

`add_global_name(Name) -> any()`

Registers a global (unique) name. @equiv reg({n,g,Name})

<a name="add_global_property-2"></a>

### add_global_property/2 ###

`add_global_property(Name, Value) -> any()`

Registers a global (non-unique) property. @equiv reg({p,g,Name},Value)

<a name="add_local_aggr_counter-1"></a>

### add_local_aggr_counter/1 ###

`add_local_aggr_counter(Name) -> any()`

Equivalent to [`reg({a, l, Name})`](#reg-1).

Registers a local (unique) aggregated counter.

<a name="add_local_counter-2"></a>

### add_local_counter/2 ###

`add_local_counter(Name, Initial) -> any()`

Registers a local (non-unique) counter. @equiv reg({c,l,Name},Value)

<a name="add_local_name-1"></a>

### add_local_name/1 ###

`add_local_name(Name) -> any()`

Registers a local (unique) name. @equiv reg({n,l,Name})

<a name="add_local_property-2"></a>

### add_local_property/2 ###

`add_local_property(Name, Value) -> any()`

Registers a local (non-unique) property. @equiv reg({p,l,Name},Value)

<a name="add_shared_local_counter-2"></a>

### add_shared_local_counter/2 ###

`add_shared_local_counter(Name, Initial) -> any()`

Equivalent to [`reg_shared({c, l, Name}, Value)`](#reg_shared-2).

Registers a local shared (unique) counter.

<a name="audit_process-1"></a>

### audit_process/1 ###

<pre><code>
audit_process(Pid::pid()) -&gt; ok
</code></pre>
<br />

<a name="await-1"></a>

### await/1 ###

<pre><code>
await(Key::<a href="#type-key">key()</a>) -&gt; {pid(), Value}
</code></pre>
<br />

Equivalent to [`await(Key, infinity)`](#await-2).

<a name="await-2"></a>

### await/2 ###

<pre><code>
await(Key::<a href="#type-key">key()</a>, Timeout) -&gt; {pid(), Value}
</code></pre>

<ul class="definitions"><li><code>Timeout = integer() | infinity</code></li></ul>

Wait for a name or aggregated counter to be registered.
The function raises an exception if the timeout expires. Timeout must be
either an interger > 0 or 'infinity'.
A small optimization: we first perform a lookup, to see if the name
is already registered. This way, the cost of the operation will be
roughly the same as of where/1 in the case where the name is already
registered (the difference: await/2 also returns the value).

<a name="await-3"></a>

### await/3 ###

<pre><code>
await(Node::node(), Key::<a href="#type-key">key()</a>, Timeout) -&gt; {pid(), Value}
</code></pre>

<ul class="definitions"><li><code>Timeout = integer() | infinity</code></li></ul>

Wait for a name or aggregated counter to be registered on `Node`.
This function works exactly like [`await/2`](#await-2), but queries a remote
node instead. An exception is thrown if `Node` cannot be reached. If gproc
is not running on a given node, this is treated the same as the node being
down.

<a name="bcast-2"></a>

### bcast/2 ###

<pre><code>
bcast(Key::<a href="#type-key">key()</a>, Msg::any()) -&gt; Msg
</code></pre>
<br />

Equivalent to [`bcast(nodes(), Key, Msg)`](#bcast-3).

<a name="bcast-3"></a>

### bcast/3 ###

<pre><code>
bcast(Nodes::[atom()], Key::<a href="#type-key">key()</a>, Msg::any()) -&gt; Msg
</code></pre>
<br />

Sends a message to processes corresponding to Key on Nodes.

This function complements `send/2` and works on locally registered resources
that `send/2` supports. Messages are routed via a special broadcast server
on each node to ensure that ordering is preserved. Distributed delivery
is asynchronous and carries the same guarantees as normal message passing
(with the added proviso that the broadcast server also needs to be available).

__See also:__ [send/2](#send-2).

<a name="cancel_wait-2"></a>

### cancel_wait/2 ###

<pre><code>
cancel_wait(Key::<a href="#type-key">key()</a>, Ref) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Ref = all | reference()</code></li></ul>

Cancels a previous call to nb_wait/1

If `Ref = all`, all wait requests on `Key` from the calling process
are canceled.

<a name="cancel_wait-3"></a>

### cancel_wait/3 ###

<pre><code>
cancel_wait(Node::node(), Key::<a href="#type-key">key()</a>, Ref) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Ref = all | reference()</code></li></ul>

Cancels a previous call to nb_wait/2

This function works just like [`cancel_wait/2`](#cancel_wait-2), but talks to a remote
node.

<a name="cancel_wait_or_monitor-1"></a>

### cancel_wait_or_monitor/1 ###

`cancel_wait_or_monitor(Key) -> any()`

<a name="default-1"></a>

### default/1 ###

`default(X1) -> any()`

<a name="demonitor-2"></a>

### demonitor/2 ###

<pre><code>
demonitor(Key::<a href="#type-key">key()</a>, Ref::reference()) -&gt; ok
</code></pre>
<br />

Remove a monitor on a registered name
This function is the reverse of monitor/1. It removes a monitor previously
set on a unique name. This function always succeeds given legal input.

<a name="ensure_reg-1"></a>

### ensure_reg/1 ###

`ensure_reg(Key) -> any()`

Equivalent to [`ensure_reg(Key, default(Key), [])`](#ensure_reg-3).

<a name="ensure_reg-2"></a>

### ensure_reg/2 ###

<pre><code>
ensure_reg(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; new | updated
</code></pre>
<br />

Equivalent to [`ensure_reg(Key, Value, [])`](#ensure_reg-3).

<a name="ensure_reg-3"></a>

### ensure_reg/3 ###

<pre><code>
ensure_reg(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Attrs::<a href="#type-attrs">attrs()</a>) -&gt; new | updated
</code></pre>
<br />

Registers a new name or property unless such and entry (by key) has
already been registered by the current process. If `Key` already exists,
the entry will be updated with the given `Value` and `Attrs`.

This function allows the caller to efficiently register an entry without
first checking whether it has already been registered. An exception is
raised if the name or property is already registered by someone else.

<a name="ensure_reg_other-2"></a>

### ensure_reg_other/2 ###

`ensure_reg_other(Key, Pid) -> any()`

<a name="ensure_reg_other-3"></a>

### ensure_reg_other/3 ###

`ensure_reg_other(Key, Pid, Value) -> any()`

Equivalent to [`ensure_reg_other(Key, Pid, Value, [])`](#ensure_reg_other-4).

<a name="ensure_reg_other-4"></a>

### ensure_reg_other/4 ###

<pre><code>
ensure_reg_other(Key::<a href="#type-key">key()</a>, Pid::pid(), Value::<a href="#type-value">value()</a>, Attrs::<a href="#type-attrs">attrs()</a>) -&gt; new | updated
</code></pre>
<br />

Register or update name or property to another process.

Equivalent to [`reg_other/3`](#reg_other-3), but allows for registration of another
process instead of the current process. Also see [`ensure_reg/3`](#ensure_reg-3).

<a name="first-1"></a>

### first/1 ###

<pre><code>
first(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

Behaves as ets:first(Tab) for a given type of registration.

See [`http://www.erlang.org/doc/man/ets.html#first-1`](http://www.erlang.org/doc/man/ets.html#first-1).
The registry behaves as an ordered_set table.

<a name="get_attribute-2"></a>

### get_attribute/2 ###

<pre><code>
get_attribute(Key, Attribute::atom()) -&gt; Value
</code></pre>
<br />

Get attribute value of `Attr` associated with `Key` for most likely Pid.

The most likely Pid in this case is `self()` for properties and counters,
and the current registration holder in case of names or aggregated counters.
An exception is raised if `Key` is not registered for the given process.

<a name="get_attribute-3"></a>

### get_attribute/3 ###

<pre><code>
get_attribute(Key, Pid::pid() | shared, Attr::atom()) -&gt; Value
</code></pre>
<br />

Get the attribute value of `Attr` associated with `Key` for process Pid.

If `Pid == shared`, the attribute of a shared key (see [`reg_shared/1`](#reg_shared-1))
will be read.

<a name="get_attribute_shared-2"></a>

### get_attribute_shared/2 ###

<pre><code>
get_attribute_shared(Key, Attr::atom()) -&gt; Value
</code></pre>
<br />

Get the attribute value of `Attr` associated with the shared `Key`.

Equivalent to `get_attribute(Key, shared, Attr)`
(see [`get_attribute/3`](#get_attribute-3)).

<a name="get_attributes-1"></a>

### get_attributes/1 ###

<pre><code>
get_attributes(Key::<a href="#type-key">key()</a>) -&gt; [{K, V}]
</code></pre>
<br />

Equivalent to [`get_attributes(Key, self())`](#get_attributes-2).

Get attributes associated with registration.

<a name="get_attributes-2"></a>

### get_attributes/2 ###

<pre><code>
get_attributes(Key::<a href="#type-key">key()</a>, Pid::pid() | shared) -&gt; [{K, V}]
</code></pre>
<br />

Returns the list of attributes associated with the registration.

This function raises a `badarg` exception if there is no corresponding
registration.

<a name="get_env-3"></a>

### get_env/3 ###

<pre><code>
get_env(Scope::<a href="#type-scope">scope()</a>, App::atom(), Key::atom()) -&gt; term()
</code></pre>
<br />

Equivalent to [`get_env(Scope, App, Key, [app_env])`](#get_env-4).

<a name="get_env-4"></a>

### get_env/4 ###

<pre><code>
get_env(Scope::<a href="#type-scope">scope()</a>, App::atom(), Key::atom(), Strategy) -&gt; term()
</code></pre>

<ul class="definitions"><li><code>Strategy = [Alternative]</code></li><li><code>Alternative = app_env | os_env | inherit | {inherit, pid()} | {inherit, <a href="#type-unique_id">unique_id()</a>} | init_arg | {mnesia, ActivityType, Oid, Pos} | {default, term()} | error</code></li></ul>

Read an environment value, potentially cached as a `gproc_env` property.

This function first tries to read the value of a cached property,
`{p, Scope, {gproc_env, App, Key}}`. If this fails, it will try the provided
alternative strategy. `Strategy` is a list of alternatives, tried in order.
Each alternative can be one of:

* `app_env` - try `application:get_env(App, Key)`
* `os_env` - try `os:getenv(ENV)`, where `ENV` is `Key` converted into an
uppercase string
* `{os_env, ENV}` - try `os:getenv(ENV)`
* `inherit` - inherit the cached value, if any, held by the parent process.
* `{inherit, Pid}` - inherit the cached value, if any, held by `Pid`.
* `{inherit, Id}` - inherit the cached value, if any, held by the process
registered in `gproc` as `Id`.
* `init_arg` - try `init:get_argument(Key)`; expects a single value, if any.
* `{mnesia, ActivityType, Oid, Pos}` - try
`mnesia:activity(ActivityType, fun() -> mnesia:read(Oid) end)`; retrieve
the value in position `Pos` if object found.
* `{default, Value}` - set a default value to return once alternatives have
been exhausted; if not set, `undefined` will be returned.
* `error` - raise an exception, `erlang:error(gproc_env, [App, Key, Scope])`.

While any alternative can occur more than once, the only one that might make
sense to use multiple times is `{default, Value}`.

The return value will be one of:

* The value of the first matching alternative, or `error` eception,
whichever comes first
* The last instance of `{default, Value}`, or `undefined`, if there is no
matching alternative, default or `error` entry in the list.

The `error` option can be used to assert that a value has been previously
cached. Alternatively, it can be used to assert that a value is either cached
or at least defined somewhere,
e.g. `get_env(l, mnesia, dir, [app_env, error])`.

<a name="get_set_env-3"></a>

### get_set_env/3 ###

<pre><code>
get_set_env(Scope::<a href="#type-scope">scope()</a>, App::atom(), Key::atom()) -&gt; term()
</code></pre>
<br />

Equivalent to [`get_set_env(Scope, App, Key, [app_env])`](#get_set_env-4).

<a name="get_set_env-4"></a>

### get_set_env/4 ###

<pre><code>
get_set_env(Scope::<a href="#type-scope">scope()</a>, App::atom(), Key::atom(), Strategy) -&gt; Value
</code></pre>
<br />

Fetch and cache an environment value, if not already cached.

This function does the same thing as [`get_env/4`](#get_env-4), but also updates the
cache. Note that the cache will be updated even if the result of the lookup
is `undefined`.

__See also:__ [get_env/4](#get_env-4).

<a name="get_value-1"></a>

### get_value/1 ###

<pre><code>
get_value(Key) -&gt; Value
</code></pre>
<br />

Reads the value stored with a key registered to the current process.

If no such key is registered to the current process, this function exits.

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Key, Pid) -&gt; Value
</code></pre>
<br />

Reads the value stored with a key registered to the process Pid.

If `Pid == shared`, the value of a shared key (see [`reg_shared/1`](#reg_shared-1))
will be read.

<a name="get_value_shared-1"></a>

### get_value_shared/1 ###

<pre><code>
get_value_shared(Key) -&gt; Value
</code></pre>
<br />

Reads the value stored with a shared key.

If no such shared key is registered, this function exits.

<a name="give_away-2"></a>

### give_away/2 ###

<pre><code>
give_away(From::<a href="#type-key">key()</a>, To::pid() | <a href="#type-key">key()</a>) -&gt; undefined | pid()
</code></pre>
<br />

Atomically transfers the key `From` to the process identified by `To`.

This function transfers any gproc key (name, property, counter, aggr counter)
from one process to another, and returns the pid of the new owner.

`To` must be either a pid or a unique name (name or aggregated counter), but
does not necessarily have to resolve to an existing process. If there is
no process registered with the `To` key, `give_away/2` returns `undefined`,
and the `From` key is effectively unregistered.

It is allowed to give away a key to oneself, but of course, this operation
will have no effect.

Fails with `badarg` if the calling process does not have a `From` key
registered.

<a name="goodbye-0"></a>

### goodbye/0 ###

<pre><code>
goodbye() -&gt; ok
</code></pre>
<br />

Unregister all items of the calling process and inform gproc
to forget about the calling process.

This function is more efficient than letting gproc perform these
cleanup operations.

<a name="i-0"></a>

### i/0 ###

<pre><code>
i() -&gt; ok
</code></pre>
<br />

Similar to the built-in shell command `i()` but inserts information
about names and properties registered in Gproc, where applicable.

<a name="info-1"></a>

### info/1 ###

<pre><code>
info(Pid::pid()) -&gt; ProcessInfo
</code></pre>

<ul class="definitions"><li><code>ProcessInfo = [{gproc, [{Key, Value}]} | ProcessInfo]</code></li></ul>

Similar to `process_info(Pid)` but with additional gproc info.

Returns the same information as process_info(Pid), but with the
addition of a `gproc` information item, containing the `{Key,Value}`
pairs registered to the process.

<a name="info-2"></a>

### info/2 ###

<pre><code>
info(Pid::pid(), Item::atom()) -&gt; {Item, Info}
</code></pre>
<br />

Similar to process_info(Pid, Item), but with additional gproc info.

For `Item = gproc`, this function returns a list of `{Key, Value}` pairs
registered to the process Pid. For other values of Item, it returns the
same as [`http://www.erlang.org/doc/man/erlang.html#process_info-2`](http://www.erlang.org/doc/man/erlang.html#process_info-2).

<a name="last-1"></a>

### last/1 ###

<pre><code>
last(Context::<a href="#type-context">context()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

Behaves as ets:last(Tab) for a given type of registration.

See [`http://www.erlang.org/doc/man/ets.html#last-1`](http://www.erlang.org/doc/man/ets.html#last-1).
The registry behaves as an ordered_set table.

<a name="lookup_global_aggr_counter-1"></a>

### lookup_global_aggr_counter/1 ###

<pre><code>
lookup_global_aggr_counter(Name::any()) -&gt; integer()
</code></pre>
<br />

Equivalent to [`lookup_value({a, g, Name})`](#lookup_value-1).

Lookup a global (unique) aggregated counter and returns its value.
Fails if there is no such object.

<a name="lookup_global_counters-1"></a>

### lookup_global_counters/1 ###

<pre><code>
lookup_global_counters(Counter::any()) -&gt; [{pid(), Value::integer()}]
</code></pre>
<br />

Equivalent to [`lookup_values({c, g, Counter})`](#lookup_values-1).

Look up all global (non-unique) instances of a given Counter.
Returns a list of {Pid, Value} tuples for all matching objects.

<a name="lookup_global_name-1"></a>

### lookup_global_name/1 ###

<pre><code>
lookup_global_name(Name::any()) -&gt; pid()
</code></pre>
<br />

Equivalent to [`where({n, g, Name})`](#where-1).

Lookup a global unique name. Fails if there is no such name.

<a name="lookup_global_properties-1"></a>

### lookup_global_properties/1 ###

<pre><code>
lookup_global_properties(Property::any()) -&gt; [{pid(), Value}]
</code></pre>
<br />

Equivalent to [`lookup_values({p, g, Property})`](#lookup_values-1).

Look up all global (non-unique) instances of a given Property.
Returns a list of {Pid, Value} tuples for all matching objects.

<a name="lookup_local_aggr_counter-1"></a>

### lookup_local_aggr_counter/1 ###

<pre><code>
lookup_local_aggr_counter(Name::any()) -&gt; integer()
</code></pre>
<br />

Equivalent to [`where({a, l, Name})`](#where-1).

Lookup a local (unique) aggregated counter and returns its value.
Fails if there is no such object.

<a name="lookup_local_counters-1"></a>

### lookup_local_counters/1 ###

<pre><code>
lookup_local_counters(Counter::any()) -&gt; [{pid(), Value::integer()}]
</code></pre>
<br />

Equivalent to [`lookup_values({c, l, Counter})`](#lookup_values-1).

Look up all local (non-unique) instances of a given Counter.
Returns a list of {Pid, Value} tuples for all matching objects.

<a name="lookup_local_name-1"></a>

### lookup_local_name/1 ###

<pre><code>
lookup_local_name(Name::any()) -&gt; pid()
</code></pre>
<br />

Equivalent to [`where({n, l, Name})`](#where-1).

Lookup a local unique name. Fails if there is no such name.

<a name="lookup_local_properties-1"></a>

### lookup_local_properties/1 ###

<pre><code>
lookup_local_properties(Property::any()) -&gt; [{pid(), Value}]
</code></pre>
<br />

Equivalent to [`lookup_values({p, l, Property})`](#lookup_values-1).

Look up all local (non-unique) instances of a given Property.
Returns a list of {Pid, Value} tuples for all matching objects.

<a name="lookup_pid-1"></a>

### lookup_pid/1 ###

<pre><code>
lookup_pid(Key) -&gt; Pid
</code></pre>
<br />

Lookup the Pid stored with a key.

This function raises a `badarg` exception if `Key` is not registered.

<a name="lookup_pids-1"></a>

### lookup_pids/1 ###

<pre><code>
lookup_pids(Key::<a href="#type-key">key()</a>) -&gt; [pid()]
</code></pre>
<br />

Returns a list of pids with the published key Key

If the type of registration is either name or aggregated counter,
this function will return either an empty list, or a list of one pid.
For non-unique types, the return value can be a list of any length.

Note: shared resources are not associated with any pid, and will
therefore be excluded.

<a name="lookup_value-1"></a>

### lookup_value/1 ###

<pre><code>
lookup_value(Key) -&gt; Value
</code></pre>
<br />

Lookup the value stored with a key.

This function raises a `badarg` exception if `Key` is not registered.

<a name="lookup_values-1"></a>

### lookup_values/1 ###

<pre><code>
lookup_values(Key::<a href="#type-key">key()</a>) -&gt; [{pid(), Value}]
</code></pre>
<br />

Retrieve the `{Pid,Value}` pairs corresponding to Key.

Key refer to any type of registry object. If it refers to a unique
object, the list will be of length 0 or 1. If it refers to a non-unique
object, the return value can be a list of any length.

<a name="monitor-1"></a>

### monitor/1 ###

`monitor(Key) -> any()`

Equivalent to [`monitor(Key, info)`](#monitor-2).

<a name="monitor-2"></a>

### monitor/2 ###

<pre><code>
monitor(Key::<a href="#type-key">key()</a>, Type::<a href="#type-monitor_type">monitor_type()</a>) -&gt; reference()
</code></pre>
<br />

monitor a registered name
`monitor(Key, info)` works much like erlang:monitor(process, Pid), but monitors
a unique name registered via gproc. A message, `{gproc, unreg, Ref, Key}`
will be sent to the requesting process, if the name is unregistered or
the registered process dies. If there is a standby monitor (see below), a
message `{gproc, {failover, ToPid}, Ref, Key}` is sent to all monitors.
If the name is passed to another process using [`give_away/2`](#give_away-2), the event
`{gproc, {migrated, ToPid}, Ref, Key}` is sent to all monitors.

`monitor(Key, standby)` sets up the monitoring process as a standby for the
registered name. If the registered process dies, the first standby process
inherits the name, and a message `{gproc, {failover, ToPid}, Ref, Key}` is
sent to all monitors, including the one that inherited the name.

If the name is not yet registered, the unreg event is sent immediately.
If the calling process in this case tried to start a `standby` monitoring,
it receives the registered name and the failover event immediately.

`monitor(Key, follow)` keeps monitoring the registered name even if it is
temporarily unregistered. The messages received are the same as for the other
monitor types, but `{gproc, registered, Ref, Key}` is also sent when a new
process registers the name.

<a name="mreg-3"></a>

### mreg/3 ###

<pre><code>
mreg(T::<a href="#type-type">type()</a>, C::<a href="#type-scope">scope()</a>, KVL::[{Key::any(), Value::any()}]) -&gt; true
</code></pre>
<br />

Register multiple {Key,Value} pairs of a given type and scope.

This function is more efficient than calling [`reg/2`](#reg-2) repeatedly.
It is also atomic in regard to unique names; either all names are registered
or none are.

<a name="munreg-3"></a>

### munreg/3 ###

<pre><code>
munreg(T::<a href="#type-type">type()</a>, C::<a href="#type-scope">scope()</a>, L::[Key::any()]) -&gt; true
</code></pre>
<br />

Unregister multiple Key items of a given type and scope.

This function is usually more efficient than calling [`unreg/1`](#unreg-1)
repeatedly.

<a name="nb_wait-1"></a>

### nb_wait/1 ###

<pre><code>
nb_wait(Key::<a href="#type-key">key()</a>) -&gt; Ref
</code></pre>
<br />

Wait for a name or aggregated counter to be registered.
The caller can expect to receive a message,
{gproc, Ref, registered, {Key, Pid, Value}}, once the name is registered.

<a name="nb_wait-2"></a>

### nb_wait/2 ###

<pre><code>
nb_wait(Node::node(), Key::<a href="#type-key">key()</a>) -&gt; Ref
</code></pre>
<br />

Wait for a name or aggregated counter to be registered on `Node`.
The caller can expect to receive a message,
{gproc, Ref, registered, {Key, Pid, Value}}, once the name is registered.

<a name="next-2"></a>

### next/2 ###

<pre><code>
next(Context::<a href="#type-context">context()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

Behaves as ets:next(Tab,Key) for a given type of registration.

See [`http://www.erlang.org/doc/man/ets.html#next-2`](http://www.erlang.org/doc/man/ets.html#next-2).
The registry behaves as an ordered_set table.

<a name="prev-2"></a>

### prev/2 ###

<pre><code>
prev(Context::<a href="#type-context">context()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

Behaves as ets:prev(Tab,Key) for a given type of registration.

See [`http://www.erlang.org/doc/man/ets.html#prev-2`](http://www.erlang.org/doc/man/ets.html#prev-2).
The registry behaves as an ordered_set table.

<a name="reg-1"></a>

### reg/1 ###

<pre><code>
reg(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Equivalent to [`reg(Key, default(Key), [])`](#reg-3).

<a name="reg-2"></a>

### reg/2 ###

<pre><code>
reg(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; true
</code></pre>
<br />

Register a name or property for the current process

<a name="reg-3"></a>

### reg/3 ###

<pre><code>
reg(Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>, Attrs::<a href="#type-attrs">attrs()</a>) -&gt; true
</code></pre>
<br />

Register a name or property for the current process
`Attrs` (default: `[]`) can be inspected using [`get_attribute/2`](#get_attribute-2).

The structure of `Key` is `{Type, Context, Name}`, where:

* `Context :: l | g` - `l` means 'local' context; `g` means 'global'
* `Type :: p | n | c | a | r | rc` specifies the type of entry

The semantics of the different types:

* `p` - 'property', is non-unique, i.e. different processes can each
register a property with the same name.
* `n` - 'name, is unique within the given context (local or global).
* `c` - 'counter', is similar to a property, but has a numeric value
and behaves roughly as an ets counter (see [`update_counter/2`](#update_counter-2).)
* `a` - 'aggregated counter', is automatically updated by gproc, and
reflects the sum of all counter objects with the same name in the given
scope. The initial value for an aggregated counter must be `undefined`.
* `r` - 'resource property', behaves like a property, but can be tracked
with a 'resource counter'.
* `rc` - 'resource counter', tracks the number of resource properties
with the same name. When the resource count reaches `0`, any triggers
specified using an `on_zero` attribute may be executed (see below).

On-zero triggers:

`Msg = {gproc, resource_on_zero, Context, Name, Pid}`

* `{send, Key}` - run `gproc:send(Key, Msg)`
* `{bcast, Key}` - run `gproc:bcast(Key, Msg)`
* `publish` - run
`gproc_ps:publish(Context, gproc_resource_on_zero, {Context, Name, Pid})`
* `{unreg_shared, Type, Name}` - unregister the shared key
`{Type, Context, Name}`

<a name="reg_or_locate-1"></a>

### reg_or_locate/1 ###

<pre><code>
reg_or_locate(Key::<a href="#type-key">key()</a>) -&gt; {pid(), NewValue}
</code></pre>
<br />

Equivalent to [`reg_or_locate(Key, default(Key))`](#reg_or_locate-2).

<a name="reg_or_locate-2"></a>

### reg_or_locate/2 ###

<pre><code>
reg_or_locate(Key::<a href="#type-key">key()</a>, Value) -&gt; {pid(), NewValue}
</code></pre>
<br />

Try registering a unique name, or return existing registration.

This function tries to register the name `Key`, if available.
If such a registration object already exists, the pid and value of
the current registration is returned instead.

<a name="reg_or_locate-3"></a>

### reg_or_locate/3 ###

<pre><code>
reg_or_locate(Key::<a href="#type-key">key()</a>, Value, Fun::function()) -&gt; {pid(), NewValue}
</code></pre>
<br />

Spawn a process with a registered name, or return existing registration.

This function checks whether a local name is registered; if not, it spawns
a new process (with `spawn(Fun)`) and gives it the name.
The pid and value of the resulting registration is returned.

When a global name is registered in this fashion, the process is
spawned on the caller's node, and the group_leader of the spawned
process is set to the group_leader of the calling process.

<a name="reg_other-2"></a>

### reg_other/2 ###

`reg_other(Key, Pid) -> any()`

Equivalent to [`reg_other(Key, Pid, default(Key), [])`](#reg_other-4).

<a name="reg_other-3"></a>

### reg_other/3 ###

`reg_other(Key, Pid, Value) -> any()`

Equivalent to [`reg_other(Key, Pid, Value, [])`](#reg_other-4).

<a name="reg_other-4"></a>

### reg_other/4 ###

<pre><code>
reg_other(Key, Pid, Value, Attrs) -&gt; true
</code></pre>
<br />

Register name or property to another process.

Equivalent to [`reg/3`](#reg-3), but allows for registration of another process
instead of the current process.

Note that registering other processes introduces the possibility of
confusing race conditions in user code. Letting each process register
its own resources is highly recommended.

Only the following resource types can be registered through this function:

* `n`  - unique names
* `a`  - aggregated counters
* `r`  - resource properties
* `rc` - resource counters

<a name="reg_shared-1"></a>

### reg_shared/1 ###

<pre><code>
reg_shared(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Register a resource, but don't tie it to a particular process.

`reg_shared({c,l,C}) -> reg_shared({c,l,C}, 0).`
`reg_shared({a,l,A}) -> reg_shared({a,l,A}, undefined).`

<a name="reg_shared-2"></a>

### reg_shared/2 ###

<pre><code>
reg_shared(Key::<a href="#type-key">key()</a>, Value) -&gt; true
</code></pre>
<br />

Register a resource, but don't tie it to a particular process.

Shared resources are all unique. They remain until explicitly unregistered
(using [`unreg_shared/1`](#unreg_shared-1)). The types of shared resources currently
supported are `counter` and `aggregated counter`. In listings and query
results, shared resources appear as other similar resources, except that
`Pid == shared`. To wit, update_counter({c,l,myCounter}, shared, 1) would
increment the shared counter `myCounter` with 1, provided it exists.

A shared aggregated counter will track updates in exactly the same way as
an aggregated counter which is owned by a process.

<a name="reg_shared-3"></a>

### reg_shared/3 ###

`reg_shared(Key, Value, Attrs) -> any()`

<a name="register_name-2"></a>

### register_name/2 ###

<pre><code>
register_name(Name::<a href="#type-key">key()</a>, Pid::pid()) -&gt; yes | no
</code></pre>
<br />

Behaviour support callback

<a name="reset_counter-1"></a>

### reset_counter/1 ###

<pre><code>
reset_counter(Key) -&gt; {ValueBefore, ValueAfter}
</code></pre>

<ul class="definitions"><li><code>Key = {c, Scope, Name} | {n, Scope, Name}</code></li><li><code>Scope = l | g</code></li><li><code>ValueBefore = integer()</code></li><li><code>ValueAfter = integer()</code></li></ul>

Reads and resets a counter in a "thread-safe" way

This function reads the current value of a counter and then resets it to its
initial value. The reset operation is done using [`update_counter/2`](#update_counter-2),
which allows for concurrent calls to [`update_counter/2`](#update_counter-2) without losing
updates. Aggregated counters are updated accordingly.

If `Key` refers to a unique name, the operation will depend on the value
part of the registration being an integer(). While non-integer values are
not permitted at all for counter objects, it is the user's responsibility to
ensure that a name, on which `reset_counter/1` is to be performed, has the
appropriate value type.

<a name="select-1"></a>

### select/1 ###

<pre><code>
select(Continuation::Arg) -&gt; [Match] | {[Match], Continuation} | '$end_of_table'
</code></pre>

<ul class="definitions"><li><code>Arg = Continuation | <a href="#type-sel_pattern">sel_pattern()</a></code></li><li><code>Match = {Key, Pid, Value}</code></li></ul>

Perform a select operation on the process registry

When Arg = Contination, resume a gproc:select/1 operation
(see [ets:select/1](http://www.erlang.org/doc/man/ets.html#select-1)

When Arg = <code><a href="#type-sel_pattern">sel_pattern()</a></code>, this function executes a select operation,
emulating ets:select/1

[`select/2`](#select-2) offers the opportunity to narrow the search
(by limiting to only global or local scope, or a single type of object).
When only a pattern as single argument is given, both global and local scope,
as well as all types of object can be searched. Note that the pattern may
still limit the select operation so that scanning the entire table is avoided.

The physical representation in the registry may differ from the above,
but the select patterns are transformed appropriately. The logical
representation for the gproc select operations is given by
<code><a href="#type-headpat">headpat()</a></code>.

<a name="select-2"></a>

### select/2 ###

<pre><code>
select(Context::<a href="#type-sel_context">sel_context()</a>, Pat::<a href="#type-sel_pattern">sel_pattern()</a>) -&gt; [{Key, Pid, Value}]
</code></pre>
<br />

Perform a select operation with limited context on the process registry

The physical representation in the registry may differ from the above,
but the select patterns are transformed appropriately.

Note that limiting the context is just a convenience function, allowing you
to write a simpler select pattern and still avoid searching the entire
registry. Whenever variables are used in the head pattern, this will result
in a wider scan, even if the values are restricted through a guard (e.g.
`select([{'$1','$2','$3'}, [{'==', {element,1,'$1'}, p}], ...])`
will count as a wild pattern on the key and result in a full scan).
In this case, specifying a Context will allow gproc to perform some
variable substitution and ensure that the scan is limited.

<a name="select-3"></a>

### select/3 ###

<pre><code>
select(Context::<a href="#type-context">context()</a>, Pat::<a href="#type-sel_patten">sel_patten()</a>, Limit::integer()) -&gt; {[Match], Continuation} | '$end_of_table'
</code></pre>
<br />

Like [`select/2`](#select-2) but returns Limit objects at a time.

See [`http://www.erlang.org/doc/man/ets.html#select-3`](http://www.erlang.org/doc/man/ets.html#select-3).

<a name="select_count-1"></a>

### select_count/1 ###

<pre><code>
select_count(Pat::<a href="#type-sel_pattern">sel_pattern()</a>) -&gt; [<a href="#type-sel_object">sel_object()</a>]
</code></pre>
<br />

Equivalent to [`select_count(all, Pat)`](#select_count-2).

<a name="select_count-2"></a>

### select_count/2 ###

<pre><code>
select_count(Context::<a href="#type-context">context()</a>, Pat::<a href="#type-sel_pattern">sel_pattern()</a>) -&gt; [{Key, Pid, Value}]
</code></pre>
<br />

Perform a select_count operation on the process registry.

The physical representation in the registry may differ from the above,
but the select patterns are transformed appropriately.

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Key::<a href="#type-process">process()</a> | <a href="#type-key">key()</a>, Msg::any()) -&gt; Msg
</code></pre>
<br />

Sends a message to the process, or processes, corresponding to Key.

If Key belongs to a unique object (name or aggregated counter), this
function will send a message to the corresponding process, or fail if there
is no such process. If Key is for a non-unique object type (counter or
property), Msg will be send to all processes that have such an object.

Key can also be anything that the erlang:send/2, or '!' operator accepts as a process
identifier, namely a pid(), an atom(), or `{Name::atom(), Node::atom()}`.

<a name="set_attributes-2"></a>

### set_attributes/2 ###

<pre><code>
set_attributes(Key::<a href="#type-key">key()</a>, Props::[{atom(), any()}]) -&gt; true
</code></pre>
<br />

Add/modify `{Key, Value}` attributes associated with a registration.

Gproc registration objects can have `{Key, Value}` attributes associated with
them. These are stored in a way that doesn't affect the cost of name lookup.

Attributs can be retrieved using `gproc:get_attribute/3` or
`gproc:get_attributes/2`.

<a name="set_attributes_shared-2"></a>

### set_attributes_shared/2 ###

<pre><code>
set_attributes_shared(Key::<a href="#type-key">key()</a>, Props::[{K, V}]) -&gt; true
</code></pre>
<br />

Add/modify `{Key, Value}` attributes associated with a shared registration.

Gproc registration objects can have `{Key, Value}` attributes associated with
them. These are stored in a way that doesn't affect the cost of name lookup.

Attributes can be retrieved using `gproc:get_attribute/3` or
`gproc:get_attributes/2`.

<a name="set_env-5"></a>

### set_env/5 ###

<pre><code>
set_env(Scope::<a href="#type-scope">scope()</a>, App::atom(), Key::atom(), Value::term(), Strategy) -&gt; Value
</code></pre>

<ul class="definitions"><li><code>Strategy = [Alternative]</code></li><li><code>Alternative = app_env | os_env | {os_env, VAR} | {mnesia, ActivityType, Oid, Pos}</code></li></ul>

Updates the cached value as well as underlying environment.

This function should be exercised with caution, as it affects the larger
environment outside gproc. This function modifies the cached value, and then
proceeds to update the underlying environment (OS environment variable or
application environment variable).

When the `mnesia` alternative is used, gproc will try to update any existing
object, changing only the `Pos` position. If no such object exists, it will
create a new object, setting any other attributes (except `Pos` and the key)
to `undefined`.

<a name="set_value-2"></a>

### set_value/2 ###

<pre><code>
set_value(Key::<a href="#type-key">key()</a>, Value) -&gt; true
</code></pre>
<br />

Sets the value of the registration given by Key

Key is assumed to exist and belong to the calling process.
If it doesn't, this function will exit.

Value can be any term, unless the object is a counter, in which case
it must be an integer.

<a name="set_value_shared-2"></a>

### set_value_shared/2 ###

<pre><code>
set_value_shared(Key::<a href="#type-key">key()</a>, Value) -&gt; true
</code></pre>
<br />

Sets the value of the shared registration given by Key

Key is assumed to exist as a shared entity.
If it doesn't, this function will exit.

Value can be any term, unless the object is a counter, in which case
it must be an integer.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

Starts the gproc server.

This function is intended to be called from gproc_sup, as part of
starting the gproc application.

<a name="table-0"></a>

### table/0 ###

<pre><code>
table() -&gt; any()
</code></pre>
<br />

Equivalent to [`table({all, all})`](#table-1).

<a name="table-1"></a>

### table/1 ###

<pre><code>
table(Context::<a href="#type-context">context()</a>) -&gt; any()
</code></pre>
<br />

Equivalent to [`table(Context, [])`](#table-2).

<a name="table-2"></a>

### table/2 ###

<pre><code>
table(Context::<a href="#type-context">context()</a>, Opts) -&gt; any()
</code></pre>
<br />

QLC table generator for the gproc registry.
Context specifies which subset of the registry should be queried.
See [`http://www.erlang.org/doc/man/qlc.html`](http://www.erlang.org/doc/man/qlc.html).

NOTE: By default, the gproc table generator will not filter out entries
belonging to processes that have just died, but which have yet to be cleared
out of the registry. Use the option `check_pids` (or `{check_pids, true}`)
if you want to filter out dead entries already in the query. There will be
some overhead associated with doing so, and given that the process monitoring
is asynchronous, there can never be any guarantee that there are no dead
entries in the list by the time your program processes it.

<a name="unreg-1"></a>

### unreg/1 ###

<pre><code>
unreg(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Unregister a name or property.

<a name="unreg_other-2"></a>

### unreg_other/2 ###

<pre><code>
unreg_other(Key::<a href="#type-key">key()</a>, Pid::pid()) -&gt; true
</code></pre>
<br />

Unregister a name registered to another process.

This function is equivalent to [`unreg/1`](#unreg-1), but specifies another
process as the holder of the registration. An exception is raised if the
name or property is not registered to the given process.

<a name="unreg_shared-1"></a>

### unreg_shared/1 ###

<pre><code>
unreg_shared(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Unregister a shared resource.

<a name="unregister_name-1"></a>

### unregister_name/1 ###

`unregister_name(Key) -> any()`

Equivalent to `unreg / 1`.

<a name="update_counter-2"></a>

### update_counter/2 ###

<pre><code>
update_counter(Key::<a href="#type-key">key()</a>, Incr::<a href="#type-increment">increment()</a>) -&gt; integer()
</code></pre>
<br />

Updates the counter registered as Key for the current process.

This function works almost exactly like ets:update_counter/3
(see [`http://www.erlang.org/doc/man/ets.html#update_counter-3`](http://www.erlang.org/doc/man/ets.html#update_counter-3)), but
will fail if the type of object referred to by Key is not a counter or
a unique name (update_counter/2 can be performed on names as well, but they
do not count as counter objects, and do not affect aggregated counters).

Aggregated counters with the same name will be updated automatically.
The `UpdateOp` patterns are the same as for `ets:update_counter/3`, except
that the position is omitted; in gproc, the value position is always `3`.

If `Key` refers to a unique name, the operation will depend on the value
part of the registration being an integer(). While non-integer values are
not permitted at all for counter objects, it is the user's responsibility to
ensure that a name, on which `update_counter/2` is to be performed, has the
appropriate value type.

<a name="update_counter-3"></a>

### update_counter/3 ###

`update_counter(Key, Pid, Incr) -> any()`

<a name="update_counters-2"></a>

### update_counters/2 ###

<pre><code>
update_counters(X1::<a href="#type-scope">scope()</a>, Cs::[{<a href="#type-key">key()</a>, pid(), <a href="#type-increment">increment()</a>}]) -&gt; [{<a href="#type-key">key()</a>, pid(), integer()}]
</code></pre>
<br />

Update a list of counters

This function is not atomic, except (in a sense) for global counters. For local counters,
it is more of a convenience function. For global counters, it is much more efficient
than calling `gproc:update_counter/2` for each individual counter.

The return value is the corresponding list of `[{Counter, Pid, NewValue}]`.

<a name="update_shared_counter-2"></a>

### update_shared_counter/2 ###

<pre><code>
update_shared_counter(Key::<a href="#type-key">key()</a>, Incr) -&gt; integer() | [integer()]
</code></pre>

<ul class="definitions"><li><code>Incr = IncrVal | UpdateOp | [UpdateOp]</code></li><li><code>UpdateOp = IncrVal | {IncrVal, Threshold, SetValue}</code></li><li><code>IncrVal = integer()</code></li></ul>

Updates the shared counter registered as Key.

This function works almost exactly like ets:update_counter/3
(see [`http://www.erlang.org/doc/man/ets.html#update_counter-3`](http://www.erlang.org/doc/man/ets.html#update_counter-3)), but
will fail if the type of object referred to by Key is not a counter.

Aggregated counters with the same name will be updated automatically.
The `UpdateOp` patterns are the same as for `ets:update_counter/3`, except
that the position is omitted; in gproc, the value position is always `3`.

<a name="where-1"></a>

### where/1 ###

<pre><code>
where(Key::<a href="#type-key">key()</a>) -&gt; pid() | undefined
</code></pre>
<br />

Returns the pid registered as Key

The type of registration must be either name or aggregated counter.
Otherwise this function will raise a `badarg` exception.
Use [`lookup_pids/1`](#lookup_pids-1) in these cases.

<a name="whereis_name-1"></a>

### whereis_name/1 ###

`whereis_name(Key) -> any()`

Equivalent to `where / 1`.

<a name="wide_await-3"></a>

### wide_await/3 ###

<pre><code>
wide_await(Nodes::[node()], Key::<a href="#type-key">key()</a>, Timeout) -&gt; {pid(), Value}
</code></pre>

<ul class="definitions"><li><code>Timeout = integer() | infinity</code></li></ul>

Wait for a local name to be registered on any of `Nodes`.
This function works rather like [`await/2`](#await-2), but queries all nodes in
the `Nodes` list at the same time. The first node to respond with a
process registered as `Key` will provide the result. Other results are
ignored. `Key` must be a unique name with local scope, i.e. `{n,l,Name}`.

An exception is thrown upon timeout, or if no node can be reached (if gproc is
not running on a given node, this is treated the same as the node being down).

