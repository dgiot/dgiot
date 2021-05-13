

# Module gproc_pool #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Load balancing functions based on Gproc.

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="description"></a>

## Description ##

This module implements support for load-balancing server pools. It was
originally intended mainly as an example of how to use various Gproc
resources (e.g. counters and shared properties), but is fully integrated
into Gproc, and fully functional.


## Concepts ##

Each pool has a list of 'named' workers (defined using `add_worker/2`) and
a load-balancing strategy. Processes can then 'connect' to the pool (with
`connect_worker/2`), using one of the defined names.

Users then 'pick' one of the currently connected processes in the pool. Which
process is picked depends on the load-balancing strategy.

The whole representation of the pool and its connected workers is in gproc.
The server `gproc_pool` is used to serialize pool management updates, but
worker selection is performed entirely in the calling process, and can be
performed by several processes concurrently.


### Load-balancing strategies ###

* `round_robin` is the default. A wrapping gproc counter keeps track of the
latest worker picked, and `gproc:next()` is used to find the next worker.
* `random` picks a random worker from the pool.
* `hash` requires a value (`pick/2`), and picks a worker based on the hash of
that value.
* `direct` takes an integer as an argument, and picks the next worker (modulo
the size of the pool). This is mainly for implementations that implement
a load-balancing strategy on top of `gproc_pool`.
* `claim` picks the first available worker and 'claims' it while executing
a user-provided fun. This means that the number of concurrently executing
jobs will not exceed the size of the pool.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#active_workers-1">active_workers/1</a></td><td>Return a list of currently connected workers in the pool.</td></tr><tr><td valign="top"><a href="#add_worker-2">add_worker/2</a></td><td>Assign a worker name to the pool, returning the worker's position.</td></tr><tr><td valign="top"><a href="#add_worker-3">add_worker/3</a></td><td>Assign a worker name to a given slot in the pool, returning the slot.</td></tr><tr><td valign="top"><a href="#claim-2">claim/2</a></td><td>Equivalent to <a href="#claim-3"><tt>claim(Pool, F, nowait)</tt></a>.</td></tr><tr><td valign="top"><a href="#claim-3">claim/3</a></td><td>Picks the first available worker in the pool and applies <code>Fun</code>.</td></tr><tr><td valign="top"><a href="#connect_worker-2">connect_worker/2</a></td><td>Connect the current process to <code>Name</code> in <code>Pool</code>.</td></tr><tr><td valign="top"><a href="#defined_workers-1">defined_workers/1</a></td><td>Return a list of added workers in the pool.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete an existing pool.</td></tr><tr><td valign="top"><a href="#disconnect_worker-2">disconnect_worker/2</a></td><td>Disconnect the current process from <code>Name</code> in <code>Pool</code>.</td></tr><tr><td valign="top"><a href="#force_delete-1">force_delete/1</a></td><td>Forcibly remove a pool, terminating all active workers.</td></tr><tr><td valign="top"><a href="#log-1">log/1</a></td><td>Update a counter associated with a worker name.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-3"><tt>new(Pool, round_robin, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Create a new pool.</td></tr><tr><td valign="top"><a href="#pick-1">pick/1</a></td><td>Pick a worker from the pool given the pool's load-balancing algorithm.</td></tr><tr><td valign="top"><a href="#pick-2">pick/2</a></td><td>Pick a worker from the pool based on <code>Value</code>.</td></tr><tr><td valign="top"><a href="#pick_worker-1">pick_worker/1</a></td><td>Pick a worker pid from the pool given the pool's load-balancing algorithm.</td></tr><tr><td valign="top"><a href="#pick_worker-2">pick_worker/2</a></td><td>Pick a worker pid from the pool given the pool's load-balancing algorithm.</td></tr><tr><td valign="top"><a href="#ptest-4">ptest/4</a></td><td></td></tr><tr><td valign="top"><a href="#randomize-1">randomize/1</a></td><td>Randomizes the "next" pointer for the pool.</td></tr><tr><td valign="top"><a href="#remove_worker-2">remove_worker/2</a></td><td>Remove a previously added worker.</td></tr><tr><td valign="top"><a href="#setup_test_pool-4">setup_test_pool/4</a></td><td></td></tr><tr><td valign="top"><a href="#test_run0-2">test_run0/2</a></td><td></td></tr><tr><td valign="top"><a href="#whereis_worker-2">whereis_worker/2</a></td><td>Look up the pid of a connected worker.</td></tr><tr><td valign="top"><a href="#worker_id-2">worker_id/2</a></td><td>Return the unique gproc name corresponding to a name in the pool.</td></tr><tr><td valign="top"><a href="#worker_pool-1">worker_pool/1</a></td><td>Return a list of slots and/or named workers in the pool.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="active_workers-1"></a>

### active_workers/1 ###

<pre><code>
active_workers(Pool::any()) -&gt; [{Name, Pid}]
</code></pre>
<br />

Return a list of currently connected workers in the pool.

<a name="add_worker-2"></a>

### add_worker/2 ###

<pre><code>
add_worker(Pool::any(), Name::any()) -&gt; integer()
</code></pre>
<br />

Assign a worker name to the pool, returning the worker's position.

Before a worker can connect to the pool, its name must be added. If no explicit
position is given (see [`add_worker/3`](#add_worker-3)), the most suitable position,
depending on load-balancing algorithm, is selected: for round_robin and direct
pools, names are packed tightly from the beginning; for hash and random pools,
slots are filled as sparsely as possible, in order to maintain an even
likelihood of hitting each worker.

An exception is raised if the pool is full (and `auto_size` is false), or if
`Name` already exists in the pool.

Before a worker can be used, a process must connect to it (see
[`connect_worker/2`](#connect_worker-2).

<a name="add_worker-3"></a>

### add_worker/3 ###

<pre><code>
add_worker(Pool::any(), Name::any(), Slot::integer()) -&gt; integer()
</code></pre>
<br />

Assign a worker name to a given slot in the pool, returning the slot.

This function allows the pool maintainer to exactly position each worker
inside the pool. An exception is raised if the position is already taken,
or if `Name` already exists in the pool. If `Slot` is larger than the current
size of the pool, an exception is raised iff `auto_size` is `false`;
otherwise the pool is expanded to accomodate the new position.

<a name="claim-2"></a>

### claim/2 ###

`claim(Pool, F) -> any()`

Equivalent to [`claim(Pool, F, nowait)`](#claim-3).

<a name="claim-3"></a>

### claim/3 ###

<pre><code>
claim(Pool, F::Fun, Wait) -&gt; {true, Res} | false
</code></pre>

<ul class="definitions"><li><code>Pool = any()</code></li><li><code>Fun = function()</code></li><li><code>Wait = nowait | {busy_wait, integer()}</code></li></ul>

Picks the first available worker in the pool and applies `Fun`.

A `claim` pool allows the caller to "claim" a worker during a short span
(essentially, a lock is set and released as soon as `Fun` returns).
Once a worker is selected, `Fun(Name, Pid)` is called, where `Name` is a
unique gproc name of the worker, and `Pid` is its process identifier.
The gproc name of the worker serves as a mutex, where its value is 0 (zero)
if the worker is free, and 1 (one) if it is busy. The mutex operation is
implemented using `gproc:update_counter/2`.

`Wait == nowait` means that the call will return `false` immediately if
there is no available worker.

`Wait == {busy_wait, Timeout}` will keep repeating the claim attempt
for `Timeout` milliseconds. If still no worker is available, it will
return `false`.

<a name="connect_worker-2"></a>

### connect_worker/2 ###

<pre><code>
connect_worker(Pool::any(), Name::any()) -&gt; true
</code></pre>
<br />

Connect the current process to `Name` in `Pool`.

Typically, a server will call this function as it starts, similarly to when
it registers itself. In fact, calling `connect_worker/2` leads to the process
being registered as `{n,l,[gproc_pool,N,Name]}`, where `N` is the position of
`Name` in the pool. This means (a) that gproc monitors the worker, and
removes the connection automatically if it dies, and (b) that the registered
names can be listed in order of their positions in the pool.

This function raises an exception if `Name` does not exist in `Pool` (or
there is no such pool), or if another worker is already connected to
`Name`.

<a name="defined_workers-1"></a>

### defined_workers/1 ###

<pre><code>
defined_workers(Pool::any()) -&gt; [{Name, Pos, Count}]
</code></pre>
<br />

Return a list of added workers in the pool.

The added workers are slots in the pool that have been given names, and thus
can be connected to. This function doesn't detect whether or not there are
any connected (active) workers.

The list contains `{Name, Pos, Count}`, where `Name` is the name of the added
worker, `Pos` is its position in the pool, and `Count` represents the number
of times the worker has been picked (assuming callers keep count by explicitly
calling [`log/1`](#log-1)).

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Pool::any()) -&gt; true
</code></pre>
<br />

Delete an existing pool.

This function will delete a pool, only if there are no connected workers.
Ensure that workers have been disconnected before deleting the pool.

<a name="disconnect_worker-2"></a>

### disconnect_worker/2 ###

<pre><code>
disconnect_worker(Pool, Name) -&gt; true
</code></pre>
<br />

Disconnect the current process from `Name` in `Pool`.

This function is similar to a `gproc:unreg()` call. It removes the
connection between `Pool`, `Name` and pid, and makes it possible for another
process to connect to `Name`.

An exception is raised if there is no prior connection between `Pool`,
`Name` and the current process.

<a name="force_delete-1"></a>

### force_delete/1 ###

<pre><code>
force_delete(Pool::any()) -&gt; true
</code></pre>
<br />

Forcibly remove a pool, terminating all active workers

This function is primarily intended for cleanup of any pools that might have
become inconsistent (for whatever reason). It will clear out all resources
belonging to the pool and send `exit(Pid, kill)` signals to all connected
workers (except the calling process).

<a name="log-1"></a>

### log/1 ###

<pre><code>
log(X1::GprocKey) -&gt; integer()
</code></pre>
<br />

Update a counter associated with a worker name.

Each added worker has a gproc counter that can be used e.g. to keep track of
the number of times the worker has been picked. Since it's associated with the
named 'slot', and not to the connected worker, its value will persist even
if the currently connected worker dies.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Pool::any()) -&gt; ok
</code></pre>
<br />

Equivalent to [`new(Pool, round_robin, [])`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Pool::any(), Type, Opts) -&gt; true
</code></pre>

<ul class="definitions"><li><code>Type = round_robin | random | hash | direct | claim</code></li><li><code>Opts = [{size, integer()} | {auto_size, boolean()}]</code></li></ul>

Create a new pool.

The pool starts out empty. If a size is not given, the pool size is set to
0 initially. `auto_size` is `true` by default if size is not specified, but
`false` by default otherwise. If `auto_size == true`, the pool will be
enlarged to accomodate new workers, when necessary. Otherwise, trying to add
a worker when the pool is full will raise an exception, as will trying to add
a worker on a specific position beyond the current size of the pool.

If the given pool already exists, this function will raise an exception.

<a name="pick-1"></a>

### pick/1 ###

<pre><code>
pick(Pool::any()) -&gt; GprocName | false
</code></pre>
<br />

Pick a worker from the pool given the pool's load-balancing algorithm.

The pool types that allows picking without an extra argument are
round_robin and random. This function returns `false` if there is no available
worker, or if `Pool` is not a valid pool.

<a name="pick-2"></a>

### pick/2 ###

<pre><code>
pick(Pool::any(), Value::any()) -&gt; GprocName | false
</code></pre>
<br />

Pick a worker from the pool based on `Value`.

The pool types that allows picking based on an extra argument are
hash and direct. This function returns `false` if there is no available
worker, or if `Pool` is not a valid pool.

If the pool is of type `direct`, `Value` must be an integer corresponding to
a position in the pool (modulo the size of the pool). If the type is
`hash`, `Value` may be any term, and its hash value will serve as a guide for
selecting a worker.

<a name="pick_worker-1"></a>

### pick_worker/1 ###

<pre><code>
pick_worker(Pool::any()) -&gt; pid() | false
</code></pre>
<br />

Pick a worker pid from the pool given the pool's load-balancing algorithm.

Like [`pick/1`](#pick-1), but returns the worker pid instead of the name.

<a name="pick_worker-2"></a>

### pick_worker/2 ###

<pre><code>
pick_worker(Pool::any(), Value::any()) -&gt; pid() | false
</code></pre>
<br />

Pick a worker pid from the pool given the pool's load-balancing algorithm.

Like [`pick/2`](#pick-2), but returns the worker pid instead of the name.

<a name="ptest-4"></a>

### ptest/4 ###

`ptest(N, I, Type, Opts) -> any()`

<a name="randomize-1"></a>

### randomize/1 ###

<pre><code>
randomize(Pool::any()) -&gt; integer()
</code></pre>
<br />

Randomizes the "next" pointer for the pool.

This function only has an effect for `round_robin` pools, which have a
reference to the next worker to be picked. Without randomizing, the load
balancing will always start with the first worker in the pool.

<a name="remove_worker-2"></a>

### remove_worker/2 ###

<pre><code>
remove_worker(Pool::any(), Name::any()) -&gt; true
</code></pre>
<br />

Remove a previously added worker.

This function will assume that any connected worker is disconnected first.
It will fail if there is no such pool, but will return `true` in the case
when `Name` did not exist in the pool in the first place.

<a name="setup_test_pool-4"></a>

### setup_test_pool/4 ###

`setup_test_pool(P, Type0, Opts, Workers) -> any()`

<a name="test_run0-2"></a>

### test_run0/2 ###

`test_run0(N, X) -> any()`

<a name="whereis_worker-2"></a>

### whereis_worker/2 ###

<pre><code>
whereis_worker(Pool::any(), Name::any()) -&gt; pid() | undefined
</code></pre>
<br />

Look up the pid of a connected worker.

This function works similarly to `gproc:where/1`: it will return the pid
of the worker connected as `Pool / Name`, if there is such a worker; otherwise
it will return `undefined`. It will raise an exception if `Name` has not been
added to the pool.

<a name="worker_id-2"></a>

### worker_id/2 ###

<pre><code>
worker_id(Pool, Name) -&gt; GprocName
</code></pre>
<br />

Return the unique gproc name corresponding to a name in the pool.

This function assumes that `Name` has been added to `Pool`. It returns the
unique name that a connected worker will be registered as. This doesn't mean
that there is, in fact, such a connected worker.

<a name="worker_pool-1"></a>

### worker_pool/1 ###

<pre><code>
worker_pool(Pool::any()) -&gt; [integer() | {Name, Pos}]
</code></pre>
<br />

Return a list of slots and/or named workers in the pool.

This function is mainly for testing, but can also be useful when implementing
your own worker placement algorithm on top of gproc_pool.

A plain integer represents an unfilled slot, and `{Name, Pos}` represents an
added worker. The pool is always filled to the current size.

