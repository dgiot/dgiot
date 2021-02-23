

# The gproc application #

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)), Joseph Wayne Norton ([`norton@geminimobile.com`](mailto:norton@geminimobile.com)).

Extended process dictionary

[![Build Status](https://travis-ci.org/uwiger/gproc.png?branch=master)](https://travis-ci.org/uwiger/gproc)
[![Hex pm](http://img.shields.io/hexpm/v/gproc.svg?style=flat)](https://hex.pm/packages/gproc)


## Note ##

Gproc has two dependencies: `gen_leader` and `edown`. Since most people don't
actively use either, they are no longer fetched by default.

* To enable fetching of `gen_leader`, export the OS environment variable`GPROC_DIST=true` (this can be done e.g. from a GNU Makefile)

* `edown` is fetched on-demand whenever `rebar get-deps doc` is called (which
  happens when you call `make doc`)



## Installation ##

You can get `gproc` from the [Hex package manager](https://hex.pm/packages/gproc)

That means declaring dependency on `{gproc, "0.5.0"}` in your `rebar3`-based applications or `{:gproc, "~> 0.5.0"}` in your `mix` based applications.


## Introduction ##

Gproc is a process dictionary for Erlang, which provides a number of useful features beyond what the built-in dictionary has:

* Use any term as a process alias

* Register a process under several aliases

* Non-unique properties can be registered simultaneously by many processes

* QLC and match specification interface for efficient queries on the
  dictionary

* Await registration, let's you wait until a process registers itself

* Atomically give away registered names and properties to another process

* Counters, and aggregated counters, which automatically maintain the
  total of all counters with a given name

* Global registry, with all the above functions applied to a network of nodes



### Use case: System inspection ###

Gproc was designed to work as a central index for "process metadata", i.e.
properties that describe the role and characteristics of each process. Having
a single registry that is flexible enough to hold important types of property
makes it easier to (a) find processes of a certain type, and (b) query and
browse key data in a running system.


### Use case: Pub/Sub patterns ###

An interesting application of gproc is building publish/subscribe patterns.
Example:

```erlang

subscribe(EventType) ->
    %% Gproc notation: {p, l, Name} means {(p)roperty, (l)ocal, Name}
    gproc:reg({p, l, {?MODULE, EventType}}).

notify(EventType, Msg) ->
    Key = {?MODULE, EventType},
    gproc:send({p, l, Key}, {self(), Key, Msg}).

```


### Use case: Environment handling ###

Gproc provides a set of functions to read environment variables, possibly from
alternative sources, and cache them for efficient lookup. Caching also provides
a way to see which processes rely on certain configuration values, as well as
which values they actually ended up using.

See [`gproc:get_env/4`](http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc.md#get_env-4), [`gproc:get_set_env/4`](http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc.md#get_set_env-4) and
[`gproc:set_env/5`](http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc.md#set_env-5) for details.


## Testing ##

Gproc has a QuickCheck test suite, covering a fairly large part of the local
gproc functionality, although none of the global registry. It requires a
commercial EQC license, but rebar is smart enough to detect whether EQC is
available, and if it isn't, the code in gproc_eqc.erl will be "defined away".

There is also an eunit suite, covering the basic operations for local and
global gproc.


## Building Edoc ##

By default, `./rebar doc` generates Github-flavored Markdown files.
If you want to change this, remove the `edoc_opts` line from `rebar.config`.

Gproc was first introduced at the ACM SIGPLAN Erlang Workshop in
Freiburg 2007 ([Paper available here](http://github.com/uwiger/gproc/blob/uw-change-license/doc/erlang07-wiger.pdf)).


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc.md" class="module">gproc</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_app.md" class="module">gproc_app</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_bcast.md" class="module">gproc_bcast</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_dist.md" class="module">gproc_dist</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_info.md" class="module">gproc_info</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_init.md" class="module">gproc_init</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_lib.md" class="module">gproc_lib</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_monitor.md" class="module">gproc_monitor</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_pool.md" class="module">gproc_pool</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_ps.md" class="module">gproc_ps</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_pt.md" class="module">gproc_pt</a></td></tr>
<tr><td><a href="http://github.com/uwiger/gproc/blob/uw-change-license/doc/gproc_sup.md" class="module">gproc_sup</a></td></tr></table>

