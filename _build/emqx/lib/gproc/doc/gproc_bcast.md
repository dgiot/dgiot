

# Module gproc_bcast #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Gproc message broadcast server
This module is used to support gproc:bcast(Key, Msg).

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

<a name="description"></a>

## Description ##
gproc:bcast/2 allows for e.g. distributed publish/subscribe, without
having to resort to global property registration.
To ensure that erlang's message ordering guarantees are kept, all sends
are channeled through a broadcast server on each node.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(X1, S, X3) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, X2, S) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(X1, S) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(X1, S) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(X1, X2) -> any()`

