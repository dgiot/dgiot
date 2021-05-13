

# Module gproc_pt #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Parse transform utility for gproc users.

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)), Dmitry Demeshchuk ([`demeshchuk@gmail.com`](mailto:demeshchuk@gmail.com)).

<a name="description"></a>

## Description ##

This module provides some closer syntactical integration for
people who are enthusiastic gproc users.

Specifically, this module transforms `Pid ! Msg` into
`gproc:send(Pid, Msg)`, which, apart from accepting any type for
`Pid` that `!` understands, is also able to handle a gproc "triple",
e.g. `{n, l, Name}` or even `{p, l, Prop}` (in the latter case, the
message may be delivered to multiple recipients).

Users should be aware that parse transforms may be confusing to
the casual reader, since they extend the semantics of possibly
ubiquitous constructs (as is the case with this transform). Therefore,
you should document clearly that this is happening.

Original suggestion by Dimitry Demeschuk.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_transform-2"></a>

### parse_transform/2 ###

`parse_transform(Forms, Options) -> any()`

