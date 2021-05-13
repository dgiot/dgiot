
# esockd [![Build Status](https://travis-ci.org/emqx/esockd.svg?branch=emqx30)](https://travis-ci.org/emqx/esockd)

Erlang General Non-blocking TCP/SSL Socket Server.

## Features

* General Non-blocking TCP/SSL Socket Server
* Acceptor Pool and Asynchronous TCP Accept
* UDP/DTLS Server
* Max connections management
* Allow/Deny by peer address
* Proxy Protocol V1/V2
* Keepalive Support
* Rate Limit
* IPv6 Support

## Usage

A Simple TCP Echo Server:

    -module(echo_server).

    -export([start_link/2, init/2]).

    start_link(Transport, Sock) ->
        {ok, spawn_link(?MODULE, init, [Transport, Sock])}.

    init(Transport, Sock) ->
        case Transport:wait(Sock) of
            {ok, NewSock} ->
                loop(Transport, NewSock);
            Error -> Error
        end.

    loop(Transport, Sock) ->
        case Transport:recv(Sock, 0) of
            {ok, Data} ->
                {ok, Peername} = Transport:peername(Sock),
                Transport:send(Sock, Data),
                loop(Transport, Sock);
            {error, Reason} ->
                io:format("TCP Error: ~s~n", [Reason]),
                {stop, Reason}
        end.

Setup Echo Server:

    %% Start esockd application
    ok = esockd:start().
    Options = [{acceptors, 10}, {max_connections, 1024}, {tcp_options, [binary, {reuseaddr, true}]}].
    MFArgs = {echo_server, start_link, []},
    esockd:open(echo, 5000, Options, MFArgs).

## Examples

Example                 | Description
------------------------|---------------------------
examples/async_recv     | prim_net async recv/send
examples/gen_server     | gen_server behaviour
examples/simple         | simple echo server
examples/ssl            | ssl echo server
examples/proxy_protocol | proxy protocol v1/2
examples/udp            | udp echo server
examples/dtls           | dtls echo server

## API

### Open a listener

    esockd:open(echo, 5000, [{tcp_options, [binary, {reuseaddr, true}]}],
                {echo_server, start_link, []}).

    esockd:open(echo, {"127.0.0.1", 6000}, [{tcp_options, [binary, {reuseaddr, true}]}],
                {echo_server, start_link, []}).

Spec:

    -spec(open(Protocol, ListenOn, Options, MFArgs) -> {ok, pid()} | {error, term()} when
               Protocol :: atom(),
               ListenOn :: inet:port_number() | {host(), inet:port_number()}),
               Options  :: [option()],
               MFArgs   :: esockd:mfargs()).

Option:

    -type(option() :: {acceptors, pos_integer()}
                    | {max_connections, pos_integer()}
                    | {max_conn_rate, pos_integer()}
                    | {access_rules, [esockd_access:rule()]}
                    | {shutdown, brutal_kill | infinity | pos_integer()}
                    | tune_buffer | {tune_buffer, boolean()}
                    | proxy_protocol | {proxy_protocol, boolean()}
                    | {proxy_protocol_timeout, timeout()}
                    | {ssl_options, [ssl:ssl_option()]}
                    | {udp_options, [gen_udp:option()]}
                    | {dtls_options, [gen_udp:option() | ssl:ssl_option()]}).

MFArgs:

    -type(mfargs() :: atom() | {atom(), atom()} | {module(), atom(), [term()]}).

### Get Setting and Stats

Get stats:

    esockd:get_stats({echo, 5000}).

Get acceptors:

    esockd:get_acceptors({echo, {"127.0.0.1", 6000}}).

Get/Set max connections:

    esockd:get_max_connections({echo, 5000}).
    esockd:set_max_connections({echo, 5000}, 100000).

### Allow/Deny

Same to Allow/Deny Syntax of nginx:

    allow address | CIDR | all;

    deny address | CIDR | all;

allow/deny by options:

    esockd:open(echo, 5000, [{access, [{deny, "192.168.1.1"}, {allow, "192.168.1.0/24"}, {deny, all}]}], MFArgs).

allow/deny by API:

    esockd:allow({echo, 5000}, all).
    esockd:allow({echo, 5000}, "192.168.0.1/24").
    esockd:deny({echo, 5000}, all).
    esockd:deny({echo, 5000}, "10.10.0.0/16").

### Close a listener

    esockd:close(echo, 5000).
    esockd:close(echo, {"127.0.0.1", 6000}).

Spec:

    -spec(close(Protocol, ListenOn) -> ok when Protocol :: atom(), ListenOn :: inet:port_number() | {host(), inet:port_number()}).

### SSL

Connecting to ssl_echo_server:

    openssl s_client -connect 127.0.0.1:5000 -ssl3

    openssl s_client -connect 127.0.0.1:5000 -tls1

## Design

### Supervisor Tree

    esockd_sup
        -> esockd_listener_sup
            -> esockd_listener
            -> esockd_acceptor_sup
                -> esockd_acceptor
                -> esockd_acceptor
                -> ......
            -> esockd_connection_sup
                -> esockd_connection
                -> esockd_connection
                -> ......

### Acceptor

1. Acceptor Pool

2. Suspend for one second when e{n, m}file errors happened

### Connection Sup

1. Create a connection, and let it run...

2. Control maximum connections

3. Count active connections

4. Count shutdown reasons

### CIDR

CIDR Wiki: https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing

## Benchmark

Benchmark 2.1.0-alpha release on one 8 cores, 32G memory ubuntu/14.04 server::

    250K concurrent connections, 50K messages/sec, 40Mbps In/Out consumed 5G memory, 20% CPU/core

## License

Apache License Version 2.0

## Author

EMQ X Team.

