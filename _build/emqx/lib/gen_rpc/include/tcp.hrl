%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

%%% Default TCP options
-define(TCP_DEFAULT_OPTS, [binary,
        {packet,4},
        {exit_on_close,true},
        {show_econnreset,true}, % Send message for reset connections
        {nodelay,true}, % Send our requests immediately
        {send_timeout_close,true}, % When the socket times out, close the connection
        {delay_send,false}, % Scheduler should favor timely delivery
        {linger,{true,2}}, % Allow the socket to flush outgoing data for 2" before closing it - useful for casts
        {reuseaddr,true}, % Reuse local port numbers
        {keepalive,true}, % Keep our channel open
        {tos,72}, % Deliver immediately
        {active,false}]). % Retrieve data from socket upon request

%%% Default TCP options
-define(ACCEPTOR_DEFAULT_TCP_OPTS, [binary,
        {packet,4},
        {exit_on_close,true},
        {active,once}]). % Retrieve data from socket upon request

%%% The TCP options that should be copied from the listener to the acceptor
-define(ACCEPTOR_COPY_TCP_OPTS, [nodelay,
        show_econnreset,
        send_timeout_close,
        delay_send,
        linger,
        reuseaddr,
        keepalive,
        tos,
        active]).

-define(DARWIN_SOL_SOCKET, 16#ffff).
-define(DARWIN_IPPROTO_TCP, 16#0006).
-define(DARWIN_SO_KEEPALIVE, 16#0008).
-define(DARWIN_TCP_KEEPALIVE, 16#10). % idle time used when SO_KEEPALIVE is enabled
-define(DARWIN_TCP_KEEPINTVL, 16#101). % interval between keepalives
-define(DARWIN_TCP_KEEPCNT, 16#102). % number of keepalives before close

-define(LINUX_SOL_SOCKET, 16#0001).
-define(LINUX_SOL_TCP, 16#0006).
-define(LINUX_SO_KEEPALIVE, 16#0009).
-define(LINUX_TCP_KEEPIDLE, 16#4). % idle time used when SO_KEEPALIVE is enabled
-define(LINUX_TCP_KEEPINTVL, 16#5). % interval between keepalives
-define(LINUX_TCP_KEEPCNT, 16#6). % number of keepalives before close
