%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_driver).

-callback connect(atom(), inet:port_number()) -> {ok, term()} | {error, term()}.

-callback listen(inet:port_number()) -> {ok, term()} | {error, term()}.

-callback accept(term()) -> {ok, inet:socket() | ssl:sslsocket()} | {error, term()}.

-callback activate_socket(term()) -> ok.

-callback authenticate_server(term()) -> ok | {error, {badtcp | badrpc, term()}}.

-callback authenticate_client(term(), tuple(), binary()) -> ok | {error, {badtcp | badrpc, term()}}.

-callback send(term(), binary()) -> ok | {error, term()}.

-callback get_peer(term()) -> {inet:ip4_address(), inet:port_number()}.

-callback copy_sock_opts(term(), term()) -> ok | {error, any()}.

-callback set_controlling_process(term(), pid()) -> ok | {error, term()}.

-callback set_send_timeout(term(), timeout() | undefined) -> ok.

-callback set_acceptor_opts(term()) -> ok.

-ifdef(TEST).
%% Stub function to fool code coverage
-export([stub/0]).
stub() -> ok.
-endif.
