%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_client_config).

-callback get_config(atom()) -> {tcp | ssl, inet:port_number()} | {error, term()}.

-ifdef(TEST).
%% Stub function to fool code coverage
-export([stub/0]).
stub() -> ok.
-endif.
