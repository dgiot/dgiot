%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_sup).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(supervisor).

%%% Supervisor functions
-export([start_link/0]).

%%% Supervisor callbacks
-export([init/1]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% ===================================================
%%% Supervisor callbacks
%%% ===================================================
init([]) ->
    {ok, {{one_for_one, 100, 1}, [
        {gen_rpc_server_tcp, {gen_rpc_server,start_link,[tcp]}, permanent, 5000, worker, [gen_rpc_server]},
        {gen_rpc_server_ssl, {gen_rpc_server,start_link,[ssl]}, permanent, 5000, worker, [gen_rpc_server]},
        {gen_rpc_acceptor_sup, {gen_rpc_acceptor_sup,start_link, []}, permanent, 5000, supervisor, [gen_rpc_acceptor_sup]},
        {gen_rpc_dispatcher, {gen_rpc_dispatcher,start_link, []}, permanent, 5000, worker, [gen_rpc_dispatcher]},
        {gen_rpc_client_sup, {gen_rpc_client_sup,start_link, []}, permanent, 5000, supervisor, [gen_rpc_client_sup]}
    ]}}.
