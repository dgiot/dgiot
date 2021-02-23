%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_acceptor_sup).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(supervisor).

%%% Include the HUT library
-include("logger.hrl").

%%% Supervisor functions
-export([start_link/0, start_child/2, stop_child/1]).

%%% Supervisor callbacks
-export([init/1]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), {inet:ip4_address(), inet:port_number()}) -> supervisor:startchild_ret().
start_child(Driver, Peer) when is_tuple(Peer) ->
    ?log(debug, "event=starting_new_acceptor peer=\"~s\"", [gen_rpc_helper:peer_to_string(Peer)]),
    case supervisor:start_child(?MODULE, [Driver,Peer]) of
        {error, {already_started, CPid}} ->
            %% If we've already started the child, terminate it and start anew
            ok = stop_child(CPid),
            supervisor:start_child(?MODULE, [Peer]);
        {error, OtherError} ->
            {error, OtherError};
        {ok, Pid} ->
            {ok, Pid}
    end.

-spec stop_child(pid()) ->  ok.
stop_child(Pid) when is_pid(Pid) ->
    ?log(debug, "event=stopping_acceptor acceptor_pid=\"~p\"", [Pid]),
    _ = supervisor:terminate_child(?MODULE, Pid),
    ok.

%%% ===================================================
%%% Supervisor callbacks
%%% ===================================================
init([]) ->
    {ok, {{simple_one_for_one, 100, 1}, [
        {gen_rpc_acceptor, {gen_rpc_acceptor,start_link,[]}, temporary, 5000, worker, [gen_rpc_acceptor]}
    ]}}.
