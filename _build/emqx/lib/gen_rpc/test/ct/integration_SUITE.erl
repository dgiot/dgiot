%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(integration_SUITE).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% CT Macros
-include_lib("test/include/ct.hrl").

%%% No need to export anything, everything is automatically exported
%%% as part of the test profile

%%% ===================================================
%%% CT callback functions
%%% ===================================================
all() ->
    gen_rpc_test_helper:get_test_functions(?MODULE).

init_per_suite(Config) ->
    %% Starting Distributed Erlang on local node
    {ok, _Pid} = gen_rpc_test_helper:start_distribution(this_node()),
    %% Setup application logging
    ok = gen_rpc_test_helper:set_application_environment(?MASTER),
    %% Setup the simple TCP driver
    ok = gen_rpc_test_helper:set_driver_configuration(tcp, ?MASTER),
    %% Starting the application locally
    {ok, _MasterApps} = application:ensure_all_started(?APP),
    Config.

end_per_suite(_Config) ->
    ok.

this_node() ->
    list_to_atom(os:getenv("NODE")).

peers() ->
    NodeStr = os:getenv("NODES"),
    NodeList = string:tokens(NodeStr, [$:]),
    NodeNames = [list_to_atom("gen_rpc@" ++ Ip) || Ip <- NodeList],
    NodeNames.

%%% ===================================================
%%% Test cases
%%% ===================================================
call(_Config) ->
    Peers = peers(),
    ok = lists:foreach(fun(Node) ->
        {_,_,_} = gen_rpc:call(Node, os, timestamp, [])
    end, peers()),
    ok = lists:foreach(fun(Node) ->
        Node = gen_rpc:call(Node, erlang, node, [])
    end, peers()),
    Alive = gen_rpc:nodes(),
    ok = lists:foreach(fun(Node) ->
        true = lists:member(Node, Alive)
    end, Peers).

multicall(_Config) ->
    Peers = peers(),
    Alive = gen_rpc:nodes(),
    {RespList, []} = gen_rpc:multicall(os, timestamp, []),
    PeersLen = length(Peers),
    AliveLen = length(Alive),
    RespLen = length(RespList),
    PeersLen = AliveLen,
    RespLen = AliveLen + 1.

remote_socket_close(_Config) ->
    Peers = peers(),
    ok = lists:foreach(fun(Node) ->
        [{_,AccPid,_,_}] = rpc:call(Node, supervisor, which_children, [gen_rpc_acceptor_sup]),
        true = rpc:call(Node, erlang, exit, [AccPid,normal])
    end, peers()),
    ok = timer:sleep(1000),
    Alive = gen_rpc:nodes(),
    ok = lists:foreach(fun(Node) ->
        false = lists:member(Node, Alive)
    end, Peers).
