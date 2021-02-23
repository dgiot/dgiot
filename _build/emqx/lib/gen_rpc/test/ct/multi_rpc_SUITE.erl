%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(multi_rpc_SUITE).
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
    {ok, _Pid} = gen_rpc_test_helper:start_distribution(?MASTER),
    %% Setup the app locally
    ok = gen_rpc_test_helper:start_master(tcp),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(sbcast_with_bad_server, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = gen_rpc_test_helper:start_slave(tcp),
    %% Set a low sbcast timeout
    ok = rpc:call(?SLAVE, application, set_env, [?APP, sbcast_receive_timeout, 500]),
    Config;

init_per_testcase(_OtherTest, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = gen_rpc_test_helper:start_slave(tcp),
    Config.

end_per_testcase(_OtherTest, Config) ->
    ok = gen_rpc_test_helper:stop_slave(),
    Config.

%%% ===================================================
%%% Test cases
%%% ===================================================
%% Test main functions
eval_everywhere_mfa_no_node(_Config) ->
    ConnectedNodes = [],
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, erlang, whereis, [node()]),
    % Nothing catastrophically on sender side after sending call to the ether.
    true = erlang:is_process_alive(whereis(gen_rpc_acceptor_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_client_sup)).

%% Eval_everywhere is fire and forget, which means some test cases need to show
%% something has been executed on target nodes.
%% The main technique used in eval_everywhere testing is to setup servers - slaves,
%% and have the test script to ping the slaves with tagged messages and then wait for
%% such and match the tags to verify originality.

%% Ping the target node with tagged msg. Target Node reply back with
%% tagged msg and its own identity.

eval_everywhere_mfa_one_node(_Config) ->
    ConnectedNodes = [?SLAVE],
    Msg = Name = 'evalmfa1',
    TestPid = self(),
    ok = terminate_process(Name),
    Pid = spawn_listener(?SLAVE, Name, TestPid),
    true = register(Name, Pid),
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, 'gen_rpc_test_helper', ping, [{?MASTER, Name, Msg}]),
    {ok, passed} = wait_for_reply(?SLAVE),
    ok.

eval_everywhere_mfa_multiple_nodes(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    Msg = Name = 'evalmfamulti',
    TestPid = self(),
    ok = terminate_process(Name),
    Pid = spawn_listener2(?SLAVE, ?SLAVE, Name, TestPid, 2),
    true = register(Name, Pid),
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, 'gen_rpc_test_helper', ping, [{?MASTER, Name, Msg}]),
    {ok, passed} = wait_for_reply(?SLAVE, ?SLAVE),
    ok.

eval_everywhere_mfa_multiple_nodes_timeout(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    Msg = Name = 'evalmfamultito',
    TestPid = self(),
    ok = terminate_process(Name),
    Pid = spawn_listener2(?SLAVE, ?SLAVE, Name, TestPid, 2),
    true = register(Name, Pid),
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, 'gen_rpc_test_helper', ping, [{?MASTER, Name, Msg}], 10),
    {ok, passed} = wait_for_reply(?SLAVE, ?SLAVE),
    ok.

eval_everywhere_mfa_exit_multiple_nodes(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, erlang, exit, [fatal]),
    % Nothing blows up on sender side after sending call to nothing
    true = erlang:is_process_alive(whereis(gen_rpc_acceptor_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_client_sup)).

eval_everywhere_mfa_throw_multiple_nodes(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, erlang, throw, ['throwXup']),
    ok = ct:pal("[erlang:throw only]. Verify the crash log from ct. You should see {{nocatch,throwXup}, ....} on the target node").

eval_everywhere_mfa_timeout_multiple_nodes(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, erlang, throw, ['throwXup']),
    ok = ct:pal("[erlang:throw only]. Verify the crash log from ct. You should see {{nocatch,throwXup}, ....} on the target node").

multicall_local_node(_Config) ->
    {[{_,_,_}], []} = gen_rpc:multicall(os, timestamp, []),
    %% Nodes should not include us
    false = lists:member(node(), gen_rpc:nodes()).

multicall_multiple_nodes(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, []),
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes).

multicall_with_bad_module_version(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    {[], ConnectedNodes} = gen_rpc:multicall(ConnectedNodes, {gen_rpc_test_helper, "X.Y.Z"}, stub_function, []),
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes).

multicall_with_good_module_version(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    {[stub_function, stub_function], []} = gen_rpc:multicall(ConnectedNodes, {gen_rpc_test_helper, "1.0.0"}, stub_function, []),
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes).

multicall_multiple_nodes_and_local(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, []),
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(os, timestamp, []),
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes).

multicall_multiple_nodes_with_timeout(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, [], 5000),
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes),
    {[], BadNodes} = gen_rpc:multicall(ConnectedNodes, timer, sleep, [500], 100),
    true = lists:member(?SLAVE, BadNodes).

multicall_multiple_nodes_with_bad_node(_Config) ->
    ConnectedNodes = [?SLAVE, ?SLAVE, ?FAKE_NODE],
    {Results, BadNodes} = gen_rpc:multicall(ConnectedNodes, os, timestamp, [], 10000),
    2 = length(Results),
    [?FAKE_NODE] = BadNodes,
    Nodes = gen_rpc:nodes(),
    true = lists:member(?SLAVE, Nodes),
    false = lists:member(?FAKE_NODE, Nodes).

abcast(_Config) ->
    true = erlang:register(test_process_123, self()),
    abcast = rpc:call(?SLAVE, gen_rpc, abcast, [[?MASTER], test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    [?MASTER] = rpc:call(?SLAVE, gen_rpc, nodes, []),
    abcast = rpc:call(?SLAVE, gen_rpc, abcast, [test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    true = erlang:unregister(test_process_123).

abcast_with_bad_server(_Config) ->
    true = erlang:register(test_process_123, self()),
    abcast = rpc:call(?SLAVE, gen_rpc, abcast, [[?MASTER, ?FAKE_NODE], test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    true = erlang:unregister(test_process_123).

sbcast(_Config) ->
    true = erlang:register(test_process_123, self()),
    {[?MASTER], []} = rpc:call(?SLAVE, gen_rpc, sbcast, [[?MASTER], test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    [?MASTER] = rpc:call(?SLAVE, gen_rpc, nodes, []),
    {[?MASTER], [?SLAVE]} = rpc:call(?SLAVE, gen_rpc, sbcast, [test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    true = erlang:unregister(test_process_123).

sbcast_with_bad_server(_Config) ->
    true = erlang:register(test_process_123, self()),
    {[?MASTER], [?FAKE_NODE]} = rpc:call(?SLAVE, gen_rpc, sbcast, [[?MASTER, ?FAKE_NODE], test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    true = erlang:unregister(test_process_123).

%%% ===================================================
%%% Auxiliary functions for test cases
%%% ===================================================
%% This is the middleman process listening for messages from slave nodes
%% Then relay back to test case Pid for check.
spawn_listener(Node, Name, TestPid)->
    spawn(fun() -> loop1(Node, Name, TestPid) end).

spawn_listener2(Node1, Node2, Name, TestPid, Count)->
    spawn(fun() -> loop2(Node1, Node2, Name, TestPid, Count) end).

loop1(Node, Name, TestPid) ->
    receive
        done ->
            {ok, done};
        {pong, {Node, _, Name}} ->
            ok = ct:pal("Receive pong from node=\"~s\" process=\"~p\"",[Node, Name]),
            TestPid ! {ok, Node, passed};
        Else ->
            ok = ct:pal("Unknown Message: \"~p\"", [Else]),
            TestPid ! Else
    after
        10000 ->
            ok = ct:pal("pong timeout", []),
            {error, pong_timeout}
    end.

loop2(_, _, _, _, Count) when Count =< 0 ->
    ok;

loop2(Node1, Node2, Name, TestPid, Count) ->
    receive
        done ->
            {ok, done};
        {pong, {Node1, _, Name}} ->
            ok = ct:pal("Receive pong from node=\"~p\" process=\"~p\"",[Node1, Name]),
            TestPid ! {ok, Node1, passed},
            loop2(Node1, Node2, Name, TestPid, Count-1);
        {pong, {Node2, _, Name}} ->
            ok = ct:pal("Receive pong from node=\"~p\" process=\"~p\"",[Node2, Name]),
            TestPid ! {ok, Node2, passed},
            loop2(Node1, Node2, Name, TestPid, Count-1);
        Else ->
            ok = ct:pal("Unknown Message: \"~p\"", [Else]),
            TestPid ! Else,
            loop2(Node1, Node2, Name, TestPid, Count)
    after
        10000 ->
            ok = ct:pal("pong timeout", []),
            {error, pong_timeout}
    end.

wait_for_reply(Node)->
    wait_for_reply(Node, 0).

wait_for_reply(_Node1, 1) ->
    {ok,passed};

wait_for_reply(Node, Acc) when is_atom(Node), is_integer(Acc) ->
    {ok, passed} = receive
        {ok, Node, passed} ->
            ok = ct:pal("function=wait_for_reply event_found_from=\"~p\"", [Node]),
            wait_for_reply(Node, Acc+1);
        Else ->
            ok = ct:pal("function=wait_for_reply event_unknown_msg=\"~p\"", [Else]),
             wait_for_reply(Node, Acc)
    after
         10000 ->
            receive
                M ->
                    {error, {msg_too_late, M}}
            end
    end;

wait_for_reply(Node1, Node2) ->
    wait_for_reply(Node1, Node2, 0).

wait_for_reply(_Node1, _Node2, 2) ->
    {ok,passed};

wait_for_reply(Node1, Node2, Acc)  when is_atom(Node1) , is_atom(Node2), is_integer(Acc) ->
    {ok,passed} = receive
        {ok, Node1, passed} ->
            ok = ct:pal("function=wait_for_reply event_found_from=\"~p\"", [Node1]),
            wait_for_reply(Node1, Node2, Acc+1);
        {ok, Node2, passed} ->
            ok = ct:pal("function=wait_for_reply event_found_from=\"~p\"", [Node2]),
            wait_for_reply(Node1, Node2, Acc+1);
        Else ->
            ok = ct:pal("function=wait_for_reply event_unkn0wn_msg=\"~p\"", [Else]),
             wait_for_reply(Node1, Node2, Acc)
    after
        10000 ->
            receive
                M ->
                    {error, {msg_too_late, M}}
            end
    end.

%% Terminate named processes
terminate_process(Name) ->
    terminate_process(Name, normal).

terminate_process(undefined, _) ->
    ok;

terminate_process(Proc, Reason) when is_atom(Proc) ->
    terminate_process(whereis(Proc), Reason);

terminate_process(Pid, Reason) when is_pid(Pid), is_atom(Reason) ->
    true = exit(Pid, Reason),
    ok.
