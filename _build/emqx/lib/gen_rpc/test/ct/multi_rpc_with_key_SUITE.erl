%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(multi_rpc_with_key_SUITE).
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
eval_everywhere_mfa_multiple_nodes(_Config) ->
    ConnectedNodes = [{?SLAVE,key1}, ?SLAVE],
    Msg = Name = 'evalmfamulti',
    TestPid = self(),
    ok = terminate_process(Name),
    Pid = spawn_listener2(?SLAVE, ?SLAVE, Name, TestPid, 2),
    true = register(Name, Pid),
    abcast = gen_rpc:eval_everywhere(ConnectedNodes, 'gen_rpc_test_helper', ping, [{?MASTER, Name, Msg}]),
    {ok, passed} = wait_for_reply(?SLAVE, ?SLAVE),
    ok.

multicall_multiple_nodes(_Config) ->
    ConnectedNodes = [{?SLAVE,random_key}, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, []),
    [?SLAVE] = gen_rpc:nodes(),
    [_WithKey, _Bare] = supervisor:which_children(gen_rpc_client_sup).

multicall_multiple_nodes_and_local(_Config) ->
    ConnectedNodes = [{?SLAVE,random_key}, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, []),
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(os, timestamp, []),
    [?SLAVE] = gen_rpc:nodes(),
    [_WithKey, _Bare, _Local] = supervisor:which_children(gen_rpc_client_sup).

multicall_multiple_nodes_with_timeout(_Config) ->
    ConnectedNodes = [{?SLAVE,random_key}, ?SLAVE],
    {[{_,_,_}, {_,_,_}], []} = gen_rpc:multicall(ConnectedNodes, os, timestamp, [], 5000),
    [?SLAVE] = gen_rpc:nodes(),
    {[], BadNodes} = gen_rpc:multicall(ConnectedNodes, timer, sleep, [500], 100),
    true = lists:member(?SLAVE, BadNodes).

abcast(_Config) ->
    true = erlang:register(test_process_123, self()),
    abcast = rpc:call(?SLAVE, gen_rpc, abcast, [[{?MASTER,random_key}], test_process_123, this_is_a_test]),
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
    abcast = rpc:call(?SLAVE, gen_rpc, abcast, [[{?MASTER,random_key}, ?FAKE_NODE], test_process_123, this_is_a_test]),
    receive
        this_is_a_test -> ok;
        _ -> erlang:error(invalid_message)
    after
        2000 -> erlang:error(timeout)
    end,
    true = erlang:unregister(test_process_123).

sbcast(_Config) ->
    true = erlang:register(test_process_123, self()),
    {[{?MASTER,random_key}], []} = rpc:call(?SLAVE, gen_rpc, sbcast, [[{?MASTER,random_key}], test_process_123, this_is_a_test]),
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
    {[{?MASTER,random_key}], [?FAKE_NODE]} = rpc:call(?SLAVE, gen_rpc, sbcast, [[{?MASTER,random_key}, ?FAKE_NODE],
                                                      test_process_123, this_is_a_test]),
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
