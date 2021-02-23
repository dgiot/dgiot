%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(remote_with_key_SUITE).
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

init_per_testcase(client_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, client_inactivity_timeout, 500),
    ok = gen_rpc_test_helper:start_slave(tcp),
    Config;

init_per_testcase(server_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = gen_rpc_test_helper:start_slave(tcp),
    ok = rpc:call(?SLAVE, application, set_env, [?APP, server_inactivity_timeout, 500]),
    Config;

init_per_testcase(_OtherTest, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = gen_rpc_test_helper:start_slave(tcp),
    Config.

end_per_testcase(client_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:stop_slave(),
    ok = application:set_env(?APP, client_inactivity_timeout, infinity),
    Config;

end_per_testcase(server_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:stop_slave(),
    ok = application:set_env(?APP, server_inactivity_timeout, infinity),
    Config;

end_per_testcase(_OtherTest, Config) ->
    ok = gen_rpc_test_helper:stop_slave(),
    Config.

%%% ===================================================
%%% Test cases
%%% ===================================================
%% Test main functions
call(_Config) ->
    {_Mega1, _Sec1, _Micro1} = gen_rpc:call({?SLAVE,random_key}, os, timestamp),
    {_Mega2, _Sec2, _Micro2} = gen_rpc:call({?SLAVE,random_key_2}, os, timestamp),
    [_NodeAndKey1, _NodeAndKey2] = supervisor:which_children(gen_rpc_client_sup).

call_module_version_check_incompatible(_Config) ->
    {badrpc, incompatible} = gen_rpc:call({?SLAVE,random_key}, {gen_rpc_test_helper, "X.Y.Z"}, stub_function, []).

call_node(_Config) ->
    ?SLAVE = gen_rpc:call({?SLAVE,random_key}, erlang, node, []).

cast(_Config) ->
    true = gen_rpc:cast({?SLAVE,random_key}, erlang, timestamp).

async_call(_Config) ->
    YieldKey0 = gen_rpc:async_call({?SLAVE,random_key}, os, timestamp, []),
    {_Mega, _Sec, _Micro} = gen_rpc:yield(YieldKey0),
    NbYieldKey0 = gen_rpc:async_call({?SLAVE,random_key2}, os, timestamp, []),
    {value,{_,_,_}}= gen_rpc:nb_yield(NbYieldKey0, 50),
    YieldKey = gen_rpc:async_call({?SLAVE,random_key}, io_lib, print, [yield_key]),
    "yield_key" = gen_rpc:yield(YieldKey),
    NbYieldKey = gen_rpc:async_call({?SLAVE,random_key2}, io_lib, print, [nb_yield_key]),
    {value, "nb_yield_key"} = gen_rpc:nb_yield(NbYieldKey, 50).

async_call_yield_reentrant(_Config) ->
    YieldKey0 = gen_rpc:async_call({?SLAVE,random_key}, os, timestamp, []),
    {_Mega, _Sec, _Micro} = gen_rpc:yield(YieldKey0),
    Self = self(),
    Pid = erlang:spawn(fun()->
        timeout = gen_rpc:yield(YieldKey0),
        Self ! {self(), something_went_wrong}
    end),
    receive
        _ ->
            exit(got_answer_when_none_expected)
    after
        5000 ->
            true = erlang:exit(Pid, brutal_kill)
    end,
    NbYieldKey0 = gen_rpc:async_call({?SLAVE,random_key2}, os, timestamp, []),
    % Verify not able to reuse Key again. Key is one time use.
    {_,_,_} = gen_rpc:yield(NbYieldKey0),
    timeout = gen_rpc:nb_yield(NbYieldKey0, 10),
    YieldKey = gen_rpc:async_call({?SLAVE,random_key}, io_lib, print, [yield_key]),
    "yield_key" = gen_rpc:yield(YieldKey),
    NbYieldKey = gen_rpc:async_call({?SLAVE,random_key2}, io_lib, print, [nb_yield_key]),
    {value, "nb_yield_key"} = gen_rpc:nb_yield(NbYieldKey, 50).

async_call_nb_yield_infinity(_Config) ->
    YieldKey = gen_rpc:async_call(?SLAVE, timer, sleep, [1000]),
    ok = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?SLAVE, timer, sleep, [1000]),
    {value, ok} = gen_rpc:nb_yield(NBYieldKey, infinity),
    ok = ct:pal("Result [async_call_yield_infinity]: timer_sleep Result={ok}").

client_inactivity_timeout(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call({?SLAVE,random_key}, os, timestamp),
    ok = timer:sleep(600),
    ClientName = gen_rpc_helper:make_process_name("client", {?SLAVE,random_key}),
    undefined = erlang:whereis(ClientName),
    [] = supervisor:which_children(gen_rpc_client_sup).

server_inactivity_timeout(_Config) ->
    {_Mega1, _Sec1, _Micro1} = gen_rpc:call({?SLAVE,random_key}, os, timestamp),
    ok = timer:sleep(600),
    ClientName = gen_rpc_helper:make_process_name("client", ?SLAVE),
    undefined = erlang:whereis(ClientName),
    [] = supervisor:which_children(gen_rpc_client_sup).

random_local_tcp_close(_Config) ->
    {_Mega1, _Sec1, _Micro1} = rpc:call(?SLAVE, gen_rpc, call, [{?MASTER,random_key}, os, timestamp, []]),
    {_Mega2, _Sec2, _Micro2} = rpc:call(?SLAVE, gen_rpc, call, [{?MASTER,random_key2}, os, timestamp, []]),
    [{_,AccPid,_,_}, _Other] = supervisor:which_children(gen_rpc_acceptor_sup),
    true = erlang:exit(AccPid, kill),
    ok = timer:sleep(600), % Give some time to the supervisor to kill the children
    [?MASTER] = rpc:call(?SLAVE, gen_rpc, nodes, []),
    [_Other] = supervisor:which_children(gen_rpc_acceptor_sup),
    [_Client] = rpc:call(?SLAVE, supervisor, which_children, [gen_rpc_client_sup]).

random_remote_tcp_close(_Config) ->
    {_Mega1, _Sec1, _Micro1} = gen_rpc:call({?SLAVE,random_key}, os, timestamp),
    {_Mega2, _Sec2, _Micro2} = gen_rpc:call({?SLAVE,random_key2}, os, timestamp),
    [{_,AccPid,_,_}, _Other] = rpc:call(?SLAVE, supervisor, which_children, [gen_rpc_acceptor_sup]),
    true = rpc:call(?SLAVE, erlang, exit, [AccPid,kill]),
    ok = timer:sleep(600),
    [?SLAVE] = gen_rpc:nodes(),
    [_Client] = supervisor:which_children(gen_rpc_client_sup),
    [_Other] = rpc:call(?SLAVE, supervisor, which_children, [gen_rpc_acceptor_sup]).

%%% ===================================================
%%% Auxiliary functions for test cases
%%% ===================================================
