%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(local_SUITE).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% CT Macros
-include_lib("test/include/ct.hrl").
%%% TCP settings
-include("tcp.hrl").

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

init_per_testcase(authentication_timeout, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, authentication_timeout, 500),
    Config;

init_per_testcase(client_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, client_inactivity_timeout, 500),
    Config;

init_per_testcase(server_inactivity_timeout, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, server_inactivity_timeout, 500),
    Config;

init_per_testcase(async_call_inexistent_node, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    Config;

init_per_testcase(remote_node_call, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = gen_rpc_test_helper:start_slave(tcp),
    Config;

init_per_testcase(rpc_module_whitelist, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, rpc_module_list, [erlang, os]),
    ok = application:set_env(?APP, rpc_module_control, whitelist),
    Config;

init_per_testcase(rpc_module_blacklist, Config) ->
    ok = gen_rpc_test_helper:restart_application(),
    ok = gen_rpc_test_helper:start_master(tcp),
    ok = application:set_env(?APP, rpc_module_list, [erlang, os]),
    ok = application:set_env(?APP, rpc_module_control, blacklist),
    Config;

init_per_testcase(_OtherTest, Config) ->
    Config.

end_per_testcase(authentication_timeout, Config) ->
    ok = application:set_env(?APP, authentication_timeout, 5000),
    Config;

end_per_testcase(client_inactivity_timeout, Config) ->
    ok = application:set_env(?APP, client_inactivity_timeout, infinity),
    Config;

end_per_testcase(server_inactivity_timeout, Config) ->
    ok = application:set_env(?APP, server_inactivity_timeout, infinity),
    Config;

end_per_testcase(remote_node_call, Config) ->
    ok = gen_rpc_test_helper:stop_slave(),
    Config;

end_per_testcase(rpc_module_whitelist, Config) ->
    ok = application:set_env(?APP, rpc_module_list, []),
    ok = application:set_env(?APP, rpc_module_control, disabled),
    Config;

end_per_testcase(rpc_module_blacklist, Config) ->
    ok = application:set_env(?APP, rpc_module_list, []),
    ok = application:set_env(?APP, rpc_module_control, disabled),
    Config;

end_per_testcase(_OtherTest, Config) ->
    Config.

%%% ===================================================
%%% Test cases
%%% ===================================================
%% Test supervisor's status
supervisor_black_box(_Config) ->
    true = erlang:is_process_alive(erlang:whereis(gen_rpc_acceptor_sup)),
    true = erlang:is_process_alive(erlang:whereis(gen_rpc_client_sup)),
    true = erlang:is_process_alive(erlang:whereis(gen_rpc_server_tcp)),
    true = erlang:is_process_alive(erlang:whereis(gen_rpc_dispatcher)),
    ok.

%% Test main functions
call(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call(?MASTER, os, timestamp).

call_anonymous_function(_Config) ->
    {_,"\"call_anonymous_function\""} = gen_rpc:call(?MASTER, erlang, apply,[fun(A) -> {self(), io_lib:print(A)} end,
                                                     ["call_anonymous_function"]]).

call_anonymous_undef(_Config) ->
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}}  = gen_rpc:call(?MASTER, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
   ok = ct:pal("Result [call_anonymous_undef]: signal=EXIT Reason={os,timestamp_undef}").

call_mfa_undef(_Config) ->
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}} = gen_rpc:call(?MASTER, os, timestamp_undef),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

call_mfa_exit(_Config) ->
    {badrpc, {'EXIT', die}} = gen_rpc:call(?MASTER, erlang, exit, ['die']),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={die}").

call_mfa_throw(_Config) ->
    'throwXdown' = gen_rpc:call(?MASTER, erlang, throw, ['throwXdown']),
    ok = ct:pal("Result [call_mfa_undef]: signal=EXIT Reason={throwXdown}").

call_with_receive_timeout(_Config) ->
    {badrpc, timeout} = gen_rpc:call(?MASTER, timer, sleep, [500], 1),
    ok = timer:sleep(500).

call_with_worker_kill(_Config) ->
    {badrpc, killed} = gen_rpc:call(?MASTER, timer, kill_after, [0]).

call_module_version_check_success(_Config) ->
    stub_function = gen_rpc:call(?MASTER, {gen_rpc_test_helper, "1.0.0"}, stub_function, []).

call_module_version_check_incompatible(_Config) ->
    {badrpc, incompatible} = gen_rpc:call(?MASTER, {gen_rpc_test_helper, "X.Y.Z"}, stub_function, []).

call_module_version_check_invalid(_Config) ->
    {badrpc, incompatible} = gen_rpc:call(?MASTER, {gen_rpc_test_helper1, "X.Y.Z"}, stub_function, []),
    {badrpc, incompatible} = gen_rpc:call(?MASTER, {rpc, 1}, cast, []).

interleaved_call(_Config) ->
    %% Spawn 3 consecutive processes that execute gen_rpc:call
    %% to the remote node and wait an inversely proportionate time
    %% for their result (effectively rendering the results out of order)
    %% in order to test proper data interleaving
    Pid1 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 1, infinity]),
    Pid2 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 2, 10]),
    Pid3 = erlang:spawn(?MODULE, interleaved_call_proc, [self(), 3, infinity]),
    ok = interleaved_call_loop(Pid1, Pid2, Pid3, 0),
    ok.

authentication_timeout(_Config) ->
    [] = supervisor:which_children(gen_rpc_acceptor_sup),
    {ok, _Sock} = gen_tcp:connect("127.0.0.1", ?MASTER_PORT, ?TCP_DEFAULT_OPTS),
    %% Give the server some time to launch the acceptor
    ok = timer:sleep(50),
    %% The acceptor has been launched
    [_Master] = supervisor:which_children(gen_rpc_acceptor_sup),
    ok = timer:sleep(1000),
    %% The acceptor should have shut down
    [] = supervisor:which_children(gen_rpc_acceptor_sup),
    ok.

cast(_Config) ->
    true = gen_rpc:cast(?MASTER, erlang, timestamp).

cast_anonymous_function(_Config) ->
    true = gen_rpc:cast(?MASTER, erlang, apply, [fun() -> os:timestamp() end, []]).

cast_mfa_undef(_Config) ->
    true = gen_rpc:cast(?MASTER, os, timestamp_undef, []).

cast_mfa_exit(_Config) ->
    true = gen_rpc:cast(?MASTER, erlang, apply, [fun() -> exit(die) end, []]).

cast_mfa_throw(_Config) ->
    true = gen_rpc:cast(?MASTER, erlang, throw, ['throwme']).

cast_inexistent_node(_Config) ->
    true = gen_rpc:cast(?FAKE_NODE, os, timestamp, [], 1000).

async_call(_Config) ->
    YieldKey0 = gen_rpc:async_call(?MASTER, os, timestamp, []),
    {_Mega, _Sec, _Micro} = gen_rpc:yield(YieldKey0),
    NbYieldKey0 = gen_rpc:async_call(?MASTER, os, timestamp, []),
    {value, {_,_,_}} = gen_rpc:nb_yield(NbYieldKey0, 500),
    YieldKey = gen_rpc:async_call(?MASTER, io_lib, print, [yield_key]),
    "yield_key" = gen_rpc:yield(YieldKey),
    NbYieldKey = gen_rpc:async_call(?MASTER, io_lib, print, [nb_yield_key]),
    {value, "nb_yield_key"} = gen_rpc:nb_yield(NbYieldKey, 500).

async_call_yield_reentrant(_Config) ->
    YieldKey0 = gen_rpc:async_call(?MASTER, os, timestamp, []),
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
    NbYieldKey0 = gen_rpc:async_call(?MASTER, os, timestamp, []),
    {value, {_,_,_}} = gen_rpc:nb_yield(NbYieldKey0, 500),
    % Verify not able to reuse Key again. Key is one time use.
    timeout = gen_rpc:nb_yield(NbYieldKey0, 5),
    YieldKey = gen_rpc:async_call(?MASTER, io_lib, print, [yield_key]),
    "yield_key" = gen_rpc:yield(YieldKey),
    NbYieldKey = gen_rpc:async_call(?MASTER, io_lib, print, [nb_yield_key]),
    {value, "nb_yield_key"} = gen_rpc:nb_yield(NbYieldKey, 500).

async_call_anonymous_function(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, erlang, apply, [fun(A) -> {self(), io_lib:print(A)} end,
                                  [yield_key_anonymous_func]]),
    {_, "yield_key_anonymous_func"} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, erlang, apply,[fun(A) -> {self(), io_lib:print(A)} end,
                                    [nb_yield_key_anonymous_func]]),
    {value, {_, "nb_yield_key_anonymous_func"}} = gen_rpc:nb_yield(NBYieldKey, 500).

async_call_anonymous_undef(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, erlang, apply, [fun() -> os:timestamp_undef() end, []]),
    {value, {badrpc, {'EXIT', {undef,[{os,timestamp_undef,[],[]},_]}}}} = gen_rpc:nb_yield(NBYieldKey, 500),
    ok = ct:pal("Result [async_call_anonymous_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_undef(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, os, timestamp_undef),
    {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, os, timestamp_undef),
    {value, {badrpc, {'EXIT', {undef,[{os,timestamp_undef,_,_},_]}}}} = gen_rpc:nb_yield(NBYieldKey, 500),
    ok = ct:pal("Result [async_call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_exit(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, erlang, exit, ['die']),
    {badrpc, {'EXIT', die}} = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, erlang, exit, ['die']),
    {value, {badrpc, {'EXIT', die}}} = gen_rpc:nb_yield(NBYieldKey, 500),
    ok = ct:pal("Result [async_call_mfa_undef]: signal=EXIT Reason={os,timestamp_undef}").

async_call_mfa_throw(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, erlang, throw, ['throwXdown']),
    'throwXdown' = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, erlang, throw, ['throwXdown']),
    {value, 'throwXdown'} = gen_rpc:nb_yield(NBYieldKey, 500),
    ok = ct:pal("Result [async_call_mfa_undef]: throw Reason={throwXdown}").

async_call_yield_timeout(_Config) ->
    NBYieldKey = gen_rpc:async_call(?MASTER, timer, sleep, [100]),
    timeout = gen_rpc:nb_yield(NBYieldKey, 5),
    ok = ct:pal("Result [async_call_yield_timeout]: signal=badrpc Reason={timeout}").

async_call_nb_yield_infinity(_Config) ->
    YieldKey = gen_rpc:async_call(?MASTER, timer, sleep, [100]),
    ok = gen_rpc:yield(YieldKey),
    NBYieldKey = gen_rpc:async_call(?MASTER, timer, sleep, [100]),
    {value, ok} = gen_rpc:nb_yield(NBYieldKey, infinity),
    ok = ct:pal("Result [async_call_yield_infinity]: timer_sleep Result={ok}").

async_call_inexistent_node(_Config) ->
    YieldKey1 = gen_rpc:async_call(?FAKE_NODE, os, timestamp, []),
    {badrpc, _} = gen_rpc:yield(YieldKey1),
    YieldKey2 = gen_rpc:async_call(?FAKE_NODE, os, timestamp, []),
    {value, {badrpc, _}} = gen_rpc:nb_yield(YieldKey2, 10000).

client_inactivity_timeout(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call(?MASTER, os, timestamp),
    ok = timer:sleep(600),
    %% Lookup the client named process, shouldn't be there
    undefined = whereis(?MASTER).

server_inactivity_timeout(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call(?MASTER, os, timestamp),
    ok = timer:sleep(600),
    %% Lookup the client named process, shouldn't be there
    [] = supervisor:which_children(gen_rpc_acceptor_sup).

random_tcp_close(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call(?MASTER, os, timestamp),
    [{_,AccPid,_,_}] = supervisor:which_children(gen_rpc_acceptor_sup),
    true = erlang:exit(AccPid, normal),
    ok = timer:sleep(500), % Give some time to the supervisor to kill the children
    [] = gen_rpc:nodes(),
    [] = supervisor:which_children(gen_rpc_acceptor_sup),
    [] = supervisor:which_children(gen_rpc_client_sup).

rpc_module_whitelist(_Config) ->
    {_Mega, _Sec, _Micro} = gen_rpc:call(?MASTER, os, timestamp),
    ?MASTER = gen_rpc:call(?MASTER, erlang, node),
    {badrpc, unauthorized} = gen_rpc:call(?MASTER, application, which_applications).

rpc_module_blacklist(_Config) ->
    {badrpc, unauthorized} = gen_rpc:call(?MASTER, os, timestamp),
    {badrpc, unauthorized} = gen_rpc:call(?MASTER, erlang, node),
    abcast = gen_rpc:abcast([?MASTER], init, {stop, stop}),
    {[], [?MASTER]} = gen_rpc:sbcast([?MASTER], init, {stop, stop}),
    60000 = gen_rpc:call(?MASTER, timer, seconds, [60]).

driver_stub(_Config) ->
    ok = gen_rpc_driver:stub().

client_config_stub(_Config) ->
    ok = gen_rpc_client_config:stub().

%%% ===================================================
%%% Auxiliary functions for test cases
%%% ===================================================
%% Loop in order to receive all messages from all workers
interleaved_call_loop(Pid1, Pid2, Pid3, Num) when Num < 3 ->
    receive
        {reply, Pid1, 1, 1} ->
            ct:pal("Received proper reply from Worker 1"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        {reply, Pid2, 2, {badrpc, timeout}} ->
            ct:pal("Received proper reply from Worker 2"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        {reply, Pid3, 3, 3} ->
            ct:pal("Received proper reply from Worker 3"),
            interleaved_call_loop(Pid1, Pid2, Pid3, Num+1);
        _Else ->
            ct:pal("Received out of order reply"),
            fail
    end;
interleaved_call_loop(_, _, _, 3) ->
    ok.

%% This function will become a spawned process that performs the RPC
%% call and then returns the value of the RPC call
%% We spawn it in order to achieve parallelism and test out-of-order
%% execution of multiple RPC calls
interleaved_call_proc(Caller, Num, Timeout) ->
    Result = gen_rpc:call(?MASTER, ?MODULE, interleaved_call_executor, [Num], Timeout),
    Caller ! {reply, self(), Num, Result},
    ok.

%% This is the function that gets executed in the "remote"
%% node, sleeping 3 minus $Num seconds and returning the number
%% effectively returning a number thats inversely proportional
%% to the number of seconds the worker slept
interleaved_call_executor(Num) when is_integer(Num) ->
    %% Sleep for 3 - Num
    ok = timer:sleep((3 - Num) * 1000),
    %% Then return the number
    Num.
