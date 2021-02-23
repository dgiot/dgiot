%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(ekka_cluster_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

init_per_suite(Config) ->
    ok = application:set_env(ekka, cluster_name, ekka),
    ok = application:set_env(ekka, cluster_discovery, {manual, []}),
    ok = ekka:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(ekka),
    ekka_mnesia:ensure_stopped().

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_join_leave(_) ->
    N0 = node(),
    N1 = ekka_ct:start_slave(ekka, n1),
    N2 = ekka_ct:start_slave(ekka, n2),
    try
        ekka_ct:wait_running(N1),
        ekka_ct:wait_running(N2),
        true = ekka:is_running(N1, ekka),
        true = ekka:is_running(N2, ekka),
        ok = rpc:call(N1, ekka_cluster, join, [N0]),
        ok = rpc:call(N2, ekka_cluster, join, [N0]),
        [N0, N1, N2] = ekka_cluster:info(running_nodes),
        ok = rpc:call(N1, ekka_cluster, leave, []),
        ok = rpc:call(N2, ekka_cluster, leave, []),
        [N0] = ekka_cluster:info(running_nodes)
    after
        ok = ekka_ct:stop_slave(N1),
        ok = ekka_ct:stop_slave(N2)
    end.

t_force_leave(_) ->
    N0 = node(),
    N1 = ekka_ct:start_slave(ekka, n1),
    N2 = ekka_ct:start_slave(ekka, n2),
    try
        ok = ekka_ct:wait_running(N1),
        ok = ekka_ct:wait_running(N2),
        true = ekka:is_running(N1, ekka),
        true = ekka:is_running(N2, ekka),
        ok = rpc:call(N1, ekka_cluster, join, [N0]),
        ok = rpc:call(N2, ekka_cluster, join, [N1]),
        [N0, N1, N2] = ekka_cluster:info(running_nodes),
        ok = ekka_cluster:force_leave(N1),
        ok = ekka_cluster:force_leave(N2),
        [N0] = ekka_cluster:info(running_nodes)
    after
        ok = ekka_ct:stop_slave(N1),
        ok = ekka_ct:stop_slave(N2)
    end.

t_heal_shutdown(_) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    ok = rpc:call(N1, ekka_cluster, heal, [shutdown]),
    ok = ekka_ct:stop_slave(N1).

t_heal_reboot(_) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    ok = rpc:call(N1, ekka_cluster, heal, [reboot]),
    ok = ekka_ct:stop_slave(N1).

t_prepare(_) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    ok = rpc:call(N1, ekka_cluster, prepare, [join]),
    ok = ekka_ct:stop_slave(N1).

t_reboot(_) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    ok = rpc:call(N1, ekka_cluster, reboot, []),
    ok = ekka_ct:stop_slave(N1).

t_status(_) ->
    N0 = node(),
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        true = ekka:is_running(N1, ekka),
        ok = rpc:call(N1, ekka_cluster, join, [N0]),
        running = ekka_cluster:status(N1),
        ok = ekka_cluster:force_leave(N1),
        false = ekka_cluster:status(N1)
    after
        ok = ekka_ct:stop_slave(N1)
    end.

