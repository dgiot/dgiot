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

-module(ekka_autoheal_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

init_per_suite(Config) ->
    ok = init_app_envs(node()),
    Envs = application:get_all_env(ekka),
    ok = ekka:start(),
    Config.

end_per_suite(_Config) ->
    ok = ekka:stop(),
    ekka_mnesia:ensure_stopped().

t_enabled(_Config) ->
    {true, 2000} = ekka_autoheal:enabled().

t_autoheal(_Config) ->
    [N1,N2,N3] = Nodes = lists:map(fun start_slave_node/1, [n1,n2,n3]),
    try
        %% Create cluster
        ok = rpc:call(N2, ekka, join, [N1]),
        ok = rpc:call(N3, ekka, join, [N1]),
        %% Simulate netsplit
        true = rpc:cast(N3, net_kernel, disconnect, [N1]),
        true = rpc:cast(N3, net_kernel, disconnect, [N2]),
        ok = timer:sleep(1000),
        %% SplitView: {[N1,N2], [N3]}
        [N1,N2] = rpc:call(N1, ekka, info, [running_nodes]),
        [N3] = rpc:call(N1, ekka, info, [stopped_nodes]),
        [N1,N2] = rpc:call(N2, ekka, info, [running_nodes]),
        [N3] = rpc:call(N2, ekka, info, [stopped_nodes]),
        [N3] = rpc:call(N3, ekka, info, [running_nodes]),
        [N1,N2] = rpc:call(N3, ekka, info, [stopped_nodes]),
        true = rpc:cast(N3, net_kernel, connect_node, [N1]),
        true = rpc:cast(N3, net_kernel, connect_node, [N2]),
        %% Wait for autoheal
        ok = timer:sleep(6000),
        [N1,N2,N3] = rpc:call(N1, ekka, info, [running_nodes]),
        [N1,N2,N3] = rpc:call(N2, ekka, info, [running_nodes]),
        [N1,N2,N3] = rpc:call(N3, ekka, info, [running_nodes]),
        rpc:call(N1, ekka, leave, []),
        rpc:call(N2, ekka, leave, []),
        rpc:call(N3, ekka, leave, [])
    after
        lists:foreach(fun ekka_ct:stop_slave/1, Nodes)
    end.

start_slave_node(Name) ->
    Node = ekka_ct:start_slave(node, Name),
    ok = init_app_envs(Node),
    ok = rpc:call(Node, ekka, start, []),
    ok = ekka_ct:wait_running(Node),
    true = ekka:is_running(Node, ekka),
    Node.

init_app_envs(N) ->
    _ = rpc:call(N, application, load, [ekka]),
    Config = [{ekka, [{cluster_name, ekka},
                      {cluster_enable, true},
                      {cluster_autoheal, 2000},
                      {cluster_discovery, {manual, []}}
                     ]
              }],
    rpc:call(N, application, set_env, [Config]).

