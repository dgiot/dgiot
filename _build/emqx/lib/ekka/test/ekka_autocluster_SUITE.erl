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

-module(ekka_autocluster_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

-define(DNS_OPTIONS, [{name, "localhost"},
                      {app, "ct"}
                     ]).

-define(ETCD_OPTIONS, [{server, ["http://127.0.0.1:2379"]},
                       {prefix, "cl"},
                       {node_ttl, 60}
                      ]).

-define(K8S_OPTIONS, [{apiserver, "http://127.0.0.1:6000"},
                      {namespace, "default"},
                      {service_name, "ekka"},
                      {address_type, ip},
                      {app_name, "ct"},
                      {suffix, ""}
                     ]).

-define(MCAST_OPTIONS, [{addr, {239,192,0,1}},
                        {ports, [5000,5001,5002]},
                        {iface, {0,0,0,0}},
                        {ttl, 255},
                        {loop, true}
                       ]).

all() -> ekka_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    _ = inets:start(),
    ok = application:set_env(ekka, cluster_name, ekka),
    ok = application:set_env(ekka, cluster_enable, true),
    ok = ekka:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(ekka),
    ekka_mnesia:ensure_stopped().

%%--------------------------------------------------------------------
%% Autocluster via 'static' strategy

t_autocluster_via_static(Config) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        ok = set_app_env(N1, {static, [{seeds, [node()]}]}),
        rpc:call(N1, ekka, autocluster, []),
        ok = wait_for_node(N1),
        ekka:force_leave(N1)
    after
        ok = ekka_ct:stop_slave(N1)
    end.

%%--------------------------------------------------------------------
%% Autocluster via 'dns' strategy

t_autocluster_via_dns(_Config) ->
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        ok = set_app_env(N1, {dns, ?DNS_OPTIONS}),
        rpc:call(N1, ekka, autocluster, []),
        ok = wait_for_node(N1),
        ok = ekka:force_leave(N1)
    after
        ok = ekka_ct:stop_slave(N1)
    end.

%%--------------------------------------------------------------------
%% Autocluster via 'etcd' strategy

t_autocluster_via_etcd(_Config) ->
    {ok, _} = start_etcd_server(2379),
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        ok = set_app_env(N1, {etcd, ?ETCD_OPTIONS}),
        _ = rpc:call(N1, ekka, autocluster, []),
        ok = wait_for_node(N1),
        ok = ekka:force_leave(N1)
    after
        ok = stop_etcd_server(2379),
        ok = ekka_ct:stop_slave(N1)
    end.

%%--------------------------------------------------------------------
%% Autocluster via 'k8s' strategy

t_autocluster_via_k8s(_Config) ->
    {ok, _} = start_k8sapi_server(6000),
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        ok = set_app_env(N1, {k8s, ?K8S_OPTIONS}),
        rpc:call(N1, ekka, autocluster, []),
        ok = wait_for_node(N1),
        ok = ekka:force_leave(N1)
    after
        ok = stop_k8sapi_server(6000),
        ok = ekka_ct:stop_slave(N1)
    end.

%%--------------------------------------------------------------------
%% Autocluster via 'mcast' strategy

t_autocluster_via_mcast(_Config) ->
    ok = reboot_ekka_with_mcast_env(),
    ok = timer:sleep(1000),
    N1 = ekka_ct:start_slave(ekka, n1),
    try
        ok = ekka_ct:wait_running(N1),
        ok = set_app_env(N1, {mcast, ?MCAST_OPTIONS}),
        rpc:call(N1, ekka, autocluster, []),
        ok = wait_for_node(N1),
        ok = ekka:force_leave(N1)
    after
        ok = ekka_ct:stop_slave(N1)
    end.

reboot_ekka_with_mcast_env() ->
    ok = ekka:stop(),
    ok = set_app_env(node(), {mcast, ?MCAST_OPTIONS}),
    ok = ekka:start().

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

set_app_env(Node, Discovery) ->
    Config = [{ekka, [{cluster_name, ekka},
                      {cluster_enable, true},
                      {cluster_discovery, Discovery}
                     ]
              }],
    rpc:call(Node, application, set_env, [Config]).

wait_for_node(Node) ->
    wait_for_node(Node, 20).
wait_for_node(Node, 0) ->
    error({autocluster_timeout, Node});
wait_for_node(Node, Cnt) ->
    ok = timer:sleep(500),
    case lists:member(Node, ekka:info(running_nodes)) of
        true -> ok;
        false -> wait_for_node(Node, Cnt-1)
    end.

start_etcd_server(Port) ->
    start_http_server(Port, mod_etcd).

start_k8sapi_server(Port) ->
    start_http_server(Port, mod_k8s_api).

start_http_server(Port, Mod) ->
    inets:start(httpd, [{port, Port},
                        {server_name, "etcd"},
                        {server_root, "."},
                        {document_root, "."},
                        {bind_address, "localhost"},
                        {modules, [Mod]}
                       ]).

stop_etcd_server(Port) ->
    stop_http_server(Port).

stop_k8sapi_server(Port) ->
    stop_http_server(Port).

stop_http_server(Port) ->
    inets:stop(httpd, {{127,0,0,1}, Port}).

