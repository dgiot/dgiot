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

-module(ekka_cluster_etcd_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

-define(OPTIONS, [{server, "http://127.0.0.1:2379"},
                  {prefix, "emqxcl"},
                  {node_ttl, 60}
                 ]).

all() -> ekka_ct:all(?MODULE).

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(httpc, [non_strict, passthrough, no_history]),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = meck:unload(httpc),
    Config.

t_discover(Config) ->
    Json = <<"{\"node\": {\"nodes\": [{\"key\": \"ekkacl/n1@127.0.0.1\"}]}}">>,
    ok = meck:expect(httpc, request, fun(get, _Req, _Opts, _) -> {ok, 200, Json} end),
    {ok, ['n1@127.0.0.1']} = ekka_cluster_etcd:discover(?OPTIONS).

t_lock(Config) ->
    ok = meck:expect(httpc, request, fun(put, _Req, _Opts, _) ->
                                             {ok, 200, <<"{\"errorCode\": 0}">>}
                                     end),
    ok = ekka_cluster_etcd:lock(?OPTIONS).

t_unlock(_) ->
    ok = meck:expect(httpc, request, fun(delete, _Req, _Opts, _) ->
                                             {ok, 200, <<"{\"errorCode\": 0}">>}
                                     end),
    ok = ekka_cluster_etcd:unlock(?OPTIONS).

t_register(_) ->
    ok = meck:new(ekka_cluster_sup, [non_strict, passthrough, no_history]),
    ok = meck:expect(ekka_cluster_sup, start_child, fun(_, _) -> {ok, self()} end),
    ok = meck:expect(httpc, request, fun(put, _Req, _Opts, _) ->
                                             {ok, 200, <<"{\"errorCode\": 0}">>}
                                     end),
    ok = ekka_cluster_etcd:register(?OPTIONS),
    ok = meck:unload(ekka_cluster_sup).

t_unregister(_) ->
    ok = meck:expect(httpc, request, fun(delete, _Req, _Opts, _) ->
                                             {ok, 200, <<"{\"errorCode\": 0}">>}
                                     end),
    ok = meck:expect(ekka_cluster_sup, stop_child, fun(_) -> ok end),
    ok = ekka_cluster_etcd:unregister(?OPTIONS),
    ok = meck:unload(ekka_cluster_sup).

t_etcd_set_node_key(_) ->
    ok = meck:expect(httpc, request, fun(put, _Req, _Opts, _) ->
                                             {ok, 200, <<"{\"errorCode\": 0}">>}
                                     end),
    {ok, #{<<"errorCode">> := 0}} = ekka_cluster_etcd:etcd_set_node_key(?OPTIONS).

