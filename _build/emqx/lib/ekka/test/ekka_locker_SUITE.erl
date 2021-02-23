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

-module(ekka_locker_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("ekka.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, locker_server).

all() -> ekka_ct:all(?MODULE).

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(ekka_membership, [non_strict, passthrough, no_history]),
    {ok, _Locker} = ekka_locker:start_link(?SERVER),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = ekka_locker:stop(locker_server),
    ok = meck:unload(ekka_membership),
    Config.

t_acquire_release_local(_) ->
    Node = node(),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource)),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource)).

t_acquire_release_leader(_) ->
    Node = node(),
    ok = meck:expect(ekka_membership, leader, fun() -> Node end),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, leader)),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, leader)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, leader)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, leader)).

t_acquire_release_quorum(_) ->
    Node = node(),
    ok = meck:expect(ekka_membership, ring, fun(_) -> [#member{node = Node, status = up}] end),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, quorum)),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, quorum)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, quorum)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, quorum)).

t_acquire_release_all(_) ->
    Node = node(),
    ok = meck:expect(ekka_membership, nodelist, fun(_) -> [Node] end),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, all)),
    ?assertEqual({true, [Node]}, ekka_locker:acquire(?SERVER, resource, all)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, all)),
    ?assertEqual({true, [Node]}, ekka_locker:release(?SERVER, resource, all)).

