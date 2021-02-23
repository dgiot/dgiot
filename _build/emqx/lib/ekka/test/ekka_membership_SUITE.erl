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

-module(ekka_membership_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("ekka.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(ekka_mnesia, [non_strict, passthrough, no_history]),
    ok = meck:expect(ekka_mnesia, cluster_status, fun(_) -> running end),
    {ok, _} = ekka_membership:start_link(),
    ok = init_membership(3),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = ekka_membership:stop(),
    ok = meck:unload(ekka_mnesia),
    Config.

t_lookup_member(_) ->
    false = ekka_membership:lookup_member('node@127.0.0.1'),
    #member{node = 'n1@127.0.0.1', status = up}
        = ekka_membership:lookup_member('n1@127.0.0.1').

t_coordinator(_) ->
    ?assertEqual(node(), ekka_membership:coordinator()),
    Nodes = ['n1@127.0.0.1', 'n2@127.0.0.1', 'n3@127.0.0.1'],
    ?assertEqual('n1@127.0.0.1', ekka_membership:coordinator(Nodes)).

t_node_down_up(_) ->
    ok = meck:expect(ekka_mnesia, is_node_in_cluster, fun(_) -> true end),
    ok = ekka_membership:node_down('n2@127.0.0.1'),
    ok = timer:sleep(100),
    #member{status = down} = ekka_membership:lookup_member('n2@127.0.0.1'),
    ok = ekka_membership:node_up('n2@127.0.0.1'),
    ok = timer:sleep(100),
    #member{status = up} = ekka_membership:lookup_member('n2@127.0.0.1').

t_mnesia_down_up(_) ->
    ok = ekka_membership:mnesia_down('n2@127.0.0.1'),
    ok = timer:sleep(100),
    #member{mnesia = stopped} = ekka_membership:lookup_member('n2@127.0.0.1'),
    ok = ekka_membership:mnesia_up('n2@127.0.0.1'),
    ok = timer:sleep(100),
    #member{status = up, mnesia = running} = ekka_membership:lookup_member('n2@127.0.0.1').

t_partition_occurred(_) ->
    ok = ekka_membership:partition_occurred('n2@127.0.0.1').

t_partition_healed(_) ->
    ok = ekka_membership:partition_healed(['n2@127.0.0.1']).

t_announce(_) ->
    ok = ekka_membership:announce(leave).

t_leader(_) ->
    ?assertEqual(node(), ekka_membership:leader()).

t_is_all_alive(_) ->
    ?assert(ekka_membership:is_all_alive()).

t_members(_) ->
    ?assertEqual(4, length(ekka_membership:members())).

t_nodelist(_) ->
    Nodes = lists:sort([node(),
                        'n1@127.0.0.1',
                        'n2@127.0.0.1',
                        'n3@127.0.0.1'
                       ]),
    ?assertEqual(Nodes, lists:sort(ekka_membership:nodelist())).

t_is_member(_) ->
    ?assert(ekka_membership:is_member('n1@127.0.0.1')),
    ?assert(ekka_membership:is_member('n2@127.0.0.1')),
    ?assert(ekka_membership:is_member('n3@127.0.0.1')).

t_local_member(_) ->
    #member{node = Node} = ekka_membership:local_member(),
    ?assertEqual(node(), Node).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

init_membership(N) ->
    lists:foreach(
      fun(Member) ->
              ok = ekka_membership:pong(node(), Member)
      end, lists:map(fun member/1, lists:seq(1, N))),
    ekka_membership:announce(join).

member(I) ->
    Node = list_to_atom("n" ++ integer_to_list(I) ++ "@127.0.0.1"),
    #member{node   = Node,
            addr   = {{127,0,0,1}, 5000 + I},
            guid   = ekka_guid:gen(),
            hash   = 1000 * I,
            status = up,
            mnesia = running,
            ltime  = erlang:timestamp()
           }.

