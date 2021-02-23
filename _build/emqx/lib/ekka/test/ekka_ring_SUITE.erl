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

-module(ekka_ring_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("ekka.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

t_find_node(_) ->
    ?assertEqual(n1, ekka_ring:find_node(key, ring(5))).

t_find_nodes(_) ->
    ?assertEqual([n1,n5,n3], ekka_ring:find_nodes(key, ring(5))),
    ?assertEqual([n1,n5,n3], ekka_ring:find_nodes(key, 3, ring(5))).

ring(N) ->
    Node = fun(I) -> list_to_atom("n" ++ integer_to_list(I)) end,
    Hash = fun(I) -> erlang:phash2(I, 4294967295) end,
    lists:keysort(#member.hash, [#member{node = Node(I),
                                         hash = Hash(I)
                                        } || I <- lists:seq(1, N)]).

