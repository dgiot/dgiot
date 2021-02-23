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

-module(ekka_node_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> ekka_ct:all(?MODULE).

t_is_aliving(_) ->
    false = ekka_node:is_aliving('x@127.0.0.1'),
    true = ekka_node:is_aliving(node()).

t_parse_name(_) ->
    ?assertEqual('a@127.0.0.1', ekka_node:parse_name("a@127.0.0.1")),
    [_Node, Host] = string:tokens(atom_to_list(node()), "@"),
    ?assertEqual(list_to_atom("b@" ++ Host), ekka_node:parse_name("b")).

t_is_running_1(_) ->
    false = ekka_node:is_running(ekka),
    false = ekka_node:is_running('x@127.0.0.1', ekka).

t_is_running_2(_) ->
    ok = ekka:start(),
    true = ekka_node:is_running(ekka),
    true = ekka_node:is_running(node(), ekka),
    ok = ekka:stop(),
    ok = ekka_mnesia:ensure_stopped().

