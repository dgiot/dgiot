%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc Workarounds for various cover deficiencies.
-module(dgiot_cover).
-author("johnliu").
-export([get_beam/1, get_abstract_code/1,
         get_clauses/2, clause_lookup_table/1]).
-export([clause_lookup_table/2]).

%% Internal

get_beam(Module) ->
    {Module, Beam, _Path} = code:get_object_code(Module),
    Beam.

get_abstract_code(Beam) ->
    {ok, {_Module,
          [{abstract_code,
            {raw_abstract_v1, L}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    L.

get_clauses(Function, Code) ->
    [L] = [Clauses || {function, _, FName, _, Clauses}
                          <- Code, FName =:= Function],
    L.

clause_lookup_table(Module, Function) ->
    clause_lookup_table(
      get_clauses(Function,
                  get_abstract_code(get_beam(Module)))).

clause_lookup_table(Clauses) ->
    lists:foldr(fun clause_fold/2, [], Clauses).

clause_fold({clause, _,
             [InTerm],
             _Guards=[],
             [OutTerm]},
            Acc) ->
    try [{erl_parse:normalise(InTerm), erl_parse:normalise(OutTerm)} | Acc]
    catch error:_ -> Acc
    end;
clause_fold(_, Acc) ->
    Acc.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
foo_table(a) -> b;
foo_table("a") -> <<"b">>;
foo_table(123) -> {4, 3, 2};
foo_table([list]) -> [];
foo_table([list1, list2]) -> [list1, list2, list3];
foo_table(ignored) -> some, code, ignored;
foo_table(Var) -> Var.

foo_table_test() ->
    T = clause_lookup_table(?MODULE, foo_table),
    [?assertEqual(V, foo_table(K)) || {K, V} <- T].

clause_lookup_table_test() ->
    ?assertEqual(b, foo_table(a)),
    ?assertEqual(ignored, foo_table(ignored)),
    ?assertEqual('Var', foo_table('Var')),
    ?assertEqual(
       [{a, b},
        {"a", <<"b">>},
        {123, {4, 3, 2}},
        {[list], []},
        {[list1, list2], [list1, list2, list3]}],
       clause_lookup_table(?MODULE, foo_table)).

-endif.
