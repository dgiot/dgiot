%% -------------------------------------------------------------------
%%
%% cuttlefish_effective: handles generating the effective configuration
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(cuttlefish_effective).

-define(FMT(F,A), lists:flatten(io_lib:format(F,A))).

-export([build/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-spec build(cuttlefish_conf:conf(), cuttlefish_schema:schema(), [proplists:property()]) -> [string()].
build(Conf, {_Translations, Mappings, _Validators} = _Schema, AdvConfig) ->
    EffectiveConfig = lists:reverse(lists:sort(cuttlefish_generator:add_defaults(Conf, Mappings))),
    %% EffectiveConfig is a list of { [string()], term() }

    %% Returns the list of cuttlefish variables that have been overridden in advanced.config
    KeysToHateOn = process_advanced(Mappings, AdvConfig),

    EffectiveOutput = lists:foldl(
        fun({Var, Value}, Acc) ->
            Variable = string:join(Var, "."),

            IsHater =
                lists:any(
                    fun(X) ->
                        cuttlefish_variable:is_fuzzy_match(Var, X)
                    end,
                    KeysToHateOn
                    ),

            Line = try ?FMT("~s = ~s", [Variable, Value]) of
                X -> X
            catch
                _:_ ->
                    %% I hate that I had to do this, 'cause you know...
                    %% Erlang and Strings, but actually this is ok because
                    %% sometimes there are going to be weird tuply things
                    %% in here, so always good to fall back on ~p.
                    %% honestly, I think this try should be built into io:format
                    ?FMT("~s = ~p", [Variable, Value])
            end,
            case IsHater of
                true ->
                    [?FMT("## ~s was overridden in advanced.config", [Variable]),
                     "## " ++ Line] ++ Acc;
                _ -> [Line | Acc]
            end
        end,
        [],
        EffectiveConfig
    ),

    case AdvConfig of
        [] -> EffectiveOutput;
        _ ->
            EffectiveOutput ++
            [
             "## The following advanced.config was used in generating the ",
             "## configuration and may have overridden some options that were ",
             "## commented out above."
            ]
            ++ advanced_as_comment(AdvConfig)
    end.

advanced_as_comment(AdvConfig) ->
    Str = lists:flatten(io_lib:format("~p", [AdvConfig])),
    [ "## " ++ L || L <- string:tokens(Str, "$\n")].

%% @doc checks a mapping's "mapping" is in the set of kvc paths in
%% the provided advanced.config
-spec process_advanced(
        [cuttlefish_mapping:mapping()],
        [proplists:property()]) -> [cuttlefish_variable:variable()].
process_advanced(Mappings, AdvancedConfig) ->
    AdvKeys = proplist_to_kvcpaths(AdvancedConfig),
    lists:foldl(
        fun(M, Acc) ->
            case lists:member(cuttlefish_mapping:mapping(M), AdvKeys) of
                true ->
                    [cuttlefish_mapping:variable(M)|Acc];
                _ -> Acc
            end
        end,
        [],
        Mappings).

%% @doc returns a list of kvcesque paths that represent the structure
%% of a proplist of proplists of proplists etc...
%% e.g. [{parent, [{child1, [{grandchild1, _}]}, {child2, _}]}] ->
%%      ["parent.child1.grandchild1", "parent.child2"]
-spec proplist_to_kvcpaths([proplists:property()]) -> [string()].
proplist_to_kvcpaths(Proplist) ->
    proplist_to_kvcpaths("", Proplist).
-spec proplist_to_kvcpaths(string(), [proplists:property()]) -> [string()].
proplist_to_kvcpaths(Prefix, Proplist) ->
    %% Handles the base case, without this, all keys would start with "."
    NewPrefix = case Prefix of
        "" -> "";
        _ -> Prefix ++ "."
    end,
    lists:foldl(fun(K, Acc) ->
            KeyedPrefix = NewPrefix ++ canonicalize_key(K),
            R = proplist_to_kvcpaths(
                KeyedPrefix,
                proplists:get_value(K, Proplist)),
            case R of
                [] ->
                    [KeyedPrefix|Acc];
                _ ->
                    Acc ++ R
            end
        end,
        [],
        keys_if_you_got_em(Proplist)
    ).

%% So this is gross, but is the simplest scheme for determining the
%% type of data coming into this function. It doesn't really
%% matter how we handle non-atoms because cuttlefish only creates 
%% proplists with atoms as the keynames.
-spec canonicalize_key(atom() | list() | binary()) -> string().
canonicalize_key(K) when is_atom(K) ->
    atom_to_list(K);
canonicalize_key(K) when is_list(K) ->
    "\"" ++ K ++ "\"";
canonicalize_key(K) when is_binary(K) ->
    "<<\""++binary_to_list(K)++"\">>".


-spec keys_if_you_got_em([proplists:property()]) -> [term()].
keys_if_you_got_em(Proplist) when is_list(Proplist) ->
    proplists:get_keys(Proplist);
keys_if_you_got_em(_) -> [].

-ifdef(TEST).

%% This is the comprehensive test of all functionality of this module
%% working together in perfect harmony
probably_the_most_important_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "namespace.var1", "app.setting1", []}
        ),
        cuttlefish_mapping:parse(
            {mapping, "namespace.2.$sub", "app.setting2", []}
        ),
        cuttlefish_mapping:parse(
            {mapping, "namespace.var3", "app.setting3", []}
        ),
        cuttlefish_mapping:parse(
            {mapping, "namespace.4.$sub", "app.setting4", []}
        )
    ],
    Conf = [
        {["namespace", "var1"], "x"},
        {["namespace", "2", "1"], "x"},
        {["namespace", "2", "2"], "x"},
        {["namespace", "2", "3"], "x"},
        {["namespace", "var3"], "y"},
        {["namespace", "4", "1"], "y"},
        {["namespace", "4", "2"], "y"},
        {["namespace", "4", "3"], "y"}
    ],
    AdvConfig = [{app, [{setting3, "z"}, {setting4, "zz"}]}],

    Effective = build(Conf, {[], Mappings, []}, AdvConfig),
    ?assertEqual(16, length(Effective)),

    %% Remember, this output is sorted by variable, even if there's a comment

    ?assertEqual("namespace.2.1 = x", lists:nth(1, Effective)),
    ?assertEqual("namespace.2.2 = x", lists:nth(2, Effective)),
    ?assertEqual("namespace.2.3 = x", lists:nth(3, Effective)),
    ?assertEqual("## namespace.4.1 was overridden in advanced.config", lists:nth(4, Effective)),
    ?assertEqual("## namespace.4.1 = y", lists:nth(5, Effective)),
    ?assertEqual("## namespace.4.2 was overridden in advanced.config", lists:nth(6, Effective)),
    ?assertEqual("## namespace.4.2 = y", lists:nth(7, Effective)),
    ?assertEqual("## namespace.4.3 was overridden in advanced.config", lists:nth(8, Effective)),
    ?assertEqual("## namespace.4.3 = y", lists:nth(9, Effective)),
    ?assertEqual("namespace.var1 = x", lists:nth(10, Effective)),
    ?assertEqual("## namespace.var3 was overridden in advanced.config", lists:nth(11, Effective)),
    ?assertEqual("## namespace.var3 = y", lists:nth(12, Effective)),
    ?assertEqual("## The following advanced.config was used in generating the ", lists:nth(13, Effective)),
    ?assertEqual("## configuration and may have overridden some options that were ", lists:nth(14, Effective)),
    ?assertEqual("## commented out above.", lists:nth(15, Effective)),
    ?assertEqual("## [{app,[{setting3,\"z\"},{setting4,\"zz\"}]}]", lists:nth(16, Effective)),
    ok.

process_advanced_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "thing.1", "a.b.c", []}
        )
    ],
    AdvConfig = [{a, [{b, [{c, ""}, {d, ""}]}]}],
    KeysToWatch = process_advanced(Mappings, AdvConfig),
    ?assertEqual([["thing", "1"]], KeysToWatch),
    ok.

build_with_sub_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "a.$whatev.thing", "a.b.c", []}
        )
    ],
    AdvConfig = [{a, [{b, [{c, ""}, {d, ""}]}]}],
    Conf = [
        {["a", "1", "thing"], "x"},
        {["a", "2", "thing"], "x"},
        {["a", "3", "thing"], "x"}
    ],

    Effective = build(Conf, {[], Mappings, []}, AdvConfig),

    ?assertEqual(10, length(Effective)),
    ?assertEqual("## a.1.thing was overridden in advanced.config", lists:nth(1, Effective)),
    ?assertEqual("## a.1.thing = x", lists:nth(2, Effective)),
    ?assertEqual("## a.2.thing was overridden in advanced.config", lists:nth(3, Effective)),
    ?assertEqual("## a.2.thing = x", lists:nth(4, Effective)),
    ?assertEqual("## a.3.thing was overridden in advanced.config", lists:nth(5, Effective)),
    ?assertEqual("## a.3.thing = x", lists:nth(6, Effective)),
    ?assertEqual("## The following advanced.config was used in generating the ", lists:nth(7, Effective)),
    ?assertEqual("## configuration and may have overridden some options that were ", lists:nth(8, Effective)),
    ?assertEqual("## commented out above.", lists:nth(9, Effective)),
    ?assertEqual("## [{a,[{b,[{c,[]},{d,[]}]}]}]", lists:nth(10, Effective)),
    ok.

proplist_to_kvcpath_test() ->
    Proplist = [{a, [
                    {b, [
                        {c, "x"}
                        ]},
                    {d, [
                        {e, "y"}
                        ]},
                    {f , "z"}
                    ]
                },
                {g, "q"}],
    Paths = proplist_to_kvcpaths(Proplist),
    ?assertEqual(sets:from_list([
        "a.b.c",
        "a.d.e",
        "a.f",
        "g"
    ]), sets:from_list(Paths)),
    ok.

proplists_to_kvcpath_riak_core_test() ->
    Proplist = [{riak_core,[
        {ring_creation_size,128},
        {cluster_mgr, {"127.0.0.1", 9080 } }
    ]}],
    Paths = proplist_to_kvcpaths(Proplist),
    ?assertEqual([
        "riak_core.ring_creation_size",
        "riak_core.cluster_mgr"
    ], Paths),
    ok.

-endif.
