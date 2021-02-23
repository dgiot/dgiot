%% -------------------------------------------------------------------
%%
%% cuttlefish_mapping: models a single mapping
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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
-module(cuttlefish_mapping).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(mapping, {
        variable::cuttlefish_variable:variable(),
        mapping::string(),
        default::term(),
        commented::term(),
        datatype = [string] :: cuttlefish_datatypes:datatype_list(),
        level = basic :: basic | intermediate | advanced,
        doc = [] :: list(),
        include_default = undefined :: string() | undefined,
        new_conf_value = undefined :: string() | undefined,
        validators = [] :: [string()],
        is_merge = false :: boolean(),
        see = [] :: [cuttlefish_variable:variable()],
        hidden = false :: boolean()
    }).

-type mapping() :: #mapping{}.
-type raw_mapping() :: {mapping, string(), string(), [proplists:property()]}.
-export_type([mapping/0]).

-export([
    parse/1,
    parse_and_merge/2,
    is_mapping/1,
    variable/1,
    is_fuzzy_variable/1,
    mapping/1,
    default/1,
    has_default/1,
    commented/1,
    datatype/1,
    level/1,
    hidden/1,
    doc/1,
    see/1,
    include_default/1,
    new_conf_value/1,
    replace/2,
    validators/1,
    validators/2,
    remove_all_but_first/2
    ]).

-spec parse(raw_mapping()) -> mapping() | cuttlefish_error:error().
parse({mapping, Variable, Mapping, Proplist}) ->

    Datatype = case proplists:get_value(datatype, Proplist, string) of
        {enum, Enums} ->
            AtomEnums = [ begin
                case is_list(E) of
                    true -> list_to_atom(E);
                    _ -> E
                end
            end || E <- Enums ],
            [{enum, AtomEnums}];
        L when is_list(L) ->
            case cuttlefish_datatypes:is_valid_list(L) of
                true -> L;
                _ -> {error, {mapping_types, L}}
            end;
        D -> [D]
    end,

    case Datatype of
        {error, _} ->
            Datatype;
        _ ->
            #mapping{
                variable = cuttlefish_variable:tokenize(Variable),
                default = proplists:get_value(default, Proplist),
                commented = proplists:get_value(commented, Proplist),
                mapping = Mapping,
                level = proplists:get_value(level, Proplist, basic),
                datatype = Datatype,
                doc = proplists:get_value(doc, Proplist, []),
                see = proplists:get_value(see, Proplist, []),
                include_default = proplists:get_value(include_default, Proplist),
                new_conf_value = proplists:get_value(new_conf_value, Proplist),
                validators = proplists:get_value(validators, Proplist, []),
                hidden = proplists:get_value(hidden, Proplist, false)
            }
    end;
parse(X) ->
    {error, {mapping_parse, X}}.

%% If this mapping exists, do something.
%% That something is usually a simple replace, unless the proplist in
%% the raw mapping contains the atom 'merge'.
%%
%% This fuction assumes it's run as part of a foldl over new schema elements
%% in which case, there's only ever one instance of a key in the list
%% so keyreplace works fine.
-spec parse_and_merge(
    raw_mapping(), [mapping()]) -> [mapping()].
parse_and_merge({mapping, Variable, _Mapping, Props} = MappingSource, Mappings) ->
    Var = cuttlefish_variable:tokenize(Variable),
    case lists:keyfind(Var, #mapping.variable, Mappings) of
        false ->
            [ parse(MappingSource) | Mappings];
        OldMapping ->
            MaybeMergedMapping = case proplists:is_defined(merge, Props) of
                true ->
                    merge(MappingSource, OldMapping);
                _ ->
                    parse(MappingSource)
            end,
            lists:keyreplace(Var, #mapping.variable, Mappings, MaybeMergedMapping)
    end.

-spec merge(raw_mapping(), mapping()) -> mapping().
merge(NewMappingSource, OldMapping) ->
    MergeMapping = parse(NewMappingSource),
    #mapping{
        variable = variable(MergeMapping),
        mapping = mapping(MergeMapping),
        default = choose(default, NewMappingSource, MergeMapping, OldMapping),
        commented = choose(commented, NewMappingSource, MergeMapping, OldMapping),
        datatype = choose(datatype, NewMappingSource, MergeMapping, OldMapping),
        level = choose(level, NewMappingSource, MergeMapping, OldMapping),
        doc = choose(doc, NewMappingSource, MergeMapping, OldMapping),
        include_default = choose(include_default, NewMappingSource, MergeMapping, OldMapping),
        new_conf_value = choose(include_default, NewMappingSource, MergeMapping, OldMapping),
        validators = choose(validators, NewMappingSource, MergeMapping, OldMapping),
        see = choose(see, NewMappingSource, MergeMapping, OldMapping),
        hidden = choose(hidden, NewMappingSource, MergeMapping, OldMapping)
    }.

choose(Field, {_, _, _, PreParseMergeProps}, MergeMapping, OldMapping) ->
    Which = case {Field,
                  proplists:is_defined(Field, PreParseMergeProps),
                  proplists:get_value(Field, PreParseMergeProps)} of
                {see, _, []} -> old;
                {doc, _, []} -> old;
                {_, true, _} -> new;
                _ -> old
    end,
    case Which of
        new ->
            ?MODULE:Field(MergeMapping);
        old ->
            ?MODULE:Field(OldMapping)
    end.

-spec is_mapping(any()) -> boolean().
is_mapping(M) ->
    is_tuple(M) andalso element(1, M) =:= mapping.

-spec variable(mapping()) -> [string()].
variable(M) -> M#mapping.variable.

-spec is_fuzzy_variable(mapping()) -> boolean().
is_fuzzy_variable(#mapping{variable=VariableDef}) ->
    lists:any(fun(X) -> hd(X) =:= $$ end, VariableDef).

-spec mapping(mapping()) -> string().
mapping(M) -> M#mapping.mapping.

-spec default(mapping()) -> term().
default(M) -> M#mapping.default.

-spec has_default(mapping()) -> boolean().
has_default(MappingRecord) ->
    default(MappingRecord) =/= undefined.

-spec commented(mapping()) -> term().
commented(M)        -> M#mapping.commented.

-spec datatype(mapping()) -> cuttlefish_datatypes:datatype_list().
datatype(M) -> M#mapping.datatype.

-spec level(mapping()) -> basic | intermediate | advanced.
level(M) -> M#mapping.level.

-spec hidden(mapping()) -> boolean().
hidden(M) -> M#mapping.hidden.

-spec doc(mapping()) -> [string()].
doc(M) -> M#mapping.doc.

-spec see(mapping()) -> [cuttlefish_variable:variable()].
see(M) -> M#mapping.see.

-spec include_default(mapping()) -> string() | undefined.
include_default(M) -> M#mapping.include_default.

-spec new_conf_value(mapping()) -> string() | undefined.
new_conf_value(M) -> M#mapping.new_conf_value.

-spec validators(mapping()) -> [string()].
validators(M) -> M#mapping.validators.

-spec validators(mapping(), [cuttlefish_validator:validator()]) -> [cuttlefish_validator:validator()].
validators(M, Validators) ->
    lists:foldr(fun(VName, Vs) ->
                        case lists:keyfind(VName, 2, Validators) of
                            false -> Vs;
                            V -> [V|Vs]
                        end
                end, [], M#mapping.validators).

-spec replace(mapping(), [mapping()]) -> [mapping()].
replace(Mapping, ListOfMappings) ->
    Exists = lists:keymember(variable(Mapping), #mapping.variable, ListOfMappings),
    case Exists of
        true ->
            lists:keyreplace(variable(Mapping), #mapping.variable, ListOfMappings, Mapping);
        _ ->
            [Mapping | ListOfMappings]
    end.

-spec remove_all_but_first(string(), [mapping()]) -> [mapping()].
remove_all_but_first(MappingName, Mappings) ->
    lists:foldr(
        fun(#mapping{mapping=MN}=M, Acc) when MN =:= MappingName->
                [M|lists:keydelete(MN, #mapping.mapping, Acc)];
            (M, Acc) ->
                [M|Acc]
        end, [], Mappings).

-ifdef(TEST).

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

mapping_test() ->

    SampleMapping = {
        mapping,
        "conf.key",
        "erlang.key",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "config_file_val"},
            {doc, ["documentation", "for feature"]},
            {validators, ["valid.the.impailer"]},
            hidden
        ]
    },

    Record = parse(SampleMapping),

    ?assertEqual(["conf","key"], Record#mapping.variable),
    ?assertEqual("default value", Record#mapping.default),
    ?assertEqual("erlang.key", Record#mapping.mapping),
    ?assertEqual(advanced, Record#mapping.level),
    ?assertEqual([{enum, [on, off]}], Record#mapping.datatype),
    ?assertEqual(["documentation", "for feature"], Record#mapping.doc),
    ?assertEqual("default_substitution", Record#mapping.include_default),
    ?assertEqual("config_file_val", Record#mapping.new_conf_value),
    ?assertEqual(["valid.the.impailer"], Record#mapping.validators),
    ?assertEqual(true, Record#mapping.hidden),

    %% funciton tests
    ?assertEqual(["conf","key"], variable(Record)),
    ?assertEqual("default value", default(Record)),
    ?assertEqual("erlang.key", mapping(Record)),
    ?assertEqual(advanced, level(Record)),
    ?assertEqual([{enum, [on, off]}], datatype(Record)),
    ?assertEqual(["documentation", "for feature"], doc(Record)),
    ?assertEqual("default_substitution", include_default(Record)),
    ?assertEqual("config_file_val", new_conf_value(Record)),
    ?assertEqual(["valid.the.impailer"], validators(Record)),
    ?assertEqual(true, hidden(Record)),

    ok.

replace_test() ->
    Element1 = parse({
        mapping,
        "conf.key18",
        "erlang.key4",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "conf_val18"},
            {doc, ["documentation", "for feature"]}
        ]
    }),

    SampleMappings = [Element1,
    parse({
        mapping,
        "conf.key",
        "erlang.key2",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "conf_val_A"},
            {doc, ["documentation", "for feature"]}
        ]
    })
    ],

    Override = parse({
        mapping,
        "conf.key",
        "erlang.key",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "conf_val_B"},
            {doc, ["documentation", "for feature"]}
        ]
    }),

    NewMappings = replace(Override, SampleMappings),
    ?assertEqual([Element1, Override], NewMappings),
    ok.

validators_test() ->
    Validators = [
    cuttlefish_validator:parse(
        {validator, "a", "a desc", fun(_X) -> true end}
    ),
    cuttlefish_validator:parse({
        validator, "b", "b desc", fun(_X) -> true end
        }),
    cuttlefish_validator:parse({
        validator, "c", "c desc", fun(_X) -> true end
        })
    ],

    %% Hack for coverage
    [ begin
        Fun = cuttlefish_validator:func(V),
        ?assert(Fun(x))
    end || V <- Validators],
    Mapping = parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {validators, ["a", "b"]}
        ]
    }),

    [A, B, _C] = Validators,

    ?assertEqual([A,B], validators(Mapping, Validators)),

    MappingWithMissingValidator = parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {validators, ["a", "d"]} %% There is no "d"
        ]
    }),
    ?assertEqual([A], validators(MappingWithMissingValidator, Validators)),

    ok.

parse_and_merge_test() ->
    SampleMappings = [parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "conf_val_A"},
            {doc, ["documentation", "for feature"]},
            hidden
        ]
    }),
    parse({
        mapping,
        "conf.key2",
        "erlang.key2",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {new_conf_value, "conf_val_B"},
            {doc, ["documentation", "for feature"]}
        ]
    })
    ],

    NewMappings = parse_and_merge({mapping, "conf.key", "erlang.key3", [{hidden, false}]}, SampleMappings),

    ?assertEqual("erlang.key3", mapping(hd(NewMappings))),
    ?assertEqual(false, hidden(hd(NewMappings))),
    ok.

smart_merge_test() ->
    OldM1 = parse({mapping, "thing.to.merge", "some.key", [
            {default, 7},
            {datatype, integer},
            {doc, ["documentation", "for feature"]}
        ]}),
    OldM2 = parse({mapping, "thing.not.merged", "some.other_key", [{default, 6}, {datatype, integer}]}),

    OriginalMappings = [OldM1, OldM2],

    NewRawNoMergeMapping = {mapping, "thing.to.merge", "some.new_other_key", [{level, advanced}]},
    [NewUnMergedMapping, OldM2] = parse_and_merge(
        NewRawNoMergeMapping,
        OriginalMappings),

    ?assertEqual(["thing", "to", "merge"], variable(NewUnMergedMapping)),
    ?assertEqual(undefined, default(NewUnMergedMapping)),
    ?assertEqual("some.new_other_key", mapping(NewUnMergedMapping)),
    ?assertEqual(advanced, level(NewUnMergedMapping)),
    ?assertEqual([string], datatype(NewUnMergedMapping)),
    ?assertEqual([], doc(NewUnMergedMapping)),
    ?assertEqual(undefined, include_default(NewUnMergedMapping)),
    ?assertEqual([], validators(NewUnMergedMapping)),

    NewRawMergeMapping = {mapping, "thing.to.merge", "some.new_key", [merge, {level, advanced}]},
    [NewMergedMapping, OldM2] = NewMappings = parse_and_merge(
        NewRawMergeMapping,
        OriginalMappings),

    ?assertEqual(["thing", "to", "merge"], variable(NewMergedMapping)),
    ?assertEqual(7, default(NewMergedMapping)),
    ?assertEqual("some.new_key", mapping(NewMergedMapping)),
    ?assertEqual(advanced, level(NewMergedMapping)),
    ?assertEqual([integer], datatype(NewMergedMapping)),
    ?assertEqual(["documentation", "for feature"], doc(NewMergedMapping)),
    ?assertEqual(undefined, include_default(NewMergedMapping)),
    ?assertEqual([], validators(NewMergedMapping)),

    NewerRawMergeMapping = {mapping, "thing.to.merge", "some.third_key", [merge, {default, 42}]},

    [NewerMergedMapping, OldM2] = parse_and_merge(
        NewerRawMergeMapping,
        NewMappings),

    ?assertEqual(["thing", "to", "merge"], variable(NewerMergedMapping)),
    ?assertEqual(42, default(NewerMergedMapping)),
    ?assertEqual("some.third_key", mapping(NewerMergedMapping)),
    ?assertEqual(advanced, level(NewerMergedMapping)),
    ?assertEqual([integer], datatype(NewerMergedMapping)),
    ?assertEqual(["documentation", "for feature"], doc(NewerMergedMapping)),
    ?assertEqual(undefined, include_default(NewerMergedMapping)),
    ?assertEqual([], validators(NewerMergedMapping)),
    ok.

accidentally_used_strings_for_enums_test() ->
    Mapping = parse({
        mapping,
        "conf.key2",
        "erlang.key2",
        [
            {datatype, {enum, ["on", "off"]}}
        ]
    }),
    ?assertEqual([{enum, [on, off]}], cuttlefish_mapping:datatype(Mapping)),
    ok.

parse_error_test() ->
    {ErrorAtom, ErrorTuple} = parse(not_a_raw_mapping),
    ?assertEqual(error, ErrorAtom),
    ?assertEqual(
        "Poorly formatted input to cuttlefish_mapping:parse/1 : not_a_raw_mapping",
        ?XLATE(ErrorTuple)),
    ok.

is_mapping_test() ->
    ?assert(not(is_mapping(not_a_mapping))),

    M = parse({
        mapping,
        "conf.key2",
        "erlang.key2",
        [
            {datatype, {enum, ["on", "off"]}}
        ]
    }),
    ?assert(is_mapping(M)),
    ok.

%% conf.key can be any integer or the atom undefined.
extended_types_parse_test() ->
    Mapping = parse({
        mapping,
        "conf.key",
        "erlang.key",
        [
         {datatype, [integer, {atom, undefined}]}
        ]
    }),

    ?assertEqual([integer, {atom, undefined}], cuttlefish_mapping:datatype(Mapping)),
    ok.

datatype_cannot_be_empty_list_test() ->
    Mapping = parse({
        mapping,
        "conf.key",
        "erlang.key",
        [
         {datatype, []}
        ]
    }),
    ?assertMatch({error, _}, Mapping),
    ok.

-endif.
