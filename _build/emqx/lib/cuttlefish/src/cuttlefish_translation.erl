%% -------------------------------------------------------------------
%%
%% cuttlefish_translation: models a cuttlefish translation
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
-module(cuttlefish_translation).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(translation, {
    mapping::string(),
    func::fun()
    }).
-type translation() :: #translation{}.
-type translation_fun() :: fun(([proplists:property()]) -> any()).
-type raw_translation() :: {translation, string(), translation_fun()} | {translation, string()}.
-export_type([translation/0]).

-export([
    parse/1,
    parse_and_merge/2,
    is_translation/1,
    mapping/1,
    func/1,
    replace/2]).

-spec parse(raw_translation()) -> translation() | cuttlefish_error:error().
parse({translation, Mapping}) ->
    #translation{
        mapping = Mapping
    };
parse({translation, Mapping, Fun}) ->
    #translation{
        mapping = Mapping,
        func = Fun
    };
parse(X) ->
    {error, {translation_parse, X}}.

%% This assumes it's run as part of a foldl over new schema elements
%% in which case, there's only ever one instance of a key in the list
%% so keyreplace works fine.
-spec parse_and_merge(
    raw_translation(), [translation()]) -> [translation()].
parse_and_merge({translation, Mapping}, Translations) ->
    lists:keydelete(Mapping, #translation.mapping, Translations);
parse_and_merge({translation, Mapping, _} = TranslationSource, Translations) ->
    NewTranslation = parse(TranslationSource),
    case lists:keyfind(Mapping, #translation.mapping, Translations) of
        false ->
            [ NewTranslation | Translations];
        _OldMapping ->
            lists:keyreplace(Mapping, #translation.mapping, Translations, NewTranslation)
    end.

-spec is_translation(any()) -> boolean().
is_translation(T) -> is_tuple(T) andalso element(1, T) =:= translation.

-spec mapping(translation()) -> string().
mapping(T)  -> T#translation.mapping.

-spec func(translation()) -> fun().
func(T)     -> T#translation.func.

-spec replace(translation(), [translation()]) -> [translation()].
replace(Translation, ListOfTranslations) ->
    Exists = lists:keymember(mapping(Translation), #translation.mapping, ListOfTranslations),
    case Exists of
        true ->
            lists:keyreplace(mapping(Translation), #translation.mapping, ListOfTranslations, Translation);
        _ ->
            [Translation | ListOfTranslations]
    end.

-ifdef(TEST).

-define(XLATE(X), lists:flatten(cuttlefish_error:xlate(X))).

parse_test() ->
    TranslationDataStruct = {
        translation,
        "mapping",
        fun(X) -> X*2 end
    },

    Translation = parse(TranslationDataStruct),

    ?assertEqual("mapping", Translation#translation.mapping),
    F = Translation#translation.func,
    ?assertEqual(4, F(2)),
    ok.


getter_test() ->
    Translation = #translation{
        mapping = "mapping",
        func = fun(X) -> X*2 end
    },

    ?assertEqual("mapping", mapping(Translation)),

    Fun = func(Translation),
    ?assertEqual(4, Fun(2)),
    ok.


replace_test() ->
    Element1 = #translation{
        mapping = "mapping18",
        func = fun(X) -> X*2 end
    },
    ?assertEqual(4, (Element1#translation.func)(2)),

    Element2 =     #translation{
        mapping = "mapping1",
        func = fun(X) -> X*4 end
    },
    ?assertEqual(8, (Element2#translation.func)(2)),

    SampleTranslations = [Element1, Element2],

    Override = #translation{
        mapping = "mapping1",
        func = fun(X) -> X*5 end
    },
    ?assertEqual(25, (Override#translation.func)(5)),

    NewTranslations = replace(Override, SampleTranslations),
    ?assertEqual([Element1, Override], NewTranslations),
    ok.

parse_and_merge_test() ->
    Sample1 = #translation{
        mapping = "mapping1",
        func = fun(X) -> X*3 end
    },
    ?assertEqual(6, (Sample1#translation.func)(2)),

    Sample2 = #translation{
        mapping = "mapping2",
        func = fun(X) -> X*4 end
    },
    ?assertEqual(8, (Sample2#translation.func)(2)),

    SampleTranslations = [Sample1, Sample2],

    NewTranslations = parse_and_merge(
        {translation, "mapping1", fun(X) -> X * 10 end},
        SampleTranslations),
    F = func(hd(NewTranslations)),
    ?assertEqual(50, F(5)),
    ok.

parse_error_test() ->
    {ErrorAtom, ErrorTuple} = parse(not_a_raw_translation),
    ?assertEqual(error, ErrorAtom),
    ?assertEqual(
        "Poorly formatted input to cuttlefish_translation:parse/1 : not_a_raw_translation",
        ?XLATE(ErrorTuple)),
    ok.

parse_empty_test() ->
    TranslationDataStruct = {
        translation,
        "mapping"
    },

    Translation = parse(TranslationDataStruct),

    ?assertEqual("mapping", Translation#translation.mapping),
    F = Translation#translation.func,
    ?assertEqual(undefined, F),
    ok.

parse_and_merge_empty_test() ->
    Sample1 = #translation{
        mapping = "mapping1",
        func = fun(X) -> X*3 end
    },
    ?assertEqual(6, (Sample1#translation.func)(2)),

    Sample2 = #translation{
        mapping = "mapping2",
        func = fun(X) -> X*4 end
    },
    ?assertEqual(8, (Sample2#translation.func)(2)),

    SampleTranslations = [Sample1, Sample2],

    NewTranslations = parse_and_merge(
        {translation, "mapping1"},
        SampleTranslations),
    F = func(hd(NewTranslations)),
    ?assertEqual(1, length(NewTranslations)),
    ?assertEqual(40, F(10)),
    ok.

is_translation_test() ->
    ?assert(not(is_translation(not_a_translation))),

    T = #translation{
        mapping = "mapping1",
        func = fun(X) -> X*3 end
    },
    ?assertEqual(6, (T#translation.func)(2)),
    ?assert(is_translation(T)),
    ok.

-endif.
