%% -------------------------------------------------------------------
%%
%% cuttlefish_util: various cuttlefish utility functions
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
-module(cuttlefish_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
    replace_proplist_value/3,
    numerify/1,
    ceiling/1,
    levenshtein/2]).

%% Legacy API
-export([
     conf_get_value/2,
     conf_get_value/3,
     filter_by_variable_starts_with/2,
     matches_for_variable_def/2,
     fuzzy_variable_match/2
]).

-include("cuttlefish.hrl").

%% @deprecated
conf_get_value(Key, Conf) ->
    ?logger:warning("cuttlefish_util:conf_get_value/2 has been deprecated. use cuttlefish:conf_get/2"),
    cuttlefish:conf_get(Key, Conf).

%% @deprecated
conf_get_value(Key, Conf, Default) ->
    ?logger:warning("cuttlefish_util:conf_get_value/3 has been deprecated. use cuttlefish:conf_get/3"),
    cuttlefish:conf_get(Key, Conf, Default).

%% @deprecated
filter_by_variable_starts_with(Prefix, Conf) ->
    ?logger:warning("cuttlefish_util:filter_by_variable_starts_with/2 has been deprecated. use cuttlefish_variable:filter_by_prefix/2"),
    cuttlefish_variable:filter_by_prefix(Prefix, Conf).

%% @deprecated
matches_for_variable_def(VarDef, Conf) ->
    ?logger:warning("cuttlefish_util:matches_for_variable_def/2 has been deprecated. use cuttlefish_variable:fuzzy_matches/2"),
    cuttlefish_variable:fuzzy_matches(VarDef, Conf).

%% @deprecated
fuzzy_variable_match(Var, VarDef) ->
    ?logger:warning("cuttlefish_util:fuzzy_variable_match/2 has been deprecated. use cuttlefish_variable:is_fuzzy_match/2"),
    cuttlefish_variable:is_fuzzy_match(Var, VarDef).

%% @doc replace the element in a proplist
-spec replace_proplist_value(atom() | string(), any(), [{string(), any()}]) -> [{string(), any()}].
replace_proplist_value(Key, Value, Proplist) ->
    lists:keystore(Key, 1, Proplist, {Key, Value}).

%% @doc Turn a string into a number if `list_to_float' or
%% `list_to_integer' accept it as valid
-spec numerify(string()) -> integer()|float()|cuttlefish_error:error().
numerify([$.|_]=Num) -> numerify([$0|Num]);
numerify(String) ->
    try list_to_float(String) of
        Float -> Float
    catch
        _:_ ->
            try list_to_integer(String) of
                Int -> Int
            catch
                _:_ ->
                    {error, {number_parse, String}}
            end
    end.

%% @doc remember when you learned about decimal places. about a minute
%% later, you learned about rounding up and down. This is rounding up.
-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Levenshtein code by Adam Lindberg, Fredrik Svensson via
%% http://www.trapexit.org/String_similar_to_(Levenshtein)
%%
%%------------------------------------------------------------------------------
%% @spec levenshtein(StringA :: string(), StringB :: string()) -> integer()
%% @doc Calculates the Levenshtein distance between two strings
%% @end
%%------------------------------------------------------------------------------
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

-ifdef(TEST).

replace_proplist_value_test() ->
    Proplist = [
        {"test1", 1},
        {"test2", 2},
        {"test3", 3}
    ],

    NewProplist = replace_proplist_value("test2", 8, Proplist),
    ?assertEqual(
        8,
        proplists:get_value("test2", NewProplist)
        ),
    ok.

replace_proplist_value_when_undefined_test() ->
    Proplist = [
        {"test1", 1},
        {"test2", 2}
    ],

    NewProplist = replace_proplist_value("test3", 3, Proplist),
        ?assertEqual(
        3,
        proplists:get_value("test3", NewProplist)
        ),
    ok.

levenshtein_test() ->
    ?assertEqual(0, levenshtein("X", "X")),
    ?assertEqual(1, levenshtein("X", "XX")),
    ?assertEqual(1, levenshtein("penguin", "penguino")),
    ?assertEqual(1, levenshtein("dtrace", "ctrace")),
    ?assertEqual(5, levenshtein("anti_entropy", "anti_entropy.tick")),
    ?assertEqual(1, levenshtein("anti_entropy", "anti-entropy")),
    ?assertEqual(4, levenshtein("", "four")),
    ?assertEqual(4, levenshtein("four", "")),
    ok.

ceiling_test() ->
    ?assertEqual(9, ceiling(8.99999)),
    ?assertEqual(9, ceiling(8.00001)),
    ?assertEqual(9, ceiling(9.0)),
    ?assertEqual(-2, ceiling(-2.0000001)),
    ?assertEqual(-2, ceiling(-2.9999999)),
    ok.

-endif.
