%% -------------------------------------------------------------------
%%
%% cuttlefish_generator: this is where the action is
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
-module(cuttlefish_error).

-type error() :: {'error', {atom(), term()}}.
-type errorlist() :: {'errorlist', [error()]}.
-export_type([error/0, errorlist/0]).

-export([
        contains_error/1,
        is_error/1,
        filter/1,
        errorlist_maybe/1,
        print/1,
        print/2,
        xlate/1
]).

-include("cuttlefish.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% We'll be calling this a lot from `xlate'
-define(STR(X, Y), xlate(cuttlefish_datatypes:to_string(X, Y))).

-spec xlate({atom(), term()}|string()) -> iolist().
xlate(Message) when is_list(Message) ->
    %% We allow for strings so that we can safely call
    %% `cuttlefish_datatypes:to_string` when creating these messages
    Message;
xlate({error, Details}) ->
    xlate(Details);
xlate({_Error, {error, NestedError}}) ->
    xlate(NestedError);
xlate({type, {Value, Type}}) ->
    io_lib:format("Tried to convert ~p but invalid datatype: ~p",
                  [Value, Type]);
xlate({range, {{Value, Type}, Range}}) ->
    [?STR(Value, Type), " can't be outside the range ", Range];
xlate({conversion, {Value, Type}}) ->
    io_lib:format("~p cannot be converted to a(n) ~s", [Value, Type]);
xlate({duration, Value}) ->
    io_lib:format("Invalid duration value: ~ts", [Value]);
xlate({enum_name, {Value, EnumNames}}) ->
    io_lib:format("~p is not a valid enum value, acceptable values are: ~ts",
                  [Value, string:join(EnumNames, ", ")]);
xlate({enum_format, Value}) ->
    %% This collapses two different type of formatting errors into one
    %% error message
    io_lib:format("Enum elements must be atoms, strings, or 2-tuples with "
                  "atom or string as first element. Bad value: ~w", [Value]);
xlate({mapping_types, List}) ->
    io_lib:format("Invalid datatype list for mapping: ~ts",
                  [string:join(List, ", ")]);
xlate({mapping_parse, Term}) ->
    io_lib:format(
        "Poorly formatted input to cuttlefish_mapping:parse/1 : ~p",
        [Term]
     );
xlate({translation_parse, Term}) ->
    io_lib:format(
      "Poorly formatted input to cuttlefish_translation:parse/1 : ~p",
      [Term]
     );
xlate({validator_parse, Term}) ->
    io_lib:format(
      "Poorly formatted input to cuttlefish_validator:parse/1 : ~p",
      [Term]
     );
xlate({conf_to_latin1, LineNum}) ->
    io_lib:format("Error converting value on line #~p to latin1", [LineNum]);
xlate({bytesize_parse, Value}) ->
    io_lib:format("Error converting value ~p to a number of bytes", [Value]);
xlate({file_open, {File, Reason}}) ->
    io_lib:format("Could not open file (~s) for Reason ~s", [File, Reason]);
xlate({conf_syntax, {File, {Line, Col}}}) ->
    io_lib:format("Syntax error in ~s after line ~p column ~p, "
                  "parsing incomplete", [File, Line, Col]);
xlate({in_file, {File, Error}}) ->
    [File, ": ", xlate(Error)];
xlate({translation_missing_setting, {Translation, Setting}}) ->
    io_lib:format("Translation for '~s' expected to find setting '~s' but was missing",
                  [Translation, Setting]);
xlate({translation_invalid_configuration, {Translation, Invalid}}) ->
    io_lib:format("Translation for '~s' found invalid configuration: ~s",
                  [Translation, Invalid]);
xlate({translation_unknown_error, {Translation, {Class, Error}}}) ->
    io_lib:format("Error running translation for ~s, [~p, ~p]",
                  [Translation, Class, Error]);
xlate({translation_arity, {Translation, Arity}}) ->
    io_lib:format("~p is not a valid arity for translation fun() ~s."
                  " Try 1 or 2", [Arity, Translation]);
xlate({map_multiple_match, VariableDefinition}) ->
    io_lib:format("~p has both a fuzzy and strict match", [VariableDefinition]);
xlate({unknown_variable, Variable}) ->
    ["Conf file attempted to set unknown variable: ", Variable];
xlate({unsupported_type, Type}) ->
    io_lib:format("~p is not a supported datatype", [Type]);
xlate({transform_type, Type}) ->
    ["Error transforming datatype for: ", Type];
xlate({transform_type_exception, {Type, {Class, Error}}}) ->
    io_lib:format("Caught exception converting to ~p: ~p:~p",
                  [Type, Class, Error]);
xlate({transform_type_unacceptable, {Value, BadValue}}) ->
    io_lib:format("~p is not accepted value: ~p", [Value, BadValue]);
xlate({circular_rhs, History}) ->
    io_lib:format("Circular RHS substitutions: ~p", [History]);
xlate({substitution_missing_config, {Substitution, Variable}}) ->
    io_lib:format("'~s' substitution requires a config variable '~s' to be set",
                  [Substitution, Variable]);
xlate({mapping_not_found, Variable}) ->
    [Variable, " not_found"];
xlate({mapping_multiple, {Variable, {Hard, Fuzzy}}}) ->
    io_lib:format("~p hard mappings and ~p fuzzy mappings found "
                  "for ~s", [Hard, Fuzzy, Variable]);
xlate({validation, {Variable, Description}}) ->
    [Variable, " invalid, ", Description];
xlate({erl_parse, {Reason, LineNo}}) ->
    ["Schema parse error near line number ", integer_to_list(LineNo),
     ": ", Reason];
xlate({erl_parse, Reason}) ->
    io_lib:format("Schema parse error: ~p", [Reason]);
xlate({erl_parse_unexpected, Error}) ->
    io_lib:format("Unexpected return from erl_parse:parse_exprs/1: ~p",
                  [Error]);
xlate({parse_schema, Value}) ->
    io_lib:format("Unknown parse return: ~p", [Value]);
xlate({erl_scan, LineNo}) ->
    ["Error scanning erlang near line ", integer_to_list(LineNo)].

-spec contains_error(list()) -> boolean().
contains_error(List) ->
    lists:any(fun is_error/1, List).

-spec is_error(any()) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

-spec filter(list()) -> errorlist().
filter(List) ->
    {errorlist, lists:filter(fun is_error/1, List)}.

-spec errorlist_maybe(any()) -> any().
errorlist_maybe(List) when is_list(List) ->
    case filter(List) of
        {errorlist, []} ->
            List;
        Errorlist ->
            Errorlist
    end;
errorlist_maybe(AnythingElse) -> AnythingElse.

-spec print(string(), [any()]) -> ok.
print(FormatString, Args) ->
    print(io_lib:format(FormatString, Args)).

-spec print(string() | error()) -> ok.
print({error, ErrorTerm}) ->
    print(lists:flatten(xlate(ErrorTerm)));
print(String) ->
    ok = ?logger:error("~s", [String]).

-ifdef(TEST).

is_error_test() ->
    ?assert(is_error({error, "oh no!"})),
    ?assert(not(is_error("just an innocent string... I mean a list... I mean... argh, erlang"))),
    ok.

contains_error_test() ->
    ?assert(contains_error(["hi", {error, "hi!"}, "bye"])),
    ?assert(not(contains_error(["hi", "I'm not an error", "bye"]))),
    ok.

filter_test() ->
    ?assertEqual({errorlist, []}, filter(["hi", "what even is an error?", "bye"])),
    ?assertEqual({errorlist, [{error, "etoomanythings"}]},
                 filter(["hi", {error, "etoomanythings"}, "bye"])),
    ok.

errorlist_maybe_test() ->
    ?assertEqual(atom, errorlist_maybe(atom)),
    ?assertEqual(12, errorlist_maybe(12)),
    %% Fool you! "string" is a list!, but doesn't contain an error()
    ?assertEqual("string", errorlist_maybe("string")),

    ?assertEqual(
       {errorlist, [{error, "etoomanythings"}]},
       errorlist_maybe(["hi", {error, "etoomanythings"}, "bye"])),
    ?assertEqual(
       ["hi", "what even is an error?", "bye"],
       errorlist_maybe(["hi", "what even is an error?", "bye"])),
    ok.

-endif.
