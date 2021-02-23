%% -------------------------------------------------------------------
%%
%% cuttlefish_flag: datatype for simple boolean settings with
%%   customizable names and values
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

-module(cuttlefish_flag).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
         parse/1,
         parse/2,
         to_string/2
        ]).

-define(FMT(F, A), lists:flatten(io_lib:format(F, A))).

parse(Value) ->
    cuttlefish_enum:parse(Value, to_enum(flag)).
parse(Value, Flag) ->
    cuttlefish_enum:parse(Value, to_enum(Flag)).

to_string(Value, Flag) ->
    cuttlefish_enum:to_string(Value, to_enum(Flag)).

to_enum({flag, {On, OnValue}, {Off, OffValue}}) ->
    {enum, [{On, OnValue}, {Off, OffValue}]};
to_enum({flag, On, Off}) ->
    {enum, [{On, true}, {Off, false}]};
to_enum(flag) ->
    {enum, [{on, true}, {off, false}]}.

-ifdef(TEST).
parse_test() ->
    ?assertEqual(true, parse("on")),
    ?assertEqual(false, parse("off")),
    ?assertEqual(true, parse("enabled", {flag, enabled, disabled})),
    ?assertEqual(false, parse("disabled", {flag, enabled, disabled})),
    ?assertEqual(tyk, parse("on", {flag, {on, tyk}, {off, torp}})),
    ?assertEqual(torp, parse("off", {flag, {on, tyk}, {off, torp}})),
    ?assertEqual({long, tuple, value},
                 parse("foo", {flag, {simple, ok},
                               {foo, {long, tuple, value}}})),
    ?assertEqual(ok,
                 parse("simple", {flag, {simple, ok},
                               {foo, {long, tuple, value}}})).

to_string_test() ->
    ?assertEqual(to_string(true, flag), "on"),
    ?assertEqual(to_string(on, flag), "on"),
    ?assertEqual(to_string(false, flag), "off"),
    ?assertEqual(to_string(off, flag), "off"),
    ?assertEqual(to_string(true, {flag, enabled, disabled}), "enabled"),
    ?assertEqual(to_string(enabled, {flag, enabled, disabled}), "enabled"),
    ?assertEqual(to_string(false, {flag, enabled, disabled}), "disabled"),
    ?assertEqual(to_string(disabled, {flag, enabled, disabled}), "disabled"),
    ?assertEqual(to_string(tyk, {flag, {on, tyk}, {off, torp}}), "on"),
    ?assertEqual(to_string(on, {flag, {on, tyk}, {off, torp}}), "on"),
    ?assertEqual(to_string(torp, {flag, {on, tyk}, {off, torp}}), "off"),
    ?assertEqual(to_string(off, {flag, {on, tyk}, {off, torp}}), "off"),
    ?assertEqual(to_string({long, tuple, value}, {flag, {simple, ok},
                               {foo, {long, tuple, value}}}),
                 "foo"),
    ?assertEqual(to_string(foo, {flag, {simple, ok},
                               {foo, {long, tuple, value}}}),
                 "foo"),
    ?assertEqual(to_string(ok, {flag, {simple, ok},
                               {foo, {long, tuple, value}}}),
                 "simple"),
    ?assertEqual(to_string(simple, {flag, {simple, ok},
                               {foo, {long, tuple, value}}}),
                 "simple").
-endif.
