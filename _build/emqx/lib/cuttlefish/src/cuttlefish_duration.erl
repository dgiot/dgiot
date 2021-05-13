%% -------------------------------------------------------------------
%%
%% cuttlefish_duration: complexity for parsing durations
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
-module(cuttlefish_duration).

-opaque time_unit() :: f | w | d | h | m | s | ms.
-export_type([time_unit/0]).

-include("cuttlefish_duration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([parse/1, parse/2, to_string/2]).

-spec to_string(integer(), time_unit()) -> string().
to_string(Fortnights, f) -> milliseconds(Fortnights * ?FORTNIGHT);
to_string(Weeks, w)      -> milliseconds(Weeks * ?WEEK);
to_string(Days, d)       -> milliseconds(Days * ?DAY);
to_string(Hours, h)      -> milliseconds(Hours * ?HOUR);
to_string(Minutes, m)    -> milliseconds(Minutes * ?MINUTE);
to_string(Seconds, s)    -> milliseconds(Seconds * ?SECOND);
to_string(Millis, ms)    -> milliseconds(Millis).

milliseconds(Millis) ->
    Units = lists:filter(fun({N, _Unit}) -> N =/= 0 end, [
        { Millis div ?WEEK,                "w"},
        {(Millis rem ?WEEK)   div ?DAY,    "d"},
        {(Millis rem ?DAY)    div ?HOUR,   "h"},
        {(Millis rem ?HOUR)   div ?MINUTE, "m"},
        {(Millis rem ?MINUTE) div ?SECOND, "s"},
        { Millis rem ?SECOND,              "ms"}
    ]),
    lists:flatten([
        integer_to_list(N) ++ Unit
    || {N, Unit} <- Units]).

-spec parse(string(), time_unit()) -> integer().
parse(DurationString, ms) -> parse(DurationString);
parse(DurationString, Unit) ->
    case parse(DurationString) of
        {error, _}=Err ->
            Err;
        Num when is_number(Num) ->
            {Unit, Multiplier} = lists:keyfind(Unit, 1, ?MULTIPLIERS),
            cuttlefish_util:ceiling(Num / Multiplier)
    end.


-spec parse(string()) -> integer() | cuttlefish_error:error().
parse(InputDurationString) ->
    DurationString = string:to_lower(InputDurationString),
    case cuttlefish_duration_parse:parse(DurationString) of
        Float when is_float(Float) ->
            cuttlefish_util:ceiling(Float);
        Int when is_integer(Int) ->
            Int;
        _ ->
            {error, {duration, InputDurationString}}
    end.

-ifdef(TEST).

milliseconds_test() ->
    ?assertEqual("500ms", milliseconds(500)),
    ?assertEqual("1s500ms", milliseconds(1500)),
    ?assertEqual("30m", milliseconds(1800000)),
    ?assertEqual("1w1d1h1m1s1ms", milliseconds(694861001)),
    ok.

seconds_test() ->
    ?assertEqual("50s", to_string(50, s)),
    ?assertEqual("1m1s", to_string(61, s)),
    ?assertEqual("30m", to_string(1800, s)),
    ?assertEqual("1w1d1h1m1s", to_string(694861, s)),
    ok.

parse_test() ->
    test_parse(500,  "500ms"),
    test_parse(500,  ".5s"),
    test_parse(1001, "1s1ms"),
    test_parse(1599, "1s599ms"),
    test_parse(1599, "1.599s"),
    test_parse(1600, "1.5999s"),
    test_parse(60000, "1m"),
    test_parse(60000, "60s"),
    test_parse(60000, "60000ms"),
    test_parse(90000, "1.5m"),
    test_parse(90000, "1m30s"),
    test_parse(90000, "1m29s1000ms"),
    test_parse(1800000, ".5h"),
    test_parse(3600000, "1h"),
    test_parse(3601000, "1h1s"),
    test_parse(3660000, "1h1m"),
    test_parse(3661000, "1h1m1s"),
    test_parse(3661001, "1h1m1s1ms"),
    test_parse(3600000, "60m"),
    test_parse(5400000, "90m"),
    test_parse(5401000, "90m1s"),
    test_parse(5401001, "90m1s1ms"),
    test_parse(5400000, "1h30m"),
    test_parse(5401000, "1h30m1s"),
    test_parse(3660000, "1h1m"),
    test_parse(86400000, "1d"),
    test_parse(86401000, "1d1s"),
    test_parse(86401001, "1d1s1ms"),
    test_parse(604800000, "1w"),
    test_parse(691200000, "1w1d"),
    test_parse(694800000, "1w1d1h"),
    test_parse(694860000, "1w1d1h1m"),
    test_parse(694861000, "1w1d1h1m1s"),
    test_parse(694861001, "1w1d1h1m1s1ms"),

    %% Weird but ok?
    test_parse(121001, "1m1s1ms1m"),

    %% Easter Egg
    test_parse(1904461001, "1f1w1d1h1m1s1ms"),
    ok.

test_parse(ExpectedMillis, StringToParse) ->
    ?assertEqual(ExpectedMillis, parse(StringToParse)).

parse_2_test() ->
    ?assertEqual(1, parse("1ms", ms)),
    ?assertEqual(1, parse("1ms", s)),
    ?assertEqual(1, parse("1ms", m)),
    ?assertEqual(1, parse("1ms", h)),
    ?assertEqual(1, parse("1ms", d)),
    ?assertEqual(1, parse("1ms", w)),
    ?assertEqual(1, parse("1ms", f)),
    ok.

to_string_test() ->
    ?assertEqual("2w", to_string(1, f)),
    ?assertEqual("2w", to_string(2, w)),
    ?assertEqual("1w", to_string(1, w)),
    ?assertEqual("1d", to_string(1, d)),
    ?assertEqual("1w", to_string(7, d)),
    ?assertEqual("1h", to_string(1, h)),
    ?assertEqual("1m", to_string(1, m)),
    ok.

parse_error_test() ->
    ?assertMatch({error, _}, parse("1q")),
    %% This previously raised badarith because it did not check the
    %% return value of parse/1.
    ?assertMatch({error, _}, catch parse("1q", h)),
    ok.

-endif.
