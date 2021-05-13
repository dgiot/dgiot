%% -------------------------------------------------------------------
%%
%% cuttlefish_bytesize: complexity for parsing bytesizes
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

-module(cuttlefish_bytesize).

-define(KILOBYTE, 1024).
-define(MEGABYTE, 1048576).
-define(GIGABYTE, 1073741824).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([parse/1, to_string/1]).

%% @doc turns an integer of bytes into a string.
%% Will use the smallest unit to not lose precision.
%% e.g. 1024 -> 1kb, but 1025 = 1025.
-spec to_string(integer()) -> string().
to_string(Bytez) ->
    case { Bytez rem ?GIGABYTE, Bytez rem ?MEGABYTE, Bytez rem ?KILOBYTE} of
        {0, _, _} ->
            integer_to_list(Bytez div ?GIGABYTE) ++ "GB";
        {_, 0, _} ->
            integer_to_list(Bytez div ?MEGABYTE) ++ "MB";
        {_, _, 0} ->
            integer_to_list(Bytez div ?KILOBYTE) ++ "KB";
        _ ->
            integer_to_list(Bytez)
    end.

%% @doc the reverse of to_string/1. turns "1kb" or "1KB" into 1024.
-spec parse(string()) -> integer()|cuttlefish_error:error().
parse(String) ->
    case lists:reverse(String) of
        [$B,$K|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)), ?KILOBYTE);
        [$b,$k|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)), ?KILOBYTE);
        [$B,$M|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)), ?MEGABYTE);
        [$b,$m|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)), ?MEGABYTE);
        [$B,$G|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)), ?GIGABYTE);
        [$b,$g|BSize] -> bmult(cuttlefish_util:numerify(lists:reverse(BSize)),?GIGABYTE);
        BSize -> cuttlefish_util:numerify(lists:reverse(BSize))
    end.

-spec bmult(number()|cuttlefish_error:error(), integer()) ->
                   number()|cuttlefish_error:error().
bmult({error, _ErrorTerm}=Error, _Mult) ->
    Error;
bmult(Quantity, Multiplier) ->
    Quantity * Multiplier.

-ifdef(TEST).
to_string_test() ->
    ?assertEqual("1KB", to_string(1024)),
    ?assertEqual("2KB", to_string(2048)),

    ?assertEqual("10MB", to_string(10485760)),

    ?assertEqual("1GB", to_string(1073741824)),

    ?assertEqual("20", to_string(20)),
    ok.

parse_test() ->
    ?assertEqual(1024, parse("1kb")),
    ?assertEqual(2048, parse("2KB")),

    ?assertEqual(10485760, parse("10mb")),
    ?assertEqual(10485760, parse("10MB")),

    ?assertEqual(1073741824, parse("1GB")),
    ?assertEqual(1073741824, parse("1gb")),

    ?assertEqual(20, parse("20")),
    ?assertEqual({error, {number_parse, "10MB10"}}, parse("10MB10kb")),
    ok.

-endif.
