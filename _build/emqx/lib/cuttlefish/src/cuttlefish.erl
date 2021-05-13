%% -------------------------------------------------------------------
%%
%%  External functions for schema writers and cuttlefish invokers
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

-module(cuttlefish).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
    conf_get/2,
    conf_get/3,
    unset/0,
    invalid/1,
    otp/2,
    otp/3,
    warn/1
]).

-include("cuttlefish.hrl").

% @doc If DesiredMinimum =&lt; the OTP you're running, then return
% IfGreaterOrEqual, otherwise IfLessThan.
-spec otp(string(), any(), any()) -> any().
otp(DesiredMinimumOTPVersion, IfGreaterOrEqual, IfLessThan) ->
    ActualOTPVersion = erlang:system_info(otp_release),
    case otp(DesiredMinimumOTPVersion, ActualOTPVersion) of
        true -> IfGreaterOrEqual;
        _ -> IfLessThan
    end.

% @doc is ActualOTPVersion >= DesiredMinimumOTPVersion?
-spec otp(string(), string()) -> boolean().
otp([], []) ->
    %% They're the same length AND all previous chars were matches
    true;
otp([$R|TMin], Ver) ->
    otp(TMin, Ver);
otp(Min, [$R|TVer]) ->
    otp(Min, TVer);
otp([H|TMin], [H|TVer]) ->
    %% The head chars are equal, test the tails
    otp(TMin, TVer);
otp([HMin|_], [HVer|_]) ->
    %% The heads are different, check which is greater
    HVer >= HMin;
otp([], _Ver) ->
    %% The actual OTP release is a longer string, but
    %% everything matched up until this point
    %% e.g. R16B02, R16B02-basho4
    true;
otp(_Min, []) ->
    %% Our Min is a longer string
    %% e.g. R16B02-basho4, R16B02
    false.

% @doc conf_get/2 is a convenience wrapper for proplists:get_value/2
% for schema writers. Keys to a Conf proplist are variable()s which
% are a list of strings.  This function will look for those, but if
% you pass it a string() instead, it will be nice and split that
% string on "." since that's how cuttlefish do.  Also, it will
% throw(not_found) if the key is not found in the list which is
% different that proplists:get_value/2's default behavior of returning
% 'undefined'. This makes it easy for cuttlefish translations to abort
% and on error, and not assume a value. If that's what you want,
% please use conf_get/3.  formerly cuttlefish_util:conf_get_value/2
-spec conf_get(
        string() | cuttlefish_variable:variable(),
        cuttlefish_conf:conf()) -> any().
conf_get([H|_T]=Variable, ConfigProplist) when is_list(H) ->
    case proplists:is_defined(Variable, ConfigProplist) of
        true ->
            proplists:get_value(Variable, ConfigProplist);
        false ->
            throw({not_found, Variable})
    end;
conf_get(Variable, ConfigProplist) ->
    conf_get(
      cuttlefish_variable:tokenize(Variable),
      ConfigProplist).

% @doc conf_get/3 works just like proplists:get_value/3. It expects a
% variable() as the Key, but is nice enough to take a string() and
% split it on "."  formerly cuttlefish_util:conf_get_value/3
-spec conf_get(
        string() | cuttlefish_variable:variable(),
        cuttlefish_conf:conf(), any()) -> any().
conf_get([H|_T]=Variable, ConfigProplist, Default) when is_list(H) ->
    proplists:get_value(Variable, ConfigProplist, Default);
conf_get(Variable, ConfigProplist, Default) ->
    conf_get(cuttlefish_variable:tokenize(Variable), ConfigProplist, Default).

%% @doc When called inside a translation, tells cuttlefish to omit the
%% Erlang setting from the generated configuration.
-spec unset() -> no_return().
unset() ->
    throw(unset).

%% @doc When called inside a translation, informs the user that input
%% configuration is invalid, using the supplied reason string.
-spec invalid(string()) -> no_return().
invalid(Reason) ->
    throw({invalid, Reason}).

%% @doc When called inside a translation, results in a warning message
%% being logged.
-spec warn(iodata()) -> ok.
warn(Str) ->
    ?logger:warning(Str, []).

-ifdef(TEST).

otp_test() ->
    ?assert(otp("R15B02", "R15B02-basho3")),
    ?assert(not(otp("R15B02-basho3", "R15B02"))),
    ?assert(otp("R16B02-basho3", "R16B03")),
    ?assert(otp("R15B01", "R15B02")),
    ?assert(otp("R15B01", "R15B02-basho3")),
    ?assert(not(otp("R16B01", "R15B02"))),
    ?assert(otp("R16", "R16B03")),
    ?assert(otp("R16", "R16A")),
    ?assert(not(otp("R16B01", "R16A"))),
    ?assert(otp("R16A", "R16A")),
    ?assert(otp("R16", "17")),
    ?assert(otp("R16B03-1", "17")),
    ?assert(not(otp("17", "R16"))),
    ?assert(otp("R16A", "17")),
    ?assert(not(otp("18", "17"))),
    ok.

-endif.
