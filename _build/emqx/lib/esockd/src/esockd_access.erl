%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(esockd_access).

-type(rule() :: {allow, all} | {allow, string()}
              | {deny,  all} | {deny,  string()}).

-type(compiled_rule() :: {allow, all}
                       | {allow, esockd_cidr:cidr()}
                       | {deny,  all}
                       | {deny,  esockd_cidr:cidr()}).
-export_type([rule/0]).

-export([compile/1, match/2]).

%% @doc Build CIDR, compile rule.
-spec(compile(rule()) -> compiled_rule()).
compile({allow, all}) ->
    {allow, all};
compile({allow, CIDR}) when is_list(CIDR) ->
    compile(allow, CIDR);
compile({deny, CIDR}) when is_list(CIDR) ->
    compile(deny, CIDR);
compile({deny, all}) ->
    {deny, all}.
compile(Type, CIDR) when is_list(CIDR) ->
    {Type, esockd_cidr:parse(CIDR, true)}. %% Adjust???

%% @doc Match Addr with Access Rules.
-spec(match(inet:ip_address(), [compiled_rule()])
      -> {matched, allow} | {matched, deny} | nomatch).
match(Addr, Rules) when is_tuple(Addr) ->
    match2(Addr, Rules).

match2(_Addr, []) ->
    nomatch;
match2(_Addr, [{allow, all} | _]) ->
    {matched, allow};
match2(_Addr, [{deny, all} | _]) ->
    {matched, deny};
match2(Addr, [{Access, CIDR = {_StartAddr, _EndAddr, _Len}} | Rules])
    when Access == allow orelse Access == deny ->
    case esockd_cidr:match(Addr, CIDR) of
        true  -> {matched, Access};
        false -> match2(Addr, Rules)
    end.

