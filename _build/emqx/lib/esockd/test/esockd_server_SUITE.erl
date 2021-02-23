%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(esockd_server_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

t_inc_dec_stats(_) ->
    {ok, _} = esockd_server:start_link(),
    Name = {echo, 3000},
    esockd_server:init_stats(Name, accepting),
    esockd_server:inc_stats(Name, accepting, 2),
    esockd_server:inc_stats(Name, accepting, 2),
    esockd_server:dec_stats(Name, accepting, 1),
    [{accepting, 3}] = esockd_server:get_stats(Name),
    ok = esockd_server:del_stats(Name),
    ok = timer:sleep(100),
    [] = esockd_server:get_stats(Name),
    ok = esockd_server:stop().

t_stats_fun(_) ->
    {ok, _} = esockd_server:start_link(),
    StatsFun = esockd_server:stats_fun({echo, 3000}, accepting),
    ok = lists:foreach(StatsFun, [{inc, 1}, {inc, 2}, {inc, 3}, {dec, 1}]),
    [{accepting, 5}] = esockd_server:get_stats({echo, 3000}),
    ok = esockd_server:stop().

t_handle_unexpected(_) ->
    {reply, ignore, state} = esockd_server:handle_call(req, from, state),
    {noreply, state} = esockd_server:handle_cast(msg, state),
    {noreply, state} = esockd_server:handle_info(info, state),
    {ok, state} = esockd_server:code_change('OldVsn', state, extra).

