%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(local_rule).
-author("dgiot").
-export([show/1, create/1, update/1, delete/1, create_resource/1, delete_resource/1]).

-define(TAB, local_rules).
-define(RES_TAB, local_resources).

show(#{<<"id">> := Id}) ->
    case ets:lookup(?TAB, Id) of
        [{Id, Rule}] -> {ok, Rule};
        [] -> {error, not_found}
    end;
show(_) ->
    {error, not_found}.

create(Params) ->
    Id = maps:get(<<"id">>, Params, dgiot_guid:to_hexstr(dgiot_guid:gen())),
    ensure_table(),
    ets:insert(?TAB, {Id, Params#{<<"id">> => Id}}),
    {ok, Params#{<<"id">> => Id}}.

update(Params) ->
    case maps:find(<<"id">>, Params) of
        {ok, Id} ->
            ensure_table(),
            case ets:lookup(?TAB, Id) of
                [{Id, _Old}] -> ets:insert(?TAB, {Id, Params});
                [] -> ets:insert(?TAB, {Id, Params})
            end,
            {ok, Params};
        error ->
            {error, missing_id}
    end.

delete(#{<<"id">> := Id}) ->
    ensure_table(),
    ets:delete(?TAB, Id),
    ok;
delete(_) ->
    {error, missing_id}.

create_resource(Params) ->
    Id = maps:get(<<"id">>, Params, dgiot_guid:to_hexstr(dgiot_guid:gen())),
    ensure_table(?RES_TAB),
    ets:insert(?RES_TAB, {Id, Params#{<<"id">> => Id}}),
    {ok, Params#{<<"id">> => Id}}.

delete_resource(#{<<"id">> := Id}) ->
    ensure_table(?RES_TAB),
    ets:delete(?RES_TAB, Id),
    ok;
delete_resource(_) ->
    {error, missing_id}.

ensure_table() ->
    ensure_table(?TAB).

ensure_table(Name) ->
    case ets:info(Name, name) of
        undefined -> ets:new(Name, [named_table, public, set, {read_concurrency, true}]);
        _ -> ok
    end.
