%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_parse_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Setups
%%--------------------------------------------------------------------

all() ->
    {ok, #{<<"objectId">> := Id}} = dgiot_parse:create_object(<<"test">>, #{<<"name">> => 1}),
    {ok, #{<<"results">> := [#{<<"objectId">> := Id1}]}} = query_object(<<"test">>, #{<<"name">> => 1}),
    ?assertEqual(Id1, Id),
    {ok, #{<<"updatedAt">> := _}} = dgiot_parse:update_object(<<"test">>, Id, #{<<"age">> => 12}),
    {ok, #{<<"age">> := Age}} = dgiot_parse:get_object(<<"test">>, Id),
    ?assertEqual(Age, 12),
    dgiot_parse:del_object(<<"test">>, Id),
    {error, _} = dgiot_parse:get_object(<<"test">>, Id).

init_per_suite(Cfg) ->
    Cfg.

end_per_suite(_Cfg) ->
    _Cfg.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_noserver_nohook(_) ->
    _.


