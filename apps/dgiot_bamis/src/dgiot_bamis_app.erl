%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_bamis Application
-module(dgiot_bamis_app).
-emqx_plugin(?MODULE).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_bamis_sup:start_link(),
    start_hooks(),
    {ok, Sup}.

stop(_State) ->
    stop_hooks(),
    ok.

start_hooks() ->
    dgiot_hook:add(one_for_one, {<<"get">>, <<"amis">>}, fun dgiot_bamis:get/1),
    dgiot_hook:add(one_for_one, {<<"post">>, <<"amis">>}, fun dgiot_bamis:post/1),
    dgiot_hook:add(one_for_one, {<<"put">>, <<"amis">>}, fun dgiot_bamis:put/1),
    dgiot_hook:add(one_for_one, {<<"delete">>, <<"amis">>}, fun dgiot_bamis:delete/1),
    ok.

stop_hooks() ->
    dgiot_hook:remove({<<"get">>, <<"amis">>}),
    dgiot_hook:remove({<<"post">>, <<"amis">>}),
    dgiot_hook:remove({<<"put">>, <<"amis">>}),
    dgiot_hook:remove({<<"delete">>, <<"amis">>}),
    ok.