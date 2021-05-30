%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(task_sup).

-include("dgiot_task.hrl").
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).


init([]) ->
    Child = [
        {task_worker, {task_worker, start_link, []}, transient, 5000, worker, [task_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.











