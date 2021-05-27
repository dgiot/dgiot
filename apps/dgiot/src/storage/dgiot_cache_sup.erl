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


-module(dgiot_cache_sup).
-author("johnliu").
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

init(Opts) ->
    MaxSize = proplists:get_value(ets_maxsize, Opts, 32 * 1024 * 1024),
    Threshold = proplists:get_value(ets_threshold, Opts, 0.85),
    Weight = proplists:get_value(ets_weight, Opts, 30),
    ValOpts = [{ets_maxsize, MaxSize}, {ets_threshold, Threshold}, {checkpid, dgiot_cache_check_worker}],
    ChkOpt = [{ets_weight, Weight}],
    ValServ = {dgiot_cache_worker, {dgiot_cache_worker, start_link, [ValOpts]},
        permanent, 2000, worker, [dgiot_cache_worker]},
    ChkServ = {dgiot_cache_check_worker, {dgiot_cache_check_worker, start_link, [ChkOpt]},
        permanent, 2000, worker, [dgiot_cache_check_worker]},
    Children = [ValServ, ChkServ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
