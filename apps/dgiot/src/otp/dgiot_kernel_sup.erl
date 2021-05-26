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

-module(dgiot_kernel_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 100},
        [child_spec(dgiot_global_gc, worker),
            child_spec(dgiot_pool_sup, supervisor),
            child_spec(dgiot_hooks, worker),
            child_spec(dgiot_stats, worker),
            child_spec(dgiot_cache_sup, supervisor, []),
            child_spec(dgiot_cron_sup, supervisor, []),
            child_spec(dgiot_ctl, worker)]}}.

child_spec(M, Type) ->
    child_spec(M, Type, []).

child_spec(M, worker, Args) ->
    #{id => M,
        start => {M, start_link, Args},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [M]
    };

child_spec(M, supervisor, Args) ->
    #{id => M,
        start => {M, start_link, Args},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [M]
    }.
