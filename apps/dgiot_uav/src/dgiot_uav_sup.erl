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

-module(dgiot_uav_sup).
-author("johnliu").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    
    ChildSpecs = [
        % #{
        %     id => dgiot_uav_test_cache_simple,
        %     start => {dgiot_uav_test_cache_simple, start_link, []},
        %     restart => permanent,
        %     shutdown => 5000,
        %     type => worker,
        %     modules => [dgiot_uav_test_cache_simple]
        % },
        %% 添加命令调度器进程
        #{
            id => dgiot_uav_command_scheduler,
            start => {dgiot_uav_command_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dgiot_uav_command_scheduler]
        },
        %% 添加告警管理器进程
        #{
            id => dgiot_uav_alarm_manager,
            start => {dgiot_uav_alarm_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dgiot_uav_alarm_manager]
        },
        %% 添加数据聚合器进程
        #{
            id => dgiot_uav_aggregator,
            start => {dgiot_uav_aggregator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dgiot_uav_aggregator]
        },
        %% 添加测试管理器进程（替代 auto_tester）
        #{
            id => dgiot_uav_test_manager,
            start => {dgiot_uav_test_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dgiot_uav_test_manager]
        }
        %% 数据汇聚由 dgiot_uav_tcp_worker 数据驱动触发，通过聚合器处理
    ],
    
    {ok, {SupFlags, ChildSpecs}}.