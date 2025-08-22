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

%% @doc UDP广播服务监督者模块
%% 负责启动和管理UDP广播工作进程
-module(udp_broadcast_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

%% @doc 启动监督者进程
%% Name: 监督者注册名称
%% 返回: {ok, Pid} | {error, Reason}
start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

%% @doc 初始化监督者
%% 设置监督策略和子进程规格
init([]) ->
    % 定义子进程规格
    % 使用simple_one_for_one策略，适用于动态添加相同类型的子进程
    % 重启策略: 5次重启/10秒内
    % 子进程规格: dgiot_udp_broadcast工作进程
    ChildSpec = [dgiot:child_spec(dgiot_udp_broadcast, worker)],
    {ok, {{simple_one_for_one, 5, 10}, ChildSpec}}.