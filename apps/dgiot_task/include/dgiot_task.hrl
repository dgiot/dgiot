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
%% distributed under the License is distributed on "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc 任务统计插件头文件
%% 定义任务统计相关的常量和宏

-author("johnliu").

%% 通道类型定义
-define(TYPE, <<"INSTRUCT">>).  %% 指令任务通道类型

%% ETS表名定义
-define(DGIOT_TASK, dgiot_task).        %% 任务客户端管理表
-define(DGIOT_PNQUE, dgiot_pnque).      %% PN队列管理表
-define(DGIOT_DATA_CACHE, dgiot_data_cache).  %% 数据缓存表

%% 任务名称生成宏
-define(TASK_NAME(Name), dgiot_utils:to_atom(lists:concat([dgiot_utils:to_atom(Name), "task"]))).

%% 任务监督者名称生成宏
-define(TASK_SUP(Name), dgiot_utils:to_atom(lists:concat(["dgiot_task_sup", dgiot_utils:to_atom(Name)]))).

%% 数据源表名定义
-define(DGIOT_DATASOURCE, dgiot_datasource).

%% 规则引擎相关定义
-define(RULE_ENGINE_TABLE, dgiot_rule_engine).  %% 规则引擎表
-define(DGIOT_RAW_DATA_PARSER, dgiot_raw_data_parser).  %% 原始数据解析钩子

%% 任务编排相关定义
-define(TASK_SCHEDULER_TABLE, dgiot_task_scheduler).  %% 任务调度表
-define(DEFAULT_TASK_INTERVAL, 5).  %% 默认任务间隔（秒）
-define(DEFAULT_TASK_ROUNDS, 1).    %% 默认任务轮次
-define(MAX_TASK_ORDER, 999).       %% 最大任务序号
