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

%% @doc 任务统计函数网关
%% 负责任务统计的接口暴露，遵循三层架构设计原则
-module(dgiot_task).
-include("dgiot_task.hrl").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

%% 客户端管理函数
-export([start/2, save_client/2, del_client/1]).
-export([send/3, get_pnque_len/1, save_pnque/4, get_pnque/1, del_pnque/1]).

%% 数据保存函数
-export([save_td/4, save_td_no_match/4, smart_save_td/4]).

%% 物模型相关函数
-export([get_props/1, get_control/3, get_collection/4, get_calculated/4, get_instruct/2, get_storage/2]).

%% 统计计算函数
-export([get_statistic/7, get_last_value/4, compare/3]).

%% 工具函数
-export([string2value/2, string2value/3]).

%% 协议处理函数
-export([needs_protocol_parsing/1, call_protocol_hook/4]).

%% 规则引擎函数
-export([rule_engine_transform/2, register_rule/2, get_rules/1]).

%% 任务编排函数
-export([schedule_tasks_from_thing_model/1, stop_tasks/1, parse_task_parameters/1]).

%% 任务编排内部函数（被timer:apply_interval动态调用）
-export([execute_task/2, get_current_round/2, update_round/3, execute_round/3]).
-export([generate_collection_command/4, send_collection_command/2]).

%% @doc 注册任务统计协议类型
%% 定义任务统计协议的基本信息，包括类型、列数、标题和描述
-protocol_type(#{
                 cType => <<"TASKSTATISTICS">>,  %% 协议类型代码
                 type => <<"TASKSTATISTICS">>,  %% 协议类型名称
                 colum => 10,  %% 显示列数
                 title => #{
                            zh => <<"任务统计"/utf8>>  %% 中文标题
                           },
                 description => #{
                                  zh => <<"任务统计"/utf8>>  %% 中文描述
                                 }
                }).

%% @doc 定义协议参数
%% 配置任务统计协议的可配置参数，包括统计类型、键、比较类型和值
-params(#{
          <<"type">> => #{
                          order => 1,  %% 参数顺序
                          type => string,
                          required => true,
                          default => #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},  %% 默认值
                          enum => [  %% 枚举选项
                                   #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},
                                   #{<<"value">> => <<"frequency">>, <<"label">> => <<"次数累加"/utf8>>}],
                          title => #{
                                     zh => <<"条件"/utf8>>
                                    },
                          description => #{
                                           zh => <<"条件"/utf8>>
                                          }
                         },
          <<"key">> => #{
                         order => 2,
                         type => string,
                         required => true,
                         default => <<"key"/utf8>>,
                         title => #{
                                    zh => <<"物模型标识符"/utf8>>
                                   },
                         description => #{
                                          zh => <<"统计的物模型标识符"/utf8>>
                                         }
                        },
          <<"comparetype">> => #{
                                 order => 3,
                                 type => string,
                                 required => true,
                                 default => #{<<"value">> => <<"EQ">>, <<"label">> => <<"等于"/utf8>>},
                                 enum => [  %% 比较类型枚举
                                          #{<<"value">> => <<"LT">>, <<"label">> => <<"小于"/utf8>>},
                                          #{<<"value">> => <<"LE">>, <<"label">> => <<"小于等于"/utf8>>},
                                          #{<<"value">> => <<"GT">>, <<"label">> => <<"大于"/utf8>>},
                                          #{<<"value">> => <<"GE">>, <<"label">> => <<"大于等于"/utf8>>},
                                          #{<<"value">> => <<"EQ">>, <<"label">> => <<"等于"/utf8>>},
                                          #{<<"value">> => <<"NE">>, <<"label">> => <<"不等于"/utf8>>}],
                                 title => #{
                                            zh => <<"条件"/utf8>>
                                           },
                                 description => #{
                                                  zh => <<"条件"/utf8>>
                                                 }
                                },
          <<"value">> => #{
                           order => 4,
                           type => string,
                           required => true,
                           default => <<"1">>,
                           title => #{
                                      zh => <<"值"/utf8>>
                                     },
                           description => #{
                                            zh => <<"物模型比较值"/utf8>>
                                           }
                          }
         }).

%%%===================================================================
%%% 客户端管理函数（转发到DAO层）
%%%===================================================================

%% @doc 启动任务客户端
%% 根据通道ID和产品ID列表启动对应的任务客户端
%% @param ChannelId 通道ID
%% @param ProductIds 产品ID列表
start(ChannelId, ProductIds) ->
    dgiot_task_dao:start(ChannelId, ProductIds).

%% @doc 保存客户端到任务列表
%% 将客户端ID保存到指定通道的任务客户端列表中
%% @param ChannelId 通道ID
%% @param ClientId 客户端ID
save_client(ChannelId, ClientId) ->
    dgiot_task_dao:save_client(ChannelId, ClientId).

%% @doc 删除通道的所有客户端
%% 停止并删除指定通道的所有任务客户端
%% @param ChannelId 通道ID
del_client(ChannelId) ->
    dgiot_task_dao:del_client(ChannelId).

%%%===================================================================
%%% PN队列管理函数（转发到DAO层）
%%%===================================================================

%% @doc 保存PN队列
%% 将产品设备对保存到DTU的PN队列中，并订阅相关MQTT主题
%% @param DtuProductId DTU产品ID
%% @param DtuAddr DTU地址
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
save_pnque(DtuProductId, DtuAddr, ProductId, DevAddr) ->
    dgiot_task_dao:save_pnque(DtuProductId, DtuAddr, ProductId, DevAddr).

%% @doc 获取PN队列长度
%% 获取指定DTU的PN队列长度
%% @param DtuId DTU设备ID
%% @return 队列长度
get_pnque_len(DtuId) ->
    dgiot_task_dao:get_pnque_len(DtuId).

%% @doc 获取PN队列
%% 轮询获取PN队列中的下一个产品设备对（循环队列）
%% @param DtuId DTU设备ID
%% @return {ProductId, DevAddr} | not_find
get_pnque(DtuId) ->
    dgiot_task_dao:get_pnque(DtuId).

%% @doc 删除PN队列
%% 删除指定DTU的PN队列
%% @param DtuId DTU设备ID
del_pnque(DtuId) ->
    dgiot_task_dao:del_pnque(DtuId).

%%%===================================================================
%%% 数据发送函数（转发到DAO层）
%%%===================================================================

%% @doc 发送数据到任务通道
%% 通过任务通道发送设备上报数据
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Payload 数据负载
send(ProductId, DevAddr, Payload) ->
    dgiot_task_dao:send(ProductId, DevAddr, Payload).

%%%===================================================================
%%% 数据保存函数（转发到服务层）
%%%===================================================================

%% @doc 保存数据到TDengine
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Ack 确认数据
%% @param AppData 应用数据
save_td(ProductId, DevAddr, Ack, AppData) ->
    dgiot_task_service:save_td(ProductId, DevAddr, Ack, AppData).

%% @doc 智能保存数据
%% 自动判断数据是否需要协议解析，智能路由处理
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Data 输入数据
%% @param Context 上下文信息
smart_save_td(ProductId, DevAddr, Data, Context) ->
    dgiot_task_service:smart_save_td(ProductId, DevAddr, Data, Context).

%% @doc 保存数据（无匹配模式）
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Ack 确认数据
%% @param AppData 应用数据
save_td_no_match(ProductId, DevAddr, Ack, AppData) ->
    dgiot_task_service:save_td_no_match(ProductId, DevAddr, Ack, AppData).

%%%===================================================================
%%% 物模型相关函数（转发到服务层）
%%%===================================================================

%% @doc 获取物模型属性列表
%% 查询产品的物模型属性配置
%% @param ProductId 产品ID
%% @return 物模型属性列表
get_props(ProductId) ->
    dgiot_task_service:get_props(ProductId).

%% @doc 获取控制值
%% 根据轮次、数据和控件模板生成控制值
%% @param Round 轮次
%% @param Data 原始数据
%% @param Control 控件模板字符串
%% @return 处理后的控制值
get_control(Round, Data, Control) ->
    dgiot_task_service:get_control(Round, Data, Control).

%% @doc 获取采集数据
%% 根据物模型配置获取用户数据
%% @param ProductId 产品ID
%% @param Dis 设备标识符列表
%% @param Payload 原始数据负载
%% @param Props 物模型属性列表
%% @return 处理后的数据映射
get_collection(ProductId, Dis, Payload, Props) ->
    dgiot_task_service:get_collection(ProductId, Dis, Payload, Props).

%% @doc 获取计算值
%% 根据物模型配置计算统计值，必须返回物模型里面的数据表示
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Calculated 已计算的数据
%% @param Props 物模型属性列表
%% @return 包含计算值的映射
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    dgiot_task_service:get_calculated(ProductId, DevAddr, Calculated, Props).

%% @doc 获取指令
%% 根据产品ID和轮次生成采集指令
%% @param ProductId 产品ID
%% @param Round 轮次
%% @return 指令列表
get_instruct(ProductId, Round) ->
    dgiot_task_service:get_instruct(ProductId, Round).

%% @doc 获取存储值
%% 从计算数据中筛选需要存储的物模型属性
%% @param Calculated 计算后的数据
%% @param Props 物模型属性列表
%% @return 需要存储的数据映射
get_storage(Calculated, Props) ->
    dgiot_task_service:get_storage(Calculated, Props).

%%%===================================================================
%%% 统计计算函数（转发到服务层）
%%%===================================================================

%% @doc 获取统计值
%% 根据统计类型（时长/次数）获取相应的统计值
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @param KeyValue 当前键值
%% @param DataSource 数据源配置
%% @param Acc 累计结果
%% @return 更新后的统计结果
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc) ->
    dgiot_task_service:get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc).

%% @doc 获取上次统计值
%% 从缓存或TDengine数据库获取上次统计的值
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @return 上次统计值
get_last_value(ProductId, DevAddr, Key, Identifier) ->
    dgiot_task_service:get_last_value(ProductId, DevAddr, Key, Identifier).

%% @doc 比较两个值
%% 根据比较类型比较两个值，支持 LT、LE、GT、GE、EQ、NE 六种比较类型
%% @param Value1 第一个值
%% @param CompareType 比较类型（<<"LT">>, <<"LE">>, <<"GT">>, <<"GE">>, <<"EQ">>, <<"NE">>）
%% @param Value2 第二个值
%% @return 布尔值，表示比较结果
compare(Value1, CompareType, Value2) ->
    dgiot_task_service:compare(Value1, CompareType, Value2).

%%%===================================================================
%%% 工具函数（转发到服务层）
%%%===================================================================

%% @doc 字符串转值
%% 将字符串转换为指定类型的值
%% @param Str 字符串
%% @param Type 类型
%% @return 转换后的值或error
string2value(Str, Type) ->
    dgiot_task_service:string2value(Str, Type).

%% @doc 字符串转值（带规格）
%% 将字符串转换为指定类型的值，考虑规格限制
%% @param Str 字符串
%% @param Type 类型
%% @param Specs 规格
%% @return 转换后的值或error
string2value(Str, Type, Specs) ->
    dgiot_task_service:string2value(Str, Type, Specs).

%%%===================================================================
%%% 协议处理函数（转发到服务层）
%%%===================================================================

%% @doc 判断数据是否需要协议解析
%% 根据数据特征判断是否需要调用协议钩子
%% @param Data 输入数据
%% @return true | false
needs_protocol_parsing(Data) ->
    dgiot_task_service:needs_protocol_parsing(Data).

%% @doc 调用协议钩子
%% 按需调用协议解析钩子
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Data 输入数据
%% @param Protocol 协议类型
%% @return {parsed, ParsedData} | {error, Reason} | {already_parsed, Data}
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    dgiot_task_service:call_protocol_hook(ProductId, DevAddr, Data, Protocol).

%%%===================================================================
%%% 规则引擎函数（转发到服务层）
%%%===================================================================

%% @doc 规则引擎转换
%% 将第三方协议数据转换为DG-IoT标准格式
%% @param ThirdPartyData 第三方数据
%% @param Protocol 协议类型
%% @return 转换后的标准数据
rule_engine_transform(ThirdPartyData, Protocol) ->
    dgiot_task_service:rule_engine_transform(ThirdPartyData, Protocol).

%% @doc 注册转换规则
%% 为指定协议注册转换规则
%% @param Protocol 协议类型
%% @param Rule 转换规则
register_rule(Protocol, Rule) ->
    dgiot_task_service:register_rule(Protocol, Rule).

%% @doc 获取协议规则
%% 获取指定协议的转换规则
%% @param Protocol 协议类型
%% @return 规则列表
get_rules(Protocol) ->
    dgiot_task_service:get_rules(Protocol).

%%%===================================================================
%%% 任务编排函数（转发到服务层）
%%%===================================================================

%% @doc 从物模型调度任务
%% 根据物模型配置自动编排采集任务
%% @param ProductId 产品ID
%% @return ok | {error, Reason}
schedule_tasks_from_thing_model(ProductId) ->
    dgiot_task_service:schedule_tasks_from_thing_model(ProductId).

%% @doc 停止任务
%% 停止指定产品的所有任务
%% @param ProductId 产品ID
%% @return ok
stop_tasks(ProductId) ->
    dgiot_task_service:stop_tasks(ProductId).

%% @doc 解析任务参数
%% 从物模型属性中解析任务参数
%% @param Props 物模型属性列表
%% @return 任务列表
parse_task_parameters(Props) ->
    dgiot_task_service:parse_task_parameters(Props).

%%%===================================================================
%%% 任务编排内部函数（转发到服务层）
%%%===================================================================

%% @doc 执行任务（内部函数，被timer:apply_interval调用）
%% 执行单个采集任务
%% @param ProductId 产品ID
%% @param Task 任务配置
execute_task(ProductId, Task) ->
    dgiot_task_service:execute_task(ProductId, Task).

%% @doc 获取当前轮次（内部函数）
%% 获取任务的当前执行轮次
%% @param ProductId 产品ID
%% @param Identifier 属性标识符
%% @return 当前轮次
get_current_round(ProductId, Identifier) ->
    dgiot_task_service:get_current_round(ProductId, Identifier).

%% @doc 更新轮次（内部函数）
%% 更新任务的执行轮次
%% @param ProductId 产品ID
%% @param Identifier 属性标识符
%% @param Round 新的轮次
update_round(ProductId, Identifier, Round) ->
    dgiot_task_service:update_round(ProductId, Identifier, Round).

%% @doc 执行轮次（内部函数）
%% 执行单个轮次的采集任务
%% @param ProductId 产品ID
%% @param Task 任务配置
%% @param Round 当前轮次
execute_round(ProductId, Task, Round) ->
    dgiot_task_service:execute_round(ProductId, Task, Round).

%% @doc 生成采集指令（内部函数）
%% 根据协议和配置生成采集指令
%% @param Protocol 协议类型
%% @param AccessMode 访问模式
%% @param DataSource 数据源配置
%% @param Round 当前轮次
%% @return 采集指令
generate_collection_command(Protocol, AccessMode, DataSource, Round) ->
    dgiot_task_service:generate_collection_command(Protocol, AccessMode, DataSource, Round).

%% @doc 发送采集指令（内部函数）
%% 发送采集指令到设备
%% @param ProductId 产品ID
%% @param Command 采集指令
send_collection_command(ProductId, Command) ->
    dgiot_task_service:send_collection_command(ProductId, Command).
