# dgiot_task - DG-IoT任务处理框架

## 概述

`dgiot_task`是DG-IoT开源版本的核心业务处理框架，专门设计用于处理设备数据采集、流式计算、数据转换和业务处理。本框架支持多种数据来源，包括设备采集通道、直接MQTT上报、第三方系统数据等。

## 核心特性

### 1. 多数据源支持
- **设备采集通道**：处理通过采集通道上报的数据
- **直接MQTT上报**：处理设备直接通过MQTT上报的数据
- **第三方系统数据**：处理来自第三方系统的数据（如DLINK协议等）
- **规则引擎转换**：支持通过规则引擎转换非标准协议数据

### 2. 智能数据处理
- **自动协议识别**：自动识别数据协议类型
- **按需协议解析**：只在需要时调用协议解析钩子
- **智能路由**：根据数据特征选择处理路径
- **流式计算**：支持简单的流式计算和业务处理
- **任务编排**：根据物模型的序号、间隔、轮次进行任务编排

### 3. 任务编排支持
- **物模型驱动**：根据物模型配置自动编排采集任务
- **序号控制**：支持按属性序号顺序执行
- **间隔控制**：支持不同属性采用不同的采集间隔
- **轮次管理**：支持多轮次任务执行和错峰调度

### 4. 规则引擎支持
- **数据格式转换**：将第三方协议转换为DG-IoT标准格式
- **协议适配**：适配各种非标准协议
- **字段映射**：支持字段重命名和格式转换
- **数据过滤**：支持数据过滤和验证

## 架构设计

### 1. 整体架构
```
任务编排 → 设备采集 → [协议识别] → [规则引擎转换] → [协议解析] → dgiot_task → 业务处理 → 存储
   ↑          ↑              ↑              ↑              ↑
物模型配置   采集指令      第三方协议      非标准数据      标准协议解析
```

### 2. 任务编排架构

#### 2.1 物模型驱动编排
`dgiot_task`根据物模型配置自动编排采集任务：

```erlang
%% 物模型任务编排配置示例
{
  "thing": {
    "properties": [
      {
        "identifier": "temperature",
        "dataForm": {
          "strategy": "采集值",
          "order": 1,          # 执行序号
          "interval": 5,       # 采集间隔（秒）
          "rounds": 10         # 执行轮次
        }
      },
      {
        "identifier": "humidity",
        "dataForm": {
          "strategy": "采集值", 
          "order": 2,
          "interval": 10,
          "rounds": 5
        }
      }
    ]
  }
}
```

#### 2.2 任务编排流程
```
1. 加载物模型配置 → 2. 解析任务参数 → 3. 创建任务队列 → 4. 定时执行 → 5. 结果处理
```

#### 2.3 错峰调度机制
```erlang
%% 错峰调度实现
schedule_tasks(Tasks) ->
    %% 根据任务间隔和优先级进行错峰调度
    SortedTasks = sort_tasks_by_priority(Tasks),
    ScheduledTasks = apply_staggered_scheduling(SortedTasks),
    
    %% 创建任务执行计划
    create_execution_plan(ScheduledTasks).
```

### 3. 数据处理流程

#### 3.1 任务编排驱动采集
```
物模型配置 → 任务编排 → 发送采集指令 → 设备响应 → 数据处理 → 存储
```

#### 3.2 标准协议数据（如Modbus RTU）
```
设备数据 → 协议识别 → 调用协议钩子解析 → dgiot_task处理 → 存储
```

#### 3.3 第三方协议数据（如DLINK）
```
第三方数据 → 协议识别 → 规则引擎转换 → 转换为标准格式 → dgiot_task处理 → 存储
```

#### 3.4 已解析数据（智能设备上报）
```
已解析数据 → 直接进入dgiot_task → 业务处理 → 存储
```

### 4. 任务编排实现

#### 4.1 任务编排核心
`dgiot_task`根据物模型的序号、间隔、轮次进行任务编排：

```erlang
%% 任务编排核心函数
schedule_tasks_from_thing_model(ProductId) ->
    %% 1. 加载物模型配置
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            %% 2. 解析任务参数
            Tasks = parse_task_parameters(Props),
            
            %% 3. 创建任务队列
            TaskQueue = create_task_queue(Tasks),
            
            %% 4. 启动任务调度
            start_task_scheduler(TaskQueue);
        Error ->
            Error
    end.

%% 解析任务参数
parse_task_parameters(Props) ->
    lists:filtermap(fun(Prop) ->
        case Prop of
            #{<<"dataForm">> := #{<<"strategy">> := <<"采集值">>} = DataForm} ->
                %% 提取任务参数
                Order = maps:get(<<"order">>, DataForm, 999),
                Interval = maps:get(<<"interval">>, DataForm, 5),
                Rounds = maps:get(<<"rounds">>, DataForm, 1),
                
                {true, #{
                    order => Order,
                    interval => Interval,
                    rounds => Rounds,
                    identifier => maps:get(<<"identifier">>, Prop)
                }};
            _ ->
                false
        end
    end, Props).
```

#### 4.2 任务执行控制
```erlang
%% 任务执行控制
execute_task(Task, Round) ->
    %% 1. 检查是否达到执行轮次
    case Round =< Task#task.rounds of
        true ->
            %% 2. 发送采集指令
            send_collection_command(Task),
            
            %% 3. 调度下一个执行
            schedule_next_execution(Task, Round + 1);
        false ->
            %% 任务完成
            task_completed(Task)
    end.
```

### 5. 规则引擎架构

#### 5.1 规则引擎定位
规则引擎负责将第三方非标准协议数据转换为DG-IoT标准格式：

```erlang
%% 规则引擎转换示例
transform_third_party_data(ThirdPartyData) ->
    %% 1. 识别协议类型
    Protocol = identify_protocol(ThirdPartyData),
    
    %% 2. 加载转换规则
    Rules = load_transformation_rules(Protocol),
    
    %% 3. 应用转换规则
    StandardData = apply_transformation_rules(ThirdPartyData, Rules),
    
    %% 4. 返回标准格式数据
    StandardData.
```

#### 5.2 转换规则配置
```json
{
  "protocol": "DLINK",
  "rules": [
    {
      "source": "temp",
      "target": "temperature",
      "transform": "value * 0.1"
    },
    {
      "source": "humi", 
      "target": "humidity",
      "transform": "value"
    },
    {
      "source": "ts",
      "target": "timestamp",
      "transform": "unix_to_iso8601(value)"
    }
  ]
}
```

## 核心功能

### 1. 任务编排功能

#### 1.1 物模型任务配置
`dgiot_task`支持在物模型中配置任务参数：

```json
{
  "identifier": "temperature",
  "dataForm": {
    "strategy": "采集值",
    "order": 1,          # 执行序号（1-999，越小优先级越高）
    "interval": 5,       # 采集间隔（秒）
    "rounds": 10,        # 执行轮次
    "timeout": 30,       # 超时时间（秒）
    "retry": 3           # 重试次数
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X00"
  }
}
```

#### 1.2 任务调度算法
```erlang
%% 任务调度算法
schedule_tasks(Tasks) ->
    %% 1. 按序号排序
    SortedTasks = lists:sort(fun(A, B) ->
        A#task.order =< B#task.order
    end, Tasks),
    
    %% 2. 计算错峰时间
    StaggeredTasks = apply_staggered_scheduling(SortedTasks),
    
    %% 3. 创建执行计划
    create_execution_plan(StaggeredTasks).

%% 错峰调度
apply_staggered_scheduling(Tasks) ->
    lists:foldl(fun(Task, {Acc, Offset}) ->
        %% 为每个任务添加错峰偏移
        NewTask = Task#task{start_offset = Offset},
        NewOffset = Offset + Task#task.interval div 2,  # 半间隔错峰
        {[NewTask | Acc], NewOffset}
    end, {[], 0}, Tasks).
```

#### 1.3 轮次管理
```erlang
%% 轮次管理
manage_rounds(Task, CurrentRound) ->
    case CurrentRound =< Task#task.rounds of
        true ->
            %% 执行当前轮次
            execute_round(Task, CurrentRound),
            
            %% 调度下一轮
            schedule_next_round(Task, CurrentRound + 1);
        false ->
            %% 所有轮次完成
            complete_task(Task)
    end.
```

### 2. 协议钩子机制

#### 2.1 钩子注册要求
每个协议层必须提供钩子给`dgiot_task`调用：

```erlang
%% 协议模块必须注册的钩子
start_hook() ->
    %% 1. 原始数据解析钩子（必须）
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"PROTOCOL_NAME">>}, 
                   fun protocol_module:parse_raw_data/3),
    ok.
```

#### 2.2 按需调用原则
`dgiot_task`智能判断何时需要调用协议钩子：

```erlang
%% 智能决策逻辑
process_data(ProductId, DevAddr, Data) ->
    case needs_protocol_parsing(Data) of
        true ->
            %% 需要解析：调用协议钩子
            Protocol = extract_protocol(Data),
            call_protocol_hook(ProductId, DevAddr, Data, Protocol);
        false ->
            %% 已解析数据：直接业务处理
            process_parsed_data(ProductId, DevAddr, Data)
    end.
```

### 3. 第三方数据支持

#### 3.1 第三方数据特征
- **非标准协议**：不符合DG-IoT协议规范
- **自定义格式**：设备厂商自定义的数据格式
- **需要转换**：需要通过规则引擎转换为标准格式

#### 3.2 处理流程
```erlang
%% 第三方数据处理
process_third_party_data(ProductId, DevAddr, ThirdPartyData) ->
    %% 1. 识别协议类型
    Protocol = identify_third_party_protocol(ThirdPartyData),
    
    %% 2. 通过规则引擎转换
    StandardData = rule_engine_transform(ThirdPartyData, Protocol),
    
    %% 3. 标准数据处理
    dgiot_task:process_standard_data(ProductId, DevAddr, StandardData).
```

### 4. 规则引擎实现

#### 4.1 规则引擎模块
```erlang
%% dgiot_rule_engine.erl
-module(dgiot_rule_engine).

%% API
-export([transform/2, register_rule/2, get_rules/1]).

%% 转换第三方数据
transform(ThirdPartyData, Protocol) ->
    %% 加载协议规则
    Rules = get_rules(Protocol),
    
    %% 应用转换规则
    apply_rules(ThirdPartyData, Rules).

%% 注册转换规则
register_rule(Protocol, Rule) ->
    dgiot_data:insert({rule_engine, Protocol}, Rule).

%% 获取协议规则
get_rules(Protocol) ->
    dgiot_data:get({rule_engine, Protocol}, []).
```

#### 4.2 规则应用
```erlang
%% 应用转换规则
apply_rules(Data, Rules) ->
    lists:foldl(fun(Rule, Acc) ->
        apply_single_rule(Data, Rule, Acc)
    end, #{}, Rules).

apply_single_rule(Data, #{source := Source, target := Target, transform := Transform}, Acc) ->
    %% 提取源字段值
    SourceValue = maps:get(Source, Data, undefined),
    
    %% 应用转换函数
    TargetValue = apply_transform(SourceValue, Transform),
    
    %% 添加到结果
    Acc#{Target => TargetValue}.
```

## 使用指南

### 1. 任务编排配置

#### 1.1 物模型任务配置
在物模型中配置采集任务参数：

```json
{
  "thing": {
    "properties": [
      {
        "identifier": "temperature",
        "name": "温度",
        "dataForm": {
          "strategy": "采集值",
          "order": 1,
          "interval": 5,
          "rounds": 100,
          "timeout": 30,
          "retry": 3
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X00",
          "originaltype": "float32_ABCD"
        }
      },
      {
        "identifier": "humidity",
        "name": "湿度", 
        "dataForm": {
          "strategy": "采集值",
          "order": 2,
          "interval": 10,
          "rounds": 50,
          "timeout": 30,
          "retry": 3
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X02",
          "originaltype": "float32_ABCD"
        }
      }
    ]
  }
}
```

#### 1.2 启动任务编排
```erlang
%% 启动产品任务编排
start_product_tasks(ProductId) ->
    dgiot_task:schedule_tasks_from_thing_model(ProductId).

%% 停止产品任务
stop_product_tasks(ProductId) ->
    dgiot_task:stop_tasks(ProductId).
```

### 2. 标准协议数据处理

#### 2.1 设备采集通道数据
```erlang
%% 采集通道调用
handle_channel_data(ChannelId, ProductId, DevAddr, Data) ->
    dgiot_task:save_td(ProductId, DevAddr, Data, #{source => <<"channel">>}).
```

#### 2.2 直接MQTT上报（标准协议）
```erlang
%% MQTT处理器
handle_mqtt_message(Topic, Payload) ->
    %% 解析MQTT消息
    Data = jsx:decode(Payload, [return_maps]),
    
    %% 从Topic提取元数据
    {ProductId, DevAddr} = extract_metadata_from_topic(Topic),
    
    %% 处理数据
    dgiot_task:save_td(ProductId, DevAddr, Data, #{source => <<"mqtt">>}).
```

### 3. 第三方协议数据处理

#### 3.1 DLINK协议示例
```erlang
%% DLINK协议处理器
handle_dlink_data(RawData) ->
    %% 1. 解析DLINK协议
    DlinkData = dlink_parser:parse(RawData),
    
    %% 2. 通过规则引擎转换为标准格式
    StandardData = dgiot_rule_engine:transform(DlinkData, <<"DLINK">>),
    
    %% 3. 提取元数据（DLINK协议可能包含在数据中）
    ProductId = maps:get(<<"product">>, StandardData, <<"default">>),
    DevAddr = maps:get(<<"device">>, StandardData, <<"unknown">>),
    
    %% 4. 处理数据
    dgiot_task:save_td(ProductId, DevAddr, StandardData, #{source => <<"dlink">>}).
```

#### 3.2 注册DLINK转换规则
```erlang
%% 初始化DLINK转换规则
init_dlink_rules() ->
    Rules = [
        #{source => <<"t">>, target => <<"temperature">>, transform => <<"value">>},
        #{source => <<"h">>, target => <<"humidity">>, transform => <<"value">>},
        #{source => <<"ts">>, target => <<"timestamp">>, transform => <<"unix_to_iso8601(value)">>}
    ],
    
    lists:foreach(fun(Rule) ->
        dgiot_rule_engine:register_rule(<<"DLINK">>, Rule)
    end, Rules).
```

### 4. 规则引擎配置

#### 4.1 配置文件示例
```json
{
  "rule_engine": {
    "DLINK": {
      "rules": [
        {
          "source": "t",
          "target": "temperature",
          "transform": "value",
          "description": "温度字段转换"
        },
        {
          "source": "h",
          "target": "humidity", 
          "transform": "value",
          "description": "湿度字段转换"
        }
      ]
    },
    "MODBUS_CUSTOM": {
      "rules": [
        {
          "source": "raw_value",
          "target": "value",
          "transform": "value * 0.1 + 25",
          "description": "自定义Modbus数据转换"
        }
      ]
    }
  }
}
```

#### 4.2 动态规则管理
```erlang
%% 动态添加规则
add_rule_dynamically(Protocol, Rule) ->
    dgiot_rule_engine:register_rule(Protocol, Rule).

%% 批量导入规则
import_rules_from_json(JsonFile) ->
    {ok, JsonData} = file:read_file(JsonFile),
    Rules = jsx:decode(JsonData, [return_maps]),
    
    maps:foreach(fun(Protocol, ProtocolRules) ->
        lists:foreach(fun(Rule) ->
            dgiot_rule_engine:register_rule(Protocol, Rule)
        end, ProtocolRules)
    end, Rules).
```

## 最佳实践

### 1. 任务编排设计

#### 1.1 任务参数设计
- **序号设计**：重要属性设置小序号，次要属性设置大序号
- **间隔设计**：频繁变化属性设置小间隔，稳定属性设置大间隔
- **轮次设计**：长期监测设置多轮次，临时任务设置少轮次
- **错峰设计**：多个设备错峰执行，避免同时采集造成压力

#### 1.2 性能优化
- **批量采集**：支持一次读取多个寄存器
- **缓存优化**：缓存物模型配置，减少数据库访问
- **异步执行**：耗时的采集任务异步执行

### 2. 协议设计原则

#### 2.1 标准协议设计
- **明确协议标识**：在数据中包含`protocol`字段
- **结构化数据**：使用标准的JSON结构
- **完整元数据**：包含产品ID、设备地址、时间戳等

#### 2.2 第三方协议适配
- **协议识别**：提供明确的协议识别机制
- **转换规则**：定义完整的字段映射规则
- **错误处理**：提供详细的转换错误信息

### 3. 性能优化

#### 3.1 任务编排优化
- **智能调度**：根据设备状态智能调整采集频率
- **规则缓存**：缓存转换规则，减少加载时间
- **预编译转换**：预编译转换函数，提高执行效率
- **批量处理**：支持批量数据转换

#### 2.2 协议解析优化
- **按需解析**：只在需要时调用协议解析
- **结果缓存**：缓存解析结果，避免重复解析
- **异步处理**：耗时的转换操作异步执行

### 3. 错误处理

#### 3.1 协议解析错误
```erlang
handle_protocol_error(Error, Data) ->
    ?LOG(error, "Protocol parsing failed: ~p, Data: ~p", [Error, Data]),
    
    %% 记录错误数据用于调试
    save_error_data(Data, Error),
    
    %% 返回错误信息
    {error, #{reason => Error, data => Data}}.
```

#### 3.2 规则转换错误
```erlang
handle_rule_error(Rule, Data, Error) ->
    ?LOG(error, "Rule transformation failed: Rule=~p, Error=~p", [Rule, Error]),
    
    %% 使用默认值或跳过该字段
    case Rule of
        #{default := DefaultValue} ->
            DefaultValue;
        _ ->
            undefined
    end.
```

## 故障排除

### 常见问题

#### 1. 协议识别失败
**症状**：无法识别数据协议类型

**解决方案**：
1. 检查数据是否包含`protocol`字段
2. 验证协议标识是否正确
3. 检查协议模块是否已注册钩子

#### 2. 规则转换失败
**症状**：第三方数据转换失败

**解决方案**：
1. 检查转换规则配置
2. 验证源字段是否存在
3. 检查转换函数语法

#### 3. 数据处理性能问题
**症状**：数据处理延迟高

**解决方案**：
1. 优化规则引擎性能
2. 启用结果缓存
3. 批量处理数据

### 调试命令
```bash
# 查看任务处理状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_task:get_stats().'

# 测试规则引擎转换
_build/emqx/rel/emqx/bin/emqx eval '
Data = #{<<"t">> => 25, <<"h">> => 60, <<"ts">> => 1672531200},
dgiot_rule_engine:transform(Data, <<"DLINK">>).
'

# 查看注册的协议钩子
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_hook:get_hooks(?DGIOT_RAW_DATA_PARSER).'
```

## 更新记录

### 2025-12-24
- **架构完善**：添加第三方数据支持和规则引擎
- **文档更新**：创建完整的README文档
- **协议钩子**：明确协议层钩子注册要求
- **规则引擎**：添加规则引擎架构和使用指南

### 2025-12-19
- **初始版本**：创建dgiot_task基础框架
- **流式计算**：实现基础流式计算功能
- **数据存储**：集成TDengine数据存储

## 贡献指南

1. **协议支持**：新增协议需要注册对应的解析钩子
2. **规则引擎**：新增第三方协议需要定义转换规则
3. **测试要求**：所有功能必须包含测试用例
4. **文档更新**：代码修改必须更新相关文档

## 许可证

本项目基于Apache License 2.0许可证开源。

## 联系我们

如有问题或建议，请通过以下方式联系：
- 项目仓库：https://gitee.com/dgiiot/dgiot
- 问题反馈：在仓库中提交Issue
- 文档改进：提交Pull Request更新文档
