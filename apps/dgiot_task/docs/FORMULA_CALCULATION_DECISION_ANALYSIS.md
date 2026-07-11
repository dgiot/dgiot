# dgiot_task公式计算决策机制分析

## 概述

本文档深入分析dgiot_task如何知道设备采集通道的值是否需要使用采集公式计算，以及这个决策是在物模型中定义还是由设备采集通道自主决定。

## 1. 核心问题澄清

### 问题：dgiot_task怎么知道设备采集通道的值是用采集公式计算过的还是没有计算过的，是在物模型一开始定义好，还是设备采集通道自主决定？

**答案：这个决策是在物模型中定义好的，dgiot_task通过解析物模型配置来决定是否需要计算。**

## 2. 决策机制分析

### 2.1 物模型定义的角色

#### 物模型中的计算策略定义
```json
{
  "identifier": "actual_temperature",
  "name": "实际温度",
  "dataForm": {
    "strategy": "计算值",  // 关键字段：标识需要计算
    "collection": "%%{raw_temperature} * 0.0625",  // 计算公式
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X0000"
  }
}
```

#### 物模型中的直接值定义
```json
{
  "identifier": "raw_temperature", 
  "name": "原始温度",
  "dataForm": {
    "strategy": "上报值",  // 关键字段：标识直接使用
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X0000"
  }
}
```

### 2.2 dgiot_task的决策流程

#### 决策流程代码分析
```erlang
%% 在dgiot_task_service.erl中的决策逻辑
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    %% 遍历物模型属性配置
    lists:foldl(fun(Prop, Acc) ->
        case maps:get(<<"dataForm">>, Prop, #{}) of
            #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} ->
                %% 需要计算：调用采集公式
                Value = calculate_collection_formula(Collection, Calculated, Prop),
                Acc#{maps:get(<<"identifier">>, Prop) => Value};
            #{<<"strategy">> := <<"上报值">>} ->
                %% 直接使用：从Calculated中获取
                Identifier = maps:get(<<"identifier">>, Prop),
                case maps:get(Identifier, Calculated, undefined) of
                    undefined -> Acc;
                    Value -> Acc#{Identifier => Value}
                end;
            _ ->
                %% 其他策略或未定义
                Acc
        end
    end, #{}, Props).
```

## 3. 物模型配置的决策权

### 3.1 物模型作为"计算蓝图"

#### 物模型定义了计算规则
```json
{
  "thing": {
    "properties": [
      {
        "identifier": "angular_x",
        "dataForm": {
          "strategy": "计算值",  // 决策：需要计算
          "collection": "block_data[0:2] * 0.1"
        }
      },
      {
        "identifier": "angular_y", 
        "dataForm": {
          "strategy": "计算值",  // 决策：需要计算
          "collection": "block_data[2:4] * 0.1"
        }
      },
      {
        "identifier": "block_data",
        "dataForm": {
          "strategy": "上报值"  // 决策：直接使用
        }
      }
    ]
  }
}
```

#### dgiot_task执行物模型定义
```erlang
%% dgiot_task严格遵循物模型定义
execute_thing_model(ProductId, DevAddr, RawData) ->
    %% 1. 获取物模型配置
    {ok, #{<<"thing">> := #{<<"properties">> := Props}}} = 
        dgiot_product:lookup_prod(ProductId),
    
    %% 2. 解析原始数据
    Calculated = parse_raw_data(RawData, Props),
    
    %% 3. 根据物模型策略处理每个属性
    process_properties_by_strategy(Props, Calculated).
```

### 3.2 设备采集通道的角色

#### 设备采集通道的职责
```erlang
%% 设备采集通道（如dgiot_modbusrtu_tcp.erl）的职责
handle_device_data(ChannelId, ProductId, DevAddr, RawData) ->
    %% 1. 转发原始数据到任务通道
    Things = #{
        <<"raw_data">> => RawData,
        <<"data_type">> => <<"modbus_rtu">>,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DevAddr
    },
    
    %% 2. 发送到dgiot_task（不决定计算策略）
    dgiot_client:send(TaskChannel, DeviceId, Topic, Things),
    
    %% 重要：设备采集通道不决定是否需要计算
    %% 它只负责转发原始数据
```

#### 设备采集通道的局限性
```erlang
%% 设备采集通道不知道计算策略的原因：
%% 1. 没有物模型配置信息
%% 2. 不知道业务计算需求
%% 3. 职责分离：只负责数据采集，不负责业务计算

%% 因此，设备采集通道无法自主决定是否需要计算
```

## 4. 决策权分配架构

### 4.1 三层决策架构

#### 第一层：物模型配置层（决策层）
```
职责：定义计算策略
位置：物模型JSON配置
决策内容：哪些属性需要计算，使用什么公式
```

#### 第二层：dgiot_task服务层（执行层）
```
职责：执行计算决策
位置：dgiot_task_service.erl
执行内容：根据物模型策略调用相应计算
```

#### 第三层：设备采集通道层（数据层）
```
职责：提供原始数据
位置：dgiot_modbusrtu_tcp.erl等
提供内容：原始采集数据，不参与决策
```

### 4.2 决策信息流

```
物模型配置 → dgiot_task解析 → 计算决策 → 执行计算
    ↓
设备采集通道 → 原始数据 → dgiot_task处理 → 结果存储
```

#### 详细信息流
```erlang
%% 1. 物模型配置（预定义）
ThingModel = #{
    properties => [
        #{identifier => "temp", strategy => "计算值", collection => "raw*0.1"},
        #{identifier => "raw", strategy => "上报值"}
    ]
}.

%% 2. dgiot_task加载配置
load_thing_model(ProductId) ->
    {ok, ThingModel} = dgiot_product:lookup_prod(ProductId).

%% 3. 设备采集数据
DeviceData = #{raw => 250}.

%% 4. dgiot_task根据配置决策
process_data(ThingModel, DeviceData) ->
    %% 根据strategy字段决定处理方式
    case maps:get(strategy, Prop) of
        "计算值" -> calculate(Prop, DeviceData);
        "上报值" -> direct_use(Prop, DeviceData)
    end.
```

## 5. 技术实现细节

### 5.1 物模型解析机制

#### 策略字段解析
```erlang
%% 解析物模型中的strategy字段
parse_strategy(Prop) ->
    case maps:get(<<"dataForm">>, Prop, #{}) of
        #{<<"strategy">> := Strategy} ->
            %% 有效的策略字段
            {ok, Strategy};
        _ ->
            %% 默认策略：上报值
            {ok, <<"上报值">>}
    end.
```

#### 计算公式提取
```erlang
%% 提取计算公式（如果存在）
extract_collection_formula(Prop) ->
    case maps:get(<<"dataForm">>, Prop, #{}) of
        #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} ->
            %% 计算值属性：需要公式
            {ok, Collection};
        #{<<"strategy">> := <<"计算值">>} ->
            %% 计算值属性但没有公式：错误
            {error, missing_collection_formula};
        _ ->
            %% 非计算值属性：不需要公式
            {ok, undefined}
    end.
```

### 5.2 计算执行机制

#### 计算值属性处理
```erlang
%% 处理计算值属性
handle_calculated_property(Prop, Calculated) ->
    Identifier = maps:get(<<"identifier">>, Prop),
    Collection = maps:get(<<"collection">>, maps:get(<<"dataForm">>, Prop)),
    
    %% 执行计算公式
    case dgiot_task_service:string2value(Collection, <<"float">>, #{}) of
        {ok, Value} ->
            %% 计算成功
            #{Identifier => Value};
        {error, Reason} ->
            %% 计算失败
            ?LOG(error, "计算属性~p失败: ~p", [Identifier, Reason]),
            #{}
    end.
```

#### 上报值属性处理
```erlang
%% 处理上报值属性
handle_reported_property(Prop, Calculated) ->
    Identifier = maps:get(<<"identifier">>, Prop),
    
    %% 直接从Calculated中获取值
    case maps:get(Identifier, Calculated, undefined) of
        undefined ->
            %% 值不存在
            ?LOG(debug, "属性~p的值不存在", [Identifier]),
            #{};
        Value ->
            %% 值存在，直接使用
            #{Identifier => Value}
    end.
```

## 6. 架构优势分析

### 6.1 决策集中化的优势

#### 优势1：配置驱动，灵活可变
```erlang
%% 通过修改物模型配置即可改变计算策略
%% 无需修改代码，无需重启服务

%% 示例：将温度计算从摄氏度改为华氏度
%% 旧配置：{"collection": "raw * 0.1"}
%% 新配置：{"collection": "raw * 0.1 * 1.8 + 32"}
%% 效果：立即生效，所有设备自动使用新公式
```

#### 优势2：职责清晰，易于维护
```
物模型配置：定义"做什么"（计算策略）
dgiot_task：执行"怎么做"（计算实现）
设备通道：提供"原材料"（原始数据）

各层职责清晰，便于独立维护和升级
```

#### 优势3：一致性保证
```erlang
%% 所有设备使用相同的决策逻辑
%% 避免不同设备通道实现不一致

%% 统一通过物模型配置决定计算策略
%% 保证整个系统计算逻辑的一致性
```

### 6.2 与设备自主决策的对比

#### 设备自主决策的问题
```erlang
%% 假设设备通道自主决定计算策略的问题：

%% 问题1：策略不一致
Device1: 决定计算温度
Device2: 决定不计算温度
Device3: 使用不同的温度计算公式

%% 问题2：配置困难
%% 需要在每个设备通道中配置计算策略
%% 难以统一管理和修改

%% 问题3：业务逻辑耦合
%% 设备通道需要了解业务计算需求
%% 违反分层架构原则
```

#### 物模型集中决策的优势
```erlang
%% 物模型集中决策的优势：

%% 优势1：统一管理
%% 所有设备的计算策略在物模型中统一管理
%% 一处修改，处处生效

%% 优势2：业务分离
%% 设备通道只负责数据采集
%% 业务计算逻辑在dgiot_task中处理

%% 优势3：灵活扩展
%% 新增计算属性只需修改物模型配置
%% 无需修改设备通道代码
```

## 7. 实际应用示例

### 7.1 温度传感器示例

#### 物模型配置
```json
{
  "identifier": "actual_temperature",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{raw_temperature} * 0.0625",
    "unit": "°C"
  }
}
```

#### dgiot_task处理流程
```erlang
%% 1. 解析物模型，发现strategy="计算值"
%% 2. 提取计算公式："%%{raw_temperature} * 0.0625"
%% 3. 从Calculated中获取raw_temperature值
%% 4. 执行计算：raw_value * 0.0625
%% 5. 存储计算结果
```

### 7.2 角度传感器示例

#### 物模型配置
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"
  }
}
```

#### dgiot_task处理流程
```erlang
%% 1. 解析物模型，发现strategy="计算值"
%% 2. 提取计算公式："block_data[0:2] * 0.1"
%% 3. 从数据块中提取block_data[0:2]
%% 4. 执行计算：extracted_value * 0.1
%% 5. 存储计算结果
```

### 7.3 直接值示例

#### 物模型配置
```json
{
  "identifier": "device_status",
  "dataForm": {
    "strategy": "上报值"
  }
}
```

#### dgiot_task处理流程
```erlang
%% 1. 解析物模型，发现strategy="上报值"
%% 2. 直接从Calculated中获取device_status值
%% 3. 直接存储，不进行计算
```

## 8. 总结

### 8.1 核心结论

**dgiot_task通过解析物模型中的`strategy`字段来知道设备采集通道的值是否需要计算：**

1. **`strategy = "计算值"`**：需要调用采集公式计算
2. **`strategy = "上报值"`**：直接使用，不需要计算

**这个决策是在物模型中预先定义好的，不是由设备采集通道自主决定的。**

### 8.2 决策权分配

#### 物模型配置层（决策权）
- **决定**：哪些属性需要计算
- **定义**：使用什么计算公式
- **配置**：计算参数和单位

#### dgiot_task服务层（执行权）
- **解析**：物模型配置
- **执行**：计算决策
- **处理**：计算公式

#### 设备采集通道层（数据权）
- **提供**：原始采集数据
- **不参与**：计算决策
- **不决定**：是否需要计算

### 8.3 架构设计意义

#### 1. 配置驱动架构
- **灵活性**：通过配置改变计算逻辑
- **可维护性**：无需修改代码即可调整计算
- **一致性**：统一的计算决策逻辑

#### 2. 职责分离架构
- **物模型**：定义计算策略（做什么）
- **dgiot_task**：执行计算（怎么做）
- **设备通道**：提供数据（原材料）

#### 3. 集中决策优势
- **统一管理**：所有计算策略集中管理
- **易于扩展**：新增计算属性只需配置
- **降低复杂度**：设备通道无需了解业务逻辑

### 8.4 实际应用价值

#### 对设备厂商
- **简化设备开发**：设备只需提供原始数据
- **提高兼容性**：同一设备支持不同计算需求
- **降低维护成本**：计算逻辑可远程配置更新

#### 对系统集成商
- **灵活配置**：根据客户需求配置计算逻辑
- **快速部署**：通过配置快速适配不同场景
- **统一管理**：集中管理所有设备的计算策略

#### 对最终用户
- **定制化计算**：根据实际需求定制计算公式
- **实时调整**：可随时调整计算参数
- **透明可控**：清楚知道每个值的计算方式

#### 对开发团队
- **清晰架构**：各层职责明确
- **易于调试**：计算逻辑可配置、可追踪
- **高效开发**：新增计算需求只需配置，无需编码

**最终结论**：DG-IoT平台通过物模型配置集中定义计算策略，dgiot_task服务层解析并执行这些策略，设备采集通道只负责提供原始数据。这种设计实现了计算决策的集中化、配置化和灵活化，是工业物联网平台架构的优秀实践。
