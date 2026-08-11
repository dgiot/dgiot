# DG-IoT物模型与解码器关系分析

## 概述

本文档分析DG-IoT平台中物模型与解码器的关系，以及采集公式变量的层级问题。

## 1. 物模型与解码器关系

### 1.1 一个物模型可以有多种解码器吗？

**答案：是的，一个物模型可以支持多种解码器，但通常一个物模型对应一个主要解码器。**

#### 技术实现分析

##### 1.1.1 协议钩子机制
DG-IoT使用钩子（Hook）机制实现协议解析的插件化：

```erlang
%% 在dgiot_task.erl中调用协议钩子
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    dgiot_task_service:call_protocol_hook(ProductId, DevAddr, Data, Protocol).

%% 在dgiot_task_service.erl中的实现
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, 
                            [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} -> {parsed, ParsedData};
        _ -> {error, no_parser_found}
    end.
```

##### 1.1.2 多解码器注册机制
不同协议可以注册自己的解码器钩子：

```erlang
%% Modbus RTU解码器注册
start_hook() ->
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:parse_frame/3),
    ok.

%% BACnet解码器注册
start_hook() ->
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"BACNET">>}, 
                   fun bacnet_decoder:parse_frame/3),
    ok.
```

##### 1.1.3 物模型配置指定解码器
物模型通过`dataForm.protocol`字段指定使用的解码器：

```json
{
  "identifier": "temperature",
  "dataForm": {
    "strategy": "采集值",
    "protocol": "MODBUSRTU"  # 指定使用Modbus RTU解码器
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X00"
  }
}
```

#### 1.1.4 多解码器支持场景

**场景1：同一设备支持多种协议**
```json
{
  "productId": "multi_protocol_device",
  "thing": {
    "properties": [
      {
        "identifier": "modbus_data",
        "dataForm": {"protocol": "MODBUSRTU"}
      },
      {
        "identifier": "bacnet_data", 
        "dataForm": {"protocol": "BACNET"}
      }
    ]
  }
}
```

**场景2：协议升级兼容**
```json
{
  "productId": "legacy_device",
  "thing": {
    "properties": [
      {
        "identifier": "data_v1",
        "dataForm": {"protocol": "LEGACY_V1"}
      },
      {
        "identifier": "data_v2",
        "dataForm": {"protocol": "MODERN_V2"}
      }
    ]
  }
}
```

### 1.2 实际限制和最佳实践

#### 限制因素
1. **数据源一致性**：同一数据包通常只包含一种协议格式
2. **设备能力限制**：大多数设备只支持一种通信协议
3. **配置复杂度**：多解码器增加配置和维护复杂度

#### 最佳实践
1. **一个物模型一个主解码器**：简化配置和维护
2. **协议转换网关**：在网关层进行协议转换，统一为一种协议
3. **版本管理**：通过物模型版本管理不同协议版本

## 2. 采集公式变量层级分析

### 2.1 采集公式变量的三个层级

#### 层级1：解码器级别（Protocol Level）
**作用范围**：协议解析过程中使用的变量

**示例**：
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"  # block_data是解码器级别的变量
  },
  "dataSource": {
    "slaveid": "block_data",  # 指向解码器解析出的数据块
    "address": "0"
  }
}
```

**特点**：
- 变量来自协议解码结果
- 作用域限于当前数据包解析
- 通常是二进制数据的结构化表示

#### 层级2：设备级别（Device Level）
**作用范围**：单个设备的历史数据和状态

**示例**：
```json
{
  "identifier": "temperature_trend",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{current_temp} - %%{last_hour_avg_temp}"  # 使用设备历史数据
  }
}
```

**变量来源**：
1. **当前数据包解析结果**
2. **设备历史数据缓存**
3. **设备状态信息**

**实现机制**：
```erlang
%% 在dgiot_task_service.erl中获取设备级别变量
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    %% Calculated包含当前解析结果
    %% 可以查询设备历史数据
    LastHourData = get_device_history(ProductId, DevAddr, "1h"),
    AvgTemp = calculate_average(LastHourData),
    
    %% 合并到变量环境
    Env = Calculated#{<<"last_hour_avg_temp">> => AvgTemp},
    
    %% 公式计算
    calculate_formulas(Props, Env).
```

#### 层级3：设备组级别（Device Group Level）
**作用范围**：同一产品类型的所有设备

**示例**：
```json
{
  "identifier": "relative_humidity",
  "dataForm": {
    "strategy": "计算值", 
    "collection": "%%{absolute_humidity} / %%{group_max_humidity} * 100"
  }
}
```

**变量来源**：
1. **设备组统计信息**
2. **产品全局配置**
3. **设备间关系数据**

**实现机制**：
```erlang
%% 获取设备组级别变量
get_group_variables(ProductId) ->
    %% 查询所有同产品设备
    AllDevices = dgiot_device:get_devices_by_product(ProductId),
    
    %% 计算组统计信息
    GroupStats = calculate_group_statistics(AllDevices),
    
    %% 返回组级别变量
    #{
        <<"group_max_humidity">> => maps:get(max_humidity, GroupStats),
        <<"group_avg_temperature">> => maps:get(avg_temperature, GroupStats),
        <<"device_count">> => length(AllDevices)
    }.
```

### 2.2 变量作用域和生命周期

#### 作用域对比
| 变量层级 | 作用域 | 生命周期 | 数据来源 |
|----------|--------|----------|----------|
| **解码器级别** | 当前数据包解析 | 短暂（毫秒级） | 原始数据包解析结果 |
| **设备级别** | 单个设备 | 中长期（小时/天） | 设备历史数据、状态、配置 |
| **设备组级别** | 同产品所有设备 | 长期（天/月） | 设备组统计、产品配置 |

#### 变量访问优先级
```
当前数据包解析变量 → 设备历史变量 → 设备组变量 → 全局配置变量
```

### 2.3 实际应用示例

#### 示例1：解码器级别变量计算
```json
{
  "identifier": "power_factor",
  "dataForm": {
    "strategy": "计算值",
    "collection": "active_power / sqrt(active_power * active_power + reactive_power * reactive_power)"
  }
}
```
**说明**：`active_power`和`reactive_power`都是当前数据包解析出的变量。

#### 示例2：设备级别变量计算
```json
{
  "identifier": "energy_consumption_today",
  "dataForm": {
    "strategy": "计算值", 
    "collection": "%%{current_energy} - %%{energy_at_midnight}"
  }
}
```
**说明**：`energy_at_midnight`需要从设备历史数据中查询。

#### 示例3：设备组级别变量计算
```json
{
  "identifier": "efficiency_score",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{output_power} / %%{group_avg_power} * 100"
  }
}
```
**说明**：`group_avg_power`需要计算同产品所有设备的平均功率。

### 2.4 技术实现细节

#### 变量解析机制
```erlang
%% 在dgiot_task_service.erl中的变量解析
resolve_variables(Formula, Context) ->
    %% Context包含多个层级的变量
    #{device_vars := DeviceVars,
      group_vars := GroupVars,
      decoder_vars := DecoderVars} = Context,
    
    %% 合并变量，优先级：解码器 > 设备 > 设备组
    AllVars = maps:merge(GroupVars, maps:merge(DeviceVars, DecoderVars)),
    
    %% 变量替换
    replace_variables(Formula, AllVars).
```

#### 变量存储和查询
```erlang
%% 解码器级别变量（临时存储）
DecoderVars = parse_raw_data(RawData, Protocol),

%% 设备级别变量（缓存存储）
DeviceVars = case dgiot_data:get({device_vars, DeviceId}) of
    not_find -> #{};
    Vars -> Vars
end,

%% 设备组级别变量（定期计算缓存）
GroupVars = case dgiot_data:get({group_vars, ProductId}) of
    not_find -> calculate_group_vars(ProductId);
    Vars -> Vars
end.
```

### 2.5 配置示例

#### 完整物模型配置示例
```json
{
  "productId": "smart_meter_001",
  "productName": "智能电表",
  "thing": {
    "properties": [
      {
        "identifier": "voltage",
        "name": "电压",
        "dataForm": {
          "strategy": "采集值",
          "protocol": "MODBUSRTU",
          "accessMode": "r"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X0000",
          "originaltype": "uint16"
        },
        "dataType": {
          "type": "float",
          "specs": {
            "unit": "V",
            "min": 0,
            "max": 300,
            "precision": 1
          }
        }
      },
      {
        "identifier": "power_factor",
        "name": "功率因数", 
        "dataForm": {
          "strategy": "计算值",
          "collection": "active_power / sqrt(active_power*active_power + reactive_power*reactive_power)"
        },
        "dataType": {
          "type": "float",
          "specs": {
            "unit": "",
            "min": 0,
            "max": 1,
            "precision": 3
          }
        }
      },
      {
        "identifier": "efficiency_rank",
        "name": "能效排名",
        "dataForm": {
          "strategy": "计算值",
          "collection": "%%{active_power} / %%{group_avg_power} * 100"
        },
        "dataType": {
          "type": "int",
          "specs": {
            "unit": "%",
            "min": 0,
            "max": 200
          }
        }
      }
    ]
  }
}
```

## 3. 总结

### 3.1 物模型与解码器关系总结

1. **一个物模型可以支持多种解码器**，但通常一个物模型对应一个主解码器
2. **通过协议钩子机制**实现解码器的插件化注册和调用
3. **物模型配置指定解码器**：通过`dataForm.protocol`字段
4. **多解码器适用场景**：协议升级兼容、多协议设备支持

### 3.2 采集公式变量层级总结

#### 三个层级：
1. **解码器级别**：当前数据包解析出的变量，作用域短暂
2. **设备级别**：单个设备的历史数据和状态，作用域中长期
3. **设备组级别**：同产品所有设备的统计信息，作用域长期

#### 技术特点：
1. **变量优先级**：解码器变量 > 设备变量 > 设备组变量
2. **生命周期管理**：不同层级变量有不同的存储和缓存策略
3. **灵活组合**：可以在公式中混合使用不同层级的变量

#### 实际应用：
1. **简单计算**：使用解码器级别变量进行实时计算
2. **趋势分析**：使用设备级别变量进行历史趋势计算
3. **对比分析**：使用设备组级别变量进行设备间对比

### 3.3 架构优势

1. **灵活性**：支持多解码器和多层级变量计算
2. **扩展性**：通过钩子机制轻松添加新解码器
3. **性能优化**：不同层级变量采用不同的缓存策略
4. **配置驱动**：通过物模型配置定义复杂计算逻辑

### 3.4 最佳实践建议

1. **解码器选择**：一个物模型使用一个主解码器，保持配置简洁
2. **变量层级使用**：
   - 实时计算使用解码器级别变量
   - 历史分析使用设备级别变量  
   - 对比分析使用设备组级别变量
3. **性能考虑**：
   - 解码器变量：临时存储，不持久化
   - 设备变量：缓存存储，定期清理
   - 设备组变量：定期计算，长期缓存

DG-IoT平台的物模型和解码器设计体现了高度的灵活性和扩展性，能够满足各种复杂的工业物联网场景需求。
