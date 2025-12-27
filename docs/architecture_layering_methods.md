# DG-IoT工程分层解耦方法总结

## 概述

本文档总结DG-IoT平台中使用的分层解耦方法，基于七层架构设计原则"分层解耦，各安其位，各司其职"。

## 七层架构概览

| 层级 | 关键模块 | 主要职责 | 解耦方法 |
|------|----------|----------|----------|
| **1. 通讯层** | `dgiot_modbusrtu_tcp.erl` | TCP/UDP连接管理、设备注册、原始数据转发 | 消息队列、原始数据封装 |
| **2. 协议层** | `modbus_rtu.erl` | Modbus RTU协议解析、数据封包/解包 | 钩子机制、模块化设计 |
| **3. 消息路由层** | `dgiot_modbusrtu_tcp.erl`中的消息路由函数 | MQTT消息路由、任务队列管理 | MQTT主题路由、父设备消息汇聚 |
| **4. 业务层** | `dgiot_task.erl` | 数据解码、属性计算、告警处理 | 插件化设计、配置驱动 |
| **5. 数据层** | `dgiot_tdengine_adapter.erl` | 时序数据存储、数据查询 | 标准API接口、适配器模式 |
| **6. 缓存层** | `dgiot_task.erl`中的缓存逻辑 | 实时数据缓存、设备状态缓存 | 缓存策略、数据合并 |
| **7. API层** | API模块 | 实时数据查询、历史数据查询 | RESTful API、权限控制 |

## 分层解耦方法详解

### 1. 通讯层解耦方法

#### 1.1 原始数据转发
```erlang
%% 通讯层只转发原始数据，不进行解码
Things = #{
    <<"raw_data">> => Buff,
    <<"data_type">> => <<"modbus_rtu">>,
    <<"product_id">> => ProductId,
    <<"dtu_addr">> => DtuAddr,
    <<"channel_id">> => ChannelId
},
send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, ProductId)
```

**解耦特点**：
- 不解析协议内容，只封装元数据
- 通过MQTT消息传递原始数据
- 支持多种设备注册方式（RegisterByIp、RegisterByPort、RegisterByRegular）

#### 1.2 设备注册分离
- **RegisterByIp**：基于IP地址注册
- **RegisterByPort**：基于端口信息注册
- **RegisterByRegular**：基于正则表达式注册

### 2. 协议层解耦方法

#### 2.1 钩子机制
```erlang
%% 协议层注册钩子
start_hook() ->
    % 注册数据源钩子
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:get_datasource/1),
    
    % 注册原始数据解析钩子
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:parse_raw_data/3),
    ok.
```

**解耦特点**：
- 通过钩子机制实现协议层与业务层的解耦
- 支持多种数据格式（原始字节、位、16位/32位有符号/无符号、浮点数）
- 模块化设计：编码器、解码器、工具模块分离

#### 2.2 协议解析分离
- **编码器模块**：`modbus_rtu_encoder.erl` - 负责数据编码
- **解码器模块**：`modbus_rtu_decoder.erl` - 负责数据解码
- **工具模块**：`modbus_rtu_utils.erl` - 提供通用工具函数

### 3. 业务层解耦方法

#### 3.1 插件化设计
```erlang
%% 业务层通过配置驱动处理数据
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>}} ->
                % 处理计算值属性
                handle_calculated_property(ProductId, DevAddr, X, Calculated, Acc);
            _ ->
                Acc
        end
    end, Calculated, Props).
```

**解耦特点**：
- 基于物模型配置驱动数据处理
- 支持多种策略：计算值、主动上报、采集值
- 统计类型分离：时长累加、次数累加

#### 3.2 数据处理流程分离
1. **数据采集**：`get_collection/4` - 从原始数据提取用户数据
2. **计算值处理**：`get_calculated/4` - 处理计算值属性
3. **存储值过滤**：`get_storage/2` - 筛选需要存储的数据
4. **告警处理**：通过MQTT发布告警消息

### 4. 数据层解耦方法

#### 4.1 适配器模式
```erlang
%% 数据层通过标准API接口
dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage)
```

**解耦特点**：
- 统一的数据库操作接口
- 支持多种数据库操作：查询、插入、更新
- SQL生成与执行分离

#### 4.2 查询构建器
```erlang
%% 查询构建器分离SQL生成逻辑
select(TableName, Query) ->
    Order = format_order(Query),
    Limit = format_limit(Query),
    Offset = format_offset(Query),
    Where = format_where(Query),
    <<"SELECT * FROM ", TableName/binary, Where/binary, Order/binary, Limit/binary, Offset/binary>>.
```

### 5. 层间通信接口

#### 5.1 通讯层 → 业务层
- **数据格式**：原始数据 + 元数据的Things映射
- **传输方式**：MQTT消息
- **接口示例**：
```erlang
Things = #{
    <<"raw_data">> => Buff,
    <<"data_type">> => <<"modbus_rtu">>,
    <<"product_id">> => ProductId,
    <<"dtu_addr">> => DtuAddr,
    <<"channel_id">> => ChannelId
}
```

#### 5.2 业务层 → 协议层
- **调用方式**：钩子机制
- **接口示例**：
```erlang
dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                    [ProductId, DevAddr, RawData])
```

#### 5.3 业务层 → 数据层
- **调用方式**：标准API
- **接口示例**：
```erlang
dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage)
```

### 6. 缓存层解耦方法

#### 6.1 双缓存策略
```erlang
%% 更新两个缓存：确保实时卡片API能获取到数据
%% 1. 更新标准缓存（?DGIOT_DATA_CACHE）
%% 2. 更新last_data缓存（供实时卡片API使用）
dgiot_data:put({last_data, DeviceId}, AllData),
dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {AllData, dgiot_datetime:now_ms()})
```

**解耦特点**：
- 缓存与业务逻辑分离
- 支持不同的缓存策略
- 数据合并机制避免频繁数据库写入

### 7. 消息路由解耦方法

#### 7.1 MQTT主题路由
```erlang
%% 基于MQTT主题的消息路由
ChildTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
ParentTopic = <<"$dg/thing/", ParentProductId/binary, "/", ParentDevAddr/binary, "/properties/report">>
```

**解耦特点**：
- 基于主题的消息路由
- 支持父子设备消息汇聚
- 灵活的消息分发机制

## 架构优势

### 1. 高内聚低耦合
- **各层职责明确**：每层只处理本层的职责
- **接口标准化**：层间通过标准接口通信
- **独立演进**：各层可以独立修改和升级

### 2. 可扩展性
- **插件化设计**：支持新协议、新功能插件
- **配置驱动**：通过配置调整系统行为
- **模块化架构**：易于添加新模块

### 3. 可维护性
- **代码复用**：通用功能模块化
- **测试友好**：各层可以独立测试
- **问题定位**：分层架构便于问题定位

### 4. 性能优化
- **缓存策略**：减少数据库访问
- **异步处理**：MQTT消息队列
- **批量操作**：数据合并减少写入次数

## 改进建议

### 1. 架构一致性检查
- [ ] 确保所有模块遵循七层架构原则
- [ ] 检查跨层调用是否符合规范
- [ ] 验证接口标准化程度

### 2. 协议层优化
- [ ] 完善钩子注册机制
- [ ] 支持更多协议类型
- [ ] 优化协议解析性能

### 3. 业务层扩展
- [ ] 支持更多计算策略
- [ ] 完善告警处理机制
- [ ] 优化统计计算性能

### 4. 数据层增强
- [ ] 支持更多数据库类型
- [ ] 优化查询性能
- [ ] 完善数据迁移工具

## 总结

DG-IoT平台通过七层架构设计实现了良好的分层解耦，主要方法包括：

1. **通讯层**：原始数据转发、设备注册分离
2. **协议层**：钩子机制、模块化设计
3. **业务层**：插件化设计、配置驱动
4. **数据层**：适配器模式、查询构建器
5. **缓存层**：双缓存策略、数据合并
6. **消息路由**：MQTT主题路由、父子设备汇聚

这些方法共同实现了"分层解耦，各安其位，各司其职"的架构目标，为系统提供了良好的可维护性、可扩展性和性能表现。
