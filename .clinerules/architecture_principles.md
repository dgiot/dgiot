# 七层架构设计原则

## 概述

本文件定义了DG-IoT平台的七层架构设计原则，确保系统分层解耦、各层职责清晰、便于维护和扩展。

## 核心原则

### 1. 分层解耦，各安其位，各司其职

**原则**：系统分为七层，每层有明确的职责，层与层之间通过标准接口通信，实现高内聚、低耦合。

### 2. 七层架构定义

| 层级 | 职责 | 关键模块 | 禁止事项 |
|------|------|----------|----------|
| **1. 通讯层** | TCP/UDP连接管理、设备注册、原始数据转发 | `dgiot_modbusrtu_tcp.erl` | 禁止数据解码、禁止业务逻辑 |
| **2. 协议层** | 协议解析、数据封包/解包、CRC校验 | `modbus_rtu.erl` | 禁止业务逻辑、禁止数据存储 |
| **3. 消息路由层** | MQTT消息路由、任务队列管理、父设备消息汇聚 | `dgiot_modbusrtu_tcp.erl`中的消息路由函数 | 禁止数据处理、禁止业务逻辑 |
| **4. 业务层** | 数据解码、属性计算、告警处理、设备状态管理 | `dgiot_task.erl` | 禁止直接存储数据、禁止协议解析 |
| **5. 数据层** | 时序数据存储、数据查询、数据聚合 | `dgiot_tdengine_adapter.erl` | 禁止业务逻辑、禁止协议处理 |
| **6. 缓存层** | 实时数据缓存、设备状态缓存、会话管理 | `dgiot_task.erl`中的缓存逻辑 | 禁止持久化存储、禁止业务逻辑 |
| **7. API层** | 实时数据查询、历史数据查询、设备状态查询、控制指令下发 | API模块 | 禁止直接访问数据库、禁止业务逻辑 |

## 详细规范

### 1. 通讯层规范

#### 职责
- 管理TCP/UDP连接
- 处理设备注册（三种方式：RegisterByIp、RegisterByPort、RegisterByRegular）
- 转发原始数据，不进行解码
- 维护连接状态

#### 禁止事项
- ❌ 禁止调用协议解析函数（如`modbus_rtu:parse_frame`）
- ❌ 禁止进行属性计算
- ❌ 禁止直接保存数据到数据库
- ❌ 禁止处理业务逻辑

#### 示例
```erlang
%% ✅ 正确：只转发原始数据
Things = #{
    <<"raw_data">> => Buff,
    <<"data_type">> => <<"modbus_rtu">>,
    <<"product_id">> => ProductId,
    <<"dtu_addr">> => DtuAddr
},
send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, ProductId)

%% ❌ 错误：在通讯层解析数据
{_Rest, ParsedData} = modbus_rtu:parse_frame(Buff, #{}, #{}),
save_to_tdengine(ProductId, DtuAddr, ParsedData)
```

### 2. 协议层规范

#### 职责
- 协议解析和封装
- 数据格式转换
- CRC校验
- 错误检测

#### 禁止事项
- ❌ 禁止处理业务逻辑
- ❌ 禁止直接保存数据
- ❌ 禁止调用业务层函数
- ❌ 禁止管理设备状态

#### 示例
```erlang
%% ✅ 正确：只负责协议解析
parse_frame(Buff, Acc, State) ->
    % 解析Modbus RTU帧
    {Rest, ParsedData} = decode_modbus_frame(Buff),
    {Rest, ParsedData}.

%% ❌ 错误：在协议层处理业务逻辑
parse_frame(Buff, Acc, State) ->
    {Rest, ParsedData} = decode_modbus_frame(Buff),
    % 错误：调用业务层函数
    dgiot_task:save_td(ProductId, DevAddr, ParsedData, #{}),
    {Rest, ParsedData}.
```

### 3. 业务层规范

#### 职责
- 数据解码和转换
- 属性计算（包括计算值属性）
- 告警处理
- 设备状态管理
- 业务规则执行

#### 禁止事项
- ❌ 禁止直接操作数据库
- ❌ 禁止处理协议细节
- ❌ 禁止管理网络连接
- ❌ 禁止处理原始数据（应通过钩子调用协议层）

#### 示例
```erlang
%% ✅ 正确：通过钩子调用协议层解析原始数据
handle_raw_modbus_data(ProductId, DevAddr, RawData, Metadata) ->
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                            [ProductId, DevAddr, RawData]) of
        {ok, [ParsedData | _]} -> ParsedData;
        _ -> #{<<"raw_data">> => RawData}
    end.

%% ❌ 错误：在业务层直接解析协议
handle_raw_modbus_data(ProductId, DevAddr, RawData, Metadata) ->
    % 错误：业务层不应该知道协议细节
    {_Rest, ParsedData} = modbus_rtu:parse_frame(RawData, #{}, #{}),
    ParsedData.
```

## 接口规范

### 1. 层间通信接口

#### 通讯层 → 业务层
- **数据格式**: 原始数据 + 元数据
- **传输方式**: MQTT消息
- **接口示例**:
```erlang
Things = #{
    <<"raw_data">> => Buff,
    <<"data_type">> => <<"modbus_rtu">>,
    <<"product_id">> => ProductId,
    <<"dtu_addr">> => DtuAddr,
    <<"channel_id">> => ChannelId
}
```

#### 业务层 → 协议层
- **调用方式**: 钩子机制
- **接口示例**:
```erlang
dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                    [ProductId, DevAddr, RawData])
```

#### 业务层 → 数据层
- **调用方式**: 标准API
- **接口示例**:
```erlang
dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage)
```

### 2. 钩子注册规范

#### 协议层注册钩子
```erlang
start_hook() ->
    % 注册数据源钩子
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:get_datasource/1),
    
    % 注册原始数据解析钩子
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:parse_raw_data/3),
    ok.
```

## 检查清单

### 通讯层检查清单
- [ ] 是否只转发原始数据，不进行解码？
- [ ] 是否移除了协议解析函数调用？
- [ ] 是否移除了属性计算函数调用？
- [ ] 是否移除了直接数据保存调用？
- [ ] 三种注册方式是否完全分离？

### 协议层检查清单
- [ ] 是否只负责协议解析和封装？
- [ ] 是否注册了必要的钩子？
- [ ] 是否避免了业务逻辑？
- [ ] 是否支持多种数据格式？

### 业务层检查清单
- [ ] 是否通过钩子调用协议层解析数据？
- [ ] 是否正确处理计算值属性？
- [ ] 是否通过标准API保存数据？
- [ ] 是否处理了业务规则和告警？

### 数据层检查清单
- [ ] 是否只负责数据存储和查询？
- [ ] 是否提供了标准API接口？
- [ ] 是否避免了业务逻辑？
- [ ] 是否支持批量操作？

## 最佳实践

### 1. 代码重用
- 写代码前先查找现有实现
- 优先使用平台核心函数
- 避免重复造轮子

### 2. 错误处理
- 每层处理本层的错误
- 不跨层传播底层错误细节
- 提供有意义的错误信息

### 3. 日志记录
- 每层记录本层的操作日志
- 使用统一的日志格式
- 包含足够的上下文信息

### 4. 性能优化
- 各层独立优化
- 避免跨层性能影响
- 使用缓存减少重复计算

## 实施指南

### 1. 新模块开发
1. 确定模块所属层级
2. 遵循该层的职责规范
3. 定义清晰的接口
4. 实现本层功能
5. 编写单元测试

### 2. 现有模块重构
1. 分析当前模块违反的架构原则
2. 制定重构计划
3. 分步骤实施重构
4. 测试验证功能
5. 更新文档

### 3. 团队协作
1. 所有成员理解七层架构
2. 代码审查时检查架构符合性
3. 定期进行架构评审
4. 分享最佳实践

## 示例场景

### 场景：Modbus RTU设备数据上报

#### 正确流程
1. **通讯层**: 接收原始数据，打包成Things格式，通过MQTT转发
2. **业务层**: 接收Things，通过钩子调用协议层解析
3. **协议层**: 解析原始数据，返回结构化数据
4. **业务层**: 计算属性值，处理业务逻辑
5. **数据层**: 保存处理后的数据到TDengine
6. **缓存层**: 更新实时数据缓存
7. **API层**: 提供数据查询接口

#### 错误流程
- 通讯层直接解析数据 ❌
- 协议层直接保存数据 ❌
- 业务层直接操作数据库 ❌
- API层直接访问设备 ❌

## 更新记录

- 2025-12-24：创建七层架构设计原则文档
- 基于Modbus RTU模块重构经验总结

## 总结

**分层解耦，各安其位，各司其职**是DG-IoT平台架构设计的核心原则。通过严格遵守七层架构，系统可以实现：

1. **高内聚**：每层功能集中，职责明确
2. **低耦合**：层间通过标准接口通信，相互独立
3. **易维护**：每层可以独立修改和测试
4. **可扩展**：新功能可以在适当层级添加
5. **高可靠**：错误隔离，避免级联故障

遵循这些原则，团队可以构建高质量、可维护、可扩展的物联网平台。
