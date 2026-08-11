# Modbus RTU 七层架构设计文档

## 概述

本文档定义了DG-IoT平台中Modbus RTU模块的七层架构设计，确保各层职责清晰、接口明确，便于团队协作和维护。

## 1. 七层架构图

```
┌─────────────────────────────────────────────────────────┐
│                      API层 (展示层)                      │
│  • 实时数据查询API                                       │
│  • 历史数据查询API                                       │
│  • 设备状态查询API                                       │
│  • 数据统计分析API                                       │
└───────────────┬─────────────────────────────────────────┘
                │ JSON/HTTP
┌───────────────▼─────────────────────────────────────────┐
│                    缓存层 (数据缓存)                     │
│  • 设备实时数据缓存 (last_data)                          │
│  • 设备状态缓存                                          │
│  • 产品配置缓存                                          │
└───────────────┬─────────────────────────────────────────┘
                │ 标准数据格式
┌───────────────▼─────────────────────────────────────────┐
│                    数据层 (持久化)                       │
│  • TDengine数据存储                                      │
│  • 时序数据管理                                          │
│  • 数据查询优化                                          │
└───────────────┬─────────────────────────────────────────┘
                │ 业务数据格式
┌───────────────▼─────────────────────────────────────────┐
│                    业务层 (核心逻辑)                     │
│  • 数据解码和验证 (dgiot_task)                           │
│  • 属性计算和派生值计算                                  │
│  • 告警规则处理                                          │
│  • 设备状态管理                                          │
└───────────────┬─────────────────────────────────────────┘
                │ MQTT消息
┌───────────────▼─────────────────────────────────────────┐
│                    消息路由层 (消息队列)                 │
│  • MQTT消息路由                                         │
│  • 任务队列管理                                         │
│  • 父设备消息汇聚                                        │
└───────────────┬─────────────────────────────────────────┘
                │ 原始数据+元数据
┌───────────────▼─────────────────────────────────────────┐
│                    协议层 (协议解析)                     │
│  • Modbus RTU协议解析 (modbus_rtu)                      │
│  • 数据封包/解包                                         │
│  • CRC校验                                               │
│  • 多种数据格式支持                                      │
└───────────────┬─────────────────────────────────────────┘
                │ 原始二进制数据
┌───────────────▼─────────────────────────────────────────┐
│                    通讯层 (设备连接)                     │
│  • TCP连接管理 (dgiot_modbusrtu_tcp)                    │
│  • 三种注册方式实现                                      │
│  • 原始数据接收和转发                                    │
└─────────────────────────────────────────────────────────┘
```

## 2. 各层详细职责

### 2.1 通讯层 (Communication Layer)
**文件**: `apps/dgiot_modbus/src/dgiot_modbusrtu_tcp.erl`

**职责**:
1. TCP连接管理（建立、维护、断开）
2. 三种设备注册方式实现：
   - `RegisterByIp`: 基于IP地址注册
   - `RegisterByPort`: 基于端口+注册报文注册
   - `RegisterByRegular`: 基于正则表达式注册
3. 原始数据接收和转发
4. 设备连接状态管理

**接口**:
- 输入: TCP原始二进制数据
- 输出: 标准格式的原始数据包
- 数据格式:
  ```erlang
  #{
    <<"raw_data">> => Buff,           % 原始二进制数据
    <<"data_type">> => <<"modbus_rtu">>, % 数据类型标识
    <<"product_id">> => ProductId,    % 产品ID
    <<"dtu_addr">> => DtuAddr,        % 设备地址
    <<"channel_id">> => ChannelId,    % 通道ID
    <<"env">> => Env                  % 环境信息（可选）
  }
  ```

**禁止**:
- 数据解码（调用`modbus_rtu:parse_frame`）
- 属性计算
- 业务逻辑处理

### 2.2 协议层 (Protocol Layer)
**文件**: `apps/dgiot_modbus/src/modbus/modbus_rtu/modbus_rtu.erl`

**职责**:
1. Modbus RTU协议解析
2. 数据封包/解包
3. CRC校验
4. 支持多种数据格式：
   - raw（原始值）
   - bit（位）
   - short16_AB/BA（16位有符号）
   - ushort16_AB/BA（16位无符号）
   - long32_ABCD/CDAB（32位有符号）
   - ulong32_ABCD/CDAB（32位无符号）
   - float32_ABCD/CDAB（32位浮点数）
5. 支持多种功能码：
   - 0x01: 读线圈寄存器
   - 0x02: 读离散输入寄存器
   - 0x03: 读保持寄存器
   - 0x04: 读输入寄存器
   - 0x05: 写单个线圈寄存器
   - 0x06: 写单个保持寄存器
   - 0x0f: 写多个线圈寄存器
   - 0x10: 写多个保持寄存器

**接口**:
- 输入: 原始数据包（来自通讯层）
- 输出: 解析后的结构化数据
- 函数:
  ```erlang
  % 解析数据帧
  parse_frame(Buff, Acc, State) -> {Rest, ParsedData}
  
  % 构建请求帧
  to_frame(DataSource) -> RequestFrame
  
  % 编码数据
  encode_data(Quality, Address, SlaveId, OperateType, Originaltype) -> EncodedData
  ```

### 2.3 消息路由层 (Message Routing Layer)
**职责**:
1. MQTT消息路由
2. 任务队列管理
3. 父设备消息汇聚
4. 消息优先级处理

**接口**:
- 输入: 解析后的结构化数据
- 输出: MQTT消息
- 关键函数:
  ```erlang
  % 发送聚合设备报告
  send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, _)
  ```

### 2.4 业务层 (Business Layer)
**文件**: `apps/dgiot_task/src/dgiot_task.erl`

**职责**:
1. 数据解码和验证
2. 属性计算和派生值计算
3. 告警规则处理
4. 设备状态管理
5. 业务规则执行

**接口**:
- 输入: 结构化数据（来自协议层）
- 输出: 业务处理后的数据
- 关键函数:
  ```erlang
  % 获取计算值
  get_calculated(ProductId, DevAddr, Calculated, Props) -> CalculatedData
  
  % 获取物模型属性
  get_props(ProductId) -> Props
  
  % 保存数据到TDengine
  save_td(ProductId, DevAddr, Ack, AppData) -> Result
  ```

### 2.5 数据层 (Data Layer)
**职责**:
1. TDengine数据存储
2. 时序数据管理
3. 数据查询优化
4. 数据一致性保证

**接口**:
- 输入: 业务处理后的数据
- 输出: 存储确认
- 关键模块: `dgiot_tdengine_adapter`

### 2.6 缓存层 (Cache Layer)
**职责**:
1. 设备实时数据缓存
2. 设备状态缓存
3. 产品配置缓存
4. 缓存同步和失效策略

**接口**:
- 缓存键:
  ```erlang
  % 实时数据缓存
  {last_data, DeviceId}
  
  % 标准数据缓存
  ?DGIOT_DATA_CACHE
  ```

### 2.7 API层 (API Layer)
**职责**:
1. 实时数据查询API
2. 历史数据查询API
3. 设备状态查询API
4. 数据统计分析API

**接口**:
- HTTP RESTful API
- WebSocket实时推送
- 数据导出功能

## 3. 层间数据格式

### 3.1 通讯层 → 协议层
```erlang
#{
  <<"raw_data">> => <<SlaveId:8, FunCode:8, Data/binary>>,
  <<"data_type">> => <<"modbus_rtu">>,
  <<"product_id">> => <<"product_id">>,
  <<"dtu_addr">> => <<"device_address">>,
  <<"channel_id">> => <<"channel_id">>
}
```

### 3.2 协议层 → 业务层
```erlang
#{
  <<"slave_id">> => 1,
  <<"func_code">> => 3,
  <<"address">> => 0,
  <<"registers">> => 2,
  <<"data">> => <<0, 100>>,
  <<"parsed_values">> => #{
    <<"temperature">> => 25.5,
    <<"humidity">> => 60.2
  }
}
```

### 3.3 业务层 → 数据层
```erlang
#{
  <<"device_id">> => <<"device_123">>,
  <<"timestamp">> => 1672531200000,
  <<"values">> => #{
    <<"temperature">> => 25.5,
    <<"humidity">> => 60.2,
    <<"status">> => <<"normal">>
  }
}
```

## 4. 三种注册方式详细说明

### 4.1 RegisterByIp（基于IP注册）
**适用场景**: 设备有固定IP地址
**注册流程**:
1. 设备通过TCP连接到服务器
2. 服务器获取设备IP地址作为设备地址
3. 根据IP地址查找对应的产品配置
4. 注册设备并建立连接

**关键代码**:
```erlang
init(#tcp{socket = Socket, state = #state{id = ChannelId, dtutype = Dtutype, regtype = <<"RegisterByIp">>} = State} = TCPState) ->
    DtuAddr = dgiot_utils:get_ip(Socket),
    handle_ip_registration(ChannelId, ProductId, DtuAddr, Dtutype, TCPState, State).
```

### 4.2 RegisterByPort（基于端口注册）
**适用场景**: NAT穿透，多设备共享IP
**注册流程**:
1. 设备发送注册报文
2. 服务器根据端口号+注册报文生成唯一设备地址
3. 解析注册报文获取产品信息
4. 注册设备并建立连接

**关键代码**:
```erlang
handle_port_registration(ChannelId, Buff, Head, Dtutype, Port, TCPState, State) ->
    case process_registration_packet(Buff, Head, Dtutype, Port) of
        {ok, ProductId, DeviceAddr, DeviceId} ->
            % 注册设备...
    end.
```

### 4.3 RegisterByRegular（基于正则表达式注册）
**适用场景**: 灵活的设备识别
**注册流程**:
1. 设备发送注册报文
2. 服务器使用正则表达式匹配注册报文
3. 从匹配结果中提取产品信息
4. 注册设备并建立连接

**关键代码**:
```erlang
handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State) ->
    case process_regular_registration_packet(Buff, Head, Dtutype) of
        {ok, ProductId, DeviceAddr, DeviceId} ->
            % 注册设备...
    end.
```

## 5. 上下行消息路由

### 5.1 上行消息（设备→平台）
```
设备 → 通讯层 → 协议层 → 消息路由层 → 业务层 → 数据层 → 缓存层 → API层
```

### 5.2 下行消息（平台→设备）
```
API层 → 业务层 → 消息路由层 → 协议层 → 通讯层 → 设备
```

## 6. 测试验证方案

### 6.1 单元测试
- 每层模块独立测试
- 接口一致性测试
- 错误处理测试

### 6.2 集成测试
- 端到端数据流测试
- 性能压力测试
- 异常场景测试

### 6.3 API测试
- API功能测试
- 数据一致性验证
- 性能基准测试

## 7. 实施计划

### 阶段一：架构完善和代码重构（2-3天）
1. 通讯层完善：确认三种注册方式完全分离
2. 协议层优化：明确协议解析职责
3. 业务层重构：实现Modbus数据解码
4. 数据层优化：完善数据存储策略

### 阶段二：消息路由和上下行通信（2天）
1. 上行消息路由实现
2. 下行消息路由实现
3. 消息队列优化

### 阶段三：设备上报完整流程（1-2天）
1. 设备注册流程完善
2. 数据上报流程优化
3. 状态管理流程实现

### 阶段四：API层和数据查询（2天）
1. 实时数据API开发
2. 历史数据API开发
3. 统计分析API开发

### 阶段五：测试验证和文档（2天）
1. 单元测试和集成测试
2. API测试
3. 文档编写

## 8. 总结

通过七层架构设计，实现了：
1. **职责清晰**：每层只负责单一职责
2. **接口明确**：层间通过标准接口通信
3. **易于维护**：模块化设计，便于独立演进
4. **可测试性**：每层可独立测试
5. **可扩展性**：支持新功能快速集成

此架构为Modbus RTU模块提供了坚实的基础，支持大规模设备连接和数据处理。
