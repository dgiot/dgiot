# DG-IoT Modbus插件

## 概述

DG-IoT Modbus插件是一个完整的Modbus协议支持模块，包含Modbus RTU/TCP协议解析、数据采集、设备管理等功能。本插件基于DG-IoT平台的配置驱动架构设计，支持灵活的物模型配置和自动化的数据处理。

## 核心特性

### 1. 完整的Modbus协议支持
- **Modbus RTU**：串口通信协议
- **Modbus TCP**：网络通信协议  
- **多种功能码**：支持读线圈、读保持寄存器、写单个寄存器等
- **多种数据格式**：支持原始字节、位数据、16/32位整数、浮点数等

### 2. 配置驱动架构
- **物模型配置**：所有处理逻辑在物模型中定义
- **设备注册时加载**：设备注册时加载完整的物模型配置
- **运行时自动处理**：基于配置自动执行协议解析和数据处理

### 3. 分层架构设计
```
通讯层 (dgiot_modbusrtu_tcp) → 协议层 (modbus_rtu) → 业务层 (dgiot_task) → 数据层
```

### 4. 数据块处理支持
- **数据块模式**：支持读取多个寄存器的数据块
- **自动拆分**：根据物模型配置自动拆分数据块
- **计算值支持**：支持基于数据块的计算值属性

## 架构设计

### 1. 配置驱动设计理念

**核心原则**：在设备登录注册的时候，所有处理逻辑就已经在物模型中描述清楚了。

#### 1.1 物模型配置示例
```json
{
  "productId": "feeb43bffb",
  "thing": {
    "properties": [
      {
        "identifier": "temperature",
        "dataForm": {
          "strategy": "采集值",
          "protocol": "MODBUSRTU"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X00",
          "originaltype": "float32_ABCD"
        }
      },
      {
        "identifier": "block_data",
        "dataForm": {
          "strategy": "采集值",
          "protocol": "MODBUSRTU"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X00",
          "key": "block_data",
          "originaltype": "raw"
        }
      },
      {
        "identifier": "angular_x",
        "dataForm": {
          "strategy": "计算值",
          "collection": "block_data[0:2] * 0.1"
        }
      }
    ]
  }
}
```

#### 1.2 设备注册流程
```erlang
%% 设备注册时加载物模型配置
register_client(ChannelId, ProductId, DtuAddr, DtuIp, Dtutype) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    
    % 加载物模型配置
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            % 创建设备并保存物模型配置
            dgiot_device:create_device(#{
                <<"product">> => ProductId,
                <<"thing">> => #{<<"properties">> => Props}
            })
    end.
```

### 2. 数据处理流程

#### 2.1 统一数据处理入口
设备采集通道调用统一的处理函数：
```erlang
%% 设备采集通道调用
process_device_data(ProductId, DevAddr, RawData) ->
    % 调用统一的处理函数
    dgiot_task:save_td(ProductId, DevAddr, #{
        <<"raw_data">> => RawData,
        <<"data_type">> => <<"modbus_rtu">>,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DevAddr
    }, #{}).
```

#### 2.2 save_td函数功能
`dgiot_task:save_td/4` 函数提供完整的数据处理：
1. **流式计算**：`get_collection`、`get_calculated`
2. **缓存管理**：`merge_cache_data`、`save_cache_data`
3. **数据存储**：`dgiot_tdengine_adapter:save`
4. **业务处理**：告警、实时数据推送

### 3. 数据块处理架构

#### 3.1 数据块处理模块
- **模块**：`modbus_rtu_data_blocks.erl`
- **功能**：数据块拆分、合并、缓存管理
- **配置驱动**：根据物模型配置自动处理

#### 3.2 数据块处理流程
```erlang
%% modbus_rtu:decode_data/5 中的数据块处理
decode_data(Buff, ProductId, DtuAddr, Address, Acc) ->
    % ... 协议解析 ...
    
    case is_data_block_mode(ProductId, SlaveId, Address) of
        true ->
            %% 数据块模式：调用数据块处理模块
            DataBlockCache = #{<<"block_data">> => UserZone},
            Props = get_product_props(ProductId),
            Result = modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props),
            {Rest1, Result};
        false ->
            %% 普通模式：原有逻辑
            Result = modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc),
            {Rest1, Result}
    end.
```

#### 3.3 数据块配置检测
```erlang
%% 判断是否为数据块模式
is_data_block_mode(ProductId, SlaveId, Address) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            has_data_block_config(Props, SlaveId, Address);
        _ ->
            false
    end.
```

## 模块结构

### 核心模块
```
apps/dgiot_modbus/
├── src/
│   ├── dgiot_modbus.erl              # 主模块，设备管理
│   ├── dgiot_modbus_app.erl          # 应用模块，钩子注册
│   ├── dgiot_modbusrtu_tcp.erl       # Modbus RTU TCP通道
│   └── modbus/
│       └── modbus_rtu/
│           ├── modbus_rtu.erl        # 协议解析主模块
│           ├── modbus_rtu_data_blocks.erl  # 数据块处理
│           ├── modbus_rtu_decoder.erl      # 数据解码器
│           ├── modbus_rtu_encoder.erl      # 数据编码器
│           └── modbus_rtu_utils.erl        # 工具函数
├── include/dgiot_modbus.hrl          # 头文件
└── README.md                         # 本文档
```

### 模块职责
1. **dgiot_modbus**：设备客户端管理、设备注册
2. **dgiot_modbusrtu_tcp**：TCP连接管理、原始数据转发
3. **modbus_rtu**：协议解析、数据块处理
4. **modbus_rtu_data_blocks**：数据块拆分和合并
5. **modbus_rtu_decoder**：数据解码和格式转换
6. **modbus_rtu_encoder**：数据编码和请求构建

## 使用指南

### 1. 编译和加载
```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

### 2. 设备注册
设备连接时自动注册，加载物模型配置：
```erlang
%% 设备注册流程
1. 设备连接 → 发送注册报文
2. 解析注册报文 → 获取ProductId、DeviceAddr
3. 调用dgiot_modbus:register_client/5
4. 加载物模型配置 → 创建设备记录
5. 设备状态更新为已注册
```

### 3. 数据处理
设备数据上报时自动处理：
```erlang
%% 数据处理流程
1. 设备发送数据 → 通讯层接收
2. 构建Things格式 → 发送到任务通道
3. 调用dgiot_task:save_td/4
4. 流式计算 → 缓存管理 → 数据存储
5. 业务处理 → 告警、实时数据
```

### 4. 数据块配置
在物模型中配置数据块：
```json
{
  "identifier": "block_data",
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X00",
    "key": "block_data",
    "originaltype": "raw"
  }
}
```

## 最佳实践

### 1. 物模型设计
- **明确协议类型**：在`dataForm.protocol`中指定`MODBUSRTU`
- **完整数据源配置**：配置`slaveid`、`address`、`originaltype`
- **合理使用计算值**：复杂计算使用计算值属性
- **数据块配置**：需要读取多个寄存器时使用数据块模式

### 2. 性能优化
- **缓存使用**：合理使用数据缓存减少数据库访问
- **批量处理**：支持数据块模式减少通信次数
- **异步处理**：通讯层快速转发，业务层异步处理

### 3. 错误处理
- **协议错误**：CRC校验失败、功能码不支持等
- **配置错误**：物模型配置不完整或不正确
- **网络错误**：连接断开、超时等

### 4. 监控和调试
- **日志记录**：关键节点添加详细日志
- **性能监控**：监控数据处理时间和资源使用
- **配置验证**：定期验证物模型配置的正确性

## 故障排除

### 常见问题

#### 1. 数据解析失败
- **检查物模型配置**：确认`slaveid`、`address`、`originaltype`正确
- **检查协议类型**：确认`dataForm.protocol`为`MODBUSRTU`
- **检查数据格式**：确认`originaltype`与设备数据格式匹配

#### 2. 数据块处理异常
- **检查数据块配置**：确认`key`字段为`block_data`
- **检查属性依赖**：计算值属性依赖的基础属性必须存在
- **检查数据长度**：数据块长度必须足够

#### 3. 设备注册失败
- **检查产品配置**：确认产品ID正确且已配置
- **检查网络连接**：确认设备可以连接到平台
- **检查注册报文**：确认注册报文格式正确

### 调试命令
```bash
# 查看产品配置
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_product:lookup_prod(<<"feeb43bffb">>).'

# 测试数据解析
_build/emqx/rel/emqx/bin/emqx eval 'modbus_rtu:parse_frame(<<...>>, #{}, #{<<"dtuproduct">> => <<"feeb43bffb">>, ...}).'

# 查看设备状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_device:lookup(<<"device_id">>).'
```

## dgiot_task框架定位和协议钩子机制

### 1. dgiot_task框架定位

#### 1.1 核心定位
`dgiot_task`是DG-IoT开源版本的核心业务处理框架，专门设计用于：
- **低频定期采集**：支持秒级或分钟级的数据采集间隔
- **简单流式计算**：提供基础的流式计算和业务处理能力
- **统一数据处理入口**：为所有数据来源提供统一处理接口

#### 1.2 设计原则
- **配置驱动**：所有处理逻辑通过物模型配置定义
- **协议无关**：框架本身不包含协议特定逻辑
- **按需调用**：只在需要时调用协议层钩子
- **性能优化**：针对低频场景优化，保持轻量级

### 2. 协议钩子机制

#### 2.1 钩子注册要求
**每个协议层都必须提供以下钩子给`dgiot_task`调用**：

```erlang
%% 协议模块必须注册的钩子
start_hook() ->
    %% 1. 原始数据解析钩子（必须）
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"PROTOCOL_NAME">>}, 
                   fun protocol_module:parse_raw_data/3),
    
    %% 2. 数据源配置钩子（可选）
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"PROTOCOL_NAME">>}, 
                   fun protocol_module:get_datasource/1),
    ok.
```

#### 2.2 钩子调用时机
`dgiot_task`在以下情况下调用协议钩子：
1. **接收到原始二进制数据**且指定了协议类型
2. **数据包含`raw_data`字段**需要协议解析
3. **物模型配置要求**特定协议的数据解析

#### 2.3 按需调用原则
```erlang
%% dgiot_task智能决策逻辑
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

### 3. 数据处理架构

#### 3.1 分层处理原则
```
数据来源 → [协议解析层] → dgiot_task → 业务处理 → 存储
           ↑
       按需调用协议钩子
```

#### 3.2 数据来源分类

**类型一：已解析数据（直接处理）**
- 来源：智能设备MQTT上报、采集通道解析后数据
- 特征：结构化JSON，不包含原始二进制字段
- 处理：直接进入业务计算，不调用协议钩子

**类型二：原始数据（需要解析）**
- 来源：简单设备MQTT上报、调试模式数据
- 特征：包含`raw_data`字段，指定`protocol`类型
- 处理：先调用协议钩子解析，再进行业务计算

#### 3.3 智能路由设计
`dgiot_task`提供智能路由功能，自动判断数据处理路径：
- **自动检测**：分析数据特征，判断是否需要协议解析
- **按需调用**：只在需要时调用协议钩子，避免不必要开销
- **性能优化**：已解析数据跳过解析步骤，提高处理效率

### 4. 协议层职责

#### 4.1 必须实现的钩子函数
```erlang
%% 原始数据解析钩子
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param RawData 原始二进制数据
%% @return 解析后的结构化数据
parse_raw_data(ProductId, DevAddr, RawData) ->
    % 协议特定解析逻辑
    {ok, ParsedData}.
```

#### 4.2 协议层最佳实践
1. **完整解析**：将原始数据解析为结构化键值对
2. **错误处理**：提供详细的解析错误信息
3. **性能优化**：优化解析算法，减少资源消耗
4. **配置支持**：支持物模型配置驱动的解析

### 5. 使用示例

#### 5.1 协议模块实现
```erlang
%% modbus_rtu_app.erl
-module(modbus_rtu_app).

start_hook() ->
    %% 注册Modbus RTU协议钩子
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:parse_raw_data/3),
    ok.

%% modbus_rtu.erl
parse_raw_data(ProductId, DevAddr, RawData) ->
    %% Modbus RTU协议解析逻辑
    case modbus_rtu:parse_frame(RawData, #{}, #{}) of
        {<<>>, ParsedData} ->
            {ok, ParsedData};
        Error ->
            {error, Error}
    end.
```

#### 5.2 dgiot_task调用
```erlang
%% 智能处理数据
handle_incoming_data(ProductId, DevAddr, Data) ->
    %% dgiot_task自动判断处理路径
    Result = dgiot_task:smart_process(ProductId, DevAddr, Data, #{}),
    
    case Result of
        {ok, ProcessedData} ->
            %% 处理成功
            ok;
        {error, Reason} ->
            %% 处理失败
            ?LOG(error, "Data processing failed: ~p", [Reason])
    end.
```

### 6. 性能优化建议

#### 6.1 避免不必要的解析
- **缓存解析结果**：对相同原始数据缓存解析结果
- **提前判断**：在调用钩子前判断是否真的需要解析
- **批量处理**：支持批量数据解析，减少调用次数

#### 6.2 监控和统计
```erlang
%% 监控钩子调用情况
monitor_hook_performance(Protocol, StartTime) ->
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, microsecond),
    
    dgiot_metrics:histogram(dgiot_task, <<"hook_duration">>, Duration),
    dgiot_metrics:inc(dgiot_task, <<"hook_calls_", Protocol/binary>>, 1).
```

## 更新记录

### 2025-12-24
- **架构优化**：实现配置驱动架构，设备注册时加载物模型配置
- **数据块支持**：在`modbus_rtu`模块中集成数据块处理
- **统一处理入口**：完善`dgiot_task:save_td/4`函数文档
- **协议钩子机制**：明确每个协议层必须提供钩子给task
- **dgiot_task定位**：明确框架定位和智能路由设计
- **README更新**：整合架构分析和使用指南

### 2025-12-19
- **初始版本**：创建Modbus插件基础功能
- **协议支持**：支持Modbus RTU/TCP协议
- **数据格式**：支持多种数据格式和字节序

## 贡献指南

1. **代码规范**：遵循项目编码规范
2. **测试要求**：新增功能必须包含测试用例
3. **文档更新**：代码修改必须更新相关文档
4. **架构原则**：遵循配置驱动和分层架构原则

## 许可证

本项目基于Apache License 2.0许可证开源。

## 联系我们

如有问题或建议，请通过以下方式联系：
- 项目仓库：https://gitee.com/dgiiot/dgiot
- 问题反馈：在仓库中提交Issue
- 文档改进：提交Pull Request更新文档
