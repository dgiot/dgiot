# DG-IoT设备采集通道到dlink MQTT协议转换规范

## 概述

DG-IoT平台要求所有设备采集通道都统一转换为标准的dlink MQTT协议。本文档详细说明这种转换的规范、实现方式和最佳实践。

## 1. 转换规范

### 1.1 核心要求
- **统一消息格式**：所有设备采集通道必须输出统一的MQTT消息格式
- **标准Topic结构**：使用标准的dlink MQTT Topic格式
- **统一数据封装**：将原始数据封装为标准的Things格式
- **协议无关性**：底层协议差异在通道层处理，上层只看到统一格式

### 1.2 转换目标
```
各种设备协议（Modbus RTU/TCP、BACnet、DL/T645等）
    ↓
设备采集通道（协议解析）
    ↓
标准Things格式（统一数据封装）
    ↓
dlink MQTT协议（标准Topic + Payload）
    ↓
dgiot_task_worker处理
```

## 2. 标准dlink MQTT协议定义

### 2.1 Topic格式
```
$dg/thing/{ProductId}/{DevAddr}/properties/report
```

**字段说明**：
- `$dg`：固定前缀，表示DG-IoT平台
- `thing`：固定标识，表示物模型
- `{ProductId}`：产品ID，标识设备类型
- `{DevAddr}`：设备地址，标识具体设备
- `properties/report`：固定后缀，表示属性上报

### 2.2 Payload格式（Things格式）

#### 2.2.1 基础结构
```json
{
  "raw_data": "原始数据（十六进制字符串）",
  "data_type": "协议类型（如modbus_rtu）",
  "product_id": "产品ID",
  "dtu_addr": "设备地址",
  "channel_id": "通道ID",
  "timestamp": "时间戳（可选）"
}
```

#### 2.2.2 扩展字段
```json
{
  "env": "环境信息（可选）",
  "report_type": "上报类型（如active_report）",
  "pn": "端口号（可选）",
  "di": "设备标识（可选）"
}
```

## 3. 设备采集通道实现示例

### 3.1 Modbus RTU通道实现

#### 3.1.1 数据接收处理
```erlang
%% 在dgiot_modbusrtu_tcp.erl中
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, product = ProductId}} = TCPState) ->
    % 1. 构建标准Things格式
    Things = #{
        <<"raw_data">> => Buff,
        <<"data_type">> => <<"modbus_rtu">>,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DtuAddr,
        <<"channel_id">> => ChannelId
    },
    
    % 2. 发送到标准MQTT Topic
    NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    
    % 3. 通过MQTT发布
    dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things),
    
    {noreply, TCPState#tcp{buff = <<>>}}.
```

#### 3.1.2 发送聚合设备报告
```erlang
%% 发送聚合设备报告函数
send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, _) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    
    % 标准Topic
    ChildTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
    
    % 发送到任务通道
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_client:send(Taskchannel, DeviceId, ChildTopic, Things),
    
    % 保存数据到TDengine
    dgiot_task:save_td(ProductId, DtuAddr, Things, #{}),
    
    ok.
```

### 3.2 通用转换函数

#### 3.2.1 构建标准Things格式
```erlang
%% 通用Things构建函数
build_standard_things(RawData, Protocol, ProductId, DevAddr, ChannelId, Extra) ->
    BaseThings = #{
        <<"raw_data">> => RawData,
        <<"data_type">> => Protocol,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DevAddr,
        <<"channel_id">> => ChannelId,
        <<"timestamp">> => dgiot_datetime:now_ms()
    },
    
    % 合并额外字段
    maps:merge(BaseThings, Extra).
```

#### 3.2.2 发布标准MQTT消息
```erlang
%% 通用MQTT发布函数
publish_standard_mqtt(ProductId, DevAddr, Things) ->
    % 构建标准Topic
    Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
    
    % 获取设备ID
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    
    % 获取任务通道
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    
    % 发布MQTT消息
    case dgiot_client:send(Taskchannel, DeviceId, Topic, Things) of
        ok -> 
            ?LOG(info, "Successfully published MQTT message: ~p", [Topic]),
            ok;
        {error, Reason} ->
            ?LOG(error, "Failed to publish MQTT message: ~p, Reason: ~p", [Topic, Reason]),
            {error, Reason}
    end.
```

## 4. dlink模块的角色

### 4.1 dlink作为协议转换桥梁
dlink模块在架构中扮演重要角色：

```
第三方原始数据 → dlink(TCP/GRPC/HTTP) → 规则引擎转换 → 标准MQTT消息 → 其他模块处理
```

### 4.2 与设备采集通道的关系
```
设备采集通道（Modbus、BACnet等） → 标准MQTT消息 → dgiot_task_worker处理
    ↑                                  ↑
    |                                  |
直接设备通信                     统一消息格式
    |                                  |
    ↓                                  ↓
各种协议设备                     统一处理逻辑
```

**关键点**：设备采集通道和dlink模块都输出**相同的标准MQTT消息格式**，实现了协议的统一。

## 5. 转换流程

### 5.1 完整转换流程
```
设备原始数据
    ↓
设备采集通道接收
    ↓
协议解析（通道特定）
    ↓
构建标准Things格式
    ↓
发布标准MQTT消息（$dg/thing/...）
    ↓
dgiot_task_worker接收处理
    ↓
数据保存和业务处理
```

### 5.2 各层职责

#### 设备采集通道层
- 接收设备原始数据
- 进行协议解析
- 构建标准Things格式
- 发布标准MQTT消息

#### dlink模块层（第三方数据）
- 接收TCP/GRPC/HTTP原始数据
- 规则引擎转换
- 构建标准Things格式
- 发布标准MQTT消息

#### 业务处理层
- 接收标准MQTT消息
- 统一的数据处理逻辑
- 数据保存到TDengine
- 业务规则执行

## 6. 优势和价值

### 6.1 技术优势
1. **统一接口**：所有数据入口输出统一格式
2. **协议无关**：上层业务逻辑不关心底层协议
3. **易于扩展**：新协议只需实现通道层转换
4. **维护简单**：统一的消息处理逻辑

### 6.2 业务价值
1. **快速集成**：新设备类型快速接入
2. **降低复杂度**：业务开发人员只需关注统一接口
3. **提高可靠性**：统一的错误处理和监控
4. **便于监控**：统一的日志和指标收集

## 7. 实施指南

### 7.1 新设备通道开发
1. **实现协议解析**：根据设备协议实现数据解析
2. **集成转换函数**：使用标准转换函数构建Things格式
3. **发布MQTT消息**：使用标准Topic格式发布消息
4. **测试验证**：验证消息格式符合规范

### 7.2 现有通道改造
1. **分析现有格式**：分析当前数据输出格式
2. **逐步迁移**：逐步替换为标准格式
3. **兼容性处理**：确保不影响现有业务
4. **全面测试**：测试转换的正确性

### 7.3 监控和调试
```erlang
%% 监控MQTT消息发布
monitor_mqtt_publish(Topic, Things) ->
    ?LOG(debug, "Publishing MQTT message: ~p", [Topic]),
    ?LOG(debug, "Payload keys: ~p", [maps:keys(Things)]),
    
    % 记录指标
    dgiot_metrics:inc(dgiot_channel, <<"mqtt_publish">>, 1),
    
    % 发送日志
    dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, 
                         "Published MQTT: ~p", [Topic]).
```

## 8. 最佳实践

### 8.1 Things格式设计
1. **必填字段**：确保raw_data、data_type、product_id、dtu_addr必填
2. **可选字段**：合理使用env、report_type等扩展字段
3. **数据大小**：控制Things大小，避免过大消息
4. **编码格式**：统一使用UTF-8编码

### 8.2 MQTT发布优化
1. **QoS设置**：根据业务需求设置合适的QoS级别
2. **保留消息**：谨慎使用保留消息功能
3. **发布频率**：控制消息发布频率，避免过载
4. **错误处理**：完善的发布失败重试机制

### 8.3 性能考虑
1. **批量处理**：支持批量数据转换和发布
2. **异步处理**：使用异步方式处理数据转换
3. **缓存优化**：缓存频繁访问的数据
4. **连接池**：使用连接池管理MQTT连接

## 9. 故障排除

### 9.1 常见问题
1. **Topic格式错误**
   - 检查ProductId和DevAddr是否正确
   - 验证Topic结构是否符合规范

2. **Things格式错误**
   - 检查必填字段是否齐全
   - 验证字段类型是否正确

3. **MQTT发布失败**
   - 检查MQTT连接状态
   - 验证权限和认证信息
   - 检查网络连接

### 9.2 调试工具
```erlang
%% 调试Things格式
debug_things_format(Things) ->
    io:format("Things format debug:~n"),
    io:format("  raw_data: ~p~n", [maps:get(<<"raw_data">>, Things, <<>>)]),
    io:format("  data_type: ~p~n", [maps:get(<<"data_type">>, Things, <<>>)]),
    io:format("  product_id: ~p~n", [maps:get(<<"product_id">>, Things, <<>>)]),
    io:format("  dtu_addr: ~p~n", [maps:get(<<"dtu_addr">>, Things, <<>>)]),
    io:format("  channel_id: ~p~n", [maps:get(<<"channel_id">>, Things, <<>>)]).

%% 调试MQTT发布
debug_mqtt_publish(Topic, Things) ->
    io:format("MQTT publish debug:~n"),
    io:format("  Topic: ~p~n", [Topic]),
    io:format("  Payload size: ~p bytes~n", [byte_size(jsx:encode(Things))]),
    io:format("  Timestamp: ~p~n", [dgiot_datetime:now_ms()]).
```

## 10. 总结

DG-IoT平台通过要求所有设备采集通道统一转换为标准的dlink MQTT协议，实现了：

### 10.1 架构统一
- **入口统一**：所有数据入口输出相同格式
- **处理统一**：上层业务逻辑统一处理
- **监控统一**：统一的监控和日志体系

### 10.2 技术标准化
- **消息标准化**：统一的Topic和Payload格式
- **协议标准化**：统一的Things数据结构
- **接口标准化**：统一的MQTT接口

### 10.3 业务价值
- **快速开发**：新功能基于统一接口快速开发
- **易于维护**：统一的代码结构和处理逻辑
- **高可靠性**：统一的错误处理和监控机制

这种设计使得DG-IoT平台能够高效、可靠地处理各种设备数据，同时保持系统的可扩展性和可维护性。
