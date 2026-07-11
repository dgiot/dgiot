# dgiot_task_worker与dlink规则引擎集成分析

## 概述

本文档分析dgiot_task_worker如何配合dlink的规则引擎转换处理第三方消息（包括数据块），实现不经过设备采集通道的数据处理。

## 1. 系统架构

### 1.1 整体架构
```
第三方系统/设备
    ↓
dlink通道（TCP/GRPC/HTTP）接收原始数据
    ↓
规则引擎转换（dlink模块）
    ↓
MQTT发布（转换后的标准数据）
    ↓
dgiot_task_worker接收MQTT消息
    ↓
数据保存（TDengine）
    ↓
前端展示
```

### 1.2 组件职责
- **dlink模块**：负责接收TCP/GRPC/HTTP原始数据，进行规则引擎转换，然后通过MQTT发布
- **dgiot_task_worker**：负责接收MQTT消息，处理转换后的数据，包括数据块处理
- **规则引擎**：负责将第三方协议数据转换为DG-IoT标准格式

## 2. dlink规则引擎转换

### 2.1 dlink消息处理机制
dlink模块主要处理**TCP/GRPC/HTTP**原始数据，而不是MQTT消息。关键处理逻辑在`dgiot_tcp2grpc_worker.erl`中：

#### 2.1.1 TCP数据处理
```erlang
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, productIds = ProductIds}} = TCPState) ->
    dgiot_bridge:send_log(ChannelId, "Buff ~p", [Buff]),
    lists:map(fun(ProductId) ->
        do_cmd(ProductId, tcp, Buff, TCPState)
              end, ProductIds),
    {noreply, TCPState}.
```

#### 2.1.2 规则引擎转换和MQTT发布
```erlang
do_cmd(ProductId, Cmd, Data, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    case dgiot_dlink_client:payload(#{data => Data, cmd => dgiot_utils:to_binary(Cmd), product => ProductId}, #{channel => ChannelId}) of
        {ok, #{ack := Ack, topic := Topic, payload := Payload} = _Result, _} ->
            % 1. 发送TCP响应（如果需要）
            case Ack of
                <<>> -> pass;
                Ack -> dgiot_tcp_server:send(TCPState, Ack)
            end,
            % 2. 发布MQTT消息（关键步骤）
            case Topic of
                <<>> -> pass;
                Topic -> dgiot_mqtt:publish(ProductId, Topic, Payload)
            end;
        _ -> pass
    end,
    {noreply, TCPState}.
```

### 2.2 规则配置
dlink通过`get_protocol`接口获取协议配置，支持多种协议的数据转换：

```erlang
do_request(get_protocol, Body, _Context, _Req) ->
    Protocols = dgiot_dlink:get_all_protocol(),
    {200, Protocols}.
```

### 2.3 HTTP API接口
dlink也提供HTTP API接收第三方数据：

```erlang
%% 第三方上报api（HTTP接口）
do_request(post_third_party_push, #{<<"imei">> := Imei} = Args, _Context, _Req) ->
    Image = maps:get(<<"Image">>, Args, <<>>),
    Result_image = maps:get(<<"Result_image">>, Args, Image),
    
    os:cmd(<<"wget -qoP /data/dgiot/go_fastdfs/files/dgiot_file/device/", Imei/binary, " ", Image/binary>>),
    os:cmd(<<"wget -qoP /data/dgiot/go_fastdfs/files/dgiot_file/device/", Imei/binary, " ", Result_image/binary>>),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}};
```

## 3. dgiot_task_worker处理第三方消息

### 3.1 消息接收处理
`dgiot_task_worker`通过`handle_info`函数处理各种消息，包括第三方消息：

#### 3.1.1 dclient_ack消息处理（关键）
```erlang
handle_info({dclient_ack, Topic, Payload}, #dclient{channel = _ChannelId, userdata = Usedata} = State) ->
    dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"thing">>, ProductId, DevAddr, <<"properties">>, <<"report">>] ->
            dgiot_task:save_td(ProductId, DevAddr, Payload, #{}),
            {noreply, send_msg(State#dclient{userdata = Usedata#device_task{product = ProductId, devaddr = DevAddr}})};
        _ ->
            io:format("~s ~p Topic = ~p.~n", [?FILE, ?LINE, Topic]),
            {noreply, send_msg(State)}
    end;
```

**处理流程**：
1. 接收`dclient_ack`消息（包含Topic和Payload）
2. 解析Topic，获取ProductId和DevAddr
3. 调用`dgiot_task:save_td/4`保存数据
4. 继续发送下一条指令

### 3.2 数据块处理
对于数据块（block data）类型的第三方消息，处理流程如下：

#### 3.2.1 数据块解析
在`dgiot_task_service:get_calculated/4`函数中处理数据块：

```erlang
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            error -> Acc;
            _ ->
                case X of
                    #{<<"isaccumulate">> := true,
                      <<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>},
                      <<"dataSource">> := #{<<"key">> := Key} = DataSource} ->
                        case maps:get(Key, Calculated, not_find) of
                            not_find -> Acc;
                            KeyValue -> get_statistic(ProductId, DevAddr, Key, Identifier, dgiot_utils:to_int(KeyValue), DataSource, Acc)
                        end;
                    #{<<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection},
                      <<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}} ->
                        Str1 = maps:fold(fun(K, V, Acc2) ->
                            Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%{", K/binary, "}">>), dgiot_utils:to_list(V), [global, {return, list}]),
                            re:replace(Str, "%{s}", dgiot_utils:to_list(V), [global, {return, list}])
                        end, dgiot_utils:to_list(Collection), Calculated),
                        case string2value(Str1, Type, Specs) of
                            error -> maps:without([Identifier], Acc);
                            Value1 -> Acc#{Identifier => Value1}
                        end;
                    _ -> Acc
                end
        end
    end, Calculated, Props).
```

#### 3.2.2 数据块处理流程
1. **识别数据块**：通过`dataSource`中的`key`字段识别数据块
2. **提取基础数据**：从Calculated中获取数据块的基础值
3. **计算派生值**：根据`collection`中的公式计算派生属性
4. **保存结果**：将计算结果保存到Acc中

## 4. 集成工作流程

### 4.1 第三方消息处理完整流程

```
第三方系统/设备（原始数据）
    ↓
dlink通道接收（TCP/GRPC/HTTP）
    ↓
规则引擎转换（dgiot_dlink_client:payload/2）
    ↓
MQTT发布（dgiot_mqtt:publish/3）
    ↓
dgiot_task_worker接收（dclient_ack消息）
    ↓
数据保存（dgiot_task:save_td/4）
    ↓
TDengine存储
```

#### 详细步骤：
1. **dlink接收原始数据**
   - 通过TCP/GRPC/HTTP接收第三方原始数据
   - 记录接收日志

2. **规则引擎转换**
   - 调用`dgiot_dlink_client:payload/2`进行数据转换
   - 根据协议配置将原始数据转换为标准格式
   - 生成MQTT Topic和Payload

3. **MQTT发布**
   - 发布到相应的MQTT Topic（如`$dg/thing/{ProductId}/{DevAddr}/properties/report`）
   - Payload包含转换后的标准数据

4. **dgiot_task_worker处理**
   - 接收`dclient_ack`消息（MQTT消息）
   - 解析Topic，获取ProductId和DevAddr
   - 调用`dgiot_task:save_td/4`保存数据

5. **数据保存**
   - 保存到TDengine数据库
   - 更新缓存数据
   - 触发相关业务逻辑

### 4.2 数据块处理示例

#### 示例：Modbus RTU数据块处理
```json
// 第三方数据（原始）
{
  "slave_id": 1,
  "function_code": 3,
  "data": [0, 0, 0, 0, 0, 0]
}

// 规则引擎转换后
{
  "block_data": "000000000000",
  "product_id": "feeb43bffb",
  "dev_addr": "port_9001"
}

// dgiot_task_worker处理
{
  "angular_x": 0.0,
  "angular_y": 0.0,
  "angular_z": 0.0,
  "temperature": 25.5
}
```

## 5. 配置示例

### 5.1 dlink协议配置
```json
{
  "protocol": "MODBUSRTU",
  "rules": [
    {
      "source": "data",
      "target": "block_data",
      "transform": "binary_to_hex(value)"
    },
    {
      "source": "slave_id",
      "target": "slaveid",
      "transform": "value"
    }
  ]
}
```

### 5.2 物模型配置（数据块）
```json
{
  "identifier": "block_data",
  "name": "数据块",
  "dataForm": {
    "strategy": "采集值",
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X00",
    "key": "block_data"
  },
  "dataType": {
    "type": "text",
    "specs": {}
  }
}
```

### 5.3 计算值属性配置
```json
{
  "identifier": "angular_x",
  "name": "角度X",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"
  },
  "dataType": {
    "type": "float",
    "specs": {
      "precision": 2
    }
  }
}
```

## 6. 错误处理和监控

### 6.1 错误处理机制
1. **规则引擎错误**：记录转换失败日志，返回错误响应
2. **数据解析错误**：记录解析失败日志，跳过错误数据
3. **保存失败**：记录保存失败日志，重试机制

### 6.2 监控指标
- `dgiot_metrics:inc(dgiot_task, <<"task_recv">>, 1)` - 接收消息计数
- `dgiot_metrics:inc(dgiot_task, <<"task_send">>, 1)` - 发送消息计数
- `dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1)` - 保存数据计数

## 7. 性能优化

### 7.1 批量处理
- 支持批量数据转换
- 批量保存到TDengine
- 减少IO操作次数

### 7.2 缓存优化
- 缓存物模型配置
- 缓存规则配置
- 减少数据库查询

### 7.3 异步处理
- 异步数据保存
- 非阻塞消息处理
- 提高并发性能

## 8. 使用场景

### 8.1 工业物联网
- **场景**：PLC数据采集，数据块解析
- **优势**：支持多种工业协议，数据块高效处理

### 8.2 智慧城市
- **场景**：传感器数据汇聚，多源数据融合
- **优势**：规则引擎灵活配置，支持复杂数据转换

### 8.3 第三方系统集成
- **场景**：ERP/MES系统数据对接
- **优势**：标准化接口，易于集成

## 9. 总结

`dgiot_task_worker`与`dlink`规则引擎的集成提供了强大的第三方消息处理能力：

### 9.1 核心优势
1. **协议无关性**：dlink处理TCP/GRPC/HTTP原始数据，规则引擎转换为标准格式
2. **MQTT桥梁**：dlink作为协议转换桥梁，将各种协议统一为MQTT消息
3. **高效性**：数据块处理优化，性能高效
4. **可靠性**：完善的错误处理和监控机制

### 9.2 关键技术
1. **协议转换**：dlink将TCP/GRPC/HTTP数据转换为MQTT消息
2. **规则引擎**：将第三方协议数据转换为DG-IoT标准格式
3. **数据块处理**：高效处理块状数据
4. **异步处理**：提高系统并发性能

### 9.3 消息流澄清
**重要澄清**：dlink模块本身不直接处理MQTT消息，而是：
1. 接收TCP/GRPC/HTTP原始数据
2. 通过规则引擎转换为标准格式
3. **发布MQTT消息**给其他模块（如dgiot_task_worker）处理

### 9.4 适用场景
- 工业物联网数据采集（PLC、传感器等）
- 多协议设备接入（非标准协议设备）
- 第三方系统集成（ERP/MES系统对接）
- 大数据量处理（数据块解析）

这种集成架构使得DG-IoT平台能够灵活处理各种第三方消息，支持复杂的业务场景，同时保持系统的高性能和可靠性。
