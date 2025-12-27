# 第三方消息处理分析

## 概述

在DG-IoT平台中，第三方消息（不经过设备采集通道的消息）可以通过多种方式处理。本文档分析dgiot_task模块中处理第三方消息的机制。

## 1. 第三方消息处理方式

### 1.1 规则引擎转换（推荐方式）

#### 函数：`rule_engine_transform/2`
```erlang
%% @doc 规则引擎转换
%% 将第三方协议数据转换为DG-IoT标准格式
%% @param ThirdPartyData 第三方数据
%% @param Protocol 协议类型
%% @return 转换后的标准数据
rule_engine_transform(ThirdPartyData, Protocol) ->
    ?LOG(info, "Transforming third-party data: Protocol=~p", [Protocol]),
    Rules = get_rules(Protocol),
    apply_rules(ThirdPartyData, Rules).
```

#### 处理流程：
1. **获取转换规则**：从规则表中获取指定协议的转换规则
2. **应用规则**：将第三方数据按照规则转换为标准格式
3. **返回标准数据**：返回DG-IoT标准格式的数据

#### 规则注册：
```erlang
%% @doc 注册转换规则
%% 为指定协议注册转换规则
%% @param Protocol 协议类型
%% @param Rule 转换规则
register_rule(Protocol, Rule) ->
    dgiot_data:insert({?RULE_ENGINE_TABLE, Protocol}, Rule).
```

#### 规则获取：
```erlang
%% @doc 获取协议规则
%% 获取指定协议的转换规则
%% @param Protocol 协议类型
%% @return 规则列表
get_rules(Protocol) ->
    dgiot_data:get({?RULE_ENGINE_TABLE, Protocol}, []).
```

### 1.2 协议钩子调用

#### 函数：`call_protocol_hook/4`
```erlang
%% @doc 调用协议钩子
%% 按需调用协议解析钩子
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Data 输入数据
%% @param Protocol 协议类型
%% @return {parsed, ParsedData} | {error, Reason} | {already_parsed, Data}
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    ?LOG(info, "Calling protocol hook: ProductId=~p, DevAddr=~p, Protocol=~p", [ProductId, DevAddr, Protocol]),
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} -> {parsed, ParsedData};
        _ -> {error, protocol_not_supported}
    end.
```

#### 处理流程：
1. **调用钩子**：通过`dgiot_hook:run_hook`调用注册的协议解析钩子
2. **解析数据**：钩子函数解析第三方协议数据
3. **返回结果**：返回解析后的数据或错误信息

### 1.3 智能保存处理

#### 函数：`smart_save_td/4`
```erlang
%% @doc 智能保存数据
%% 自动判断数据是否需要协议解析，智能路由处理
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Data 输入数据
%% @param Context 上下文信息
smart_save_td(ProductId, DevAddr, Data, Context) ->
    ?LOG(info, "Smart processing data for ProductId=~p, DevAddr=~p", [ProductId, DevAddr]),
    save_td(ProductId, DevAddr, Data, Context).
```

#### 处理流程：
1. **智能判断**：自动判断数据是否需要特殊处理
2. **路由处理**：根据数据类型选择适当的处理方式
3. **保存数据**：调用标准保存函数处理数据

## 2. 第三方消息处理流程

### 2.1 完整处理流程

```
第三方消息接收
    ↓
判断消息类型
    ↓
┌─────────────────────────────────────────────┐
│             处理方式选择                     │
└─────────────────────────────────────────────┘
    │
    ├── 规则引擎转换 → 转换为标准格式 → 保存数据
    │
    ├── 协议钩子解析 → 解析协议数据 → 保存数据
    │
    └── 智能保存处理 → 自动判断处理 → 保存数据
```

### 2.2 具体实现示例

#### 示例1：HTTP API接收第三方数据
```erlang
%% 在API处理器中接收第三方数据
handle_third_party_data(ProductId, DevAddr, ThirdPartyData, Protocol) ->
    % 1. 使用规则引擎转换
    StandardData = dgiot_task:rule_engine_transform(ThirdPartyData, Protocol),
    
    % 2. 保存转换后的数据
    dgiot_task:save_td(ProductId, DevAddr, StandardData, #{}).
```

#### 示例2：MQTT接收第三方数据
```erlang
%% 在MQTT消息处理器中
handle_mqtt_message(<<"thirdparty/", Protocol/binary, "/", ProductId/binary, "/", DevAddr/binary>>, Payload) ->
    % 1. 解析JSON数据
    ThirdPartyData = jsx:decode(Payload, [return_maps]),
    
    % 2. 智能保存处理
    dgiot_task:smart_save_td(ProductId, DevAddr, ThirdPartyData, #{protocol => Protocol}).
```

## 3. 规则配置示例

### 3.1 JSON格式规则配置
```json
{
  "protocol": "MODBUSRTU",
  "rules": [
    {
      "source": "temperature",
      "target": "temp",
      "transform": "value * 0.1"
    },
    {
      "source": "humidity", 
      "target": "humi",
      "transform": "value"
    },
    {
      "source": "status",
      "target": "state",
      "transform": "value == 1 ? 'online' : 'offline'"
    }
  ]
}
```

### 3.2 Erlang格式规则配置
```erlang
[
  #{
    <<"source">> => <<"temperature">>,
    <<"target">> => <<"temp">>,
    <<"transform">> => <<"value * 0.1">>
  },
  #{
    <<"source">> => <<"humidity">>,
    <<"target">> => <<"humi">>,
    <<"transform">> => <<"value">>
  }
]
```

## 4. 协议钩子注册示例

### 4.1 注册Modbus RTU协议钩子
```erlang
%% 在modbus_rtu模块中注册钩子
start_hook() ->
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                   fun modbus_rtu:parse_raw_data/3),
    ok.

%% 协议解析函数
parse_raw_data(ProductId, DevAddr, RawData) ->
    % 解析Modbus RTU原始数据
    {ok, ParsedData} = modbus_rtu:parse_frame(RawData, #{}, #{}),
    ParsedData.
```

### 4.2 注册自定义协议钩子
```erlang
%% 注册自定义协议解析钩子
register_custom_protocol() ->
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"CUSTOM_PROTOCOL">>}, 
                   fun custom_protocol:parse_data/3),
    ok.
```

## 5. 使用场景

### 5.1 第三方系统集成
- **场景**：第三方系统通过HTTP API推送数据
- **处理方式**：规则引擎转换 + 标准保存
- **优势**：无需修改现有代码，通过配置实现集成

### 5.2 多协议设备支持
- **场景**：支持多种非标准协议设备
- **处理方式**：协议钩子解析
- **优势**：可扩展性强，支持新协议快速接入

### 5.3 数据格式转换
- **场景**：不同系统间数据格式不一致
- **处理方式**：规则引擎转换
- **优势**：配置灵活，支持复杂转换逻辑

## 6. 最佳实践

### 6.1 规则设计原则
1. **单一职责**：每个规则只负责一个字段的转换
2. **可配置性**：规则应该易于配置和修改
3. **可测试性**：规则应该易于单元测试

### 6.2 协议钩子设计
1. **错误处理**：钩子函数应该有完善的错误处理
2. **性能考虑**：避免在钩子函数中进行复杂计算
3. **日志记录**：记录详细的解析日志便于调试

### 6.3 数据验证
1. **格式验证**：验证第三方数据格式
2. **范围检查**：检查数据值是否在合理范围内
3. **异常处理**：处理数据解析异常

## 7. 故障排除

### 7.1 常见问题
1. **规则不生效**
   - 检查规则是否正确注册
   - 验证规则格式是否符合要求
   - 检查协议名称是否匹配

2. **数据转换错误**
   - 检查源字段是否存在
   - 验证转换表达式语法
   - 检查数据类型是否匹配

3. **协议钩子调用失败**
   - 检查钩子是否已注册
   - 验证钩子函数参数是否正确
   - 检查协议名称是否一致

### 7.2 调试方法
```erlang
%% 调试规则引擎
debug_rule_engine(ThirdPartyData, Protocol) ->
    Rules = dgiot_task:get_rules(Protocol),
    io:format("Rules for protocol ~p: ~p~n", [Protocol, Rules]),
    Result = dgiot_task:rule_engine_transform(ThirdPartyData, Protocol),
    io:format("Transformation result: ~p~n", [Result]).

%% 调试协议钩子
debug_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    Result = dgiot_task:call_protocol_hook(ProductId, DevAddr, Data, Protocol),
    io:format("Protocol hook result: ~p~n", [Result]).
```

## 8. 性能优化

### 8.1 规则缓存
- 缓存已加载的规则，避免重复读取
- 使用ETS表存储规则数据
- 定期更新规则缓存

### 8.2 钩子优化
- 避免在钩子函数中进行IO操作
- 使用进程池处理并发请求
- 实现钩子函数的超时机制

### 8.3 批量处理
- 支持批量数据转换
- 减少单个请求的处理时间
- 实现异步处理机制

## 9. 总结

dgiot_task模块提供了三种处理第三方消息的方式：

1. **规则引擎转换**：适合结构化数据的格式转换
2. **协议钩子解析**：适合复杂协议的深度解析
3. **智能保存处理**：适合简单数据的快速处理

这些机制使得DG-IoT平台能够灵活处理各种第三方消息，支持多种集成场景，同时保持系统的可扩展性和可维护性。
