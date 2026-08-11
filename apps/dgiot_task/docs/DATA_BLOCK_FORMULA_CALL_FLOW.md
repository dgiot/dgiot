# DG-IoT自有设备采集通道数据块处理和公式转换代码调用流程

## 概述

本文档详细说明DG-IoT自有设备采集通道中数据块处理、采集公式和控制公式的代码调用流程，展示各模块之间的调用关系和执行顺序。

## 1. 数据块处理代码调用流程

### 1.1 数据接收和协议解析调用流程

```
dgiot_modbusrtu_tcp.erl:handle_info/2
    ↓ (接收TCP数据)
dgiot_modbusrtu_tcp.erl:send_to_task_channel/5
    ↓ (发送到任务通道)
dgiot_task.erl:save_td/4
    ↓ (调用协议钩子)
dgiot_hook:run_hook/2 → {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}
    ↓ (调用协议解析)
modbus_rtu.erl:parse_frame/3
    ↓ (检查数据块模式)
modbus_rtu.erl:is_data_block_mode/3
    ↓ (调用数据块处理)
modbus_rtu_data_blocks.erl:process_data_blocks/2
```

### 1.2 详细调用序列

```erlang
%% 1. TCP数据接收入口
dgiot_modbusrtu_tcp:handle_info({tcp, Buff}, TCPState)
    → dgiot_modbusrtu_tcp:send_to_task_channel/5
        → dgiot_client:send/4 (发送到MQTT主题)

%% 2. 任务通道处理入口
dgiot_task:save_td(ProductId, DevAddr, Ack, _AppData)
    → dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, [ProductId, DevAddr, Data])
        → modbus_rtu:parse_frame/3

%% 3. 数据块模式检查
modbus_rtu:parse_frame/3
    → modbus_rtu:is_data_block_mode/3
        → modbus_rtu:has_data_block_config/3

%% 4. 数据块处理
modbus_rtu:decode_data/5 (数据块模式分支)
    → modbus_rtu_data_blocks:process_data_blocks/2
        → modbus_rtu_data_blocks:process_props_recursive/3
            → modbus_rtu_data_blocks:process_single_prop/3
```

### 1.3 关键调用代码

```erlang
%% 在modbus_rtu.erl中的调用点
case is_data_block_mode(ProductId, SlaveId, Address) of
    true ->
        %% 调用数据块处理模块
        DataBlockCache = #{<<"block_data">> => UserZone},
        Props = get_product_props(ProductId),
        Result = modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props),
        {Rest1, Result};
    false ->
        %% 普通模式处理
        Result = modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc),
        {Rest1, Result}
end.
```

## 2. 采集公式处理代码调用流程

### 2.1 采集公式计算调用流程

```
modbus_rtu_decoder.erl:modbus_decoder/5
    ↓ (预处理数据片段)
modbus_rtu_decoder.erl:preprocess_data_fragments/4
    ↓ (处理非计算值属性)
modbus_rtu_decoder.erl:process_decoder_props/5
    ↓ (处理计算值属性)
modbus_rtu_decoder.erl:process_calculated_props/3
    ↓ (调用公式计算)
modbus_rtu_decoder.erl:format_value/3
```

### 2.2 任务服务层采集公式调用流程

```
dgiot_task_service.erl:save_td/4
    ↓ (获取计算值)
dgiot_task_service.erl:get_calculated/4
    ↓ (公式字符串处理)
dgiot_task_service.erl:string2value/3
    ↓ (Erlang表达式计算)
erl_eval:exprs/2
```

### 2.3 详细调用序列

```erlang
%% 1. 解码器层采集公式处理
modbus_rtu_decoder:modbus_decoder(ProductId, SlaveId, Address, Data, Acc1)
    → modbus_rtu_decoder:preprocess_data_fragments/4
    → modbus_rtu_decoder:process_decoder_props/5
    → modbus_rtu_decoder:process_calculated_props/3
        → modbus_rtu_decoder:format_value/3
            → modbus_rtu_decoder:parse_by_format/3 或 parse_single_format/2

%% 2. 任务服务层采集公式处理
dgiot_task_service:save_td(ProductId, DevAddr, Ack, _AppData)
    → dgiot_task_service:get_calculated(ProductId, DevAddr, Calculated, Props)
        → dgiot_task_service:string2value(Str1, Type, Specs)
            → erl_scan:string/1
            → erl_parse:parse_exprs/1
            → erl_eval:exprs/2
```

### 2.4 关键调用代码

```erlang
%% 在dgiot_task_service.erl中的公式计算调用
case string2value(Str1, Type, Specs) of
    error -> maps:without([Identifier], Acc);
    Value1 -> Acc#{Identifier => Value1}
end.

%% string2value/3的内部调用
string2value(Str, Type, Specs) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    case string2value(Str, Type1) of
        error -> error;
        Value ->
            case Type1 of
                <<"INT">> -> round(Value);
                Type2 when Type2 == <<"FLOAT">>; Type2 == <<"DOUBLE">> ->
                    Precision = maps:get(<<"precision">>, Specs, 3),
                    dgiot_utils:to_float(Value, Precision);
                _ -> Value
            end
    end.
```

## 3. 控制公式处理代码调用流程

### 3.1 控制公式转换调用流程

```
dgiot_task_service.erl:get_control/3
    ↓ (构建控制指令)
modbus_rtu.erl:set_params/3
    ↓ (处理单个参数)
modbus_rtu.erl:process_single_param/2
    ↓ (构建RTU请求)
modbus_rtu.erl:build_rtu_request/8
    ↓ (应用控制公式)
dgiot_task:string2value/2
```

### 3.2 详细调用序列

```erlang
%% 1. 控制指令构建入口
dgiot_task_service:get_control(Round, Data, Control)
    → modbus_rtu:set_params(Payload, ProductId, DevAddr)
        → modbus_rtu:process_single_param(Data, Acc)
            → modbus_rtu:build_rtu_request(Value, Setting, SlaveId, Address, OperateType, Originaltype, DataSource, Acc)

%% 2. 控制公式应用
modbus_rtu:build_rtu_request/8
    → Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}])
    → Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>))
        → dgiot_task:string2value/2
            → erl_scan:string/1
            → erl_parse:parse_exprs/1
            → erl_eval:exprs/2

%% 3. 指令编码和发送
modbus_rtu_encoder:build_req_message(RtuReq)
    → dgiot_tcp_server:send/2 (发送到设备)
```

### 3.3 关键调用代码

```erlang
%% 在modbus_rtu.erl中的控制公式调用
Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),

%% 构建RTU请求
RtuReq = #rtu_req{
    slaveId = Sh * 256 + Sl,
    funcode = dgiot_utils:to_int(FunCode),
    address = H * 256 + L,
    registersnumber = dgiot_utils:to_int(Registersnumber),
    dataByteSize = dgiot_utils:to_int(Bytes),
    quality = Value1
},
Acc ++ [modbus_rtu_encoder:build_req_message(RtuReq)].
```

## 4. 完整端到端调用流程

### 4.1 数据上报完整调用链

```
1. dgiot_modbusrtu_tcp:handle_info({tcp, Buff}, TCPState)
2. dgiot_modbusrtu_tcp:send_to_task_channel/5
3. dgiot_client:send/4 (发送到MQTT: $dg/thing/.../properties/report)
4. dgiot_task:save_td/4
5. dgiot_hook:run_hook/2 → modbus_rtu:parse_frame/3
6. modbus_rtu:is_data_block_mode/3
7. modbus_rtu_data_blocks:process_data_blocks/2
8. modbus_rtu_decoder:modbus_decoder/5
9. modbus_rtu_decoder:process_calculated_props/3
10. dgiot_task_service:get_calculated/4
11. dgiot_task_service:string2value/3 → erl_eval:exprs/2
12. dgiot_tdengine_adapter:save/3
```

### 4.2 控制下发完整调用链

```
1. 前端API调用 → dgiot_xxx_handler:handle_request/3
2. dgiot_task_service:get_control/3
3. modbus_rtu:set_params/3
4. modbus_rtu:build_rtu_request/8
5. dgiot_task:string2value/2 → erl_eval:exprs/2
6. modbus_rtu_encoder:build_req_message/1
7. dgiot_tcp_server:send/2
8. 设备接收并执行
```

## 5. 模块间接口定义

### 5.1 数据块处理接口

```erlang
%% modbus_rtu.erl 调用数据块处理模块
-module(modbus_rtu).
-export([parse_frame/3, is_data_block_mode/3]).

%% 调用点
case is_data_block_mode(ProductId, SlaveId, Address) of
    true ->
        Result = modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props);
    false ->
        Result = modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc)
end.

%% modbus_rtu_data_blocks.erl 接口
-module(modbus_rtu_data_blocks).
-export([process_data_blocks/2]).
```

### 5.2 采集公式处理接口

```erlang
%% modbus_rtu_decoder.erl 调用公式计算
-module(modbus_rtu_decoder).
-export([modbus_decoder/5, format_value/3]).

%% 调用点
case catch format_value(Fragment, X, []) of
    {Value1, _Rest} -> Acc#{Identifier => Value1};
    _ -> Acc
end.

%% dgiot_task_service.erl 调用公式引擎
-module(dgiot_task_service).
-export([get_calculated/4, string2value/3]).

%% 调用点
case string2value(Str1, Type, Specs) of
    error -> maps:without([Identifier], Acc);
    Value1 -> Acc#{Identifier => Value1}
end.
```

### 5.3 控制公式处理接口

```erlang
%% modbus_rtu.erl 调用控制公式
-module(modbus_rtu).
-export([set_params/3, build_rtu_request/8]).

%% 调用点
Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>))

%% dgiot_task.erl 公式引擎接口
-module(dgiot_task).
-export([string2value/2]).
```

## 6. 配置驱动的调用流程

### 6.1 物模型配置解析调用

```
dgiot_product:lookup_prod/1
    ↓ (获取物模型配置)
dgiot_task_service:get_props/1
    ↓ (解析属性配置)
遍历Props列表，根据strategy字段调用不同处理函数：
    - <<"采集值">> → modbus_rtu_decoder:process_decoder_props/5
    - <<"计算值">> → modbus_rtu_decoder:process_calculated_props/3
    - <<"控制值">> → modbus_rtu:build_rtu_request/8
```

### 6.2 配置到代码的映射

```erlang
%% 物模型配置示例
#{
    <<"identifier">> => <<"angular_x">>,
    <<"dataForm">> => #{
        <<"strategy">> => <<"计算值">>,
        <<"collection">> => <<"block_data[0:2] * 0.1">>
    },
    <<"dataSource">> => #{
        <<"slaveid">> => <<"block_data">>,
        <<"address">> => <<"0">>,
        <<"registersnumber">> => <<"1">>,
        <<"originaltype">> => <<"short16_AB">>
    }
}

%% 对应的代码调用
1. 识别为计算值属性：strategy = <<"计算值">>
2. 调用：modbus_rtu_decoder:process_calculated_props/3
3. 获取基础值：maps:get(<<"block_data">>, Acc)
4. 应用公式：block_data[0:2] * 0.1
5. 调用：modbus_rtu_decoder:format_value/3
6. 结果保存：Acc#{<<"angular_x">> => Value1}
```

## 7. 错误处理和监控调用

### 7.1 错误处理调用流程

```erlang
%% 公式计算错误处理
case catch erl_eval:exprs(Exprs, Bindings) of
    {value, Value, _} -> Value;
    _ -> 0  % 返回默认值
end

%% 数据不足错误处理
case byte_size(Value) >= IntOffset + IntLen of
    true -> ... % 正常处理
    false -> Acc  % 跳过，保持原累积结果
end

%% 配置错误处理
case dgiot_product:lookup_prod(ProductId) of
    {ok, #{<<"thing">> := #{<<"properties">> := Props}}} -> ... % 正常处理
    _Error -> []  % 返回空列表
end
```

### 7.2 监控和日志调用

```erlang
%% 调试日志调用
io:format("~s ~p Processing data block: ProductId=~p, SlaveId=~p, Address=~p~n", 
         [?FILE, ?LINE, ProductId, SlaveId, Address])

%% 业务日志调用
dgiot_bridge:send_log(Channel, ProductId, DevAddr, 
                     "~s ~p save td => ProductId ~p DevAddr ~p ~ts ", 
                     [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(dgiot_json:encode(Storage))])

%% 指标监控调用
dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1)
```

## 8. 总结

### 8.1 代码调用特点

1. **分层清晰**：TCP层 → 协议层 → 业务层 → 存储层
2. **模块化调用**：各模块职责明确，通过标准接口调用
3. **配置驱动**：根据物模型配置动态调用处理逻辑
4. **错误隔离**：各层独立错误处理，避免级联故障

### 8.2 关键调用模式

1. **钩子机制调用**：`dgiot_hook:run_hook/2` 实现协议解析插件化
2. **递归处理调用**：`modbus_rtu_data_blocks:process_props_recursive/3` 处理数据块
3. **公式引擎调用**：`erl_eval:exprs/2` 实现动态公式计算
4. **异步消息调用**：`dgiot_client:send/4` 实现模块间异步通信

### 8.3 性能优化调用

1. **缓存调用**：`dgiot_data:get/1` 和 `dgiot_data:insert/2` 减少数据库访问
2. **批量处理调用**：`lists:foldl/3` 实现批量属性处理
3. **异步保存调用**：`dgiot_tdengine_adapter:save/3` 异步保存数据

这种代码调用设计使得DG-IoT平台能够高效、可靠地处理各种设备数据，同时保持系统的可扩展性和可维护性。
