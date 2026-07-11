# DG-IoT自有设备采集通道数据块处理和公式转换分析

## 概述

本文档详细分析DG-IoT自有设备采集通道如何处理好数据块处理、采集公式和控制公式的转换处理，确保设备数据能够正确解析、计算和存储。

## 1. 数据块处理机制

### 1.1 数据块定义
数据块（Data Block）是指一次Modbus读取操作返回的多个寄存器数据，这些数据包含多个子属性的值。

#### 示例：角度传感器数据块
```
原始数据块：6个寄存器（12字节）
包含：角度X、角度Y、角度Z等子属性
```

### 1.2 数据块识别
在`modbus_rtu.erl`中通过`is_data_block_mode/3`函数识别数据块：

```erlang
is_data_block_mode(ProductId, SlaveId, Address) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            has_data_block_config(Props, SlaveId, Address);
        _ -> false
    end.
```

#### 数据块配置识别
```erlang
has_data_block_config(Props, SlaveId, Address) ->
    lists:any(fun(Prop) ->
        case Prop of
            #{<<"dataSource">> := #{<<"slaveid">> := ConfigSlaveId, 
                                   <<"address">> := ConfigAddress,
                                   <<"key">> := <<"block_data">>}} ->
                % 检查从机地址和寄存器地址是否匹配
                ConfigSlaveIdBin = dgiot_utils:to_binary(ConfigSlaveId),
                ConfigAddressBin = dgiot_utils:to_binary(ConfigAddress),
                SlaveIdBin = dgiot_utils:to_binary(SlaveId),
                AddressBin = dgiot_utils:to_binary(Address),
                ConfigSlaveIdBin =:= SlaveIdBin andalso ConfigAddressBin =:= AddressBin;
            _ -> false
        end
    end, Props).
```

### 1.3 数据块处理流程

#### 1.3.1 数据块模式处理
```erlang
%% 在modbus_rtu.erl的decode_data函数中
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

#### 1.3.2 数据块处理模块
`modbus_rtu_data_blocks.erl`负责处理数据块：

```erlang
%% 处理数据块（递归入口）
process_data_blocks(DataBlockCache, Props) ->
    case is_list(Props) of
        true -> process_props_recursive(Props, DataBlockCache, #{});
        false -> process_single_prop(Props, DataBlockCache, #{})
    end.
```

### 1.4 数据块配置示例

#### 物模型配置
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

## 2. 采集公式处理

### 2.1 采集公式定义
采集公式用于从数据块中提取和计算子属性的值。

#### 示例：角度X计算公式
```
角度X = block_data[0:2] * 0.1
```

### 2.2 采集公式处理流程

#### 2.2.1 在`modbus_rtu_decoder.erl`中处理
```erlang
%% 处理计算值属性
process_calculated_props([X | Rest], Acc, DataFragments) ->
    NewAcc = case X of
        #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>,
                             <<"protocol">> := <<"MODBUSRTU">>},
          <<"identifier">> := Identifier,
          <<"dataSource">> := #{<<"slaveid">> := BitIdentifier,
                               <<"address">> := Offset,
                               <<"registersnumber">> := Num,
                               <<"originaltype">> := Originaltype}
        } ->
            % 从已解析的属性中获取基础值
            case maps:get(BitIdentifier, Acc, undefined) of
                undefined -> Acc;
                BaseValue ->
                    % 使用偏移量计算新值
                    <<OffsetH:8, OffsetL:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(Offset)),
                    IntOffset = OffsetH * 256 + OffsetL,
                    <<NumH:8, NumL:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(Num)),
                    IntNum = NumH * 256 + NumL,
                    IntLen = modbus_rtu_utils:get_len(IntNum, Originaltype),
                    
                    case BaseValue of
                        Value when is_binary(Value) ->
                            case byte_size(Value) >= IntOffset + IntLen of
                                true ->
                                    <<_:IntOffset/binary, Fragment:IntLen/binary, _/binary>> = Value,
                                    case catch format_value(Fragment, X, []) of
                                        {Value1, _Rest} -> Acc#{Identifier => Value1};
                                        _ -> Acc
                                    end;
                                false -> Acc
                            end;
                        _ -> Acc
                    end
            end;
        _ -> Acc
    end,
    process_calculated_props(Rest, NewAcc, DataFragments).
```

#### 2.2.2 在`dgiot_task_service.erl`中处理
```erlang
%% 获取计算值
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            error -> Acc;
            _ ->
                case X of
                    #{<<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection},
                      <<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}} ->
                        Str1 = maps:fold(fun(K, V, Acc2) ->
                            Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%{", K/binary, "}">>), 
                                           dgiot_utils:to_list(V), [global, {return, list}]),
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

### 2.3 采集公式配置示例

#### 计算值属性配置
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

## 3. 控制公式处理

### 3.1 控制公式定义
控制公式用于将用户设置的值转换为设备可理解的指令。

#### 示例：温度设置控制公式
```
设置温度 = 目标温度 * 10  # 转换为设备寄存器值
```

### 3.2 控制公式处理流程

#### 3.2.1 在`modbus_rtu.erl`中处理
```erlang
%% 构建RTU请求
build_rtu_request(Value, Setting, SlaveId, Address, OperateType, Originaltype, DataSource, Acc) ->
    FunCode = modbus_rtu_encoder:get_funcode(OperateType),
    AddressHex = is16(Address),
    SlaveIdHex = is16(SlaveId),
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(AddressHex),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(SlaveIdHex),
    
    % 应用控制公式
    Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),
    
    Registersnumber = maps:get(<<"registersnumber">>, DataSource, <<"1">>),
    Bytes = modbus_rtu_utils:get_len(Registersnumber, Originaltype),
    
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

#### 3.2.2 字符串转值函数
```erlang
%% 字符串转值
string2value(Str, <<"TEXT">>) when is_list(Str) ->
    case string:find(Str, "%%") of
        nomatch -> Str;
        _ -> error
    end;
string2value(Str, _) ->
    case string:find(Str, "%%") of
        nomatch ->
            {ok, Tokens, _} = erl_scan:string(Str ++ "."),
            case erl_parse:parse_exprs(Tokens) of
                {error, _} -> error;
                {ok, Exprs} ->
                    Bindings = erl_eval:new_bindings(),
                    case catch erl_eval:exprs(Exprs, Bindings) of
                        {value, Value, _} -> Value;
                        _ -> 0
                    end
            end;
        _ -> error
    end.
```

### 3.3 控制公式配置示例

#### 控制属性配置
```json
{
  "identifier": "target_temperature",
  "name": "目标温度",
  "dataForm": {
    "strategy": "控制值",
    "control": "%{d} * 10",
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X10",
    "operatetype": "writeHreg",
    "originaltype": "short16_AB"
  },
  "dataType": {
    "type": "int",
    "specs": {
      "min": 0,
      "max": 100
    }
  }
}
```

## 4. 完整处理流程

### 4.1 数据上报处理流程
```
设备原始数据 → Modbus RTU通道 → 数据块识别 → 数据块解析 → 采集公式计算 → 数据保存
```

#### 详细步骤：
1. **数据接收**：`dgiot_modbusrtu_tcp.erl`接收原始数据
2. **协议解析**：`modbus_rtu.erl`解析Modbus RTU帧
3. **数据块识别**：检查是否为数据块模式
4. **数据块处理**：`modbus_rtu_data_blocks.erl`处理数据块
5. **采集公式计算**：`modbus_rtu_decoder.erl`和`dgiot_task_service.erl`计算派生值
6. **数据保存**：保存到TDengine和缓存

### 4.2 控制指令下发流程
```
用户设置值 → 控制公式转换 → Modbus指令构建 → 设备下发
```

#### 详细步骤：
1. **用户输入**：前端输入目标值
2. **公式转换**：`modbus_rtu.erl`应用控制公式
3. **指令构建**：`modbus_rtu_encoder.erl`构建Modbus指令
4. **设备下发**：通过TCP通道下发到设备

## 5. 关键技术实现

### 5.1 公式引擎
DG-IoT使用Erlang的`erl_eval`模块实现公式计算：

```erlang
%% 公式计算核心
case catch erl_eval:exprs(Exprs, Bindings) of
    {value, Value, _} -> Value;
    _ -> 0
end
```

### 5.2 变量替换
支持`%{变量名}`格式的变量替换：

```erlang
Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}])
```

### 5.3 数据类型转换
支持多种数据类型转换：

```erlang
case Type1 of
    <<"INT">> -> round(Value);
    Type2 when Type2 == <<"FLOAT">>; Type2 == <<"DOUBLE">> ->
        Precision = maps:get(<<"precision">>, Specs, 3),
        dgiot_utils:to_float(Value, Precision);
    _ -> Value
end
```

## 6. 配置示例

### 6.1 完整物模型配置示例
```json
{
  "thing": {
    "properties": [
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
      },
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
      },
      {
        "identifier": "target_temperature",
        "name": "目标温度",
        "dataForm": {
          "strategy": "控制值",
          "control": "%{d} * 10",
          "protocol": "MODBUSRTU"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X10",
          "operatetype": "writeHreg",
          "originaltype": "short16_AB"
        },
        "dataType": {
          "type": "int",
          "specs": {
            "min": 0,
            "max": 100
          }
        }
      }
    ]
  }
}
```

## 7. 错误处理和调试

### 7.1 错误处理机制
1. **公式语法错误**：捕获`erl_eval`异常，返回默认值
2. **数据不足错误**：检查数据块大小，确保足够数据
3. **配置错误**：验证物模型配置格式

### 7.2 调试信息
```erlang
%% 调试日志
io:format("~s ~p Processing data block: ProductId=~p, SlaveId=~p, Address=~p~n", 
         [?FILE, ?LINE, ProductId, SlaveId, Address]),
io:format("~s ~p Data block size: ~p bytes~n", [?FILE, ?LINE, byte_size(Data)]),
io:format("~s ~p Calculated values: ~p~n", [?FILE, ?LINE, Calculated]).
```

## 8. 性能优化

### 8.1 缓存优化
- 缓存物模型配置，减少数据库查询
- 缓存数据块解析结果，避免重复计算
- 使用ETS表存储频繁访问的数据

### 8.2 批量处理
- 支持批量数据块处理
- 批量公式计算
- 批量数据保存

### 8.3 异步处理
- 异步数据解析
- 异步公式计算
- 异步数据保存

## 9. 总结

DG-IoT自有设备采集通道通过以下机制处理好数据块处理、采集公式和控制公式的转换处理：

### 9.1 数据块处理
1. **智能识别**：自动识别数据块模式
2. **灵活配置**：通过`key: block_data`标识数据块
3. **高效解析**：递归处理数据块中的子属性

### 9.2 采集公式处理
1. **公式引擎**：使用Erlang原生表达式计算
2. **变量替换**：支持`%{变量名}`格式变量
3. **类型转换**：自动处理数据类型转换

### 9.3 控制公式处理
1. **反向
