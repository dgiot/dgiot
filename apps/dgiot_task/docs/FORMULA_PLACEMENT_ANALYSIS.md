# DG-IoT采集公式和控制公式放置位置分析

## 概述

本文档分析DG-IoT平台中采集公式和控制公式应该放置的位置，基于七层架构设计原则和最佳实践。

## 1. 七层架构回顾

根据DG-IoT的七层架构设计原则：

| 层级 | 职责 | 公式相关职责 |
|------|------|--------------|
| 1. 通讯层 | TCP/UDP连接管理 | ❌ 禁止公式计算 |
| 2. 协议层 | 协议解析、数据封包/解包 | ✅ 基础数据解析，❌ 禁止业务公式 |
| 3. 消息路由层 | MQTT消息路由 | ❌ 禁止公式计算 |
| 4. 业务层 | 数据解码、属性计算、告警处理 | ✅ 采集公式计算 |
| 5. 数据层 | 时序数据存储、数据查询 | ❌ 禁止公式计算 |
| 6. 缓存层 | 实时数据缓存、设备状态缓存 | ❌ 禁止公式计算 |
| 7. API层 | 实时数据查询、历史数据查询 | ✅ 控制公式计算（API调用时） |

## 2. 采集公式放置位置

### 2.1 正确位置：业务层（dgiot_task_service.erl）

#### 技术实现
```erlang
%% 在dgiot_task_service.erl中的采集公式计算
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(Prop, Acc) ->
        case maps:get(<<"dataForm">>, Prop, #{}) of
            #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} = DataForm ->
                %% 采集公式计算
                case string2value(Collection, maps:get(<<"type">>, DataForm, <<"float">>), 
                                 maps:get(<<"specs">>, DataForm, #{})) of
                    error -> maps:without([maps:get(<<"identifier">>, Prop)], Acc);
                    Value1 -> Acc#{maps:get(<<"identifier">>, Prop) => Value1}
                end;
            _ -> Acc
        end
    end, Calculated, Props).
```

#### 放置理由
1. **符合架构原则**：业务层负责属性计算和业务逻辑
2. **数据完整性**：此时已获得完整的解析数据
3. **变量可用**：可以访问解码器变量、设备历史变量
4. **错误处理**：业务层有完善的错误处理机制

### 2.2 错误位置示例

#### ❌ 错误：协议层（modbus_rtu.erl）
```erlang
%% 错误示例：在协议层进行复杂公式计算
parse_frame(Data, Acc, State) ->
    %% 协议解析...
    %% 错误：在协议层进行业务公式计算
    Temperature = RawValue * 0.0625 + 25.5,  # 业务公式不应该在协议层
    {Rest, Acc#{<<"temperature">> => Temperature}}.
```

#### ❌ 错误：通讯层（dgiot_modbusrtu_tcp.erl）
```erlang
%% 错误示例：在通讯层进行公式计算
handle_info({tcp, Buff}, TCPState) ->
    %% 错误：通讯层不应该知道业务公式
    Value = binary_to_integer(Buff) * 1.8 + 32,  # 业务逻辑泄露到通讯层
    {noreply, TCPState}.
```

## 3. 控制公式放置位置

### 3.1 正确位置：协议层（modbus_rtu.erl） + 业务层（dgiot_task_service.erl）

#### 3.1.1 控制公式解析：业务层
```erlang
%% 在dgiot_task_service.erl中的控制公式解析
get_control(Round, Data, Control) ->
    %% 解析控制公式，生成控制指令模板
    ControlTemplate = parse_control_formula(Control, Data),
    {ok, ControlTemplate}.
```

#### 3.1.2 控制公式执行：协议层
```erlang
%% 在modbus_rtu.erl中的控制公式执行
build_rtu_request(Value, Setting, SlaveId, Address, OperateType, Originaltype, DataSource, Acc) ->
    %% 应用控制公式：%{d}替换为实际值
    Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
    
    %% 调用公式引擎计算最终值
    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),
    
    %% 构建RTU请求
    RtuReq = #rtu_req{
        slaveId = SlaveId,
        funcode = dgiot_utils:to_int(FunCode),
        address = Address,
        quality = Value1
    },
    Acc ++ [modbus_rtu_encoder:build_req_message(RtuReq)].
```

#### 放置理由
1. **职责分离**：
   - 业务层：解析控制公式，生成指令模板
   - 协议层：执行控制公式，构建具体协议指令
2. **协议无关性**：业务层不关心具体协议细节
3. **协议相关性**：协议层知道如何构建特定协议的指令

### 3.2 控制公式调用流程
```
前端API → 业务层解析控制公式 → 生成控制指令模板 → 协议层执行公式 → 构建协议指令 → 发送到设备
```

## 4. 物模型配置中的公式定义

### 4.1 采集公式配置位置
```json
{
  "identifier": "temperature",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{raw_temp} * 0.0625"  # 采集公式定义在这里
  },
  "dataType": {
    "type": "float",
    "specs": {"precision": 1}
  }
}
```

### 4.2 控制公式配置位置
```json
{
  "identifier": "target_temperature",
  "dataForm": {
    "strategy": "控制值", 
    "control": "%{d} * 1.8 + 32"  # 控制公式定义在这里
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X0000"
  }
}
```

## 5. 公式引擎的统一放置

### 5.1 公式引擎位置：dgiot_task_service.erl

#### 核心公式引擎函数
```erlang
%% 统一公式引擎（dgiot_task_service.erl）
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

#### 放置理由
1. **统一维护**：所有公式计算使用同一套引擎
2. **错误处理统一**：统一的异常捕获和默认值返回
3. **性能优化统一**：可以统一优化公式计算性能
4. **类型转换统一**：统一的类型转换和精度处理

## 6. 各层调用关系

### 6.1 采集公式调用链
```
设备数据 → 通讯层接收 → 协议层解析 → 业务层计算（采集公式） → 数据层存储
      ↓
dgiot_modbusrtu_tcp.erl → modbus_rtu.erl → dgiot_task_service.erl → TDengine
```

### 6.2 控制公式调用链
```
前端请求 → API层接收 → 业务层解析（控制公式） → 协议层执行 → 通讯层发送
      ↓
dgiot_xxx_handler.erl → dgiot_task_service.erl → modbus_rtu.erl → dgiot_tcp_server.erl
```

## 7. 最佳实践总结

### 7.1 采集公式放置最佳实践

#### ✅ 正确做法
1. **定义位置**：物模型配置的`dataForm.collection`字段
2. **计算位置**：`dgiot_task_service.erl`的`get_calculated/4`函数
3. **引擎位置**：`dgiot_task_service.erl`的`string2value/3`函数

#### ❌ 避免做法
1. 不要在协议层进行复杂业务公式计算
2. 不要在通讯层进行任何公式计算
3. 不要在不同地方重复实现公式引擎

### 7.2 控制公式放置最佳实践

#### ✅ 正确做法
1. **定义位置**：物模型配置的`dataForm.control`字段
2. **解析位置**：`dgiot_task_service.erl`的`get_control/3`函数
3. **执行位置**：协议层（如`modbus_rtu.erl`）的`build_rtu_request/8`函数

#### ❌ 避免做法
1. 不要在API层直接执行控制公式
2. 不要在业务层构建具体协议指令
3. 不要在不同协议层重复实现控制公式逻辑

### 7.3 公式引擎最佳实践

#### ✅ 正确做法
1. **统一引擎**：所有公式使用`dgiot_task_service:string2value/3`
2. **统一调用**：通过`dgiot_task:string2value/2`对外暴露接口
3. **统一优化**：在公式引擎层统一进行性能优化

## 8. 配置示例

### 8.1 完整物模型配置示例
```json
{
  "productId": "temperature_controller",
  "productName": "温度控制器",
  "thing": {
    "properties": [
      {
        "identifier": "current_temperature",
        "name": "当前温度",
        "dataForm": {
          "strategy": "计算值",
          "collection": "%%{raw_temp} * 0.0625",  # 采集公式
          "protocol": "MODBUSRTU"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X0000"
        },
        "dataType": {
          "type": "float",
          "specs": {"unit": "°C", "precision": 1}
        }
      },
      {
        "identifier": "target_temperature",
        "name": "目标温度",
        "dataForm": {
          "strategy": "控制值",
          "control": "%{d} * 1.8 + 32",  # 控制公式（华氏度转摄氏度）
          "protocol": "MODBUSRTU"
        },
        "dataSource": {
          "slaveid": "0X01",
          "address": "0X0001"
        },
        "dataType": {
          "type": "float",
          "specs": {"unit": "°C", "precision": 1}
        }
      }
    ]
  }
}
```

### 8.2 代码调用示例

#### 采集公式调用
```erlang
%% 在dgiot_task_service.erl中
case dgiot_product:lookup_prod(ProductId) of
    {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
        Calculated = parse_raw_data(RawData),  % 协议层解析的基础数据
        Result = get_calculated(ProductId, DevAddr, Calculated, Props),  % 业务层公式计算
        save_to_database(Result);
    _ -> {error, product_not_found}
end.
```

#### 控制公式调用
```erlang
%% 在dgiot_task_service.erl中（API调用时）
handle_control_request(DeviceId, TargetValue) ->
    %% 获取控制公式配置
    {ok, ControlFormula} = get_control_formula(DeviceId),
    
    %% 解析控制公式
    ControlTemplate = get_control(1, #{value => TargetValue}, ControlFormula),
    
    %% 调用协议层执行
    modbus_rtu:set_params(ControlTemplate, ProductId, DevAddr).
```

## 9. 总结

### 9.1 核心原则

#### 采集公式放置原则
1. **定义在物模型**：`dataForm.collection`字段
2. **计算在业务层**：`dgiot_task_service.erl`
3. **引擎统一**：`string2value/3`函数

#### 控制公式放置原则
1. **定义在物模型**：`dataForm.control`字段
2. **解析在业务层**：`dgiot_task_service.erl`
3. **执行在协议层**：协议相关模块（如`modbus_rtu.erl`）

### 9.2 架构优势

1. **符合七层架构**：各层职责清晰，不越界
2. **统一维护**：公式引擎统一，便于维护和优化
3. **灵活扩展**：通过配置支持新公式，无需修改代码
4. **错误隔离**：各层独立错误处理，避免级联故障

### 9.3 实际意义

1. **对开发者**：清晰的代码组织和职责划分
2. **对维护者**：统一的维护入口和错误处理
3. **对用户**：通过配置即可定义复杂业务逻辑
4. **对系统**：高性能、高可靠性的公式计算能力

DG-IoT平台的这种公式放置设计体现了现代软件工程的**分层设计**、**单一职责**和**开闭原则**，为工业物联网应用提供了强大而灵活的数据处理能力。
