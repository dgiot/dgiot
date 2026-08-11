# Modbus设备采集通道调用采集公式和控制公式的场景分析

## 概述

本文档详细分析Modbus设备采集通道在什么情况下可以调用采集公式和控制公式，基于DG-IoT平台的七层架构和实际应用场景。

## 1. Modbus设备数据流架构

### 1.1 完整数据流
```
设备数据 → Modbus TCP/UDP通道 → 协议解析 → 任务通道 → 业务处理 → 数据存储
      ↓
dgiot_modbusrtu_tcp.erl → modbus_rtu.erl → dgiot_task.erl → dgiot_task_service.erl → TDengine
```

### 1.2 控制流
```
前端请求 → API层 → 业务层 → 协议层 → Modbus通道 → 设备
      ↓
dgiot_xxx_handler.erl → dgiot_task_service.erl → modbus_rtu.erl → dgiot_tcp_server.erl
```

## 2. 采集公式调用场景

### 2.1 场景1：数据上报时调用采集公式

#### 触发条件
- **设备主动上报数据**：设备定时或事件触发上报
- **通道接收到数据**：`dgiot_modbusrtu_tcp.erl`的`handle_info/2`函数
- **数据需要业务计算**：物模型配置了`strategy = "计算值"`

#### 调用流程
```erlang
%% 1. 通道接收数据
dgiot_modbusrtu_tcp:handle_info({tcp, Buff}, TCPState) ->
    %% 发送到任务通道
    send_to_task_channel(ChannelId, ProductId, DtuAddr, Things, ProductId).

%% 2. 任务通道处理
dgiot_task:save_td(ProductId, DevAddr, Ack, _AppData) ->
    %% 调用协议钩子解析
    dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, [ProductId, DevAddr, Data]).

%% 3. 协议解析
modbus_rtu:parse_frame(Buff, Acc, State) ->
    %% 基础协议解析
    {Rest, ParsedData}.

%% 4. 业务层采集公式计算
dgiot_task_service:get_calculated(ProductId, DevAddr, Calculated, Props) ->
    %% 遍历属性，计算采集公式
    case maps:get(<<"dataForm">>, Prop, #{}) of
        #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} ->
            string2value(Collection, Type, Specs);
        _ -> ...
    end.
```

#### 实际示例：温度传感器
```json
{
  "identifier": "actual_temperature",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{raw_temp} * 0.0625"  # 原始值转实际温度
  }
}
```
**调用时机**：每次温度传感器上报数据时

### 2.2 场景2：数据块模式下的采集公式

#### 触发条件
- **数据块模式**：`is_data_block_mode/3`返回true
- **多个属性共享数据块**：如角度传感器的X/Y/Z三个角度
- **需要从数据块提取并计算**：使用`block_data[offset:length]`语法

#### 调用流程
```erlang
%% 1. 检查数据块模式
modbus_rtu:is_data_block_mode(ProductId, SlaveId, Address) ->
    %% 检查物模型配置
    has_data_block_config(ProductId, SlaveId, Address).

%% 2. 数据块处理
modbus_rtu_data_blocks:process_data_blocks(DataBlockCache, Props) ->
    %% 递归处理属性
    process_props_recursive(Props, DataBlockCache, #{}).

%% 3. 采集公式计算
case maps:get(<<"dataForm">>, Prop, #{}) of
    #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} ->
        %% 如：block_data[0:2] * 0.1
        string2value(Collection, Type, Specs);
    _ -> ...
end.
```

#### 实际示例：角度传感器
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"  # 从数据块提取并计算
  }
}
```
**调用时机**：数据块模式下，每个计算值属性都需要调用采集公式

### 2.3 场景3：历史数据计算

#### 触发条件
- **需要历史数据参与计算**：如日累计、月平均等
- **设备级别变量**：使用`%%{variable}`格式引用历史数据
- **定时计算任务**：通过任务编排定时触发

#### 调用流程
```erlang
%% 1. 获取历史数据
get_device_history(ProductId, DevAddr, TimeRange) ->
    dgiot_tdengine_adapter:query(ProductId, Query).

%% 2. 准备变量环境
Env = Calculated#{<<"last_hour_avg">> => AvgValue,
                  <<"yesterday_total">> => YesterdayTotal}.

%% 3. 采集公式计算
string2value("%%{current_value} - %%{last_hour_avg}", Type, Specs).
```

#### 实际示例：能耗计算
```json
{
  "identifier": "daily_energy",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{current_reading} - %%{midnight_reading}"  # 日累计能耗
  }
}
```
**调用时机**：每次数据上报时，需要查询历史数据参与计算

## 3. 控制公式调用场景

### 3.1 场景1：设备控制指令下发

#### 触发条件
- **前端控制请求**：用户通过界面设置设备参数
- **API调用**：`dgiot_xxx_handler.erl`处理控制请求
- **物模型配置控制公式**：`strategy = "控制值"`

#### 调用流程
```erlang
%% 1. API层接收请求
dgiot_device_handler:handle_request('POST', <<"/api/v1/control">>, Req) ->
    %% 解析请求参数
    {ok, Body, _} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    
    %% 调用业务层
    dgiot_task_service:get_control(Round, Data, Control).

%% 2. 业务层解析控制公式
get_control(Round, Data, Control) ->
    %% 解析控制公式，生成指令模板
    {ok, ControlTemplate}.

%% 3. 协议层执行控制公式
modbus_rtu:set_params(Payload, ProductId, DevAddr) ->
    %% 遍历参数，构建RTU请求
    lists:foldl(fun process_single_param/2, [], Payload).

%% 4. 执行控制公式
build_rtu_request(Value, Setting, SlaveId, Address, ...) ->
    %% 应用控制公式：%{d}替换为实际值
    Str1 = re:replace(Setting, "%{d}", "(" ++ Value ++ ")", [global]),
    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),
    
    %% 构建协议指令
    RtuReq = #rtu_req{quality = Value1}.
```

#### 实际示例：温度设定
```json
{
  "identifier": "target_temperature",
  "dataForm": {
    "strategy": "控制值",
    "control": "%{d} * 1.8 + 32"  # 摄氏度转华氏度
  }
}
```
**调用时机**：用户设置目标温度时，需要将摄氏度转换为设备理解的华氏度

### 3.2 场景2：自动控制任务

#### 触发条件
- **定时控制任务**：通过物模型任务编排配置
- **条件触发控制**：满足特定条件时自动控制
- **联动控制**：多个设备联动控制

#### 调用流程
```erlang
%% 1. 任务编排触发
dgiot_task_service:execute_task(ProductId, Task) ->
    %% 获取当前轮次
    Round = get_current_round(ProductId, Identifier),
    
    %% 生成控制指令
    Instruct = get_instruct(ProductId, Round),
    
    %% 执行控制
    send_control_command(ProductId, Instruct).

%% 2. 控制公式应用
process_single_param(Data, Acc) ->
    #{<<"value">> := Value, <<"setting">> := Setting} = Data,
    
    %% 应用控制公式
    build_rtu_request(Value, Setting, ...).
```

#### 实际示例：定时通风控制
```json
{
  "identifier": "fan_speed",
  "dataForm": {
    "strategy": "控制值",
    "control": "if %{d} > 30 then 100 else 50"  # 温度高于30度全速，否则半速
  }
}
```
**调用时机**：定时任务触发时，根据当前温度自动控制风扇转速

### 3.3 场景3：批量控制指令

#### 触发条件
- **批量设备控制**：同时控制多个设备
- **参数归一化**：不同设备需要不同的控制公式
- **协议转换**：统一控制参数，转换为设备特定协议

#### 调用流程
```erlang
%% 1. 批量控制请求
handle_batch_control(DeviceList, ControlParams) ->
    lists:foreach(fun(Device) ->
        %% 获取设备特定的控制公式
        ControlFormula = get_device_control_formula(Device),
        
        %% 应用控制公式
        Command = apply_control_formula(ControlParams, ControlFormula),
        
        %% 发送控制指令
        send_to_device(Device, Command)
    end, DeviceList).
```

#### 实际示例：多品牌设备控制
```json
{
  "identifier": "valve_position",
  "dataForm": {
    "strategy": "控制值",
    "control": "brand_a_formula(%{d})"  # 品牌A专用公式
  }
}
```
**调用时机**：批量控制不同品牌设备时，需要应用不同的控制公式

## 4. 特殊场景分析

### 4.1 场景：协议层直接调用采集公式（特殊情况）

#### 触发条件
- **简单实时计算**：不需要历史数据的简单计算
- **性能要求高**：需要最低延迟
- **计算在协议层已完成**：如数据格式转换

#### 限制条件
- **仅限简单计算**：不能涉及历史数据查询
- **不能有副作用**：不能修改设备状态或历史数据
- **必须轻量级**：不能影响协议解析性能

#### 示例：原始值缩放
```erlang
%% 在modbus_rtu_decoder.erl中
format_value(Fragment, #{<<"originaltype">> := <<"short16_AB">>}, _) ->
    <<Value:16/signed>> = Fragment,
    {Value * 0.1, <<>>}.  # 简单缩放计算
```
**注意**：这属于基础数据解析，不是业务层采集公式

### 4.2 场景：混合公式调用

#### 触发条件
- **既有采集又有控制**：如自动调节系统
- **反馈控制**：根据采集结果自动控制
- **复杂业务逻辑**：需要多个公式协同工作

#### 调用流程
```erlang
%% 1. 采集数据并计算
CollectedData = get_calculated(ProductId, DevAddr, RawData, Props),

%% 2. 根据采集结果决定控制
case maps:get(<<"temperature">>, CollectedData) of
    Temp when Temp > 30 ->
        %% 调用控制公式
        ControlCommand = get_control(1, #{value => 100}, ControlFormula),
        send_control(ControlCommand);
    _ -> ok
end.
```

#### 实际示例：温控系统
```json
[
  {
    "identifier": "current_temp",
    "dataForm": {"strategy": "计算值", "collection": "%%{raw} * 0.1"}
  },
  {
    "identifier": "heater_power", 
    "dataForm": {"strategy": "控制值", "control": "pid_control(%{d}, %%{current_temp})"}
  }
]
```
**调用时机**：采集温度后，根据PID算法自动控制加热器功率

## 5. 调用条件和限制

### 5.1 采集公式调用条件

#### ✅ 允许调用的情况：
1. **数据上报时**：设备主动上报数据
2. **任务触发时**：定时采集任务触发
3. **历史计算时**：需要历史数据参与计算
4. **数据块模式**：从数据块提取并计算
5. **实时计算**：简单的实时数据转换

#### ❌ 禁止调用的情况：
1. **协议解析过程中**：影响协议解析性能
2. **通讯层**：违反七层架构原则
3. **无数据时**：没有基础数据可供计算
4. **错误状态**：设备或通道处于错误状态

### 5.2 控制公式调用条件

#### ✅ 允许调用的情况：
1. **用户控制请求**：前端API调用
2. **自动控制任务**：定时或条件触发
3. **批量控制**：批量设备控制
4. **联动控制**：设备间联动控制
5. **协议转换**：统一参数转换为设备特定值

#### ❌ 禁止调用的情况：
1. **数据上报过程中**：控制不应干扰数据采集
2. **设备离线时**：设备不在线无法控制
3. **无权限时**：用户没有控制权限
4. **安全限制**：违反安全策略的控制

## 6. 最佳实践建议

### 6.1 采集公式最佳实践

1. **简单计算放协议层**：如数据格式转换、单位换算
2. **复杂计算放业务层**：涉及历史数据、业务逻辑的计算
3. **避免频繁历史查询**：缓存历史数据，减少数据库查询
4. **错误处理完善**：公式计算失败时提供默认值
5. **性能监控**：监控公式计算性能，及时优化

### 6.2 控制公式最佳实践

1. **参数验证**：控制参数必须验证范围和有效性
2. **设备状态检查**：控制前检查设备在线状态
3. **控制结果反馈**：控制后验证执行结果
4. **安全限制**：重要的控制操作需要安全验证
5. **批量控制优化**：批量控制时优化网络通信

### 6.3 性能优化建议

1. **公式缓存**：缓存编译后的公式表达式
2. **变量预计算**：预计算不变的变量
3. **异步处理**：耗时的计算异步处理
4. **批量处理**：批量数据一起计算
5. **监控告警**：监控公式计算性能，设置告警

## 7. 总结

### 7.1 采集公式调用场景总结

Modbus设备采集通道在以下情况调用采集公式：
1. **设备数据上报时**：将原始数据转换为业务数据
2. **数据块模式下**：从共享数据块提取并计算多个属性
3. **历史数据计算时**：需要历史数据参与的趋势计算
4. **定时采集任务**：定时触发的数据采集和计算
5. **简单实时计算**：轻量级的实时数据转换

### 7.2 控制公式调用场景总结

Modbus设备采集通道在以下情况调用控制公式：
1. **用户控制请求**：前端发起的设备控制
2. **自动控制任务**：定时或条件触发的自动控制
3. **批量设备控制**：同时控制多个设备
4. **协议参数转换**：将统一参数转换为设备特定值
5. **联动控制系统**：多个设备协同工作的复杂控制

### 7.3 架构设计意义

1. **职责清晰**：各层职责明确，不越界调用
2. **性能优化**：根据场景选择最优调用位置
3. **灵活扩展**：支持各种复杂的业务场景
4. **可靠稳定**：完善的错误处理和安全控制
5. **易于维护**：清晰的调用关系和日志记录

DG-IoT平台的这种设计使得Modbus设备采集通道能够灵活、高效、可靠地处理各种采集和控制需求，满足工业物联网应用的复杂业务场景。
