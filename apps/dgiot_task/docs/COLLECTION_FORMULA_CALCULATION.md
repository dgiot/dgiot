# DG-IoT采集公式计算方式详细分析

## 概述

本文档详细分析DG-IoT平台中采集公式的计算方式，列举各种计算场景和实现机制。

## 1. 采集公式计算的基本原理

### 1.1 公式引擎核心
DG-IoT使用Erlang的`erl_eval`模块作为公式计算引擎：

```erlang
%% 核心计算函数（dgiot_task_service.erl）
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

### 1.2 计算流程
```
原始公式字符串 → 词法分析(erl_scan) → 语法分析(erl_parse) → 表达式求值(erl_eval) → 结果
```

## 2. 采集公式计算的各种情况

### 2.1 简单算术运算

#### 情况1：基本四则运算
```json
{
  "collection": "10 + 20 * 3"
}
```
**计算过程**：
1. 公式字符串：`"10 + 20 * 3"`
2. 词法分析：`[10, '+', 20, '*', 3]`
3. 语法分析：`{op, '+', 10, {op, '*', 20, 3}}`
4. 求值：`10 + (20 * 3) = 70`

#### 情况2：带括号的运算
```json
{
  "collection": "(10 + 20) * 3"
}
```
**计算过程**：
1. 公式：`"(10 + 20) * 3"`
2. 语法树：`{op, '*', {op, '+', 10, 20}, 3}`
3. 求值：`(10 + 20) * 3 = 90`

### 2.2 变量替换计算

#### 情况3：使用物模型属性变量
```json
{
  "collection": "%%{temperature} * 1.8 + 32"
}
```
**计算过程**：
1. 变量替换前：`"%%{temperature} * 1.8 + 32"`
2. 获取当前温度值：假设`temperature = 25`
3. 变量替换后：`"25 * 1.8 + 32"`
4. 求值：`25 * 1.8 + 32 = 77`

#### 情况4：多个变量计算
```json
{
  "collection": "%%{pressure} / %%{area}"
}
```
**计算过程**：
1. 假设：`pressure = 100`, `area = 5`
2. 替换后：`"100 / 5"`
3. 求值：`20`

### 2.3 数据块偏移计算

#### 情况5：从数据块提取子数据
```json
{
  "collection": "block_data[0:2] * 0.1"
}
```
**在modbus_rtu_decoder.erl中的处理**：
```erlang
%% 提取数据块片段
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
end.
```

**计算过程**：
1. 提取`block_data`的前2个字节
2. 将二进制数据转换为数值
3. 乘以0.1得到最终值

### 2.4 类型转换计算

#### 情况6：整数转浮点数
```json
{
  "collection": "%%{raw_value} / 10.0",
  "dataType": {
    "type": "float",
    "specs": {"precision": 2}
  }
}
```
**在string2value/3中的类型转换**：
```erlang
case Type1 of
    <<"INT">> -> round(Value);
    Type2 when Type2 == <<"FLOAT">>; Type2 == <<"DOUBLE">> ->
        Precision = maps:get(<<"precision">>, Specs, 3),
        dgiot_utils:to_float(Value, Precision);
    _ -> Value
end
```

**计算过程**：
1. 假设：`raw_value = 123`
2. 公式计算：`123 / 10.0 = 12.3`
3. 类型转换：保留2位小数 → `12.30`

### 2.5 条件判断计算

#### 情况7：条件表达式
```json
{
  "collection": "if %%{value} > 100 then %%{value} * 0.9 else %%{value}"
}
```
**Erlang表达式计算**：
```erlang
%% Erlang的if表达式
if 
    Value > 100 -> Value * 0.9;
    true -> Value
end
```

**计算过程**：
1. 假设：`value = 120`
2. 条件判断：`120 > 100` → true
3. 计算：`120 * 0.9 = 108`

### 2.6 函数调用计算

#### 情况8：数学函数计算
```json
{
  "collection": "sin(%%{angle} * 3.14159 / 180)"
}
```
**计算过程**：
1. 假设：`angle = 30`
2. 角度转弧度：`30 * 3.14159 / 180 = 0.5236`
3. 正弦计算：`sin(0.5236) = 0.5`

### 2.7 统计计算

#### 情况9：累计统计
```json
{
  "collection": "%%{count} + 1",
  "dataSource": {
    "type": "frequency"
  }
}
```
**在get_statistic函数中的处理**：
```erlang
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, 
              #{<<"type">> := <<"frequency">>} = DataSource, Acc) ->
    dgiot_task_utils:handle_frequency_statistic(ProductId, DevAddr, Key, 
                                               Identifier, KeyValue, DataSource, Acc);
```

## 3. 实际应用场景示例

### 3.1 温度传感器计算

#### 场景1：原始值转实际温度
```json
{
  "identifier": "temperature",
  "collection": "%%{raw_temp} * 0.0625",
  "dataType": {
    "type": "float",
    "specs": {"precision": 1}
  }
}
```
**计算示例**：
- 原始值：`raw_temp = 400`
- 计算：`400 * 0.0625 = 25.0`
- 结果：`25.0°C`

### 3.2 压力传感器计算

#### 场景2：带零点和量程校准
```json
{
  "identifier": "pressure",
  "collection": "(%%{raw_pressure} - 2048) * 100 / 16384",
  "dataType": {
    "type": "float",
    "specs": {"precision": 2}
  }
}
```
**计算示例**：
- 原始值：`raw_pressure = 10240`
- 计算：`(10240 - 2048) * 100 / 16384 = 50.0`
- 结果：`50.0 kPa`

### 3.3 流量计计算

#### 场景3：脉冲计数转流量
```json
{
  "identifier": "flow_rate",
  "collection": "%%{pulse_count} * 0.01 / %%{time_interval}",
  "dataType": {
    "type": "float",
    "specs": {"precision": 3}
  }
}
```
**计算示例**：
- 脉冲数：`pulse_count = 1500`
- 时间间隔：`time_interval = 60` (秒)
- 计算：`1500 * 0.01 / 60 = 0.25`
- 结果：`0.250 m³/s`

## 4. 计算过程中的关键技术

### 4.1 变量绑定机制

```erlang
%% 在dgiot_task_service.erl中的变量替换
Str1 = maps:fold(fun(K, V, Acc2) ->
    Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%{", K/binary, "}">>), 
                   dgiot_utils:to_list(V), [global, {return, list}]),
    re:replace(Str, "%{s}", dgiot_utils:to_list(V), [global, {return, list}])
end, dgiot_utils:to_list(Collection), Calculated),
```

### 4.2 错误处理机制

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
```

### 4.3 性能优化技术

#### 4.3.1 表达式缓存
```erlang
%% 可以实现的表达式缓存优化
case get_cached_expression(FormulaStr) of
    {ok, {Tokens, Exprs}} ->
        %% 使用缓存的词法和语法分析结果
        erl_eval:exprs(Exprs, Bindings);
    not_found ->
        %% 首次分析并缓存
        {ok, Tokens} = erl_scan:string(FormulaStr ++ "."),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        cache_expression(FormulaStr, {Tokens, Exprs}),
        erl_eval:exprs(Exprs, Bindings)
end
```

#### 4.3.2 变量预绑定
```erlang
%% 预绑定常用函数
Bindings = erl_eval:new_bindings(),
Bindings1 = erl_eval:add_binding('sin', fun math:sin/1, Bindings),
Bindings2 = erl_eval:add_binding('cos', fun math:cos/1, Bindings1),
Bindings3 = erl_eval:add_binding('sqrt', fun math:sqrt/1, Bindings2),
```

## 5. 计算流程总结

### 5.1 完整计算流程
```
1. 获取原始公式字符串
2. 变量替换：%%{variable} → 实际值
3. 词法分析：字符串 → Token列表
4. 语法分析：Token列表 → 抽象语法树
5. 表达式求值：语法树 + 绑定环境 → 结果
6. 类型转换：根据dataType规范转换结果
7. 精度处理：根据specs设置保留小数位数
```

### 5.2 各阶段示例

**示例公式**：`"%%{temp} * 1.8 + 32"`

| 阶段 | 输入 | 输出 | 说明 |
|------|------|------|------|
| 原始公式 | `"%%{temp} * 1.8 + 32"` | - | 从物模型配置获取 |
| 变量替换 | `temp = 25` | `"25 * 1.8 + 32"` | 替换变量为实际值 |
| 词法分析 | `"25 * 1.8 + 32"` | `[25, '*', 1.8, '+', 32]` | 分割为Token |
| 语法分析 | Token列表 | `{op, '+', {op, '*', 25, 1.8}, 32}` | 构建语法树 |
| 表达式求值 | 语法树 | `77.0` | 计算表达式值 |
| 类型转换 | `77.0` | `77.0` | 保持float类型 |
| 精度处理 | `77.0` | `77.0` | 保留1位小数 |

## 6. 特殊情况处理

### 6.1 除零错误处理
```json
{
  "collection": "%%{numerator} / %%{denominator}"
}
```
**处理方式**：
```erlang
case Denominator of
    0 -> 0;  % 返回默认值，避免除零错误
    _ -> Numerator / Denominator
end
```

### 6.2 无效变量处理
```json
{
  "collection": "%%{valid_var} + %%{invalid_var}"
}
```
**处理方式**：
```erlang
case maps:get(invalid_var, Calculated, not_find) of
    not_find -> ValidVar;  % 忽略无效变量
    InvalidVar -> ValidVar + InvalidVar
end
```

### 6.3 公式语法错误处理
```json
{
  "collection": "10 + * 20"  # 语法错误
}
```
**处理方式**：
```erlang
case erl_parse:parse_exprs(Tokens) of
    {error, ErrorInfo} -> 
        ?LOG(error, "Formula syntax error: ~p", [ErrorInfo]),
        0;  % 返回默认值
    {ok, Exprs} -> ...
end
```

## 7. 总结

### 7.1 采集公式计算特点
1. **灵活性强**：支持算术运算、变量替换、函数调用等
2. **类型安全**：自动类型转换和精度控制
3. **错误容忍**：完善的错误处理和默认值机制
4. **性能优化**：支持表达式缓存和预绑定优化

### 7.2 适用场景
1. **传感器数据转换**：原始值转实际物理量
2. **数据校准计算**：零点和量程校准
3. **统计计算**：累计、平均、最大最小值
4. **业务逻辑计算**：条件判断、复杂公式

### 7.3 技术优势
1. **使用Erlang原生能力**：无需外部依赖
2. **动态计算**：支持运行时公式修改
3. **配置驱动**：通过物模型配置定义计算逻辑
4. **统一处理**：所有公式使用同一套计算引擎

DG-IoT平台的采集公式计算机制提供了强大而灵活的数据处理能力，能够满足各种工业物联网场景的需求。
