# DG-IoT采集公式和控制公式实现分析

## 问题：采集公式和控制公式是不是只需要实现一次就可以了？

**简短回答：是的，DG-IoT平台中采集公式和控制公式的核心实现只需要一次，通过统一的公式引擎和配置驱动机制实现。**

## 详细分析

### 1. 公式实现的统一性

#### 1.1 核心公式引擎
DG-IoT平台使用**统一的公式引擎**来处理所有类型的公式：

```erlang
%% 核心公式计算函数（在dgiot_task_service.erl中）
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

%% 底层公式解析（使用Erlang原生表达式计算）
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

#### 1.2 公式引擎的特点
1. **统一入口**：所有公式都通过`string2value/2`或`string2value/3`函数处理
2. **统一语法**：使用Erlang表达式语法，支持数学运算、函数调用等
3. **统一错误处理**：统一的异常捕获和默认值返回机制
4. **统一类型转换**：自动处理INT、FLOAT、DOUBLE等数据类型

### 2. 采集公式的实现

#### 2.1 采集公式调用点
采集公式在**两个地方**被调用，但使用**同一个公式引擎**：

```erlang
%% 1. 在modbus_rtu_decoder.erl中（协议层）
case catch format_value(Fragment, X, []) of
    {Value1, _Rest} -> Acc#{Identifier => Value1};
    _ -> Acc
end.

%% 2. 在dgiot_task_service.erl中（业务层）
case string2value(Str1, Type, Specs) of
    error -> maps:without([Identifier], Acc);
    Value1 -> Acc#{Identifier => Value1}
end.
```

#### 2.2 采集公式配置
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"  # 采集公式
  }
}
```

### 3. 控制公式的实现

#### 3.1 控制公式调用点
控制公式在**一个地方**被调用，使用**同一个公式引擎**：

```erlang
%% 在modbus_rtu.erl中（控制指令构建）
Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>))
```

#### 3.2 控制公式配置
```json
{
  "identifier": "target_temperature",
  "dataForm": {
    "strategy": "控制值",
    "control": "%{d} * 10"  # 控制公式
  }
}
```

### 4. 为什么只需要实现一次？

#### 4.1 架构设计原则
1. **DRY原则**（Don't Repeat Yourself）：避免重复代码
2. **单一职责原则**：公式引擎只负责公式计算
3. **开闭原则**：通过扩展配置支持新公式，而不是修改代码

#### 4.2 技术实现优势
1. **统一维护**：公式引擎bug修复只需修改一处
2. **统一升级**：公式功能升级只需升级引擎
3. **统一测试**：公式引擎可以独立测试
4. **统一优化**：性能优化只需优化引擎

### 5. 公式引擎的复用机制

#### 5.1 函数复用
```erlang
%% 所有模块都调用同一个公式引擎
dgiot_task:string2value/2           # 从dgiot_task模块调用
dgiot_task_service:string2value/3   # 从服务层调用（内部转发）
modbus_rtu_decoder:format_value/3   # 从解码器调用（最终调用string2value）
```

#### 5.2 配置复用
```erlang
%% 物模型配置驱动公式调用
case maps:get(<<"strategy">>, DataForm) of
    <<"计算值">> -> 
        Collection = maps:get(<<"collection">>, DataForm),
        string2value(Collection, Type, Specs);
    <<"控制值">> -> 
        Control = maps:get(<<"control">>, DataForm),
        string2value(Control, Type, Specs);
    _ -> ...
end
```

### 6. 实际应用示例

#### 6.1 角度传感器采集公式
```json
{
  "collection": "block_data[0:2] * 0.1 + 25.5"
}
```
**调用流程**：
1. 协议层：`modbus_rtu_decoder:format_value/3`
2. 业务层：`dgiot_task_service:string2value/3`
3. 引擎层：`erl_eval:exprs/2`

#### 6.2 温度控制公式
```json
{
  "control": "(%{d} - 32) * 5 / 9"  # 华氏度转摄氏度
}
```
**调用流程**：
1. 控制层：`modbus_rtu:build_rtu_request/8`
2. 公式层：`dgiot_task:string2value/2`
3. 引擎层：`erl_eval:exprs/2`

### 7. 扩展性设计

#### 7.1 新公式类型支持
要支持新类型的公式，**不需要修改公式引擎**，只需：
1. 在物模型中添加新配置
2. 在调用点添加对新配置的处理
3. 公式引擎自动处理

#### 7.2 新函数支持
要支持新函数（如三角函数、对数函数），**只需要扩展公式引擎**：
```erlang
%% 扩展公式引擎支持新函数
Bindings = erl_eval:new_bindings(),
%% 添加数学函数绑定
Bindings1 = erl_eval:add_binding('sin', fun math:sin/1, Bindings),
Bindings2 = erl_eval:add_binding('cos', fun math:cos/1, Bindings1),
%% 所有公式自动支持新函数
```

### 8. 性能考虑

#### 8.1 公式引擎性能优化
1. **表达式预编译**：可以缓存编译后的表达式
2. **变量预绑定**：预绑定常用变量和函数
3. **结果缓存**：缓存相同公式的计算结果

#### 8.2 调用性能
```erlang
%% 优化后的调用示例
case get_cached_formula(FormulaStr) of
    {ok, CompiledExpr} ->
        %% 使用缓存的编译表达式
        erl_eval:exprs(CompiledExpr, Bindings);
    not_found ->
        %% 首次编译并缓存
        {ok, Tokens, _} = erl_scan:string(FormulaStr ++ "."),
        {ok, CompiledExpr} = erl_parse:parse_exprs(Tokens),
        cache_formula(FormulaStr, CompiledExpr),
        erl_eval:exprs(CompiledExpr, Bindings)
end
```

### 9. 总结

#### 9.1 核心结论
**是的，采集公式和控制公式在DG-IoT平台中只需要实现一次**，通过：

1. **统一的公式引擎**：`string2value`函数
2. **统一的调用接口**：所有模块调用同一个引擎
3. **统一的配置机制**：通过物模型配置驱动公式调用
4. **统一的错误处理**：统一的异常捕获和默认值

#### 9.2 架构优势
1. **维护简单**：公式逻辑集中在一处
2. **扩展容易**：通过配置支持新公式
3. **测试方便**：公式引擎可以独立测试
4. **性能优化**：可以集中优化公式计算性能
5. **质量保证**：统一的质量控制和错误处理

#### 9.3 实际意义
对于DG-IoT平台开发者和用户来说：
- **开发者**：不需要为每个设备或协议重新实现公式逻辑
- **用户**：可以通过配置定义复杂的业务逻辑，无需编码
- **维护者**：bug修复和功能升级只需修改一处

这种设计体现了**"一次实现，多处使用"**的软件工程最佳实践，是DG-IoT平台架构设计的重要优势。
