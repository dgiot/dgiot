# 公式计算系统详解

## 概述

本文档详细说明DG-IoT平台中的公式计算系统，包括计算决策机制、公式语法、变量作用域和执行流程。

## 1. 计算决策机制

### 1.1 物模型配置驱动

#### 策略字段定义
dgiot_task通过解析物模型中的`strategy`字段决定是否需要计算：

```json
{
  "identifier": "actual_temperature",
  "dataForm": {
    "strategy": "计算值",  // 需要计算
    "collection": "%%{raw_temperature} * 0.0625"
  }
}
```

#### 策略类型
1. **计算值** (`strategy: "计算值"`)：需要执行计算公式
2. **上报值** (`strategy: "上报值"`)：直接使用原始值
3. **默认策略**：未指定时默认为"上报值"

### 1.2 决策执行流程

```erlang
%% dgiot_task_service.erl中的决策逻辑
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(Prop, Acc) ->
        case maps:get(<<"dataForm">>, Prop, #{}) of
            #{<<"strategy">> := <<"计算值">>, <<"collection">> := Collection} ->
                %% 需要计算：调用采集公式
                Value = calculate_collection_formula(Collection, Calculated, Prop),
                Acc#{maps:get(<<"identifier">>, Prop) => Value};
            #{<<"strategy">> := <<"上报值">>} ->
                %% 直接使用：从Calculated中获取
                Identifier = maps:get(<<"identifier">>, Prop),
                case maps:get(Identifier, Calculated, undefined) of
                    undefined -> Acc;
                    Value -> Acc#{Identifier => Value}
                end;
            _ ->
                %% 其他策略或未定义
                Acc
        end
    end, #{}, Props).
```

## 2. 公式语法系统

### 2.1 基本语法

#### 变量引用
```erlang
%% 引用其他属性的值
"%%{property_name}"  %% 引用名为property_name的属性值

%% 示例
"%%{raw_temperature} * 0.0625"  %% 引用raw_temperature属性
```

#### 数学运算
```erlang
%% 支持的基本运算
"%%{a} + %%{b}"      %% 加法
"%%{a} - %%{b}"      %% 减法  
"%%{a} * %%{b}"      %% 乘法
"%%{a} / %%{b}"      %% 除法
"(%%{a} + %%{b}) * 0.5"  %% 括号和混合运算
```

#### 数据块访问
```erlang
%% 数据块偏移量访问
"block_data[0:2] * 0.1"      %% 访问数据块0-2字节
"block_data[2:4] * 0.1"      %% 访问数据块2-4字节
"block_data[4:6] * 0.1"      %% 访问数据块4-6字节
```

### 2.2 高级语法

#### 条件表达式
```erlang
%% 条件判断
"if(%%{temp} > 30, '高温', '正常')"

%% 多条件判断
"if(%%{temp} > 30, '高温', if(%%{temp} < 10, '低温', '正常'))"
```

#### 函数调用
```erlang
%% 内置函数
"sqrt(%%{value})"            %% 平方根
"abs(%%{value})"             %% 绝对值
"round(%%{value}, 2)"        %% 四舍五入到2位小数
```

#### 时间函数
```erlang
%% 时间相关计算
"now()"                      %% 当前时间戳
"hour(%%{timestamp})"        %% 提取小时
"date(%%{timestamp})"        %% 提取日期
```

## 3. 变量作用域

### 3.1 变量来源

#### 1. 基础属性值
```erlang
%% 从Calculated映射中获取
Calculated = #{
    <<"raw_temperature">> => 250,
    <<"raw_humidity">> => 500,
    <<"block_data">> => <<0, 100, 0, 200, 0, 300>>
}
```

#### 2. 数据块提取值
```erlang
%% 从数据块中提取
BlockData = <<0, 100, 0, 200, 0, 300>>,
Value1 = binary:decode_unsigned(binary:part(BlockData, 0, 2)),  %% 100
Value2 = binary:decode_unsigned(binary:part(BlockData, 2, 2)),  %% 200
Value3 = binary:decode_unsigned(binary:part(BlockData, 4, 2)),  %% 300
```

#### 3. 环境变量
```erlang
%% 系统环境变量
Env = #{
    <<"device_id">> => DeviceId,
    <<"product_id">> => ProductId,
    <<"timestamp">> => Timestamp
}
```

### 3.2 作用域层次

#### 第一层：当前属性计算
```erlang
%% 当前属性的计算公式中只能引用已解析的属性
"%%{raw_temperature} * 0.0625"  %% raw_temperature必须已存在
```

#### 第二层：数据块依赖
```erlang
%% 计算值属性可以依赖数据块属性
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1"  %% 依赖block_data
  },
  "dataSource": {
    "key": "block_data"  %% 指定依赖的基础属性
  }
}
```

#### 第三层：跨属性引用
```erlang
%% 属性间可以相互引用（注意循环依赖）
{
  "identifier": "temp_celsius",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{raw_temp} * 0.1"
  }
},
{
  "identifier": "temp_fahrenheit", 
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{temp_celsius} * 1.8 + 32"  %% 引用另一个计算值属性
  }
}
```

## 4. 计算执行流程

### 4.1 完整计算流程

```
原始数据 → 协议解析 → 基础属性提取 → 计算值属性计算 → 结果存储
```

#### 详细步骤：
1. **协议解析**：调用协议钩子解析原始数据
2. **基础属性提取**：提取上报值属性到Calculated映射
3. **计算值属性识别**：识别strategy="计算值"的属性
4. **公式解析**：解析collection字段中的公式
5. **变量替换**：将%%{variable}替换为实际值
6. **表达式求值**：执行数学表达式计算
7. **结果存储**：将计算结果存入Calculated映射

### 4.2 代码实现

#### 公式解析函数
```erlang
%% 解析并计算公式
calculate_collection_formula(Collection, Calculated, Prop) ->
    %% 1. 提取公式中的变量
    Variables = extract_variables(Collection),
    
    %% 2. 获取变量值
    Values = get_variable_values(Variables, Calculated, Prop),
    
    %% 3. 替换变量
    Expression = replace_variables(Collection, Values),
    
    %% 4. 执行计算
    case dgiot_task_service:string2value(Expression, <<"float">>, #{}) of
        {ok, Value} -> Value;
        {error, Reason} ->
            ?LOG(error, "公式计算失败: ~p, 公式: ~p", [Reason, Collection]),
            undefined
    end.
```

#### 变量提取函数
```erlang
%% 提取公式中的变量
extract_variables(Collection) ->
    %% 匹配 %%{variable_name} 格式
    Pattern = "%%\\{([^}]+)\\}",
    case re:run(Collection, Pattern, [global, {capture, all, list}]) of
        {match, Matches} ->
            lists:map(fun([_, Var]) -> list_to_binary(Var) end, Matches);
        nomatch ->
            []
    end.
```

#### 变量值获取函数
```erlang
%% 获取变量值
get_variable_values(Variables, Calculated, Prop) ->
    lists:foldl(fun(Variable, Acc) ->
        case maps:get(Variable, Calculated, undefined) of
            undefined ->
                %% 尝试从数据源获取
                case get_value_from_datasource(Variable, Prop) of
                    {ok, Value} -> Acc#{Variable => Value};
                    {error, _} -> Acc
                end;
            Value ->
                Acc#{Variable => Value}
        end
    end, #{}, Variables).
```

## 5. 错误处理与调试

### 5.1 常见错误

#### 错误1：变量未定义
```erlang
%% 症状：公式引用不存在的变量
"%%{non_existent} * 0.1"  %% non_existent变量不存在

%% 解决方案：检查物模型配置，确保引用的属性已定义
```

#### 错误2：公式语法错误
```erlang
%% 症状：公式语法不正确
"%%{a} * "  %% 不完整的公式

%% 解决方案：验证公式语法，使用公式验证工具
```

#### 错误3：循环依赖
```erlang
%% 症状：属性间相互引用形成循环
A: "%%{B} * 2"
B: "%%{A} / 2"  %% 循环依赖

%% 解决方案：检查物模型配置，避免循环依赖
```

### 5.2 调试工具

#### 日志输出
```erlang
%% 在公式计算关键点添加日志
?LOG(debug, "开始计算属性 ~p", [Identifier]),
?LOG(debug, "公式: ~p", [Collection]),
?LOG(debug, "变量值: ~p", [Values]),
?LOG(debug, "计算结果: ~p", [Value]).
```

#### 公式验证工具
```erlang
%% 公式验证函数
validate_formula(Collection, Calculated) ->
    try
        %% 尝试执行公式
        {ok, _} = dgiot_task_service:string2value(
            replace_test_variables(Collection), 
            <<"float">>, 
            #{}
        ),
        {ok, valid}
    catch
        _:Reason ->
            {error, {invalid_formula, Reason}}
    end.
```

#### 测试数据生成
```erlang
%% 生成测试数据验证公式
generate_test_data_for_formula(Collection) ->
    %% 提取公式中的变量
    Variables = extract_variables(Collection),
    
    %% 为每个变量生成测试值
    TestValues = lists:map(fun(Var) ->
        {Var, rand:uniform(100)}
    end, Variables),
    
    %% 替换变量并计算
    Expression = replace_variables(Collection, maps:from_list(TestValues)),
    {ok, Value} = dgiot_task_service:string2value(Expression, <<"float">>, #{}),
    
    #{test_values => TestValues, result => Value}.
```

## 6. 性能优化

### 6.1 公式预编译

#### 缓存解析结果
```erlang
%% 缓存公式解析结果
-define(FORMULA_CACHE, formula_cache).

cache_formula(Collection, ParsedFormula) ->
    dgiot_data:insert(?FORMULA_CACHE, Collection, ParsedFormula).

get_cached_formula(Collection) ->
    case dgiot_data:get(?FORMULA_CACHE, Collection) of
        not_find -> undefined;
        ParsedFormula -> ParsedFormula
    end.
```

#### 预编译常用公式
```erlang
%% 预编译常用公式模板
precompile_common_formulas() ->
    CommonFormulas = [
        {"%%{raw} * 0.1", fun(Raw) -> Raw * 0.1 end},
        {"%%{a} + %%{b}", fun(A, B) -> A + B end},
        {"%%{a} * %%{b}", fun(A, B) -> A * B end}
    ],
    
    lists:foreach(fun({Formula, Fun}) ->
        cache_formula(Formula, Fun)
    end, CommonFormulas).
```

### 6.2 批量计算优化

#### 批量变量替换
```erlang
%% 批量处理多个公式
batch_calculate_formulas(Formulas, Calculated) ->
    %% 提取所有公式的变量
    AllVariables = lists:foldl(fun({_, Collection}, Acc) ->
        Variables = extract_variables(Collection),
        sets:union(Acc, sets:from_list(Variables))
    end, sets:new(), Formulas),
    
    %% 批量获取变量值
    VariableValues = get_batch_variable_values(
        sets:to_list(AllVariables), 
        Calculated
    ),
    
    %% 批量计算
    lists:map(fun({Identifier, Collection}) ->
        Value = calculate_with_preloaded_values(
            Collection, 
            VariableValues
        ),
        {Identifier, Value}
    end, Formulas).
```

## 7. 扩展机制

### 7.1 自定义函数注册

#### 函数注册接口
```erlang
%% 注册自定义函数
register_custom_function(Name, Arity, Fun) ->
    dgiot_data:insert(custom_functions, {Name, Arity}, Fun).

%% 调用自定义函数
call_custom_function(Name, Args) ->
    case dgiot_data:get(custom_functions, {Name, length(Args)}) of
        not_find -> {error, function_not_found};
        Fun -> apply(Fun, Args)
    end.
```

#### 示例：注册温度转换函数
```erlang
%% 注册摄氏度转华氏度函数
register_custom_function(celsius_to_fahrenheit, 1, 
    fun(Celsius) -> Celsius * 1.8 + 32 end).

%% 在公式中使用
"celsius_to_fahrenheit(%%{temp_celsius})"
```

### 7.2 插件式公式引擎

#### 插件接口定义
```erlang
%% 公式引擎插件行为
-callback parse_formula(Collection :: binary(), 
                       Context :: map()) -> 
    {ok, ParsedFormula} | {error, Reason}.

-callback evaluate_formula(ParsedFormula :: term(),
                          Variables :: map()) ->
    {ok, Value} | {error, Reason}.
```

#### 插件注册
```erlang
%% 注册公式引擎插件
register_formula_engine(Name, Module) ->
    dgiot_data:insert(formula_engines, Name, Module).

%% 根据公式类型选择引擎
select_formula_engine(Collection) ->
    %% 根据公式特征选择引擎
    case is_math_expression(Collection) of
        true -> math_engine;
        false -> default_engine
    end.
```

## 8. 最佳实践

### 8.1 公式设计原则

#### 保持简单
```erlang
%% 好：简单直接的公式
"%%{raw} * 0.0625"

%% 不好：过于复杂的公式
"if(%%{a} > %%{b}, %%{a} * 0.1 + %%{c} / 2, %%{b} * 0.2 - %%{d})"
```

#### 避免循环依赖
```erlang
%% 好：单向依赖
A → B → C

%% 不好：循环依赖  
A → B → A
```

#### 使用有意义的变量名
```erlang
%% 好：有意义的变量名
"%%{temperature_raw} * 0.0625"

%% 不好：无意义的变量名
"%%{a} * 0.0625"
```

### 8.2 性能优化建议

#### 预编译常用公式
- 将频繁使用的公式预编译缓存
- 减少运行时解析开销

#### 批量处理
- 批量获取变量值
- 批量计算公式结果

#### 监控和调优
- 监控公式计算性能
- 识别性能瓶颈
- 优化热点公式

## 9. 测试验证

### 9.1 单元测试

#### 公式解析测试
```erlang
formula_parsing_test() ->
    ?assertEqual([<<"raw_temperature">>], 
                 extract_variables(<<"%%{raw_temperature} * 0.0625">>)),
    ?assertEqual([<<"a">>, <<"b">>], 
                 extract_variables(<<"%%{a} + %%{b}">>)).
```

#### 公式计算测试
```erlang
formula_calculation_test() ->
    Calculated = #{<<"raw_temperature">> => 250},
    ?assertEqual(15.625, 
                 calculate_collection_formula(
                     <<"%%{raw_temperature} * 0.0625">>, 
                     Calculated, 
                     #{}
                 )).
```

### 9.2 集成测试

#### 完整流程测试
```erlang
end_to_end_formula_test() ->
    %% 1. 准备测试数据
    RawData = <<...>>,
    Props = [...],
    
    %% 2. 执行完整流程
    {ok, Calculated} = dgiot_task:get_calculated(
        <<"test_product">>, 
        <<"test_device">>, 
        #{}, 
        Props
    ),
    
    %% 3. 验证结果
    ?assert(maps:is_key(<<"actual_temperature">>, Calculated)),
    ?assert(is_number(maps:get(<<"actual_temperature">>, Calculated))).
```

## 10. 更新记录

### 版本历史
- **v1.0 (2025-12-25)**
