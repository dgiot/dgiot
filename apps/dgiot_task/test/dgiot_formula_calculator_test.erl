%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc 公式计算器测试模块
-module(dgiot_formula_calculator_test).

-include_lib("eunit/include/eunit.hrl").

%% 测试集
all_test_() ->
    [
        {"基础公式计算测试", fun basic_formula_test/0},
        {"变量提取测试", fun variable_extraction_test/0},
        {"公式验证测试", fun formula_validation_test/0},
        {"预编译公式测试", fun precompile_formula_test/0},
        {"批量计算测试", fun batch_calculation_test/0},
        {"自定义函数测试", fun custom_function_test/0},
        {"类型转换测试", fun type_conversion_test/0},
        {"复杂公式测试", fun complex_formula_test/0}
    ].

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 基础公式计算测试
basic_formula_test() ->
    %% 测试1: 简单算术运算
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => 10, <<"b">> => 20},
    Prop1 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
    
    Result1 = dgiot_formula_calculator:calculate_formula(Collection1, Variables1, Prop1),
    ?assertEqual(30.0, Result1),
    
    %% 测试2: 带括号的运算
    Collection2 = <<"(%%{x} + %%{y}) * %%{z}">>,
    Variables2 = #{<<"x">> => 5, <<"y">> => 3, <<"z">> => 2},
    Prop2 = #{<<"dataType">> => #{<<"type">> => <<"INT">>}},
    
    Result2 = dgiot_formula_calculator:calculate_formula(Collection2, Variables2, Prop2),
    ?assertEqual(16, Result2),
    
    %% 测试3: 除法运算
    Collection3 = <<"%%{total} / %%{count}">>,
    Variables3 = #{<<"total">> => 100, <<"count">> => 4},
    Prop3 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 1}}},
    
    Result3 = dgiot_formula_calculator:calculate_formula(Collection3, Variables3, Prop3),
    ?assertEqual(25.0, Result3).

%% @doc 变量提取测试
variable_extraction_test() ->
    %% 测试1: 提取单个变量
    Collection1 = <<"%%{temperature} * 1.8 + 32">>,
    Variables1 = dgiot_formula_calculator:extract_variables(Collection1),
    ?assertEqual([<<"temperature">>], Variables1),
    
    %% 测试2: 提取多个变量
    Collection2 = <<"(%%{x} - %%{min}) / (%%{max} - %%{min})">>,
    Variables2 = dgiot_formula_calculator:extract_variables(Collection2),
    ?assertEqual([<<"x">>, <<"min">>, <<"max">>], lists:sort(Variables2)),
    
    %% 测试3: 无变量
    Collection3 = <<"3.14159 * 2">>,
    Variables3 = dgiot_formula_calculator:extract_variables(Collection3),
    ?assertEqual([], Variables3).

%% @doc 公式验证测试
formula_validation_test() ->
    %% 测试1: 有效公式验证
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => 10, <<"b">> => 20},
    
    {ok, ValidVariables1} = dgiot_formula_calculator:validate_formula(Collection1, Variables1),
    ?assertEqual(#{<<"a">> => 10, <<"b">> => 20}, ValidVariables1),
    
    %% 测试2: 缺失变量验证
    Collection2 = <<"%%{x} * %%{y} + %%{z}">>,
    Variables2 = #{<<"x">> => 5, <<"y">> => 3},
    
    {error, {missing_variables, MissingVars}} = dgiot_formula_calculator:validate_formula(Collection2, Variables2),
    ?assertEqual([<<"z">>], MissingVars),
    
    %% 测试3: 空变量验证
    Collection3 = <<"3.14 * 2">>,
    Variables3 = #{},
    
    {ok, ValidVariables3} = dgiot_formula_calculator:validate_formula(Collection3, Variables3),
    ?assertEqual(#{}, ValidVariables3).

%% @doc 预编译公式测试
precompile_formula_test() ->
    %% 测试1: 预编译简单公式
    Collection1 = <<"%%{a} * %%{b}">>,
    {ok, CompiledFun1} = dgiot_formula_calculator:precompile_formula(Collection1),
    
    Variables1 = #{<<"a">> => 5, <<"b">> => 4},
    Result1 = CompiledFun1(Variables1),
    ?assertEqual(20, Result1),
    
    %% 测试2: 预编译复杂公式
    Collection2 = <<"sqrt(%%{x} * %%{x} + %%{y} * %%{y})">>,
    {ok, CompiledFun2} = dgiot_formula_calculator:precompile_formula(Collection2),
    
    Variables2 = #{<<"x">> => 3, <<"y">> => 4},
    Result2 = CompiledFun2(Variables2),
    ?assertEqual(5.0, Result2),
    
    %% 测试3: 预编译失败（语法错误）
    Collection3 = <<"%%{a} + * %%{b}">>,
    {error, {compilation_failed, _}} = dgiot_formula_calculator:precompile_formula(Collection3).

%% @doc 批量计算测试
batch_calculation_test() ->
    %% 测试数据
    Formulas = [
        {<<"avg_temperature">>, <<"(%%{temp1} + %%{temp2} + %%{temp3}) / 3">>},
        {<<"max_temperature">>, <<"max([%%{temp1}, %%{temp2}, %%{temp3}])">>},
        {<<"min_temperature">>, <<"min([%%{temp1}, %%{temp2}, %%{temp3}])">>}
    ],
    
    Variables = #{
        <<"temp1">> => 25.5,
        <<"temp2">> => 26.0,
        <<"temp3">> => 24.5
    },
    
    %% 执行批量计算
    Results = dgiot_formula_calculator:batch_calculate(Formulas, Variables),
    
    %% 验证结果
    ?assertEqual(25.333333333333332, maps:get(<<"avg_temperature">>, Results)),
    ?assertEqual(26.0, maps:get(<<"max_temperature">>, Results)),
    ?assertEqual(24.5, maps:get(<<"min_temperature">>, Results)).

%% @doc 自定义函数测试
custom_function_test() ->
    %% 注册自定义函数
    SquareFun = fun(X) -> X * X end,
    dgiot_formula_calculator:register_custom_function(<<"square">>, 1, SquareFun),
    
    %% 测试自定义函数调用
    Result1 = dgiot_formula_calculator:call_custom_function(<<"square">>, [5]),
    ?assertEqual(25, Result1),
    
    %% 测试不存在的函数
    {error, function_not_found} = dgiot_formula_calculator:call_custom_function(<<"nonexistent">>, [1, 2, 3]),
    
    %% 测试函数执行错误
    ErrorFun = fun(_) -> error(test_error) end,
    dgiot_formula_calculator:register_custom_function(<<"error_fun">>, 1, ErrorFun),
    
    {error, function_execution_failed} = dgiot_formula_calculator:call_custom_function(<<"error_fun">>, [123]).

%% @doc 类型转换测试
type_conversion_test() ->
    %% 测试1: INT类型转换
    Collection1 = <<"%%{value}">>,
    Variables1 = #{<<"value">> => 3.7},
    Prop1 = #{<<"dataType">> => #{<<"type">> => <<"INT">>}},
    
    Result1 = dgiot_formula_calculator:calculate_formula(Collection1, Variables1, Prop1),
    ?assertEqual(4, Result1),
    
    %% 测试2: FLOAT类型转换（指定精度）
    Collection2 = <<"%%{a} / %%{b}">>,
    Variables2 = #{<<"a">> => 10, <<"b">> => 3},
    Prop2 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
    
    Result2 = dgiot_formula_calculator:calculate_formula(Collection2, Variables2, Prop2),
    ?assertEqual(3.33, Result2),
    
    %% 测试3: DOUBLE类型转换（高精度）
    Collection3 = <<"sqrt(%%{x})">>,
    Variables3 = #{<<"x">> => 2},
    Prop3 = #{<<"dataType">> => #{<<"type">> => <<"DOUBLE">>, <<"specs">> => #{<<"precision">> => 6}}},
    
    Result3 = dgiot_formula_calculator:calculate_formula(Collection3, Variables3, Prop3),
    ?assertEqual(1.414214, Result3),
    
    %% 测试4: TEXT类型转换
    Collection4 = <<"\"Temperature: \" ++ float_to_list(%%{temp}, [{decimals, 1}])">>,
    Variables4 = #{<<"temp">> => 25.5},
    Prop4 = #{<<"dataType">> => #{<<"type">> => <<"TEXT">>}},
    
    Result4 = dgiot_formula_calculator:calculate_formula(Collection4, Variables4, Prop4),
    ?assert(is_binary(Result4)).

%% @doc 复杂公式测试
complex_formula_test() ->
    %% 测试1: 条件表达式
    Collection1 = <<"if(%%{value} > 100, \"High\", \"Normal\")">>,
    Variables1_high = #{<<"value">> => 150},
    Variables1_normal = #{<<"value">> => 80},
    Prop1 = #{<<"dataType">> => #{<<"type">> => <<"TEXT">>}},
    
    Result1_high = dgiot_formula_calculator:calculate_formula(Collection1, Variables1_high, Prop1),
    Result1_normal = dgiot_formula_calculator:calculate_formula(Collection1, Variables1_normal, Prop1),
    
    ?assertEqual(<<"High">>, Result1_high),
    ?assertEqual(<<"Normal">>, Result1_normal),
    
    %% 测试2: 温度转换公式
    Collection2 = <<"celsius_to_fahrenheit(%%{celsius})">>,
    Variables2 = #{<<"celsius">> => 25},
    Prop2 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 1}}},
    
    Result2 = dgiot_formula_calculator:calculate_formula(Collection2, Variables2, Prop2),
    ?assertEqual(77.0, Result2),
    
    %% 测试3: 统计计算
    Collection3 = <<"avg([%%{t1}, %%{t2}, %%{t3}, %%{t4}])">>,
    Variables3 = #{<<"t1">> => 20, <<"t2">> => 22, <<"t3">> => 21, <<"t4">> => 23},
    Prop3 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
    
    Result3 = dgiot_formula_calculator:calculate_formula(Collection3, Variables3, Prop3),
    ?assertEqual(21.5, Result3),
    
    %% 测试4: 带超时的复杂计算
    Collection4 = <<"lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, %%{n}))">>,
    Variables4 = #{<<"n">> => 5},
    Prop4 = #{<<"dataType">> => #{<<"type">> => <<"INT">>}},
    Options4 = #{timeout => 1000, precision => 0},
    
    Result4 = dgiot_formula_calculator:calculate_formula(Collection4, Variables4, Prop4, Options4),
    ?assertEqual(120, Result4).

%%%===================================================================
%%% 模块导出
%%%===================================================================

%% @doc 运行测试套件
run_test_suite() ->
    eunit:test({module, ?MODULE}).

%% @doc 快速测试
quick_test() ->
    run_test_suite().
