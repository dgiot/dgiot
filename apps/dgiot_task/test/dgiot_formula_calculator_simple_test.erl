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

%% @doc 公式计算器简单测试模块
-module(dgiot_formula_calculator_simple_test).

-include_lib("eunit/include/eunit.hrl").

%% 测试集
all_test_() ->
    [
        {"测试变量提取", fun test_extract_variables/0},
        {"测试变量替换", fun test_replace_variables/0},
        {"测试表达式计算", fun test_evaluate_expression/0},
        {"测试公式验证", fun test_validate_formula/0},
        {"测试简单公式计算", fun test_simple_formula_calculation/0}
    ].

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试变量提取
test_extract_variables() ->
    %% 测试1: 提取单个变量
    Collection1 = <<"%%{temperature} * 1.8 + 32">>,
    Variables1 = dgiot_formula_calculator_simple:extract_variables(Collection1),
    ?assertEqual([<<"temperature">>], Variables1),
    
    %% 测试2: 提取多个变量（注意：min出现两次，但只提取一次）
    Collection2 = <<"(%%{x} - %%{min}) / (%%{max} - %%{min})">>,
    Variables2 = dgiot_formula_calculator_simple:extract_variables(Collection2),
    ?assertEqual([<<"max">>, <<"min">>, <<"x">>], lists:sort(Variables2)),
    
    %% 测试3: 无变量
    Collection3 = <<"3.14159 * 2">>,
    Variables3 = dgiot_formula_calculator_simple:extract_variables(Collection3),
    ?assertEqual([], Variables3).

%% @doc 测试变量替换
test_replace_variables() ->
    %% 测试1: 替换单个变量
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => "10", <<"b">> => "20"},
    Result1 = dgiot_formula_calculator_simple:replace_variables(Collection1, Variables1),
    ?assertEqual("10 + 20", Result1),
    
    %% 测试2: 替换多个变量
    Collection2 = <<"%%{x} * %%{y} + %%{z}">>,
    Variables2 = #{<<"x">> => "5", <<"y">> => "3", <<"z">> => "2"},
    Result2 = dgiot_formula_calculator_simple:replace_variables(Collection2, Variables2),
    ?assertEqual("5 * 3 + 2", Result2).

%% @doc 测试表达式计算
test_evaluate_expression() ->
    %% 测试1: 简单算术
    Expression1 = "10 + 20",
    Result1 = dgiot_formula_calculator_simple:evaluate_expression(Expression1),
    ?assertEqual(30, Result1),
    
    %% 测试2: 带括号的运算
    Expression2 = "(5 + 3) * 2",
    Result2 = dgiot_formula_calculator_simple:evaluate_expression(Expression2),
    ?assertEqual(16, Result2),
    
    %% 测试3: 除法运算
    Expression3 = "100 / 4",
    Result3 = dgiot_formula_calculator_simple:evaluate_expression(Expression3),
    ?assertEqual(25.0, Result3).

%% @doc 测试公式验证
test_validate_formula() ->
    %% 测试1: 有效公式验证
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => 10, <<"b">> => 20},
    {ok, ValidVariables1} = dgiot_formula_calculator_simple:validate_formula(Collection1, Variables1),
    ?assertEqual(#{<<"a">> => 10, <<"b">> => 20}, ValidVariables1),
    
    %% 测试2: 缺失变量验证
    Collection2 = <<"%%{x} * %%{y} + %%{z}">>,
    Variables2 = #{<<"x">> => 5, <<"y">> => 3},
    {error, {missing_variables, MissingVars}} = dgiot_formula_calculator_simple:validate_formula(Collection2, Variables2),
    ?assertEqual([<<"z">>], MissingVars),
    
    %% 测试3: 空变量验证
    Collection3 = <<"3.14 * 2">>,
    Variables3 = #{},
    {ok, ValidVariables3} = dgiot_formula_calculator_simple:validate_formula(Collection3, Variables3),
    ?assertEqual(#{}, ValidVariables3).

%% @doc 测试简单公式计算
test_simple_formula_calculation() ->
    %% 测试1: 简单算术运算
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => 10, <<"b">> => 20},
    Prop1 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
    
    Result1 = dgiot_formula_calculator_simple:calculate_formula(Collection1, Variables1, Prop1),
    ?assertEqual(30.0, Result1),
    
    %% 测试2: 带括号的运算
    Collection2 = <<"(%%{x} + %%{y}) * %%{z}">>,
    Variables2 = #{<<"x">> => 5, <<"y">> => 3, <<"z">> => 2},
    Prop2 = #{<<"dataType">> => #{<<"type">> => <<"INT">>}},
    
    Result2 = dgiot_formula_calculator_simple:calculate_formula(Collection2, Variables2, Prop2),
    ?assertEqual(16, Result2),
    
    %% 测试3: 除法运算
    Collection3 = <<"%%{total} / %%{count}">>,
    Variables3 = #{<<"total">> => 100, <<"count">> => 4},
    Prop3 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 1}}},
    
    Result3 = dgiot_formula_calculator_simple:calculate_formula(Collection3, Variables3, Prop3),
    ?assertEqual(25.0, Result3).

%%%===================================================================
%%% 模块导出
%%%===================================================================

%% @doc 运行测试套件
run_test_suite() ->
    eunit:test({module, ?MODULE}).

%% @doc 快速测试
quick_test() ->
    run_test_suite().
