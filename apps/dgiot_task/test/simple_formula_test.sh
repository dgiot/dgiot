#!/bin/bash

# 简单公式测试脚本

echo "=== 公式计算器简单测试 ==="
echo ""

# 编译模块
echo "1. 编译公式计算器模块..."
erlc -o apps/dgiot_task/ebin apps/dgiot_task/src/utils/dgiot_formula_calculator_simple.erl

echo "2. 编译测试模块..."
erlc -o apps/dgiot_task/test apps/dgiot_task/test/dgiot_formula_calculator_simple_test.erl

echo "3. 运行测试..."
erl -pa apps/dgiot_task/ebin -pa apps/dgiot_task/test -eval '
    io:format("=== 运行单元测试 ===~n"),
    eunit:test(dgiot_formula_calculator_simple_test),
    
    io:format("~n=== 手动测试 ===~n"),
    
    % 测试1: 简单加法
    Collection1 = <<"%%{a} + %%{b}">>,
    Variables1 = #{<<"a">> => 10, <<"b">> => 20},
    Prop1 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>}},
    Result1 = dgiot_formula_calculator_simple:calculate_formula(Collection1, Variables1, Prop1),
    io:format("测试1: ~s = ~p~n", [Collection1, Result1]),
    
    % 测试2: 带括号的运算
    Collection2 = <<"(%%{x} + %%{y}) * %%{z}">>,
    Variables2 = #{<<"x">> => 5, <<"y">> => 3, <<"z">> => 2},
    Prop2 = #{<<"dataType">> => #{<<"type">> => <<"INT">>}},
    Result2 = dgiot_formula_calculator_simple:calculate_formula(Collection2, Variables2, Prop2),
    io:format("测试2: ~s = ~p~n", [Collection2, Result2]),
    
    % 测试3: 除法运算
    Collection3 = <<"%%{total} / %%{count}">>,
    Variables3 = #{<<"total">> => 100, <<"count">> => 4},
    Prop3 = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}},
    Result3 = dgiot_formula_calculator_simple:calculate_formula(Collection3, Variables3, Prop3),
    io:format("测试3: ~s = ~p~n", [Collection3, Result3]),
    
    % 测试4: 变量提取
    Collection4 = <<"(%%{x} - %%{min}) / (%%{max} - %%{min})">>,
    Vars4 = dgiot_formula_calculator_simple:extract_variables(Collection4),
    io:format("测试4: 提取变量 ~s => ~p~n", [Collection4, Vars4]),
    
    init:stop().'

echo ""
echo "=== 测试完成 ==="
