#!/bin/bash
# 快速测试脚本

echo "========================================"
echo "开始dgiot_task模块快速测试"
echo "========================================"

cd "$(dirname "$0")/../../.."

echo "1. 编译模块..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_task).'

echo "2. 测试模块加载..."
_build/emqx/rel/emqx/bin/emqx eval 'io:format("测试开始~n").'
_build/emqx/rel/emqx/bin/emqx eval 'io:format("模块: ~p~n", [dgiot_task:module_info(name)]).'

echo "3. 测试简单函数..."
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:string2value("1+2", <<"int">>), io:format("string2value结果: ~p~n", [Result]).'
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:compare(5, <<"LT">>, 10), io:format("compare结果: ~p~n", [Result]).'

echo "4. 测试物模型函数..."
_build/emqx/rel/emqx/bin/emqx eval 'Props = dgiot_task:get_props(<<"test">>), io:format("属性数量: ~p~n", [length(Props)]).'
_build/emqx/rel/emqx/bin/emqx eval 'Control = dgiot_task:get_control(1, #{<<"value">> => 10}, <<"control">>), io:format("控制结果: ~p~n", [Control]).'

echo "========================================"
echo "快速测试完成"
echo "========================================"
