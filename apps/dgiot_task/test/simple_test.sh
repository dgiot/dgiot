#!/bin/bash
# 简单的dgiot_task测试脚本

echo "========================================"
echo "开始dgiot_task模块简单测试"
echo "========================================"

# 切换到项目根目录
cd "$(dirname "$0")/../../.."

echo "1. 编译dgiot_task模块..."
echo "----------------------------------------"

# 使用热编译命令
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_task).'

echo "2. 测试模块加载..."
echo "----------------------------------------"

# 测试模块是否加载成功
_build/emqx/rel/emqx/bin/emqx eval '
    case code:ensure_loaded(dgiot_task) of
        {module, dgiot_task} ->
            io:format("✅ dgiot_task模块加载成功~n"),
            io:format("模块信息: ~p~n", [dgiot_task:module_info()]);
        Error ->
            io:format("❌ dgiot_task模块加载失败: ~p~n", [Error])
    end.
'

echo "3. 测试基本函数..."
echo "----------------------------------------"

# 测试基本函数
_build/emqx/rel/emqx/bin/emqx eval '
    io:format("测试string2value函数...~n"),
    case dgiot_task:string2value("1+2", <<"int">>) of
        3 -> io:format("✅ string2value测试通过~n");
        Result -> io:format("❌ string2value测试失败，结果: ~p~n", [Result])
    end,
    
    io:format("测试compare函数...~n"),
    case dgiot_task:compare(5, <<"LT">>, 10) of
        true -> io:format("✅ compare测试通过~n");
        Result2 -> io:format("❌ compare测试失败，结果: ~p~n", [Result2])
    end.
'

echo "4. 测试物模型函数..."
echo "----------------------------------------"

# 测试物模型函数
_build/emqx/rel/emqx/bin/emqx eval '
    ProductId = <<"test_product">>,
    io:format("测试get_props函数...~n"),
    Props = dgiot_task:get_props(ProductId),
    io:format("属性数量: ~p~n", [length(Props)]),
    
    io:format("测试get_control函数...~n"),
    ControlResult = dgiot_task:get_control(1, #{<<"value">> => 10}, <<"control">>),
    io:format("控制结果: ~p~n", [ControlResult]).
'

echo "5. 测试统计函数..."
echo "----------------------------------------"

# 测试统计函数
_build/emqx/rel/emqx/bin/emqx eval '
    io:format("测试get_last_value函数...~n"),
    LastValue = dgiot_task:get_last_value(<<"test_product">>, <<"test_device">>, <<"temperature">>, <<"avg">>),
    io:format("最后值: ~p~n", [LastValue]).
'

echo "========================================"
echo "简单测试完成"
echo "========================================"
