#!/bin/bash
echo "=== Modbus RTU简化测试 ==="
echo "测试时间: $(date)"
echo ""

# 配置
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000
TEST_PRODUCT="feeb43bffb"

echo "1. 检查端口监听..."
if netstat -tlnp | grep ":$TEST_PORT" > /dev/null; then
    echo "✅ 端口 $TEST_PORT 正在监听"
else
    echo "❌ 端口 $TEST_PORT 未监听"
    echo "尝试启动Modbus通道..."
    _build/emqx/rel/emqx/bin/emqx eval '
        io:format("尝试启动Modbus通道~n"),
        Args = #{
            <<"port">> => 20000,
            <<"regtype">> => <<"RegisterByPort">>,
            <<"regular">> => <<"wrj_**-***">>,
            <<"product">> => [#{<<"feeb43bffb">> => #{}}],
            <<"dtutype">> => <<"DGIOT">>
        },
        case dgiot_modbus_channel:init(<<"MODBUS">>, <<"test_channel">>, Args) of
            {ok, State, Spec} -> 
                io:format("通道初始化成功: ~p~n", [State]),
                io:format("Child spec: ~p~n", [Spec]);
            Error -> 
                io:format("通道初始化失败: ~p~n", [Error])
        end.
    ' 2>&1 | grep -v "escript"
fi

echo ""
echo "2. 发送注册报文..."
echo "发送: $TEST_DEVICE"
if echo "$TEST_DEVICE" | nc -w 5 127.0.0.1 $TEST_PORT 2>/dev/null; then
    echo "✅ 注册报文发送成功"
else
    echo "❌ 注册报文发送失败"
fi

echo ""
echo "3. 检查日志..."
sleep 2
tail -20 _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(tcp|RegisterByPort|wrj_dm-zqy|error)" | tail -5

echo ""
echo "=== 测试完成 ==="
