#!/bin/bash
# 快速编译测试

echo "=== 开始快速编译测试 ==="
echo ""

# 切换到项目目录
cd /root/gitee/dgiot

# 清理之前的编译产物（可选）
echo "1. 清理编译产物..."
# rm -rf _build/emqx/rel/emqx/lib/dgiot_uav*

echo "2. 编译dgiot_uav插件..."
make emqx 2>&1 | tee /tmp/compile.log

# 检查编译结果
echo ""
echo "=== 编译结果检查 ==="

if grep -q "Compiling dgiot_uav_auto_tester.erl failed" /tmp/compile.log; then
    echo "❌ dgiot_uav_auto_tester.erl 编译失败"
    echo "错误信息:"
    grep -A10 "dgiot_uav_auto_tester.erl" /tmp/compile.log | grep -E "(error|Error|failed|variable)"
    exit 1
elif grep -q "Compiling dgiot_uav_ground_station_mapper.erl failed" /tmp/compile.log; then
    echo "❌ dgiot_uav_ground_station_mapper.erl 编译失败"
    echo "错误信息:"
    grep -A10 "dgiot_uav_ground_station_mapper.erl" /tmp/compile.log | grep -E "(error|Error|failed|syntax)"
    exit 1
else
    echo "✅ 编译成功"
    
    # 检查是否生成了beam文件
    if [ -f "_build/emqx/rel/emqx/lib/dgiot_uav-*/ebin/dgiot_uav_auto_tester.beam" ]; then
        echo "✅ dgiot_uav_auto_tester.beam 已生成"
    fi
    
    if [ -f "_build/emqx/rel/emqx/lib/dgiot_uav-*/ebin/dgiot_uav_ground_station_mapper.beam" ]; then
        echo "✅ dgiot_uav_ground_station_mapper.beam 已生成"
    fi
    
    echo ""
    echo "=== 可以开始测试磁航向工位了！==="
fi
