#!/bin/bash
# 最终验证脚本 - 检查所有修复是否正确

echo "=== 编译错误修复验证 ==="
echo ""

echo "1. 检查 dgiot_uav_auto_tester.erl 修复:"
echo ""

# 检查第142行
if grep -q "load_test_items(_StationId) ->" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第142行: StationId -> _StationId"
else
    echo "❌ 第142行: 未修复"
    exit 1
fi

# 检查第185行
if grep -q "PassedCount = length(\[ #{status := passed} <- Results\])," /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第185行: 列表推导式已修复（删除不需要的变量）"
else
    echo "❌ 第185行: 未修复"
    exit 1
fi

# 检查第186行
if grep -q "FailedCount = length(\[ #{status := failed} <- Results\])," /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第186行: 列表推导式已修复（删除不需要的变量）"
else
    echo "❌ 第186行: 未修复"
    exit 1
fi

# 检查第204行
if grep -q "_ItemId = maps:get" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第204行: ItemId -> _ItemId"
else
    echo "❌ 第204行: 未修复"
    exit 1
fi

# 检查第224行
if grep -q "test_check_power(_DeviceId) ->" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第224行: DeviceId -> _DeviceId"
else
    echo "❌ 第224行: 未修复"
    exit 1
fi

# 检查第230行
if grep -q "test_magnetic_calibration(_DeviceId, _StationId) ->" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl; then
    echo "✅ 第230行: DeviceId, StationId -> _DeviceId, _StationId"
else
    echo "❌ 第230行: 未修复"
    exit 1
fi

echo ""
echo "2. 检查 dgiot_uav_ground_station_mapper.erl 修复:"
echo ""

# 检查第236行（不应该有重复的update_test_step）
if sed -n '236p' /root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_ground_station_mapper.erl | grep -q "update_test_step.*update_test_step"; then
    echo "❌ 第236行: 仍有重复的update_test_step调用"
    exit 1
else
    echo "✅ 第236行: 重复的update_test_step调用已删除"
fi

echo ""
echo "=== 所有修复验证通过！==="
echo ""
echo "现在可以执行: make emqx"
echo "然后执行: make run"
echo "最后测试: python3 station_1700_magnetic_scenario.py"
