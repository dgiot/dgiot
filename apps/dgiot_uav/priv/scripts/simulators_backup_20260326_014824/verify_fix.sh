#!/bin/bash
# 验证编译错误修复

echo "=== 检查 dgiot_uav_auto_tester.erl 修复情况 ==="
echo ""

echo "1. 检查第142行 (StationId -> _StationId):"
grep -n "load_test_items(_StationId)" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl

echo ""
echo "2. 检查第185-186行 (R -> _R):"
grep -n "length(\[_R\|#{" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl | head -2

echo ""
echo "3. 检查第204行 (ItemId -> _ItemId):"
grep -n "_ItemId = maps:get" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl

echo ""
echo "4. 检查第224行 (DeviceId -> _DeviceId):"
grep -n "test_check_power(_DeviceId)" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl

echo ""
echo "5. 检查第230行 (DeviceId -> _DeviceId, StationId -> _StationId):"
grep -n "test_magnetic_calibration(_DeviceId, _StationId)" /root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl

echo ""
echo "=== 检查 dgiot_uav_ground_station_mapper.erl 修复情况 ==="
echo ""
echo "1. 检查第236行语法错误:"
sed -n '236p' /root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_ground_station_mapper.erl

echo ""
echo "2. 检查第270行:"
sed -n '270p' /root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_ground_station_mapper.erl

echo ""
echo "=== 验证完成 ==="
