#!/bin/bash
# 完整修复dgiot_uav_auto_tester.erl的所有编译错误

file="/root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl"

echo "=== 开始修复 dgiot_uav_auto_tester.erl 编译错误 ==="

# 修复1: 第20行 - 导出列表语法
echo "1. 修复第20行：导出列表语法"
sed -i '20s/:]).*$/])./' "$file"

# 修复2: 第142行 - 未使用变量
echo "2. 修复第142行：StationId -> _StationId"
sed -i '142s/load_test_items(StationId) ->$/load_test_items(_StationId) ->/' "$file"

# 修复3: 第185-186行 - 列表推导式空格
echo "3. 修复第185-186行：列表推导式空格"
sed -i '185s/\[ #/\[ #/; 186s/\[ #/\[ #/' "$file"

# 修复4: 第204行 - 未使用变量
echo "4. 修复第204行：ItemId -> _ItemId"
sed -i '204s/    ItemId = maps:get(<<"id">>, TestItem),$/    _ItemId = maps:get(<<"id">>, TestItem),/' "$file"

# 修复5: 第224行 - 未使用变量
echo "5. 修复第224行：DeviceId -> _DeviceId"
sed -i '224s/test_check_power(DeviceId) ->$/test_check_power(_DeviceId) ->/' "$file"

# 修复6: 第230行 - 未使用变量×2
echo "6. 修复第230行：DeviceId, StationId -> _DeviceId, _StationId"
sed -i '230s/test_magnetic_calibration(DeviceId, StationId) ->$/test_magnetic_calibration(_DeviceId, _StationId) ->/' "$file"

echo ""
echo "=== 验证修复 ==="

# 验证1: 第20行
if sed -n '20p' "$file" | grep -q '^\]);$'; then
    echo "✅ 第20行: 导出列表已修复"
else
    echo "❌ 第20行: 未修复"
    sed -n '20p' "$file"
fi

# 验证2: 第142行
if grep -q "load_test_items(_StationId) ->" "$file"; then
    echo "✅ 第142行: StationId -> _StationId"
else
    echo "❌ 第142行: 未修复"
fi

# 验证3: 第185-186行
if grep -q "length([ #{status := passed} <- Results])," "$file"; then
    echo "✅ 第185行: 列表推导式已修复"
else
    echo "❌ 第185行: 未修复"
    sed -n '185,186p' "$file"
fi

# 验证4: 第204行
if grep -q "_ItemId = maps:get" "$file"; then
    echo "✅ 第204行: ItemId -> _ItemId"
else
    echo "❌ 第204行: 未修复"
fi

# 验证5: 第224行
if grep -q "test_check_power(_DeviceId) ->" "$file"; then
    echo "✅ 第224行: DeviceId -> _DeviceId"
else
    echo "❌ 第224行: 未修复"
fi

# 验证6: 第230行
if grep -q "test_magnetic_calibration(_DeviceId, _StationId) ->" "$file"; then
    echo "✅ 第230行: DeviceId, StationId -> _DeviceId, _StationId"
else
    echo "❌ 第230行: 未修复"
fi

echo ""
echo "=== 所有修复完成！==="
echo "现在可以执行: make emqx"
