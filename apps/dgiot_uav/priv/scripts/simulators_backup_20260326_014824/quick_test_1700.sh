#!/bin/bash
# 磁航向工位快速测试示例

set -e

echo "================================"
echo "磁航向工位快速测试"
echo "================================"

cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 步骤1: 清理环境
echo ""
echo "[步骤1] 清理环境..."
python3 station_test_manager.py clean 1700

# 步骤2: 启动测试（60秒）
echo ""
echo "[步骤2] 启动测试（60秒）..."
python3 station_test_manager.py start 1700 60

# 步骤3: 等待5秒
echo ""
echo "[步骤3] 等待5秒..."
sleep 5

# 步骤4: 查看状态
echo ""
echo "[步骤4] 查看状态..."
python3 station_test_manager.py status 1700

# 步骤5: 验证IP映射
echo ""
echo "[步骤5] 验证IP映射..."
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'

# 步骤6: 等待测试完成
echo ""
echo "[步骤6] 等待测试完成（剩余55秒）..."
sleep 55

# 步骤7: 查看最终状态
echo ""
echo "[步骤7] 查看最终状态..."
python3 station_test_manager.py status 1700

# 步骤8: 查看MES接收的数据
echo ""
echo "[步骤8] 查看MES接收的数据..."
if [ -f "/tmp/station_tests/mes_1700_data.jsonl" ]; then
    echo "MES接收数据记录数: $(wc -l < /tmp/station_tests/mes_1700_data.jsonl)"
    tail -3 /tmp/station_tests/mes_1700_data.jsonl
else
    echo "未找到MES数据文件"
fi

# 步骤9: 停止测试
echo ""
echo "[步骤9] 停止测试..."
python3 station_test_manager.py stop 1700

# 步骤10: 验证清理结果
echo ""
echo "[步骤10] 验证清理结果..."
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'

echo ""
echo "================================"
echo "测试完成"
echo "================================"
echo ""
echo "查看详细日志:"
echo "  tail -f /tmp/station_tests/device_1700.log"
echo "  tail -f /tmp/station_tests/mes_1700.log"
