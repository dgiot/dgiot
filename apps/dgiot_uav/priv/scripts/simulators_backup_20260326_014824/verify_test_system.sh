#!/bin/bash
# 快速验证工位测试系统

echo "================================"
echo "工位测试系统验证"
echo "================================"

cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 1. 检查Python环境
echo ""
echo "[1] 检查Python环境..."
python3 --version

# 2. 检查脚本文件
echo ""
echo "[2] 检查脚本文件..."
if [ -f "station_test_manager.py" ]; then
    echo "  OK station_test_manager.py"
else
    echo "  ERROR 缺少 station_test_manager.py"
fi

# 3. 检查DG-IoT服务
echo ""
echo "[3] 检查DG-IoT服务..."
if netstat -tlnp 2>/dev/null | grep -q ":20000"; then
    echo "  OK DG-IoT服务运行中 (端口20000)"
else
    echo "  ERROR DG-IoT服务未运行"
fi

# 4. 检查PLC模拟器
echo ""
echo "[4] 检查PLC模拟器..."
if [ -f "plc_simulator.py" ]; then
    echo "  OK plc_simulator.py 存在"
else
    echo "  WARN plc_simulator.py 不存在"
fi

# 5. 检查治具模拟器
echo ""
echo "[5] 检查治具模拟器..."
if [ -f "fixture_simulator.py" ]; then
    echo "  OK fixture_simulator.py 存在"
else
    echo "  WARN fixture_simulator.py 不存在"
fi

# 6. 测试命令帮助
echo ""
echo "[6] 测试管理命令帮助..."
python3 station_test_manager.py

echo ""
echo "================================"
echo "验证完成"
echo "================================"
echo ""
echo "快速开始:"
echo "  python3 station_test_manager.py clean 1700"
echo "  python3 station_test_manager.py start 1700"
echo "  python3 station_test_manager.py status 1700"
echo "  python3 station_test_manager.py stop 1700"
