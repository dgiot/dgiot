#!/bin/bash
# 运行pymodbus测试用例的脚本

echo "=== 运行pymodbus测试用例 ==="
echo "开始时间: $(date)"
echo ""

# 检查Python环境
echo "1. 检查Python环境..."
python3 --version
if [ $? -ne 0 ]; then
    echo "❌ Python3未安装"
    exit 1
fi

# 检查pymodbus
echo "2. 检查pymodbus..."
python3 -c "import pymodbus; print('pymodbus版本:', pymodbus.__version__)"
if [ $? -ne 0 ]; then
    echo "❌ pymodbus未安装，尝试安装..."
    pip install pymodbus==3.6.8
    if [ $? -ne 0 ]; then
        echo "❌ pymodbus安装失败"
        exit 1
    fi
fi

# 检查requests
echo "3. 检查requests..."
python3 -c "import requests; print('requests版本:', requests.__version__)" 2>/dev/null || {
    echo "⚠️  requests未安装，尝试安装..."
    pip install requests
}

# 创建测试目录
TEST_DIR="apps/dgiot_modbus/test/protocol/pymodbus_tests"
if [ ! -d "$TEST_DIR" ]; then
    echo "❌ 测试目录不存在: $TEST_DIR"
    exit 1
fi

# 运行基础测试
echo ""
echo "4. 运行Modbus基础协议测试..."
cd "$(dirname "$0")/../../../../.."  # 回到项目根目录
python3 "$TEST_DIR/test_modbus_basic.py"
BASIC_RESULT=$?

# 运行RTU解析测试
echo ""
echo "5. 运行Modbus RTU数据解析测试..."
python3 "$TEST_DIR/test_modbus_rtu_parsing.py"
RTU_RESULT=$?

# 汇总结果
echo ""
echo "=== 测试结果汇总 ==="
echo "基础协议测试: $([ $BASIC_RESULT -eq 0 ] && echo '✅ 通过' || echo '❌ 失败')"
echo "RTU解析测试: $([ $RTU_RESULT -eq 0 ] && echo '✅ 通过' || echo '❌ 失败')"

if [ $BASIC_RESULT -eq 0 ] && [ $RTU_RESULT -eq 0 ]; then
    echo ""
    echo "🎉 所有测试通过！"
    echo ""
    echo "📋 下一步建议:"
    echo "1. 检查后端日志确认数据解析情况"
    echo "2. 验证数据是否正确存储到TDengine"
    echo "3. 确认API返回的数据格式正确"
    echo "4. 根据测试结果优化代码"
    exit 0
else
    echo ""
    echo "⚠️  部分测试失败"
    echo ""
    echo "🔧 故障排除建议:"
    echo "1. 检查DG-IoT系统是否正常运行"
    echo "2. 检查端口20000是否监听"
    echo "3. 查看后端日志中的错误信息"
    echo "4. 确认Modbus插件已正确加载"
    exit 1
fi
