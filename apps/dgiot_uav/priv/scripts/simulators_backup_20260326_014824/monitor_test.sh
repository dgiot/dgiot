#!/bin/bash
# 综合监测脚本：Erlang后台日志 + Python测试脚本日志

echo "=== 开始监测磁航向工位测试 ==="
echo ""

# 日志文件
ERLANG_LOG="/root/gitee/dgiot/_build/emqx/rel/emqx/log/console.log"
PYTHON_LOG="/tmp/station_1700_test.log"
MAKE_LOG="/tmp/make_run.log"

# 测试结果追踪
PASSED=0
FAILED=0
CURRENT_STEP=""

echo "1. 创建日志监测窗口"
tail -f $ERLANG_LOG 2>/dev/null &
ERLANG_PID=$!

echo "2. 启动Python测试日志监测"
tail -f $PYTHON_LOG 2>/dev/null &
PYTHON_PID=$!

# 清理函数
cleanup() {
    echo ""
    echo "=== 测试结束 ==="
    kill $ERLANG_PID 2>/dev/null
    kill $PYTHON_PID 2>/dev/null
    
    echo ""
    echo "=== 测试结果汇总 ==="
    echo "通过的步骤: $PASSED"
    echo "失败的步骤: $FAILED"
    echo ""
    echo "=== Erlang日志最后20行 ==="
    tail -20 $ERLANG_LOG
    echo ""
    echo "=== Python测试日志最后20行 ==="
    tail -20 $PYTHON_LOG
}

# 设置退出时清理
trap cleanup EXIT INT TERM

# 实时监控
echo "3. 实时监控中..."
echo ""
echo "按 Ctrl+C 停止监测"
echo ""

while true; do
    # 监控Erlang日志
    if tail -5 $ERLANG_LOG | grep -q "AUTO_TEST"; then
        echo "[Erlang] 检测到自动化测试日志..."
        
        # 提取当前步骤
        CURRENT_STEP=$(tail -10 $ERLANG_LOG | grep "执行步骤" | tail -1 | grep -oP "步骤[0-9]:")
        echo "[Erlang] 当前步骤: $CURRENT_STEP"
        
        # 检查通过和失败
        PASSED=$(tail -10 $ERLANG_LOG | grep -oP "✅.*通过" | wc -l)
        FAILED=$(tail -10 $ERLANG_LOG | grep -oP "❌.*失败" | wc -l)
        
        echo "[Erlang] 通过: $PASSED, 失败: $FAILED"
    fi
    
    # 监控Python日志
    if tail -5 $PYTHON_LOG | grep -q "步骤\|测试\|完成"; then
        echo "[Python] 检测到测试执行..."
        
        # 提取当前步骤
        CURRENT_STEP_PY=$(tail -5 $PYTHON_LOG | grep "执行步骤" | tail -1 | grep -oP "步骤[0-9]:")
        echo "[Python] 当前步骤: $CURRENT_STEP_PY"
    fi
    
    sleep 2
done

cleanup
