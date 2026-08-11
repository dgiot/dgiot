#!/bin/bash
# TaskStart Hook测试脚本
# 这个Hook会在Cline任务开始时自动执行

echo "========================================"
echo "🚀 Cline TaskStart Hook 触发成功!"
echo "========================================"
echo "触发时间: $(date '+%Y-%m-%d %H:%M:%S %Z')"
echo "任务名称: ${CLINE_TASK_NAME:-未设置}"
echo "任务ID: ${CLINE_TASK_ID:-未设置}"
echo "用户: ${CLINE_USER:-未设置}"
echo "工作空间: ${CLINE_WORKSPACE:-未设置}"
echo "========================================"

# 记录到日志文件
LOG_FILE="/tmp/cline_hooks_test.log"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] TaskStart Hook触发: ${CLINE_TASK_NAME:-unknown}" >> "$LOG_FILE"

# 如果是UDP多播相关任务，特别记录
if [[ "${CLINE_TASK_NAME:-}" == *"UDP"* ]] || [[ "${CLINE_TASK_NAME:-}" == *"多播"* ]] || [[ "${CLINE_TASK_NAME:-}" == *"udp"* ]]; then
    echo "🎯 检测到UDP多播相关任务!"
    UDP_LOG="/tmp/uav_udp_hooks.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] UDP任务开始: ${CLINE_TASK_NAME}" >> "$UDP_LOG"
    
    # 记录到项目内存
    PROJECT_LOG="/root/clawd/memory/$(date +%Y-%m-%d).md"
    if [ -f "$PROJECT_LOG" ]; then
        echo "" >> "$PROJECT_LOG"
        echo "### $(date '+%H:%M:%S') - Cline Hook触发" >> "$PROJECT_LOG"
        echo "- TaskStart Hook: UDP多播任务开始 - ${CLINE_TASK_NAME}" >> "$PROJECT_LOG"
    fi
fi

echo "✅ Hook执行完成"
echo "========================================"