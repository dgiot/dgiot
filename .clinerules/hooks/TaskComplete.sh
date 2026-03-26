#!/bin/bash
# TaskComplete Hook测试脚本
# 这个Hook会在Cline任务完成时自动执行

echo "========================================"
echo "✅ Cline TaskComplete Hook 触发成功!"
echo "========================================"
echo "完成时间: $(date '+%Y-%m-%d %H:%M:%S %Z')"
echo "任务名称: ${CLINE_TASK_NAME:-未设置}"
echo "任务状态: ${CLINE_TASK_STATUS:-完成}"
echo "执行时长: ${CLINE_TASK_DURATION:-未知}"
echo "========================================"

# 记录到日志文件
LOG_FILE="/tmp/cline_hooks_test.log"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] TaskComplete Hook触发: ${CLINE_TASK_NAME:-unknown} (状态: ${CLINE_TASK_STATUS:-完成})" >> "$LOG_FILE"

# 如果是UDP多播相关任务，特别记录
if [[ "${CLINE_TASK_NAME:-}" == *"UDP"* ]] || [[ "${CLINE_TASK_NAME:-}" == *"多播"* ]] || [[ "${CLINE_TASK_NAME:-}" == *"udp"* ]]; then
    echo "🎯 UDP多播任务完成!"
    UDP_LOG="/tmp/uav_udp_hooks.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] UDP任务完成: ${CLINE_TASK_NAME} (状态: ${CLINE_TASK_STATUS:-完成})" >> "$UDP_LOG"
    
    # 更新项目状态
    echo "📊 更新无人机项目状态..."
    
    # 记录到项目内存
    PROJECT_LOG="/root/clawd/memory/$(date +%Y-%m-%d).md"
    if [ -f "$PROJECT_LOG" ]; then
        echo "" >> "$PROJECT_LOG"
        echo "### $(date '+%H:%M:%S') - Cline Hook触发" >> "$PROJECT_LOG"
        echo "- TaskComplete Hook: UDP多播任务完成 - ${CLINE_TASK_NAME} (状态: ${CLINE_TASK_STATUS:-完成})" >> "$PROJECT_LOG"
    fi
fi

echo "✅ Hook执行完成"
echo "========================================"