#!/bin/bash
# UAV项目监控Hook
# 专门用于无人机自动化产线项目的监控和状态更新

echo "========================================"
echo "🚁 UAV项目监控Hook启动"
echo "========================================"
echo "监控时间: $(date '+%Y-%m-%d %H:%M:%S %Z')"
echo "触发事件: $1"
echo "事件数据: $2"
echo "========================================"

# 项目状态文件
PROJECT_STATUS_FILE="/root/clawd/project_status.csv"
MEMORY_FILE="/root/clawd/memory/$(date +%Y-%m-%d).md"

# 确保状态文件存在
if [ ! -f "$PROJECT_STATUS_FILE" ]; then
    echo "时间戳,事件类型,任务名称,详细信息" > "$PROJECT_STATUS_FILE"
fi

# 记录到状态文件
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
EVENT_TYPE="$1"
TASK_NAME="${CLINE_TASK_NAME:-未设置}"
EVENT_DATA="$2"

echo "$TIMESTAMP,$EVENT_TYPE,$TASK_NAME,$EVENT_DATA" >> "$PROJECT_STATUS_FILE"

# 根据事件类型执行不同操作
case "$EVENT_TYPE" in
    "TASK_START")
        echo "📋 任务开始: $TASK_NAME"
        # 记录到内存文件
        if [ -f "$MEMORY_FILE" ]; then
            echo "" >> "$MEMORY_FILE"
            echo "### $(date '+%H:%M:%S') - Cline任务开始" >> "$MEMORY_FILE"
            echo "- 任务名称: $TASK_NAME" >> "$MEMORY_FILE"
            echo "- 事件: $EVENT_DATA" >> "$MEMORY_FILE"
        fi
        ;;
        
    "TASK_COMPLETE")
        echo "✅ 任务完成: $TASK_NAME"
        # 更新项目进度
        if [[ "$TASK_NAME" == *"UDP"* ]] || [[ "$TASK_NAME" == *"多播"* ]]; then
            echo "🎯 UDP多播任务完成，更新技术攻关状态..."
            UDP_PROGRESS_FILE="/root/clawd/udp_multicast_progress.md"
            echo "## $(date '+%Y-%m-%d %H:%M:%S')" > "$UDP_PROGRESS_FILE"
            echo "- 任务完成: $TASK_NAME" >> "$UDP_PROGRESS_FILE"
            echo "- 状态: $EVENT_DATA" >> "$UDP_PROGRESS_FILE"
            echo "- 更新时间: $(date)" >> "$UDP_PROGRESS_FILE"
        fi
        ;;
        
    "TOOL_EXEC")
        echo "🔧 工具执行: $EVENT_DATA"
        # 记录工具使用统计
        TOOL_STATS="/tmp/uav_tool_stats.log"
        echo "$(date '+%Y-%m-%d %H:%M:%S'),$EVENT_DATA" >> "$TOOL_STATS"
        ;;
        
    "NETWORK_TEST")
        echo "🌐 网络测试: $EVENT_DATA"
        # 记录网络测试结果
        NETWORK_LOG="/root/clawd/network_tests.log"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] $EVENT_DATA" >> "$NETWORK_LOG"
        ;;
        
    "PRODUCTION_STATUS")
        echo "🏭 产线状态更新: $EVENT_DATA"
        # 更新产线状态
        PRODUCTION_STATUS="/root/clawd/production_status.md"
        echo "## 产线状态 - $(date '+%Y-%m-%d %H:%M:%S')" > "$PRODUCTION_STATUS"
        echo "$EVENT_DATA" >> "$PRODUCTION_STATUS"
        ;;
        
    *)
        echo "📝 通用事件: $EVENT_TYPE - $EVENT_DATA"
        ;;
esac

# 生成状态摘要
echo "========================================"
echo "📊 项目状态摘要"
echo "========================================"
echo "状态文件: $PROJECT_STATUS_FILE"
echo "记录数量: $(wc -l < "$PROJECT_STATUS_FILE" 2>/dev/null || echo "0")"
echo "最后记录: $(tail -1 "$PROJECT_STATUS_FILE" 2>/dev/null || echo "无记录")"
echo "========================================"

echo "✅ UAV项目监控Hook执行完成"
echo "========================================"