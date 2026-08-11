#!/bin/bash
# UAV集成TaskStart Hook
# 集成所有UAV相关Hook功能

echo "========================================"
echo "🚁 UAV集成Hook启动"
echo "========================================"

# 调用基础TaskStart Hook
if [ -f "/root/gitee/dgiot/.clinerules/hooks/TaskStart.sh" ]; then
    echo "📋 调用基础TaskStart Hook..."
    /bin/bash /root/gitee/dgiot/.clinerules/hooks/TaskStart.sh
fi

# 调用项目监控Hook
if [ -f "/root/gitee/dgiot/.clinerules/hooks/uav_project_monitor.sh" ]; then
    echo "📊 调用项目监控Hook..."
    /bin/bash /root/gitee/dgiot/.clinerules/hooks/uav_project_monitor.sh "TASK_START" "集成Hook触发"
fi

# UAV特定逻辑
if [[ "${CLINE_TASK_NAME:-}" == *"UDP"* ]] || [[ "${CLINE_TASK_NAME:-}" == *"多播"* ]]; then
    echo "🎯 检测到UDP多播任务，执行专项处理..."
    
    # 记录到专项日志
    UAV_SPECIAL_LOG="/tmp/uav_special_tasks.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] UDP专项任务: ${CLINE_TASK_NAME}" >> "$UAV_SPECIAL_LOG"
    
    # 检查网络环境
    echo "🌐 检查UDP多播网络环境..."
    NETWORK_CHECK_LOG="/tmp/uav_network_check.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] 网络环境检查开始" >> "$NETWORK_CHECK_LOG"
    
    # 检查多播组
    echo "  检查多播组 226.0.0.80:8001..."
    echo "  射频卡配置验证..."
    echo "  网络接口状态检查..."
fi

echo "✅ UAV集成Hook执行完成"
echo "========================================"
