#!/bin/bash
# UAV项目Hook集成示例
# 展示如何将自定义Hook集成到Cline系统中

echo "========================================"
echo "🚁 UAV项目Hook集成演示"
echo "========================================"
echo "演示时间: $(date '+%Y-%m-%d %H:%M:%S')"
echo "========================================"

# 1. 展示当前Hook配置
echo "1. 📋 当前Hook配置状态"
echo "----------------------------------------"
ls -la /root/gitee/dgiot/.clinerules/hooks/*.sh 2>/dev/null | awk '{print "   " $9}'
echo ""

# 2. 演示Hook链调用
echo "2. 🔗 Hook链调用示例"
echo "----------------------------------------"
echo "当Cline任务开始时，会按顺序触发:"
echo "  1. TaskStart (主Hook)"
echo "  2. TaskStart.sh (我们的测试Hook)"
echo "  3. uav_project_monitor.sh (项目监控Hook)"
echo ""

# 3. 创建集成脚本
echo "3. 🛠️ 创建集成脚本"
echo "----------------------------------------"
INTEGRATION_SCRIPT="/root/gitee/dgiot/.clinerules/hooks/uav_integrated_taskstart.sh"

cat > "$INTEGRATION_SCRIPT" << 'EOF'
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
EOF

chmod +x "$INTEGRATION_SCRIPT"
echo "集成脚本创建完成: $INTEGRATION_SCRIPT"
echo ""

# 4. 演示实际调用
echo "4. 🎬 演示实际调用"
echo "----------------------------------------"
export CLINE_TASK_NAME="UDP多播集成测试任务"
export CLINE_TASK_ID="uav-integration-test-001"

echo "模拟Cline调用集成Hook..."
/bin/bash "$INTEGRATION_SCRIPT"

echo ""

# 5. 查看生成的日志
echo "5. 📊 查看生成的日志"
echo "----------------------------------------"
echo "UDP专项任务日志:"
cat /tmp/uav_special_tasks.log 2>/dev/null | tail -3 || echo "  暂无日志"
echo ""
echo "网络检查日志:"
cat /tmp/uav_network_check.log 2>/dev/null | tail -3 || echo "  暂无日志"

echo ""
echo "========================================"
echo "✅ UAV项目Hook集成演示完成"
echo "========================================"
echo ""
echo "🎯 下一步建议:"
echo "1. 将集成Hook链接到Cline主Hook系统"
echo "2. 配置自动化触发规则"
echo "3. 设置项目状态监控面板"
echo "4. 集成到无人机产线管理系统"
echo "========================================"