#!/bin/bash
# PreToolUse Hook - 在工具使用前触发
# 这个Hook会在Cline使用任何工具之前执行

TOOL_NAME="$1"
TOOL_ARGS="$2"

echo "========================================"
echo "🔧 Cline PreToolUse Hook 触发"
echo "========================================"
echo "触发时间: $(date '+%Y-%m-%d %H:%M:%S')"
echo "工具名称: ${TOOL_NAME:-未设置}"
echo "工具参数: ${TOOL_ARGS:-未设置}"
echo "当前任务: ${CLINE_TASK_NAME:-未设置}"
echo "========================================"

# 记录到工具使用日志
TOOL_LOG="/tmp/cline_tools_usage.log"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] 工具使用: ${TOOL_NAME} - ${TOOL_ARGS:0:100}..." >> "$TOOL_LOG"

# 特别监控UDP相关工具
if [[ "${TOOL_NAME}" == "exec" ]] && [[ "${TOOL_ARGS}" == *"udp"* || "${TOOL_ARGS}" == *"UDP"* || "${TOOL_ARGS}" == *"226.0.0.80"* || "${TOOL_ARGS}" == *"multicast"* ]]; then
    echo "🎯 检测到UDP多播相关工具调用!"
    UDP_TOOL_LOG="/tmp/uav_udp_tools.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] UDP工具调用: ${TOOL_NAME} - ${TOOL_ARGS}" >> "$UDP_TOOL_LOG"
    
    # 记录到项目状态
    echo "📡 记录UDP网络操作..."
    
    # 如果是网络测试命令，特别记录
    if [[ "${TOOL_ARGS}" == *"ping"* ]] || [[ "${TOOL_ARGS}" == *"nmap"* ]] || [[ "${TOOL_ARGS}" == *"netstat"* ]]; then
        echo "🌐 网络诊断命令执行中..."
        NETWORK_LOG="/tmp/uav_network_diagnostics.log"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] 网络诊断: ${TOOL_ARGS}" >> "$NETWORK_LOG"
    fi
fi

# 如果是文件操作，记录到文件变更日志
if [[ "${TOOL_NAME}" == "read" ]] || [[ "${TOOL_NAME}" == "write" ]] || [[ "${TOOL_NAME}" == "edit" ]]; then
    FILE_LOG="/tmp/cline_file_operations.log"
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] 文件操作: ${TOOL_NAME} - ${TOOL_ARGS:0:50}..." >> "$FILE_LOG"
    
    # 如果是UDP相关文件，特别记录
    if [[ "${TOOL_ARGS}" == *"dgiot_uav"* ]] || [[ "${TOOL_ARGS}" == *"udp_worker"* ]]; then
        echo "📄 UDP相关文件操作检测到!"
        UAV_FILE_LOG="/tmp/uav_code_changes.log"
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] UAV代码变更: ${TOOL_NAME} - ${TOOL_ARGS}" >> "$UAV_FILE_LOG"
    fi
fi

echo "✅ PreToolUse Hook执行完成"
echo "========================================"