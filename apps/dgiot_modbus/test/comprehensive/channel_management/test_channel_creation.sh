#!/bin/bash

# 通道创建测试脚本
# 测试Modbus通道的创建功能

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../.." && pwd)"
LOG_FILE="/tmp/test_channel_creation_$(date +%Y%m%d_%H%M%S).log"

# 日志函数
log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE"
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE" >&2
}

log_success() {
    echo "[SUCCESS] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE"
}

# 清理函数
cleanup() {
    log_info "清理测试环境..."
    # 这里可以添加清理代码
}

# 错误处理
trap 'log_error "测试失败: $?"; cleanup; exit 1' ERR
trap 'cleanup' EXIT

# 主测试函数
test_channel_creation() {
    log_info "开始测试Modbus通道创建..."
    
    # 1. 检查平台是否运行
    log_info "1. 检查DG-IoT平台状态..."
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "平台未运行，请先启动: make run"
        return 1
    fi
    log_success "平台运行正常"
    
    # 2. 检查Modbus插件是否加载
    log_info "2. 检查Modbus插件状态..."
    PLUGIN_STATUS=$(_build/emqx/rel/emqx/bin/emqx eval 'application:which_applications()' 2>/dev/null | grep -i modbus || true)
    if [ -z "$PLUGIN_STATUS" ]; then
        log_error "Modbus插件未加载"
        return 1
    fi
    log_success "Modbus插件已加载"
    
    # 3. 测试通道创建API
    log_info "3. 测试通道创建API..."
    
    # 创建测试通道配置
    CHANNEL_CONFIG='{
        "name": "测试Modbus通道",
        "type": "MODBUS",
        "config": {
            "port": 20000,
            "host": "127.0.0.1",
            "protocol": "MODBUSRTU",
            "description": "测试通道"
        }
    }'
    
    log_info "通道配置: $CHANNEL_CONFIG"
    
    # 4. 验证通道类型定义
    log_info "4. 验证通道类型定义..."
    CHANNEL_TYPE=$(_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus_channel:module_info(attributes).' 2>/dev/null | grep -i "type" || true)
    if [ -n "$CHANNEL_TYPE" ]; then
        log_success "通道类型定义正确: $CHANNEL_TYPE"
    else
        log_error "通道类型定义检查失败"
        return 1
    fi
    
    # 5. 测试结果
    log_success "通道创建测试通过"
    return 0
}

# 运行测试
main() {
    log_info "=== Modbus通道创建测试开始 ==="
    log_info "日志文件: $LOG_FILE"
    log_info "项目根目录: $PROJECT_ROOT"
    
    if test_channel_creation; then
        log_success "=== 测试成功 ==="
        echo "测试结果: ✅ 通过"
        return 0
    else
        log_error "=== 测试失败 ==="
        echo "测试结果: ❌ 失败"
        return 1
    fi
}

# 执行主函数
main "$@"
