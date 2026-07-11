#!/bin/bash
# dgiot核心模块调试脚本
# 位置：scripts/debug_plugin_template.sh
# 用法：复制到插件目录并修改PLUGIN_NAME和MODULES

PLUGIN_NAME="dgiot"  # 修改为插件名称
PLUGIN_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$PLUGIN_DIR/../.." && pwd)"

# 插件模块列表（从rebar.config或自动检测）
# 修改为插件的实际模块列表
MODULES=(
    "dgiot"
    "dgiot_app"
    "dgiot_sup"
    "dgiot_plugin"
    "dgiot_channelx"
    "dgiot_data"
    "dgiot_hook"
    "dgiot_utils"
    "dgiot_logger"
    "dgiot_cache"
    "dgiot_mnesia"
    "dgiot_rule_engine"
    "dgiot_httpc"
    "dgiot_mqtt"
    "dgiot_tcp_server"
    "dgiot_udp_server"
)

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数
log_info() { echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*" >&2; }

# 检查系统状态
check_system() {
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "DG-IoT平台未运行，请先启动：make run"
        return 1
    fi
    log_info "系统运行正常"
    return 0
}

# 开启调试模式
enable_debug_mode() {
    log_info "开启 $PLUGIN_NAME 调试模式..."
    
    for MODULE in "${MODULES[@]}"; do
        log_info "  设置 $MODULE 为debug级别..."
        "$PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx" eval "logger:set_module_level($MODULE, debug)." 2>/dev/null || true
    done
    
    log_success "调试模式已开启"
}

# 恢复商用模式
enable_production_mode() {
    log_info "恢复 $PLUGIN_NAME 商用模式..."
    
    for MODULE in "${MODULES[@]}"; do
        log_info "  设置 $MODULE 为error级别..."
        "$PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx" eval "logger:set_module_level($MODULE, error)." 2>/dev/null || true
    done
    
    log_success "商用模式已恢复"
}

# 查看日志级别
show_log_levels() {
    log_info "查看 $PLUGIN_NAME 日志级别..."
    
    for MODULE in "${MODULES[@]}"; do
        echo -n "  $MODULE: "
        RESULT=$("$PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx" eval "logger:get_module_level($MODULE)." 2>/dev/null)
        if [ -z "$RESULT" ]; then
            echo "使用系统默认"
        elif echo "$RESULT" | grep -q "escript: exception error"; then
            echo "查询失败"
        else
            LEVEL=$(echo "$RESULT" | grep -o "debug\|info\|warning\|error" | head -1)
            if [ -n "$LEVEL" ]; then
                echo "$LEVEL"
            else
                echo "未设置"
            fi
        fi
    done
}

# 监控日志
monitor_logs() {
    log_info "开始监控 $PLUGIN_NAME 日志（Ctrl+C停止）..."
    echo "=== $PLUGIN_NAME 插件日志监控 ==="
    tail -f "$PROJECT_ROOT/_build/emqx/rel/emqx/log/emqx.log.1" | grep -E "($(echo "${MODULES[@]}" | tr ' ' '|')|DEBUG|INFO|WARNING|ERROR)"
}

# 显示帮助
show_help() {
    echo "用法: $0 [命令]"
    echo ""
    echo "命令:"
    echo "  debug      开启调试模式（设置所有模块为debug级别）"
    echo "  production 恢复商用模式（设置所有模块为error级别）"
    echo "  levels     查看当前日志级别"
    echo "  monitor    监控插件日志"
    echo "  help       显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 debug      # 开启调试模式"
    echo "  $0 levels     # 查看日志级别"
    echo "  $0 monitor    # 监控日志"
    echo ""
    echo "插件信息:"
    echo "  名称: $PLUGIN_NAME"
    echo "  模块数: ${#MODULES[@]}"
    echo "  目录: $PLUGIN_DIR"
}

# 主函数
main() {
    if [ $# -eq 0 ]; then
        show_help
        exit 0
    fi
    
    case $1 in
        debug)
            check_system && enable_debug_mode
            ;;
        production)
            check_system && enable_production_mode
            ;;
        levels)
            check_system && show_log_levels
            ;;
        monitor)
            check_system && monitor_logs
            ;;
        help|--help|-h)
            show_help
            ;;
        *)
            log_error "未知命令: $1"
            show_help
            exit 1
            ;;
    esac
}

# 执行主函数
main "$@"