#!/bin/bash
# log_utils.sh - DGIOT标准日志工具

# 导入配置系统
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="$(dirname "$SCRIPT_DIR")/config"
source "$CONFIG_DIR/config.sh" 2>/dev/null || {
    echo -e "\033[0;31m错误: 无法加载配置系统\033[0m"
    exit 1
}

# 初始化配置
init_config

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# 日志级别
LOG_LEVEL_DEBUG=0
LOG_LEVEL_INFO=1
LOG_LEVEL_WARN=2
LOG_LEVEL_ERROR=3
LOG_LEVEL_CRITICAL=4

# 默认日志级别
DEFAULT_LOG_LEVEL_NAME=$(get_config "logging.default_level" "info")

# 将日志级别名称转换为数字
case "$DEFAULT_LOG_LEVEL_NAME" in
    "debug"|"DEBUG")
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_DEBUG
        ;;
    "info"|"INFO")
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_INFO
        ;;
    "warn"|"WARN"|"warning"|"WARNING")
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_WARN
        ;;
    "error"|"ERROR")
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_ERROR
        ;;
    "critical"|"CRITICAL")
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_CRITICAL
        ;;
    *)
        DEFAULT_LOG_LEVEL=$LOG_LEVEL_INFO
        ;;
esac

# 当前日志级别
CURRENT_LOG_LEVEL=${DGIOT_LOG_LEVEL:-$DEFAULT_LOG_LEVEL}

# 从配置系统获取路径
LOG_DIR=$(get_config "dgiot.log_dir" "/root/gitee/dgiot/_build/emqx/rel/emqx/log")
LOG_FILE="$LOG_DIR/$(get_config "logging.log_file" "dgiot_compile_debug.log")"
EMQX_LOG_FILE="$LOG_DIR/emqx.log"  # 主EMQX日志文件
EMQX_LOG1_FILE="$LOG_DIR/emqx.log.1"  # 轮转的EMQX日志文件
MAX_LOG_SIZE=$(( $(get_config "logging.max_size_mb" 10) * 1024 * 1024 )) # 从配置获取大小

# 初始化日志系统
init_log_system() {
    # 创建日志目录
    mkdir -p "$LOG_DIR" 2>/dev/null
    
    # 检查日志文件大小
    if [[ -f "$LOG_FILE" ]] && [[ $(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0) -gt $MAX_LOG_SIZE ]]; then
        mv "$LOG_FILE" "${LOG_FILE}.$(date +%Y%m%d_%H%M%S)"
    fi
    
    # 创建日志文件
    touch "$LOG_FILE" 2>/dev/null
}

# 设置日志级别
set_log_level() {
    local level="$1"
    
    case "$level" in
        "debug"|"DEBUG")
            CURRENT_LOG_LEVEL=$LOG_LEVEL_DEBUG
            ;;
        "info"|"INFO")
            CURRENT_LOG_LEVEL=$LOG_LEVEL_INFO
            ;;
        "warn"|"WARN"|"warning"|"WARNING")
            CURRENT_LOG_LEVEL=$LOG_LEVEL_WARN
            ;;
        "error"|"ERROR")
            CURRENT_LOG_LEVEL=$LOG_LEVEL_ERROR
            ;;
        "critical"|"CRITICAL")
            CURRENT_LOG_LEVEL=$LOG_LEVEL_CRITICAL
            ;;
        *)
            echo "无效的日志级别: $level"
            return 1
            ;;
    esac
    
    export DGIOT_LOG_LEVEL=$CURRENT_LOG_LEVEL
    log_info "设置日志级别为: $level"
}

# 标准日志函数
log_debug() {
    [[ $CURRENT_LOG_LEVEL -le $LOG_LEVEL_DEBUG ]] && _log "DEBUG" "$BLUE" "$@"
}

log_info() {
    [[ $CURRENT_LOG_LEVEL -le $LOG_LEVEL_INFO ]] && _log "INFO" "$GREEN" "$@"
}

log_warn() {
    [[ $CURRENT_LOG_LEVEL -le $LOG_LEVEL_WARN ]] && _log "WARN" "$YELLOW" "$@"
}

log_error() {
    [[ $CURRENT_LOG_LEVEL -le $LOG_LEVEL_ERROR ]] && _log "ERROR" "$RED" "$@"
}

log_critical() {
    [[ $CURRENT_LOG_LEVEL -le $LOG_LEVEL_CRITICAL ]] && _log "CRITICAL" "$RED" "$@"
}

# 内部日志函数
_log() {
    local level="$1"
    local color="$2"
    local message="$3"
    shift 3
    
    # 格式化消息
    if [[ $# -gt 0 ]]; then
        # 使用printf格式化
        message=$(printf "$message" "$@")
    fi
    
    # 时间戳
    local timestamp=$(date "+%Y-%m-%d %H:%M:%S")
    
    # 获取调用者信息
    local caller_info=""
    if [[ "$level" == "DEBUG" ]]; then
        local caller_file="${BASH_SOURCE[2]##*/}"
        local caller_line="${BASH_LINENO[1]}"
        local caller_func="${FUNCNAME[2]}"
        caller_info="[$caller_file:$caller_line:$caller_func]"
    fi
    
    # 输出到控制台
    echo -e "${color}[$timestamp] [$level]${caller_info} $message${NC}"
    
    # 输出到文件
    echo "[$timestamp] [$level]${caller_info} $message" >> "$LOG_FILE" 2>/dev/null
}

# 通道日志函数（模拟dgiot_bridge:send_log）
channel_log() {
    local channel_id="$1"
    local product_id="$2"
    local dev_addr="$3"
    local format="$4"
    shift 4
    
    # 构建日志消息
    local message=""
    if [[ $# -gt 0 ]]; then
        message=$(printf "$format" "$@")
    else
        message="$format"
    fi
    
    # 构建主题
    local topic=""
    if [[ -n "$dev_addr" ]] && [[ "$dev_addr" != "undefined" ]]; then
        topic="\$dg/user/channel/$channel_id/$product_id/$dev_addr"
    elif [[ -n "$product_id" ]] && [[ "$product_id" != "undefined" ]]; then
        topic="\$dg/user/channel/$channel_id/$product_id"
    else
        topic="\$dg/user/channel/$channel_id/channelid"
    fi
    
    # 输出通道日志
    log_info "[CHANNEL] Topic: $topic, Message: $message"
    
    # 这里可以添加实际的MQTT发布逻辑
    # dgiot_mqtt:publish($channel_id, $topic, $message)
}

# 中文处理日志函数
chinese_log() {
    local level="$1"
    local chinese_message="$2"
    local format="$3"
    shift 3
    
    # 处理中文消息
    local encoded_message=$(echo "$chinese_message" | iconv -f UTF-8 -t UTF-8//IGNORE 2>/dev/null || echo "$chinese_message")
    
    # 格式化消息
    local full_message=""
    if [[ $# -gt 0 ]]; then
        full_message=$(printf "$format" "$@" "$encoded_message")
    else
        full_message="$encoded_message"
    fi
    
    # 根据级别记录日志
    case "$level" in
        "debug"|"DEBUG")
            log_debug "$full_message"
            ;;
        "info"|"INFO")
            log_info "$full_message"
            ;;
        "warn"|"WARN")
            log_warn "$full_message"
            ;;
        "error"|"ERROR")
            log_error "$full_message"
            ;;
        "critical"|"CRITICAL")
            log_critical "$full_message"
            ;;
        *)
            log_info "$full_message"
            ;;
    esac
}

# 标准Erlang风格日志函数
erlang_style_log() {
    local level="$1"
    local file="$2"
    local line="$3"
    local format="$4"
    shift 4
    
    # 构建Erlang风格的消息
    local message=$(printf "~s ~p $format" "$file" "$line" "$@")
    
    # 记录日志
    case "$level" in
        "debug")
            log_debug "$message"
            ;;
        "info")
            log_info "$message"
            ;;
        "warn"|"warning")
            log_warn "$message"
            ;;
        "error")
            log_error "$message"
            ;;
        *)
            log_info "$message"
            ;;
    esac
}

# 动态设置插件/模块日志级别
set_plugin_log_level() {
    local type="$1"  # system, app, module
    local name="$2"
    local level="$3"
    
    case "$type" in
        "system")
            log_info "设置系统日志级别: $name -> $level"
            # emqx_logger:set_log_level(Level)
            ;;
        "app")
            log_info "设置应用日志级别: $name -> $level"
            # logger:set_application_level(Name, Level)
            ;;
        "module")
            log_info "设置模块日志级别: $name -> $level"
            # logger:set_module_level(Name, Level)
            ;;
        *)
            log_error "无效的日志类型: $type"
            return 1
            ;;
    esac
    
    # 这里可以添加实际的Erlang调用
    # /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "dgiot_logger:set_loglevel(<<\"$type\">>, <<\"$name\">>, <<\"$level\">>)."
}

# 查看日志文件
view_logs() {
    local lines="${1:-50}"
    local log_type="${2:-compile}"
    
    case "$log_type" in
        "compile")
            local log_file="$LOG_FILE"
            ;;
        "emqx")
            local log_file="$EMQX_LOG_FILE"
            ;;
        "emqx1")
            local log_file="$EMQX_LOG1_FILE"
            ;;
        "crash")
            local log_file="$LOG_DIR/crash.dump"
            ;;
        "all")
            echo -e "${CYAN}=== 所有可用日志文件 ===${NC}"
            find "$LOG_DIR" -name "*.log*" -type f | while read -r file; do
                echo "文件: $(basename "$file") - 大小: $(du -h "$file" | cut -f1) - 行数: $(wc -l < "$file" 2>/dev/null || echo "0")"
            done
            return 0
            ;;
        *)
            local log_file="$LOG_DIR/$log_type"
            ;;
    esac
    
    if [[ -f "$log_file" ]]; then
        echo -e "${CYAN}=== 查看日志: $log_file (最后 $lines 行) ===${NC}"
        tail -n "$lines" "$log_file"
    else
        log_warning "日志文件不存在: $log_file"
        
        # 尝试查找类似的日志文件
        echo -e "${YELLOW}尝试查找可用日志文件...${NC}"
        find "$LOG_DIR" -name "*.log*" -type f 2>/dev/null | head -5 | while read -r found_file; do
            echo "找到: $found_file"
        done
        
        return 1
    fi
}

# 清理日志
clean_logs() {
    local days="${1:-7}"
    
    log_info "清理 $days 天前的日志文件..."
    
    # 清理编译调试日志
    find "$LOG_DIR" -name "dgiot_compile_debug.log.*" -mtime +$days -delete 2>/dev/null
    
    # 清理其他日志备份
    find "$LOG_DIR" -name "*.log.*" -mtime +$days -delete 2>/dev/null
    
    log_info "日志清理完成"
}

# 监控日志
monitor_logs() {
    local log_type="${1:-compile}"
    local keyword="${2:-}"
    
    case "$log_type" in
        "compile")
            local log_file="$LOG_FILE"
            ;;
        "emqx")
            local log_file="$EMQX_LOG_FILE"
            ;;
        "emqx1")
            local log_file="$EMQX_LOG1_FILE"
            ;;
        *)
            local log_file="$LOG_DIR/$log_type"
            ;;
    esac
    
    if [[ ! -f "$log_file" ]]; then
        log_warning "日志文件不存在: $log_file"
        
        # 尝试查找可用的日志文件
        local available_logs=$(find "$LOG_DIR" -name "*.log*" -type f 2>/dev/null | head -1)
        if [[ -n "$available_logs" ]]; then
            log_info "使用找到的日志文件: $available_logs"
            log_file="$available_logs"
        else
            log_error "没有找到任何日志文件"
            return 1
        fi
    fi
    
    echo -e "${CYAN}=== 监控日志: $log_file ===${NC}"
    echo -e "${YELLOW}按 Ctrl+C 停止监控${NC}"
    echo ""
    
    if [[ -n "$keyword" ]]; then
        tail -f "$log_file" | grep --color=always "$keyword"
    else
        tail -f "$log_file"
    fi
}

# 生成日志报告
generate_log_report() {
    local report_file="$LOG_DIR/log_report_$(date +%Y%m%d_%H%M%S).txt"
    
    log_info "生成日志报告: $report_file"
    
    {
        echo "=== DGIOT日志报告 ==="
        echo "生成时间: $(date)"
        echo "系统: $(uname -a)"
        echo "当前目录: $(pwd)"
        echo "日志目录: $LOG_DIR"
        echo ""
        
        echo "=== 日志文件统计 ==="
        find "$LOG_DIR" -name "*.log*" -type f 2>/dev/null | while read -r file; do
            echo "文件: $(basename "$file")"
            echo "路径: $file"
            echo "大小: $(du -h "$file" 2>/dev/null | cut -f1 || echo "N/A")"
            echo "修改时间: $(stat -c %y "$file" 2>/dev/null || echo "N/A")"
            echo "行数: $(wc -l < "$file" 2>/dev/null || echo "0")"
            echo ""
        done
        
        echo "=== 最近错误日志 (从所有日志文件) ==="
        find "$LOG_DIR" -name "*.log*" -type f 2>/dev/null | while read -r file; do
            echo "--- $file 中的错误 ---"
            grep -i "error\|fail\|exception\|critical" "$file" 2>/dev/null | tail -5
            echo ""
        done
        
        echo "=== 最近警告日志 (从所有日志文件) ==="
        find "$LOG_DIR" -name "*.log*" -type f 2>/dev/null | while read -r file; do
            echo "--- $file 中的警告 ---"
            grep -i "warn\|warning" "$file" 2>/dev/null | tail -5
            echo ""
        done
        
        echo "=== 编译调试相关日志 ==="
        if [[ -f "$LOG_FILE" ]]; then
            echo "--- $LOG_FILE 最后20行 ---"
            tail -20 "$LOG_FILE" 2>/dev/null
        else
            echo "编译调试日志文件不存在"
        fi
        
    } > "$report_file"
    
    log_info "日志报告已生成: $report_file"
    cat "$report_file"
}

# 测试日志功能
test_logging() {
    log_info "=== 开始测试日志功能 ==="
    
    # 测试不同日志级别
    log_debug "这是一条调试日志"
    log_info "这是一条信息日志"
    log_warn "这是一条警告日志"
    log_error "这是一条错误日志"
    log_critical "这是一条严重错误日志"
    
    # 测试中文日志
    chinese_log "info" "中文测试消息" "处理中文: %s"
    
    # 测试Erlang风格日志
    erlang_style_log "info" "test_module.erl" 123 "测试消息: %s" "参数"
    
    # 测试通道日志
    channel_log "channel_123" "product_456" "device_789" "通道日志测试: %s %s" "参数1" "参数2"
    
    log_info "=== 日志功能测试完成 ==="
}

# 显示帮助信息
show_log_help() {
    echo -e "${CYAN}DGIOT标准日志工具${NC}"
    echo ""
    echo -e "${GREEN}用法:${NC} source log_utils.sh [命令]"
    echo ""
    echo -e "${YELLOW}命令:${NC}"
    echo "  test_logging                  - 测试日志功能"
    echo "  view_logs [行数] [类型]       - 查看日志 (类型: compile, emqx, emqx1, crash, all)"
    echo "  monitor_logs [类型] [关键词]  - 监控日志"
    echo "  clean_logs [天数]             - 清理旧日志"
    echo "  generate_log_report           - 生成日志报告"
    echo "  help                          - 显示帮助信息"
    echo ""
    echo -e "${YELLOW}可用函数 (导入后使用):${NC}"
    echo "  log_debug <消息> [参数...]     - 调试日志"
    echo "  log_info <消息> [参数...]      - 信息日志"
    echo "  log_warn <消息> [参数...]      - 警告日志"
    echo "  log_error <消息> [参数...]     - 错误日志"
    echo "  log_critical <消息> [参数...]  - 严重错误日志"
    echo ""
    echo "  chinese_log <级别> <中文消息> <格式> [参数...] - 中文日志"
    echo "  erlang_style_log <级别> <文件> <行号> <格式> [参数...] - Erlang风格日志"
    echo "  channel_log <通道ID> <产品ID> <设备地址> <格式> [参数...] - 通道日志"
    echo ""
    echo "  set_log_level <级别>          - 设置日志级别 (debug, info, warn, error, critical)"
    echo "  set_plugin_log_level <类型> <名称> <级别> - 设置插件/模块日志级别"
    echo ""
    echo -e "${YELLOW}环境变量:${NC}"
    echo "  DGIOT_LOG_LEVEL - 设置日志级别 (默认: $(get_config "logging.default_level"))"
    echo ""
    echo -e "${YELLOW}日志文件位置:${NC}"
    echo "  编译调试日志: $LOG_FILE"
    echo "  EMQX主日志: $EMQX_LOG_FILE"
    echo "  EMQX轮转日志: $EMQX_LOG1_FILE"
    echo "  日志目录: $LOG_DIR"
    echo ""
    echo -e "${YELLOW}配置来源:${NC}"
    echo "  配置文件: $CONFIG_DIR/skill_config.json"
    echo ""
    echo -e "${YELLOW}示例:${NC}"
    echo "  source log_utils.sh"
    echo "  set_log_level debug"
    echo "  log_info \"测试消息: %s\" \"参数\""
    echo "  chinese_log info \"中文消息\" \"处理结果: %s\""
    echo "  log_utils.sh view_logs 100 emqx"
    echo "  log_utils.sh view_logs all"
    echo "  log_utils.sh monitor_logs compile \"error\""
}

# 初始化日志系统
init_log_system

# 主函数
main() {
    local command="$1"
    shift
    
    case "$command" in
        "test_logging")
            test_logging
            ;;
        "view_logs")
            view_logs "$@"
            ;;
        "monitor_logs")
            monitor_logs "$@"
            ;;
        "clean_logs")
            clean_logs "$@"
            ;;
        "generate_log_report")
            generate_log_report
            ;;
        "help"|"")
            show_log_help
            ;;
        *)
            echo -e "${RED}未知命令: $command${NC}"
            show_log_help
            return 1
            ;;
    esac
    
    return $?
}

# 如果直接执行脚本，执行主函数
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
