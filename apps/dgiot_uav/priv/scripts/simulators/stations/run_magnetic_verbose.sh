#!/bin/bash
# 磁航向工位一键调测脚本（详细日志版）
# 整合环境检查、IP绑定、详细日志测试、结果验证等功能

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 脚本目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$SCRIPT_DIR/logs"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# 日志文件
MAIN_LOG="$LOG_DIR/magnetic_test_${TIMESTAMP}.log"

# 创建日志目录
mkdir -p "$LOG_DIR"

# 日志函数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$MAIN_LOG"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$MAIN_LOG"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$MAIN_LOG"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$MAIN_LOG"
}

log_separator() {
    echo "============================================================" | tee -a "$MAIN_LOG"
}

# 检查Python环境
check_python() {
    log_info "检查Python环境..."
    
    if ! command -v python3 &> /dev/null; then
        log_error "Python3未安装"
        return 1
    fi
    
    log_success "Python3版本: $(python3 --version)"
    return 0
}

# 检查IP绑定状态
check_ip_binding() {
    log_info "检查IP绑定状态..."
    
    local ip_list=("192.168.100.20" "192.168.100.21" "192.168.100.23")
    local all_bound=true
    
    for ip in "${ip_list[@]}"; do
        if ip addr show | grep -q "$ip"; then
            log_success "IP $ip 已绑定"
        else
            log_warning "IP $ip 未绑定"
            all_bound=false
        fi
    done
    
    if [ "$all_bound" = true ]; then
        log_success "所有IP已绑定"
        return 0
    else
        log_warning "部分IP未绑定，建议使用 --auto-bind 参数"
        return 1
    fi
}

# 绑定IP地址
bind_ip() {
    log_info "绑定IP地址..."
    
    local ip_list=("192.168.100.20" "192.168.100.21" "192.168.100.23")
    local all_success=true
    
    for ip in "${ip_list[@]}"; do
        if sudo ip addr add "$ip/24" dev eth0 2>> "$MAIN_LOG"; then
            log_success "IP $ip 绑定成功"
        else
            log_warning "IP $ip 绑定失败（可能已绑定）"
        fi
    done
    
    log_info "验证IP绑定状态..."
    check_ip_binding
}

# 检查DG-IoT服务状态
check_dgiot_status() {
    log_info "检查DG-IoT服务状态..."
    
    if netstat -tuln | grep -q ":20000 "; then
        log_success "DG-IoT服务正在运行（端口20000）"
        return 0
    else
        log_error "DG-IoT服务未运行（端口20000未监听）"
        return 1
    fi
}

# 检查端口监听状态
check_port_status() {
    log_info "检查端口监听状态..."
    
    local ports=("502" "1234" "10007" "1801")
    
    for port in "${ports[@]}"; do
        if netstat -tuln | grep -q ":$port "; then
            log_success "端口 $port 已监听"
        else
            log_warning "端口 $port 未监听"
        fi
    done
}

# 运行详细日志测试
run_verbose_test() {
    log_info "运行磁航向工位详细日志测试..."
    
    local verbose_script="$SCRIPT_DIR/station_1700_magnetic_verbose.py"
    
    if [ ! -f "$verbose_script" ]; then
        log_error "测试脚本不存在: $verbose_script"
        return 1
    fi
    
    log_separator
    python3 "$verbose_script" --verbose 2>&1 | tee -a "$MAIN_LOG"
    local test_result=$?
    log_separator
    
    if [ $test_result -eq 0 ]; then
        log_success "详细日志测试完成"
        return 0
    else
        log_error "详细日志测试失败"
        return 1
    fi
}

# 查看日志
view_logs() {
    log_info "查看日志文件..."
    
    if [ -f "$MAIN_LOG" ]; then
        echo ""
        log_separator
        cat "$MAIN_LOG"
        log_separator
        echo ""
    else
        log_warning "日志文件不存在"
    fi
}

# 清理旧日志
cleanup_old_logs() {
    log_info "清理7天前的旧日志..."
    
    find "$LOG_DIR" -name "magnetic_test_*.log" -mtime +7 -delete
    find "$LOG_DIR" -name "magnetic_verbose_*.log" -mtime +7 -delete
    
    log_success "旧日志清理完成"
}

# 显示帮助信息
show_help() {
    cat << EOF
磁航向工位一键调测脚本（详细日志版）

用法: $0 [选项]

选项:
  --help           显示此帮助信息
  --auto-bind      自动绑定IP地址
  --check-only     仅检查环境，不运行测试
  --skip-check     跳过环境检查
  --view-logs      查看日志文件
  --cleanup        清理旧日志
  --verbose        详细日志输出

示例:
  $0                          # 运行完整测试
  $0 --auto-bind              # 自动绑定IP并测试
  $0 --check-only             # 仅检查环境
  $0 --skip-check             # 跳过环境检查直接测试
  $0 --view-logs              # 查看日志文件
  $0 --cleanup                # 清理旧日志

日志文件:
  主日志: $LOG_DIR/magnetic_test_<timestamp>.log
  详细日志: $LOG_DIR/magnetic_verbose_<timestamp>.log

EOF
}

# 主函数
main() {
    local auto_bind=false
    local check_only=false
    local skip_check=false
    local view_logs_only=false
    local cleanup_only=false
    local verbose_mode=false
    
    # 解析参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help)
                show_help
                exit 0
                ;;
            --auto-bind)
                auto_bind=true
                shift
                ;;
            --check-only)
                check_only=true
                shift
                ;;
            --skip-check)
                skip_check=true
                shift
                ;;
            --view-logs)
                view_logs_only=true
                shift
                ;;
            --cleanup)
                cleanup_only=true
                shift
                ;;
            --verbose)
                verbose_mode=true
                shift
                ;;
            *)
                log_error "未知参数: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 清理旧日志
    if [ "$cleanup_only" = true ]; then
        cleanup_old_logs
        exit 0
    fi
    
    # 查看日志
    if [ "$view_logs_only" = true ]; then
        view_logs
        exit 0
    fi
    
    # 检查Python环境
    if ! check_python; then
        exit 1
    fi
    
    log_separator
    log_info "磁航向工位一键调测开始"
    log_info "时间: $(date '+%Y-%m-%d %H:%M:%S')"
    log_separator
    
    # 环境检查
    if [ "$skip_check" = false ]; then
        log_info "开始环境检查..."
        
        # 自动绑定IP
        if [ "$auto_bind" = true ]; then
            bind_ip
        else
            check_ip_binding
        fi
        
        # 检查DG-IoT状态
        check_dgiot_status
        
        # 检查端口状态
        check_port_status
        
        # 仅检查模式
        if [ "$check_only" = true ]; then
            log_info "环境检查完成"
            exit 0
        fi
    else
        log_warning "跳过环境检查"
    fi
    
    # 运行测试
    log_separator
    log_info "开始测试执行..."
    log_separator
    
    if run_verbose_test; then
        log_success "测试执行成功"
    else
        log_error "测试执行失败"
        exit 1
    fi
    
    # 完成
    log_separator
    log_success "磁航向工位一键调测完成"
    log_info "时间: $(date '+%Y-%m-%d %H:%M:%S')"
    log_separator
    
    # 显示日志位置
    log_info "日志文件: $MAIN_LOG"
    
    return 0
}

# 执行主函数
main "$@"
