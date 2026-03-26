#!/bin/bash
# 磁航向工位一键调测脚本
# 集成环境准备、IP绑定、测试执行、结果验证等完整流程

set -e  # 遇到错误立即退出

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 配置
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
TEST_SCRIPT="$SCRIPT_DIR/station_1700_magnetic_enhanced.py"
LOG_DIR="$PROJECT_ROOT/test_records/station_1700"

# 磁航向工位配置
STATION_ID=1700
STATION_NAME="磁航向校准工位"

***REMOVED***地址配置
PLC_IP="192.168.100.20"
GROUND_STATION_IP="192.168.100.21"
SCANNER_IP="192.168.100.23"
DGIOT_HOST="192.168.100.100"
DGIOT_PORT=20000

# 打印带颜色的消息
print_message() {
    local color=$1
    local message=$2
    echo -e "${color}${message}${NC}"
}

print_header() {
    echo ""
    print_message "$BLUE" "======================================================================"
    print_message "$BLUE" "$1"
    print_message "$BLUE" "======================================================================"
    echo ""
}

print_success() {
    print_message "$GREEN" "✅ $1"
}

print_warning() {
    print_message "$YELLOW" "⚠️  $1"
}

print_error() {
    print_message "$RED" "❌ $1"
}

print_step() {
    echo -e "${BLUE}[步骤]${NC} $1"
}

# 检查命令是否存在
check_command() {
    if ! command -v $1 &> /dev/null; then
        print_error "$1 未安装"
        return 1
    fi
    return 0
}

# 检查Python环境
check_python() {
    print_step "检查Python环境..."
    
    if ! check_command python3; then
        print_error "Python3未安装，请先安装Python3"
        return 1
    fi
    
    python_version=$(python3 --version | awk '{print $2}')
    print_success "Python版本: $python_version"
    
    return 0
}

# 检查DG-IoT服务器状态
check_dgiot() {
    print_step "检查DG-IoT服务器状态..."
    
    if nc -z $DGIOT_HOST $DGIOT_PORT 2>/dev/null; then
        print_success "DG-IoT服务器运行中: $DGIOT_HOST:$DGIOT_PORT"
        return 0
    else
        print_error "DG-IoT服务器未运行: $DGIOT_HOST:$DGIOT_PORT"
        print_warning "请先启动DG-IoT服务器: cd $PROJECT_ROOT && make run"
        return 1
    fi
}

# 检查并绑定IP地址
check_and_bind_ips() {
    print_step "检查IP地址绑定状态..."
    
    local required_ips=($PLC_IP $GROUND_STATION_IP $SCANNER_IP)
    local missing_ips=()
    
    # 检查网络接口
    local interface=$(ip route | grep default | awk '{print $5}')
    if [ -z "$interface" ]; then
        print_error "无法确定网络接口"
        return 1
    fi
    
    print_success "网络接口: $interface"
    
    # 检查IP绑定状态
    for ip in "${required_ips[@]}"; do
        if ip addr show $interface | grep -q "$ip"; then
            print_success "IP已绑定: $ip"
        else
            print_warning "IP未绑定: $ip"
            missing_ips+=($ip)
        fi
    done
    
    # 绑定缺失的IP
    if [ ${#missing_ips[@]} -gt 0 ]; then
        print_step "绑定缺失的IP地址..."
        
        for ip in "${missing_ips[@]}"; do
            print_step "绑定IP: $ip"
            if sudo ip addr add "$ip/24" dev $interface 2>/dev/null; then
                print_success "IP绑定成功: $ip"
            else
                print_error "IP绑定失败: $ip"
                print_warning "可能需要sudo权限，请手动执行: sudo ip addr add $ip/24 dev $interface"
                return 1
            fi
        done
    fi
    
    return 0
}

# 检查测试脚本
check_test_script() {
    print_step "检查测试脚本..."
    
    if [ ! -f "$TEST_SCRIPT" ]; then
        print_error "测试脚本不存在: $TEST_SCRIPT"
        return 1
    fi
    
    if [ ! -x "$TEST_SCRIPT" ]; then
        print_step "添加执行权限..."
        chmod +x "$TEST_SCRIPT"
    fi
    
    print_success "测试脚本就绪: $TEST_SCRIPT"
    return 0
}

# 创建日志目录
create_log_dir() {
    print_step "创建日志目录..."
    
    mkdir -p "$LOG_DIR/packets"
    print_success "日志目录: $LOG_DIR"
    
    return 0
}

# 执行测试
run_test() {
    print_header "开始执行磁航向工位测试"
    
    local device_id="${1:-UAV-001}"
    local auto_bind="${2:-false}"
    local verbose="${3:-false}"
    
    local cmd_args="--device-id $device_id"
    
    if [ "$auto_bind" = "true" ]; then
        cmd_args="$cmd_args --auto-bind"
    fi
    
    if [ "$verbose" = "true" ]; then
        cmd_args="$cmd_args --verbose"
    fi
    
    print_step "执行测试命令: python3 $TEST_SCRIPT $cmd_args"
    
    cd "$SCRIPT_DIR"
    python3 "$TEST_SCRIPT" $cmd_args
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        return 0
    else
        return 1
    fi
}

# 查看测试结果
show_results() {
    print_header "查看测试结果"
    
    # 查找最新的测试日志
    local latest_log=$(ls -t "$LOG_DIR"/test_*.log 2>/dev/null | head -1)
    
    if [ -n "$latest_log" ]; then
        print_success "最新测试日志: $latest_log"
        echo ""
        print_step "测试日志内容:"
        cat "$latest_log"
    else
        print_warning "未找到测试日志"
    fi
    
    # 查找报文日志
    local latest_packet=$(ls -t "$LOG_DIR"/packets/packets_*.log 2>/dev/null | head -1)
    
    if [ -n "$latest_packet" ]; then
        echo ""
        print_success "报文日志: $latest_packet"
        local packet_count=$(wc -l < "$latest_packet")
        print_step "报文数量: $packet_count"
    fi
}

# 验证DG-IoT中的设备状态
verify_dgiot_device() {
    print_header "验证DG-IoT中的设备状态"
    
    print_step "查询设备绑定状态..."
    local result=$("$PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx eval" \
        'ets:tab2list(uav_ip_station_mapping).' 2>/dev/null)
    
    if [ -n "$result" ]; then
        print_success "设备绑定状态:"
        echo "$result"
    fi
    
    print_step "查询工位信息..."
    result=$("$PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx eval" \
        "dgiot_uav_business_service:get_station_by_ip(<<\"$GROUND_STATION_IP\">>)." 2>/dev/null)
    
    if [ -n "$result" ]; then
        print_success "工位信息:"
        echo "$result"
    fi
}

# 显示使用帮助
show_help() {
    cat << EOF
磁航向工位一键调测脚本

使用方法:
  $0 [选项]

选项:
  --device-id <ID>      指定设备ID (默认: UAV-001)
  --auto-bind           自动绑定IP地址
  --verbose             详细日志输出
  --skip-check          跳过环境检查
  --show-results        仅显示测试结果
  --verify              验证DG-IoT中的设备状态
  --help                显示此帮助信息

示例:
  # 基本用法
  $0

  # 自动绑定IP并测试
  $0 --auto-bind

  # 指定设备ID
  $0 --device-id UAV-002

  # 详细日志
  $0 --verbose

  # 仅查看测试结果
  $0 --show-results

  # 验证设备状态
  $0 --verify

配置:
  工位ID: $STATION_ID
  工位名称: $STATION_NAME
  PLC IP: $PLC_IP
  地测口IP: $GROUND_STATION_IP
  扫码枪IP: $SCANNER_IP
  DG-IoT: $DGIOT_HOST:$DGIOT_PORT

EOF
}

# 主函数
main() {
    local device_id="UAV-001"
    local auto_bind=false
    local verbose=false
    local skip_check=false
    local show_only=false
    local verify_only=false
    
    # 解析参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --device-id)
                device_id="$2"
                shift 2
                ;;
            --auto-bind)
                auto_bind=true
                shift
                ;;
            --verbose)
                verbose=true
                shift
                ;;
            --skip-check)
                skip_check=true
                shift
                ;;
            --show-results)
                show_only=true
                shift
                ;;
            --verify)
                verify_only=true
                shift
                ;;
            --help)
                show_help
                exit 0
                ;;
            *)
                print_error "未知选项: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    print_header "磁航向工位一键调测系统"
    print_success "工位: $STATION_NAME ($STATION_ID)"
    print_success "设备: $device_id"
    print_success "自动绑定IP: $auto_bind"
    
    # 仅显示结果
    if [ "$show_only" = "true" ]; then
        show_results
        exit 0
    fi
    
    # 仅验证状态
    if [ "$verify_only" = "true" ]; then
        verify_dgiot_device
        exit 0
    fi
    
    # 检查环境
    print_header "环境检查"
    
    if ! check_python; then
        exit 1
    fi
    
    if ! check_dgiot; then
        exit 1
    fi
    
    if [ "$skip_check" = "false" ]; then
        if ! check_and_bind_ips; then
            exit 1
        fi
    fi
    
    if ! check_test_script; then
        exit 1
    fi
    
    if ! create_log_dir; then
        exit 1
    fi
    
    # 执行测试
    print_header "准备执行测试"
    
    if run_test "$device_id" "$auto_bind" "$verbose"; then
        print_header "测试完成"
        print_success "磁航向工位测试成功完成"
        
        # 显示测试结果
        show_results
        
        # 验证设备状态
        if ! verify_only; then
            verify_dgiot_device
        fi
        
        exit 0
    else
        print_header "测试失败"
        print_error "磁航向工位测试失败"
        print_warning "请查看日志文件: $LOG_DIR"
        exit 1
    fi
}

# 捕获中断信号
trap 'print_warning "接收到中断信号，退出脚本"; exit 1' INT TERM

# 执行主函数
main "$@"
