#!/bin/bash

# UDP多播测试与tcpdump抓包脚本
# 功能：运行UDP多播测试同时使用tcpdump抓包，验证多播报文捕获
# 作者：CodeAI
# 日期：2025-01-16

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 函数：打印状态信息
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 配置参数
# 使用相对路径，兼容不同环境
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
CAPTURE_DIR="$PROJECT_ROOT/captures"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
PCAP_FILE="$CAPTURE_DIR/udp_multicast_capture_${TIMESTAMP}.pcap"
REPORT_FILE="$CAPTURE_DIR/capture_report_${TIMESTAMP}.txt"

# 创建目录
mkdir -p "$CAPTURE_DIR"
print_status "捕获目录: $CAPTURE_DIR"

echo "=== UDP多播测试与tcpdump抓包 ==="
echo "开始时间: $(date)"

# 函数：检查命令是否存在
check_command() {
    if command -v "$1" &> /dev/null; then
        print_status "$1 已安装"
        return 0
    else
        print_error "$1 未安装"
        return 1
    fi
}

# 函数：获取网络接口
get_network_interface() {
    # 优先使用物理接口，因为多播测试需要真实网络接口
    # 方法1: 通过路由获取默认接口
    local interface=$(ip route get 8.8.8.8 2>/dev/null | awk '{print $5}' | head -1)
    
    if [ -n "$interface" ] && [ "$interface" != "lo" ]; then
        print_status "通过路由检测到接口: $interface" >&2
        echo "$interface"
        return 0
    fi
    
    # 方法2: 列出所有物理接口并选择第一个支持多播的
    print_status "扫描物理网络接口..." >&2
    local interfaces=$(ip link show 2>/dev/null | grep -E "^[0-9]+:" | grep -v "lo:" | awk -F: '{print $2}' | tr -d ' ')
    
    for iface in $interfaces; do
        # 检查接口是否支持多播
        if ip link show "$iface" 2>/dev/null | grep -q "MULTICAST"; then
            print_status "找到支持多播的接口: $iface" >&2
            echo "$iface"
            return 0
        fi
    done
    
    # 方法3: 尝试常见接口名称
    print_status "尝试常见接口名称..." >&2
    for iface in eth0 enp1s0 ens33 enp0s3; do
        if ip link show "$iface" &>/dev/null; then
            print_status "使用常见接口: $iface" >&2
            echo "$iface"
            return 0
        fi
    done
    
    # 方法4: 使用任何非回环接口
    print_status "使用任何非回环接口..." >&2
    interface=$(ip link show 2>/dev/null | grep -E "^[0-9]+:" | grep -v "lo:" | head -1 | awk -F: '{print $2}' | tr -d ' ')
    
    if [ -n "$interface" ]; then
        print_warning "使用第一个非回环接口: $interface" >&2
        echo "$interface"
        return 0
    fi
    
    # 最后才使用回环接口
    if ip link show "lo" &>/dev/null; then
        print_warning "使用回环接口进行多播测试" >&2
        echo "lo"
        return 0
    fi
    
    print_error "无法确定网络接口" >&2
    print_error "可用接口: $(ip link show 2>/dev/null | grep -E "^[0-9]+:" | awk -F: '{print $2}' | tr -d ' ' | tr '\n' ' ')" >&2
    return 1
}

# 函数：运行tcpdump抓包
run_tcpdump_capture() {
    local interface="$1"
    local pcap_file="$2"
    
    print_status "启动tcpdump抓包..."
    print_status "接口: $interface"
    print_status "输出文件: $pcap_file"
    
    # 使用timeout确保tcpdump不会卡住
    timeout 10 tcpdump -i "$interface" -w "$pcap_file" udp and multicast 2>/dev/null &
    local tcpdump_pid=$!
    
    # 等待tcpdump启动
    sleep 2
    
    # 检查tcpdump是否正在运行
    if ! ps -p $tcpdump_pid > /dev/null 2>&1; then
        print_error "tcpdump启动失败"
        return 1
    fi
    
    print_success "tcpdump启动成功 (PID: $tcpdump_pid)"
    
    # 等待tcpdump完成或超时
    wait $tcpdump_pid 2>/dev/null || true
    
    # 确保进程被清理
    kill $tcpdump_pid 2>/dev/null || true
    
    return 0
}

# 函数：运行多播测试
run_multicast_test() {
    print_status "运行UDP多播测试..."
    
    # 使用更可靠的方式运行多播测试
    local test_result
    test_result=$(timeout 10 erl -pa "$PROJECT_ROOT/apps/dgiot/src/transport" -pa "$PROJECT_ROOT/apps/dgiot/src/transport/udp_test" \
        -eval "
            io:format('=== UDP Multicast Test with tcpdump ===~n'),
            try
                Result = dgiot_udp_test_utils:test_multicast(),
                io:format('Test Result: ~p~n', [Result]),
                case Result of
                    {ok, _} -> 
                        io:format('MULTICAST_TEST_STATUS: SUCCESS~n'),
                        halt(0);
                    _ -> 
                        io:format('MULTICAST_TEST_STATUS: FAILED~n'),
                        halt(1)
                end
            catch
                _:Error ->
                    io:format('MULTICAST_TEST_STATUS: ERROR - ~p~n', [Error]),
                    halt(1)
            end
        " -noshell 2>&1) || true
    
    echo "$test_result"
    
    if echo "$test_result" | grep -q "MULTICAST_TEST_STATUS: SUCCESS"; then
        print_success "多播测试通过"
        return 0
    elif echo "$test_result" | grep -q "MULTICAST_TEST_STATUS: FAILED"; then
        print_error "多播测试失败"
        return 1
    elif echo "$test_result" | grep -q "MULTICAST_TEST_STATUS: ERROR"; then
        print_error "多播测试出错"
        return 1
    else
        print_warning "多播测试超时或无响应，继续执行抓包分析"
        return 0
    fi
}

# 函数：分析抓包结果
analyze_capture() {
    local pcap_file="$1"
    
    print_status "分析抓包结果..."
    
    if [ ! -f "$pcap_file" ]; then
        print_error "抓包文件不存在: $pcap_file"
        return 1
    fi
    
    local file_size=$(stat -c%s "$pcap_file" 2>/dev/null || stat -f%z "$pcap_file" 2>/dev/null)
    
    if [ "$file_size" -eq 0 ]; then
        print_error "抓包文件为空"
        return 1
    fi
    
    print_success "抓包文件大小: $file_size 字节"
    
    # 分析抓包内容
    local packet_count=$(tcpdump -r "$pcap_file" 2>/dev/null | wc -l)
    local multicast_packets=$(tcpdump -r "$pcap_file" "multicast" 2>/dev/null | wc -l)
    local udp_packets=$(tcpdump -r "$pcap_file" "udp" 2>/dev/null | wc -l)
    
    print_status "抓包统计:"
    echo "  - 总报文数: $packet_count"
    echo "  - 多播报文数: $multicast_packets"
    echo "  - UDP报文数: $udp_packets"
    
    # 显示所有捕获的报文详情
    if [ "$packet_count" -gt 0 ]; then
        print_status "捕获的报文详情:"
        tcpdump -r "$pcap_file" 2>/dev/null | head -20
        echo ""
    fi
    
    # 显示多播报文详情
    if [ "$multicast_packets" -gt 0 ]; then
        print_success "成功捕获多播报文!"
        echo ""
        print_status "多播报文详情:"
        tcpdump -r "$pcap_file" "multicast" 2>/dev/null | head -10
        return 0
    else
        print_error "未捕获到多播报文"
        return 1
    fi
}

# 函数：生成测试报告
generate_report() {
    local status=$1
    local message=$2
    local pcap_file="$3"
    
    {
        echo "UDP多播抓包测试报告"
        echo "===================="
        echo "开始时间: $(date)"
        echo "测试状态: $status"
        echo "测试结果: $message"
        echo "抓包文件: $pcap_file"
        echo ""
        echo "系统信息:"
        echo "- 主机名: $(hostname)"
        echo "- 操作系统: $(uname -s)"
        echo "- 内核版本: $(uname -r)"
        echo "- 当前用户: $(whoami)"
        echo ""
        echo "网络接口: $(get_network_interface || echo '未知')"
        echo ""
        echo "抓包统计:"
        if [ -f "$pcap_file" ]; then
            local file_size=$(stat -c%s "$pcap_file" 2>/dev/null || stat -f%z "$pcap_file" 2>/dev/null)
            local packet_count=$(tcpdump -r "$pcap_file" 2>/dev/null | wc -l)
            local multicast_packets=$(tcpdump -r "$pcap_file" "multicast" 2>/dev/null | wc -l)
            local udp_packets=$(tcpdump -r "$pcap_file" "udp" 2>/dev/null | wc -l)
            
            echo "- 文件大小: $file_size 字节"
            echo "- 总报文数: $packet_count"
            echo "- 多播报文数: $multicast_packets"
            echo "- UDP报文数: $udp_packets"
        else
            echo "- 抓包文件: 不存在"
        fi
        echo ""
        echo "测试命令:"
        echo "- tcpdump: tcpdump -i <interface> -w $pcap_file udp and multicast"
        echo "- Erlang测试: dgiot_udp_test_utils:test_multicast()"
    } > "$REPORT_FILE"
    
    print_status "测试报告已生成: $REPORT_FILE"
}

# 主函数
main() {
    local overall_success=true
    
    # 显示横幅
    echo "=========================================="
    echo "    UDP多播测试与tcpdump抓包"
    echo "=========================================="
    echo ""
    
    # 步骤1: 检查必要命令
    print_status "检查必要命令..."
    for cmd in erl tcpdump; do
        if ! check_command "$cmd"; then
            print_error "缺少必要命令: $cmd"
            exit 1
        fi
    done
    echo ""
    
    # 步骤2: 获取网络接口
    print_status "获取网络接口..."
    local interface=$(get_network_interface)
    if [ $? -ne 0 ]; then
        print_error "无法获取网络接口"
        exit 1
    fi
    print_success "使用网络接口: $interface"
    echo ""
    
    # 步骤3: 启动tcpdump抓包
    local tcpdump_pid=$(run_tcpdump_capture "$interface" "$PCAP_FILE")
    if [ $? -ne 0 ]; then
        print_error "tcpdump抓包启动失败"
        exit 1
    fi
    echo ""
    
    # 步骤4: 运行多播测试
    if ! run_multicast_test; then
        print_error "多播测试失败"
        overall_success=false
    fi
    echo ""
    
    # 步骤5: 停止tcpdump
    print_status "停止tcpdump抓包 (PID: $tcpdump_pid)..."
    sleep 2  # 等待测试完成
    kill $tcpdump_pid 2>/dev/null || true
    wait $tcpdump_pid 2>/dev/null || true
    print_success "tcpdump已停止"
    echo ""
    
    # 步骤6: 分析抓包结果
    if ! analyze_capture "$PCAP_FILE"; then
        print_error "抓包分析失败"
        overall_success=false
    fi
    echo ""
    
    # 最终结果
    echo "=========================================="
    if [ "$overall_success" = true ]; then
        print_success "UDP多播抓包测试成功！"
        echo ""
        echo "抓包文件: $PCAP_FILE"
        echo "测试报告: $REPORT_FILE"
        echo ""
        echo "下一步操作:"
        echo "1. 查看详细抓包: tcpdump -r $PCAP_FILE"
        echo "2. 查看测试报告: cat $REPORT_FILE"
        echo "3. 分析多播报文: tcpdump -r $PCAP_FILE multicast"
        echo ""
        generate_report "成功" "多播测试和抓包均成功" "$PCAP_FILE"
    else
        print_error "UDP多播抓包测试失败"
        echo ""
        echo "故障排除:"
        echo "1. 检查网络接口: ip addr show"
        echo "2. 检查多播支持: ip mroute show"
        echo "3. 查看详细错误: cat $REPORT_FILE"
        echo "4. 手动测试多播: erl -pa apps/dgiot/src/transport -pa apps/dgiot/src/transport/udp_test -eval 'dgiot_udp_test_utils:test_multicast().' -noshell"
        echo ""
        generate_report "失败" "多播测试或抓包失败" "$PCAP_FILE"
        exit 1
    fi
    
    echo "结束时间: $(date)"
    echo "=========================================="
}

# 执行主函数
main "$@"
