#!/bin/bash
#
# 无人机测试产线 - 一键式测试启动脚本
#
# 用法:
#   ./start_one_click_test.sh              # 交互式选择
#   ./start_one_click_test.sh --station 1500 # 测试总测工位
#   ./start_one_click_test.sh --full-line    # 测试完整产线
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 打印带颜色的消息
print_info() {
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

# 显示标题
show_banner() {
    echo -e "${BLUE}"
    echo "============================================================================"
    echo "                  无人机测试产线 - 一键式端到端测试"
    echo "============================================================================"
    echo -e "${NC}"
}

# 检查Python环境
check_python() {
    print_info "检查Python环境..."

    if ! command -v python3 &> /dev/null; then
        print_error "Python3未安装，请先安装Python3"
        return 1
    fi

    print_success "Python3版本: $(python3 --version)"
    return 0
}

# 检查脚本文件
check_script() {
    print_info "检查测试脚本..."

    if [ ! -f "one_click_production_test.py" ]; then
        print_error "测试脚本不存在: one_click_production_test.py"
        return 1
    fi

    print_success "测试脚本存在"
    return 0
}

# 创建日志目录
create_log_dir() {
    local log_dir="./test_logs"
    if [ ! -d "$log_dir" ]; then
        mkdir -p "$log_dir"
        print_info "创建日志目录: $log_dir"
    fi
}

# 列出工位
list_stations() {
    print_info "可用工位列表:"
    echo ""
    echo "  1. 磁航向工位 (1200) - 扫码绑定入口，磁航向校准测试"
    echo "  2. 总测工位 (1500) - 10步测试流程，完整功能验证"
    echo "  3. 拷机工位 (1600) - 舵面数据采集测试"
    echo "  4. 桁架工位 (1100) - 桁架机械手测试"
    echo "  5. 告警检测工位 (1700) - 全程噪音监控"
    echo "  6. 完整产线测试 (1200→1500→1600→1100)"
    echo ""
}

# 显示菜单
show_menu() {
    echo ""
    echo "请选择测试模式:"
    echo ""
    echo "  1. 查看工位列表"
    echo "  2. 查看工位详情"
    echo "  3. 测试单个工位"
    echo "  4. 测试完整产线"
    echo "  5. 查看最新测试结果"
    echo "  6. 查看最新日志"
    echo "  0. 退出"
    echo ""
    read -p "请输入选项 (0-6): " choice
    echo ""
}

# 查看工位详情
show_station_detail() {
    list_stations
    read -p "请输入工位编号 (1-5): " station_num

    case $station_num in
        1) python3 one_click_production_test.py --station-detail 1200 ;;
        2) python3 one_click_production_test.py --station-detail 1500 ;;
        3) python3 one_click_production_test.py --station-detail 1600 ;;
        4) python3 one_click_production_test.py --station-detail 1100 ;;
        5) print_warning "告警检测工位详情待实现" ;;
        *) print_error "无效的工位编号" ;;
    esac
}

# 测试单个工位
test_single_station() {
    list_stations
    read -p "请输入工位编号 (1-4): " station_num

    case $station_num in
        1) python3 one_click_production_test.py --station 1200 ;;
        2) python3 one_click_production_test.py --station 1500 ;;
        3) python3 one_click_production_test.py --station 1600 ;;
        4) python3 one_click_production_test.py --station 1100 ;;
        *) print_error "无效的工位编号" ;;
    esac
}

# 查看最新测试结果
show_latest_result() {
    local log_dir="./test_logs"
    local latest_json=$(ls -t "$log_dir"/result_*.json 2>/dev/null | head -1)

    if [ -z "$latest_json" ]; then
        print_warning "没有找到测试结果文件"
        return 1
    fi

    print_info "最新测试结果: $latest_json"
    echo ""
    cat "$latest_json"
}

# 查看最新日志
show_latest_log() {
    local log_dir="./test_logs"
    local latest_log=$(ls -t "$log_dir"/production_test_*.log 2>/dev/null | head -1)

    if [ -z "$latest_log" ]; then
        print_warning "没有找到日志文件"
        return 1
    fi

    print_info "最新日志文件: $latest_log"
    echo ""
    tail -50 "$latest_log"
}

# 主函数
main() {
    show_banner

    # 检查环境
    check_python || exit 1
    check_script || exit 1
    create_log_dir

    # 处理命令行参数
    if [ $# -gt 0 ]; then
        case $1 in
            --station)
                if [ -z "$2" ]; then
                    print_error "请指定工位ID"
                    exit 1
                fi
                python3 one_click_production_test.py --station "$2"
                ;;
            --full-line)
                python3 one_click_production_test.py --full-line --generate-report
                ;;
            --list-stations)
                python3 one_click_production_test.py --list-stations
                ;;
            *)
                print_error "未知参数: $1"
                echo "用法: $0 [--station ID|--full-line|--list-stations]"
                exit 1
                ;;
        esac
        exit $?
    fi

    # 交互式菜单
    while true; do
        show_menu

        case $choice in
            1)
                list_stations
                ;;
            2)
                show_station_detail
                ;;
            3)
                test_single_station
                ;;
            4)
                print_info "开始完整产线测试..."
                python3 one_click_production_test.py --full-line --generate-report
                ;;
            5)
                show_latest_result
                ;;
            6)
                show_latest_log
                ;;
            0)
                print_info "退出测试系统"
                exit 0
                ;;
            *)
                print_error "无效的选项"
                ;;
        esac

        echo ""
        read -p "按Enter键继续..."
    done
}

# 运行主函数
main "$@"
