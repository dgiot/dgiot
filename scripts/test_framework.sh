#!/bin/bash
# test_framework.sh - 插件测试框架
# 用法: ./test_framework.sh <插件名> <测试用例名>
# 或: ./test_framework.sh <插件名> --all

set -euo pipefail

# 测试用例注册文件
TESTCASES_FILE=".testcases"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $*"; }

# 检查测试用例文件
check_testcases_file() {
    if [ ! -f "$TESTCASES_FILE" ]; then
        log_error "测试用例注册文件不存在: $TESTCASES_FILE"
        exit 1
    fi
}

# 列出所有插件
list_plugins() {
    check_testcases_file
    log_info "可用插件:"
    cut -d: -f1 "$TESTCASES_FILE" | grep -v "^#" | sort -u | while read plugin; do
        echo "  $plugin"
    done
}

# 列出指定插件的测试用例
list_testcases() {
    local plugin="$1"
    check_testcases_file
    log_info "插件 $plugin 的测试用例:"
    grep "^$plugin:" "$TESTCASES_FILE" | grep -v "^#" | while read line; do
        testcase=$(echo "$line" | cut -d: -f2)
        desc=$(echo "$line" | cut -d: -f4)
        echo "  $testcase - $desc"
    done
}

# 执行单个测试用例
run_testcase() {
    local plugin="$1"
    local testcase="$2"
    
    check_testcases_file
    
    log_info "执行测试: $plugin:$testcase"
    
    # 查找测试用例
    line=$(grep "^$plugin:$testcase:" "$TESTCASES_FILE" | head -1)
    if [ -z "$line" ]; then
        log_error "测试用例未找到: $plugin:$testcase"
        echo "使用以下命令查看可用测试用例:"
        echo "  ./test_framework.sh $plugin --list"
        exit 1
    fi
    
    script=$(echo "$line" | cut -d: -f3)
    desc=$(echo "$line" | cut -d: -f4)
    
    log_info "测试描述: $desc"
    log_info "测试脚本: $script"
    
    if [ ! -f "$script" ]; then
        log_error "测试脚本不存在: $script"
        exit 1
    fi
    
    log_info "开始执行测试..."
    echo "================================================================================"
    
    # 设置执行权限
    chmod +x "$script" 2>/dev/null || true
    
    # 执行测试脚本
    if echo "$script" | grep -q "\.sh$"; then
        bash "$script"
    elif echo "$script" | grep -q "\.py$"; then
        python3 "$script"
    else
        log_error "不支持的脚本类型: $script"
        exit 1
    fi
    
    echo "================================================================================"
    log_info "测试执行完成: $plugin:$testcase"
}

# 执行插件所有测试用例
run_all_testcases() {
    local plugin="$1"
    
    check_testcases_file
    
    log_info "执行插件 $plugin 的所有测试用例..."
    
    grep "^$plugin:" "$TESTCASES_FILE" | grep -v "^#" | while read line; do
        testcase=$(echo "$line" | cut -d: -f2)
        desc=$(echo "$line" | cut -d: -f4)
        
        echo ""
        echo "================================================================================="
        echo "执行测试用例: $testcase - $desc"
        echo "================================================================================="
        
        if ! run_testcase "$plugin" "$testcase"; then
            log_warn "测试用例 $testcase 执行失败"
        fi
    done
    
    echo ""
    echo "================================================================================="
    log_info "插件 $plugin 的所有测试用例执行完成"
    echo "================================================================================="
}

# 显示帮助信息
show_help() {
    echo "插件测试框架使用说明:"
    echo ""
    echo "用法:"
    echo "  ./test_framework.sh <命令> [参数]"
    echo ""
    echo "命令:"
    echo "  --list-plugins             列出所有插件"
    echo "  --list <插件名>            列出指定插件的测试用例"
    echo "  --run <插件名> <测试用例>  执行单个测试用例"
    echo "  --all <插件名>             执行插件的所有测试用例"
    echo "  --help                     显示帮助信息"
    echo ""
    echo "示例:"
    echo "  ./test_framework.sh --list-plugins"
    echo "  ./test_framework.sh --list dgiot_modbus"
    echo "  ./test_framework.sh --run dgiot_modbus simple"
    echo "  ./test_framework.sh --all dgiot_modbus"
    echo ""
    echo "快速命令:"
    echo "  ./test_framework.sh modbus              # 测试所有Modbus用例"
    echo "  ./test_framework.sh modbus simple       # 测试Modbus简化用例"
    echo "  ./test_framework.sh modbus register     # 测试Modbus注册用例"
}

# 主函数
main() {
    case "$1" in
        --list-plugins)
            list_plugins
            ;;
        --list)
            if [ -z "$2" ]; then
                log_error "需要指定插件名"
                show_help
                exit 1
            fi
            list_testcases "$2"
            ;;
        --run)
            if [ -z "$2" ] || [ -z "$3" ]; then
                log_error "需要指定插件名和测试用例名"
                show_help
                exit 1
            fi
            run_testcase "$2" "$3"
            ;;
        --all)
            if [ -z "$2" ]; then
                log_error "需要指定插件名"
                show_help
                exit 1
            fi
            run_all_testcases "$2"
            ;;
        --help|-h)
            show_help
            ;;
        modbus)
            if [ -z "$2" ]; then
                # 如果没有指定测试用例，执行所有
                run_all_testcases "dgiot_modbus"
            else
                # 映射常用测试用例名
                case "$2" in
                    simple)
                        run_testcase "dgiot_modbus" "simple"
                        ;;
                    register)
                        run_testcase "dgiot_modbus" "registerbyport"
                        ;;
                    simulator)
                        run_testcase "dgiot_modbus" "simulator_complete"
                        ;;
                    *)
                        run_testcase "dgiot_modbus" "$2"
                        ;;
                esac
            fi
            ;;
        *)
            log_error "未知命令: $1"
            show_help
            exit 1
            ;;
    esac
}

# 如果没有参数，显示帮助
if [ $# -eq 0 ]; then
    show_help
    exit 0
fi

main "$@"
