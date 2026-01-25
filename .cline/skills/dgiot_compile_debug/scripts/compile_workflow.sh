#!/bin/bash
# compile_workflow.sh - 智能编译调试工作流

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# 导入插件识别脚本
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/detect_plugin.sh"

# 日志函数
log_step() {
    echo -e "\n${CYAN}=== $1 ===${NC}"
}

log_action() {
    echo -e "${MAGENTA}➜ $1${NC}"
}

log_result() {
    echo -e "${GREEN}✓ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

log_error() {
    echo -e "${RED}✗ $1${NC}"
}

# 检查命令是否存在
check_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        log_error "命令 '$1' 不存在"
        return 1
    fi
    return 0
}

# 检查DGIOT环境
check_dgiot_environment() {
    log_step "检查DGIOT环境"
    
    if [[ ! -d "/root/gitee/dgiot" ]]; then
        log_error "DGIOT项目目录不存在: /root/gitee/dgiot"
        return 1
    fi
    
    if [[ ! -f "/root/gitee/dgiot/rebar3" ]]; then
        log_error "rebar3不存在"
        return 1
    fi
    
    if [[ ! -d "/root/gitee/dgiot/_build/emqx/rel/emqx/bin" ]]; then
        log_warning "EMQX构建目录不存在，可能需要先运行全量编译"
    fi
    
    log_result "DGIOT环境检查通过"
    return 0
}

# 智能热编译
hot_compile() {
    local plugin_name="$1"
    local verbose="${2:-false}"
    
    log_step "智能热编译"
    log_action "识别当前插件..."
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "热编译插件: $plugin_name"
    
    local compile_cmd="dgiot_plugin:compile($plugin_name)"
    if [[ "$verbose" == "true" ]]; then
        compile_cmd="dgiot_plugin:compile($plugin_name, verbose)"
    fi
    
    if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
        log_action "执行编译命令: $compile_cmd"
        /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "$compile_cmd"
        local result=$?
        
        if [[ $result -eq 0 ]]; then
            log_result "热编译成功: $plugin_name"
            return 0
        else
            log_error "热编译失败: $plugin_name"
            return 1
        fi
    else
        log_warning "EMQX二进制不存在，尝试使用rebar3编译"
        cd /root/gitee/dgiot && ./rebar3 compile
        return $?
    fi
}

# 智能热加载
hot_reload() {
    local plugin_name="$1"
    
    log_step "智能热加载"
    log_action "识别当前插件..."
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "热加载插件: $plugin_name"
    
    if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
        log_action "执行热加载命令: dgiot_plugin:reload_plugin($plugin_name)"
        /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:reload_plugin($plugin_name)"
        local result=$?
        
        if [[ $result -eq 0 ]]; then
            log_result "热加载成功: $plugin_name"
            return 0
        else
            log_error "热加载失败: $plugin_name"
            return 1
        fi
    else
        log_warning "EMQX二进制不存在，无法热加载"
        return 1
    fi
}

# 智能运行测试
run_tests() {
    local plugin_name="$1"
    local test_type="${2:-all}"
    
    log_step "智能运行测试"
    log_action "识别当前插件..."
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "运行测试: $plugin_name ($test_type)"
    
    local test_cmd=""
    case "$test_type" in
        "unit")
            test_cmd="${plugin_name}_test:unit_test_suite()."
            ;;
        "integration")
            test_cmd="${plugin_name}_test:integration_test_suite()."
            ;;
        "performance")
            test_cmd="${plugin_name}_test:performance_test()."
            ;;
        "parse")
            test_cmd="${plugin_name}_test:test_parse_packet()."
            ;;
        "all"|*)
            test_cmd="${plugin_name}_test:test_suite()."
            ;;
    esac
    
    if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
        log_action "执行测试命令: $test_cmd"
        /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "$test_cmd"
        local result=$?
        
        if [[ $result -eq 0 ]]; then
            log_result "测试成功: $plugin_name"
            return 0
        else
            log_error "测试失败: $plugin_name"
            return 1
        fi
    else
        log_warning "EMQX二进制不存在，无法运行测试"
        return 1
    fi
}

# 全量编译
full_compile() {
    local clean="${1:-false}"
    local jobs="${2:-4}"
    
    log_step "全量编译"
    
    cd /root/gitee/dgiot || return 1
    
    if [[ "$clean" == "true" ]]; then
        log_action "清理构建..."
        ./rebar3 clean
    fi
    
    log_action "开始全量编译 (jobs: $jobs)..."
    
    if [[ "$jobs" -gt 1 ]]; then
        ./rebar3 compile -j "$jobs"
    else
        ./rebar3 compile
    fi
    
    local result=$?
    
    if [[ $result -eq 0 ]]; then
        log_result "全量编译成功"
        return 0
    else
        log_error "全量编译失败"
        return 1
    fi
}

# 完整工作流
complete_workflow() {
    local plugin_name="$1"
    
    log_step "开始完整编译调试工作流"
    
    # 1. 检查环境
    check_dgiot_environment || return 1
    
    # 2. 识别插件
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    log_action "工作流插件: $plugin_name"
    
    # 3. 热编译
    hot_compile "$plugin_name" || {
        log_warning "热编译失败，尝试全量编译..."
        full_compile false 4 || return 1
    }
    
    # 4. 热加载
    hot_reload "$plugin_name" || {
        log_warning "热加载失败，继续执行..."
    }
    
    # 5. 运行测试
    run_tests "$plugin_name" "all" || {
        log_warning "测试失败，继续执行..."
    }
    
    log_result "完整工作流执行完成"
    return 0
}

# 快速开发迭代工作流
quick_development_workflow() {
    local plugin_name="$1"
    
    log_step "快速开发迭代工作流"
    
    # 识别插件
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    log_action "开发插件: $plugin_name"
    
    # 热编译
    log_action "步骤1: 热编译验证"
    hot_compile "$plugin_name" true || return 1
    
    # 热加载
    log_action "步骤2: 热加载测试"
    hot_reload "$plugin_name" || return 1
    
    # 运行测试
    log_action "步骤3: 运行测试"
    run_tests "$plugin_name" "unit" || {
        log_warning "单元测试失败，但继续执行..."
    }
    
    log_result "快速开发迭代完成"
    return 0
}

# 调试工作流
debug_workflow() {
    local plugin_name="$1"
    local module_name="$2"
    
    log_step "调试工作流"
    
    # 识别插件
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    log_action "调试插件: $plugin_name"
    
    if [[ -n "$module_name" ]]; then
        log_action "调试模块: $module_name"
    fi
    
    # 热编译
    log_action "步骤1: 热编译定位问题"
    hot_compile "$plugin_name" true || {
        log_error "编译失败，请检查代码"
        return 1
    }
    
    # 热加载特定模块
    if [[ -n "$module_name" ]]; then
        log_action "步骤2: 热加载模块 $module_name"
        if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
            /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "dgiot_plugin:reload_module($module_name)."
        fi
    else
        # 热加载整个插件
        log_action "步骤2: 热加载插件"
        hot_reload "$plugin_name" || return 1
    fi
    
    # 运行相关测试
    log_action "步骤3: 运行相关测试"
    if [[ -n "$module_name" ]]; then
        # 尝试运行模块相关测试
        local test_func=$(echo "$module_name" | sed 's/^dgiot_//' | sed 's/_channel$//' | sed 's/_protocol$//')
        run_tests "$plugin_name" "$test_func" || true
    else
        run_tests "$plugin_name" "all" || true
    fi
    
    log_result "调试工作流完成"
    return 0
}

# 显示帮助信息
show_help() {
    echo -e "${CYAN}DGIOT编译调试工作流工具${NC}"
    echo ""
    echo -e "${GREEN}用法:${NC} $0 [命令] [选项]"
    echo ""
    echo -e "${YELLOW}命令:${NC}"
    echo "  complete       完整工作流 (检查环境 → 热编译 → 热加载 → 测试)"
    echo "  quick          快速开发迭代工作流 (热编译 → 热加载 → 单元测试)"
    echo "  debug          调试工作流 (热编译 → 热加载模块 → 测试)"
    echo "  compile        智能热编译"
    echo "  reload         智能热加载"
    echo "  test           智能运行测试"
    echo "  full           全量编译"
    echo "  detect         识别当前插件"
    echo "  help           显示帮助信息"
    echo ""
    echo -e "${YELLOW}选项:${NC}"
    echo "  --plugin NAME  指定插件名称 (默认自动识别)"
    echo "  --module NAME  指定模块名称 (用于调试工作流)"
    echo "  --type TYPE    测试类型: unit, integration, performance, parse, all"
    echo "  --clean        清理后编译 (用于全量编译)"
    echo "  --jobs N       并行编译任务数 (默认: 4)"
    echo "  --verbose      详细输出"
    echo ""
    echo -e "${YELLOW}示例:${NC}"
    echo "  $0 complete"
    echo "  $0 quick --plugin dgiot_uav"
    echo "  $0 debug --module dgiot_uav_channel"
    echo "  $0 compile --verbose"
    echo "  $0 test --type unit"
    echo "  $0 full --clean --jobs 8"
}

# 解析命令行参数
parse_arguments() {
    local command=""
    local options=()
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            complete|quick|debug|compile|reload|test|full|detect|help)
                command="$1"
                shift
                ;;
            --plugin)
                PLUGIN_NAME="$2"
                shift 2
                ;;
            --module)
                MODULE_NAME="$2"
                shift 2
                ;;
            --type)
                TEST_TYPE="$2"
                shift 2
                ;;
            --clean)
                CLEAN_BUILD="true"
                shift
                ;;
            --jobs)
                JOBS="$2"
                shift 2
                ;;
            --verbose)
                VERBOSE="true"
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                log_error "未知参数: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    echo "$command"
}

# 主函数
main() {
    local command="$1"
    
    # 如果没有指定命令，显示帮助
    if [[ -z "$command" ]]; then
        show_help
        exit 0
    fi
    
    # 检查DGIOT环境
    check_dgiot_environment || exit 1
    
    # 执行命令
    case "$command" in
        "complete")
            complete_workflow "$PLUGIN_NAME"
            ;;
        "quick")
            quick_development_workflow "$PLUGIN_NAME"
            ;;
        "debug")
            debug_workflow "$PLUGIN_NAME" "$MODULE_NAME"
            ;;
        "compile")
            hot_compile "$PLUGIN_NAME" "$VERBOSE"
            ;;
        "reload")
            hot_reload "$PLUGIN_NAME"
            ;;
        "test")
            run_tests "$PLUGIN_NAME" "$TEST_TYPE"
            ;;
        "full")
            full_compile "$CLEAN_BUILD" "$JOBS"
            ;;
        "detect")
            detect_plugin
            ;;
        "help")
            show_help
            ;;
        *)
            log_error "未知命令: $command"
            show_help
            exit 1
            ;;
    esac
    
    local result=$?
    echo ""
    
    if [[ $result -eq 0 ]]; then
        log_result "命令执行成功: $command"
    else
        log_error "命令执行失败: $command"
    fi
    
    exit $result
}

# 全局变量
PLUGIN_NAME=""
MODULE_NAME=""
TEST_TYPE="all"
CLEAN_BUILD="false"
JOBS="4"
VERBOSE="false"

# 执行主函数
main "$@"
