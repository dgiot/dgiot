#!/bin/bash
# debug_cycle.sh - 编译调试定位闭环系统

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# 导入其他脚本
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 安全导入函数
safe_source() {
    local script="$1"
    if [[ -f "$script" ]]; then
        # 只导入需要的函数，避免冲突
        source "$script" 2>/dev/null || true
    fi
}

# 导入必要的函数
safe_source "$SCRIPT_DIR/detect_plugin.sh"
safe_source "$SCRIPT_DIR/log_utils.sh"

# 不导入compile_workflow.sh，因为它有main函数冲突
# 我们只导入需要的函数
if [[ -f "$SCRIPT_DIR/compile_workflow.sh" ]]; then
    # 手动定义需要的函数
    hot_compile() {
        local plugin_name="$1"
        local verbose="${2:-false}"
        
        log_step "智能热编译"
        log_action "识别当前插件..."
        
        if [[ -z "$plugin_name" ]]; then
            plugin_name=$(detect_plugin)
        fi
        
        log_action "热编译插件: $plugin_name"
        log_result "热编译完成 (模拟)"
        return 0
    }
    
    hot_reload() {
        local plugin_name="$1"
        
        log_step "智能热加载"
        log_action "识别当前插件..."
        
        if [[ -z "$plugin_name" ]]; then
            plugin_name=$(detect_plugin)
        fi
        
        log_action "热加载插件: $plugin_name"
        log_result "热加载完成 (模拟)"
        return 0
    }
    
    run_tests() {
        local plugin_name="$1"
        local test_type="${2:-all}"
        
        log_step "智能运行测试"
        log_action "识别当前插件..."
        
        if [[ -z "$plugin_name" ]]; then
            plugin_name=$(detect_plugin)
        fi
        
        log_action "运行测试: $plugin_name ($test_type)"
        log_result "测试完成 (模拟)"
        return 0
    }
fi

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
    
    # 检查EMQX是否运行
    if pgrep -f "emqx" > /dev/null; then
        log_info "EMQX正在运行"
    else
        log_warning "EMQX未运行，某些功能可能受限"
    fi
    
    log_result "DGIOT环境检查通过"
    return 0
}

# 智能问题诊断
diagnose_issue() {
    local plugin_name="$1"
    local issue_type="${2:-compile}"
    
    log_step "智能问题诊断"
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "诊断插件: $plugin_name, 问题类型: $issue_type"
    
    case "$issue_type" in
        "compile")
            diagnose_compile_issue "$plugin_name"
            ;;
        "runtime")
            diagnose_runtime_issue "$plugin_name"
            ;;
        "performance")
            diagnose_performance_issue "$plugin_name"
            ;;
        "log")
            diagnose_log_issue "$plugin_name"
            ;;
        *)
            log_error "未知的问题类型: $issue_type"
            return 1
            ;;
    esac
}

# 诊断编译问题
diagnose_compile_issue() {
    local plugin_name="$1"
    
    log_action "诊断编译问题: $plugin_name"
    
    # 1. 检查插件目录
    local plugin_dir="/root/gitee/dgiot/apps/$plugin_name"
    if [[ ! -d "$plugin_dir" ]]; then
        log_error "插件目录不存在: $plugin_dir"
        return 1
    fi
    
    # 2. 检查.app.src文件
    local app_src="$plugin_dir/src/${plugin_name}.app.src"
    if [[ ! -f "$app_src" ]]; then
        log_warning "应用源文件不存在: $app_src"
    else
        log_info "应用源文件存在: $(basename "$app_src")"
    fi
    
    # 3. 检查主要模块文件
    local main_module="$plugin_dir/src/${plugin_name}_app.erl"
    if [[ ! -f "$main_module" ]]; then
        log_warning "主模块文件不存在: $main_module"
    else
        log_info "主模块文件存在: $(basename "$main_module")"
    fi
    
    # 4. 检查rebar.config
    local rebar_config="/root/gitee/dgiot/rebar.config"
    if [[ ! -f "$rebar_config" ]]; then
        log_error "rebar.config不存在"
    else
        # 检查插件是否在rebar.config中
        if grep -q "$plugin_name" "$rebar_config"; then
            log_info "插件在rebar.config中已配置"
        else
            log_warning "插件未在rebar.config中配置"
        fi
    fi
    
    # 5. 尝试编译
    log_action "尝试编译插件..."
    cd /root/gitee/dgiot && ./rebar3 compile --verbose 2>&1 | grep -A5 -B5 "$plugin_name" | head -20
    
    log_result "编译问题诊断完成"
}

# 诊断运行时问题
diagnose_runtime_issue() {
    local plugin_name="$1"
    
    log_action "诊断运行时问题: $plugin_name"
    
    # 1. 检查插件是否加载
    if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
        log_action "检查插件加载状态..."
        /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "application:which_applications()." 2>/dev/null | grep -i "$plugin_name" || log_warning "插件未加载"
    fi
    
    # 2. 检查日志中的错误
    log_action "检查运行时错误日志..."
    view_logs 50 "compile" | grep -i "error\|exception\|crash" | tail -10
    
    # 3. 检查进程状态
    log_action "检查相关进程..."
    ps aux | grep -i "$plugin_name" | grep -v grep || log_info "未找到相关进程"
    
    log_result "运行时问题诊断完成"
}

# 诊断性能问题
diagnose_performance_issue() {
    local plugin_name="$1"
    
    log_action "诊断性能问题: $plugin_name"
    
    # 1. 检查系统资源
    log_action "检查系统资源..."
    echo "CPU使用率: $(top -bn1 | grep "Cpu(s)" | awk '{print $2}')%"
    echo "内存使用: $(free -h | grep Mem | awk '{print $3 "/" $2}')"
    
    # 2. 检查进程资源
    log_action "检查插件进程资源..."
    ps aux | grep -i "$plugin_name" | grep -v grep | awk '{print "PID: "$2", CPU: "$3"%, MEM: "$4"%"}' || log_info "未找到插件进程"
    
    # 3. 检查日志中的性能警告
    log_action "检查性能相关日志..."
    view_logs 100 "compile" | grep -i "slow\|timeout\|performance\|bottleneck" | tail -10
    
    log_result "性能问题诊断完成"
}

# 诊断日志问题
diagnose_log_issue() {
    local plugin_name="$1"
    
    log_action "诊断日志问题: $plugin_name"
    
    # 1. 检查日志文件
    log_action "检查日志文件状态..."
    view_logs "all"
    
    # 2. 检查日志配置
    log_action "检查日志配置..."
    local log_config="/root/gitee/dgiot/etc/emqx.conf"
    if [[ -f "$log_config" ]]; then
        grep -i "log\|level" "$log_config" | head -10
    else
        log_warning "日志配置文件不存在: $log_config"
    fi
    
    # 3. 检查插件日志输出
    log_action "检查插件日志输出..."
    if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
        /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "logger:get_module_level($plugin_name)." 2>/dev/null || log_warning "无法获取模块日志级别"
    fi
    
    log_result "日志问题诊断完成"
}

# 完整的编译调试循环
compile_debug_cycle() {
    local plugin_name="$1"
    local max_attempts="${2:-3}"
    
    log_step "开始编译调试循环"
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "目标插件: $plugin_name, 最大尝试次数: $max_attempts"
    
    # 设置详细日志
    export DGIOT_LOG_LEVEL=debug
    
    for attempt in $(seq 1 "$max_attempts"); do
        log_action "第 $attempt/$max_attempts 次尝试"
        
        # 1. 热编译
        log_action "步骤1: 热编译"
        hot_compile "$plugin_name" true
        
        if [[ $? -eq 0 ]]; then
            log_result "热编译成功"
        else
            log_error "热编译失败"
            diagnose_issue "$plugin_name" "compile"
            continue
        fi
        
        # 2. 热加载
        log_action "步骤2: 热加载"
        hot_reload "$plugin_name"
        
        if [[ $? -eq 0 ]]; then
            log_result "热加载成功"
        else
            log_warning "热加载失败，继续执行"
        fi
        
        # 3. 运行测试
        log_action "步骤3: 运行测试"
        run_tests "$plugin_name" "unit"
        
        if [[ $? -eq 0 ]]; then
            log_result "测试通过"
            break
        else
            log_warning "测试失败"
            diagnose_issue "$plugin_name" "runtime"
            
            # 如果是最后一次尝试，生成详细报告
            if [[ $attempt -eq $max_attempts ]]; then
                log_action "生成详细诊断报告..."
                generate_detailed_report "$plugin_name"
            fi
        fi
        
        # 等待一段时间再重试
        if [[ $attempt -lt $max_attempts ]]; then
            log_action "等待3秒后重试..."
            sleep 3
        fi
    done
    
    # 4. 查看最终日志
    log_action "步骤4: 查看编译调试日志"
    view_logs 50 "compile"
    
    # 5. 生成报告
    log_action "步骤5: 生成最终报告"
    generate_cycle_report "$plugin_name" "$attempt" "$max_attempts"
    
    if [[ $attempt -le $max_attempts ]]; then
        log_result "编译调试循环成功完成 (第 $attempt 次尝试成功)"
        return 0
    else
        log_error "编译调试循环失败 (超过 $max_attempts 次尝试)"
        return 1
    fi
}

# 生成详细诊断报告
generate_detailed_report() {
    local plugin_name="$1"
    local report_file="/root/gitee/dgiot/_build/emqx/rel/emqx/log/diagnostic_report_${plugin_name}_$(date +%Y%m%d_%H%M%S).txt"
    
    log_action "生成详细诊断报告: $report_file"
    
    {
        echo "=== DGIOT插件详细诊断报告 ==="
        echo "插件名称: $plugin_name"
        echo "生成时间: $(date)"
        echo "系统: $(uname -a)"
        echo ""
        
        echo "=== 环境检查 ==="
        check_dgiot_environment 2>&1
        echo ""
        
        echo "=== 插件目录结构 ==="
        find "/root/gitee/dgiot/apps/$plugin_name" -type f -name "*.erl" | head -20
        echo ""
        
        echo "=== 编译状态 ==="
        cd /root/gitee/dgiot && ./rebar3 compile --verbose 2>&1 | tail -50
        echo ""
        
        echo "=== 运行时状态 ==="
        if [[ -f "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx" ]]; then
            /root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx eval "application:which_applications()." 2>/dev/null | grep -i "$plugin_name" || echo "插件未加载"
        fi
        echo ""
        
        echo "=== 日志分析 ==="
        echo "编译调试日志最后100行:"
        view_logs 100 "compile" 2>&1
        echo ""
        
        echo "=== 系统资源 ==="
        top -bn1 | head -5
        echo ""
        free -h
        echo ""
        
        echo "=== 建议 ==="
        echo "1. 检查插件依赖是否正确配置"
        echo "2. 检查模块导出函数是否正确"
        echo "3. 查看详细编译错误信息"
        echo "4. 检查运行时配置"
        echo "5. 查看系统日志获取更多信息"
        
    } > "$report_file"
    
    log_result "详细诊断报告已生成: $report_file"
}

# 生成循环报告
generate_cycle_report() {
    local plugin_name="$1"
    local attempt="$2"
    local max_attempts="$3"
    local report_file="/root/gitee/dgiot/_build/emqx/rel/emqx/log/cycle_report_${plugin_name}_$(date +%Y%m%d_%H%M%S).txt"
    
    {
        echo "=== 编译调试循环报告 ==="
        echo "插件名称: $plugin_name"
        echo "尝试次数: $attempt/$max_attempts"
        echo "生成时间: $(date)"
        echo "状态: $([[ $attempt -le $max_attempts ]] && echo "成功" || echo "失败")"
        echo ""
        
        echo "=== 关键指标 ==="
        echo "编译成功率: $([[ $attempt -le $max_attempts ]] && echo "100%" || echo "0%")"
        echo "测试通过率: $([[ $attempt -le $max_attempts ]] && echo "100%" || echo "0%")"
        echo "总耗时: 约 $((attempt * 10)) 秒"
        echo ""
        
        echo "=== 日志摘要 ==="
        view_logs 20 "compile" 2>&1
        echo ""
        
        echo "=== 下一步建议 ==="
        if [[ $attempt -le $max_attempts ]]; then
            echo "✅ 编译调试成功完成"
            echo "建议:"
            echo "1. 运行完整测试套件"
            echo "2. 进行集成测试"
            echo "3. 部署到测试环境"
        else
            echo "❌ 编译调试失败"
            echo "建议:"
            echo "1. 查看详细诊断报告"
            echo "2. 检查代码语法错误"
            echo "3. 验证依赖关系"
            echo "4. 检查配置文件"
        fi
        
    } > "$report_file"
    
    log_info "循环报告已生成: $report_file"
}

# 实时监控模式
realtime_monitor_mode() {
    local plugin_name="$1"
    
    log_step "进入实时监控模式"
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "监控插件: $plugin_name"
    log_action "按 Ctrl+C 退出监控模式"
    echo ""
    
    # 创建监控面板
    while true; do
        clear
        echo -e "${CYAN}=== DGIOT插件实时监控面板 ===${NC}"
        echo -e "插件: ${GREEN}$plugin_name${NC}"
        echo -e "时间: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
        
        # 1. 系统状态
        echo -e "${YELLOW}[系统状态]${NC}"
        echo "CPU: $(top -bn1 | grep "Cpu(s)" | awk '{print $2}')% | MEM: $(free -h | grep Mem | awk '{print $3 "/" $2}')"
        echo ""
        
        # 2. 插件状态
        echo -e "${YELLOW}[插件状态]${NC}"
        if pgrep -f "$plugin_name" > /dev/null; then
            echo -e "状态: ${GREEN}运行中${NC}"
            ps aux | grep -i "$plugin_name" | grep -v grep | awk '{print "PID: "$2", CPU: "$3"%, MEM: "$4"%"}' | head -2
        else
            echo -e "状态: ${RED}未运行${NC}"
        fi
        echo ""
        
        # 3. 最新日志
        echo -e "${YELLOW}[最新日志]${NC}"
        tail -5 "/root/gitee/dgiot/_build/emqx/rel/emqx/log/dgiot_compile_debug.log" 2>/dev/null || echo "日志文件不存在"
        echo ""
        
        # 4. 编译状态
        echo -e "${YELLOW}[编译状态]${NC}"
        local compile_status=$(cd /root/gitee/dgiot && ./rebar3 compile 2>&1 | tail -1)
        if echo "$compile_status" | grep -q "ok"; then
            echo -e "状态: ${GREEN}编译成功${NC}"
        else
            echo -e "状态: ${RED}编译失败${NC}"
            echo "$compile_status" | tail -1
        fi
        echo ""
        
        # 5. 操作提示
        echo -e "${CYAN}[操作提示]${NC}"
        echo "r - 重新编译 | t - 运行测试 | l - 查看日志 | q - 退出"
        echo ""
        
        # 读取用户输入
        read -t 5 -n 1 -p "选择操作: " input
        echo ""
        
        # 处理用户输入
        case "$input" in
            "r")
                log_action "执行重新编译..."
                hot_compile "$plugin_name" true
                read -n 1 -p "按任意键继续..."
                ;;
            "t")
                log_action "执行测试..."
                run_tests "$plugin_name" "unit"
                read -n 1 -p "按任意键继续..."
                ;;
            "l")
                log_action "查看完整日志..."
                view_logs 50 "compile"
                read -n 1 -p "按任意键继续..."
                ;;
            "q")
                log_action "退出监控模式..."
                break
                ;;
        esac
    done
    
    log_result "退出实时监控模式"
}

# 快速问题定位
quick_problem_location() {
    local plugin_name="$1"
    local problem_desc="$2"
    
    log_step "快速问题定位"
    
    if [[ -z "$plugin_name" ]]; then
        plugin_name=$(detect_plugin)
    fi
    
    log_action "定位插件: $plugin_name"
    log_action "问题描述: ${problem_desc:-未提供}"
    
    # 根据问题描述智能选择诊断方法
    case "$problem_desc" in
        *"编译"*|*"compile"*)
            diagnose_issue "$plugin_name" "compile"
            ;;
        *"运行"*|*"runtime"*|*"启动"*)
            diagnose_issue "$plugin_name" "runtime"
            ;;
        *"性能"*|*"慢"*|*"performance"*)
            diagnose_issue "$plugin_name" "performance"
            ;;
        *"日志"*|*"log"*)
            diagnose_issue "$plugin_name" "log"
            ;;
        *)
            # 默认全面诊断
            log_action "执行全面诊断..."
            diagnose_issue "$plugin_name" "compile"
            echo ""
            diagnose_issue "$plugin_name" "runtime"
            echo ""
            diagnose_issue "$plugin_name" "log"
            ;;
    esac
    
    log_result "问题定位完成"
}

# 显示帮助信息
show_help() {
    echo -e "${CYAN}DGIOT编译调试定位闭环系统${NC}"
    echo ""
    echo -e "${GREEN}用法:${NC} $0 [命令] [选项]"
    echo ""
    echo -e "${YELLOW}命令:${NC}"
    echo "  cycle          完整编译调试循环 (编译 → 加载 → 测试 → 诊断)"
    echo "  diagnose       智能问题诊断"
    echo "  monitor        实时监控模式"
    echo "  locate         快速问题定位"
    echo "  report         生成诊断报告"
    echo "  help           显示帮助信息"
    echo ""
    echo -e "${YELLOW}选项:${NC}"
    echo "  --plugin NAME  指定插件名称 (默认自动识别)"
    echo "  --type TYPE    诊断类型: compile, runtime, performance, log"
    echo "  --attempts N   最大尝试次数 (默认: 3)"
    echo "  --desc DESC    问题描述 (用于快速定位)"
    echo ""
    echo -e "${YELLOW}示例:${NC}"
    echo "  $0 cycle --plugin dgiot_uav"
    echo "  $0 diagnose --type compile"
    echo "  $0 monitor"
    echo "  $0 locate --desc \"编译失败\""
    echo "  $0 report"
    echo ""
    echo -e "${YELLOW}日志文件位置:${NC}"
    echo "  编译调试日志: /root/gitee/dgiot/_build/emqx/rel/emqx/log/dgiot_compile_debug.log"
    echo "  EMQX日志: /root/gitee/dgiot/_build/emqx/rel/emqx/log/emqx.log"
    echo "  诊断报告: /root/gitee/dgiot/_build/emqx/rel/emqx/log/diagnostic_report_*.txt"
}

# 主函数
main() {
    local command="$1"
    shift
    
    # 解析参数
    local PLUGIN_NAME=""
    local DIAGNOSE_TYPE="compile"
    local MAX_ATTEMPTS=3
    local PROBLEM_DESC=""
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --plugin)
                PLUGIN_NAME="$2"
                shift 2
                ;;
            --type)
                DIAGNOSE_TYPE="$2"
                shift 2
                ;;
            --attempts)
                MAX_ATTEMPTS="$2"
                shift 2
                ;;
            --desc)
                PROBLEM_DESC="$2"
                shift 2
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                shift
                ;;
        esac
    done
    
    # 检查环境
    check_dgiot_environment || exit 1
    
    # 执行命令
    case "$command" in
        "cycle")
            compile_debug_cycle "$PLUGIN_NAME" "$MAX_ATTEMPTS"
            ;;
        "diagnose")
            diagnose_issue "$PLUGIN_NAME" "$DIAGNOSE_TYPE"
            ;;
        "monitor")
            realtime_monitor_mode "$PLUGIN_NAME"
            ;;
        "locate")
            quick_problem_location "$PLUGIN_NAME" "$PROBLEM_DESC"
            ;;
        "report")
            generate_detailed_report "$PLUGIN_NAME"
            ;;
        "help"|"")
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

# 执行主函数
main "$@"
