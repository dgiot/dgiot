#!/bin/bash
# main.sh - DGIOT编译调试技能主入口

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# 技能根目录
SKILL_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG_DIR="$SKILL_ROOT/config"
SCRIPTS_DIR="$SKILL_ROOT/scripts"

# 导入配置系统
source "$CONFIG_DIR/config.sh" 2>/dev/null || {
    echo -e "${RED}错误: 无法加载配置系统${NC}"
    echo "请确保配置系统已正确安装: $CONFIG_DIR/config.sh"
    exit 1
}

# 初始化配置
init_config

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

# 检查环境
check_environment() {
    log_step "检查环境"
    
    # 检查配置健康状态
    if ! check_config_health; then
        log_warning "配置存在警告，某些功能可能受限"
    fi
    
    # 检查必要工具
    local missing_tools=()
    
    for tool in rebar3 git grep find ps; do
        if ! command -v "$tool" >/dev/null 2>&1; then
            missing_tools+=("$tool")
        fi
    done
    
    if [[ ${#missing_tools[@]} -gt 0 ]]; then
        log_warning "缺少必要工具: ${missing_tools[*]}"
    fi
    
    log_result "环境检查完成"
}

# 显示技能信息
show_skill_info() {
    log_step "DGIOT编译调试技能"
    
    echo -e "${YELLOW}技能名称:${NC} $(get_config "skill.name")"
    echo -e "${YELLOW}版本:${NC} $(get_config "skill.version")"
    echo -e "${YELLOW}作者:${NC} $(get_config "skill.author")"
    echo -e "${YELLOW}描述:${NC} $(get_config "skill.description")"
    echo ""
    
    echo -e "${CYAN}可用脚本:${NC}"
    echo "  detect_plugin.sh    - 智能插件识别"
    echo "  log_utils.sh        - 标准日志工具"
    echo "  compile_workflow.sh - 编译调试工作流"
    echo "  debug_cycle.sh      - 编译调试定位闭环"
    echo ""
    
    echo -e "${CYAN}配置文件:${NC}"
    echo "  $CONFIG_DIR/skill_config.json"
    echo "  $CONFIG_DIR/.env"
    echo ""
}

# 运行脚本
run_script() {
    local script_name="$1"
    shift
    
    local script_path="$SCRIPTS_DIR/$script_name"
    
    if [[ ! -f "$script_path" ]]; then
        log_error "脚本不存在: $script_name"
        return 1
    fi
    
    if [[ ! -x "$script_path" ]]; then
        chmod +x "$script_path"
    fi
    
    log_action "运行脚本: $script_name"
    "$script_path" "$@"
}

# 智能插件识别
detect_plugin() {
    run_script "detect_plugin.sh" "$@"
}

# 标准日志工具
log_utils() {
    run_script "log_utils.sh" "$@"
}

# 编译工作流
compile_workflow() {
    run_script "compile_workflow.sh" "$@"
}

# 调试循环
debug_cycle() {
    run_script "debug_cycle.sh" "$@"
}

# 配置管理
config_manager() {
    local command="$1"
    shift
    
    case "$command" in
        "init"|"show"|"wizard"|"health"|"reset"|"export"|"import"|"help")
            source "$CONFIG_DIR/config.sh" "$command" "$@"
            ;;
        "set")
            if [[ $# -lt 2 ]]; then
                log_error "使用方法: config set <键> <值>"
                return 1
            fi
            source "$CONFIG_DIR/config.sh" "set" "$1" "$2"
            ;;
        "get")
            if [[ $# -lt 1 ]]; then
                log_error "使用方法: config get <键>"
                return 1
            fi
            source "$CONFIG_DIR/config.sh" "get" "$1"
            ;;
        *)
            log_error "未知配置命令: $command"
            source "$CONFIG_DIR/config.sh" "help"
            return 1
            ;;
    esac
}

# 显示帮助信息
show_help() {
    echo -e "${CYAN}DGIOT编译调试技能 - 主入口${NC}"
    echo ""
    echo -e "${GREEN}用法:${NC} $0 [模块] [命令] [选项]"
    echo ""
    echo -e "${YELLOW}模块:${NC}"
    echo "  detect         智能插件识别"
    echo "  log            标准日志工具"
    echo "  compile        编译调试工作流"
    echo "  debug          编译调试定位闭环"
    echo "  config         配置管理"
    echo "  info           显示技能信息"
    echo "  help           显示帮助信息"
    echo ""
    echo -e "${YELLOW}示例:${NC}"
    echo "  $0 detect test              # 测试插件识别"
    echo "  $0 log test_logging         # 测试日志功能"
    echo "  $0 compile complete         # 运行完整编译工作流"
    echo "  $0 debug diagnose --type compile  # 诊断编译问题"
    echo "  $0 config show              # 显示当前配置"
    echo "  $0 config wizard            # 交互式配置向导"
    echo "  $0 config set dgiot.project_root /new/path  # 设置配置"
    echo "  $0 info                     # 显示技能信息"
    echo ""
    echo -e "${YELLOW}直接运行脚本:${NC}"
    echo "  $SCRIPTS_DIR/detect_plugin.sh [命令]"
    echo "  $SCRIPTS_DIR/log_utils.sh [命令]"
    echo "  $SCRIPTS_DIR/compile_workflow.sh [命令]"
    echo "  $SCRIPTS_DIR/debug_cycle.sh [命令]"
    echo ""
    echo -e "${YELLOW}环境配置:${NC}"
    echo "  所有配置都存储在: $CONFIG_DIR/"
    echo "  无硬编码路径，完全可配置"
}

# 主函数
main() {
    local module="$1"
    shift
    
    # 检查环境
    check_environment
    
    case "$module" in
        "detect")
            detect_plugin "$@"
            ;;
        "log")
            log_utils "$@"
            ;;
        "compile")
            compile_workflow "$@"
            ;;
        "debug")
            debug_cycle "$@"
            ;;
        "config")
            config_manager "$@"
            ;;
        "info")
            show_skill_info
            ;;
        "help"|"")
            show_help
            ;;
        *)
            # 尝试直接运行脚本
            if [[ -f "$SCRIPTS_DIR/$module" ]]; then
                run_script "$module" "$@"
            else
                log_error "未知模块: $module"
                show_help
                return 1
            fi
            ;;
    esac
    
    local result=$?
    echo ""
    
    if [[ $result -eq 0 ]]; then
        log_result "操作完成"
    else
        log_error "操作失败"
    fi
    
    return $result
}

# 执行主函数
main "$@"
