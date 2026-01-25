#!/bin/bash
# config.sh - DGIOT编译调试技能配置系统

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# 配置文件目录
CONFIG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SKILL_ROOT="$(dirname "$CONFIG_DIR")"
CONFIG_FILE="$CONFIG_DIR/skill_config.json"
ENV_FILE="$CONFIG_DIR/.env"

# 默认配置
DEFAULT_CONFIG='{
  "dgiot": {
    "project_root": "/root/gitee/dgiot",
    "rebar3_path": "/root/gitee/dgiot/rebar3",
    "emqx_bin_path": "/root/gitee/dgiot/_build/emqx/rel/emqx/bin/emqx",
    "log_dir": "/root/gitee/dgiot/_build/emqx/rel/emqx/log",
    "apps_dir": "/root/gitee/dgiot/apps",
    "etc_dir": "/root/gitee/dgiot/etc"
  },
  "skill": {
    "name": "dgiot_compile_debug",
    "version": "1.0.0",
    "author": "DGIOT Team",
    "description": "DGIOT编译调试技能"
  },
  "logging": {
    "default_level": "info",
    "log_file": "dgiot_compile_debug.log",
    "max_size_mb": 10,
    "retention_days": 7
  },
  "workflow": {
    "max_compile_attempts": 3,
    "retry_delay_seconds": 3,
    "default_test_type": "unit",
    "parallel_jobs": 4
  },
  "detection": {
    "methods": ["current_dir", "open_files", "recent_files", "git_status"],
    "fallback_plugin": "dgiot_uav",
    "interactive_fallback": true
  }
}'

# 初始化配置系统
init_config() {
    # 创建配置目录
    mkdir -p "$CONFIG_DIR" 2>/dev/null
    
    # 创建默认配置文件
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo "$DEFAULT_CONFIG" > "$CONFIG_FILE"
        log_info "创建默认配置文件: $CONFIG_FILE"
    fi
    
    # 创建环境文件
    if [[ ! -f "$ENV_FILE" ]]; then
        create_env_file
    fi
    
    # 加载配置
    load_config
}

# 创建环境文件
create_env_file() {
    cat > "$ENV_FILE" << EOF
# DGIOT编译调试技能环境配置
# 自动生成于: $(date)

# DGIOT项目配置
export DGIOT_PROJECT_ROOT="/root/gitee/dgiot"
export DGIOT_REBAR3_PATH="\$DGIOT_PROJECT_ROOT/rebar3"
export DGIOT_EMQX_BIN_PATH="\$DGIOT_PROJECT_ROOT/_build/emqx/rel/emqx/bin/emqx"
export DGIOT_LOG_DIR="\$DGIOT_PROJECT_ROOT/_build/emqx/rel/emqx/log"
export DGIOT_APPS_DIR="\$DGIOT_PROJECT_ROOT/apps"
export DGIOT_ETC_DIR="\$DGIOT_PROJECT_ROOT/etc"

# 技能配置
export DGIOT_SKILL_NAME="dgiot_compile_debug"
export DGIOT_SKILL_VERSION="1.0.0"
export DGIOT_SKILL_ROOT="$SKILL_ROOT"

# 日志配置
export DGIOT_LOG_LEVEL="info"
export DGIOT_LOG_FILE="\$DGIOT_LOG_DIR/dgiot_compile_debug.log"
export DGIOT_LOG_MAX_SIZE_MB=10
export DGIOT_LOG_RETENTION_DAYS=7

# 工作流配置
export DGIOT_MAX_COMPILE_ATTEMPTS=3
export DGIOT_RETRY_DELAY_SECONDS=3
export DGIOT_DEFAULT_TEST_TYPE="unit"
export DGIOT_PARALLEL_JOBS=4

# 检测配置
export DGIOT_DETECTION_METHODS="current_dir,open_files,recent_files,git_status"
export DGIOT_FALLBACK_PLUGIN="dgiot_uav"
export DGIOT_INTERACTIVE_FALLBACK=true

# 路径配置
export PATH="\$DGIOT_PROJECT_ROOT/bin:\$PATH"
EOF
    
    log_info "创建环境配置文件: $ENV_FILE"
}

# 加载配置
load_config() {
    # 加载环境变量
    if [[ -f "$ENV_FILE" ]]; then
        source "$ENV_FILE" 2>/dev/null || true
    fi
    
    # 加载JSON配置
    if [[ -f "$CONFIG_FILE" ]]; then
        # 简单的JSON解析（使用jq如果可用）
        if command -v jq >/dev/null 2>&1; then
            export DGIOT_CONFIG_JSON=$(cat "$CONFIG_FILE")
        fi
    fi
    
    # 验证关键配置
    validate_config
}

# 验证配置
validate_config() {
    local errors=()
    
    # 检查DGIOT项目目录
    if [[ ! -d "${DGIOT_PROJECT_ROOT:-}" ]]; then
        errors+=("DGIOT项目目录不存在: ${DGIOT_PROJECT_ROOT:-未设置}")
    fi
    
    # 检查rebar3
    if [[ ! -f "${DGIOT_REBAR3_PATH:-}" ]]; then
        errors+=("rebar3不存在: ${DGIOT_REBAR3_PATH:-未设置}")
    fi
    
    # 检查日志目录
    if [[ ! -d "${DGIOT_LOG_DIR:-}" ]]; then
        log_warning "日志目录不存在: ${DGIOT_LOG_DIR:-未设置}，将尝试创建"
        mkdir -p "${DGIOT_LOG_DIR}" 2>/dev/null || true
    fi
    
    if [[ ${#errors[@]} -gt 0 ]]; then
        for error in "${errors[@]}"; do
            log_error "$error"
        done
        return 1
    fi
    
    return 0
}

# 获取配置值
get_config() {
    local key="$1"
    local default="$2"
    
    # 首先尝试从环境变量获取
    local env_var="DGIOT_$(echo "$key" | tr '[:lower:]' '[:upper:]' | tr '.' '_')"
    local value="${!env_var:-}"
    
    if [[ -n "$value" ]]; then
        echo "$value"
        return 0
    fi
    
    # 然后尝试从JSON配置获取
    if [[ -n "${DGIOT_CONFIG_JSON:-}" ]] && command -v jq >/dev/null 2>&1; then
        local json_value=$(echo "$DGIOT_CONFIG_JSON" | jq -r ".$key // \"\"")
        if [[ -n "$json_value" ]] && [[ "$json_value" != "null" ]]; then
            echo "$json_value"
            return 0
        fi
    fi
    
    # 返回默认值
    echo "${default:-}"
}

# 设置配置值
set_config() {
    local key="$1"
    local value="$2"
    
    # 更新环境变量
    local env_var="DGIOT_$(echo "$key" | tr '[:lower:]' '[:upper:]' | tr '.' '_')"
    export "$env_var"="$value"
    
    # 更新JSON配置
    if [[ -f "$CONFIG_FILE" ]] && command -v jq >/dev/null 2>&1; then
        local temp_file=$(mktemp)
        jq ".$key = \"$value\"" "$CONFIG_FILE" > "$temp_file" && mv "$temp_file" "$CONFIG_FILE"
        log_info "更新配置: $key = $value"
    fi
    
    # 更新环境文件
    update_env_file "$key" "$value"
}

# 更新环境文件
update_env_file() {
    local key="$1"
    local value="$2"
    local env_var="DGIOT_$(echo "$key" | tr '[:lower:]' '[:upper:]' | tr '.' '_')"
    
    if [[ -f "$ENV_FILE" ]]; then
        # 检查是否已存在该变量
        if grep -q "^export $env_var=" "$ENV_FILE"; then
            # 更新现有变量
            sed -i "s|^export $env_var=.*|export $env_var=\"$value\"|" "$ENV_FILE"
        else
            # 添加新变量
            echo "export $env_var=\"$value\"" >> "$ENV_FILE"
        fi
    fi
}

# 显示配置
show_config() {
    log_step "当前配置"
    
    echo -e "${YELLOW}=== DGIOT项目配置 ===${NC}"
    echo "项目根目录: $(get_config "dgiot.project_root")"
    echo "rebar3路径: $(get_config "dgiot.rebar3_path")"
    echo "EMQX二进制: $(get_config "dgiot.emqx_bin_path")"
    echo "日志目录: $(get_config "dgiot.log_dir")"
    echo "应用目录: $(get_config "dgiot.apps_dir")"
    echo "配置目录: $(get_config "dgiot.etc_dir")"
    echo ""
    
    echo -e "${YELLOW}=== 技能配置 ===${NC}"
    echo "技能名称: $(get_config "skill.name")"
    echo "版本: $(get_config "skill.version")"
    echo "技能根目录: $SKILL_ROOT"
    echo ""
    
    echo -e "${YELLOW}=== 日志配置 ===${NC}"
    echo "默认日志级别: $(get_config "logging.default_level")"
    echo "日志文件: $(get_config "logging.log_file")"
    echo "最大大小(MB): $(get_config "logging.max_size_mb")"
    echo "保留天数: $(get_config "logging.retention_days")"
    echo ""
    
    echo -e "${YELLOW}=== 工作流配置 ===${NC}"
    echo "最大编译尝试次数: $(get_config "workflow.max_compile_attempts")"
    echo "重试延迟(秒): $(get_config "workflow.retry_delay_seconds")"
    echo "默认测试类型: $(get_config "workflow.default_test_type")"
    echo "并行任务数: $(get_config "workflow.parallel_jobs")"
    echo ""
    
    echo -e "${YELLOW}=== 检测配置 ===${NC}"
    echo "检测方法: $(get_config "detection.methods")"
    echo "回退插件: $(get_config "detection.fallback_plugin")"
    echo "交互式回退: $(get_config "detection.interactive_fallback")"
}

# 交互式配置向导
config_wizard() {
    log_step "交互式配置向导"
    
    echo -e "${CYAN}欢迎使用DGIOT编译调试技能配置向导${NC}"
    echo ""
    
    # DGIOT项目目录
    read -p "DGIOT项目根目录 [$(get_config "dgiot.project_root")]: " project_root
    if [[ -n "$project_root" ]]; then
        set_config "dgiot.project_root" "$project_root"
    fi
    
    # 日志级别
    read -p "默认日志级别 (debug/info/warn/error) [$(get_config "logging.default_level")]: " log_level
    if [[ -n "$log_level" ]]; then
        set_config "logging.default_level" "$log_level"
    fi
    
    # 最大编译尝试次数
    read -p "最大编译尝试次数 [$(get_config "workflow.max_compile_attempts")]: " max_attempts
    if [[ -n "$max_attempts" ]]; then
        set_config "workflow.max_compile_attempts" "$max_attempts"
    fi
    
    # 回退插件
    read -p "默认回退插件 [$(get_config "detection.fallback_plugin")]: " fallback_plugin
    if [[ -n "$fallback_plugin" ]]; then
        set_config "detection.fallback_plugin" "$fallback_plugin"
    fi
    
    log_result "配置向导完成"
    show_config
}

# 重置配置
reset_config() {
    log_step "重置配置"
    
    read -p "确定要重置所有配置为默认值吗？(y/N): " confirm
    if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
        rm -f "$CONFIG_FILE" "$ENV_FILE"
        init_config
        log_result "配置已重置为默认值"
    else
        log_info "取消重置操作"
    fi
}

# 导出配置
export_config() {
    local output_file="${1:-$CONFIG_DIR/config_export_$(date +%Y%m%d_%H%M%S).sh}"
    
    log_step "导出配置到: $output_file"
    
    {
        echo "#!/bin/bash"
        echo "# DGIOT编译调试技能配置导出"
        echo "# 导出时间: $(date)"
        echo ""
        echo "# 环境变量配置"
        grep "^export DGIOT_" "$ENV_FILE" 2>/dev/null || echo "# 环境文件不存在"
        echo ""
        echo "# JSON配置"
        cat "$CONFIG_FILE" 2>/dev/null || echo "# 配置文件不存在"
        
    } > "$output_file"
    
    chmod +x "$output_file"
    log_result "配置已导出到: $output_file"
}

# 导入配置
import_config() {
    local input_file="$1"
    
    if [[ ! -f "$input_file" ]]; then
        log_error "导入文件不存在: $input_file"
        return 1
    fi
    
    log_step "从文件导入配置: $input_file"
    
    # 备份当前配置
    local backup_dir="$CONFIG_DIR/backup_$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$backup_dir"
    cp -f "$CONFIG_FILE" "$backup_dir/" 2>/dev/null || true
    cp -f "$ENV_FILE" "$backup_dir/" 2>/dev/null || true
    
    # 提取环境变量
    if grep -q "^export DGIOT_" "$input_file"; then
        # 创建新的环境文件
        grep "^export DGIOT_" "$input_file" > "$ENV_FILE.new"
        
        # 合并现有配置
        if [[ -f "$ENV_FILE" ]]; then
            grep -v "^export DGIOT_" "$ENV_FILE" >> "$ENV_FILE.new"
        fi
        
        mv "$ENV_FILE.new" "$ENV_FILE"
    fi
    
    # 提取JSON配置
    if grep -q "^{" "$input_file"; then
        # 提取JSON部分
        sed -n '/^{/,/^}/p' "$input_file" > "$CONFIG_FILE.new"
        mv "$CONFIG_FILE.new" "$CONFIG_FILE"
    fi
    
    # 重新加载配置
    load_config
    
    log_result "配置导入完成，原配置已备份到: $backup_dir"
    show_config
}

# 检查配置健康状态
check_config_health() {
    log_step "检查配置健康状态"
    
    local warnings=()
    local errors=()
    
    # 检查关键路径
    local project_root=$(get_config "dgiot.project_root")
    if [[ ! -d "$project_root" ]]; then
        errors+=("DGIOT项目目录不存在: $project_root")
    else
        # 检查rebar3
        local rebar3_path=$(get_config "dgiot.rebar3_path")
        if [[ ! -f "$rebar3_path" ]]; then
            warnings+=("rebar3不存在: $rebar3_path")
        fi
        
        # 检查EMQX二进制
        local emqx_bin=$(get_config "dgiot.emqx_bin_path")
        if [[ ! -f "$emqx_bin" ]]; then
            warnings+=("EMQX二进制不存在: $emqx_bin (可能需要先编译)")
        fi
    fi
    
    # 检查日志目录
    local log_dir=$(get_config "dgiot.log_dir")
    if [[ ! -d "$log_dir" ]]; then
        warnings+=("日志目录不存在: $log_dir (将自动创建)")
        mkdir -p "$log_dir" 2>/dev/null || errors+=("无法创建日志目录: $log_dir")
    fi
    
    # 显示结果
    if [[ ${#errors[@]} -eq 0 ]] && [[ ${#warnings[@]} -eq 0 ]]; then
        log_result "配置健康状态: 优秀 ✓"
        return 0
    fi
    
    if [[ ${#warnings[@]} -gt 0 ]]; then
        echo -e "${YELLOW}警告:${NC}"
        for warning in "${warnings[@]}"; do
            echo "  ⚠ $warning"
        done
        echo ""
    fi
    
    if [[ ${#errors[@]} -gt 0 ]]; then
        echo -e "${RED}错误:${NC}"
        for error in "${errors[@]}"; do
            echo "  ✗ $error"
        done
        echo ""
        return 1
    fi
    
    return 0
}

# 日志函数（与log_utils.sh兼容）
log_info() {
    echo -e "${GREEN}[INFO] $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}[WARN] $1${NC}"
}

log_error() {
    echo -e "${RED}[ERROR] $1${NC}"
}

log_step() {
    echo -e "\n${CYAN}=== $1 ===${NC}"
}

log_result() {
    echo -e "${GREEN}✓ $1${NC}"
}

# 显示帮助信息
show_help() {
    echo -e "${CYAN}DGIOT编译调试技能配置系统${NC}"
    echo ""
    echo -e "${GREEN}用法:${NC} source config.sh [命令]"
    echo ""
    echo -e "${YELLOW}命令:${NC}"
    echo "  init           初始化配置系统"
    echo "  show           显示当前配置"
    echo "  wizard         交互式配置向导"
    echo "  set <键> <值>  设置配置值"
    echo "  get <键>       获取配置值"
    echo "  health         检查配置健康状态"
    echo "  reset          重置配置为默认值"
    echo "  export [文件]  导出配置到文件"
    echo "  import <文件>  从文件导入配置"
    echo "  help           显示帮助信息"
    echo ""
    echo -e "${YELLOW}示例:${NC}"
    echo "  source config.sh init"
    echo "  source config.sh show"
    echo "  source config.sh set dgiot.project_root /path/to/dgiot"
    echo "  source config.sh get logging.default_level"
    echo "  source config.sh wizard"
    echo "  source config.sh health"
    echo ""
    echo -e "${YELLOW}配置文件位置:${NC}"
    echo "  JSON配置: $CONFIG_FILE"
    echo "  环境配置: $ENV_FILE"
    echo "  配置目录: $CONFIG_DIR"
}

# 主函数
main() {
    local command="$1"
    local arg1="$2"
    local arg2="$3"
    
    # 初始化配置
    init_config
    
    case "$command" in
        "init")
            init_config
            ;;
        "show")
            show_config
            ;;
        "wizard")
            config_wizard
            ;;
        "set")
            if [[ -z "$arg1" ]] || [[ -z "$arg2" ]]; then
                log_error "使用方法: set <键> <值>"
                return 1
            fi
            set_config "$arg1" "$arg2"
            ;;
        "get")
            if [[ -z "$arg1" ]]; then
                log_error "使用方法: get <键>"
                return 1
            fi
            get_config "$arg1"
            ;;
        "health")
            check_config_health
            ;;
        "reset")
            reset_config
            ;;
        "export")
            export_config "$arg1"
            ;;
        "import")
            if [[ -z "$arg1" ]]; then
                log_error "使用方法: import <文件>"
                return 1
            fi
            import_config "$arg1"
            ;;
        "help"|"")
            show_help
            ;;
        *)
            log_error "未知命令: $command"
            show_help
            return 1
            ;;
    esac
    
    return $?
}

# 如果直接执行脚本，执行主函数
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
