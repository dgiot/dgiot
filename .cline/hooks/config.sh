#!/bin/bash
# 通用Hook配置脚本
# 提供无硬编码的路径配置

# 获取项目根目录
get_project_root() {
    # 方法1: 从环境变量获取
    if [ -n "$DGIOT_PROJECT_ROOT" ]; then
        echo "$DGIOT_PROJECT_ROOT"
        return 0
    fi
    
    # 方法2: 从当前目录向上查找.cline目录
    local current_dir="$(pwd)"
    while [ "$current_dir" != "/" ]; do
        if [ -d "$current_dir/.cline" ]; then
            echo "$current_dir"
            return 0
        fi
        current_dir="$(dirname "$current_dir")"
    done
    
    # 方法3: 默认使用脚本所在目录计算
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    echo "$(cd "$script_dir/../../.." && pwd)"
}

# 获取Hook目录
get_hooks_dir() {
    local project_root="$1"
    echo "$project_root/.cline/hooks"
}

# 获取配置目录
get_config_dir() {
    local project_root="$1"
    echo "$project_root/.cline/config"
}

# 获取日志目录
get_logs_dir() {
    local project_root="$1"
    echo "$project_root/.cline/logs"
}

# 获取Git Hook目录
get_git_hooks_dir() {
    local project_root="$1"
    echo "$project_root/.git/hooks"
}

# 获取VS Code配置目录
get_vscode_dir() {
    local project_root="$1"
    echo "$project_root/.vscode"
}

# 导出配置变量
export_config() {
    export DGIOT_PROJECT_ROOT="$(get_project_root)"
    export DGIOT_HOOKS_DIR="$(get_hooks_dir "$DGIOT_PROJECT_ROOT")"
    export DGIOT_CONFIG_DIR="$(get_config_dir "$DGIOT_PROJECT_ROOT")"
    export DGIOT_LOGS_DIR="$(get_logs_dir "$DGIOT_PROJECT_ROOT")"
    export DGIOT_GIT_HOOKS_DIR="$(get_git_hooks_dir "$DGIOT_PROJECT_ROOT")"
    export DGIOT_VSCODE_DIR="$(get_vscode_dir "$DGIOT_PROJECT_ROOT")"
    
    # 日志文件路径
    export DGIOT_HOOK_LOG="$DGIOT_HOOKS_DIR/hook_integration.log"
    export DGIOT_GIT_HOOK_LOG="$DGIOT_HOOKS_DIR/git_hook.log"
    export DGIOT_DAILY_REPORT="$DGIOT_HOOKS_DIR/reports/$(date +%Y-%m-%d)_chinese_check.md"
    
    # 配置文件路径
    export DGIOT_CHINESE_PRINTING_HOOK_CONFIG="$DGIOT_CONFIG_DIR/chinese_printing_hook.yaml"
    export DGIOT_TEST_SCRIPT="$DGIOT_HOOKS_DIR/test_hook_mechanism.js"
    export DGIOT_DAILY_CHECK_SCRIPT="$DGIOT_HOOKS_DIR/daily_check.sh"
}

# 打印配置信息
print_config() {
    echo "=== DGIOT Hook配置 ==="
    echo "项目根目录: $DGIOT_PROJECT_ROOT"
    echo "Hook目录: $DGIOT_HOOKS_DIR"
    echo "配置目录: $DGIOT_CONFIG_DIR"
    echo "日志目录: $DGIOT_LOGS_DIR"
    echo "Git Hook目录: $DGIOT_GIT_HOOKS_DIR"
    echo "VS Code目录: $DGIOT_VSCODE_DIR"
    echo ""
    echo "日志文件:"
    echo "  Hook集成日志: $DGIOT_HOOK_LOG"
    echo "  Git Hook日志: $DGIOT_GIT_HOOK_LOG"
    echo "  每日报告: $DGIOT_DAILY_REPORT"
    echo ""
    echo "配置文件:"
    echo "  中文打印Hook配置: $DGIOT_CHINESE_PRINTING_HOOK_CONFIG"
    echo "  测试脚本: $DGIOT_TEST_SCRIPT"
    echo "  每日检查脚本: $DGIOT_DAILY_CHECK_SCRIPT"
}

# 检查目录是否存在
check_directories() {
    local missing_dirs=()
    
    for dir in "$DGIOT_PROJECT_ROOT" "$DGIOT_HOOKS_DIR" "$DGIOT_CONFIG_DIR" "$DGIOT_LOGS_DIR"; do
        if [ ! -d "$dir" ]; then
            missing_dirs+=("$dir")
        fi
    done
    
    if [ ${#missing_dirs[@]} -gt 0 ]; then
        echo "警告: 以下目录不存在:"
        for dir in "${missing_dirs[@]}"; do
            echo "  $dir"
        done
        return 1
    fi
    
    return 0
}

# 主函数 - 如果直接执行则导出配置
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    export_config
    print_config
    check_directories
fi