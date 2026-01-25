#!/bin/bash
# detect_plugin.sh - 智能识别当前工作插件

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 主函数：识别当前插件
detect_plugin() {
    local plugin_name=""
    
    log_info "开始识别当前插件..."
    
    # 方法1: 从当前目录识别
    plugin_name=$(detect_from_current_directory)
    if [[ -n "$plugin_name" ]]; then
        log_success "从当前目录识别到插件: $plugin_name"
        echo "$plugin_name"
        return 0
    fi
    
    # 方法2: 从打开的文件识别
    plugin_name=$(detect_from_open_files)
    if [[ -n "$plugin_name" ]]; then
        log_success "从打开的文件识别到插件: $plugin_name"
        echo "$plugin_name"
        return 0
    fi
    
    # 方法3: 从最近修改的文件识别
    plugin_name=$(detect_from_recent_files)
    if [[ -n "$plugin_name" ]]; then
        log_success "从最近修改的文件识别到插件: $plugin_name"
        echo "$plugin_name"
        return 0
    fi
    
    # 方法4: 从Git状态识别
    plugin_name=$(detect_from_git_status)
    if [[ -n "$plugin_name" ]]; then
        log_success "从Git状态识别到插件: $plugin_name"
        echo "$plugin_name"
        return 0
    fi
    
    # 方法5: 默认插件
    log_warning "无法自动识别插件，使用默认插件: dgiot"
    echo "dgiot"
    return 1
}

# 方法1: 从当前目录识别
detect_from_current_directory() {
    local current_dir="$PWD"
    
    # 检查是否在apps目录下
    if [[ "$current_dir" =~ /apps/([^/]+) ]]; then
        echo "${BASH_REMATCH[1]}"
        return 0
    fi
    
    # 检查是否在插件相关目录
    if [[ "$current_dir" =~ /dgiot_([^/]+) ]]; then
        echo "dgiot_${BASH_REMATCH[1]}"
        return 0
    fi
    
    return 1
}

# 方法2: 从打开的文件识别
detect_from_open_files() {
    # 尝试从VS Code获取打开的文件
    local open_files=""
    
    # 方法2.1: 检查VS Code进程
    if pgrep -f "code.*--user-data-dir" >/dev/null 2>&1; then
        # 获取VS Code打开的文件
        open_files=$(get_vscode_open_files)
    fi
    
    # 方法2.2: 检查环境变量中的打开文件
    if [[ -z "$open_files" ]] && [[ -n "$VISUAL_STUDIO_CODE_OPEN_TABS" ]]; then
        open_files="$VISUAL_STUDIO_CODE_OPEN_TABS"
    fi
    
    # 分析打开的文件
    if [[ -n "$open_files" ]]; then
        # 转换为行
        while IFS= read -r line; do
            if [[ "$line" =~ apps/([^/]+)/ ]]; then
                echo "${BASH_REMATCH[1]}"
                return 0
            fi
        done <<< "$open_files"
    fi
    
    return 1
}

# 获取VS Code打开的文件
get_vscode_open_files() {
    local vscode_pid=$(pgrep -f "code.*--user-data-dir" | head -1)
    if [[ -n "$vscode_pid" ]]; then
        # 尝试通过lsof获取打开的文件
        lsof -p "$vscode_pid" 2>/dev/null | \
            grep -E "\.erl$|\.app\.src$|\.md$" | \
            awk '{print $9}' | \
            grep -v "^$"
    fi
}

# 方法3: 从最近修改的文件识别
detect_from_recent_files() {
    local recent_file=""
    
    # 查找最近修改的Erlang文件
    recent_file=$(find /root/gitee/dgiot/apps -name "*.erl" -o -name "*.app.src" 2>/dev/null | \
        xargs ls -t 2>/dev/null | head -1)
    
    if [[ -n "$recent_file" ]] && [[ "$recent_file" =~ apps/([^/]+)/ ]]; then
        echo "${BASH_REMATCH[1]}"
        return 0
    fi
    
    return 1
}

# 方法4: 从Git状态识别
detect_from_git_status() {
    local git_status=""
    
    if command -v git >/dev/null 2>&1 && [[ -d "/root/gitee/dgiot/.git" ]]; then
        cd /root/gitee/dgiot || return 1
        git_status=$(git status --porcelain 2>/dev/null | head -5)
        
        # 分析修改的文件
        while IFS= read -r line; do
            local file=$(echo "$line" | awk '{print $2}')
            if [[ "$file" =~ ^apps/([^/]+)/ ]]; then
                echo "${BASH_REMATCH[1]}"
                return 0
            fi
        done <<< "$git_status"
    fi
    
    return 1
}

# 方法5: 交互式选择插件
interactive_select_plugin() {
    local plugins=()
    
    log_info "正在列出所有可用插件..."
    
    # 获取所有插件
    if [[ -d "/root/gitee/dgiot/apps" ]]; then
        plugins=($(ls /root/gitee/dgiot/apps/ | grep -E "^dgiot_" | head -10))
    fi
    
    if [[ ${#plugins[@]} -eq 0 ]]; then
        log_error "未找到任何插件"
        return 1
    fi
    
    echo -e "\n${GREEN}可用插件列表:${NC}"
    for i in "${!plugins[@]}"; do
        echo "  $((i+1)). ${plugins[$i]}"
    done
    
    echo -e "\n${YELLOW}请选择插件 (输入编号或名称):${NC}"
    read -r selection
    
    # 处理数字选择
    if [[ "$selection" =~ ^[0-9]+$ ]] && [[ "$selection" -le ${#plugins[@]} ]]; then
        echo "${plugins[$((selection-1))]}"
        return 0
    fi
    
    # 处理名称选择
    for plugin in "${plugins[@]}"; do
        if [[ "$plugin" == "$selection" ]]; then
            echo "$plugin"
            return 0
        fi
    done
    
    log_error "无效的选择: $selection"
    return 1
}

# 测试函数
test_plugin_detection() {
    log_info "=== 插件识别测试 ==="
    
    echo -e "\n${YELLOW}1. 测试当前目录识别:${NC}"
    cd /root/gitee/dgiot/apps/dgiot_uav 2>/dev/null && \
        echo "当前目录: $PWD" && \
        echo "识别结果: $(detect_from_current_directory)" || \
        echo "测试跳过: 不在插件目录"
    
    echo -e "\n${YELLOW}2. 测试打开文件识别:${NC}"
    echo "识别结果: $(detect_from_open_files)"
    
    echo -e "\n${YELLOW}3. 测试最近文件识别:${NC}"
    echo "识别结果: $(detect_from_recent_files)"
    
    echo -e "\n${YELLOW}4. 测试Git状态识别:${NC}"
    echo "识别结果: $(detect_from_git_status)"
    
    echo -e "\n${YELLOW}5. 测试完整识别流程:${NC}"
    echo "最终识别结果: $(detect_plugin)"
}

# 主程序
main() {
    local action="${1:-detect}"
    
    case "$action" in
        "detect")
            detect_plugin
            ;;
        "test")
            test_plugin_detection
            ;;
        "interactive")
            interactive_select_plugin
            ;;
        "help"|"--help"|"-h")
            echo "用法: $0 [命令]"
            echo "命令:"
            echo "  detect      自动识别当前插件 (默认)"
            echo "  test        测试所有识别方法"
            echo "  interactive 交互式选择插件"
            echo "  help        显示帮助信息"
            ;;
        *)
            echo "未知命令: $action"
            echo "使用 '$0 help' 查看帮助"
            exit 1
            ;;
    esac
}

# 执行主程序
main "$@"
