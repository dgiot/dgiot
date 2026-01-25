#!/bin/bash
# 中文打印Hook集成脚本
# 将自动触发机制集成到DGIOT项目中

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 项目根目录 - 使用脚本所在目录动态计算
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HOOKS_DIR="$DGIOT_HOOKS_DIR"
LOG_FILE="$HOOKS_DIR/hook_integration.log"

# 日志函数
log() {
    local level=$1
    local message=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    case $level in
        "INFO")
            echo -e "${GREEN}[INFO]${NC} $message"
            ;;
        "WARN")
            echo -e "${YELLOW}[WARN]${NC} $message"
            ;;
        "ERROR")
            echo -e "${RED}[ERROR]${NC} $message"
            ;;
        "DEBUG")
            echo -e "${BLUE}[DEBUG]${NC} $message"
            ;;
    esac
    
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
}

# 检查依赖
check_dependencies() {
    log "INFO" "检查依赖..."
    
    # 检查Node.js
    if command -v node &> /dev/null; then
        log "INFO" "✅ Node.js 已安装: $(node --version)"
    else
        log "ERROR" "❌ Node.js 未安装"
        exit 1
    fi
    
    # 检查npm
    if command -v npm &> /dev/null; then
        log "INFO" "✅ npm 已安装: $(npm --version)"
    else
        log "WARN" "⚠️  npm 未安装，某些功能可能受限"
    fi
    
    # 检查Erlang
    if command -v erl &> /dev/null; then
        log "INFO" "✅ Erlang 已安装: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>/dev/null | tr -d '\n')"
    else
        log "WARN" "⚠️  Erlang 未安装，但Hook机制仍可工作"
    fi
    
    # 检查Git
    if command -v git &> /dev/null; then
        log "INFO" "✅ Git 已安装: $(git --version)"
    else
        log "WARN" "⚠️  Git 未安装，Git Hook功能不可用"
    fi
}

# 创建目录结构
create_directories() {
    log "INFO" "创建目录结构..."
    
    # 确保.cline目录存在
    mkdir -p "$PROJECT_ROOT/.cline"
    
    # 创建必要的子目录
    local dirs=("hooks" "logs" "cache" "config")
    for dir in "${dirs[@]}"; do
        local full_dir="$PROJECT_ROOT/.cline/$dir"
        if [ ! -d "$full_dir" ]; then
            mkdir -p "$full_dir"
            log "INFO" "✅ 创建目录: $full_dir"
        else
            log "DEBUG" "目录已存在: $full_dir"
        fi
    done
}

# 复制Hook配置文件
copy_hook_configs() {
    log "INFO" "复制Hook配置文件..."
    
    # 检查源文件是否存在
    local source_config="$HOOKS_DIR/chinese_printing_hook.yaml"
    if [ ! -f "$source_config" ]; then
        log "ERROR" "❌ Hook配置文件不存在: $source_config"
        exit 1
    fi
    
    # 复制到配置目录
    local target_config="$PROJECT_ROOT/.cline/config/chinese_printing_hook.yaml"
    cp "$source_config" "$target_config"
    log "INFO" "✅ 复制Hook配置到: $target_config"
    
    # 复制测试脚本
    local test_script="$HOOKS_DIR/test_hook_mechanism.js"
    if [ -f "$test_script" ]; then
        cp "$test_script" "$PROJECT_ROOT/.cline/hooks/"
        log "INFO" "✅ 复制测试脚本"
    fi
}

# 创建Git Hook
setup_git_hooks() {
    log "INFO" "设置Git Hook..."
    
    local git_hooks_dir="$PROJECT_ROOT/.git/hooks"
    
    if [ ! -d "$git_hooks_dir" ]; then
        log "WARN" "⚠️  .git/hooks目录不存在，跳过Git Hook设置"
        return
    fi
    
    # 创建pre-commit hook
    local pre_commit_hook="$git_hooks_dir/pre-commit"
    cat > "$pre_commit_hook" << 'EOF'
#!/bin/bash
# Git pre-commit hook for Chinese printing checks

set -e

PROJECT_ROOT="$DGIOT_PROJECT_ROOT"
HOOKS_DIR="$DGIOT_HOOKS_DIR"
LOG_FILE="$HOOKS_DIR/git_hook.log"

# 获取暂存的文件
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.erl$')

if [ -z "$STAGED_FILES" ]; then
    echo "没有Erlang文件需要检查"
    exit 0
fi

echo "检查Erlang文件中的中文打印问题..."

# 运行检查
for file in $STAGED_FILES; do
    if [ ! -f "$file" ]; then
        continue
    fi
    
    echo "检查文件: $file"
    
    # 检查是否包含中文
    if grep -q -P "[\x{4e00}-\x{9fff}]" "$file"; then
        echo "  ⚠️  文件中包含中文文本"
        
        # 检查是否有问题的io:format调用
        if grep -q -P 'io:format\s*\([^)]*[\x{4e00}-\x{9fff}][^)]*\)' "$file"; then
            echo "  ❌ 检测到有问题的io:format调用包含中文"
            echo "     建议使用: use_skill chinese_printing_solution"
            echo "     文件: $file"
            echo ""
        fi
        
        # 检查缺少/utf8后缀的中文字符串
        if grep -q -P '<<\"[^\"]*[\x{4e00}-\x{9fff}]+[^\"]*\">>(?!\s*\/utf8)' "$file"; then
            echo "  ❌ 检测到缺少/utf8后缀的中文字符串"
            echo "     建议使用: use_skill erlang_chinese_utf8"
            echo "     文件: $file"
            echo ""
        fi
    fi
done

echo "检查完成"
echo "详细日志: $LOG_FILE"

# 记录到日志
{
    echo "=== pre-commit检查 ==="
    echo "时间: $(date)"
    echo "文件数: $(echo "$STAGED_FILES" | wc -l)"
    echo "文件列表:"
    echo "$STAGED_FILES"
    echo ""
} >> "$LOG_FILE"

exit 0
EOF
    
    chmod +x "$pre_commit_hook"
    log "INFO" "✅ 创建pre-commit hook"
    
    # 创建post-commit hook用于报告
    local post_commit_hook="$git_hooks_dir/post-commit"
    cat > "$post_commit_hook" << 'EOF'
#!/bin/bash
# Git post-commit hook for Chinese printing report

PROJECT_ROOT="$DGIOT_PROJECT_ROOT"
HOOKS_DIR="$DGIOT_HOOKS_DIR"
REPORT_FILE="$HOOKS_DIR/chinese_printing_report.md"

# 获取最近提交的文件
RECENT_FILES=$(git show --name-only --pretty="" HEAD | grep '\.erl$')

if [ -z "$RECENT_FILES" ]; then
    exit 0
fi

# 生成报告
{
    echo "# 中文打印检查报告"
    echo "生成时间: $(date)"
    echo "提交: $(git log -1 --pretty=format:'%h - %s')"
    echo ""
    echo "## 检查的文件"
    echo ""
    
    for file in $RECENT_FILES; do
        echo "### $file"
        echo ""
        
        if grep -q -P "[\x{4e00}-\x{9fff}]" "$file"; then
            echo "✅ 包含中文文本"
            
            # 检查编码规范
            if grep -q -P 'io:format\s*\([^)]*[\x{4e00}-\x{9fff}][^)]*\)' "$file"; then
                echo "⚠️  发现未优化的io:format调用"
                echo "   建议: 使用二进制字符串和/utf8后缀"
            else
                echo "✅ io:format调用规范"
            fi
            
            if grep -q -P '<<\"[^\"]*[\x{4e00}-\x{9fff}]+[^\"]*\">>(?!\s*\/utf8)' "$file"; then
                echo "❌ 发现缺少/utf8后缀的中文字符串"
                echo "   建议: 添加/utf8后缀"
            else
                echo "✅ 二进制字符串规范"
            fi
        else
            echo "ℹ️  不包含中文文本"
        fi
        echo ""
    done
    
    echo "## 建议"
    echo ""
    echo "1. 对于有问题的文件，建议运行:"
    echo "   ```bash"
    echo "   use_skill chinese_printing_solution"
    echo "   ```"
    echo ""
    echo "2. 对于缺少/utf8后缀的文件，建议运行:"
    echo "   ```bash"
    echo "   use_skill erlang_chinese_utf8"
    echo "   ```"
    echo ""
    echo "---"
    echo "*报告由中文打印Hook自动生成*"
    
} > "$REPORT_FILE"

echo "中文打印报告已生成: $REPORT_FILE"

exit 0
EOF
    
    chmod +x "$post_commit_hook"
    log "INFO" "✅ 创建post-commit hook"
}

# 创建VS Code任务
setup_vscode_tasks() {
    log "INFO" "设置VS Code任务..."
    
    local vscode_dir="$PROJECT_ROOT/.vscode"
    mkdir -p "$vscode_dir"
    
    # 创建tasks.json
    local tasks_file="$vscode_dir/tasks.json"
    cat > "$tasks_file" << 'EOF'
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "检查中文打印问题",
            "type": "shell",
            "command": "node",
            "args": [
                "${workspaceFolder}/.cline/hooks/test_hook_mechanism.js"
            ],
            "group": {
                "kind": "build",
                "isDefault": false
            },
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared",
                "showReuseMessage": true,
                "clear": false
            },
            "problemMatcher": []
        },
        {
            "label": "扫描项目中文问题",
            "type": "shell",
            "command": "find",
            "args": [
                "${workspaceFolder}",
                "-name",
                "*.erl",
                "-exec",
                "grep",
                "-l",
                "-P",
                "[\\x{4e00}-\\x{9fff}]",
                "{}",
                ";"
            ],
            "group": {
                "kind": "build",
                "isDefault": false
            },
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared",
                "showReuseMessage": true,
                "clear": false
            },
            "problemMatcher": []
        },
        {
            "label": "生成中文打印报告",
            "type": "shell",
            "command": "bash",
            "args": [
                "${workspaceFolder}/.cline/hooks/generate_report.sh"
            ],
            "group": {
                "kind": "build",
                "isDefault": false
            },
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared",
                "showReuseMessage": true,
                "clear": false
            }
        }
    ]
}
EOF
    
    log "INFO" "✅ 创建VS Code任务配置"
    
    # 创建settings.json推荐
    local settings_file="$vscode_dir/settings.json"
    if [ ! -f "$settings_file" ]; then
        cat > "$settings_file" << 'EOF'
{
    "files.encoding": "utf8",
    "files.autoGuessEncoding": true,
    "[erlang]": {
        "editor.tabSize": 4,
        "editor.insertSpaces": true,
        "editor.detectIndentation": false
    },
    "emeraldwalk.runonsave": {
        "commands": [
            {
                "match": "\\.erl$",
                "cmd": "echo 'Erlang文件已保存，运行中文检查...' && node ${workspaceFolder}/.cline/hooks/test_hook_mechanism.js"
            }
        ]
    }
}
EOF
        log "INFO" "✅ 创建VS Code设置文件"
    else
        log "DEBUG" "VS Code设置文件已存在，跳过创建"
    fi
}

# 创建定期检查脚本
create_scheduled_check() {
    log "INFO" "创建定期检查脚本..."
    
    local check_script="$HOOKS_DIR/daily_check.sh"
    cat > "$check_script" << 'EOF'
#!/bin/bash
# 每日中文打印检查脚本

set -e

# 使用脚本所在目录计算项目根目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
HOOKS_DIR="$DGIOT_HOOKS_DIR"
REPORT_DIR="$HOOKS_DIR/reports"
DAILY_REPORT="$REPORT_DIR/$(date +%Y-%m-%d)_chinese_check.md"

# 创建报告目录
mkdir -p "$REPORT_DIR"

# 查找所有Erlang文件
ERL_FILES=$(find "$PROJECT_ROOT" -name "*.erl" -type f | grep -v -E "(\.git|_build|deps|ebin)")

TOTAL_FILES=$(echo "$ERL_FILES" | wc -l)
FILES_WITH_CHINESE=0
FILES_WITH_ISSUES=0

# 生成报告
{
    echo "# 每日中文打印检查报告"
    echo "生成时间: $(date)"
    echo "项目: DGIOT"
    echo ""
    echo "## 统计信息"
    echo ""
    echo "- 总Erlang文件数: $TOTAL_FILES"
    echo ""
    
    echo "## 详细检查结果"
    echo ""
    
    for file in $ERL_FILES; do
        # 跳过太长的路径显示
        short_file=${file#$PROJECT_ROOT/}
        
        if grep -q -P "[\x{4e00}-\x{9fff}]" "$file"; then
            ((FILES_WITH_CHINESE++))
            
            echo "### $short_file"
            echo ""
            echo "✅ 包含中文文本"
            
            ISSUES=0
            
            # 检查问题
            if grep -q -P 'io:format\s*\([^)]*[\x{4e00}-\x{9fff}][^)]*\)' "$file"; then
                echo "❌ 发现未优化的io:format调用"
                ((ISSUES++))
            fi
            
            if grep -q -P '<<\"[^\"]*[\x{4e00}-\x{9fff}]+[^\"]*\">>(?!\s*\/utf8)' "$file"; then
                echo "❌ 发现缺少/utf8后缀的中文字符串"
                ((ISSUES++))
            fi
            
            if [ $ISSUES -eq 0 ]; then
                echo "✅ 编码规范良好"
            else
                ((FILES_WITH_ISSUES++))
                echo ""
                echo "**建议操作:**"
                echo "1. 运行: \`use_skill chinese_printing_solution\`"
                echo "2. 运行: \`use_skill erlang_chinese_utf8\`"
            fi
            
            echo ""
        fi
    done
    
    echo "## 总结"
    echo ""
    echo "- 包含中文的文件: $FILES_WITH_CHINESE"
    echo "- 有问题的文件: $FILES_WITH_ISSUES"
    echo "- 规范良好的文件: $((FILES_WITH_CHINESE - FILES_WITH_ISSUES))"
    echo ""
    
    if [ $FILES_WITH_ISSUES -gt 0 ]; then
        echo "## 🚨 需要关注的文件"
        echo ""
        echo "以下文件需要立即处理:"
        echo ""
        
        for file in $ERL_FILES; do
            if grep -q -P "[\x{4e00}-\x{9fff}]" "$file"; then
                if grep -q -P '<<\"[^\"]*[\x{4e00}-\x{9fff}]+[^\"]*\">>(?!\s*\/utf8)' "$file"; then
                    short_file=${file#$PROJECT_ROOT/}
                    echo "- $short_file (缺少/utf8后缀)"
                fi
            fi
        done
    else
        echo "## ✅ 所有文件规范良好"
        echo ""
        echo "恭喜！所有包含中文的Erlang文件都符合编码规范。"
    fi
    
    echo ""
    echo "---"
    echo "*报告由中文打印Hook自动生成*"
    
} > "$DAILY_REPORT"

echo "每日检查报告已生成: $DAILY_REPORT"
}

# 主函数
main() {
    log "INFO" "开始集成中文打印Hook机制..."
    log "INFO" "项目根目录: $PROJECT_ROOT"
    
    check_dependencies
    create_directories
    copy_hook_configs
    setup_git_hooks
    setup_vscode_tasks
    create_scheduled_check
    
    log "INFO" "✅ Hook机制集成完成!"
    log "INFO" ""
    log "INFO" "下一步操作:"
    log "INFO" "1. 运行测试: node $HOOKS_DIR/test_hook_mechanism.js"
    log "INFO" "2. 手动检查: bash $HOOKS_DIR/daily_check.sh"
    log "INFO" "3. 在VS Code中运行任务: '检查中文打印问题'"
    log "INFO" ""
    log "INFO" "详细日志: $LOG_FILE"
    
    echo ""
    echo "✅ 中文打印Hook机制集成完成!"
    echo "   详细日志: $LOG_FILE"
}

# 执行主函数
main
