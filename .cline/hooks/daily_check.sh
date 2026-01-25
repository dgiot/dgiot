#!/bin/bash
# 每日中文打印检查脚本 - 无硬编码版本

set -e

# 加载配置
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/config.sh"
export_config

# 创建报告目录
REPORT_DIR="$DGIOT_HOOKS_DIR/reports"
mkdir -p "$REPORT_DIR"
DAILY_REPORT="$REPORT_DIR/$(date +%Y-%m-%d)_chinese_check.md"

echo "开始每日中文打印检查..."
echo "项目: $DGIOT_PROJECT_ROOT"
echo "报告: $DAILY_REPORT"
echo ""

# 查找所有Erlang文件
ERL_FILES=$(find "$DGIOT_PROJECT_ROOT" -name "*.erl" -type f | grep -v -E "(\.git|_build|deps|ebin)")

TOTAL_FILES=$(echo "$ERL_FILES" | wc -l)
FILES_WITH_CHINESE=0
FILES_WITH_ISSUES=0

# 生成报告
{
    echo "# 每日中文打印检查报告"
    echo "生成时间: $(date)"
    echo "项目: $(basename "$DGIOT_PROJECT_ROOT")"
    echo "项目路径: $DGIOT_PROJECT_ROOT"
    echo ""
    echo "## 统计信息"
    echo ""
    echo "- 总Erlang文件数: $TOTAL_FILES"
    echo ""
    
    echo "## 详细检查结果"
    echo ""
    
    for file in $ERL_FILES; do
        # 跳过太长的路径显示
        short_file=${file#$DGIOT_PROJECT_ROOT/}
        
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
                    short_file=${file#$DGIOT_PROJECT_ROOT/}
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
    echo "## 建议"
    echo ""
    echo "1. 运行Hook集成:"
    echo "   \`bash $DGIOT_HOOKS_DIR/integrate_hook.sh\`"
    echo ""
    echo "2. 设置环境变量:"
    echo "   \`source $DGIOT_HOOKS_DIR/setup_env.sh\`"
    echo ""
    echo "3. 验证无硬编码:"
    echo "   \`bash $DGIOT_HOOKS_DIR/validate_no_hardcoding.sh\`"
    echo ""
    echo "---"
    echo "*报告由中文打印Hook自动生成*"
    echo "*项目: $DGIOT_PROJECT_ROOT*"
    
} > "$DAILY_REPORT"

echo "每日检查报告已生成: $DAILY_REPORT"
echo ""
echo "总结:"
echo "- 总Erlang文件数: $TOTAL_FILES"
echo "- 包含中文的文件: $FILES_WITH_CHINESE"
echo "- 有问题的文件: $FILES_WITH_ISSUES"

if [ $FILES_WITH_ISSUES -gt 0 ]; then
    echo "⚠️  发现 $FILES_WITH_ISSUES 个文件有问题，请查看报告详情"
    exit 1
else
    echo "✅ 所有文件规范良好"
    exit 0
fi