#!/bin/bash
# validate_rules.sh - 规则验证脚本
echo "=== DG-IoT规则体系验证 ==="
echo "验证时间: $(date)"
cd "$(dirname "$0")"
# 计算核心Rules文件（不包括模板和统计文档）
core_files="api_rules.md architecture_principles.md coding_standards.md development_rules.md INDEX.md protocol_layer_boundaries.md rule_validation.md security_rules.md"
file_count=$(echo "$core_files" | wc -w)
total_lines=0
for file in $core_files; do
    if [ -f "$file" ]; then
        lines=$(wc -l "$file" | awk '{print $1}')
        total_lines=$((total_lines + lines))
    fi
done
echo "核心Rules文件数量: $file_count"
echo "总行数: $total_lines"
# 核心标准：4-8个核心文件，总行数≤750行
if [ $file_count -ge 4 ] && [ $file_count -le 8 ] && [ $total_lines -le 750 ]; then
    echo "✅ 规则简洁性验证通过"
    exit 0
else
    echo "❌ 规则简洁性验证失败"
    exit 1
fi
