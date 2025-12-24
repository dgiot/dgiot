#!/bin/bash
# validate_rules.sh - 规则验证脚本
echo "=== DG-IoT规则体系验证 ==="
echo "验证时间: $(date)"
cd "$(dirname "$0")"
file_count=$(ls -1 *.md 2>/dev/null | wc -l)
total_lines=$(wc -l *.md 2>/dev/null | grep total | awk "{print \$1}")
echo "文件数量: $file_count"
echo "总行数: ${total_lines:-0}"
# 调整标准：考虑完整的规则体系（核心规则+统计文档+指南+调试规范）
if [ $file_count -ge 4 ] && [ $file_count -le 12 ]; then
    echo "✅ 规则简洁性验证通过"
    exit 0
else
    echo "❌ 规则简洁性验证失败"
    exit 1
fi
