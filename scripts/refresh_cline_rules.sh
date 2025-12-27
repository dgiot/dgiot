#!/bin/bash

# 刷新Cline规则脚本
echo "开始刷新Cline规则面板..."

# 1. 检查Cline扩展状态
echo "1. 检查Cline扩展状态..."
if code --list-extensions | grep -q "ai-henryalps.clinerules"; then
    echo "✅ Cline扩展已安装: ai-henryalps.clinerules"
else
    echo "❌ Cline扩展未安装"
    exit 1
fi

# 2. 检查规则目录
echo "2. 检查规则目录..."
echo "项目通用规则: .clinerules/"
ls -la .clinerules/*.md | wc -l
echo "插件特定规则:"
find apps -name ".clinerules" -type d | while read dir; do
    echo "  $dir: $(ls -la "$dir/" | grep README.md | wc -l)个规则文件"
done

# 3. 检查VS Code配置
echo "3. 检查VS Code配置..."
if [ -f ".vscode/settings.json" ]; then
    echo "✅ VS Code配置存在"
    grep -A5 "cline.rulesPaths" .vscode/settings.json
else
    echo "❌ VS Code配置不存在"
fi

# 4. 创建规则索引文件（帮助Cline发现规则）
echo "4. 创建规则索引文件..."
cat > .clinerules/INDEX.md << 'EOF'
# Cline规则索引

## 项目通用规则
- general_rules.md - 通用开发规则
- coding_standards.md - 编码规范
- api_management_rules.md - API接口管理规则
- security_rules.md - 安全规则
- private_rules_management.md - 私密规则管理指南

## 插件特定规则
EOF

# 添加插件规则到索引
find apps -name "README.md" -path "*/.clinerules/*" | while read file; do
    plugin=$(echo "$file" | cut -d'/' -f2)
    echo "- $plugin/.clinerules/README.md - $(basename "$plugin")插件规则" >> .clinerules/INDEX.md
done

echo "✅ 规则索引文件已创建"

# 5. 输出刷新指令
echo ""
echo "========================================"
echo "刷新Cline面板的几种方法："
echo ""
echo "方法1: 重新加载VS Code窗口"
echo "  按 Ctrl+Shift+P 然后输入 'Developer: Reload Window'"
echo ""
echo "方法2: 重启VS Code"
echo "  关闭并重新打开VS Code"
echo ""
echo "方法3: 手动触发规则刷新"
echo "  1. 打开Cline面板（通常在侧边栏）"
echo "  2. 查找刷新按钮或重新加载选项"
echo "  3. 如果支持，运行命令: Cline: Refresh Rules"
echo ""
echo "方法4: 检查Cline扩展设置"
echo "  1. 打开扩展面板 (Ctrl+Shift+X)"
echo "  2. 搜索 'Cline Rules'"
echo "  3. 检查配置是否正确"
echo "========================================"
echo ""
echo "刷新完成！请尝试以上方法之一。"
