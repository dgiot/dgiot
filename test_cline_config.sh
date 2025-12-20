#!/bin/bash

echo "=== Cline配置验证脚本 ==="
echo "执行时间: $(date)"
echo ""

# 1. 检查Cline扩展是否安装
echo "1. 检查Cline扩展状态..."
if code --list-extensions | grep -q "saoudrizwan.claude-dev"; then
    echo "   ✅ Cline扩展已安装"
else
    echo "   ❌ Cline扩展未安装"
    echo "   请安装扩展: saoudrizwan.claude-dev"
fi

echo ""

# 2. 检查配置文件
echo "2. 检查配置文件..."
if [ -f ".vscode/settings.json" ]; then
    echo "   ✅ .vscode/settings.json 存在"
    
    # 检查Cline配置
    if grep -q "cline.rulesPaths" .vscode/settings.json; then
        echo "   ✅ Cline配置存在"
        
        # 显示配置的路径
        echo "   配置的规则路径:"
        grep -A 10 "cline.rulesPaths" .vscode/settings.json | grep "\"\${workspaceFolder}" | sed 's/^/      /'
    else
        echo "   ❌ Cline配置不存在"
    fi
else
    echo "   ❌ .vscode/settings.json 不存在"
fi

echo ""

# 3. 检查插件规则文件
echo "3. 检查插件规则文件..."
PLUGIN_RULES=$(find apps/ -name "README.md" -path "*/.clinerules/*" -type f 2>/dev/null)
COUNT=$(echo "$PLUGIN_RULES" | wc -l)

if [ "$COUNT" -gt 0 ]; then
    echo "   ✅ 找到 $COUNT 个插件规则文件:"
    echo "$PLUGIN_RULES" | sed 's/^/      /'
else
    echo "   ❌ 未找到插件规则文件"
fi

echo ""

# 4. 验证路径匹配
echo "4. 验证路径匹配..."
echo "   配置中的路径应该指向以下实际文件:"
paths=(
    ".clinerules"
    "apps/dgiot_tdengine/.clinerules"
    "apps/dgiot_modbus/.clinerules"
    "apps/dgiot_drone/.clinerules"
    "apps/dgiot_hikvision/.clinerules"
    "apps/dgiot_aidrive/.clinerules"
    "apps/dgiot_cnooc/.clinerules"
)

for path in "${paths[@]}"; do
    if [ -d "$path" ]; then
        echo "   ✅ $path 目录存在"
        
        # 检查目录中是否有README.md
        if [ -f "$path/README.md" ]; then
            echo "      ✅ README.md 文件存在"
        else
            echo "      ⚠️  README.md 文件不存在"
        fi
    else
        echo "   ❌ $path 目录不存在"
    fi
done

echo ""

# 5. 建议操作
echo "5. 建议操作:"
echo "   a) 重新加载VS Code窗口:"
echo "      按 Ctrl+Shift+P，输入 'Developer: Reload Window'"
echo ""
echo "   b) 检查Cline面板:"
echo "      1. 点击VS Code左侧的Cline图标"
echo "      2. 查看规则面板是否显示插件规则"
echo ""
echo "   c) 如果仍然看不到插件规则:"
echo "      1. 检查Cline扩展的控制台输出"
echo "      2. 尝试重启VS Code"
echo ""

echo "=== 验证完成 ==="
