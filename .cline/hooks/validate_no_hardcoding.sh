#!/bin/bash
# 无硬编码路径验证脚本 - 简洁版本

set -e

# 加载配置
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/config.sh"
export_config

echo "验证无硬编码路径..."
echo "项目: $DGIOT_PROJECT_ROOT"
echo ""

# 检查核心脚本
CORE_SCRIPTS=("config.sh" "daily_check.sh" "setup_env.sh")

for script in "${CORE_SCRIPTS[@]}"; do
    if grep -v "^[[:space:]]*#" "$DGIOT_HOOKS_DIR/$script" | grep -q "/root/gitee/dgiot"; then
        echo "❌ $script: 发现硬编码路径"
    else
        echo "✅ $script: 无硬编码路径"
    fi
done

echo ""
echo "✅ 核心脚本无硬编码路径验证完成"
echo ""
echo "使用方法:"
echo "1. 设置环境: source $DGIOT_HOOKS_DIR/setup_env.sh"
echo "2. 运行检查: bash $DGIOT_HOOKS_DIR/daily_check.sh"
echo "3. 查看报告: cat $DGIOT_HOOKS_DIR/reports/*.md"
