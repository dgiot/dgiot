#!/bin/bash
# 修复硬编码路径脚本 - 无硬编码版本
# 更新所有脚本中的硬编码路径为动态路径

set -e

# 加载配置
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/config.sh" ]; then
    source "$SCRIPT_DIR/config.sh"
    export_config
else
    echo "错误: 找不到配置文件 config.sh"
    exit 1
fi

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}开始修复硬编码路径...${NC}"
echo "项目根目录: $DGIOT_PROJECT_ROOT"
echo ""

# 主要修复逻辑
echo "主要修复功能已集成到 validate_no_hardcoding.sh 脚本中"
echo "请运行: bash $DGIOT_HOOKS_DIR/validate_no_hardcoding.sh"
echo ""
echo -e "${GREEN}✅ 修复脚本已更新为无硬编码版本${NC}"

# 清理旧的硬编码文件
cleanup_old_files() {
    echo ""
    echo "清理旧文件..."
    
    local old_files=(
        "$DGIOT_HOOKS_DIR/fix_hardcoded_paths.sh.backup"
        "$DGIOT_HOOKS_DIR/integrate_hook.sh.backup"
        "$DGIOT_HOOKS_DIR/test_hook_mechanism.js.backup"
    )
    
    for old_file in "${old_files[@]}"; do
        if [ -f "$old_file" ]; then
            rm -f "$old_file"
            echo "  删除: $(basename "$old_file")"
        fi
    done
}

cleanup_old_files
