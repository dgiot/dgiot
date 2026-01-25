#!/bin/bash
# 环境设置脚本
# 设置无硬编码的环境变量

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 加载配置
source "$SCRIPT_DIR/config.sh"
export_config

echo "环境变量已设置:"
echo "  DGIOT_PROJECT_ROOT=$DGIOT_PROJECT_ROOT"
echo "  DGIOT_HOOKS_DIR=$DGIOT_HOOKS_DIR"
echo "  DGIOT_CONFIG_DIR=$DGIOT_CONFIG_DIR"
echo ""
echo "添加到bashrc:"
echo "  echo 'source $DGIOT_HOOKS_DIR/setup_env.sh' >> ~/.bashrc"
echo ""
echo "立即生效:"
echo "  source $DGIOT_HOOKS_DIR/setup_env.sh"
