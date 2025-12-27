#!/bin/bash

# Cline规则切换脚本
# 用于按需加载插件规则，避免VS Code性能问题

CONFIG_FILE=".vscode/settings.json"
BACKUP_FILE=".vscode/settings.json.backup"

# 备份当前配置
backup_config() {
    if [ -f "$CONFIG_FILE" ]; then
        cp "$CONFIG_FILE" "$BACKUP_FILE"
        echo "✅ 配置已备份到: $BACKUP_FILE"
    else
        echo "❌ 配置文件不存在: $CONFIG_FILE"
        exit 1
    fi
}

# 恢复备份配置
restore_config() {
    if [ -f "$BACKUP_FILE" ]; then
        cp "$BACKUP_FILE" "$CONFIG_FILE"
        echo "✅ 配置已从备份恢复"
    else
        echo "❌ 备份文件不存在: $BACKUP_FILE"
    fi
}

# 只加载项目通用规则（性能最优）
load_only_global_rules() {
    backup_config
    
    cat > "$CONFIG_FILE" << 'EOF'
{
  // Cline配置 - 优化性能，减少规则加载
  "cline.rulesPaths": [
    "${workspaceFolder}/.clinerules" // 只加载项目通用规则
  ],
  // 规则加载策略 - 禁用自动合并，提高性能
  "cline.rulesLoadStrategy": "none",
  // 禁用开发者协同功能，减少开销
  "cline.collaboration": {
    "enabled": false,
    "shareRules": false,
    "syncOnOpen": false
  },
  // 性能优化配置
  "cline.performance": {
    "maxRules": 10, // 限制最大规则数量
    "cacheRules": true, // 启用规则缓存
    "lazyLoad": true // 启用懒加载
  }
}
EOF
    
    echo "✅ 已切换到：只加载项目通用规则（性能最优）"
    echo "   需要重新加载VS Code窗口才能生效"
}

# 加载TDengine和Modbus插件规则
load_tdengine_modbus() {
    backup_config
    
    cat > "$CONFIG_FILE" << 'EOF'
{
  // Cline配置 - 加载TDengine和Modbus插件规则
  "cline.rulesPaths": [
    "${workspaceFolder}/.clinerules",
    "${workspaceFolder}/apps/dgiot_tdengine/.clinerules",
    "${workspaceFolder}/apps/dgiot_modbus/.clinerules"
  ],
  "cline.rulesLoadStrategy": "merge",
  "cline.performance": {
    "maxRules": 15,
    "cacheRules": true,
    "lazyLoad": true
  }
}
EOF
    
    echo "✅ 已切换到：TDengine + Modbus插件规则"
    echo "   需要重新加载VS Code窗口才能生效"
}

# 加载视频相关插件规则
load_video_plugins() {
    backup_config
    
    cat > "$CONFIG_FILE" << 'EOF'
{
  // Cline配置 - 加载视频相关插件规则
  "cline.rulesPaths": [
    "${workspaceFolder}/.clinerules",
    "${workspaceFolder}/apps/dgiot_hikvision/.clinerules",
    "${workspaceFolder}/apps/dgiot_aidrive/.clinerules"
  ],
  "cline.rulesLoadStrategy": "merge",
  "cline.performance": {
    "maxRules": 15,
    "cacheRules": true,
    "lazyLoad": true
  }
}
EOF
    
    echo "✅ 已切换到：海康威视 + 智驱力插件规则"
    echo "   需要重新加载VS Code窗口才能生效"
}

# 加载所有插件规则（不推荐）
load_all_plugins() {
    backup_config
    
    cat > "$CONFIG_FILE" << 'EOF'
{
  // Cline配置 - 加载所有插件规则（可能导致性能问题）
  "cline.rulesPaths": [
    "${workspaceFolder}/.clinerules",
    "${workspaceFolder}/apps/dgiot_tdengine/.clinerules",
    "${workspaceFolder}/apps/dgiot_modbus/.clinerules",
    "${workspaceFolder}/apps/dgiot_drone/.clinerules",
    "${workspaceFolder}/apps/dgiot_hikvision/.clinerules",
    "${workspaceFolder}/apps/dgiot_aidrive/.clinerules",
    "${workspaceFolder}/apps/dgiot_cnooc/.clinerules"
  ],
  "cline.rulesLoadStrategy": "merge",
  "cline.performance": {
    "maxRules": 30,
    "cacheRules": true,
    "lazyLoad": true
  }
}
EOF
    
    echo "⚠️  已切换到：所有插件规则（可能导致VS Code变卡）"
    echo "   需要重新加载VS Code窗口才能生效"
}

# 显示当前配置
show_config() {
    echo "=== 当前Cline配置 ==="
    if [ -f "$CONFIG_FILE" ]; then
        grep -A 30 "cline.rulesPaths" "$CONFIG_FILE" || echo "未找到Cline配置"
    else
        echo "配置文件不存在: $CONFIG_FILE"
    fi
    echo ""
}

# 显示帮助信息
show_help() {
    echo "=== Cline规则切换脚本 ==="
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  global       只加载项目通用规则（性能最优）"
    echo "  tdmod        TDengine + Modbus插件规则"
    echo "  video        海康威视 + 智驱力插件规则"
    echo "  all          加载所有插件规则（不推荐）"
    echo "  show         显示当前配置"
    echo "  restore      从备份恢复配置"
    echo "  help         显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 global      # 切换到性能最优模式"
    echo "  $0 tdmod       # 切换到TDengine和Modbus开发"
    echo "  $0 video       # 切换到视频相关插件开发"
    echo ""
    echo "注意：切换后需要重新加载VS Code窗口才能生效"
    echo "      按 Ctrl+Shift+P，输入 'Developer: Reload Window'"
}

# 主程序
case "$1" in
    "global")
        load_only_global_rules
        ;;
    "tdmod")
        load_tdengine_modbus
        ;;
    "video")
        load_video_plugins
        ;;
    "all")
        load_all_plugins
        ;;
    "show")
        show_config
        ;;
    "restore")
        restore_config
        ;;
    "help"|"-h"|"--help"|"")
        show_help
        ;;
    *)
        echo "❌ 未知选项: $1"
        echo "使用 '$0 help' 查看帮助信息"
        exit 1
        ;;
esac

# 提示重新加载窗口
if [[ "$1" =~ ^(global|tdmod|video|all)$ ]]; then
    echo ""
    echo "=== 下一步操作 ==="
    echo "1. 重新加载VS Code窗口:"
    echo "   按 Ctrl+Shift+P，输入 'Developer: Reload Window'"
    echo ""
    echo "2. 检查Cline面板:"
    echo "   点击VS Code左侧的Cline图标"
    echo ""
    echo "3. 验证规则显示:"
    echo "   应该能看到相应的规则文件"
fi
