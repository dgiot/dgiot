#!/bin/bash

# 激活所有 CodeBuddy 技能
# 包括插件内部的技能和主项目的技能

set -e

echo "=== 激活所有 CodeBuddy 技能 ==="
echo "开始时间: $(date)"
echo ""

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 检查 CodeBuddy 状态
check_codebuddy_status() {
    echo "1. 检查 CodeBuddy 状态..."
    if pgrep -f "codebuddy" > /dev/null; then
        echo -e "  ${YELLOW}⚠ CodeBuddy 正在运行中${NC}"
        echo "  建议在激活后重启 CodeBuddy 以加载新技能"
    else
        echo -e "  ${GREEN}✓ CodeBuddy 未在运行${NC}"
        echo "  技能将在下次启动时自动加载"
    fi
}

# 检查 Git 排除规则
check_git_exclusions() {
    echo ""
    echo "2. 检查 Git 排除规则..."
    
    # 检查主项目 .codebuddy 排除
    if grep -q "^\.codebuddy/" /home/gitee/dgiot/.gitignore; then
        echo -e "  ${GREEN}✓ 主项目 .codebuddy 目录已排除${NC}"
    else
        echo -e "  ${RED}✗ 主项目 .codebuddy 目录未排除${NC}"
        echo "  正在修复..."
        echo ".codebuddy/" >> /home/gitee/dgiot/.gitignore
        echo -e "  ${GREEN}✓ 已修复${NC}"
    fi
    
    # 检查插件 .codebuddy 排除
    if grep -q "^apps/dgiot_uav/\.codebuddy/" /home/gitee/dgiot/.gitignore; then
        echo -e "  ${GREEN}✓ 插件 .codebuddy 目录已排除${NC}"
    else
        echo -e "  ${RED}✗ 插件 .codebuddy 目录未排除${NC}"
        echo "  正在修复..."
        echo "apps/dgiot_uav/.codebuddy/" >> /home/gitee/dgiot/.gitignore
        echo -e "  ${GREEN}✓ 已修复${NC}"
    fi
    
    # 检查插件整体排除（不应该排除整个插件）
    if grep -q "^apps/dgiot_uav$" /home/gitee/dgiot/.gitignore; then
        echo -e "  ${RED}✗ 警告: 整个 apps/dgiot_uav 目录被排除${NC}"
        echo "  这会导致插件源代码无法提交到仓库"
        echo "  建议修改为只排除敏感目录"
    else
        echo -e "  ${GREEN}✓ 插件目录未被整体排除${NC}"
    fi
}

# 激活主项目技能
activate_main_skills() {
    echo ""
    echo "3. 激活主项目技能..."
    
    if [ -f "/home/gitee/dgiot/activate_main_codebuddy.sh" ]; then
        echo "  运行主项目技能激活脚本..."
        /home/gitee/dgiot/activate_main_codebuddy.sh
    else
        echo -e "  ${YELLOW}⚠ 主项目技能激活脚本不存在${NC}"
        echo "  跳过主项目技能激活"
    fi
}

# 激活插件技能
activate_plugin_skills() {
    echo ""
    echo "4. 激活插件技能..."
    
    if [ -f "/home/gitee/dgiot/activate_codebuddy_skills.sh" ]; then
        echo "  运行插件技能激活脚本..."
        /home/gitee/dgiot/activate_codebuddy_skills.sh
    else
        echo -e "  ${YELLOW}⚠ 插件技能激活脚本不存在${NC}"
        echo "  跳过插件技能激活"
    fi
}

# 检查 Git 状态
check_git_status() {
    echo ""
    echo "5. 检查 Git 状态..."
    
    cd /home/gitee/dgiot
    echo "  当前分支: $(git branch --show-current)"
    
    # 检查是否有应该排除的文件在 Git 中
    EXCLUDED_IN_GIT=$(git ls-files | grep -E "(\.codebuddy|apps/dgiot_uav/\.)" | head -10)
    
    if [ -n "$EXCLUDED_IN_GIT" ]; then
        echo -e "  ${RED}✗ 发现应该排除的文件在 Git 中:${NC}"
        echo "$EXCLUDED_IN_GIT"
        echo ""
        echo "  建议执行以下操作:"
        echo "  git rm --cached -r .codebuddy/"
        echo "  git rm --cached -r apps/dgiot_uav/.codebuddy/"
        echo "  git rm --cached -r apps/dgiot_uav/.codeartsdoer/"
        echo "  然后提交更改"
    else
        echo -e "  ${GREEN}✓ 没有发现应该排除的文件在 Git 中${NC}"
    fi
}

# 创建重启脚本
create_restart_script() {
    echo ""
    echo "6. 创建重启脚本..."
    
    cat > /root/.codebuddy/restart_codebuddy.sh << 'EOF'
#!/bin/bash

echo "重启 CodeBuddy 以加载新技能..."
echo "当前时间: $(date)"

# 检查 CodeBuddy 是否在运行
if pgrep -f "codebuddy" > /dev/null; then
    echo "正在停止 CodeBuddy..."
    pkill -f codebuddy
    sleep 2
fi

echo "CodeBuddy 已停止"
echo "请手动重新启动 CodeBuddy 以加载新技能"
echo ""
echo "技能列表:"
echo "1. uav_test_management - UAV测试管理"
echo "2. uav_protocol_analysis - UAV协议分析"
echo ""
echo "测试命令:"
echo "  use_skill uav_test_management"
echo "  use_skill uav_protocol_analysis"
EOF

    chmod +x /root/.codebuddy/restart_codebuddy.sh
    echo -e "  ${GREEN}✓ 重启脚本已创建: /root/.codebuddy/restart_codebuddy.sh${NC}"
}

# 创建最终指南
create_final_guide() {
    echo ""
    echo "7. 创建最终指南..."
    
    cat > /home/gitee/dgiot/CODEDBUDDY_FINAL_GUIDE.md << 'EOF'
# CodeBuddy 技能激活完成指南

## 激活状态

### ✅ 已完成的工作
1. **Git 排除规则已修复**
   - 主项目 `.codebuddy/` 目录已排除
   - 插件 `apps/dgiot_uav/.codebuddy/` 目录已排除
   - 插件 `apps/dgiot_uav/.codeartsdoer/` 目录已排除
   - 只排除敏感目录，不排除整个插件

2. **技能已激活**
   - 主项目技能: `/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/`
   - 插件技能: `/root/.codebuddy/skills-marketplace/skills/dgiot_uav/`

3. **配置文件已复制**
   - `workflow.json` - 工作流程配置
   - `.codebuddy-project.json` - 项目配置

## 使用技能

### 基本使用方法
```bash
# 在 CodeBuddy 对话中使用
use_skill uav_test_management
use_skill uav_protocol_analysis
```

### 关键词触发
- "无人机测试" → 推荐 `uav_test_management`
- "协议解析" → 推荐 `uav_protocol_analysis`
- "工位绑定" → 推荐 `uav_test_management`
- "EB90调试" → 推荐 `uav_protocol_analysis`

## Git 管理

### 当前状态
检查是否有应该排除的文件在 Git 中:
```bash
cd /home/gitee/dgiot
git ls-files | grep -E "(\.codebuddy|apps/dgiot_uav/\.)"
```

### 如果发现误添加的文件
```bash
# 从 Git 中移除应该排除的目录
git rm --cached -r .codebuddy/
git rm --cached -r apps/dgiot_uav/.codebuddy/
git rm --cached -r apps/dgiot_uav/.codeartsdoer/
git rm --cached -r apps/dgiot_uav/priv/sensitive/

# 提交更改
git commit -m "chore: 移除应该排除的敏感目录"
```

### 验证 .gitignore
```bash
# 检查排除规则
grep -E "(\.codebuddy|apps/dgiot_uav)" /home/gitee/dgiot/.gitignore
```

## 重启 CodeBuddy

### 如果需要重启
```bash
# 运行重启脚本
/root/.codebuddy/restart_codebuddy.sh

# 或者手动重启
pkill -f codebuddy
# 等待几秒后重新启动 CodeBuddy
```

### 验证技能激活
```bash
# 运行验证脚本
/root/.codebuddy/verify_main_skills.sh

# 测试技能
# 在 CodeBuddy 中执行:
use_skill uav_test_management
```

## 维护指南

### 更新技能
1. 修改技能源文件:
   - 主项目: `/home/gitee/dgiot/.codebuddy/skills/`
   - 插件: `/home/gitee/dgiot/apps/dgiot_uav/.codebuddy/skills/`

2. 重新运行激活脚本:
   ```bash
   ./activate_all_codebuddy.sh
   ```

3. 重启 CodeBuddy

### 添加新技能
1. 在源目录创建技能目录和 `SKILL.md` 文件
2. 运行激活脚本
3. 更新索引文件
4. 重启 CodeBuddy

## 故障排除

### 问题1: 技能未找到
**解决方案**:
```bash
# 重新激活
./activate_all_codebuddy.sh

# 重启 CodeBuddy
pkill -f codebuddy
```

### 问题2: Git 仍然跟踪排除的文件
**解决方案**:
```bash
# 从 Git 中移除
git rm --cached -r .codebuddy/
git rm --cached -r apps/dgiot_uav/.codebuddy/

# 提交更改
git commit -m "chore: 修复排除规则"
```

### 问题3: 插件源代码被排除
**症状**: `apps/dgiot_uav/src/` 等目录无法提交
**解决方案**:
检查 `.gitignore` 中是否有 `apps/dgiot_uav`（排除整个插件）
应该只有:
```
apps/dgiot_uav/.codebuddy/
apps/dgiot_uav/.codeartsdoer/
apps/dgiot_uav/priv/sensitive/
```

## 文件位置参考

### 源文件
- 主项目技能: `/home/gitee/dgiot/.codebuddy/`
- 插件技能: `/home/gitee/dgiot/apps/dgiot_uav/.codebuddy/`
- Git排除配置: `/home/gitee/dgiot/.gitignore`

### 目标文件 (CodeBuddy)
- 主项目技能: `/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/`
- 插件技能: `/root/.codebuddy/skills-marketplace/skills/dgiot_uav/`
- 配置文件: `/root/.codebuddy/`

## 联系支持

如有问题，请检查:
1. Git 状态: `git status`
2. 技能文件: `ls -la /root/.codebuddy/skills-marketplace/skills/dgiot_uav*/`
3. 排除规则: `grep -n "dgiot_uav" .gitignore`

---
**指南版本**: 1.0.0
**生成时间**: $(date)
**激活状态**: ✅ 完成
EOF

    echo -e "  ${GREEN}✓ 最终指南已创建: /home/gitee/dgiot/CODEDBUDDY_FINAL_GUIDE.md${NC}"
}

# 执行所有步骤
main() {
    check_codebuddy_status
    check_git_exclusions
    activate_main_skills
    activate_plugin_skills
    check_git_status
    create_restart_script
    create_final_guide
    
    echo ""
    echo "========================================"
    echo -e "${GREEN}✅ 所有 CodeBuddy 技能激活完成！${NC}"
    echo "========================================"
    echo ""
    echo "下一步操作:"
    echo "1. 查看最终指南: cat /home/gitee/dgiot/CODEDBUDDY_FINAL_GUIDE.md"
    echo "2. 重启 CodeBuddy: /root/.codebuddy/restart_codebuddy.sh"
    echo "3. 测试技能: use_skill uav_test_management"
    echo ""
    echo "重要提醒:"
    echo "• 检查是否有应该排除的文件在 Git 中"
    echo "• 确保插件源代码可以正常提交"
    echo "• 测试技能功能是否正常"
    echo ""
    echo "结束时间: $(date)"
}

# 运行主函数
main