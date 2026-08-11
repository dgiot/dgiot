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
