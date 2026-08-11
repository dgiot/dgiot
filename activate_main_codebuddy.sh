#!/bin/bash

# 激活主项目 .codebuddy 目录下的技能
# 这个脚本专门处理 /home/gitee/dgiot/.codebuddy 目录下的技能

set -e

echo "=== 激活主项目 CodeBuddy 技能 ==="
echo "开始时间: $(date)"
echo ""

# 检查源目录和目标目录
SOURCE_DIR="/home/gitee/dgiot/.codebuddy"
CODEBUDDY_HOME="/root/.codebuddy"

if [ ! -d "$SOURCE_DIR" ]; then
    echo "错误: 源目录不存在: $SOURCE_DIR"
    exit 1
fi

if [ ! -d "$CODEBUDDY_HOME" ]; then
    echo "错误: CodeBuddy 主目录不存在: $CODEBUDDY_HOME"
    exit 1
fi

# 1. 复制工作流程配置
echo "1. 复制工作流程配置..."
if [ -f "$SOURCE_DIR/workflow.json" ]; then
    cp "$SOURCE_DIR/workflow.json" "$CODEBUDDY_HOME/"
    echo "  已复制: workflow.json"
else
    echo "  警告: workflow.json 不存在"
fi

# 2. 复制技能文件
echo "2. 复制技能文件..."
TARGET_SKILLS_DIR="$CODEBUDDY_HOME/skills-marketplace/skills/dgiot_uav_main"
mkdir -p "$TARGET_SKILLS_DIR"

if [ -d "$SOURCE_DIR/skills/uav_test_management" ]; then
    cp -r "$SOURCE_DIR/skills/uav_test_management" "$TARGET_SKILLS_DIR/"
    echo "  已复制: uav_test_management"
fi

if [ -d "$SOURCE_DIR/skills/uav_protocol_analysis" ]; then
    cp -r "$SOURCE_DIR/skills/uav_protocol_analysis" "$TARGET_SKILLS_DIR/"
    echo "  已复制: uav_protocol_analysis"
fi

# 3. 复制项目配置文件
echo "3. 复制项目配置文件..."
if [ -f "/home/gitee/dgiot/.codebuddy-project.json" ]; then
    cp "/home/gitee/dgiot/.codebuddy-project.json" "$CODEBUDDY_HOME/"
    echo "  已复制: .codebuddy-project.json"
fi

# 4. 创建索引文件
echo "4. 创建技能索引..."
cat > "$TARGET_SKILLS_DIR/INDEX.md" << 'EOF'
# DG-IoT UAV 主项目技能索引

## 可用技能

### 1. UAV 测试管理技能 (uav_test_management)
**位置**: 主项目 .codebuddy 目录
**描述**: 专注于超近距无人机自动化测试产线的全面管理

**核心功能**:
- 测试项配置与管理
- 工位与设备绑定
- 自动化测试执行
- 测试结果分析
- PLC通信调试

### 2. UAV 协议分析技能 (uav_protocol_analysis)
**位置**: 主项目 .codebuddy 目录
**描述**: 专注于EB90协议栈的解析、编码、调试和优化

**核心功能**:
- EB90协议解析
- 遥测数据处理
- CRC16校验计算
- 协议兼容性测试
- 测试报文生成

## 使用说明

### 激活状态
这些技能来自主项目的 `.codebuddy` 目录，已经通过激活脚本复制到 CodeBuddy。

### 使用方法
```bash
# 在 CodeBuddy 中使用技能
use_skill uav_test_management
use_skill uav_protocol_analysis
```

### 文件来源
- 源目录: `/home/gitee/dgiot/.codebuddy/skills/`
- 目标目录: `/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/`

## 维护指南

### 更新技能
1. 修改 `/home/gitee/dgiot/.codebuddy/skills/` 中的技能文件
2. 运行激活脚本: `./activate_main_codebuddy.sh`
3. 重启 CodeBuddy 以加载更新

### 添加新技能
1. 在源目录创建新的技能目录
2. 运行激活脚本
3. 更新索引文件

## 排除规则
主项目的 `.codebuddy` 目录已在 `.gitignore` 中排除:
```
.codebuddy/
```

因此这些技能文件不会提交到 Git 仓库。

---
**激活时间**: $(date)
**技能版本**: 1.0.0
**维护者**: UAV项目团队
EOF

echo "  已创建: INDEX.md"

# 5. 创建验证脚本
echo "5. 创建验证脚本..."
cat > "$CODEBUDDY_HOME/verify_main_skills.sh" << 'EOF'
#!/bin/bash

echo "=== 验证主项目 CodeBuddy 技能 ==="

echo "1. 检查技能目录..."
if [ -d "/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main" ]; then
    echo "  ✓ 技能目录存在"
    echo "  目录内容:"
    ls -la "/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/"
else
    echo "  ✗ 技能目录不存在"
fi

echo ""
echo "2. 检查技能文件..."
if [ -f "/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/uav_test_management/SKILL.md" ]; then
    echo "  ✓ UAV测试管理技能文件存在"
else
    echo "  ✗ UAV测试管理技能文件缺失"
fi

if [ -f "/root/.codebuddy/skills-marketplace/skills/dgiot_uav_main/uav_protocol_analysis/SKILL.md" ]; then
    echo "  ✓ UAV协议分析技能文件存在"
else
    echo "  ✗ UAV协议分析技能文件缺失"
fi

echo ""
echo "3. 检查配置文件..."
if [ -f "/root/.codebuddy/workflow.json" ]; then
    echo "  ✓ 工作流程配置文件存在"
else
    echo "  ✗ 工作流程配置文件缺失"
fi

if [ -f "/root/.codebuddy/.codebuddy-project.json" ]; then
    echo "  ✓ 项目配置文件存在"
else
    echo "  ✗ 项目配置文件缺失"
fi

echo ""
echo "=== 验证完成 ==="
echo "如果所有检查都通过，技能已成功激活。"
echo "请在 CodeBuddy 中测试: use_skill uav_test_management"
EOF

chmod +x "$CODEBUDDY_HOME/verify_main_skills.sh"
echo "  已创建: verify_main_skills.sh"

# 完成信息
echo ""
echo "=== 激活完成 ==="
echo ""
echo "主项目 CodeBuddy 技能已成功激活！"
echo ""
echo "目录结构:"
echo "  源目录: $SOURCE_DIR"
echo "  目标目录: $TARGET_SKILLS_DIR"
echo ""
echo "验证步骤:"
echo "  1. 运行验证脚本: $CODEBUDDY_HOME/verify_main_skills.sh"
echo "  2. 重启 CodeBuddy: pkill -f codebuddy"
echo "  3. 测试技能: use_skill uav_test_management"
echo ""
echo "结束时间: $(date)"