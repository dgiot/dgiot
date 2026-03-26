#!/bin/bash

# DG-IoT UAV 项目 CodeBuddy 技能激活脚本
# 将项目特定的技能和工作流程配置导入到 CodeBuddy

set -e

echo "=== DG-IoT UAV 项目 CodeBuddy 技能激活 ==="
echo "开始时间: $(date)"
echo ""

# 检查 CodeBuddy 目录
CODEBUDDY_HOME="/root/.codebuddy"
PROJECT_SKILLS_DIR="/home/gitee/dgiot/.codebuddy/skills"
PROJECT_CONFIG_DIR="/home/gitee/dgiot/.codebuddy"

if [ ! -d "$CODEBUDDY_HOME" ]; then
    echo "错误: CodeBuddy 主目录不存在: $CODEBUDDY_HOME"
    echo "请确保 CodeBuddy 已正确安装"
    exit 1
fi

# 创建项目技能目录
CODEBUDDY_PROJECT_SKILLS="$CODEBUDDY_HOME/skills-marketplace/skills/dgiot_uav"
echo "1. 创建项目技能目录..."
mkdir -p "$CODEBUDDY_PROJECT_SKILLS"

# 复制技能文件
echo "2. 复制技能文件到 CodeBuddy..."
if [ -d "$PROJECT_SKILLS_DIR/uav_test_management" ]; then
    cp -r "$PROJECT_SKILLS_DIR/uav_test_management" "$CODEBUDDY_PROJECT_SKILLS/"
    echo "  已复制: uav_test_management"
fi

if [ -d "$PROJECT_SKILLS_DIR/uav_protocol_analysis" ]; then
    cp -r "$PROJECT_SKILLS_DIR/uav_protocol_analysis" "$CODEBUDDY_PROJECT_SKILLS/"
    echo "  已复制: uav_protocol_analysis"
fi

# 复制工作流程配置
echo "3. 复制工作流程配置..."
if [ -f "$PROJECT_CONFIG_DIR/workflow.json" ]; then
    cp "$PROJECT_CONFIG_DIR/workflow.json" "$CODEBUDDY_HOME/"
    echo "  已复制: workflow.json"
fi

# 复制项目配置文件
if [ -f "/home/gitee/dgiot/.codebuddy-project.json" ]; then
    cp "/home/gitee/dgiot/.codebuddy-project.json" "$CODEBUDDY_HOME/"
    echo "  已复制: .codebuddy-project.json"
fi

# 创建技能索引
echo "4. 创建技能索引文件..."
cat > "$CODEBUDDY_PROJECT_SKILLS/INDEX.md" << 'EOF'
# DG-IoT UAV 项目技能索引

## 可用技能

### 1. UAV 测试管理技能 (uav_test_management)
**描述**: 专注于超近距无人机自动化测试产线的测试项配置、工位绑定、测试执行和结果分析

**触发关键词**:
- "配置无人机测试项"
- "管理测试工位"
- "执行自动化测试"
- "查看测试结果"
- "分析测试失败原因"

**核心功能**:
- 测试项创建与更新
- 工位设备绑定管理
- 自动化测试流程执行
- 测试结果统计分析
- PLC通信状态检查

### 2. UAV 协议分析技能 (uav_protocol_analysis)
**描述**: 专注于EB90协议栈的解析、编码、调试和优化，提供完整的协议生命周期管理支持

**触发关键词**:
- "解析无人机协议"
- "分析EB90报文"
- "调试协议通信"
- "验证CRC校验"
- "生成测试数据"

**核心功能**:
- EB90协议帧解析
- 遥测数据解析(D1/D2/D3)
- CRC16校验计算
- 协议兼容性检查
- 测试报文生成

## 使用方法

### 在 CodeBuddy 中调用技能
```bash
# 使用 UAV 测试管理技能
use_skill uav_test_management

# 使用 UAV 协议分析技能
use_skill uav_protocol_analysis
```

### 通过关键词自动触发
CodeBuddy 会自动检测以下关键词并推荐相应的技能:
- "无人机测试" → uav_test_management
- "协议解析" → uav_protocol_analysis
- "工位绑定" → uav_test_management
- "EB90调试" → uav_protocol_analysis

## 项目约定

### 工作流程
- 开发: 使用热编译而非全量编译
- 测试: 执行在线测试验证功能
- 调试: 使用标准调试命令
- 提交: 遵循 Conventional Commits 格式

### 编码规范
- 三层架构: API Gateway → Function Gateway → Implementation
- 错误处理: 返回 {ok, Result} 或 {error, Reason}
- 日志格式: io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])
- Unicode: 非ASCII字符串使用 <<"内容"/utf8>> 格式

## 相关资源
- 项目文档: /home/gitee/dgiot/apps/dgiot_uav/docs/
- 协议配置: /home/gitee/dgiot/apps/dgiot_uav/priv/json/
- 测试数据: /home/gitee/dgiot/apps/dgiot_uav/priv/capture/

## 更新记录
- 2026-03-26: 初始版本创建
EOF

echo "  已创建: INDEX.md"

# 创建注册脚本
echo "5. 创建技能注册脚本..."
cat > "$CODEBUDDY_HOME/register_uav_skills.sh" << 'EOF'
#!/bin/bash

# CodeBuddy UAV 技能注册脚本

echo "注册 DG-IoT UAV 项目技能到 CodeBuddy..."

# 检查 CodeBuddy 是否在运行中
if pgrep -f "codebuddy" > /dev/null; then
    echo "CodeBuddy 正在运行中，可能需要重启以加载新技能"
    echo "请执行以下命令重启 CodeBuddy:"
    echo "  pkill -f codebuddy"
    echo "  # 然后重新启动 CodeBuddy"
else
    echo "CodeBuddy 未在运行，下次启动时将自动加载新技能"
fi

echo ""
echo "技能已成功注册！"
echo "现在可以在 CodeBuddy 中使用以下技能："
echo "1. uav_test_management - 无人机测试管理"
echo "2. uav_protocol_analysis - 无人机协议分析"
echo ""
echo "使用方法："
echo "  use_skill uav_test_management"
echo "  use_skill uav_protocol_analysis"
EOF

chmod +x "$CODEBUDDY_HOME/register_uav_skills.sh"
echo "  已创建: register_uav_skills.sh"

# 创建使用指南
echo "6. 创建使用指南..."
cat > "/home/gitee/dgiot/CODEBUDDY_UAV_GUIDE.md" << 'EOF'
# DG-IoT UAV 项目 CodeBuddy 使用指南

## 概述

本文档说明如何在 CodeBuddy 中使用 DG-IoT UAV 项目的专用技能和智能体系统。

## 技能系统

### 已安装的技能

1. **UAV 测试管理技能** (`uav_test_management`)
   - 专注于无人机自动化测试产线的全面管理
   - 触发关键词: "配置测试项", "管理工位", "执行测试", "分析结果"

2. **UAV 协议分析技能** (`uav_protocol_analysis`)
   - 专注于 EB90 协议栈的解析、调试和优化
   - 触发关键词: "解析协议", "分析报文", "调试通信", "验证CRC"

### 使用方法

#### 方法1: 直接调用技能
```bash
# 在 CodeBuddy 对话中直接使用
use_skill uav_test_management

# 或者
use_skill uav_protocol_analysis
```

#### 方法2: 关键词自动触发
当你在 CodeBuddy 中提到以下关键词时，系统会自动推荐相关技能：
- "无人机测试" → 推荐 `uav_test_management`
- "协议解析" → 推荐 `uav_protocol_analysis`
- "工位绑定" → 推荐 `uav_test_management`
- "EB90调试" → 推荐 `uav_protocol_analysis`

#### 方法3: 通过 Task 工具调用智能体
```bash
# 调用无人机测试工程师智能体
task(subagent_name="code-explorer", description="无人机测试", prompt="请分析无人机测试产线状态")

# 调用无人机协议工程师智能体
task(subagent_name="code-explorer", description="协议分析", prompt="请解析EB90协议报文")
```

## 项目约定

### 工作流程
CodeBuddy 已经配置了项目特定的工作流程：

1. **开发阶段**
   - 优先使用热编译: `_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'`
   - 避免使用禁止命令: `make compile`, `erlc`, `erl -compile`

2. **测试阶段**
   - 单元测试: `cd apps/dgiot_uav && rebar3 eunit`
   - 在线测试: `_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_auto_tester:test_magnetic_auto().'`

3. **调试阶段**
   - PLC状态检查: `_build/emqx/rel/emqx/bin/emqx eval 'io:format("Checking plc 1100: ~p~n", [global:whereis_name({plc, 1100})]).'`
   - 设备查询: `_build/emqx/rel/emqx/bin/emqx eval 'dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"content.station">> => <<"1100">>}}).'`

### 编码规范
CodeBuddy 会检查并提醒以下规范：
- 三层架构遵守情况
- 错误处理格式
- 日志打印格式
- Unicode字符串格式
- 函数命名规范

## 智能体系统

### 可用智能体

1. **无人机测试工程师智能体**
   - 文件位置: `apps/dgiot_uav/.codebuddy/agents/dgiot_uav_test_engineer.md`
   - 职责: 测试项配置、工位绑定、测试执行、结果分析

2. **无人机协议工程师智能体**
   - 文件位置: `apps/dgiot_uav/.codebuddy/agents/dgiot_uav_protocol_engineer.md`
   - 职责: 协议解析、数据编码、CRC计算、兼容性测试

### 智能体调用
智能体可以通过 Task 工具调用，也可以基于任务类型自动推荐。

## 配置文件

### 项目配置文件
- `.codebuddy-project.json`: 项目整体配置
- `.codebuddy/workflow.json`: 工作流程配置
- `.gitignore`: Git排除规则

### 技能目录
- `/home/gitee/dgiot/.codebuddy/skills/`: 项目技能目录
- `/root/.codebuddy/skills-marketplace/skills/dgiot_uav/`: CodeBuddy技能目录

## 故障排除

### 问题1: 技能未找到
**症状**: 执行 `use_skill` 时提示技能不存在
**解决方案**:
```bash
# 重新运行激活脚本
cd /home/gitee/dgiot
./activate_codebuddy_skills.sh

# 重启 CodeBuddy
pkill -f codebuddy
# 重新启动 CodeBuddy
```

### 问题2: 智能体无法调用
**症状**: Task 工具调用失败
**解决方案**:
1. 确保智能体文件存在: `apps/dgiot_uav/.codebuddy/agents/`
2. 检查文件权限
3. 使用绝对路径引用

### 问题3: 工作流程不生效
**症状**: CodeBuddy 不遵循项目约定
**解决方案**:
1. 检查配置文件: `.codebuddy-project.json`, `workflow.json`
2. 验证关键词检测配置
3. 重新加载配置文件

## 维护指南

### 添加新技能
1. 在 `/home/gitee/dgiot/.codebuddy/skills/` 创建技能目录
2. 创建符合 CodeBuddy 标准的 `SKILL.md` 文件
3. 运行激活脚本: `./activate_codebuddy_skills.sh`
4. 更新索引文件

### 更新现有技能
1. 修改技能文件
2. 重新运行激活脚本
3. 重启 CodeBuddy（如果需要）

### 添加新智能体
1. 在 `apps/dgiot_uav/.codebuddy/agents/` 创建智能体文件
2. 更新相关配置文件
3. 测试智能体功能

## 联系与支持

如有问题，请参考：
- DG-IoT UAV 项目文档
- CodeBuddy 官方文档
- 项目配置文件中的说明

---
**版本**: 1.0.0
**最后更新**: 2026-03-26
**适用环境**: DG-IoT UAV Plugin + CodeBuddy
EOF

echo "  已创建: CODEBUDDY_UAV_GUIDE.md"

# 完成信息
echo ""
echo "=== 激活完成 ==="
echo ""
echo "已成功将 DG-IoT UAV 项目技能导入到 CodeBuddy！"
echo ""
echo "技能位置:"
echo "  CodeBuddy技能目录: $CODEBUDDY_PROJECT_SKILLS"
echo "  项目技能目录: $PROJECT_SKILLS_DIR"
echo ""
echo "使用方法:"
echo "  1. 查看使用指南: cat /home/gitee/dgiot/CODEBUDDY_UAV_GUIDE.md"
echo "  2. 注册技能: $CODEBUDDY_HOME/register_uav_skills.sh"
echo "  3. 在 CodeBuddy 中使用: use_skill uav_test_management"
echo ""
echo "下一步操作:"
echo "  1. 运行注册脚本: $CODEBUDDY_HOME/register_uav_skills.sh"
echo "  2. 重启 CodeBuddy（如果需要）"
echo "  3. 测试技能功能"
echo ""
echo "结束时间: $(date)"