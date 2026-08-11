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
