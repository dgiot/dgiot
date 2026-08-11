# 测试 CodeBuddy UAV 项目技能

## 测试环境准备

### 1. 检查 CodeBuddy 状态
```bash
# 检查 CodeBuddy 是否运行
pgrep -f "codebuddy"

# 如果 CodeBuddy 在运行，可以重启以加载新技能
pkill -f codebuddy
# 然后重新启动 CodeBuddy
```

### 2. 验证技能文件
```bash
# 检查技能是否已正确安装
ls -la /root/.codebuddy/skills-marketplace/skills/dgiot_uav/

# 检查技能文件内容
cat /root/.codebuddy/skills-marketplace/skills/dgiot_uav/uav_test_management/SKILL.md | head -20
cat /root/.codebuddy/skills-marketplace/skills/dgiot_uav/uav_protocol_analysis/SKILL.md | head -20
```

## 测试用例

### 测试1: 使用 UAV 测试管理技能

**测试命令**:
```bash
# 在 CodeBuddy 对话中输入
use_skill uav_test_management
```

**预期响应**:
```
激活 UAV 测试管理技能...
我是无人机测试管理专家，专注于超近距无人机自动化测试产线的全面管理。

我可以帮助你：
1. 配置无人机测试项
2. 管理测试工位和设备绑定
3. 执行自动化测试流程
4. 分析测试结果和故障诊断

请告诉我你需要什么帮助？
```

**测试对话示例**:
```
用户: "如何配置一个新的测试项？"
CodeBuddy(使用技能): "我来帮你配置测试项。首先需要确定测试项名称、所属工位和测试步骤。测试项应该包含..."
```

### 测试2: 使用 UAV 协议分析技能

**测试命令**:
```bash
# 在 CodeBuddy 对话中输入
use_skill uav_protocol_analysis
```

**预期响应**:
```
激活 UAV 协议分析技能...
我是无人机协议分析专家，专注于 EB90 协议栈的解析、编码、调试和优化。

我可以帮助你：
1. 解析 EB90 协议报文
2. 分析遥测数据(D1/D2/D3)
3. 验证 CRC16 校验
4. 生成测试报文
5. 调试协议通信问题

请告诉我你需要什么帮助？
```

**测试对话示例**:
```
用户: "如何解析一个EB90报文？"
CodeBuddy(使用技能): "我来帮你解析EB90报文。首先需要验证同步头(0xEB90)，然后解析目的地址、源地址..."
```

### 测试3: 关键词自动检测

**测试关键词**:
- "无人机测试" - 应该推荐 `uav_test_management` 技能
- "协议解析" - 应该推荐 `uav_protocol_analysis` 技能
- "工位绑定" - 应该推荐 `uav_test_management` 技能
- "EB90调试" - 应该推荐 `uav_protocol_analysis` 技能

**测试方法**:
在 CodeBuddy 对话中输入上述关键词，观察是否自动推荐相关技能。

### 测试4: 项目约定遵守测试

**测试场景1: 编译命令**
```
用户: "编译无人机插件"
预期响应: CodeBuddy 应该推荐热编译命令而非全量编译
推荐命令: _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'
```

**测试场景2: 调试命令**
```
用户: "检查PLC状态"
预期响应: CodeBuddy 应该提供标准调试命令
推荐命令: _build/emqx/rel/emqx/bin/emqx eval 'io:format("Checking plc 1100: ~p~n", [global:whereis_name({plc, 1100})]).'
```

**测试场景3: 编码规范**
```
用户: "如何在Erlang中打印中文？"
预期响应: CodeBuddy 应该推荐正确的Unicode格式
推荐格式: <<"中文内容"/utf8>>
```

## 验证技能功能

### 1. 验证测试管理技能功能

**测试项配置**:
```erlang
% 技能应该能够提供测试项配置示例
% 预期输出包含标准的测试项结构
```

**工位绑定管理**:
```erlang
% 技能应该能够提供设备绑定示例
% 预期输出包含 bind_device_to_station 函数
```

**自动化测试执行**:
```erlang
% 技能应该能够提供测试执行示例
% 预期输出包含 start_test_process 函数
```

### 2. 验证协议分析技能功能

**协议解析**:
```erlang
% 技能应该能够提供EB90协议解析示例
% 预期输出包含 parse_eb90_frame 函数
```

**CRC校验**:
```erlang
% 技能应该能够提供CRC16计算示例
% 预期输出包含 crc16 函数（小端格式）
```

**命令解析**:
```erlang
% 技能应该能够提供D1协议解析示例
% 预期输出包含 parse_d1_protocol 函数
```

## 测试脚本

### 快速测试脚本
创建测试脚本 `test_codebuddy_skills.sh`:

```bash
#!/bin/bash

echo "=== 测试 CodeBuddy UAV 技能 ==="

# 测试1: 检查技能文件
echo "1. 检查技能文件..."
if [ -f "/root/.codebuddy/skills-marketplace/skills/dgiot_uav/uav_test_management/SKILL.md" ]; then
    echo "  ✓ UAV测试管理技能文件存在"
else
    echo "  ✗ UAV测试管理技能文件缺失"
fi

if [ -f "/root/.codebuddy/skills-marketplace/skills/dgiot_uav/uav_protocol_analysis/SKILL.md" ]; then
    echo "  ✓ UAV协议分析技能文件存在"
else
    echo "  ✗ UAV协议分析技能文件缺失"
fi

# 测试2: 检查配置文件
echo "2. 检查配置文件..."
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

# 测试3: 检查索引文件
echo "3. 检查索引文件..."
if [ -f "/root/.codebuddy/skills-marketplace/skills/dgiot_uav/INDEX.md" ]; then
    echo "  ✓ 技能索引文件存在"
    echo "  索引内容预览:"
    head -20 "/root/.codebuddy/skills-marketplace/skills/dgiot_uav/INDEX.md"
else
    echo "  ✗ 技能索引文件缺失"
fi

echo ""
echo "=== 测试完成 ==="
echo "请在 CodeBuddy 中手动测试技能使用:"
echo "1. use_skill uav_test_management"
echo "2. use_skill uav_protocol_analysis"
```

### 运行测试
```bash
chmod +x test_codebuddy_skills.sh
./test_codebuddy_skills.sh
```

## 故障排除

### 问题1: 技能未找到
**症状**: `use_skill` 命令返回 "技能未找到"
**解决方案**:
```bash
# 重新运行激活脚本
cd /home/gitee/dgiot
./activate_codebuddy_skills.sh

# 重启 CodeBuddy
pkill -f codebuddy
# 等待几秒后重新启动 CodeBuddy
```

### 问题2: 技能内容不正确
**症状**: 技能响应不符合预期
**解决方案**:
```bash
# 检查技能文件内容
cat /root/.codebuddy/skills-marketplace/skills/dgiot_uav/uav_test_management/SKILL.md

# 如果需要，重新复制技能文件
cp -r /home/gitee/dgiot/.codebuddy/skills/uav_test_management /root/.codebuddy/skills-marketplace/skills/dgiot_uav/
```

### 问题3: 关键词检测不工作
**症状**: 输入关键词后没有技能推荐
**解决方案**:
```bash
# 检查工作流程配置
cat /root/.codebuddy/workflow.json | grep -A5 -B5 "keywords"

# 重启 CodeBuddy 重新加载配置
pkill -f codebuddy
```

## 成功标准

### 基本成功标准
- [ ] `use_skill uav_test_management` 命令成功执行
- [ ] `use_skill uav_protocol_analysis` 命令成功执行
- [ ] 技能响应包含正确的功能介绍
- [ ] 关键词检测能够推荐相关技能

### 高级成功标准
- [ ] 技能能够提供正确的代码示例
- [ ] 技能能够指导项目约定遵守
- [ ] 技能能够处理实际业务场景
- [ ] 智能体推荐能够基于任务类型

## 测试报告

### 测试环境
- 项目路径: `/home/gitee/dgiot`
- CodeBuddy路径: `/root/.codebuddy`
- 测试时间: 2026-03-26

### 测试结果
完成测试后，请填写以下结果：

| 测试项目 | 结果 | 备注 |
|---------|------|------|
| 技能文件检查 | □ 通过 □ 失败 | |
| 配置文件检查 | □ 通过 □ 失败 | |
| 索引文件检查 | □ 通过 □ 失败 | |
| 测试管理技能 | □ 通过 □ 失败 | |
| 协议分析技能 | □ 通过 □ 失败 | |
| 关键词检测 | □ 通过 □ 失败 | |
| 项目约定指导 | □ 通过 □ 失败 | |

### 发现的问题
1. 
2. 
3. 

### 建议改进
1. 
2. 
3. 

## 后续步骤

### 立即执行
1. 重启 CodeBuddy 加载新技能
2. 测试基本技能功能
3. 验证关键词检测

### 短期计划
1. 收集使用反馈
2. 优化技能内容
3. 添加更多测试用例

### 长期计划
1. 扩展更多智能体
2. 集成更多工作流程
3. 优化性能监控

## 联系支持

如果测试中遇到问题，请参考：
- 使用指南: `cat /home/gitee/dgiot/CODEBUDDY_UAV_GUIDE.md`
- 项目文档: `apps/dgiot_uav/docs/`
- 配置文件: `/root/.codebuddy/`

---
**测试文档版本**: 1.0.0
**最后更新**: 2026-03-26
**测试负责人**: UAV项目开发团队