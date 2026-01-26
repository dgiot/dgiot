# Rules、Skills和Workflows边界梳理与优化方案

## 概述

本文档分析现有的Rules、Skills和Workflows，明确各自的职责边界，提出优化方案，让三者各司其职，形成完整的开发支持体系。

## 1. 现状分析

### 1.1 现有Rules（17个文件）
**核心规则（7个）**：
1. `architecture_principles.md` - 七层架构设计原则 ✅（适合作为Rule）
2. `development_rules.md` - 开发流程和命令 ✅（适合作为Rule）
3. `coding_standards.md` - 代码质量规范 ✅（适合作为Rule）
4. `api_rules.md` - API设计和管理 ✅（适合作为Rule）
5. `security_rules.md` - 安全管理 ✅（适合作为Rule）
6. `rule_validation.md` - 规则验证方案 ✅（适合作为Rule）
7. `integration_test_workflow_rules.md` - 集成测试工作流程规则 ⚠️（更适合作为Workflow）

**调试规范（4个）**：
8. `debug_protocol_issues.md` - Modbus RTU协议调试规范 ⚠️（更适合作为Skill）
9. `sensor_data_workflow.md` - 传感器数据工作流 ⚠️（更适合作为Workflow或Skill）
10. `log_system_operations.md` - 日志系统运维 ⚠️（更适合作为Skill）
11. `plugin_test_script_rules.md` - 插件测试脚本管理 ⚠️（更适合作为Skill）

**统计和指南（3个）**：
12. `ai_rule_compliance_report.md` - AI规则遵守统计 ✅（适合作为Rule）
13. `statistical_methodology.md` - 统计指标说明 ✅（适合作为Rule）
14. `makefile_commands_guide.md` - Makefile命令指南 ⚠️（更适合作为Workflow）

**其他（3个）**：
15. `INDEX.md` - 规则索引 ✅（必需）
16. `cline_config.json` - Cline配置 ✅（必需）
17. `validate_rules.sh` - 验证脚本 ✅（必需）

### 1.2 现有Workflows（3个文件）
1. `deployment.md` - 部署工作流 ✅（适合作为Workflow）
2. `plugin_development.md` - 插件开发工作流 ✅（适合作为Workflow）
3. `testing_framework.md` - 测试框架工作流 ✅（适合作为Workflow）

### 1.3 现有Skills（28个目录）
已覆盖大部分开发场景，包括：
- 架构设计：`dgiot_architecture_learning`
- 编码规范：`dgiot_erlang_best_practices`
- 编译调试：`dgiot_compile_debug`
- 在线调试：`dgiot_online_debug`
- 协议分析：`uav_protocol_analyzer`
- 数据存储：`dgiot_data_storage`
- 自主开发：`dgiot_autonomous_development`

## 2. 职责边界定义

### 2.1 Rules（规则）
**本质**：系统级的指导原则，必须遵守的规范
**特点**：
- 简洁高效（每个文件≤150行）
- 必须遵守，不可违反
- 团队统一标准
- 快速参考，检查清单

**适合内容**：
- 架构设计原则
- 编码规范标准
- 安全规则
- 质量验证标准
- 团队协作规范

### 2.2 Workflows（工作流）
**本质**：重复性任务的步骤序列
**特点**：
- 可执行的操作步骤
- 包含检查清单
- 支持自动化
- 可重复使用

**适合内容**：
- 插件开发流程
- 部署流程
- 测试流程
- 故障排查流程
- 代码审查流程

### 2.3 Skills（技能）
**本质**：具体的技能实现，AI可执行的指令
**特点**：
- 包含触发短语
- 详细实现指导
- 工具和脚本
- 可被AI调用

**适合内容**：
- 协议调试技能
- 日志运维技能
- 代码重用技能
- 架构设计技能
- 编译调试技能

## 3. 优化方案

### 3.1 Rules优化（精简核心规则）

**保留（7个核心规则）**：
1. `architecture_principles.md` - 七层架构设计原则
2. `development_rules.md` - 开发流程和命令
3. `coding_standards.md` - 代码质量规范
4. `api_rules.md` - API设计和管理
5. `security_rules.md` - 安全管理
6. `rule_validation.md` - 规则验证方案
7. `ai_rule_compliance_report.md` - AI规则遵守统计

**迁移到Skills（4个）**：
1. `debug_protocol_issues.md` → `dgiot_protocol_debug`技能
2. `log_system_operations.md` → `dgiot_log_operations`技能
3. `plugin_test_script_rules.md` → `dgiot_plugin_test_management`技能
4. `sensor_data_workflow.md` → `dgiot_sensor_data_workflow`技能

**迁移到Workflows（2个）**：
1. `integration_test_workflow_rules.md` → `integration_testing.md`工作流
2. `makefile_commands_guide.md` → `makefile_usage.md`工作流

### 3.2 Workflows优化（完善工作流体系）

**现有（3个）**：
1. `deployment.md` - 部署工作流
2. `plugin_development.md` - 插件开发工作流
3. `testing_framework.md` - 测试框架工作流

**新增（4个）**：
1. `integration_testing.md` - 集成测试工作流（从Rules迁移）
2. `makefile_usage.md` - Makefile使用工作流（从Rules迁移）
3. `code_review.md` - 代码审查工作流
4. `troubleshooting.md` - 故障排查工作流

### 3.3 Skills优化（完善技能体系）

**新增技能（4个）**：
1. `dgiot_protocol_debug` - 协议调试技能
2. `dgiot_log_operations` - 日志运维技能
3. `dgiot_plugin_test_management` - 插件测试管理技能
4. `dgiot_sensor_data_workflow` - 传感器数据工作流技能

## 4. 实施计划

### 阶段1：Rules精简（立即执行）
1. 创建4个新的Skills
2. 创建2个新的Workflows
3. 更新INDEX.md，移除迁移的文件
4. 验证Rules符合简洁原则（≤150行）

### 阶段2：Workflows完善（1天内）
1. 完善现有的3个Workflows
2. 新增4个Workflows
3. 创建Workflows索引
4. 集成到开发流程

### 阶段3：Skills集成（1天内）
1. 将新Skills集成到自主开发流程
2. 更新技能触发短语
3. 创建技能检查工具
4. 验证技能效果

### 阶段4：验证和优化（2天内）
1. 运行完整测试
2. 收集团队反馈
3. 持续优化改进
4. 文档更新

## 5. 详细迁移方案

### 5.1 Rules → Skills迁移

#### 1. debug_protocol_issues.md → dgiot_protocol_debug技能
**原内容**：Modbus RTU协议调试规范
**新技能**：
- 触发短语：协议调试、Modbus调试、报文解析问题
- 功能：提供协议调试流程、关键日志查看点、常见问题解决方案
- 工具：调试脚本、日志分析工具

#### 2. log_system_operations.md → dgiot_log_operations技能
**原内容**：日志系统运维命令参考
**新技能**：
- 触发短语：日志查看、日志级别调整、日志运维
- 功能：日志级别管理、日志查看命令、日志文件管理
- 工具：日志分析脚本、级别调整脚本

#### 3. plugin_test_script_rules.md → dgiot_plugin_test_management技能
**原内容**：插件测试脚本管理规则
**新技能**：
- 触发短语：测试脚本管理、插件测试、测试用例
- 功能：测试脚本创建审批、目录结构规范、命名规范
- 工具：测试脚本检查工具、审批流程工具

#### 4. sensor_data_workflow.md → dgiot_sensor_data_workflow技能
**原内容**：传感器数据上报到前端展示完整工作流
**新技能**：
- 触发短语：传感器数据、数据流调试、前端无数据
- 功能：七层数据流分析、逐层检查清单、常见问题解决方案
- 工具：数据流检查脚本、问题诊断工具

### 5.2 Rules → Workflows迁移

#### 1. integration_test_workflow_rules.md → integration_testing.md工作流
**原内容**：集成测试工作流程规则
**新工作流**：
- 名称：集成测试工作流
- 步骤：环境搭建→登录发包→日志检查→数据库验证→代码修改→热编译→重新测试
- 检查清单：完整测试循环检查清单

#### 2. makefile_commands_guide.md → makefile_usage.md工作流
**原内容**：Makefile统计和自动上库命令指南
**新工作流**：
- 名称：Makefile使用工作流
- 步骤：规则统计→自动上库→多平台推送→验证检查
- 检查清单：Makefile命令使用检查清单

### 5.3 新增Workflows

#### 1. code_review.md - 代码审查工作流
- 步骤：代码提交→自动化检查→人工审查→问题修复→合并批准
- 检查清单：代码质量检查清单

#### 2. troubleshooting.md - 故障排查工作流
- 步骤：问题复现→日志分析→代码定位→修复验证→预防措施
- 检查清单：故障排查检查清单

## 6. 预期效果

### 6.1 Rules效果
- **简洁性**：每个Rule文件≤150行，总计≤610行
- **高效性**：关键词查找<100ms，结构清晰
- **有效性**：核心覆盖4个领域，实用命令≥12个

### 6.2 Workflows效果
- **可执行性**：每个Workflow都有可执行的步骤
- **自动化**：支持脚本化执行
- **标准化**：团队使用统一的工作流

### 6.3 Skills效果
- **覆盖率**：覆盖所有开发场景
- **智能化**：AI可自动调用相应技能
- **工具化**：每个技能都有配套工具

## 7. 检查清单

### Rules精简检查清单
- [ ] 核心Rules数量：7个（目标4-6个，可接受7个）
- [ ] 每个文件行数：≤150行
- [ ] 总行数：≤610行
- [ ] 内容简洁：无冗余内容
- [ ] 检查清单：每个Rule都有检查清单

### Workflows完善检查清单
- [ ] Workflows数量：7个（3现有+4新增）
- [ ] 每个Workflow都有完整步骤
- [ ] 每个Workflow都有检查清单
- [ ] 支持自动化执行
- [ ] 集成到开发流程

### Skills集成检查清单
- [ ] Skills数量：32个（28现有+4新增）
- [ ] 每个Skill都有触发短语
- [ ] 每个Skill都有详细指导
- [ ] 每个Skill都有配套工具
- [ ] 集成到自主开发流程

## 8. 更新记录

- **2026-01-26**：创建Rules、Skills和Workflows边界梳理与优化方案
- **下一步**：按照实施计划执行优化

---

**总结**：通过本次优化，将实现：
1. **Rules回归本质**：简洁高效的指导原则
2. **Workflows完善体系**：完整的重复性任务流程
3. **Skills全面覆盖**：智能化的开发技能支持
4. **三者协同工作**：形成完整的开发支持体系