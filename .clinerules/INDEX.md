# dgaiot团队规则索引

## 核心规则（简洁高效）

### 1. 开发规则
- **development_rules.md** - 开发流程和命令
  - 热编译/热加载命令
  - 测试和调试流程
  - 工程最佳实践

### 2. 编码规范
- **coding_standards.md** - 代码质量规范
  - Erlang/OTP最佳实践
  - 三层架构要求
  - 日志和错误处理

### 3. API规则
- **api_rules.md** - API设计和管理
  - 三层架构实现
  - API生命周期管理
  - 检查清单

### 4. 安全规则
- **security_rules.md** - 安全管理
  - 敏感信息处理
  - 配置模板
  - Git管理

### 5. 质量验证
- **rule_validation.md** - 规则验证方案
  - 简洁性、高效性、有效性标准
  - 自动验证脚本
- **ai_rule_compliance_report.md** - AI规则遵守统计
  - AI自动化编程符合率分析
  - 详细统计数据和改进建议
  - 每次git push自动验证规则质量
  - 实时监控和持续改进
- **statistical_methodology.md** - 统计指标科学合理性说明
  - 统计指标设计原理和计算方法
  - 科学性和合理性验证
  - 改进和优化建议
- **makefile_commands_guide.md** - Makefile统计和自动上库命令指南
  - 新增6个Makefile命令使用说明
  - GitHub/Gitee自动推送配置
  - 日常开发流程优化

## 新增调试规范

### 6. 协议调试规范
- **debug_protocol_issues.md** - Modbus RTU协议调试规范
  - 调试流程和关键日志查看点
  - 常见问题检查清单
  - 故障排除和最佳实践

### 7. 传感器数据工作流
- **sensor_data_workflow.md** - 传感器数据上报到前端展示完整工作流
  - 完整数据流架构和各层职责
  - 逐层调试检查清单和快速诊断脚本
  - 常见问题解决方案和最佳实践
  - 工具命令参考和团队协作规范

## 快速开始

```bash
# 热编译（日常开发）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot).'

# 全量编译调试
make run
```

## 使用方式

### 简洁高效原则
1. **规则精炼**：每个文件不超过150行
2. **直接实用**：只包含必须遵守的核心规范
3. **团队统一**：确保所有人使用相同规则
4. **快速参考**：包含检查清单和示例

### 日常开发
- 开发命令：参考 development_rules.md
- 编码规范：参考 coding_standards.md
- API设计：参考 api_rules.md
- 安全管理：参考 security_rules.md

## 规则特点
- ✅ 简洁：每个文件精炼实用
- ✅ 高效：快速查找和参考
- ✅ 统一：团队一致性
- ✅ 完整：覆盖核心开发流程

## 更新记录
- 2025-12-19：为dgaiot团队优化，追求简洁高效
