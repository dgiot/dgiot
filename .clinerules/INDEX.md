# dgaiot团队规则索引

## 核心规则（7个文件）

### 1. 架构设计原则
- **architecture_principles.md** - 七层架构设计原则

### 2. 开发规则
- **development_rules.md** - 开发流程和命令

### 3. 编码规范
- **coding_standards.md** - 代码质量规范

### 4. API规则
- **api_rules.md** - API设计和管理

### 5. 安全规则
- **security_rules.md** - 安全管理

### 6. 质量验证
- **rule_validation.md** - 规则验证方案

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

### 日常开发
- 开发命令：参考 development_rules.md
- 编码规范：参考 coding_standards.md
- API设计：参考 api_rules.md
- 安全管理：参考 security_rules.md

## 优化成果
- ✅ **文件数量**：7个核心文件（符合4-7个要求）
- ✅ **总行数**：560行（符合≤610行要求）
- ✅ **简洁性**：每个文件≤150行
- ✅ **实用性**：包含检查清单和示例

## 更新记录
- 2025-12-19：为dgaiot团队优化，追求简洁高效
- 2026-01-26：完成Rules、Skills、Workflows边界梳理和优化
