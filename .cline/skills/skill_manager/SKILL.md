---
name: skill_manager
description: 技能管理专家，提供技能创建、验证、更新和管理的完整工具集，确保技能格式正确且符合Cline标准
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-19
category: development
tags: [skills, management, creation, validation, templates, automation]
trigger_phrases:
  - "创建新技能"
  - "管理技能"
  - "验证技能格式"
  - "更新技能"
  - "技能模板"
  - "技能检查"
  - "修复技能格式"
  - "技能开发"
---

# Skill Manager

技能管理专家，提供完整的技能生命周期管理工具，确保所有技能符合Cline标准格式，避免重复犯错。

## 快速开始

```bash
# 创建新技能
python3 scripts/create_skill.py --name "新技能名称" --description "技能描述"

# 验证技能格式
python3 scripts/validate_skill.py --skill skill_name

# 修复技能格式
python3 scripts/fix_skill_format.py --skill skill_name

# 列出所有技能
python3 scripts/list_skills.py
```

## 核心能力

### 1. 技能创建向导
- 交互式技能创建流程
- 自动生成标准YAML frontmatter
- 提供技能模板和示例
- 验证必填字段

### 2. 技能格式验证
- 检查YAML frontmatter格式
- 验证必需字段 (name, description)
- 检查触发短语格式
- 检测常见格式错误

### 3. 技能修复工具
- 自动修复格式问题
- 添加缺失的元数据字段
- 标准化技能结构
- 移除多余内容

### 4. 技能管理
- 技能列表和状态查看
- 技能启用/禁用管理
- 技能版本控制
- 技能依赖检查

### 5. 模板系统
- 多种技能类型模板
- 可自定义模板
- 模板导出和导入
- 批量技能创建

## 使用场景

### 新技能开发
- 快速创建符合标准的技能
- 避免格式错误
- 提供最佳实践示例

### 技能维护
- 定期验证技能格式
- 批量更新技能元数据
- 修复历史技能格式问题

### 团队协作
- 统一技能开发标准
- 技能模板共享
- 技能质量检查

### 教育培训
- 学习技能开发最佳实践
- 理解Cline技能格式要求
- 掌握技能调试方法

## 目录结构

```
skill_manager/
├── SKILL.md                    # 本文件
├── README.md                   # 详细说明文档
├── scripts/                   # 可执行脚本
│   ├── create_skill.py            # 创建新技能
│   ├── validate_skill.py          # 验证技能格式
│   ├── fix_skill_format.py        # 修复技能格式
│   ├── list_skills.py             # 列出所有技能
│   ├── update_skill.py            # 更新技能
│   └── skill_template_generator.py # 技能模板生成器
├── templates/                 # 技能模板
│   ├── basic_skill_template.md    # 基础技能模板
│   ├── protocol_skill_template.md # 协议技能模板
│   ├── integration_skill_template.md # 集成技能模板
│   ├── productivity_skill_template.md # 生产力技能模板
│   └── custom_template.yaml       # 自定义模板配置
├── config/                   # 配置文件
│   ├── skill_standards.yaml      # 技能标准配置
│   ├── validation_rules.yaml     # 验证规则配置
│   └── template_config.yaml      # 模板配置
├── examples/                 # 示例文件
│   ├── example_skill.md          # 完整技能示例
│   ├── minimal_skill.md          # 最小技能示例
│   └── advanced_skill.md         # 高级技能示例
└── docs/                     # 文档
    ├── skill_creation_guide.md   # 技能创建指南
    ├── format_specification.md   # 格式规范
    ├── best_practices.md         # 最佳实践
    └── troubleshooting.md        # 故障排除
```

## 与Cline集成

### 技能创建工作流

```
用户请求创建技能 → skill_manager激活 → 
1. 收集技能信息 (名称、描述、类别等)
2. 选择技能模板
3. 生成标准YAML frontmatter
4. 创建技能目录结构
5. 验证技能格式
6. 输出创建报告
```

### 技能验证工作流

```
定期技能检查 → skill_manager激活 →
1. 扫描所有技能目录
2. 验证YAML frontmatter格式
3. 检查必需字段
4. 检测格式问题
5. 生成验证报告
6. 提供修复建议
```

### 技能修复工作流

```
发现格式问题 → skill_manager激活 →
1. 分析技能文件
2. 识别格式错误
3. 应用修复规则
4. 备份原始文件
5. 生成修复版本
6. 验证修复结果
```

## 详细功能说明

### 技能创建向导

```python
def create_skill_interactive():
    """交互式技能创建"""
    
    # 1. 收集技能信息
    skill_info = collect_skill_info()
    
    # 2. 选择模板
    template = select_template(skill_info["category"])
    
    # 3. 生成技能文件
    skill_content = generate_skill_content(skill_info, template)
    
    # 4. 创建目录结构
    create_skill_structure(skill_info["name"])
    
    # 5. 保存技能文件
    save_skill_file(skill_info["name"], skill_content)
    
    # 6. 验证格式
    validation_result = validate_skill(skill_info["name"])
    
    return {
        "status": "success",
        "skill_name": skill_info["name"],
        "validation": validation_result,
        "files_created": get_created_files()
    }
```

### 技能格式验证规则

```yaml
# validation_rules.yaml
validation_rules:
  required_fields:
    - name
    - description
  
  optional_fields:
    - version
    - author
    - created_date
    - category
    - tags
    - trigger_phrases
  
  format_rules:
    yaml_frontmatter:
      required: true
      pattern: "^---\\s*\\n.*?\\n---\\s*\\n"
    
    name:
      pattern: "^[a-z0-9_]+$"
      max_length: 50
    
    description:
      min_length: 10
      max_length: 200
    
    trigger_phrases:
      min_items: 1
      max_items: 20
```

### 技能修复算法

```python
def fix_skill_format(skill_name):
    """修复技能格式问题"""
    
    # 1. 读取技能文件
    skill_content = read_skill_file(skill_name)
    
    # 2. 检测问题
    issues = detect_format_issues(skill_content)
    
    # 3. 应用修复
    fixed_content = apply_fixes(skill_content, issues)
    
    # 4. 备份原始文件
    backup_original_file(skill_name)
    
    # 5. 保存修复后的文件
    save_fixed_file(skill_name, fixed_content)
    
    # 6. 验证修复结果
    validation_result = validate_skill(skill_name)
    
    return {
        "issues_found": len(issues),
        "issues_fixed": count_fixed_issues(issues),
        "validation_passed": validation_result["passed"],
        "backup_created": True
    }
```

## 使用示例

### 示例1: 创建新技能
```bash
# 交互式创建
$ python3 scripts/create_skill.py
🎯 技能创建向导
请输入技能名称: data_analyzer
请输入技能描述: 数据分析专家，支持多种数据格式的解析和可视化
请选择技能类别 (protocol/integration/productivity/development): development
请添加触发短语 (用逗号分隔): 数据分析, 数据解析, 数据可视化, 生成图表
✅ 技能创建成功: data_analyzer
📁 创建的文件: .cline/skills/data_analyzer/SKILL.md
📋 验证结果: 通过
```

### 示例2: 验证技能格式
```bash
# 验证特定技能
$ python3 scripts/validate_skill.py --skill main_objective_tracker
🔍 验证技能: main_objective_tracker
✅ YAML frontmatter: 通过
✅ 必需字段: 通过 (name, description)
✅ 可选字段: 通过 (version, author, created_date, category, tags, trigger_phrases)
✅ 触发短语: 8个短语
📊 总体评分: 95/100
💡 建议: 无
```

### 示例3: 修复技能格式
```bash
# 修复格式问题
$ python3 scripts/fix_skill_format.py --skill old_skill
🔧 修复技能: old_skill
⚠️ 发现问题: 缺少YAML frontmatter
⚠️ 发现问题: 缺少created_date字段
⚠️ 发现问题: 触发短语格式不正确
✅ 应用修复: 添加YAML frontmatter
✅ 应用修复: 添加created_date字段
✅ 应用修复: 标准化触发短语格式
📁 备份创建: old_skill.SKILL.md.backup
🎯 修复完成: 3个问题已修复
```

### 示例4: 列出所有技能
```bash
# 列出技能状态
$ python3 scripts/list_skills.py
📋 技能列表 (共6个技能):
1. uav_protocol_analyzer ✅ 格式正确 (v1.0.0)
2. main_objective_tracker ✅ 格式正确 (v1.0.0)
3. hook_manager ✅ 格式正确 (v1.0.0)
4. skill_manager ✅ 格式正确 (v1.0.0)
5. drone ⚠️ 需要更新 (缺少version字段)
6. 111 ⚠️ 需要更新 (触发短语过少)
📊 统计: 4个正常, 2个需要维护
```

## 配置说明

### 技能标准配置 (`config/skill_standards.yaml`)
```yaml
skill_standards:
  file_structure:
    required_files:
      - SKILL.md
    optional_files:
      - README.md
      - scripts/
      - config/
      - examples/
  
  yaml_frontmatter:
    required: true
    delimiter: "---"
    fields_order:
      - name
      - description
      - version
      - author
      - created_date
      - category
      - tags
      - trigger_phrases
  
  categories:
    - protocol_analysis
    - integration
    - productivity
    - development
    - automation
    - monitoring
  
  versioning:
    format: "major.minor.patch"
    auto_increment: true
```

### 模板配置 (`config/template_config.yaml`)
```yaml
templates:
  basic_skill:
    name: "basic_skill_template"
    description: "基础技能模板"
    category: "development"
    tags: ["basic", "template"]
    include_sections:
      - "快速开始"
      - "核心能力"
      - "使用场景"
      - "目录结构"
  
  protocol_skill:
    name: "protocol_skill_template"
    description: "协议分析技能模板"
    category: "protocol_analysis"
    tags: ["protocol", "analysis", "parsing"]
    include_sections:
      - "协议支持"
      - "解析算法"
      - "数据格式"
      - "错误处理"
  
  integration_skill:
    name: "integration_skill_template"
    description: "系统集成技能模板"
    category: "integration"
    tags: ["integration", "api", "workflow"]
    include_sections:
      - "集成点"
      - "API接口"
      - "工作流"
      - "错误恢复"
```

## 维护信息

- **版本**: 1.0.0
- **最后更新**: 2026-01-19
- **作者**: Cline AI Assistant
- **许可证**: MIT
- **依赖**: Python 3.8+, PyYAML, argparse, datetime, os, re

## 故障排除

### 常见问题

1. **技能创建失败**: 检查技能名称是否符合命名规范
2. **格式验证错误**: 确保YAML frontmatter格式正确
3. **模板加载失败**: 检查模板文件是否存在且格式正确
4. **权限问题**: 确保有写入技能目录的权限

### 调试方法

```bash
# 启用详细日志
python3 scripts/create_skill.py --verbose --debug

# 测试特定功能
python3 scripts/validate_skill.py --skill test_skill --output detailed

# 生成技能报告
python3 scripts/list_skills.py --report skills_report.html
```

## 扩展开发

### 添加新模板
1. 在templates/目录下创建新模板文件
2. 更新template_config.yaml配置
3. 添加模板示例
4. 更新文档

### 自定义验证规则
1. 修改validation_rules.yaml
2. 实现新的验证函数
3. 添加测试用例
4. 更新验证报告格式

### 集成CI/CD
1. 添加自动化技能检查
2. 集成到构建流程
3. 添加质量门禁
4. 生成技能质量报告

---

*本技能文档最后更新于2026年1月19日*