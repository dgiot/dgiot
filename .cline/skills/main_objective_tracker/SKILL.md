---
name: main_objective_tracker
description: 主目标跟踪与偏离检测技能，帮助AI助手保持聚焦在核心任务上，避免偏离主目标
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-19
category: productivity
tags: [focus, objective, tracking, deviation, todo, priority]
trigger_phrases:
  - "判断当前主目标"
  - "是否偏离主目标"
  - "聚焦主目标"
  - "主目标是什么"
  - "当前任务优先级"
  - "检查目标偏离"
  - "目标跟踪"
  - "偏离检测"
---

# Main Objective Tracker

主目标跟踪与偏离检测技能，帮助AI助手保持聚焦在核心任务上，避免偏离主目标。

## 快速开始

```bash
# 运行主目标跟踪器
python3 scripts/use_main_objective_tracker.py

# 运行Hook集成示例
python3 scripts/hook_integration_example.py
```

## 核心能力

### 1. 主目标识别与跟踪
- 动态识别当前主目标
- 跟踪目标状态和进度
- 提供目标优先级排序

### 2. 任务相关性分析
- 分析任务与主目标的相关性
- 计算相关性分数（0-100）
- 提供执行建议

### 3. 偏离检测与纠正
- 检测任务执行是否偏离主目标
- 提供纠正建议
- 自动回到核心任务

### 4. Todo List管理
- 智能分类任务（核心、优化、文档、低优先级）
- 管理非核心但必要的任务
- 提供任务状态跟踪

### 5. Hook集成系统
- 任务前检查：判断任务相关性
- 任务中检查：检测偏离（10分钟阈值）
- 任务后检查：确保回到主目标

## 使用场景

### AI助手工作流
- 每次收到任务时，首先分析任务相关性
- 执行任务前进行任务前检查
- 执行任务中定期检查是否偏离
- 执行任务后确保回到主目标

### 项目管理
- 跟踪项目核心目标
- 管理任务优先级
- 避免范围蔓延

### 团队协作
- 确保团队成员聚焦核心任务
- 管理并行任务
- 提供透明的任务状态

## 目录结构

```
main_objective_tracker/
├── SKILL.md                    # 本文件
├── README.md                   # 详细说明文档
├── scripts/                   # 可执行脚本
│   ├── use_main_objective_tracker.py  # 主目标跟踪器
│   ├── hook_integration_example.py    # Hook集成示例
│   └── task_analyzer.py               # 任务分析器
├── config/                   # 配置文件
│   ├── main_objective_config.json    # 主目标配置
│   ├── rules_config.json             # 规则配置
│   └── hooks_config.json             # Hook配置
└── examples/                # 使用示例
    ├── basic_usage.py               # 基础使用
    ├── deviation_detection.py       # 偏离检测
    └── todo_list_management.py      # Todo list管理
```

## 与Cline AI助手集成

### 技能激活
当用户提到以下关键词时，激活本技能：
- "判断当前主目标"
- "是否偏离主目标"
- "聚焦主目标"
- "主目标是什么"
- "当前任务优先级"
- "检查目标偏离"

### 工作流集成
```
用户请求 → 技能激活 → 分析任务相关性 → 
高相关性 → 立即执行 → 任务后检查 → 回到主目标
低相关性 → 添加到todo list → 询问用户 → 按需执行
```

### Hook集成点
1. **Pre-task Hook**: 任务执行前检查相关性
2. **Mid-task Hook**: 任务执行中检查偏离
3. **Post-task Hook**: 任务执行后确保回到主目标

## 详细功能说明

### 主目标规则系统

#### RULE_001: 核心业务功能优先 (HIGH)
- 优先处理直接影响无人机协议解析、业务层摘要生成、系统核心功能的任务
- 示例: 修改uav_protocol.py以支持业务层解析

#### RULE_002: 架构清理与优化 (MEDIUM)
- 清理重复代码、优化目录结构、统一接口设计
- 示例: 清理src/backend目录

#### RULE_003: 文档与技能完善 (MEDIUM)
- 创建必要的文档、技能、配置说明
- 示例: 创建技能文件

#### RULE_004: 临时脚本与调试 (LOW)
- 临时性的调试脚本、测试代码、一次性工具
- 示例: 创建临时测试脚本

### 偏离检测指标

1. **花费过多时间在临时脚本上**
2. **创建非必要的重复功能**
3. **偏离用户明确指定的方向**
4. **过度优化非核心部分**
5. **忽略用户反馈的核心问题**

### 纠正动作

1. **立即停止当前偏离任务**
2. **重新确认用户的主目标**
3. **将偏离任务添加到todo list**
4. **回到最近的核心任务节点**
5. **向用户确认优先级**

## 当前主目标配置

```json
{
  "id": "MO_001",
  "title": "完善无人机协议业务层解析系统",
  "description": "修改uav_protocol.py使其能解析完报文后还能解析业务层协议，提供用户需要的业务层协议的报文摘要",
  "status": "IN_PROGRESS",
  "priority": "HIGH",
  "related_files": [
    "src/protocols/uav_protocol.py",
    "src/backend/services/uav_multicast_service.py",
    "src/backend/app.py"
  ],
  "completion_criteria": [
    "uav_protocol.py能正确解析业务层协议",
    "提供完整的业务层报文摘要",
    "集成到多播服务中",
    "通过API提供业务层摘要"
  ]
}
```

## 使用示例

### 示例1: 高优先级任务
```python
# 任务: "修改src/protocols/uav_protocol.py以支持业务层解析"
# 相关性: 是 (包含关键词: uav_protocol, 业务层)
# 建议: ✅ 任务与主目标相关，可以执行
# 结果: 成功执行
```

### 示例2: 低优先级任务
```python
# 任务: "优化前端UI界面设计"
# 相关性: 否 (不包含核心关键词)
# 建议: ⚠️ 任务与主目标相关性较低
# 结果: ❌ 任务被阻止执行
```

### 示例3: 用户要求的任务
```python
# 任务: "用户要求清理src/backend目录"
# 相关性: 否
# 建议: ⚠️ 任务相关性低，但可能是用户要求的必要任务
# 结果: ✅ 允许执行（因为是用户要求）
```

### 示例4: 偏离检测
```python
# 任务: "长时间低优先级任务"
# 执行时间: 12分钟 (>10分钟阈值)
# 检测: 🚨 检测到偏离: 任务执行时间超过10分钟
# 建议: 立即停止当前任务，回到主目标!
```

## 配置说明

### 主目标配置 (`config/main_objective_config.json`)
```json
{
  "current_main_objective": {
    "title": "完善无人机协议业务层解析系统",
    "description": "修改uav_protocol.py使其能解析完报文后还能解析业务层协议...",
    "priority": "HIGH"
  },
  "deviation_threshold": {
    "time_minutes": 10,
    "steps_count": 5
  }
}
```

### 规则配置 (`config/rules_config.json`)
```json
{
  "rules": [
    {
      "id": "RULE_001",
      "name": "核心业务功能优先",
      "priority": "HIGH",
      "keywords": ["uav_protocol", "业务层", "协议解析", "报文摘要"]
    }
  ]
}
```

## 维护信息

- **版本**: 1.0.0
- **最后更新**: 2026-01-19
- **作者**: Cline AI Assistant
- **许可证**: MIT
- **依赖**: Python 3.8+, json, datetime, typing

## 故障排除

### 常见问题

1. **技能未激活**: 确保使用正确的触发关键词
2. **相关性分析不准确**: 更新规则配置中的关键词
3. **偏离检测太敏感**: 调整deviation_threshold配置
4. **Todo list管理问题**: 检查分类配置

### 调试方法

```bash
# 启用详细日志
python3 scripts/use_main_objective_tracker.py --verbose

# 测试特定任务
python3 scripts/use_main_objective_tracker.py --task "修改uav_protocol.py"

# 生成分析报告
python3 scripts/use_main_objective_tracker.py --report
```

## 扩展开发

### 添加新规则
1. 在config/rules_config.json中添加新规则
2. 定义规则的关键词和优先级
3. 更新技能文档

### 自定义偏离检测
1. 实现新的偏离检测算法
2. 配置检测阈值
3. 添加测试用例

### 集成新工作流
1. 实现新的Hook点
2. 配置工作流集成
3. 添加使用示例

---

*本技能文档最后更新于2026年1月19日*