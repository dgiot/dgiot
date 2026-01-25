---
name: hook_manager
description: Hook管理技能，用于在Cline执行生命周期的特定点集成主目标跟踪功能
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-21
category: automation
tags: [hook, cline, automation, monitoring, objective-tracking]
trigger_phrases:
  - "Hook管理"
  - "Cline Hook配置"
  - "主目标跟踪"
  - "任务监控"
  - "自动化集成"
  - "执行生命周期"
  - "PreToolUse Hook"
---

# Hook管理技能

## 概述

本技能提供了在Cline执行生命周期中集成Hook的完整解决方案，特别关注主目标跟踪和任务监控功能。通过Hook系统，可以在关键执行点插入自定义逻辑，实现自动化监控、目标跟踪和性能优化。

## Hook类型

### 1. 可用Hook类型

Cline支持以下Hook类型，在生命周期的不同点执行：

| Hook类型 | 执行时机 | 使用场景 |
|---------|---------|---------|
| **TaskStart** | 新任务开始时 | 初始化任务环境，记录开始时间，检查前置条件 |
| **TaskResume** | 任务恢复时 | 恢复任务状态，重新初始化环境 |
| **TaskCancel** | 任务取消时 | 清理资源，记录取消原因 |
| **TaskComplete** | 任务完成时 | 评估任务成果，生成总结报告 |
| **PreToolUse** | 工具使用前 | 检查工具使用合理性，监控目标偏离 |
| **PostToolUse** | 工具使用后 | 记录工具使用结果，更新执行状态 |
| **UserPromptSubmit** | 用户提交提示时 | 预处理用户输入，添加上下文信息 |
| **PreCompact** | 上下文压缩前 | 选择要保留的关键信息 |

### 2. Hook执行流程

```
用户提交任务 → TaskStart Hook → 任务执行循环 → PreToolUse Hook → 工具执行 → PostToolUse Hook → 任务完成 → TaskComplete Hook
```

## 主目标跟踪Hook实现

### 1. PreToolUse Hook - 实时目标偏离检测

#### 功能特点
- **实时监控**：在每次工具使用前检查是否偏离主目标
- **关键词匹配**：基于主目标关键词进行相关性分析
- **分级响应**：根据偏离程度提供不同级别的反馈
- **历史记录**：记录所有工具使用历史用于分析

#### 核心逻辑
```bash
# 主目标跟踪逻辑
check_main_objective_deviation() {
  local tool_name="$1"
  local tool_args="$2"
  
  # 主目标关键词（可动态调整）
  MAIN_OBJECTIVE_KEYWORDS=(
    "dgiot_uav" "无人机" "uav" "protocol" "协议"
    "tdengine" "时序数据" "存储" "save_td"
    "channel" "通道" "tcp" "bridge"
    "skill" "技能" "hook" "钩子"
  )
  
  # 偏离检测关键词
  DEVIATION_KEYWORDS=(
    "unrelated" "offtopic" "distraction"
    "social" "entertainment" "personal"
  )
  
  # 检查相关性
  local is_related=false
  local is_deviation=false
  
  # ... 匹配逻辑 ...
  
  # 返回结果
  if $is_deviation; then
    echo "deviation"
  elif $is_related; then
    echo "related"
  else
    echo "neutral"
  fi
}
```

#### 响应策略
```bash
case $DEVIATION_RESULT in
  "deviation")
    # 检测到偏离，提供警告和建议
    ERROR_MSG="检测到可能偏离主目标的操作。"
    CONTEXT_MOD="注意：请确保工具使用与主目标相关。"
    ;;
  "related")
    # 与主目标相关，提供积极反馈
    CONTEXT_MOD="很好！当前操作与主目标相关。"
    ;;
  *)
    # 中性操作，正常记录
    CONTEXT_MOD="工具使用记录。请确保操作有助于完成主目标。"
    ;;
esac
```

### 2. TaskComplete Hook - 主目标达成度评估

#### 功能特点
- **量化评估**：基于关键词匹配计算达成度百分比
- **分级评价**：优秀(≥80%)、良好(≥60%)、一般(≥40%)、不足(<40%)
- **报告生成**：自动生成详细的任务总结报告
- **改进建议**：根据评估结果提供针对性的改进建议

#### 评估算法
```bash
evaluate_main_objective_achievement() {
  local task_result="$1"
  
  # 计算关键词匹配数量
  local match_count=0
  local total_keywords=${#MAIN_OBJECTIVE_KEYWORDS[@]}
  
  for keyword in "${MAIN_OBJECTIVE_KEYWORDS[@]}"; do
    if [[ "$result_lower" == *"$keyword"* ]]; then
      ((match_count++))
    fi
  done
  
  # 计算达成度百分比
  local achievement_percentage=$((match_count * 100 / total_keywords))
  
  # 分级评估
  if [ $achievement_percentage -ge 80 ]; then
    echo "excellent:$achievement_percentage"
  elif [ $achievement_percentage -ge 60 ]; then
    echo "good:$achievement_percentage"
  elif [ $achievement_percentage -ge 40 ]; then
    echo "fair:$achievement_percentage"
  else
    echo "poor:$achievement_percentage"
  fi
}
```

#### 报告生成
```bash
generate_task_summary() {
  local task_id="$1"
  local achievement_level="$2"
  local achievement_percent="$3"
  
  case $achievement_level in
    "excellent")
      SUMMARY="🎯 主目标达成度：优秀 ($achievement_percent%)"
      DETAILS="任务成功完成，主目标高度达成。"
      RECOMMENDATION="继续保持高效工作。"
      ;;
    # ... 其他等级 ...
  esac
  
  # 生成Markdown格式报告
  cat << EOF
# 任务完成总结报告

## 基本信息
- **任务ID**: $task_id
- **评估等级**: $achievement_level
- **达成度**: $achievement_percent%

## 总结
$SUMMARY

## 详细评估
$DETAILS

## 改进建议
$RECOMMENDATION
EOF
}
```

## Hook配置指南

### 1. 文件位置
```
.clinerules/hooks/
├── TaskStart          # 任务开始Hook
├── PreToolUse         # 工具使用前Hook（主目标跟踪）
├── TaskComplete       # 任务完成Hook（达成度评估）
└── UserPromptSubmit   # 用户提示提交Hook
```

### 2. 文件权限
```bash
# 添加执行权限
chmod +x .clinerules/hooks/*

# 检查权限
ls -la .clinerules/hooks/
```

### 3. Hook输入输出格式

#### 输入格式（JSON）
```json
{
  "taskId": "unique-task-id",
  "preToolUse": {
    "toolName": "execute_command",
    "toolArgs": {
      "command": "ls -la",
      "requires_approval": false
    }
  },
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}
```

#### 输出格式（JSON）
```json
{
  "cancel": false,
  "contextModification": "额外的上下文信息",
  "errorMessage": "错误信息（如果有）"
}
```

## 最佳实践

### 1. Hook设计原则

#### 保持轻量
```bash
# 好：快速检查，立即返回
check_deviation() {
  # 简单关键词匹配
  # 快速返回结果
}

# 不好：复杂计算，长时间运行
analyze_deeply() {
  # 复杂AI分析
  # 网络请求
  # 数据库查询
}
```

#### 错误处理
```bash
# 好：优雅降级
if command -v jq &> /dev/null; then
  # 使用jq解析JSON
else
  # 降级到基本解析
fi

# 记录错误但不中断
echo "[Hook] 警告：功能降级" >&2
```

#### 日志记录
```bash
# 记录到标准错误
echo "[PreToolUse] 工具使用检测 - 工具: $TOOL_NAME" >&2

# 保存到日志文件
LOG_FILE="/tmp/cline_hook_${TASK_ID}.log"
echo "$TIMESTAMP | $EVENT" >> "$LOG_FILE"
```

### 2. 主目标关键词管理

#### 动态关键词
```bash
# 从配置文件加载关键词
load_keywords_from_config() {
  if [ -f ".clinerules/main_objectives.txt" ]; then
    mapfile -t MAIN_OBJECTIVE_KEYWORDS < ".clinerules/main_objectives.txt"
  else
    # 默认关键词
    MAIN_OBJECTIVE_KEYWORDS=("dgiot" "uav" "protocol" "tdengine")
  fi
}
```

#### 上下文感知
```bash
# 根据任务类型调整关键词
adjust_keywords_by_task() {
  local task_description="$1"
  
  if [[ "$task_description" == *"无人机"* ]]; then
    MAIN_OBJECTIVE_KEYWORDS+=("uav" "drone" "飞行控制" "遥测")
  elif [[ "$task_description" == *"数据库"* ]]; then
    MAIN_OBJECTIVE_KEYWORDS+=("tdengine" "时序数据" "存储" "查询")
  fi
}
```

### 3. 性能优化

#### 缓存机制
```bash
# 缓存关键词匹配结果
CACHE_FILE="/tmp/cline_keyword_cache_${TASK_ID}.txt"

check_with_cache() {
  local tool_name="$1"
  local cache_key="${tool_name}_${TASK_ID}"
  
  # 检查缓存
  if [ -f "$CACHE_FILE" ] && grep -q "$cache_key" "$CACHE_FILE"; then
    cached_result=$(grep "$cache_key" "$CACHE_FILE" | cut -d'|' -f2)
    echo "$cached_result"
    return
  fi
  
  # 计算新结果
  local result=$(check_main_objective_deviation "$tool_name")
  
  # 更新缓存
  echo "$cache_key|$result" >> "$CACHE_FILE"
  echo "$result"
}
```

#### 批量处理
```bash
# 批量检查工具序列
batch_check_tools() {
  local tool_sequence=("$@")
  local deviation_count=0
  
  for tool in "${tool_sequence[@]}"; do
    result=$(check_main_objective_deviation "$tool")
    if [ "$result" = "deviation" ]; then
      ((deviation_count++))
    fi
  done
  
  echo "$deviation_count"
}
```

## 集成示例

### 1. 与技能系统集成

```bash
# 在Hook中调用技能
integrate_with_skill() {
  local task_result="$1"
  
  # 检查是否需要调用特定技能
  if [[ "$task_result" == *"中文打印"* ]]; then
    echo "[Hook] 检测到中文打印需求，建议使用chinese_printing_solution技能" >&2
    CONTEXT_MOD="$CONTEXT_MOD 提示：可使用chinese_printing_solution技能解决中文乱码问题。"
  fi
  
  if [[ "$task_result" == *"时序数据"* ]]; then
    echo "[Hook] 检测到时序数据存储需求，建议使用tdengine_timeseries_storage技能" >&2
    CONTEXT_MOD="$CONTEXT_MOD 提示：可使用tdengine_timeseries_storage技能优化数据存储。"
  fi
}
```

### 2. 与项目特定需求集成

```bash
# DGIOT项目特定集成
integrate_dgiot_specific() {
  local tool_name="$1"
  
  # DGIOT特定检查
  DGIOT_SPECIFIC_KEYWORDS=(
    "dgiot" "parse" "channel" "bridge"
    "product" "device" "thing" "model"
    "task" "statistic" "protocol"
  )
  
  # 检查是否为DGIOT相关操作
  local is_dgiot_related=false
  for keyword in "${DGIOT_SPECIFIC_KEYWORDS[@]}"; do
    if [[ "$tool_name" == *"$keyword"* ]]; then
      is_dgiot_related=true
      break
    fi
  done
  
  if $is_dgiot_related; then
    echo "[Hook] DGIOT相关操作检测通过" >&2
    return 0
  else
    echo "[Hook] 警告：非DGIOT标准操作" >&2
    return 1
  fi
}
```

## 故障排除

### 常见问题

#### 问题1: Hook不执行
**症状**: Hook脚本已创建但未执行
**解决方案**:
1. 检查文件权限：`chmod +x .clinerules/hooks/HookName`
2. 检查文件位置：确保在`.clinerules/hooks/`目录下
3. 检查语法错误：`bash -n .clinerules/hooks/HookName`
4. 检查JSON格式：确保输出为有效的JSON

#### 问题2: Hook执行错误
**症状**: Hook执行失败，返回错误信息
**解决方案**:
1. 检查依赖工具：确保`jq`等工具已安装
2. 检查文件路径：使用绝对路径避免相对路径问题
3. 添加调试信息：在关键点添加`echo "Debug: ..." >&2`
4. 检查权限：确保有读写临时文件的权限

#### 问题3: 性能问题
**症状**: Hook执行缓慢，影响任务执行
**解决方案**:
1. 优化匹配算法：使用简单的字符串匹配
2. 添加缓存：缓存频繁检查的结果
3. 减少IO操作：避免频繁的文件读写
4. 异步处理：将非关键操作移到后台

### 调试工具

#### Hook调试脚本
```bash
#!/bin/bash
# debug_hook.sh

# 模拟Hook输入
SIMULATED_INPUT='{
  "taskId": "test-123",
  "preToolUse": {
    "toolName": "execute_command",
    "toolArgs": {
      "command": "find . -name \"*.erl\"",
      "requires_approval": false
    }
  },
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'

# 测试Hook
echo "测试PreToolUse Hook..."
echo "$SIMULATED_INPUT" | .clinerules/hooks/PreToolUse

echo -e "\n测试TaskComplete Hook..."
COMPLETE_INPUT='{
  "taskId": "test-123",
  "taskComplete": {
    "result": "成功创建了dgiot_uav_tcp通道并分析了时序数据存储"
  },
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'
echo "$COMPLETE_INPUT" | .clinerules/hooks/TaskComplete
```

#### 日志分析工具
```bash
#!/bin/bash
# analyze_hook_logs.sh

LOG_FILE="/tmp/cline_tool_usage_*.log"

echo "=== Hook执行日志分析 ==="
echo ""

# 统计工具使用频率
echo "工具使用频率统计："
cat $LOG_FILE 2>/dev/null | cut -d'|' -f2 | sort | uniq -c | sort -rn

echo ""
echo "偏离检测统计："
cat $LOG_FILE 2>/dev/null | cut -d'|' -f3 | sort | uniq -c

echo ""
echo "时间分布："
cat $LOG_FILE 2>/dev/null | cut -d'|' -f1 | cut -d'T' -f1 | sort | uniq -c
```

## 总结

Hook管理系统为Cline提供了强大的扩展能力，特别是在主目标跟踪方面：

### 核心价值
1. **实时监控**：在任务执行过程中实时检测目标偏离
2. **量化评估**：提供主目标达成度的量化评估
3. **自动化报告**：自动生成任务总结和改进建议
4. **技能集成**：与技能系统无缝集成，提供智能建议

### 实施建议
1. **渐进式部署**：先从关键Hook开始，逐步扩展
2. **持续优化**：根据实际使用情况调整关键词和算法
3. **团队协作**：分享最佳实践，建立统一的Hook标准
4. **监控改进**：定期分析Hook日志，优化性能

通过有效的Hook管理，可以显著提升Cline的任务执行效率和质量，确保始终聚焦于主目标，避免偏离和资源浪费。
