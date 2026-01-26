deyi---
name: dgiot_autonomous_development
description: DGIOT自主开发技能，实现Erlang编码、编译和调测的完整闭环，无需人工介入即可独立完成需求开发调测
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: development
tags: [dgiot, autonomous, development, erlang, compile, debug, workflow, automation, closed_loop]
trigger_phrases:
  - 自主开发
  - 闭环开发
  - 独立完成需求
  - 无人介入开发
  - 自动化编码
  - 自动化调测
  - DGIOT全流程
  - Erlang全流程
  - 编码编译调测一体化
---

# DGIOT自主开发技能

## 概述

本技能实现DGIOT项目的Erlang编码、编译和调测完整闭环，通过智能集成现有技能体系，使Cline能够独立完成需求开发调测，无需人工介入。

## 核心目标

**实现目标**: Cline能够独立完成DGIOT项目的：
1. **需求分析** → 2. **架构设计** → 3. **编码实现** → 4. **编译验证** → 5. **在线调试** → 6. **测试验证** → 7. **部署发布**

## 技能体系集成

### 1. 完整技能闭环

```
需求输入 → 主目标跟踪 → 架构设计 → 编码实现 → 编译调试 → 在线测试 → 质量验证 → 部署输出
    ↓          ↓          ↓          ↓          ↓          ↓          ↓          ↓
用户需求 → main_objective_tracker → dgiot_architecture_learning → erlang_include_system → dgiot_compile_debug → dgiot_online_debug → erlang_compile_warnings_fix → 部署脚本
```

### 2. 技能职责划分

| 阶段 | 主要技能 | 辅助技能 | 功能描述 |
|------|----------|----------|----------|
| **需求分析** | main_objective_tracker | hook_manager | 理解需求，跟踪目标，防止偏离 |
| **架构设计** | dgiot_architecture_learning | dgiot_core_concepts | 设计系统架构，理解核心概念 |
| **编码实现** | dgiot_erlang_best_practices | erlang_include_system | 确保Erlang编程思想和风格，处理头文件包含 |
| **代码重用** | dgiot_code_reuse_solution | - | 查找和重用现有代码，避免重复造轮子 |
| **编译调试** | dgiot_compile_debug | erlang_compile_warnings_fix | 热编译，修复编译警告，调试代码 |
| **在线测试** | dgiot_online_debug | development_workflow_cycle | 添加test函数，在线执行测试，遵循开发流程 |
| **质量验证** | erlang_chinese_utf8 | continuous_iteration_cycle | 验证中文打印，持续迭代优化 |
| **架构利用** | dgiot_architecture_learning | - | 充分利用DGIOT七层架构和Hook系统 |
| **数据存储** | dgiot_data_storage | tdengine_timeseries_storage | 设计数据存储，优化时序数据 |
| **部署发布** | skill_manager | - | 管理技能，准备部署 |

## 自主开发工作流

### 1. 全自动工作流

```
用户提出需求
    ↓
[main_objective_tracker] 分析需求，确定主目标
    ↓
[hook_manager] 设置任务监控Hook
    ↓
[dgiot_architecture_learning] 设计系统架构，遵循七层架构
    ↓
[dgiot_erlang_best_practices] 确保Erlang编程思想和风格
    ↓
[erlang_include_system] 创建模块和头文件，正确处理包含
    ↓
[dgiot_code_reuse_solution] 查找和重用现有代码
    ↓
编写Erlang代码（遵循最佳实践）
    ↓
[dgiot_compile_debug] 热编译代码，使用Erlang独有的调测方法
    ↓
[erlang_compile_warnings_fix] 修复编译警告
    ↓
[dgiot_online_debug] 添加test函数，在线调测
    ↓
[development_workflow_cycle] 遵循开发流程
    ↓
[erlang_chinese_utf8] 验证中文打印，解决乱码问题
    ↓
[dgiot_architecture_learning] 充分利用DGIOT系统架构
    ↓
[dgiot_data_storage] 设计数据存储方案
    ↓
[continuous_iteration_cycle] 持续迭代优化
    ↓
[skill_manager] 准备部署，考虑插件业务场景
    ↓
输出符合Erlang最佳实践和DGIOT架构的完整解决方案
```

### 2. 智能决策逻辑

```erlang
%% 自主开发决策逻辑
autonomous_development_decision(Requirement) ->
    %% 分析需求类型
    case analyze_requirement_type(Requirement) of
        {protocol, ProtocolType} ->
            %% 协议开发：使用协议分析技能
            activate_skill(uav_protocol_analyzer),
            activate_skill(dgiot_erlang_best_practices),  %% 确保Erlang最佳实践
            protocol_development_workflow(ProtocolType);
            
        {channel, ChannelType} ->
            %% 通道开发：使用通道架构技能
            activate_skill(dgiot_channel_architecture),
            activate_skill(dgiot_erlang_best_practices),  %% 确保Erlang最佳实践
            channel_development_workflow(ChannelType);
            
        {data_storage, StorageType} ->
            %% 数据存储开发：使用数据存储技能
            activate_skill(dgiot_data_storage),
            activate_skill(dgiot_erlang_best_practices),  %% 确保Erlang最佳实践
            storage_development_workflow(StorageType);
            
        {api, ApiType} ->
            %% API开发：使用API系统技能
            activate_skill(dgiot_data_api_auth_system),
            activate_skill(dgiot_erlang_best_practices),  %% 确保Erlang最佳实践
            api_development_workflow(ApiType);
            
        _ ->
            %% 通用开发：使用标准工作流，始终包含Erlang最佳实践
            activate_skill(dgiot_erlang_best_practices),
            standard_development_workflow(Requirement)
    end.
```

## 技能协同机制

### 1. Hook驱动的技能激活

```bash
#!/bin/bash
# .clinerules/hooks/PreToolUse - 智能技能激活Hook

# 根据工具使用情况激活相应技能
activate_skill_by_tool_usage() {
    local tool_name="$1"
    local tool_args="$2"
    
    # 编码相关操作
    if [[ "$tool_name" == "write_to_file" ]] && [[ "$tool_args" == *".erl"* ]]; then
        echo "[Hook] 检测到Erlang编码操作，激活erlang_include_system技能" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：正在编写Erlang代码，建议使用erlang_include_system技能处理头文件包含。"
    fi
    
    # 编译相关操作
    if [[ "$tool_name" == "execute_command" ]] && [[ "$tool_args" == *"dgiot_plugin:compile"* ]]; then
        echo "[Hook] 检测到编译操作，激活dgiot_compile_debug技能" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：正在编译代码，建议使用dgiot_compile_debug技能进行热编译调试。"
    fi
    
    # 测试相关操作
    if [[ "$tool_name" == "execute_command" ]] && [[ "$tool_args" == *"test()"* ]]; then
        echo "[Hook] 检测到测试操作，激活dgiot_online_debug技能" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：正在执行测试，建议使用dgiot_online_debug技能进行在线调试。"
    fi
    
    # 架构设计相关
    if [[ "$tool_args" == *"架构"* ]] || [[ "$tool_args" == *"设计"* ]]; then
        echo "[Hook] 检测到架构设计操作，激活dgiot_architecture_learning技能" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：正在进行架构设计，建议使用dgiot_architecture_learning技能。"
    fi
}
```

### 2. 任务状态跟踪

```erlang
%% 任务状态跟踪模块
-module(autonomous_development_tracker).

-export([track_task/2, get_next_step/1, evaluate_progress/1]).

%% 跟踪任务状态
track_task(TaskId, CurrentStep) ->
    %% 记录任务状态
    dgiot_data:insert(autonomous_tasks, TaskId, 
                     #{step => CurrentStep, 
                       timestamp => erlang:system_time(),
                       status => in_progress}),
    
    %% 根据当前步骤确定下一步
    NextStep = get_next_step(CurrentStep),
    
    %% 激活相应技能
    activate_skill_for_step(NextStep),
    
    {ok, NextStep}.

%% 获取下一步
get_next_step(CurrentStep) ->
    case CurrentStep of
        requirement_analysis -> architecture_design;
        architecture_design -> coding;
        coding -> compile_debug;
        compile_debug -> online_testing;
        online_testing -> quality_verification;
        quality_verification -> deployment;
        deployment -> completed;
        completed -> completed
    end.

%% 激活步骤对应技能
activate_skill_for_step(Step) ->
    case Step of
        architecture_design -> activate_skill(dgiot_architecture_learning);
        coding -> activate_skill(erlang_include_system);
        compile_debug -> activate_skill(dgiot_compile_debug);
        online_testing -> activate_skill(dgiot_online_debug);
        quality_verification -> activate_skill(erlang_chinese_utf8);
        _ -> ok
    end.
```

## 实际应用场景

### 场景1: 新增UAV协议解析模块

```
用户需求: "需要新增一个UAV协议解析模块"
    ↓
[main_objective_tracker] 确认目标：UAV协议解析
    ↓
[hook_manager] 设置协议开发监控
    ↓
[uav_protocol_analyzer] 分析协议格式
    ↓
[dgiot_architecture_learning] 设计模块架构
    ↓
[erlang_include_system] 创建协议头文件
    ↓
编写协议解析代码
    ↓
[dgiot_compile_debug] 热编译验证
    ↓
[dgiot_online_debug] 添加协议测试函数
    ↓
在线测试协议解析
    ↓
[erlang_chinese_utf8] 验证日志输出
    ↓
输出完整协议模块
```

### 场景2: 优化TDengine数据存储

```
用户需求: "优化TDengine时序数据存储性能"
    ↓
[main_objective_tracker] 确认目标：TDengine性能优化
    ↓
[tdengine_timeseries_storage] 分析当前存储方案
    ↓
[dgiot_data_storage] 设计优化方案
    ↓
[dgiot_code_reuse_solution] 查找现有优化代码
    ↓
编写优化代码
    ↓
[dgiot_compile_debug] 编译测试
    ↓
[dgiot_online_debug] 性能测试
    ↓
[continuous_iteration_cycle] 持续优化
    ↓
输出优化方案和代码
```

### 场景3: 修复中文打印问题

```
用户需求: "修复Erlang代码中的中文打印乱码"
    ↓
[main_objective_tracker] 确认目标：中文打印修复
    ↓
[erlang_chinese_utf8] 分析乱码原因
    ↓
[erlang_include_system] 检查头文件包含
    ↓
修改代码使用安全打印函数
    ↓
[dgiot_compile_debug] 编译验证
    ↓
[dgiot_online_debug] 测试中文打印
    ↓
[erlang_logging_solution] 验证日志系统
    ↓
输出修复方案
```

## 质量保证机制

### 1. 自动化代码审查

```erlang
%% 自动化代码审查
automated_code_review(Code) ->
    %% 检查编码规范
    Checks = [
        check_include_files(Code),
        check_chinese_printing(Code),
        check_logging_format(Code),
        check_error_handling(Code),
        check_performance(Code)
    ],
    
    %% 汇总检查结果
    Passed = [Check || {pass, _} = Check <- Checks],
    Failed = [Check || {fail, _} = Check <- Checks],
    
    %% 生成审查报告
    Report = #{
        total_checks => length(Checks),
        passed => length(Passed),
        failed => length(Failed),
        details => #{passed => Passed, failed => Failed},
        recommendations => generate_recommendations(Failed)
    },
    
    {ok, Report}.
```

### 2. 持续集成检查

```bash
#!/bin/bash
# autonomous_ci_check.sh - 自主开发持续集成检查

echo "=== 自主开发持续集成检查 ==="

# 1. 编译检查
echo "1. 编译检查..."
make compile 2>&1 | grep -E "(error|warning)"

# 2. 单元测试
echo "2. 单元测试..."
make eunit

# 3. 集成测试
echo "3. 集成测试..."
make integration-test

# 4. 代码质量
echo "4. 代码质量检查..."
check_erlang_code_quality() {
    # 检查头文件包含
    grep -r "include(\"" apps/ --include="*.erl" | grep -v "include_lib"
    
    # 检查中文打印
    grep -r "io:format.*[\x80-\xFF]" apps/ --include="*.erl"
    
    # 检查日志格式
    grep -r "?LOG" apps/ --include="*.erl" | head -10
}

# 5. 生成报告
echo "5. 生成检查报告..."
generate_ci_report
```

## 部署和运维

### 1. 自动化部署脚本

```bash
#!/bin/bash
# autonomous_deployment.sh - 自主开发自动化部署

echo "=== 自主开发自动化部署 ==="

# 1. 验证代码质量
echo "1. 验证代码质量..."
./autonomous_ci_check.sh

# 2. 热编译部署
echo "2. 热编译部署..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 3. 热加载插件
echo "3. 热加载插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot).'

# 4. 验证部署
echo "4. 验证部署..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot:test().'

# 5. 监控状态
echo "5. 监控状态..."
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(启动|启动完成|error)"
```

### 2. 运维监控

```erlang
%% 自主开发运维监控
autonomous_operations_monitoring() ->
    %% 监控系统状态
    Monitors = [
        monitor_compile_status(),
        monitor_test_results(),
        monitor_performance(),
        monitor_error_logs(),
        monitor_resource_usage()
    ],
    
    %% 分析监控数据
    Analysis = analyze_monitors(Monitors),
    
    %% 自动调整
    case Analysis of
        #{needs_optimization := true} ->
            auto_optimize();
        #{needs_fix := true} ->
            auto_fix_issues();
        _ ->
            ok
    end,
    
    {ok, Analysis}.
```

## 技能管理

### 1. 技能生命周期管理

```erlang
%% 技能生命周期管理
manage_skill_lifecycle(SkillName, Action) ->
    case Action of
        activate ->
            %% 激活技能
            activate_skill(SkillName),
            ?LOG(info, "技能 ~s 已激活", [SkillName]);
            
        deactivate ->
            %% 停用技能
            deactivate_skill(SkillName),
            ?LOG(info, "技能 ~s 已停用", [SkillName]);
            
        update ->
            %% 更新技能
            update_skill(SkillName),
            ?LOG(info, "技能 ~s 已更新", [SkillName]);
            
        evaluate ->
            %% 评估技能效果
            evaluate_skill(SkillName),
            ?LOG(info, "技能 ~s 已评估", [SkillName])
    end.
```

### 2. 技能效果评估

```bash
#!/bin/bash
# evaluate_skills.sh - 评估技能效果

echo "=== 技能效果评估 ==="

# 评估每个技能的使用效果
for skill in .cline/skills/*/; do
    skill_name=$(basename "$skill")
    
    echo "评估技能: $skill_name"
    
    # 检查使用频率
    usage_count=$(grep -r "use_skill.*$skill_name" . | wc -l)
    
    # 检查效果
    effect_score=$(calculate_skill_effect "$skill_name")
    
    # 生成评估报告
    echo "  使用次数: $usage_count"
    echo "  效果评分: $effect_score/100"
    
    # 建议
    if [ $usage_count -eq 0 ]; then
        echo "  建议: 考虑停用或优化该技能"
    elif [ $effect_score -lt 60 ]; then
        echo "  建议: 需要优化技能内容"
    else
        echo "  建议: 技能效果良好"
    fi
    
    echo ""
done
```

## 成功指标

### 1. 量化指标
- **开发效率**: 任务完成时间减少50%
- **代码质量**: 编译警告减少90%
- **测试覆盖率**: 单元测试覆盖率提升至80%
- **问题解决**: 自主解决问题比例达到70%
- **人工介入**: 人工介入需求减少60%

### 2. 质量指标
- **架构一致性**: 所有模块遵循统一架构标准
- **代码规范性**: 100%符合DGIOT编码规范
- **调试标准化**: 所有调试使用在线调试方式
- **文档完整性**: 自动生成完整开发文档
- **可维护性**: 代码易于理解和维护

## 未来扩展

### 1. 智能优化
- **机器学习**: 基于历史数据优化决策逻辑
- **自适应调整**: 根据项目特点自动调整工作流
- **预测分析**: 预测潜在问题并提前预防

### 2. 生态系统集成
- **CI/CD集成**: 与持续集成系统深度集成
- **监控告警**: 与运维监控系统集成
- **知识库**: 构建自主开发知识库