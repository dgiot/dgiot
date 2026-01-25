---
name: dgiot_compile_debug
description: DGIOT编译调试技能，提供热编译、热加载、在线测试和全量编译调试功能，支持插件开发和调试工作流
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-23
category: development
tags: [dgiot, compile, debug, hot_reload, erlang, testing, plugin_development]
trigger_phrases:
  - DGIOT编译调试
  - 热编译热加载
  - Erlang在线测试
  - 插件开发调试
  - 编译错误解决
  - 调试工作流
  - 全量编译
  - 单文件编译
---

# DGIOT编译调试技能

DGIOT编译调试专家，提供完整的编译、调试、测试工作流，支持插件开发和实时调试。

## 快速开始

当用户需要编译DGIOT代码、调试插件或解决编译错误时，激活本技能。

## 核心能力

### 1. 编译系统
- **全量编译**: `make compile` 编译整个项目
- **单文件编译**: `erlc file.erl` 编译单个文件
- **插件编译**: `dgiot_plugin:compile(App)` 动态编译插件
- **增量编译**: 只编译修改过的文件

### 2. 调试工具
- **热加载**: `code:load_file/1` 实时加载修改
- **进程调试**: `observer:start()` 查看进程树
- **消息跟踪**: `dbg:tracer()` 跟踪消息流
- **状态检查**: `sys:get_status/1` 检查进程状态

### 3. 测试框架
- **单元测试**: EUnit测试框架
- **集成测试**: Common Test框架
- **在线测试**: 实时测试插件功能
- **性能测试**: 压力测试和性能分析

### 4. 错误诊断
- **编译错误**: 语法错误、依赖错误诊断
- **运行时错误**: 进程崩溃、消息丢失分析
- **性能问题**: 内存泄漏、CPU占用分析
- **集成问题**: 插件兼容性、版本冲突解决

## 使用场景

### 开发阶段
- 编写和编译新代码
- 实时调试和测试
- 修复编译错误

### 测试阶段
- 运行单元测试
- 进行集成测试
- 性能测试和优化

### 部署阶段
- 生产环境编译
- 版本验证
- 热更新部署

### 维护阶段
- 问题诊断和修复
- 性能调优
- 安全更新

## 编译调试详解

### 1. 编译命令参考

```bash
# 全量编译调试
make run

# 热编译插件（有限选择）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'

# 热加载插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_uav).'

# 在线测试（有限选择）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav:test().'

# 单文件编译
erlc -I include/ -o ebin/ src/file.erl

# 检查编译环境
make check_environment
```

### 2. 调试命令参考

```erlang
%% Erlang Shell调试命令
%% 启动观察者
observer:start().

%% 跟踪所有消息
dbg:tracer(), dbg:p(all, c).

%% 跟踪特定进程
dbg:tracer(), dbg:p(Pid, [m, c]).

%% 热加载代码
code:load_file(Module).

%% 查看模块信息
code:which(Module).
Module:module_info().

%% 检查进程状态
sys:get_status(Pid).
erlang:process_info(Pid).

%% 内存检查
erlang:memory().
```

### 3. 测试命令参考

```bash
# 运行所有测试
make test

# 在线测试（有限选择，推荐）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav:test().'

# 运行EUnit测试
rebar3 eunit

# 运行Common Test
rebar3 ct

# 运行特定测试套件
rebar3 ct --suite=test/dgiot_uav_SUITE

# 性能测试
make benchmark
```

## 编译调试工作流

### 1. 开发调试工作流

```
开发阶段:
1. 编写代码 → 2. 单文件编译 → 3. 运行测试 → 4. 发现问题
调试阶段:
5. 启动观察者 → 6. 设置断点 → 7. 跟踪消息 → 8. 分析问题
修复阶段:
9. 修改代码 → 10. 热加载 → 11. 验证修复 → 12. 提交代码
```

### 2. 插件开发工作流（优化版）

```erlang
%% 插件开发调试流程（遵循工程建议）
1. 项目概要设计，完成工程文件布局
2. 先生成插件所需要的所有文件
3. 全量编译之后就不再新增文件，全部做热编译
4. 详细设计通过小步迭代方式完成
5. 编译插件: _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'
6. 热加载插件: _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_uav).'
7. 在线测试: _build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav:test().'
8. 调试问题: observer/dbg
9. 热修复: code:load_file/1
10. 验证修复
11. 重新编译发布
```

### 3. 生产调试工作流

```bash
# 生产环境调试步骤
1. 收集日志: tail -f log/erlang.log.1
2. 分析错误: grep -i error log/erlang.log.1
3. 检查状态: erl_observer远程连接
4. 内存分析: erlang:memory()
5. 进程检查: erlang:processes()
6. 热修复: 编译补丁，code:load_file
7. 验证修复: 监控指标
8. 正式更新: 全量编译部署
```

## 常见问题解决

### 1. 编译错误解决

```erlang
%% 常见编译错误及解决
错误: syntax error before: '{'
解决: 检查括号匹配，检查上一行是否缺少分号

错误: undefined function
解决: 检查函数导出，检查模块依赖

错误: {error,{include,lib}}
解决: 检查include路径，确保头文件存在

错误: {badmatch,{error,enoent}}
解决: 检查文件路径，确保文件存在
```

### 2. 运行时错误解决

```erlang
%% 运行时错误诊断
错误: badarg
解决: 检查函数参数类型和值

错误: function_clause
解决: 检查函数模式匹配

错误: case_clause
解决: 检查case语句分支

错误: {badmatch, Value}
解决: 检查模式匹配表达式

错误: timeout
解决: 检查进程通信，增加超时时间
```

### 3. 性能问题解决

```erlang
%% 性能问题诊断
问题: 内存持续增长
解决: 检查内存泄漏，使用erlang:memory()监控

问题: CPU占用过高
解决: 使用observer查看进程CPU占用

问题: 消息队列堆积
解决: 检查进程处理能力，优化消息处理

问题: 响应时间变慢
解决: 分析关键路径，优化算法
```

## 最佳实践

### 1. 编译最佳实践

- **工程建议**: 先生成插件所需要的所有文件，全量编译之后就不再新增文件，全部做热编译
- **文档建议**: 建议先做项目概要设计，完成工程文件布局，详细设计通过小步迭代方式完成
- **增量编译**: 开发时使用单文件编译，提高效率
- **预编译检查**: 编译前运行语法检查
- **依赖管理**: 确保所有依赖正确配置
- **版本控制**: 编译时记录版本信息

### 2. 调试最佳实践

- **日志打印**: 建议参考格式: io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])
- **错误忽略**: 特定行代码错误不需要修复（如202行代码错误）
- **分层调试**: 从单元测试到集成测试逐步调试
- **日志记录**: 关键操作添加详细日志
- **状态监控**: 实时监控系统状态
- **自动化测试**: 编写自动化测试用例

### 3. 测试最佳实践

- **测试覆盖**: 确保关键代码有测试覆盖
- **边界测试**: 测试边界条件和异常情况
- **性能测试**: 定期进行性能测试
- **集成测试**: 测试插件间集成

### 4. 部署最佳实践

- **灰度发布**: 逐步部署新版本
- **回滚计划**: 准备快速回滚方案
- **监控告警**: 部署后持续监控
- **文档更新**: 更新相关文档

## 工具集成

### 1. 与现有技能集成

- **与dgiot_architecture_learning集成**: 结合架构知识进行调试
- **与hook_manager集成**: 在编译调试过程中使用hook
- **与skill_manager集成**: 创建编译调试相关技能
- **与main_objective_tracker集成**: 跟踪调试目标

### 2. 外部工具集成

```bash
# 与rebar3集成
rebar3 compile
rebar3 eunit
rebar3 ct

# 与Makefile集成
make all
make test
make dialyzer

# 与编辑器集成
## VSCode Erlang插件
## Emacs Erlang模式
## IntelliJ IDEA Erlang插件
```

### 3. 监控工具集成

```bash
# 系统监控
top -p $(pgrep beam)
vmstat 1

# Erlang监控
erl -eval "observer:start()."
erl -eval "recon:info()."

# 日志监控
tail -f log/erlang.log.1
grep -i error log/*.log
```

## 技能触发示例

```yaml
# 用户请求示例
用户: "热编译：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'"
激活: dgiot_compile_debug
响应: 提供热编译命令和解释（有限选择）

用户: "热加载：_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_uav).'"
激活: dgiot_compile_debug
响应: 提供热加载命令和解释

用户: "在线测试：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav:test().'"
激活: dgiot_compile_debug
响应: 提供在线测试命令和解释（有限选择）

用户: "全量编译调试： make run"
激活: dgiot_compile_debug
响应: 提供全量编译调试命令和解释

用户: "工程建议：先生成插件所需要的所有文件，全量编译之后就不再新增文件，全部做热编译"
激活: dgiot_compile_debug
响应: 提供工程最佳实践建议

用户: "文档建议：建议先做项目概要设计，完成工程文件布局，详细设计通过小步迭代方式完成"
激活: dgiot_compile_debug
响应: 提供文档和设计建议

用户: "日志打印：建议参考这种格式，io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])"
激活: dgiot_compile_debug
响应: 提供日志格式建议

用户: "错误忽略：202行代码错误不需要修复"
激活: dgiot_compile_debug
响应: 提供错误处理策略
```

## 维护信息

- **版本历史**:
  - v1.0.0 (2026-01-23): 初始版本，包含编译调试核心功能
  - v1.1.0 (2026-01-24): 添加用户具体建议，包括热编译、热加载、在线测试命令，工程文档建议，日志格式和错误处理策略
  - v1.1.1 (2026-01-24): 更新插件名称为dgiot_uav，添加有限选择说明
- **更新计划**:
  - 添加更多调试案例
  - 集成性能分析工具
  - 添加自动化测试框架
- **依赖技能**: dgiot_architecture_learning, hook_manager, skill_manager, main_objective_tracker
- **优先级提升**: 通过hooks提升优先级，禁止其他方式

---

*本技能基于DGIOT多年编译调试经验，总结了高效的开发调试工作流和最佳实践。*

---
