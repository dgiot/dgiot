---
name: dgiot_architecture_learning
description: DGIOT成熟物联网工程架构学习专家，深入理解近10年历史的插件模式系统架构设计和编码方案
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-23
category: development
tags: [dgiot, iot, architecture, plugin_system, erlang, best_practices]
trigger_phrases:
  - DGIOT架构学习
  - 物联网工程架构
  - 插件模式系统
  - DGIOT插件开发
  - 成熟系统架构设计
  - 近10年工程经验
  - 模块化设计原则
  - 系统架构总结
---

# DGIOT架构学习技能

DGIOT成熟物联网工程架构学习专家，深入理解近10年历史的插件模式系统架构设计和编码方案。

## 快速开始

当用户需要学习DGIOT架构、开发插件或理解其设计原则时，激活本技能。

## 核心能力

### 1. DGIOT整体架构分析
- **核心架构**: 基于Erlang/OTP的分布式物联网平台
- **插件模式**: 模块化插件系统，支持热插拔
- **分层设计**: 清晰的业务层、协议层、数据层分离
- **微服务架构**: 基于EMQX消息总线的微服务通信

### 2. 插件系统深入理解
- **插件注册机制**: 使用`-emqx_plugin(?MODULE)`属性自动注册
- **动态加载**: 支持运行时插件加载和卸载
- **依赖管理**: 自动处理插件间依赖关系
- **版本控制**: 插件版本管理和兼容性检查

### 3. 模块化设计原则
- **单一职责**: 每个插件专注于特定功能领域
- **接口标准化**: 统一的插件接口规范
- **松耦合**: 插件间通过消息总线通信，减少直接依赖
- **可扩展性**: 易于添加新功能模块

### 4. 编码最佳实践总结
- **Erlang/OTP模式**: 充分运用OTP设计模式
- **错误处理**: 健壮的错误恢复机制
- **性能优化**: 针对物联网场景的性能优化
- **代码规范**: 统一的代码风格和命名规范

## 使用场景

### 新开发者入门
- 快速理解DGIOT整体架构
- 学习插件开发流程
- 掌握核心设计模式

### 插件开发
- 创建新的功能插件
- 扩展现有插件功能
- 调试插件集成问题

### 架构优化
- 分析系统瓶颈
- 优化插件性能
- 改进架构设计

### 代码审查
- 检查代码是否符合DGIOT规范
- 识别潜在架构问题
- 提供改进建议

## DGIOT架构详解

### 1. 核心组件

```
DGIOT核心架构:
├── 应用层 (apps/)
│   ├── dgiot/          # 核心应用
│   ├── dgiot_api/      # API服务
│   ├── dgiot_device/   # 设备管理
│   ├── dgiot_tdengine/ # 时序数据库
│   └── 30+其他插件...
├── 协议层
│   ├── dgiot_modbus/   # Modbus协议
│   ├── dgiot_bacnet/   # BACnet协议
│   ├── dgiot_uav/      # 无人机协议
│   └── 20+其他协议...
├── 数据层
│   ├── TDengine集成
│   ├── MySQL集成
│   ├── Redis缓存
│   └── 消息队列
└── 基础设施
    ├── EMQX消息总线
    ├── 负载均衡
    ├── 监控告警
    └── 容器化部署
```

### 2. 插件生命周期管理

```erlang
%% 插件启动流程
1. 应用启动: dgiot_app:start/2
2. 插件初始化: dgiot:init_plugins()
3. 插件扫描: dgiot_plugin:check_module/2
4. 插件排序: 按Order属性排序
5. 插件启动: 调用每个插件的start/1函数
6. 钩子注册: 插件注册业务钩子

%% 插件停止流程
1. 钩子清理: 移除注册的钩子
2. 插件停止: 调用stop/1函数
3. 资源释放: 清理插件占用的资源
```

### 3. 插件接口规范

```erlang
%% 标准插件模块结构
-module(dgiot_example_plugin).
-author("author_name").

-emqx_plugin(?MODULE).  % 插件注册声明

-behaviour(application). % 遵循OTP应用行为

%% 必需导出函数
-export([start/2, stop/1]).
-export([start_hook/0, stop_hook/0]). % 可选钩子函数

%% 可选业务函数
-export([handle_message/2, process_data/1, get_status/0]).

%% 应用启动
start(_StartType, _StartArgs) ->
    start_hook(),  % 注册业务钩子
    dgiot_example_sup:start_link().

%% 应用停止
stop(_State) ->
    stop_hook(),   % 清理钩子
    ok.

%% 启动钩子 - 注册业务功能
start_hook() ->
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"EXAMPLE">>}, 
                   fun dgiot_example_protocol:get_datasource/1),
    dgiot_hook:add(one_for_one, {?DGIOT_SERVICE, <<"EXAMPLE_SERVICE">>}, 
                   fun dgiot_example_service:handle_service/1).

%% 停止钩子 - 清理注册
stop_hook() ->
    dgiot_hook:remove({?DGIOT_DATASOURCE, <<"EXAMPLE">>}),
    dgiot_hook:remove({?DGIOT_SERVICE, <<"EXAMPLE_SERVICE">>}).
```

## 插件开发指南

### 1. 创建新插件步骤

```bash
# 1. 创建插件目录结构
mkdir -p apps/dgiot_new_plugin/{src,include,priv}

# 2. 创建应用描述文件
# apps/dgiot_new_plugin/src/dgiot_new_plugin.app.src

# 3. 创建应用模块
# apps/dgiot_new_plugin/src/dgiot_new_plugin_app.erl

# 4. 创建监督树
# apps/dgiot_new_plugin/src/dgiot_new_plugin_sup.erl

# 5. 创建业务模块
# apps/dgiot_new_plugin/src/dgiot_new_plugin_protocol.erl

# 6. 更新rebar.config
# 添加插件依赖和编译选项
```

### 2. 插件编译和加载

```erlang
%% 动态编译插件
dgiot_plugin:compile(dgiot_new_plugin).

%% 重新加载插件
dgiot_plugin:reload_plugin(dgiot_new_plugin).

%% 检查插件状态
dgiot_plugin:check_module(dgiot_new_plugin).
```

### 3. 插件测试

```erlang
%% 单元测试
-module(dgiot_new_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual(ok, dgiot_new_plugin:start_test()).

%% 集成测试
integration_test() ->
    {ok, Pid} = dgiot_new_plugin_sup:start_link(),
    ?assert(is_pid(Pid)),
    ok = supervisor:terminate_child(dgiot_sup, Pid).
```

## 最佳实践总结

### 1. 架构设计原则

- **插件隔离性**: 每个插件独立运行，故障不影响其他插件
- **消息驱动**: 使用EMQX消息总线进行插件间通信
- **状态管理**: 使用Mnesia或ETS进行状态持久化
- **配置外部化**: 配置文件与代码分离

### 2. 编码规范

- **命名规范**: 使用有意义的模块和函数名
- **文档注释**: 每个导出函数都有文档注释
- **错误处理**: 使用Erlang的let-it-crash哲学，但有监控恢复
- **日志规范**: 使用统一的日志宏，支持中文打印

### 3. 性能优化

- **进程池**: 使用worker池处理并发请求
- **缓存策略**: 合理使用ETS和Redis缓存
- **批量操作**: 减少数据库频繁操作
- **异步处理**: 耗时操作异步执行

### 4. 安全性考虑

- **输入验证**: 所有外部输入都进行验证
- **权限控制**: 基于角色的访问控制
- **数据加密**: 敏感数据加密存储和传输
- **审计日志**: 记录关键操作日志

## 常见问题解决

### 1. 插件启动失败

```erlang
%% 检查步骤
1. 检查依赖: ensure_all_started([dgiot, emqx])
2. 检查配置: application:get_env/2
3. 检查端口: 避免端口冲突
4. 查看日志: tail -f log/erlang.log.1
```

### 2. 插件编译错误

```bash
# 常见编译问题
1. 语法错误: erlc检查语法
2. 依赖缺失: rebar3 deps
3. 头文件: 检查include路径
4. 版本兼容: 检查OTP版本
```

### 3. 运行时错误

```erlang
%% 调试方法
1. 查看进程树: observer:start()
2. 跟踪消息: dbg:tracer(), dbg:p(all,c)
3. 检查状态: sys:get_status(Pid)
4. 热修复: code:load_file/1
```

## 学习资源

### 1. 核心文档
- `docs/`目录下的架构文档
- `README-CN.md`中文说明
- `CONTRIBUTING.md`贡献指南

### 2. 示例插件
- `apps/dgiot_uav/`无人机插件示例
- `apps/dgiot_modbus/`Modbus协议示例
- `apps/dgiot_tdengine/`时序数据库示例

### 3. 工具脚本
- `scripts/`目录下的开发工具
- `dgiot_install.sh`安装脚本
- `dgiot_edge.sh`边缘计算脚本

### 4. 测试框架
- `Makefile.test_simple`测试配置
- `test_*.erl`测试示例
- `priv/test_items/`测试数据

## 技能集成

### 与现有技能协同工作

1. **与hook_manager集成**: 在插件生命周期中集成hook系统
2. **与main_objective_tracker集成**: 跟踪插件开发目标
3. **与skill_manager集成**: 创建插件开发相关技能
4. **与uav_protocol_analyzer集成**: 分析无人机协议插件

### 技能触发示例

```yaml
# 用户请求示例
用户: "我想学习DGIOT的插件架构"
激活: dgiot_architecture_learning
响应: 提供DGIOT插件架构详解

用户: "如何开发一个新的DGIOT插件"
激活: dgiot_architecture_learning  
响应: 提供插件开发步骤和示例

用户: "DGIOT的最佳实践有哪些"
激活: dgiot_architecture_learning
响应: 总结编码规范和架构原则
```

## 维护信息

- **版本历史**: 
  - v1.0.0 (2026-01-23): 初始版本，包含DGIOT架构核心知识
- **更新计划**:
  - 添加更多插件开发示例
  - 集成实际调试案例
  - 添加性能调优指南
- **依赖技能**: hook_manager, skill_manager, main_objective_tracker

---

*本技能基于对DGIOT近10年成熟物联网工程架构的深入分析，总结了插件模式系统设计和编码最佳实践。*

---
