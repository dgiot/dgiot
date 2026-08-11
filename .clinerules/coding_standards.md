b# DG-IoT编码规范

## 概述

Erlang/OTP编码规范和最佳实践，确保代码质量和一致性。

## 核心规范

### 1. 模块结构
- 使用标准模块模板
- 包含必要的`-export`和`-include`
- 函数有`@doc`注释

### 2. 函数规范
- **函数命名**：动词+名词（`parse_packet/1`, `send_message/2`）
- **函数注释**：使用`@doc`、`@spec`、`@param`、`@returns`
- **三层架构**：API Gateway → Function Gateway → Implementation

### 3. 错误处理
- **返回值**：`{ok, Result}` / `{error, Reason}`
- **异常处理**：使用`try-catch`处理异常

### 4. 日志格式
- **标准格式**：`io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])`
- **带上下文**：`io:format("~s ~p [~p] ~p = ~p.~n", [?FILE, ?LINE, Module, Action, Data])`

### 5. 非ASCII字符串打印
- **核心原则**：非ASCII字符串必须使用`<<"内容"/utf8>>`格式
- **推荐方案**：优先使用`dgiot_utils:safe_format`函数
- **检查清单**：检查所有打印语句是否包含非ASCII字符

### 8. 避免硬编码路径规范
- **原则**：禁止硬编码绝对路径，使用相对路径或环境变量
- **示例**：`cd "$(dirname "$0")/../.."` 代替 `cd /absolute/path`

### 9. 日志系统重用规则
- **原则**：重用现有OTP logger系统，禁止创建新日志系统
- **动态调整**：使用`logger:set_module_level/2`调整日志级别
- **商用环境**：默认error级别，只记录错误

### 10. 代码重用规则
- **原则**：写代码前先查找现有实现
- **优先级**：平台核心函数 > 插件通用函数 > 相似模块函数 > 第三方库
- **禁止**：重复实现平台已有功能

### 11. Parse库操作规则
- **核心原则**：禁止在Parse库中随意添加字段
- **适用范围**：Product、Device等所有Parse对象
- **操作限制**：
  - 只能查询和操作已有的标准字段
  - 不允许添加自定义字段到Parse对象
  - 确保数据库结构一致性和可维护性
- **违规后果**：可能导致系统不稳定、数据结构混乱、数据同步失败
- **正确做法**：
  - 使用content字段存储业务配置（已在产品/设备模型中定义）
  - 时序数据存储到TDengine
  - 其他数据使用合适的存储层（ETS、DETS、Mnesia等）

## 目录结构
```
apps/dgiot_plugin/
├── src/
│   ├── dgiot_plugin.erl          # 函数网关
│   ├── dgiot_plugin_handler.erl   # API网关
│   ├── dgiot_plugin_service.erl   # 业务服务
│   └── dgiot_parser.erl          # 协议解析
├── include/dgiot_plugin.hrl
└── test/dgiot_plugin_test.erl
```

## 子模块命名规范
- `*_service.erl` - 业务逻辑服务
- `*_dao.erl` - 数据访问对象
- `*_utils.erl` - 工具函数
- `*_parser.erl` - 协议解析
- `*_channel.erl` - 通道管理
- `*_handler.erl` - HTTP请求处理（API网关）

## 快速检查清单
- [ ] 模块结构符合模板
- [ ] 函数有`@doc`注释
- [ ] 遵循三层架构
- [ ] 错误处理完整
- [ ] 日志格式正确
- [ ] 有单元测试
- [ ] **Parse库操作未添加自定义字段**

## 更新记录
- 2025-12-19：融合全局规则，创建统一编码规范
- 2026-03-16：添加Parse库操作规则，禁止随意添加字段
