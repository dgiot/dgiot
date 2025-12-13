# CodeAI通用开发规则

## 概述

本文件定义了DG-IoT平台开发的通用规则，适用于所有插件和模块的开发工作。

## 开发命令

### 1. 热编译和热加载

```bash
# 通用插件热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 通用插件热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot).'

# 特定插件热编译（示例：modbus）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 特定插件热加载（示例：modbus）
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_modbus).'
```

### 2. 测试命令

```bash
# 在线测试（UDP多播测试）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_udp_test:test_multicast_with_tcpdump().'

# 全量编译调试
make run

# 特定插件测试（示例：modbus）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus:test().'
```

### 3. API调试

```bash
# 新增API后更新Schema
dgiot_parse_utils:update_schemas_json().
```

## 工程建议

### 1. 文件生成策略

- **先生成插件所需要的所有文件**：在全量编译之前，确保所有必要的文件都已创建
- **全量编译之后就不再新增文件**：一旦完成全量编译，后续修改只做热编译
- **全部做热编译**：日常开发中使用热编译提高效率

### 2. 文档设计流程

- **建议先做项目概要设计**：明确项目目标、范围和架构
- **完成工程文件布局**：创建标准的目录结构和文件组织
- **详细设计通过小步迭代方式完成**：采用敏捷开发，小步快跑

## 编码规范

### 1. 日志打印格式

```erlang
% 标准日志格式
io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])

% 带上下文的日志
io:format("~s ~p [~p] ~p = ~p.~n", [?FILE, ?LINE, Module, Action, Data])
```

### 2. 错误处理

```erlang
% 使用try-catch处理异常
try
    do_something()
catch
    error:Reason ->
        io:format("~s ~p Error: ~p~n", [?FILE, ?LINE, Reason]),
        {error, Reason}
end.
```

### 3. 函数命名约定

- **动词+名词**：如 `parse_packet/1`, `send_message/2`
- **查询函数**：以 `get_` 开头，如 `get_device_status/1`
- **设置函数**：以 `set_` 开头，如 `set_config/2`
- **检查函数**：以 `check_` 或 `is_` 开头，如 `check_connection/0`, `is_valid/1`

## 测试规范

### 1. 单元测试

```erlang
% 测试文件命名：*_test.erl
% 测试函数命名：*_test_*
-module(dgiot_example_test).

-include_lib("eunit/include/eunit.hrl").

parse_packet_test() ->
    Packet = <<16#EB, 16#90, 0, 0, 0, 0>>,
    ?assertEqual({ok, #{magic => 16#EB90}}, dgiot_example:parse_packet(Packet)).
```

### 2. 集成测试

- 创建独立的测试目录：`test/`
- 使用自动化测试脚本
- 包含网络测试和协议测试

### 3. 性能测试

- 使用压力测试工具
- 监控内存和CPU使用
- 记录响应时间

## 部署规范

### 1. 版本管理

- 使用语义化版本号：`主版本.次版本.修订版本`
- 更新CHANGELOG.md文件
- 打标签发布

### 2. 配置管理

- 环境特定的配置文件
- 敏感信息使用环境变量
- 配置验证和默认值

### 3. 监控和告警

- 集成监控指标
- 设置性能阈值
- 异常告警机制

## 最佳实践

### 1. 代码审查

- 使用统一的代码风格
- 检查错误处理是否完整
- 验证测试覆盖率

### 2. 文档维护

- 保持README.md更新
- 添加代码注释
- 创建API文档

### 3. 性能优化

- 避免不必要的进程创建
- 使用ETS表缓存数据
- 优化网络通信

## 文件读取优化

为了提高效率并避免处理大文件时出现问题，CodeAI在读取文件时应遵循以下规则：

- 在读取文件之前，先检查文件大小。
- 如果文件大小超过 **10 MB**（10,485,760字节），则认为文件过大，不读取全部内容。
- 对于大文件，采取以下措施：
  - 如果只需要查看文件的部分内容，使用 `head` 或 `tail` 命令读取前100行或后100行。
  - 当使用 `read_file` 工具时，如果检测到大文件，应提示用户文件过大，只显示部分内容，并给出选项是否继续读取全部内容。
- 默认情况下，当文件过大时，自动使用 `head -n 100` 读取前100行并返回，同时在结果中注明文件大小并提示此为部分内容。

示例：

```bash
# 检查文件大小
stat -c %s filename

# 读取前100行
head -n 100 filename

# 读取后100行
tail -n 100 filename
```

## 故障排除

### 1. 常见问题

```bash
# 插件加载失败
# 检查依赖是否正确
# 验证模块导出函数

# 编译错误
# 检查语法错误
# 验证函数参数
```

### 2. 调试技巧

```erlang
% 使用dbg进行调试
dbg:tracer().
dbg:p(all, c).
dbg:tpl(Module, Function, x).

% 打印调试信息
io:format("DEBUG: ~p = ~p~n", [VariableName, VariableValue]).
```

## 更新记录

- 2025-12-03：创建通用规则文档
- 2025-12-06：新增文件读取优化规则
- 基于现有规则优化和扩展

## 相关链接

- [DG-IoT官方文档](https://github.com/dgiot/dgiot)
- [Erlang/OTP编程指南](http://erlang.org/doc/)
- [EUnit测试框架](http://erlang.org/doc/apps/eunit/chapter.html)
