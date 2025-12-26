
# DG-IoT通用开发规则

## 概述

本文件定义了DG-IoT平台开发的通用规则，适用于所有插件和模块的开发工作。

## 编译命令规范

### ✅ 允许使用的命令

1. **`make`** - 全量编译检查，只编译不运行
   - 用途：验证编译是否通过，检查语法错误
   - 注意：可能耗时较长，建议在提交前使用

2. **`make run`** - 全量编译并启动在线调试环境
   - 用途：首次环境搭建、新增文件、依赖变更、重大重构后验证

### ❌ 禁止使用的命令

1. **`make compile`** - 此命令不存在或不支持，可能导致系统卡死或错误

### 🔥 日常开发推荐（优先使用）

- **热编译命令** - 提高开发效率：

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

### ⚠️ 热编译质量要求

1. **零警告原则**：热编译必须消除所有编译警告
   - 修复未使用变量警告（使用`_`前缀或移除未使用变量）
   - 修复未使用函数警告（添加`-export`或移除未使用函数）
   - 修复类型不匹配警告（确保类型一致性）

2. **代码质量检查清单**（每次热编译前检查）：
   - [ ] 没有未使用变量（`unused_var`警告）
   - [ ] 没有未使用函数（`unused_function`警告）
   - [ ] 没有语法错误（`syntax_error`）
   - [ ] 没有类型不匹配（`type_mismatch`）
   - [ ] 没有拼写错误（如`fasle`应为`false`）

3. **常见警告修复示例**：
   ```erlang
   % 错误：未使用变量警告
   parse_frame(<<MbAddr:8, BadCode:8, ErrorCode:8, Crc:2/binary>> = Buff, Acc,
       #{<<"addr">> := DtuAddr} = State) ->
   
   % 修复：移除未使用的DtuAddr
   parse_frame(<<MbAddr:8, BadCode:8, ErrorCode:8, Crc:2/binary>> = Buff, Acc, State) ->
   ```

   ```erlang
   % 错误：未使用变量警告
   get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc) ->
   
   % 修复：使用_前缀表示忽略该变量
   get_write(ResponseData, SlaveId, FunCode, _DtuAddr, ProductId, Address, Acc) ->
   ```

   ```erlang
   % 错误：未使用变量警告
   case dgiot_product:lookup_prod(ProductId) of
       {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
           {ok, Result};
       Error ->
           {error, product_not_found}
   end.
   
   % 修复：使用_前缀
   case dgiot_product:lookup_prod(ProductId) of
       {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
           {ok, Result};
       _Error ->
           {error, product_not_found}
   end.
   ```

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

## 快速检查清单

- [ ] 使用正确的编译命令
- [ ] 热编译零警告（必须满足）
- [ ] 日志格式符合规范
- [ ] 错误处理完整
- [ ] 有单元测试
- [ ] 遵循三层架构

## 更新记录

- 2025-12-19：融合全局规则，创建统一通用开发规则
- 2025-12-24：添加热编译质量要求，强调零警告原则
