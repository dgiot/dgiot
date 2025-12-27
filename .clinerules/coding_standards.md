b# DG-IoT编码规范

## 概述

Erlang/OTP编码规范和最佳实践，确保代码质量和一致性。

## 核心规范

### 1. 模块结构

```erlang
%%%-------------------------------------------------------------------
%%% @doc 模块说明 @end
%%%-------------------------------------------------------------------
-module(dgiot_example).
-export([start/0, stop/0, parse_packet/1]).
-include("dgiot.hrl").

%% @doc 启动函数
start() -> ok.

%% @doc 解析数据包
parse_packet(Packet) when is_binary(Packet) ->
    case Packet of
        <<16#EB, 16#90, Rest/binary>> -> {ok, #{magic => 16#EB90, data => Rest}};
        _ -> {error, invalid_magic}
    end.
```

### 2. 函数规范

#### 2.1 函数注释

```erlang
%% @doc 函数说明
%% @spec function_name(Type1, Type2) -> ReturnType
%% @param Param1 参数1说明
%% @param Param2 参数2说明
%% @returns 返回值说明
function_name(Param1, Param2) ->
    % 函数体
    ok.
```

#### 2.2 函数命名

- **动词+名词**：`parse_packet/1`, `send_message/2`
- **查询函数**：`get_device_status/1`, `find_by_id/1`
- **设置函数**：`set_config/2`, `update_status/2`
- **检查函数**：`check_connection/0`, `is_valid/1`
- **转换函数**：`to_binary/1`, `from_json/1`

### 3. 三层架构（必须遵守）

```
API Gateway (handler) → Function Gateway (dgiot_*.erl) → Implementation (service/dao)
```

- **禁止**在Handler中实现业务逻辑
- **禁止**在Function Gateway中实现具体逻辑

### 4. ETS表规范

```erlang
-dgiot_data("ets").
-export([init_ets/0]).
-define(USER_ETS, user_ets).

init_ets() ->
    dgiot_data:init(?USER_ETS),
    dgiot_data:init(?ORDER_ETS, [ordered_set]).
```

### 5. 错误处理规范

#### 5.1 返回值约定

```erlang
% 成功返回 {ok, Result}
% 失败返回 {error, Reason}
% 可选返回 {ok, Result, Extra}
```

#### 5.2 异常处理

```erlang
% 使用try-catch处理异常
safe_operation(Data) ->
    try
        do_risky_operation(Data)
    catch
        error:badarg ->
            {error, invalid_argument};
        error:Reason ->
            {error, Reason}
    end.
```

### 6. 日志格式

```erlang
% 标准日志格式
io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])

% 带上下文的日志
io:format("~s ~p [~p] ~p = ~p.~n", [?FILE, ?LINE, Module, Action, Data])
```

### 7. 非ASCII字符串打印规范

#### 7.1 适用范围
所有包含非ASCII字符（中文、日文、韩文、俄文、阿拉伯文等）的字符串打印，包括：
- `io:format/2` 函数调用
- `lager:info/2`、`lager:debug/2` 等日志函数
- `?LOG` 宏输出
- 其他字符串输出函数

#### 7.2 核心原则
**非ASCII字符串必须使用二进制格式配合 `/utf8` 标志**

```erlang
% 错误：直接使用非ASCII字符串字面量
io:format("中文测试~n").                % ❌ 可能导致编码错误
io:format("日本語テスト~n").            % ❌ 可能导致编码错误

% 正确：显式声明UTF-8编码的二进制
io:format("~p ~n", [<<"中文测试"/utf8>>]).      % ✅ 确保兼容性
io:format("~p ~n", [<<"日本語テスト"/utf8>>]).  % ✅ 确保兼容性
```

#### 7.3 技术原理
Erlang shell的 `list_to_binary/1` 函数在处理非ASCII字符时可能因编码问题失败。
使用 `<<"内容"/utf8>>` 格式可确保字符串以正确的UTF-8编码存储和传输。

#### 7.4 实用示例模板
```erlang
% 纯非ASCII字符串打印
io:format("~p ~n", [<<"设备注册成功"/utf8>>]).

% 混合内容（ASCII + 非ASCII）
io:format("Device ~s: ~p ~n", [DeviceId, <<"状态更新成功"/utf8>>]).

% 日志中的非ASCII内容
?LOG(info, "~p", [<<"数据接收完成"/utf8>>]).
?LOG(debug, "~p: ~p", [<<"调试信息"/utf8>>, Data]).

% 调试信息中的非ASCII内容
io:format("~s ~p [DEBUG] ~p = ~p ~n", [
    ?FILE, ?LINE, <<"解析结果"/utf8>>, Result
]).

% 多语言支持示例
io:format("~p ~n", [<<"English: OK, 中文: 成功, 日本語: 成功"/utf8>>]).
```

#### 7.5 检查清单
- [ ] 检查所有打印语句是否包含非ASCII字符
- [ ] 确认非ASCII字符串使用 `<<"内容"/utf8>>` 格式
- [ ] 验证 `io:format/2`、`lager:info/2`、`?LOG` 宏等输出
- [ ] 确保单元测试中的断言也遵循此规范

### 7.6 安全打印函数规范（推荐）

#### 7.6.1 安全打印函数目的
为解决中文打印编码问题，平台提供了标准的安全打印函数 `dgiot_utils:safe_format/2` 和 `dgiot_utils:safe_format/3`，推荐在所有需要打印非ASCII字符的场景中使用。

#### 7.6.2 安全打印函数优势
- **自动编码处理**：自动将字符串转换为UTF-8编码的二进制格式
- **统一解决方案**：避免每个模块重复处理编码问题
- **易于调试**：自动添加文件路径和行号信息
- **类型安全**：支持各种数据类型（二进制、字符串、原子、整数、浮点数、映射等）

#### 7.6.3 使用示例
```erlang
% 传统方式（容易出错）
io:format("设备注册成功~n").  % ❌ 可能导致中文编码错误

% 手动处理方式（繁琐）
io:format("~p ~n", [<<"设备注册成功"/utf8>>]).  % ✅ 但繁琐

% 推荐：使用安全打印函数
dgiot_utils:safe_format("设备注册成功~n", []).  % ✅ 简洁安全
dgiot_utils:safe_format("设备 ~s 状态: ~p~n", [DeviceId, Status]).  % ✅ 混合内容
```

#### 7.6.4 安全打印函数实现原理
```erlang
%% @doc 安全格式化字符串，解决中文打印问题
%% 使用二进制格式配合/utf8标志，确保非ASCII字符正确显示
safe_format(Format, Args) ->
    % 将格式字符串转换为二进制，确保UTF-8编码
    BinaryFormat = ensure_utf8_binary(Format),
    % 将参数转换为二进制格式
    BinaryArgs = lists:map(fun ensure_utf8_binary/1, Args),
    % 使用io:format打印
    io:format(BinaryFormat, BinaryArgs).

ensure_utf8_binary(Value) when is_binary(Value) ->
    case unicode:characters_to_binary(Value, utf8, utf8) of
        {error, _, _} -> <<"Invalid UTF-8 data">>/utf8;
        {incomplete, _, _} -> <<"Incomplete UTF-8 data">>/utf8;
        Binary -> Binary
    end.
```

#### 7.6.5 检查清单
- [ ] 新代码优先使用 `dgiot_utils:safe_format` 函数
- [ ] 现有代码逐步迁移到安全打印函数
- [ ] 测试脚本必须使用安全打印函数
- [ ] 确保所有中文打印都正确处理编码

### 8. 避免硬编码路径规范（高优先级）

#### 8.1 适用范围
所有Shell脚本、配置文件、测试脚本中的文件路径和目录引用。

#### 8.2 核心原则
**禁止在代码中硬编码绝对路径，必须使用相对路径或环境变量**

```bash
# 错误：硬编码绝对路径 ❌
cd /absolute/path/to/project
source /absolute/path/to/project/scripts/config.sh

# 正确：使用相对路径或变量 ✅
cd "$(dirname "$0")/../.."
PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
source "$PROJECT_ROOT/scripts/config.sh"
```

#### 8.3 推荐方案
1. **使用相对路径**：基于脚本位置计算项目根目录
2. **环境变量**：通过环境变量传递关键路径
3. **参数传递**：通过命令行参数传递路径
4. **配置文件**：将路径配置在配置文件中

#### 8.4 实用示例模板
```bash
#!/bin/bash
# 示例：避免硬编码路径的最佳实践

# 方案1：基于脚本位置计算项目根目录
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# 方案2：使用环境变量（优先）
PROJECT_ROOT="${DGIT_PROJECT_ROOT:-$(pwd)}"

# 方案3：通过参数传递
if [ -n "$1" ]; then
    PROJECT_ROOT="$1"
else
    PROJECT_ROOT="$(pwd)"
fi

# 使用变量代替硬编码路径
cd "$PROJECT_ROOT"
source "$PROJECT_ROOT/scripts/common.sh"
LOG_FILE="$PROJECT_ROOT/logs/test_$(date +%Y%m%d).log"
```

#### 8.5 检查清单（高优先级）
- [ ] 检查脚本中是否包含 `/root/`、`/home/` 等硬编码路径
- [ ] 确认使用相对路径或环境变量
- [ ] 验证路径计算逻辑的正确性
- [ ] 确保跨平台兼容性（避免Windows/Unix路径差异）

### 9. 日志系统重用规则（必须遵守）

#### 9.1 重用现有日志系统原则
- **原则**：必须重用平台现有的OTP logger系统，禁止创建新的日志系统
- **配置继承**：模块默认继承系统全局日志级别（`emqx.conf`中的`log.level`）
- **动态调整**：使用标准`logger:set_module_level/2`进行模块级别动态调整

#### 9.2 日志级别使用规范
```erlang
% 正确：使用标准?LOG宏，遵循现有日志系统
?LOG(debug, "详细调试信息: ~p", [Data])      % 调试用，商用环境默认不显示
?LOG(info, "重要事件: ~p", [Event])          % 重要事件，商用环境默认不显示
?LOG(warning, "警告信息: ~p", [Warning])     % 警告信息，商用环境默认不显示
?LOG(error, "错误信息: ~p", [Error])         % 错误信息，商用环境默认显示

% 错误：不要创建自定义日志系统或环境变量配置
% ❌ os:getenv("DGIOT_MODBUS_LOG_LEVEL")  # 不要添加环境变量配置
% ❌ 自定义日志函数或宏                     # 不要重复造轮子
```

#### 9.3 商用与调试环境管理
```bash
# 商用环境：使用系统默认error级别（emqx.conf配置）
# 无需任何调整，只记录错误日志

# 调试环境：动态调整模块级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, debug).'

# 查看当前模块日志级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:get_module_level(dgiot_modbusrtu_tcp).'

# 恢复商用级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'
```

#### 9.4 检查清单
- [ ] 是否使用了标准`?LOG`宏？
- [ ] 是否避免了环境变量配置日志级别？
- [ ] 是否重用现有OTP logger系统？
- [ ] 是否通过动态调整而非代码修改来切换日志级别？
- [ ] 商用环境是否保持默认error级别？

### 10. 代码重用规则（必须遵守）

#### 10.1 写代码前先查找现有实现
- **原则**：在编写新代码之前，必须先查找是否有现有的实现
- **查找范围**：
  1. 当前模块内的相似函数
  2. 同一插件内的其他模块
  3. 其他插件中的通用实现
  4. 平台核心库（`dgiot/include/`）

#### 10.2 查找方法
```bash
# 1. 使用grep查找相似功能
grep -r "function_name" apps/ --include="*.erl"

# 2. 使用search_files工具查找
# 在Cline中使用：search_files工具

# 3. 查看相关模块的导出函数
grep -n "export" apps/dgiot_*/src/*.erl | head -20
```

#### 10.3 重用优先级
1. **平台核心函数**：优先使用`dgiot_utils`、`dgiot_data`等核心模块
2. **插件通用函数**：使用同一插件内的通用函数
3. **相似模块函数**：参考相似模块的实现
4. **第三方库**：使用标准的OTP库和第三方库

#### 10.4 避免重复造轮子
- **禁止**：重复实现平台已有的功能
- **要求**：使用现有的工具函数和库
- **示例**：
  ```erlang
  % 错误：自己实现二进制转十六进制
  custom_binary_to_hex(Binary) -> ...
  
  % 正确：使用平台提供的函数
  dgiot_utils:binary_to_hex(Binary)
  ```

#### 10.5 代码复用检查清单
- [ ] 是否查找过现有实现？
- [ ] 是否使用了平台核心函数？
- [ ] 是否避免了重复实现？
- [ ] 是否参考了相似模块？
- [ ] 是否使用了标准的OTP库？

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
- `*_statistics.erl` - 统计功能
- `*_handler.erl` - HTTP请求处理（API网关）

## 快速检查清单

- [ ] 模块结构符合模板
- [ ] 函数有`@doc`注释
- [ ] 遵循三层架构
- [ ] 错误处理完整
- [ ] 日志格式正确
- [ ] 有单元测试
- [ ] ETS表规范正确

## 更新记录

- 2025-12-19：融合全局规则，创建统一编码规范
