---
name: erlang_logging_solution
description: Erlang日志打印解决方案，提供统一的日志宏定义、中文打印支持和最佳实践
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-22
category: development
tags: [erlang, logging, chinese, unicode, best_practices]
trigger_phrases:
  - 日志打印
  - 中文打印
  - Erlang日志
  - io:format
  - 日志宏
  - 乱码问题
  - 中文输出
  - Unicode编码
---

# Erlang日志打印解决方案

## 概述

本技能提供Erlang项目中统一的日志打印解决方案，特别针对中文打印和Unicode编码问题。基于对DGIOT无人机项目代码的分析，总结了项目中使用的各种日志打印模式，并提供了最佳实践。

## 核心问题

在Erlang项目中，常见的日志打印问题包括：
1. **中文乱码**：使用`io:format`直接打印中文导致乱码
2. **日志格式不统一**：不同模块使用不同的日志宏定义
3. **缺少日志级别**：没有统一的DEBUG/INFO/WARNING/ERROR级别
4. **性能问题**：频繁的日志打印影响性能

## 解决方案

### 1. DGIOT项目实际日志模式

基于对DGIOT项目的全局搜索分析，项目中实际使用的日志模式如下：

#### 1.1 日志头文件引入
```erlang
%% 在Erlang模块顶部引入日志头文件
-include_lib("dgiot/include/logger.hrl").
```

#### 1.2 实际日志宏使用
DGIOT项目使用`?LOG(Level, Format, Args)`宏，其中：
- **Level**: debug, info, warning, error, notice, critical, alert, emergency
- **Format**: 格式化字符串，支持`~p`, `~s`, `~ts`等格式
- **Args**: 参数列表

#### 1.3 常见日志模式示例
```erlang
%% 基本日志
?LOG(info, "Channel ~p", [Event]).

%% 带中文的日志（使用~ts格式）
?LOG(info, "~ts: ~p", [<<"磁航向报文处理"/utf8>>, dgiot_utils:binary_to_hex(Packet)]).

%% 错误日志
?LOG(error, "Failed to create device ~p: ~p", [DeviceId, Reason]).

%% 调试日志
?LOG(debug, "Parent device ~p found for device ~p", [ParentId, DeviceId]).

%% 警告日志（通道停止）
?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]).
?LOG(info, "channel stop ~p,~p", [ChannelType, ChannelId]).
```

#### 1.4 日志级别优先级
根据搜索结果，项目中实际使用的日志级别优先级（从高到低）：
1. **error** - 错误信息，需要立即关注
2. **warning** - 警告信息，潜在问题
3. **info** - 重要运行时信息
4. **debug** - 调试信息，生产环境通常关闭
5. **notice** - 通知信息（较少使用）

### 2. 统一的日志宏定义

基于DGIOT项目的最佳实践，提供以下日志宏定义：

```erlang
%% 简化日志宏定义（适用于没有dgiot_uav.hrl的情况）
-define(UAV_DEBUG(Format), io:format("[DEBUG] " ++ Format ++ "~n")).
-define(UAV_DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-define(UAV_INFO(Format), io:format("[INFO] " ++ Format ++ "~n")).
-define(UAV_INFO(Format, Args), io:format("[INFO] " ++ Format ++ "~n", Args)).
-define(UAV_WARNING(Format), io:format("[WARNING] " ++ Format ++ "~n")).
-define(UAV_WARNING(Format, Args), io:format("[WARNING] " ++ Format ++ "~n", Args)).
-define(UAV_ERROR(Format), io:format("[ERROR] " ++ Format ++ "~n")).
-define(UAV_ERROR(Format, Args), io:format("[ERROR] " ++ Format ++ "~n", Args)).
```

### 2. 中文打印解决方案

针对中文打印问题，提供以下解决方案：

#### 方案A：使用Unicode编码（推荐）
```erlang
%% 使用~ts格式和<<"/utf8>>语法
io:format("=== ~ts ===~n", [<<"测试control_protocol模块中文打印"/utf8>>]),
io:format("~n~ts:~n", [<<"平台类型解析测试"/utf8>>]),
```

#### 方案B：使用unicode:characters_to_binary/1
```erlang
%% 使用unicode:characters_to_binary转换
io:format("~ts ", [unicode:characters_to_binary("开始测试")]), io:format("~n"),
```

#### 方案C：环境设置
```erlang
%% 在Erlang shell中设置正确的编码环境
io:setopts([{encoding, unicode}]).

%% 系统环境变量设置
%% Linux/Mac: export LANG=zh_CN.UTF-8
%% Windows: chcp 65001 并设置字体为支持中文的字体
```

### 3. 高级日志宏（带模块和行号）

```erlang
%% 高级日志宏，包含模块、函数名和行号
-define(LOG(Level, Format, Args),
    io:format("~p [~p:~p:~p] " ++ Format ++ "~n",
              [Level, ?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

%% 使用示例
?LOG(info, "设备 ~p 已注册，PID: ~p", [DeviceType, DevicePid]).
```

### 4. 性能优化日志宏

```erlang
%% 条件编译日志宏，生产环境可关闭DEBUG日志
-ifdef(DEBUG).
-define(DEBUG_LOG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-else.
-define(DEBUG_LOG(Format, Args), ok).
-endif.

%% 惰性求值日志宏，避免不必要的字符串构建
-define(LAZY_DEBUG(Fun),
    case application:get_env(your_app, debug_mode, false) of
        true -> io:format("[DEBUG] ~s~n", [Fun()]);
        false -> ok
    end).
```

## 使用示例

### 示例1：基本日志打印
```erlang
-module(example_module).

%% 引入日志宏
-define(INFO(Format, Args), io:format("[INFO] " ++ Format ++ "~n", Args)).
-define(WARNING(Format, Args), io:format("[WARNING] " ++ Format ++ "~n", Args)).
-define(ERROR(Format, Args), io:format("[ERROR] " ++ Format ++ "~n", Args)).
-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).

test_function() ->
    ?INFO("开始执行测试函数", []),
    
    try
        Result = do_something(),
        ?DEBUG("计算结果: ~p", [Result]),
        ?INFO("测试成功完成", [])
    catch
        _:Error ->
            ?ERROR("测试失败: ~p", [Error]),
            {error, Error}
    end.
```

### 示例2：中文日志打印
```erlang
-module(chinese_example).

test_chinese_printing() ->
    %% 设置编码环境
    io:setopts([{encoding, unicode}]),
    
    %% 使用Unicode编码打印中文
    io:format("========== ~ts ==========~n~n", [<<"开始中文打印修复测试"/utf8>>]),
    
    %% 测试各种中文打印场景
    TestCommands = [16#A2, 16#E6, 16#3D, 16#E9, 16#51, 16#B8, 16#B9, 16#3F],
    lists:foreach(fun(Cmd) ->
        Result = control_protocol:switch_command_str(Cmd),
        io:format("  命令 0x~2.16.0B: ~ts~n", [Cmd, Result])
    end, TestCommands),
    
    io:format("~n========== ~ts ==========~n", [<<"中文打印修复测试完成"/utf8>>]).
```

### 示例3：结构化日志
```erlang
-module(structured_logging).

%% 结构化日志宏
-define(STRUCTURED_LOG(Level, Event, Data),
    io:format("[~p] event=~s ~s~n", 
              [Level, Event, format_kv_pairs(Data)])).

format_kv_pairs(Data) ->
    lists:flatten([io_lib:format("~s=~p ", [K, V]) || {K, V} <- maps:to_list(Data)]).

log_user_login(UserId, Ip) ->
    ?STRUCTURED_LOG(info, "user_login", #{
        user_id => UserId,
        ip_address => Ip,
        timestamp => erlang:system_time(millisecond),
        success => true
    }).
```

## 最佳实践

### 1. 日志级别使用规范
- **DEBUG**: 详细的调试信息，生产环境通常关闭
- **INFO**: 重要的运行时信息（服务启动、配置加载等）
- **WARNING**: 潜在的问题，但不影响系统运行
- **ERROR**: 错误信息，需要关注和修复

### 2. 中文打印规范
1. 始终使用`~ts`格式而不是`~s`
2. 中文字符串使用`<<"中文"/utf8>>`语法
3. 在模块初始化时设置`io:setopts([{encoding, unicode}])`
4. 确保终端环境支持UTF-8编码

### 3. 性能优化建议
1. 使用条件编译控制DEBUG日志
2. 避免在热路径中进行昂贵的日志计算
3. 使用惰性求值避免不必要的字符串构建
4. 考虑使用异步日志记录

### 4. 日志格式统一
1. 项目中使用统一的日志宏定义
2. 包含时间戳、模块名、函数名等上下文信息
3. 结构化日志便于机器解析
4. 一致的日志级别和格式

## 常见问题解决

### 问题1：中文显示为乱码
**解决方案**：
```erlang
%% 方法1：设置编码环境
io:setopts([{encoding, unicode}]).

%% 方法2：使用正确的格式和编码
io:format("~ts~n", [<<"中文测试"/utf8>>]).

%% 方法3：检查系统环境
%% 在shell中执行：export LANG=zh_CN.UTF-8
```

### 问题2：日志输出到文件
**解决方案**：
```erlang
%% 将日志重定向到文件
{ok, LogFile} = file:open("app.log", [write, append]),
io:format(LogFile, "[~p] ~s~n", [Level, Message]),
file:close(LogFile).

%% 或者使用lager等日志库
```

### 问题3：日志性能问题
**解决方案**：
```erlang
%% 使用条件编译
-ifdef(PRODUCTION).
-define(LOG_DEBUG(Format, Args), ok).
-else.
-define(LOG_DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-endif.

%% 使用惰性求值
-define(LOG_DEBUG(Fun),
    case application:get_env(app, debug, false) of
        true -> io:format("[DEBUG] ~s~n", [Fun()]);
        false -> ok
    end).
```

## 集成到现有项目

### 步骤1：创建公共日志头文件
```erlang
%% 文件：include/app_logger.hrl
%% 统一的日志宏定义

-ifndef(APP_LOGGER_HRL).
-define(APP_LOGGER_HRL, true).

%% 基本日志宏
-define(DEBUG(Format), io:format("[DEBUG] " ++ Format ++ "~n")).
-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-define(INFO(Format), io:format("[INFO] " ++ Format ++ "~n")).
-define(INFO(Format, Args), io:format("[INFO] " ++ Format ++ "~n", Args)).
-define(WARNING(Format), io:format("[WARNING] " ++ Format ++ "~n")).
-define(WARNING(Format, Args), io:format("[WARNING] " ++ Format ++ "~n", Args)).
-define(ERROR(Format), io:format("[ERROR] " ++ Format ++ "~n")).
-define(ERROR(Format, Args), io:format("[ERROR] " ++ Format ++ "~n", Args)).

%% 中文安全打印宏
-define(PRINT_ZH(Text), io:format("~ts~n", [unicode:characters_to_binary(Text)])).
-define(PRINT_ZH(Format, Args), io:format("~ts~n", [unicode:characters_to_binary(io_lib:format(Format, Args))])).

-endif. % APP_LOGGER_HRL
```

### 步骤2：在模块中引入
```erlang
%% 在Erlang模块中引入日志头文件
-module(your_module).
-include("app_logger.hrl").

your_function() ->
    ?INFO("函数开始执行", []),
    ?PRINT_ZH("中文日志测试"),
    ?DEBUG("详细数据: ~p", [SomeData]).
```

### 步骤3：配置编译选项
```erlang
%% 在rebar.config或Makefile中
{erl_opts, [
    {d, 'DEBUG'},  % 启用DEBUG日志
    {i, "include"} % 包含头文件路径
]}.
```

## 测试验证

### 测试1：基本日志功能
```erlang
test_basic_logging() ->
    io:format("=== 测试基本日志功能 ===~n"),
    
    ?DEBUG("这是一条DEBUG日志", []),
    ?INFO("这是一条INFO日志", []),
    ?WARNING("这是一条WARNING日志", []),
    ?ERROR("这是一条ERROR日志", []),
    
    io:format("=== 测试完成 ===~n").
```

### 测试2：中文打印测试
```erlang
test_chinese_logging() ->
    io:format("=== 测试中文打印 ===~n"),
    
    %% 测试各种中文打印方法
    ?PRINT_ZH("中文测试1：直接打印"),
    
    io:format("~ts~n", [<<"中文测试2：使用UTF8二进制"/utf8>>]),
    
    ChineseText = unicode:characters_to_binary("中文测试3：使用unicode转换"),
    io:format("~ts~n", [ChineseText]),
    
    io:format("=== 中文打印测试完成 ===~n").
```

### 测试3：性能测试
```erlang
test_performance() ->
    io:format("=== 测试日志性能 ===~n"),
    
    StartTime = erlang:system_time(microsecond),
    
    %% 测试1000次日志打印
    lists:foreach(fun(I) ->
        ?DEBUG("测试日志 ~p", [I])
    end, lists:seq(1, 1000)),
    
    EndTime = erlang:system_time(microsecond),
    Duration = (EndTime - StartTime) / 1000000,
    
    io:format("1000次日志打印耗时: ~.3f 秒 (~.0f 次/秒)~n", 
              [Duration, 1000 / Duration]),
    
    io:format("=== 性能测试完成 ===~n").
```

## 扩展功能

### 1. 日志文件轮转
```erlang
%% 简单的日志文件轮转
rotate_log_file(File, MaxSize) ->
    case file:read_file_info(File) of
        {ok, #file_info{size = Size}} when Size > MaxSize ->
            Timestamp = integer_to_list(erlang:system_time(second)),
            NewFile = File ++ "." ++ Timestamp ++ ".bak",
            file:rename(File, NewFile),
            file:write_file(File, <<>>);
        _ ->
            ok
    end.
```

### 2. 日志级别过滤
```erlang
%% 根据配置过滤日志级别
should_log(Level, Config) ->
    MinLevel = proplists:get_value(min_log_level, Config, info),
    LevelPriority = level_priority(Level),
    MinPriority = level_priority(MinLevel),
    LevelPriority >= MinPriority.

level_priority(debug) -> 4;
level_priority(info) -> 3;
level_priority(warning) -> 2;
level_priority(error) -> 1;
level_priority(_) -> 0.
```

### 3. 结构化日志输出
```erlang
%% JSON格式的结构化日志
log_json(Level, Event, Data) ->
    Json = jsx:encode(#{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"level">> => atom_to_binary(Level, utf8),
        <<"event">> => Event,
        <<"data">> => Data,
        <<"module">> => ?MODULE,
        <<"line">> => ?LINE
    }),
    io:format("~s~n", [Json]).
```

## 优先级建议

基于对DGIOT项目的分析，以下是日志解决方案的实施优先级建议：

### 高优先级（立即处理）
1. **修复编译错误**：解决uav_handler.erl中的二进制匹配问题
   ```erlang
   %% 问题：值无法匹配二进制段大小
   %% 解决方案：调整二进制段大小或使用正确的值
   <<1467833315:32>>  %% 而不是 <<1467833315:24>>
   ```

2. **统一日志头文件引入**：确保所有模块使用`-include_lib("dgiot/include/logger.hrl")`
3. **修复未使用函数警告**：添加函数到导出列表或删除未使用函数

### 中优先级（本周内处理）
1. **标准化日志格式**：统一使用`?LOG(Level, Format, Args)`格式
2. **中文打印规范化**：确保所有中文日志使用`~ts`格式和`<<"中文"/utf8>>`语法
3. **日志级别一致性**：统一使用debug/info/warning/error级别

### 低优先级（本月内处理）
1. **性能优化**：添加条件编译控制DEBUG日志
2. **结构化日志**：实现JSON格式的结构化日志输出
3. **日志文件管理**：添加日志轮转和归档功能

## 实施路线图

### 阶段1：基础修复（1-2天）
1. 修复所有编译错误和警告
2. 确保所有模块正确引入日志头文件
3. 统一基本日志格式

### 阶段2：标准化（3-5天）
1. 审核并统一所有模块的日志级别使用
2. 修复中文打印问题
3. 添加日志格式检查工具

### 阶段3：优化（1-2周）
1. 实现条件编译日志
2. 添加性能监控
3. 集成结构化日志

## 总结

本技能提供了Erlang项目中完整的日志打印解决方案，包括：

1. **统一的日志宏定义**：基于DGIOT项目的最佳实践
2. **中文打印支持**：解决中文乱码问题的多种方案
3. **性能优化**：条件编译和惰性求值等优化技术
4. **最佳实践**：日志级别规范、格式统一等
5. **集成指南**：如何将方案集成到现有项目
6. **优先级建议**：分阶段实施路线图

通过使用本技能，可以确保Erlang项目中的日志打印：
- 中文显示正确，无乱码问题
- 格式统一，便于阅读和分析
- 性能优化，不影响系统运行
- 易于维护和扩展

---
*最后更新：2026年1月22日*
*版本：1.1.0*