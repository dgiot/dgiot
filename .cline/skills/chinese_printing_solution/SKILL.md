---
name: chinese_printing_solution
description: 中文打印解决方案，确保所有中文输出都使用正确的Unicode编码，避免乱码问题
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-21
category: development
tags: [chinese, unicode, printing, formatting, erlang]
trigger_phrases:
  - "中文打印问题"
  - "io:format中文乱码"
  - "Unicode编码中文"
  - "中文输出格式化"
  - "Erlang中文打印"
  - "解决中文乱码"
  - "中文字符串处理"
---

# 中文打印解决方案

## 问题描述

在Erlang项目中，直接使用`io:format("中文文本")`可能导致中文乱码问题，特别是在不同终端和环境中。需要统一的解决方案来确保中文文本正确显示。

## 核心原则

### 1. 使用Unicode编码
所有中文文本都应该使用Unicode编码，避免直接使用字符串字面量。

### 2. 正确的格式化选项
使用`~ts`格式化选项来处理Unicode字符串，而不是`~s`。

### 3. 二进制字符串转换
使用`unicode:characters_to_binary/1`或`<<>>/utf8`语法确保正确的编码。

## 解决方案

### 方案1: 使用二进制字符串（推荐）
```erlang
% 不推荐 - 可能导致乱码
io:format("=== 测试遥测帧摘要 ===~n"),

% 推荐 - 使用二进制字符串
io:format("=== ~ts ===~n", [<<"测试遥测帧摘要"/utf8>>]),
```

### 方案2: 使用unicode模块转换
```erlang
% 将普通字符串转换为Unicode二进制
ChineseText = unicode:characters_to_binary("测试遥测帧摘要"),
io:format("=== ~ts ===~n", [ChineseText]),
```

### 方案3: 封装为辅助函数
```erlang
%% @doc 安全打印中文文本
-spec print_chinese(string() | binary()) -> ok.
print_chinese(Text) when is_list(Text) ->
    io:format("~ts~n", [unicode:characters_to_binary(Text)]);
print_chinese(Text) when is_binary(Text) ->
    io:format("~ts~n", [Text]).

%% @doc 带格式的中文打印
-spec format_chinese(string(), [term()]) -> ok.
format_chinese(Format, Args) when is_list(Format) ->
    BinaryFormat = unicode:characters_to_binary(Format),
    io:format(BinaryFormat, Args).
```

## 使用示例

### 示例1: 基本中文打印
```erlang
% 旧方式（可能乱码）
io:format("开始解析无人机协议~n"),
io:format("解析完成，共发现 ~p 个帧~n", [FrameCount]),

% 新方式（推荐）
io:format("~ts~n", [<<"开始解析无人机协议"/utf8>>]),
io:format("~ts ~p ~ts~n", [<<"解析完成，共发现"/utf8>>, FrameCount, <<"个帧"/utf8>>]),
```

### 示例2: 复杂格式输出
```erlang
% 旧方式
io:format("=== 无人机协议测试报告 ===~n"),
io:format("测试时间: ~s~n", [Timestamp]),
io:format("测试结果: ~s~n", [Result]),
io:format("错误信息: ~s~n", [ErrorMessage]),

% 新方式
io:format("=== ~ts ===~n", [<<"无人机协议测试报告"/utf8>>]),
io:format("~ts: ~ts~n", [<<"测试时间"/utf8>>, Timestamp]),
io:format("~ts: ~ts~n", [<<"测试结果"/utf8>>, Result]),
io:format("~ts: ~ts~n", [<<"错误信息"/utf8>>, ErrorMessage]),
```

### 示例3: 表格输出
```erlang
% 旧方式
io:format("+----------------+--------+~n"),
io:format("| 字段名称       | 值     |~n"),
io:format("+----------------+--------+~n"),
io:format("| 目标地址       | ~p     |~n", [DestAddr]),
io:format("| 源地址         | ~p     |~n", [SrcAddr]),
io:format("| 数据类型       | 0x~2.16.0B |~n", [DataType]),

% 新方式
io:format("+----------------+--------+~n"),
io:format("| ~ts       | ~ts     |~n", [<<"字段名称"/utf8>>, <<"值"/utf8>>]),
io:format("+----------------+--------+~n"),
io:format("| ~ts       | ~p     |~n", [<<"目标地址"/utf8>>, DestAddr]),
io:format("| ~ts         | ~p     |~n", [<<"源地址"/utf8>>, SrcAddr]),
io:format("| ~ts       | 0x~2.16.0B |~n", [<<"数据类型"/utf8>>, DataType]),
```

## 工具函数库

### 完整的中文打印模块
```erlang
%%%-------------------------------------------------------------------
%%% @doc 中文打印工具模块
%%% 提供安全的中文文本打印功能，避免乱码问题
%%%-------------------------------------------------------------------
-module(chinese_printer).

-export([
    print/1,           % 打印中文文本
    print/2,           % 带格式的中文打印
    format/2,          % 格式化中文输出
    format/3,          % 带参数列表的格式化
    banner/1,          % 打印横幅
    table_header/1,    % 打印表格头部
    table_row/2,       % 打印表格行
    separator/0,       % 打印分隔线
    success/1,         % 成功消息
    error/1,           % 错误消息
    warning/1,         % 警告消息
    info/1             % 信息消息
]).

%% @doc 打印中文文本
print(Text) when is_list(Text) ->
    io:format("~ts~n", [unicode:characters_to_binary(Text)]);
print(Text) when is_binary(Text) ->
    io:format("~ts~n", [Text]).

%% @doc 带格式的中文打印
print(Format, Args) when is_list(Format) ->
    BinaryFormat = unicode:characters_to_binary(Format),
    io:format(BinaryFormat, Args).

%% @doc 格式化中文输出
format(Format, Args) ->
    print(Format, Args).

%% @doc 带参数列表的格式化
format(Format, Args, Options) ->
    % 可以添加额外的格式化选项
    print(Format, Args).

%% @doc 打印横幅
banner(Title) when is_list(Title) ->
    BinaryTitle = unicode:characters_to_binary(Title),
    io:format("~n"),
    io:format("========================================~n"),
    io:format("  ~ts~n", [BinaryTitle]),
    io:format("========================================~n"),
    io:format("~n").

%% @doc 打印表格头部
table_header(Headers) when is_list(Headers) ->
    % 转换所有表头为二进制
    BinaryHeaders = [unicode:characters_to_binary(H) || H <- Headers],
    
    % 计算列宽
    Widths = [byte_size(H) + 2 || H <- BinaryHeaders],
    
    % 打印分隔线
    Separator = lists:map(fun(W) -> lists:duplicate(W, $-) end, Widths),
    io:format("+~s+~n", [string:join(Separator, "+")]),
    
    % 打印表头
    io:format("|~s|~n", [
        string:join([io_lib:format(" ~ts ", [H]) || H <- BinaryHeaders], "|")
    ]),
    
    % 打印分隔线
    io:format("+~s+~n", [string:join(Separator, "+")]).

%% @doc 打印表格行
table_row(Headers, Values) when is_list(Headers), is_list(Values) ->
    % 转换所有表头为二进制
    BinaryHeaders = [unicode:characters_to_binary(H) || H <- Headers],
    
    % 计算列宽
    Widths = [byte_size(H) + 2 || H <- BinaryHeaders],
    
    % 格式化值
    FormattedValues = lists:map(fun
        (V) when is_list(V) -> unicode:characters_to_binary(V);
        (V) when is_binary(V) -> V;
        (V) -> list_to_binary(io_lib:format("~p", [V]))
    end, Values),
    
    % 打印行
    io:format("|~s|~n", [
        string:join([
            io_lib:format(" ~*ts ", [W - 2, V]) 
            || {W, V} <- lists:zip(Widths, FormattedValues)
        ], "|")
    ]).

%% @doc 打印分隔线
separator() ->
    io:format("----------------------------------------~n").

%% @doc 成功消息
success(Message) ->
    io:format("[~ts] ~ts~n", [<<"成功"/utf8>>, unicode:characters_to_binary(Message)]).

%% @doc 错误消息
error(Message) ->
    io:format("[~ts] ~ts~n", [<<"错误"/utf8>>, unicode:characters_to_binary(Message)]).

%% @doc 警告消息
warning(Message) ->
    io:format("[~ts] ~ts~n", [<<"警告"/utf8>>, unicode:characters_to_binary(Message)]).

%% @doc 信息消息
info(Message) ->
    io:format("[~ts] ~ts~n", [<<"信息"/utf8>>, unicode:characters_to_binary(Message)]).
```

## 迁移指南

### 步骤1: 识别需要修改的代码
```bash
# 查找所有包含中文的io:format调用
grep -r "io:format.*[\x{4e00}-\x{9fff}]" --include="*.erl" .
```

### 步骤2: 创建替换规则
1. `io:format("中文文本")` → `io:format("~ts", [<<"中文文本"/utf8>>])`
2. `io:format("文本 ~p", [Value])` → `io:format("~ts ~p", [<<"文本"/utf8>>, Value])`
3. `io:format("~s", [ChineseString])` → `io:format("~ts", [unicode:characters_to_binary(ChineseString)])`

### 步骤3: 批量替换脚本
```python
#!/usr/bin/env python3
import re
import os

def fix_chinese_printing(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 匹配中文文本
    pattern = r'io:format\("([^"]*[\u4e00-\u9fff]+[^"]*)"\)'
    
    def replace_match(match):
        chinese_text = match.group(1)
        # 转义特殊字符
        escaped = chinese_text.replace('~', '~~')
        return f'io:format("~ts", [<<"{escaped}"/utf8>>])'
    
    new_content = re.sub(pattern, replace_match, content)
    
    if new_content != content:
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True
    return False

# 遍历所有Erlang文件
for root, dirs, files in os.walk('.'):
    for file in files:
        if file.endswith('.erl'):
            file_path = os.path.join(root, file)
            if fix_chinese_printing(file_path):
                print(f"Fixed: {file_path}")
```

## 测试验证

### 测试用例
```erlang
-module(test_chinese_printing).

-export([test_all/0]).

test_all() ->
    io:format("~n=== 测试中文打印 ===~n"),
    
    % 测试基本打印
    io:format("~ts~n", [<<"基本中文测试"/utf8>>]),
    
    % 测试带参数的打印
    Value = 123,
    io:format("~ts: ~p~n", [<<"测试值"/utf8>>, Value]),
    
    % 测试表格打印
    io:format("+--------+--------+~n"),
    io:format("| ~ts | ~ts |~n", [<<"名称"/utf8>>, <<"数值"/utf8>>]),
    io:format("+--------+--------+~n"),
    io:format("| ~ts | ~p |~n", [<<"测试1"/utf8>>, 100]),
    io:format("| ~ts | ~p |~n", [<<"测试2"/utf8>>, 200]),
    io:format("+--------+--------+~n"),
    
    io:format("~ts~n", [<<"测试完成"/utf8>>]),
    ok.
```

## 最佳实践

### 1. 统一编码标准
- 所有源文件使用UTF-8编码
- 在文件头部添加编码声明
- 使用统一的换行符风格

### 2. 代码审查要点
- 检查所有`io:format`调用是否正确处理中文
- 验证二进制字符串使用`/utf8`后缀
- 确保格式化选项使用`~ts`而不是`~s`

### 3. 性能考虑
- 避免在循环中重复转换字符串
- 对于频繁使用的字符串，预转换为二进制
- 使用二进制连接而不是列表连接

### 4. 兼容性
- 确保与Erlang/OTP 20+版本兼容
- 测试在不同终端下的显示效果
- 考虑跨平台兼容性（Linux/Windows/macOS）

## 常见问题解答

### Q1: 为什么使用`~ts`而不是`~s`？
A: `~s`格式化选项假设字符串是Latin-1编码，而`~ts`支持Unicode编码，能正确处理中文字符。

### Q2: `/utf8`后缀的作用是什么？
A: `/utf8`后缀告诉Erlang编译器将字符串字面量编译为UTF-8编码的二进制，确保正确的编码处理。

### Q3: 如何批量修改现有代码？
A: 使用提供的Python脚本或手动替换，建议先备份代码，然后分模块逐步修改。

### Q4: 是否所有中文都需要转换？
A: 是的，所有在`io:format`中使用的中文字符串都应该转换为二进制格式，以确保一致性。

### Q5: 其他输出函数如何处理？
A: 同样的原则适用于`io:put_chars/1`、`io:write/1`等输出函数，都需要使用Unicode编码。

## 总结

通过实施本解决方案，可以确保：
1. ✅ 中文文本在各种环境下正确显示
2. ✅ 代码具有更好的可维护性和一致性
3. ✅ 避免因编码问题导致的调试困难
4. ✅ 提高代码的跨平台兼容性

建议将本技能集成到项目的代码审查流程中，确保所有新代码都遵循中文打印的最佳实践。
