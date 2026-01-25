---
name: erlang_chinese_utf8
description: Erlang代码中文字符串UTF8后缀检查与修复，确保所有中文字符串使用正确的/utf8后缀，避免编码问题
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-22
category: development
tags: [erlang, chinese, unicode, utf8, encoding, code-quality, hook]
trigger_phrases:
  - "中文怎么又不加/utf8"
  - "中文字符串utf8后缀"
  - "Erlang中文编码"
  - "检查中文字符串"
  - "修复中文编码"
  - "自动添加utf8后缀"
  - "二进制字符串编码"
---

# Erlang中文字符串UTF8后缀检查与修复

## 问题描述

在Erlang项目中，中文字符串应该使用`<<"中文"/utf8>>`格式而不是`<<"中文">>`，以确保正确的Unicode编码。缺少`/utf8`后缀可能导致编码问题，特别是在跨平台和不同终端环境下。

## 核心功能

### 1. 中文字符串检测
- 自动识别Erlang代码中的中文字符串
- 检查是否缺少`/utf8`后缀
- 支持多种中文字符串格式

### 2. 自动修复
- 自动添加缺失的`/utf8`后缀
- 保持代码格式和缩进
- 支持批量修复

### 3. Hook集成
- 集成到Cline的hook系统
- 在PreToolUse阶段自动检查
- 提供修复建议

### 4. 代码质量检查
- 验证现有代码的编码一致性
- 生成修复报告
- 提供最佳实践建议

## 使用场景

### 场景1: 代码审查时自动检查
当用户编辑Erlang文件时，自动检查中文字符串并提示添加`/utf8`后缀。

### 场景2: 批量修复现有代码
扫描整个项目，自动修复所有缺少`/utf8`后缀的中文字符串。

### 场景3: 持续集成检查
在CI/CD流程中集成检查，确保新代码符合编码标准。

## 检测规则

### 规则1: 二进制字符串中的中文
```erlang
% 需要修复
<<"左前翼舵面">>
<<"右前翼舵面">>
<<"左侧翼舵面">>
<<"右侧翼舵面">>

% 正确格式
<<"左前翼舵面"/utf8>>
<<"右前翼舵面"/utf8>>
<<"左侧翼舵面"/utf8>>
<<"右侧翼舵面"/utf8>>
```

### 规则2: 日志消息中的中文
```erlang
% 需要修复
?LOG(info, "~ts: ~p", [<<"舵面报文处理">>, Packet])

% 正确格式
?LOG(info, "~ts: ~p", [<<"舵面报文处理"/utf8>>, Packet])
```

### 规则3: 映射中的中文值
```erlang
% 需要修复
#{device_type => <<"未知舵面">>, protocol => <<"unknown">>}

% 正确格式
#{device_type => <<"未知舵面"/utf8>>, protocol => <<"unknown">>}
```

## 实现方案

### 1. 正则表达式检测
```python
# 检测缺少/utf8后缀的中文字符串
pattern = r'<<("([^"]*[\u4e00-\u9fff]+[^"]*)"|([^>]*[\u4e00-\u9fff]+[^>]*))>>(?!/utf8)'
```

### 2. 修复算法
```python
def fix_chinese_utf8(content):
    """修复Erlang代码中的中文字符串，添加/utf8后缀"""
    
    # 匹配所有缺少/utf8后缀的中文字符串
    pattern = r'(<<("[^"]*[\u4e00-\u9fff]+[^"]*"|[^>]*[\u4e00-\u9fff]+[^>]*)>>)(?!/utf8)'
    
    def replace_match(match):
        chinese_string = match.group(1)
        # 确保字符串以>>结束，然后添加/utf8
        if chinese_string.endswith('>>'):
            return chinese_string[:-2] + '/utf8>>'
        return chinese_string + '/utf8>>'
    
    return re.sub(pattern, replace_match, content)
```

### 3. Hook集成代码
```python
def pre_tool_use_hook(context):
    """PreToolUse阶段的hook函数"""
    
    # 检查当前操作是否涉及Erlang文件
    if is_erlang_file_operation(context):
        # 读取文件内容
        content = read_file_content(context.file_path)
        
        # 检查中文字符串
        issues = check_chinese_utf8_issues(content)
        
        if issues:
            # 提供修复建议
            return {
                "action": "suggest_fix",
                "issues": issues,
                "fix_function": "fix_chinese_utf8",
                "message": f"发现{len(issues)}个缺少/utf8后缀的中文字符串"
            }
    
    return {"action": "continue"}
```

## 工具函数

### 1. 检查函数
```erlang
%% @doc 检查文件中的中文字符串UTF8问题
-spec check_file(string()) -> {ok, [issue()]} | {error, term()}.
check_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Issues = check_content(binary_to_list(Content)),
            {ok, Issues};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查内容中的问题
-spec check_content(string()) -> [issue()].
check_content(Content) ->
    % 实现检查逻辑
    [].
```

### 2. 修复函数
```erlang
%% @doc 修复文件中的中文字符串UTF8问题
-spec fix_file(string()) -> {ok, integer()} | {error, term()}.
fix_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            FixedContent = fix_content(Content),
            case file:write_file(Filename, FixedContent) of
                ok -> {ok, 1};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## Hook集成配置

### 1. 注册到hook系统
```yaml
# hook_config.yaml
hooks:
  pre_tool_use:
    - name: erlang_chinese_utf8_check
      priority: high  # 高级别hook
      function: erlang_chinese_utf8:pre_tool_use_check
      file_patterns: ["*.erl"]
      description: "检查Erlang文件中的中文字符串UTF8后缀"
```

### 2. Hook实现
```erlang
-module(erlang_chinese_utf8_hook).

-export([pre_tool_use_check/1]).

%% @doc PreToolUse阶段的检查
pre_tool_use_check(Context) ->
    case maps:get(file_type, Context, undefined) of
        "erl" ->
            check_erlang_file(Context);
        _ ->
            {continue, Context}
    end.

check_erlang_file(Context) ->
    FilePath = maps:get(file_path, Context),
    case erlang_chinese_utf8:check_file(FilePath) of
        {ok, []} ->
            {continue, Context};
        {ok, Issues} ->
            % 提供修复建议
            Suggestion = #{
                type => "chinese_utf8_fix",
                issues => Issues,
                fix_function => "erlang_chinese_utf8:fix_file",
                message => io_lib:format("发现~p个缺少/utf8后缀的中文字符串", [length(Issues)])
            },
            {suggest, Context#{suggestions => [Suggestion | maps:get(suggestions, Context, [])]}};
        {error, Reason} ->
            {continue, Context}
    end.
```

## 使用示例

### 示例1: 手动检查文件
```bash
# 检查单个文件
$ erl -noshell -eval 'erlang_chinese_utf8:check_file("apps/dgiot_uav/src/channel/rudder_handler.erl"), init:stop().'

# 批量检查目录
$ erl -noshell -eval 'erlang_chinese_utf8:check_directory("apps/"), init:stop().'
```

### 示例2: 自动修复
```bash
# 修复单个文件
$ erl -noshell -eval 'erlang_chinese_utf8:fix_file("apps/dgiot_uav/src/channel/rudder_handler.erl"), init:stop().'

# 批量修复
$ erl -noshell -eval 'erlang_chinese_utf8:fix_directory("apps/dgiot_uav/src/"), init:stop().'
```

### 示例3: 集成到Makefile
```makefile
# Makefile中添加检查目标
check-chinese-utf8:
	erl -noshell -eval 'erlang_chinese_utf8:check_directory("apps/"), init:stop().'

fix-chinese-utf8:
	erl -noshell -eval 'erlang_chinese_utf8:fix_directory("apps/"), init:stop().'
```

## 测试用例

### 测试文件示例
```erlang
-module(test_chinese_utf8).

-export([test/0]).

test() ->
    % 需要修复的案例
    Bad1 = <<"左前翼舵面">>,
    Bad2 = <<"右前翼舵面">>,
    Bad3 = <<"左侧翼舵面">>,
    Bad4 = <<"右侧翼舵面">>,
    
    % 正确的案例
    Good1 = <<"左前翼舵面"/utf8>>,
    Good2 = <<"右前翼舵面"/utf8>>,
    Good3 = <<"左侧翼舵面"/utf8>>,
    Good4 = <<"右侧翼舵面"/utf8>>,
    
    % 日志消息
    ?LOG(info, "~ts: ~p", [<<"舵面报文处理">>, Packet]),  % 需要修复
    ?LOG(info, "~ts: ~p", [<<"舵面报文处理"/utf8>>, Packet]),  % 正确
    
    ok.
```

### 预期修复结果
```erlang
-module(test_chinese_utf8).

-export([test/0]).

test() ->
    % 修复后的案例
    Good1 = <<"左前翼舵面"/utf8>>,
    Good2 = <<"右前翼舵面"/utf8>>,
    Good3 = <<"左侧翼舵面"/utf8>>,
    Good4 = <<"右侧翼舵面"/utf8>>,
    
    % 日志消息
    ?LOG(info, "~ts: ~p", [<<"舵面报文处理"/utf8>>, Packet]),  % 已修复
    
    ok.
```

## 最佳实践

### 1. 开发阶段
- 在编辑器中集成实时检查
- 提交代码前自动运行检查
- 代码审查时重点关注中文字符串

### 2. 维护阶段
- 定期扫描整个项目
- 更新历史代码
- 确保新代码符合标准

### 3. 团队协作
- 统一编码标准
- 提供培训文档
- 集成到CI/CD流程

## 故障排除

### 常见问题1: 误检测
**问题**: 非中文字符串被误检测为需要修复
**解决**: 调整正则表达式，确保准确识别中文字符

### 常见问题2: 格式破坏
**问题**: 修复后代码格式被破坏
**解决**: 使用更精确的替换算法，保持原有格式

### 常见问题3: 性能问题
**问题**: 大文件检查速度慢
**解决**: 优化检测算法，使用增量检查

## 与现有技能集成

### 与chinese_printing_solution集成
```erlang
% chinese_printing_solution处理io:format中的中文
% erlang_chinese_utf8处理二进制字符串中的中文
% 两者互补，确保所有中文处理都正确
```

### 与hook_manager集成
```erlang
% 通过hook_manager注册高级别hook
% 确保在代码编辑时实时检查
```

## 总结

通过实施本技能，可以确保：
1. ✅ 所有Erlang代码中的中文字符串使用正确的`/utf8`后缀
2. ✅ 避免因编码问题导致的运行时错误
3. ✅ 提高代码的可维护性和一致性
4. ✅ 支持团队协作和代码审查
5. ✅ 集成到开发工作流，实现自动化检查

建议将本技能作为Erlang项目开发的必备工具，集成到开发环境的各个阶段。
