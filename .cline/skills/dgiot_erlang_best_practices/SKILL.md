---
name: dgiot_erlang_best_practices
description: DGIOT Erlang最佳实践技能，确保使用Erlang编程思想和风格，正确处理头文件、热编译、在线调测，解决中文乱码问题，充分利用DGIOT系统架构和插件业务场景
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: development
tags: [dgiot, erlang, best_practices, hot_compile, online_debug, chinese_encoding, architecture, plugin_scenario]
trigger_phrases:
  - Erlang编程思想
  - Erlang编程风格
  - 头文件问题
  - 热编译
  - 在线调测
  - 中文乱码
  - DGIOT架构
  - 插件业务场景
  - Erlang最佳实践
  - 集成测试方法
---

# DGIOT Erlang最佳实践技能

## 概述

本技能确保在DGIOT开发中正确使用Erlang编程思想和风格，解决常见问题：头文件包含、热编译使用、在线调测方法、中文乱码问题，并充分利用DGIOT系统架构和插件业务场景知识。

## 核心问题解决

### 1. Erlang编程思想和风格问题

#### 常见错误
- ❌ 使用面向过程编程，而不是函数式编程
- ❌ 没有充分利用模式匹配和递归
- ❌ 没有使用OTP行为模式
- ❌ 错误处理不规范

#### 正确实践
```erlang
%% ✅ 正确：函数式编程，模式匹配
handle_message({register, DeviceId, Data}, State) ->
    %% 使用模式匹配解构数据
    #{<<"product_id">> := ProductId, <<"dtu_addr">> := DtuAddr} = Data,
    
    %% 使用递归处理列表
    process_device_data(DeviceId, ProductId, DtuAddr, State);

%% ✅ 正确：使用OTP行为模式
-behaviour(gen_server).

%% ✅ 正确：规范的错误处理
handle_call(Request, From, State) ->
    try
        Result = do_operation(Request),
        {reply, {ok, Result}, State}
    catch
        error:Reason ->
            ?LOG(error, "操作失败: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end.
```

### 2. 头文件包含问题

#### 常见错误
- ❌ 使用`include("file.hrl")`而不是`include_lib`
- ❌ 头文件路径错误
- ❌ 循环包含

#### 正确实践
```erlang
%% ✅ 正确：使用include_lib包含平台头文件
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot.hrl").

%% ✅ 正确：使用相对路径包含插件头文件
-include("dgiot_uav.hrl").

%% ✅ 正确：避免循环包含
%% 头文件只包含类型定义和宏，不包含其他头文件
```

### 3. 热编译和在线调测问题

#### 常见错误
- ❌ 不使用热编译，每次都`make run`
- ❌ 不添加test函数进行在线调试
- ❌ 不使用Erlang Shell实时测试

#### 正确实践
```erlang
%% ✅ 正确：在模块中添加test函数
-module(dgiot_example).

-export([test/0, test_parsing/1]).

%% @doc 在线测试入口
test() ->
    io:format("=== 在线测试开始 ===~n"),
    
    TestResults = [
        test_parsing(<<"test_data">>),
        test_connection(),
        test_business_logic()
    ],
    
    %% 汇总结果
    {ok, TestResults}.

%% ✅ 正确：使用热编译命令
%% 开发时使用：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_example).'
%% 测试时使用：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_example:test().'
```

### 4. 中文乱码问题

#### 常见错误
- ❌ 直接使用中文字符串：`io:format("中文测试")`
- ❌ 不使用UTF-8编码
- ❌ 不使用安全打印函数

#### 正确实践
```erlang
%% ✅ 正确：使用二进制格式配合/utf8标志
io:format("~p ~n", [<<"中文测试"/utf8>>]).

%% ✅ 正确：使用平台提供的安全打印函数
dgiot_utils:safe_format("设备 ~s 状态更新成功", [DeviceId]).

%% ✅ 正确：在日志中使用中文
?LOG(info, "~p", [<<"设备注册成功"/utf8>>]).
```

### 5. DGIOT架构利用问题

#### 常见错误
- ❌ 不遵循七层架构
- ❌ 直接操作数据库，不通过标准API
- ❌ 不利用Hook系统
- ❌ 不重用现有模块

#### 正确实践
```erlang
%% ✅ 正确：遵循七层架构
%% 通讯层：只转发原始数据
%% 协议层：只解析协议
%% 业务层：处理业务逻辑
%% 数据层：通过标准API保存数据

%% ✅ 正确：使用Hook系统
dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                    [ProductId, DevAddr, RawData]).

%% ✅ 正确：重用现有模块
%% 写代码前先查找现有实现
case dgiot_utils:binary_to_hex(Binary) of
    {ok, Hex} -> Hex;
    {error, _} -> <<>>
end.
```

### 6. 插件业务场景知识

#### 常见错误
- ❌ 不了解插件生命周期
- ❌ 不遵循插件开发规范
- ❌ 不利用插件配置系统

#### 正确实践
```erlang
%% ✅ 正确：遵循插件生命周期
-module(dgiot_my_plugin).

%% 插件启动
start() ->
    dgiot_plugin:load(?MODULE).

%% 插件停止
stop() ->
    dgiot_plugin:unload(?MODULE).

%% ✅ 正确：使用插件配置
get_config(Key, Default) ->
    dgiot_plugin:get_config(?MODULE, Key, Default).
```

## 最佳实践检查清单

### 1. 编码前检查
- [ ] 是否查找了现有实现？（使用`dgiot_code_reuse_solution`技能）
- [ ] 是否理解了业务场景？（使用`dgiot_architecture_learning`技能）
- [ ] 是否设计了正确的架构？（遵循七层架构）

### 2. 编码时检查
- [ ] 是否使用了正确的头文件包含？（使用`erlang_include_system`技能）
- [ ] 是否遵循了Erlang编程风格？（函数式、模式匹配）
- [ ] 是否处理了中文编码？（使用`erlang_chinese_utf8`技能）

### 3. 编译时检查
- [ ] 是否使用了热编译？（使用`dgiot_compile_debug`技能）
- [ ] 是否修复了所有编译警告？（使用`erlang_compile_warnings_fix`技能）
- [ ] 是否添加了test函数？（使用`dgiot_online_debug`技能）

### 4. 测试时检查
- [ ] 是否使用了在线调测？（在Erlang Shell中执行测试）
- [ ] 是否验证了中文输出？
- [ ] 是否测试了所有业务场景？

### 5. 部署时检查
- [ ] 是否遵循了插件开发规范？
- [ ] 是否利用了Hook系统？
- [ ] 是否配置了正确的参数？

## 集成测试工作流

### 1. Erlang独有的调测方法
```
编写代码 → 添加test函数 → 热编译 → Erlang Shell测试 → 分析结果 → 修改优化
    ↓          ↓          ↓          ↓          ↓          ↓
遵循Erlang风格  包含头文件  使用热编译命令  在线执行测试  查看日志  持续迭代
```

### 2. 完整调测示例
```bash
# 1. 编写代码时添加test函数
cat > apps/dgiot_example/src/dgiot_example.erl << 'EOF'
-module(dgiot_example).
-export([test/0, parse_data/1]).

test() ->
    io:format("测试开始~n"),
    TestData = <<"test">>,
    case parse_data(TestData) of
        {ok, Result} -> 
            io:format("测试通过: ~p~n", [Result]),
            {ok, test_passed};
        {error, Reason} ->
            io:format("测试失败: ~p~n", [Reason]),
            {error, test_failed}
    end.

parse_data(Data) when is_binary(Data) ->
    {ok, #{data => Data}}.
EOF

# 2. 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_example).'

# 3. 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_example:test().'

# 4. 查看结果
# 预期输出：测试开始\n测试通过: #{data => <<"test">>}\n{ok,test_passed}
```

## 常见问题解决方案

### 问题1: 头文件找不到
**症状**: `include file "dgiot.hrl" not found`
**解决方案**:
```erlang
%% 错误：include("dgiot.hrl")
%% 正确：include_lib("dgiot/include/dgiot.hrl")
```

### 问题2: 中文显示乱码
**症状**: 控制台显示``或乱码
**解决方案**:
```erlang
%% 错误：io:format("中文")
%% 正确：io:format("~p ~n", [<<"中文"/utf8>>])
%% 更好：dgiot_utils:safe_format("中文")
```

### 问题3: 热编译失败
**症状**: `undefined function` 或编译错误
**解决方案**:
```bash
# 1. 检查语法
erlc -I include/ -o /dev/null src/module.erl

# 2. 检查依赖
_build/emqx/rel/emqx/bin/emqx eval 'code:which(dgiot_plugin).'

# 3. 清理后重新编译
make clean
make compile
```

### 问题4: 测试函数不执行
**症状**: `undefined function dgiot_example:test/0`
**解决方案**:
```erlang
%% 1. 确保函数已导出
-export([test/0]).

%% 2. 重新热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_example).'

%% 3. 热加载插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_example).'
```

## DGIOT架构最佳实践

### 1. 七层架构应用
```erlang
%% 通讯层：只负责连接和数据转发
handle_tcp_data(Socket, Data) ->
    %% 不解析数据，只转发
    Things = #{<<"raw_data">> => Data},
    send_to_task_channel(Things).

%% 协议层：只负责协议解析
parse_protocol(RawData) ->
    %% 解析协议，返回结构化数据
    {ok, ParsedData}.

%% 业务层：处理业务逻辑
handle_business_logic(ParsedData) ->
    %% 计算属性，处理告警
    {ok, Result}.

%% 数据层：通过标准API保存
save_data(Result) ->
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Result).
```

### 2. Hook系统利用
```erlang
%% 注册Hook
init_hooks() ->
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"MYPROTOCOL">>}, 
                   fun my_protocol:get_datasource/1),
    ok.

%% 调用Hook
parse_data(RawData) ->
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MYPROTOCOL">>}, 
                            [ProductId, DevAddr, RawData]) of
        {ok, [ParsedData | _]} -> ParsedData;
        _ -> #{<<"raw_data">> => RawData}
    end.
```

### 3. 插件开发规范
```erlang
%% 插件模块结构
-module(dgiot_my_plugin).

%% 必须导出的函数
-export([start/0, stop/0, load/1, unload/1]).

%% 插件配置
-define(CONFIG_KEYS, [<<"key1">>, <<"key2">>]).

%% 插件启动
start() ->
    ?LOG(info, "插件 ~s 启动", [?MODULE]),
    ok.

%% 插件业务函数
-export([my_function/1]).

my_function(Args) ->
    %% 业务逻辑
    {ok, Result}.
```

## 技能集成

### 1. 与自主开发技能集成
```
dgiot_autonomous_development 激活
    ↓
[dgiot_erlang_best_practices] 确保Erlang最佳实践
    ↓
[erlang_include_system] 处理头文件
    ↓
[dgiot_compile_debug] 热编译
    ↓
[dgiot_online_debug] 在线调测
    ↓
[erlang_chinese_utf8] 解决中文乱码
    ↓
[dgiot_architecture_learning] 利用DGIOT架构
    ↓
输出符合最佳实践的代码
```

### 2. Hook集成
```bash
#!/bin/bash
# .clinerules/hooks/PreToolUse - Erlang最佳实践检查

check_erlang_best_practices() {
    local tool_name="$1"
    local tool_args="$2"
    
    # 检查Erlang编码
    if [[ "$tool_name" == "write_to_file" ]] && [[ "$tool_args" == *".erl"* ]]; then
        echo "[Hook] 检测到Erlang编码，检查最佳实践..." >&2
        
        # 检查头文件包含
        if [[ "$tool_args" == *"include(\""* ]] && [[ "$tool_args" != *"include_lib"* ]]; then
            CONTEXT_MOD="$CONTEXT_MOD 警告：检测到include(\"file.hrl\")，建议使用include_lib(\"dgiot/include/file.hrl\")。"
        fi
        
        # 检查中文打印
        if [[ "$tool_args" == *"io:format(\"[\x80-\xFF]"* ]]; then
            CONTEXT_MOD="$CONTEXT_MOD 警告：检测到直接中文字符串，建议使用io:format(\"~p ~n\", [<<\"中文\"/utf8>>])或dgiot_utils:safe_format。"
        fi
        
        # 检查test函数
        if [[ "$tool_args" == *"-export("* ]] && [[ "$tool_args" != *"test/0"* ]]; then
            CONTEXT_MOD="$CONTEXT_MOD 建议：添加test/0函数用于在线调试。"
        fi
    fi
}
```

## 培训材料

### 1. Erlang编程思想培训
```erlang
%% 培训示例：从面向过程到函数式
%% 错误：面向过程
process_data(Data) ->
    Result = [],
    for i in 1 to length(Data) do
        Element = get_element(Data, i),
        if Element > 0 then
            Result = Result ++ [Element]
        end
    end,
    Result.

%% 正确：函数式编程
process_data(Data) ->
    lists:filter(fun(Element) -> Element > 0 end, Data).

%% 正确：使用列表推导
process_data(Data) ->
    [Element || Element <- Data, Element > 0].
```

### 2. DGIOT架构培训
```
七层架构理解：
1. 通讯层：TCP/UDP连接，原始数据转发
2. 协议层：协议解析，不处理业务
3. 消息路由层：MQTT消息路由
4. 业务层：数据解码，属性计算
5. 数据层：时序数据存储
6. 缓存层：实时数据缓存
7. API层：数据查询，控制指令

原则：分层解耦，各安其位，各司其职
```

## 总结

通过本技能，确保在DGIOT开发中：

1. **正确使用Erlang编程思想和风格**
2. **正确处理头文件包含问题**
3. **充分利用热编译和在线调测**
4. **彻底解决中文乱码问题**
5. **充分利用DGIOT系统架构**
6. **深入理解插件业务场景**

这些最佳实践将显著提高开发效率、代码质量和系统稳定性。