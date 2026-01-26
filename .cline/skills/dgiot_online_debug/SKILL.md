
---
name: dgiot_online_debug
description: DGIOT在线调试技能，专注于在已经运行的dgiot OTP平台中通过添加test函数进行在线调试，无需复杂调用关系，直接在Erlang Shell中执行测试
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: development
tags: [dgiot, online_debug, test_functions, erlang_shell, hot_compile, realtime_testing, otp_platform]
trigger_phrases:
  - 在线调试
  - 在线调测
  - 添加test函数
  - Erlang Shell调试
  - 热编译测试
  - 实时调试
  - OTP平台调试
  - 无需调用关系
  - 直接执行测试
  - dgiot在线测试
---

# DGIOT在线调试技能

专注于在已经运行的dgiot OTP平台中通过添加test函数进行在线调试，无需复杂调用关系，直接在Erlang Shell中执行测试。

## 快速开始

当用户需要在已经运行的dgiot系统中进行实时调试、添加测试函数或验证代码功能时，激活本技能。

## 核心概念

### 1. 在线调试定义
**在线调试** = 在已经运行的dgiot OTP平台中，通过热编译添加test函数，直接在Erlang Shell中执行测试，无需重启系统或建立复杂调用关系。

### 2. 调试环境
```
运行中的dgiot系统 (make run启动)
    ↓
Erlang Shell (通过emqx eval访问)
    ↓
热编译代码 (dgiot_plugin:compile/1)
    ↓
在线执行测试 (Module:test().)
    ↓
实时查看结果 (日志/返回值)
```

### 3. 核心优势
- **实时性**: 无需重启系统，立即验证
- **简单性**: 无需复杂调用关系，直接测试
- **安全性**: 在隔离环境中测试，不影响生产
- **高效性**: 快速迭代，快速反馈

## 在线调试工作流

### 1. 标准工作流
```
1. 启动dgiot系统: make run
2. 添加test函数: 在模块中导出test/0函数
3. 热编译代码: _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(Module).'
4. 在线执行: _build/emqx/rel/emqx/bin/emqx eval 'Module:test().'
5. 分析结果: 查看日志和返回值
6. 修改优化: 根据结果修改代码
7. 重复测试: 继续下一轮调试
```

### 2. 详细步骤

#### 步骤1: 准备测试环境
```bash
# 1. 确保dgiot系统正在运行
make run

# 2. 验证系统状态
netstat -tlnp | grep :1883
```

#### 步骤2: 添加test函数
```erlang
%% 在模块中添加test函数示例
%% 文件: apps/dgiot_uav/src/communication/dgiot_uav_tcp_worker.erl
-module(dgiot_uav_tcp_worker).

%% 导出test函数
-export([test/0, test_connection/0, test_parsing/1]).

%% ... 其他代码 ...

%% @doc 在线测试入口函数
test() ->
    io:format("=== DGIOT UAV TCP Worker 在线测试 ===~n"),
    
    %% 执行各个测试用例
    TestResults = [
        test_connection(),
        test_parsing(<<"test_data">>),
        test_registration()
    ],
    
    %% 汇总结果
    Passed = lists:filter(fun({pass, _}) -> true; (_) -> false end, TestResults),
    Failed = lists:filter(fun({fail, _}) -> true; (_) -> false end, TestResults),
    
    io:format("测试完成: ~p通过, ~p失败~n", [length(Passed), length(Failed)]),
    
    case Failed of
        [] -> {ok, all_tests_passed};
        _ -> {error, {some_tests_failed, Failed}}
    end.

%% @doc 测试连接功能
test_connection() ->
    try
        %% 模拟连接测试
        io:format("测试连接...~n"),
        {pass, connection_ok}
    catch
        _:Reason ->
            {fail, {connection_error, Reason}}
    end.

%% @doc 测试数据解析
test_parsing(TestData) ->
    try
        io:format("测试数据解析: ~p~n", [TestData]),
        %% 调用实际的解析函数
        Result = parse_uav_packet(TestData),
        {pass, {parsing_result, Result}}
    catch
        _:Reason ->
            {fail, {parsing_error, Reason}}
    end.
```

#### 步骤3: 热编译代码
```bash
# 热编译特定模块
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav_tcp_worker).'

# 或热编译整个插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'

# 热加载插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_uav).'
```

#### 步骤4: 在线执行测试
```bash
# 执行test函数
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_tcp_worker:test().'

# 执行特定测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_tcp_worker:test_connection().'

# 带参数的测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_tcp_worker:test_parsing(<<"EB900020010000000100000000000000">>).'
```

#### 步骤5: 分析结果
```bash
# 查看测试输出
# 测试成功示例:
# === DGIOT UAV TCP Worker 在线测试 ===
# 测试连接...
# 测试数据解析: <<"test_data">>
# 测试完成: 2通过, 0失败
# {ok,all_tests_passed}

# 查看系统日志
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(test|TEST|调试)"
```

## 测试函数设计模式

### 1. 基础测试模式
```erlang
%% 模式1: 简单验证模式
test_simple() ->
    io:format("开始简单测试~n"),
    case some_function(TestInput) of
        {ok, Result} ->
            io:format("✓ 测试通过: ~p~n", [Result]),
            ok;
        {error, Reason} ->
            io:format("✗ 测试失败: ~p~n", [Reason]),
            {error, Reason}
    end.
```

### 2. 参数化测试模式
```erlang
%% 模式2: 参数化测试
test_with_params() ->
    TestCases = [
        {<<"case1">>, Input1, Expected1},
        {<<"case2">>, Input2, Expected2},
        {<<"case3">>, Input3, Expected3}
    ],
    
    lists:foreach(
        fun({CaseName, Input, Expected}) ->
            io:format("测试用例: ~s~n", [CaseName]),
            case test_function(Input) of
                Expected ->
                    io:format("  ✓ 通过~n");
                Actual ->
                    io:format("  ✗ 失败: 期望 ~p, 实际 ~p~n", [Expected, Actual])
            end
        end,
        TestCases
    ).
```

### 3. 集成测试模式
```erlang
%% 模式3: 集成测试
test_integration() ->
    %% 测试完整流程
    Steps = [
        {step1, "初始化", fun init_test/0},
        {step2, "连接测试", fun test_connection/0},
        {step3, "数据处理", fun test_data_processing/0},
        {step4, "清理", fun cleanup/0}
    ],
    
    run_test_steps(Steps).

run_test_steps([]) -> ok;
run_test_steps([{StepId, StepName, StepFun} | Rest]) ->
    io:format("执行步骤 ~s: ~s~n", [StepId, StepName]),
    try
        Result = StepFun(),
        io:format("  ✓ 步骤完成: ~p~n", [Result]),
        run_test_steps(Rest)
    catch
        _:Reason ->
            io:format("  ✗ 步骤失败: ~p~n", [Reason]),
            {error, {step_failed, StepId, Reason}}
    end.
```

## 调试技巧

### 1. 实时日志调试
```erlang
%% 在test函数中添加详细日志
test_with_debug_logs() ->
    ?LOG(debug, "开始测试函数"),
    
    %% 记录关键变量
    ImportantVar = calculate_value(),
    ?LOG(debug, "重要变量值: ~p", [ImportantVar]),
    
    %% 记录执行步骤
    ?LOG(info, "执行步骤1"),
    step1(),
    
    ?LOG(info, "执行步骤2"),
    step2(),
    
    ?LOG(info, "测试完成").
```

### 2. 状态检查
```erlang
%% 检查系统状态
check_system_state() ->
    %% 检查进程状态
    Processes = erlang:processes(),
    io:format("系统进程数: ~p~n", [length(Processes)]),
    
    %% 检查内存使用
    Memory = erlang:memory(),
    io:format("内存使用: ~p~n", [Memory]),
    
    %% 检查模块加载
    LoadedModules = code:all_loaded(),
    io:format("已加载模块数: ~p~n", [length(LoadedModules)]).
```

### 3. 性能测试
```erlang
%% 性能测试函数
test_performance() ->
    StartTime = erlang:monotonic_time(),
    
    %% 执行性能测试
    Results = [expensive_operation() || _ <- lists:seq(1, 1000)],
    
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    
    io:format("性能测试完成~n"),
    io:format("执行次数: 1000~n"),
    io:format("总耗时: ~p ms~n", [Duration]),
    io:format("平均耗时: ~p ms/次~n", [Duration / 1000]).
```

## 常见场景

### 1. 协议解析调试
```erlang
%% 调试协议解析
test_protocol_parsing() ->
    TestPackets = [
        {<<"注册报文">>, <<"wrj_dm-zqy">>},
        {<<"心跳报文">>, <<"heartbeat">>},
        {<<"数据报文">>, <<"EB900020010000000100000000000000">>}
    ],
    
    lists:foreach(
        fun({PacketName, PacketData}) ->
            io:format("测试报文: ~s~n", [PacketName]),
            io:format("  原始数据: ~p~n", [dgiot_utils:binary_to_hex(PacketData)]),
            
            case parse_uav_packet(PacketData) of
                {ok, Parsed} ->
                    io:format("  ✓ 解析成功: ~p~n", [Parsed]);
                {error, Reason} ->
                    io:format("  ✗ 解析失败: ~p~n", [Reason])
            end
        end,
        TestPackets
    ).
```

### 2. 设备连接调试
```erlang
%% 调试设备连接
test_device_connection() ->
    %% 模拟设备连接
    io:format("测试设备连接...~n"),
    
    %% 创建测试socket
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 20000, [binary, {active, false}]),
    io:format("  TCP连接建立: ~p~n", [Socket]),
    
    %% 发送测试数据
    TestData = <<"test_device">>,
    ok = gen_tcp:send(Socket, TestData),
    io:format("  发送数据: ~p~n", [TestData]),
    
    %% 接收响应
    {ok, Response} = gen_tcp:recv(Socket, 0),
    io:format("  接收响应: ~p~n", [Response]),
    
    gen_tcp:close(Socket),
    io:format("  连接关闭~n").
```

### 3. 数据库操作调试
```erlang
%% 调试数据库操作
test_database_operations() ->
    io:format("测试数据库操作...~n"),
    
    %% 测试设备查询
    TestDeviceId = <<"test_device_123">>,
    case dgiot_device:lookup(TestDeviceId) of
        {ok, Device} ->
            io:format("  ✓ 设备查询成功: ~p~n", [Device]);
        {error, not_found} ->
            io:format("  ℹ 设备不存在，测试正常~n");
        {error, Reason} ->
            io:format("  ✗ 设备查询失败: ~p~n", [Reason])
    end,
    
    %% 测试TDengine查询
    TestProductId = <<"test_product">>,
    case dgiot_tdengine_adapter:query(TestProductId, <<"SELECT 1">>) of
        {ok, Result} ->
            io:format("  ✓ TDengine查询成功: ~p~n", [Result]);
        {error, Reason} ->
            io:format("  ✗ TDengine查询失败: ~p~n", [Reason])
    end.
```

## 最佳实践

### 1. 测试函数设计原则
- **独立性**: 每个测试函数应该独立，不依赖外部状态
- **可重复性**: 测试结果应该可重复
- **自描述性**: 测试输出应该清晰描述测试过程和结果
- **安全性**: 测试不应该影响生产数据

### 2. 热编译注意事项
- **零警告**: 热编译前确保没有编译警告
- **依赖检查**: 确保所有依赖模块已正确加载
- **版本兼容**: 测试函数应该与生产代码兼容
- **回滚准备**: 准备好回滚到原始代码

### 3. 在线调试安全准则
- **隔离环境**: 在测试环境中进行在线调试
- **数据备份**: 重要数据操作前进行备份
- **监控告警**: 调试时监控系统状态
- **及时清理**: 测试完成后清理测试数据

## 故障排除

### 1. 热编译失败
```bash
# 问题: 热编译报错
# 解决方案:
# 1. 检查语法错误
erlc -I include/ -o /dev/null src/module.erl

# 2. 检查依赖
_build/emqx/rel/emqx/bin/emqx eval 'code:which(dgiot_plugin).'

# 3. 清理编译文件
make clean
make compile
```

### 2. 测试函数未找到
```bash
# 问题: undefined function
# 解决方案:
# 1. 检查函数是否导出
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_tcp_worker:module_info(exports).'

# 2. 重新热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav_tcp_worker).'

# 3. 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_uav).'
```

### 3. 测试结果不符合预期
```erlang
%% 问题: 测试结果错误
%% 解决方案:
%% 1. 添加详细日志
?LOG(debug, "输入参数: ~p", [Input]),
?LOG(debug, "中间结果: ~p", [IntermediateResult]),

%% 2. 分步调试
test_step_by_step() ->
    Step1 = do_step1(),
    io:format("步骤1结果: ~p~n", [Step1]),
    
    Step2 = do_step2(Step1),
    io:format("步骤2结果: ~p~n", [Step2]),
    
    %% ...
```

## 技能集成

### 1. 与现有技能协同
- **与dgiot_compile_debug集成**: 提供编译和热加载支持
- **与development_workflow_cycle集成**: 作为开发工作流的一部分
- **与erlang_include_system集成**: 解决头文件包含问题
- **与main_objective_tracker集成**: 确保调试不偏离目标

### 2. 完整调试工作流
```
用户请求在线调试 → dgiot_online_debug激活 →
1. 分析调试需求
2. 设计test函数
3. 指导热编译
4. 执行在线测试
5. 分析测试结果
6. 提供优化建议
```

## 与Cline Hooks系统集成

### 1. Hook集成原理
**核心原则**: 如果是Erlang编程，都应该使用在线调试方式来进行调测，通过Cline的Hook系统实现标准化调试流程。

```bash
# Hook集成示例：PreToolUse Hook
# 文件: .clinerules/hooks/PreToolUse
#!/bin/bash

# 检查是否为Erlang编程相关的工具使用
check_erlang_online_debug() {
    local tool_name="$1"
    local tool_args="$2"
    
    # Erlang编程相关关键词
    ERLANG_KEYWORDS=(
        "erlang" "erl" "compile" "eval"
        "dgiot_plugin" "hot_compile" "test"
        "emqx" "otp" "beam" "hrl"
    )
    
    # 在线调试相关关键词
    ONLINE_DEBUG_KEYWORDS=(
        "在线调试" "在线调测" "test函数"
        "热编译" "实时调试" "Erlang Shell"
        "dgiot_online_debug" "添加test"
    )
    
    # 检查是否为Erlang编程相关操作
    local is_erlang_related=false
    for keyword in "${ERLANG_KEYWORDS[@]}"; do
        if [[ "$tool_name" == *"$keyword"* ]] || [[ "$tool_args" == *"$keyword"* ]]; then
            is_erlang_related=true
            break
        fi
    done
    
    # 检查是否为在线调试相关操作
    local is_online_debug=false
    for keyword in "${ONLINE_DEBUG_KEYWORDS[@]}"; do
        if [[ "$tool_name" == *"$keyword"* ]] || [[ "$tool_args" == *"$keyword"* ]]; then
            is_online_debug=true
            break
        fi
    done
    
    # 如果是Erlang编程但未使用在线调试，建议使用dgiot_online_debug技能
    if $is_erlang_related && ! $is_online_debug; then
        echo "[Hook] 检测到Erlang编程操作，建议使用dgiot_online_debug技能进行在线调试" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：对于Erlang编程，推荐使用dgiot_online_debug技能进行在线调试，无需复杂调用关系，直接在Erlang Shell中执行测试。"
    fi
    
    if $is_online_debug; then
        echo "[Hook] 检测到在线调试操作，dgiot_online_debug技能适用" >&2
        CONTEXT_MOD="$CONTEXT_MOD 提示：正在使用dgiot_online_debug技能推荐的在线调试方式。"
    fi
}
```

### 2. TaskComplete Hook集成
```bash
# TaskComplete Hook集成
# 文件: .clinerules/hooks/TaskComplete
#!/bin/bash

# 评估在线调试技能使用情况
evaluate_online_debug_usage() {
    local task_result="$1"
    
    # 检查是否使用了在线调试技能
    local debug_keywords=(
        "test()" "热编译" "emqx eval" "dgiot_plugin:compile"
        "在线执行" "实时调试" "Erlang Shell"
    )
    
    local debug_count=0
    for keyword in "${debug_keywords[@]}"; do
        if [[ "$task_result" == *"$keyword"* ]]; then
            ((debug_count++))
        fi
    done
    
    # 评估在线调试使用情况
    if [ $debug_count -ge 3 ]; then
        echo "[Hook] 优秀：充分使用了dgiot_online_debug技能进行在线调试" >&2
        SUMMARY="$SUMMARY 本次任务充分使用了在线调试技能，实现了高效的Erlang编程调测。"
    elif [ $debug_count -ge 1 ]; then
        echo "[Hook] 良好：部分使用了在线调试技能" >&2
        SUMMARY="$SUMMARY 本次任务使用了在线调试技能，建议进一步推广使用。"
    else
        echo "[Hook] 建议：对于Erlang编程，推荐使用dgiot_online_debug技能进行在线调试" >&2
        RECOMMENDATION="$RECOMMENDATION 对于Erlang编程任务，强烈推荐使用dgiot_online_debug技能进行在线调试，提高调试效率。"
    fi
}
```

### 3. Hook配置示例

#### PreToolUse Hook配置
```bash
#!/bin/bash
# .clinerules/hooks/PreToolUse

# 读取输入
read -r input
tool_name=$(echo "$input" | jq -r '.preToolUse.toolName // ""')
tool_args=$(echo "$input" | jq -r '.preToolUse.toolArgs // ""')

# 检查Erlang在线调试
check_erlang_online_debug "$tool_name" "$tool_args"

# 输出结果
cat << EOF
{
  "cancel": false,
  "contextModification": "$CONTEXT_MOD",
  "errorMessage": ""
}
EOF
```

#### TaskComplete Hook配置
```bash
#!/bin/bash
# .clinerules/hooks/TaskComplete

# 读取输入
read -r input
task_result=$(echo "$input" | jq -r '.taskComplete.result // ""')

# 评估在线调试使用情况
evaluate_online_debug_usage "$task_result"

# 输出结果
cat << EOF
{
  "cancel": false,
  "contextModification": "$CONTEXT_MOD",
  "errorMessage": "",
  "summary": "$SUMMARY",
  "recommendation": "$RECOMMENDATION"
}
EOF
```

### 4. Hook触发条件

#### 自动触发条件
1. **工具使用前** (PreToolUse Hook):
   - 当使用`execute_command`工具执行Erlang相关命令时
   - 当使用`read_file`工具读取Erlang源文件时
   - 当使用`replace_in_file`工具修改Erlang代码时

2. **任务完成时** (TaskComplete Hook):
   - 当任务涉及Erlang编程时
   - 当任务结果包含调试相关关键词时
   - 当任务使用了热编译或在线测试时

#### 手动触发条件
用户可以通过以下方式手动触发：
```bash
# 手动测试Hook
echo '{"preToolUse": {"toolName": "execute_command", "toolArgs": "dgiot_plugin:compile(dgiot_uav)"}}' | .clinerules/hooks/PreToolUse
```

### 5. Hook集成优势

#### 实时指导
- **智能建议**: 在Erlang编程时自动推荐使用在线调试技能
- **最佳实践**: 引导用户遵循DGIOT在线调试标准流程
- **效率提升**: 减少调试时间，提高开发效率

#### 质量保证
- **标准化**: 确保所有Erlang调试都使用统一的在线调试方式
- **可追溯**: 记录所有调试操作，便于问题追踪
- **可复用**: 调试经验可以积累和复用

#### 团队协作
- **统一标准**: 团队所有成员使用相同的调试方法
- **知识共享**: 通过Hook系统共享调试最佳实践
- **持续改进**: 根据Hook反馈不断优化调试流程

### 6. 集成工作流

```
Erlang编程任务开始 → PreToolUse Hook检测 →
1. 检查是否为Erlang编程相关操作
2. 如果是，建议使用dgiot_online_debug技能
3. 用户激活dgiot_online_debug技能
4. 按照技能指导进行在线调试
5. TaskComplete Hook评估调试效果
6. 提供改进建议和总结报告
```

## 维护信息

- **版本历史**:
  - v1.0.0 (2026-01-26): 初始版本，专注于DGIOT在线调试
  - v1.1.0 (2026-01-26): 添加与Cline Hooks系统集成
- **更新计划**:
  - 添加更多调试案例
  - 集成性能分析工具
  - 添加自动化测试框架
  - 优化Hook集成逻辑
-
