# 测试用例模板

## 概述

本模板用于指导测试用例的编写，确保测试覆盖全面且符合DG-IoT平台的测试规范。

## 1. 测试文件结构

### 1.1 测试文件命名
- 单元测试文件：`<module_name>_test.erl`
- 集成测试文件：`<module_name>_integration_test.erl`
- 性能测试文件：`<module_name>_performance_test.erl`

### 1.2 测试目录结构
```
test/
├── <module_name>_test.erl              # 单元测试
├── <module_name>_integration_test.erl  # 集成测试
├── <module_name>_performance_test.erl  # 性能测试
└── test_data/                          # 测试数据
    ├── valid/                          # 有效数据
    ├── invalid/                        # 无效数据
    ├── edge_cases/                     # 边界条件数据
    └── performance/                    # 性能测试数据
```

## 2. 单元测试模板

### 2.1 基本结构
```erlang
-module(dgiot_<module_name>_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_<module_name>.hrl").

%% 测试集定义
all_test_() ->
    [
        {"功能测试", fun function_test/0},
        {"边界条件测试", fun edge_case_test/0},
        {"错误处理测试", fun error_handling_test/0}
    ].

%% 功能测试
function_test() ->
    % 准备测试数据
    TestData = prepare_test_data(),
    
    % 执行测试
    Result = dgiot_<module_name>:function_to_test(TestData),
    
    % 验证结果
    ?assertMatch({ok, _}, Result).

%% 边界条件测试
edge_case_test() ->
    % 测试边界条件
    EdgeCases = [
        {empty_data, <<>>},
        {max_length, binary:copy(<<1>>, 1024)},
        {min_length, <<0>>}
    ],
    
    lists:foreach(
        fun({CaseName, Data}) ->
            io:format("~s ~p Testing edge case: ~p~n", [?FILE, ?LINE, CaseName]),
            Result = dgiot_<module_name>:function_to_test(Data),
            ?assert(is_tuple(Result))
        end,
        EdgeCases
    ).

%% 错误处理测试
error_handling_test() ->
    % 测试错误场景
    ErrorCases = [
        {invalid_input, <<255, 255, 255, 255>>},
        {wrong_format, "string_instead_of_binary"},
        {null_data, undefined}
    ],
    
    lists:foreach(
        fun({CaseName, Data}) ->
            io:format("~s ~p Testing error case: ~p~n", [?FILE, ?LINE, CaseName]),
            Result = dgiot_<module_name>:function_to_test(Data),
            ?assertMatch({error, _}, Result)
        end,
        ErrorCases
    ).
```

### 2.2 协议解析测试模板
```erlang
%% 协议解析测试
parse_packet_test_() ->
    [
        {"测试有效报文解析", fun test_valid_packet_parsing/0},
        {"测试无效报文解析", fun test_invalid_packet_parsing/0},
        {"测试CRC校验", fun test_crc_validation/0},
        {"测试命令字解析", fun test_command_parsing/0}
    ].

test_valid_packet_parsing() ->
    % 从测试数据文件读取有效报文
    {ok, ValidPackets} = file:read_file("test/test_data/valid/valid_packets.bin"),
    
    % 解析每个报文
    Packets = split_packets(ValidPackets),
    lists:foreach(
        fun(Packet) ->
            Result = dgiot_<module_name>:parse_packet(Packet),
            ?assertMatch({ok, #{}}, Result)
        end,
        Packets
    ).

test_invalid_packet_parsing() ->
    % 从测试数据文件读取无效报文
    {ok, InvalidPackets} = file:read_file("test/test_data/invalid/invalid_packets.bin"),
    
    % 解析每个报文
    Packets = split_packets(InvalidPackets),
    lists:foreach(
        fun(Packet) ->
            Result = dgiot_<module_name>:parse_packet(Packet),
            ?assertMatch({error, _}, Result)
        end,
        Packets
    ).

test_crc_validation() ->
    % 测试CRC校验
    PacketWithValidCRC = <<16#EB, 16#90, 0, 32, 16#01, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0>>,
    PacketWithInvalidCRC = <<16#EB, 16#90, 0, 32, 16#01, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1>>,
    
    ?assertEqual(true, dgiot_<module_name>:validate_crc(PacketWithValidCRC)),
    ?assertEqual(false, dgiot_<module_name>:validate_crc(PacketWithInvalidCRC)).

test_command_parsing() ->
    % 测试不同命令字解析
    Commands = [
        {16#01, "心跳包"},
        {16#02, "状态上报"},
        {16#03, "控制指令"}
    ],
    
    lists:foreach(
        fun({Command, Description}) ->
            io:format("~s ~p Testing command: ~s (0x~2.16.0B)~n", 
                     [?FILE, ?LINE, Description, Command]),
            Packet = build_test_packet(Command),
            Result = dgiot_<module_name>:parse_packet(Packet),
            ?assertMatch({ok, #{command := Command}}, Result)
        end,
        Commands
    ).
```

## 3. 集成测试模板

### 3.1 端到端测试
```erlang
-module(dgiot_<module_name>_integration_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_<module_name>.hrl").

%% 集成测试
end_to_end_test_() ->
    {timeout, 30, fun test_end_to_end_flow/0}.

test_end_to_end_flow() ->
    % 1. 启动插件
    io:format("~s ~p Starting plugin...~n", [?FILE, ?LINE]),
    ok = dgiot_<module_name>:start(),
    
    % 2. 准备测试数据
    TestData = generate_test_data(),
    
    % 3. 执行完整流程
    io:format("~s ~p Executing end-to-end flow...~n", [?FILE, ?LINE]),
    {ok, ParsedData} = dgiot_<module_name>:parse_packet(TestData),
    {ok, EncodedData} = dgiot_<module_name>:encode_packet(ParsedData),
    
    % 4. 验证结果
    ?assert(is_binary(EncodedData)),
    ?assert(is_map(ParsedData)),
    
    % 5. 停止插件
    io:format("~s ~p Stopping plugin...~n", [?FILE, ?LINE]),
    ok = dgiot_<module_name>:stop().
```

### 3.2 网络通信测试
```erlang
%% 网络通信测试
network_communication_test_() ->
    {setup,
     fun setup_network/0,
     fun cleanup_network/1,
     fun test_network_communication/1}.

setup_network() ->
    % 启动模拟服务器
    {ok, ServerPid} = start_mock_server(),
    ServerPid.

cleanup_network(ServerPid) ->
    % 停止模拟服务器
    stop_mock_server(ServerPid).

test_network_communication(ServerPid) ->
    % 测试网络通信
    TestPacket = generate_test_packet(),
    
    % 发送数据
    {ok, Socket} = gen_tcp:connect("localhost", 8001, [binary, {active, false}]),
    ok = gen_tcp:send(Socket, TestPacket),
    
    % 接收响应
    {ok, Response} = gen_tcp:recv(Socket, 0),
    
    % 验证响应
    ?assert(is_binary(Response)),
    ?assertMatch({ok, _}, dgiot_<module_name>:parse_packet(Response)),
    
    gen_tcp:close(Socket).
```

## 4. 性能测试模板

### 4.1 性能基准测试
```erlang
-module(dgiot_<module_name>_performance_test).

-include_lib("eunit/include/eunit.hrl").

%% 性能测试
performance_benchmark_test_() ->
    {timeout, 60, fun run_performance_benchmark/0}.

run_performance_benchmark() ->
    % 准备测试数据
    TestData = load_performance_test_data(),
    
    % 运行基准测试
    io:format("~s ~p Starting performance benchmark...~n", [?FILE, ?LINE]),
    
    % 测试解析性能
    ParseTimes = test_parse_performance(TestData, 1000),
    io:format("~s ~p Parse performance: ~p ms per packet~n", 
              [?FILE, ?LINE, lists:sum(ParseTimes) / length(ParseTimes)]),
    
    % 测试编码性能
    EncodeTimes = test_encode_performance(TestData, 1000),
    io:format("~s ~p Encode performance: ~p ms per packet~n", 
              [?FILE, ?LINE, lists:sum(EncodeTimes) / length(EncodeTimes)]),
    
    % 验证性能要求
    ?assert(lists:sum(ParseTimes) / length(ParseTimes) < 10),  % 小于10ms
    ?assert(lists:sum(EncodeTimes) / length(EncodeTimes) < 10). % 小于10ms

test_parse_performance(Data, Count) ->
    test_performance(fun() -> dgiot_<module_name>:parse_packet(Data) end, Count).

test_encode_performance(Data, Count) ->
    {ok, Parsed} = dgiot_<module_name>:parse_packet(Data),
    test_performance(fun() -> dgiot_<module_name>:encode_packet(Parsed) end, Count).

test_performance(Fun, Count) ->
    lists:map(
        fun(_) ->
            StartTime = erlang:monotonic_time(millisecond),
            Fun(),
            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end,
        lists:seq(1, Count)
    ).
```

## 5. 测试数据管理

### 5.1 测试数据生成
```erlang
%% 测试数据生成函数
generate_test_data() ->
    % 生成有效测试数据
    Magic = 16#EB90,
    Length = 32,
    Command = 16#01,
    Sequence = 1,
    Data = binary:copy(<<0>>, 20),
    CRC = calculate_crc(<<Magic:16, Length:16, Command:8, Sequence:32, Data/binary>>),
    
    <<Magic:16, Length:16, Command:8, Sequence:32, Data/binary, CRC:16>>.

%% 加载测试数据文件
load_test_data(FileName) ->
    FilePath = filename:join(["test", "test_data", FileName]),
    case file:read_file(FilePath) of
        {ok, Data} -> Data;
        {error, Reason} ->
            io:format("~s ~p Failed to load test data from ~s: ~p~n", 
                     [?FILE, ?LINE, FilePath, Reason]),
            <<>>
    end.
```

## 6. 测试执行和报告

### 6.1 测试脚本模板
```bash
#!/bin/bash
# run_tests.sh

echo "开始执行测试..."
echo "========================================"

# 1. 编译测试
echo "1. 编译测试代码..."
make test-compile

# 2. 运行单元测试
echo "2. 运行单元测试..."
make eunit

# 3. 运行集成测试
echo "3. 运行集成测试..."
make integration-test

# 4. 运行性能测试
echo "4. 运行性能测试..."
make performance-test

# 5. 生成测试报告
echo "5. 生成测试报告..."
make test-report

echo "========================================"
echo "测试完成！"
```

### 6.2 测试报告模板
```erlang
%% 测试报告生成
generate_test_report() ->
    % 收集测试结果
    UnitResults = run_unit_tests(),
    IntegrationResults = run_integration_tests(),
    PerformanceResults = run_performance_tests(),
    
    % 生成报告
    Report = #{
        timestamp => erlang:system_time(),
        unit_tests => UnitResults,
        integration_tests => IntegrationResults,
        performance_tests => PerformanceResults,
        summary => calculate_summary(UnitResults, IntegrationResults, PerformanceResults)
    },
    
    % 保存报告
    save_report(Report).
```

## 7. 测试最佳实践

### 7.1 测试覆盖要求
- [ ] 单元测试覆盖率达到80%以上
- [ ] 集成测试覆盖所有主要功能
- [ ] 性能测试覆盖关键路径
- [ ] 边界条件测试完整

### 7.2 测试数据要求
- [ ] 使用真实抓包数据作为测试数据
- [ ] 包含有效和无效数据
- [ ] 包含边界条件数据
- [ ] 数据量足够进行性能测试

### 7.3 测试执行要求
- [ ] 测试可重复执行
- [ ] 测试结果稳定可靠
- [ ] 测试执行时间合理
- [ ] 测试报告清晰完整

---

**提示：** 将 `<module_name>` 替换为实际的模块名称，根据具体协议调整测试数据生成函数。
