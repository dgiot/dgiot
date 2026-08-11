# 测试框架工作流

## 概述

本工作流定义了DG-IoT平台插件测试的标准化流程，确保测试全面、自动化且可重复。

## 1. 测试环境准备

### 1.1 环境检查
```bash
# 1. 检查基础环境
make run  # 确保DG-IoT平台正常运行

# 2. 检查插件编译环境
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_drone).'

# 3. 检查Python环境（用于测试脚本）
python3 --version
pip3 list | grep pyshark  # 检查Wireshark解析依赖
```

### 1.2 测试数据准备
```bash
# 创建测试数据目录结构
mkdir -p test/test_data/{valid,invalid,edge_cases,performance}

# 准备测试数据源
# 1. 地测口报文 (baowen/)
# 2. Wireshark抓包 (priv/capture/wireshark/)
# 3. 模拟生成数据
```

## 2. 单元测试工作流

### 2.1 测试文件结构
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

### 2.2 单元测试模板
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

## 3. 协议解析测试工作流

### 3.1 报文解析测试
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

## 4. Wireshark报文解析工作流

### 4.1 Wireshark解析工具使用
```bash
# 1. 安装依赖
pip3 install pyshark

# 2. 解析Wireshark文件并列出所有报文
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --list

# 3. 解析并生成测试用例
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --generate-test

# 4. 解析并重放报文
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --replay 127.0.0.1:8001
```

### 4.2 统一解析器工作流
```python
# test/unified_parser.py 使用流程
# 1. 智能扫描工具
python3 test/unified_parser.py --mode scan --input baowen/

# 2. 协议分析
python3 test/unified_parser.py --mode analyze --input priv/capture/wireshark/drone_capture_001.pcapng

# 3. 生成测试数据
python3 test/unified_parser.py --mode generate --output test/test_data/valid/

# 4. 验证解析结果
python3 test/unified_parser.py --mode validate --erlang-module dgiot_drone
```

## 5. 报文模拟器工作流

### 5.1 模拟器使用流程
```bash
# 1. 启动模拟器（单包模式）
python3 test/packet_simulator.py --mode single --target 127.0.0.1:8001

# 2. 序列发送模式
python3 test/packet_simulator.py --mode sequence --count 10 --interval 1000

# 3. 重放模式（从Wireshark文件）
python3 test/packet_simulator.py --mode replay --source priv/capture/wireshark/drone_capture_001.pcapng

# 4. 生成模式（创建测试数据）
python3 test/packet_simulator.py --mode generate --output test/test_data/generated/
```

### 5.2 模拟器配置
```python
# 模拟器支持的模式
MODES = {
    'single': '单包发送',
    'sequence': '序列发送', 
    'replay': '重放模式',
    'generate': '生成模式'
}

# 目标配置
TARGET_HOST = '127.0.0.1'
TARGET_PORT = 8001

# 发送间隔（毫秒）
SEND_INTERVAL = 1000
```

## 6. 集成测试工作流

### 6.1 端到端测试脚本
```bash
#!/bin/bash
# test/integration_test.sh

echo "开始插件集成测试..."
echo "========================================"

# 1. 编译插件
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'

# 2. 加载插件
echo "2. 加载插件..."
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'

# 3. 启动测试服务器
echo "3. 启动测试服务器..."
python3 test/start_test_server.py --port 8001 &

# 4. 运行模拟器测试
echo "4. 运行模拟器测试..."
python3 test/packet_simulator.py --mode sequence --count 5 --interval 500

# 5. 验证结果
echo "5. 验证结果..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_<plugin_name>:verify_test_results().'

# 6. 清理
echo "6. 清理测试环境..."
pkill -f "start_test_server.py"

echo "========================================"
echo "集成测试完成！"
```

### 6.2 网络通信测试
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

## 7. 性能测试工作流

### 7.1 性能基准测试
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

### 7.2 压力测试
```bash
#!/bin/bash
# test/stress_test.sh

echo "开始压力测试..."
echo "========================================"

# 1. 准备测试数据
echo "1. 准备测试数据..."
python3 test/packet_simulator.py --mode generate --count 10000 --output /tmp/stress_test_data.bin

# 2. 启动性能监控
echo "2. 启动性能监控..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_performance_monitor:start().' &

# 3. 运行压力测试
echo "3. 运行压力测试..."
for i in {1..10}; do
    echo "第 $i 轮压力测试..."
    python3 test/packet_simulator.py --mode replay --source /tmp/stress_test_data.bin --interval 10
    sleep 1
done

# 4. 收集性能数据
echo "4. 收集性能数据..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_performance_monitor:get_stats().'

echo "========================================"
echo "压力测试完成！"
```

## 8. 自动化测试工作流

### 8.1 测试执行脚本
```bash
#!/bin/bash
# test/run_all_tests.sh

echo "开始执行完整测试套件..."
echo "========================================"

# 1. 单元测试
echo "1. 运行单元测试..."
make eunit

# 2. 集成测试
echo "2. 运行集成测试..."
./test/integration_test.sh

# 3. 性能测试
echo "3. 运行性能测试..."
./test/performance_test.sh

# 4. 压力测试
echo "4. 运行压力测试..."
./test/stress_test.sh

# 5. 生成测试报告
echo "5. 生成测试报告..."
./test/generate_test_report.sh

echo "========================================"
echo "完整测试套件执行完成！"
```

### 8.2 持续集成配置
```yaml
# .github/workflows/test.yml
name: DG-IoT Plugin Tests

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Set up Erlang
      uses: erlef/setup-beam@v1
      with:
        otp-version: '24'
        
    - name: Set up Python
      uses: actions/setup-python@v2
      with:
        python-version: '3.9'
        
    - name: Install dependencies
      run: |
        make deps
        pip install pyshark
        
    - name: Run unit tests
      run: make eunit
      
    - name: Run integration tests
      run: ./test/integration_test.sh
      
    - name: Run performance tests
      run: ./test/performance_test.sh
```

## 9. 测试数据管理

### 9.1 测试数据生成
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
    FilePath = filename:join
