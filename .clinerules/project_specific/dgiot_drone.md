# 无人机插件开发规则

## 概述

本文件定义了DG-IoT无人机插件的开发规则和最佳实践，专门针对无人机协议解析和测试。

## 协议规范

### 1. 协议格式
```
无人机协议数据包格式：
+--------+--------+--------+--------+--------+--------+
| 魔术字 | 长度   | 命令字 | 序列号 | 数据   | CRC    |
| 2字节  | 2字节  | 1字节  | 4字节  | N字节  | 2字节  |
+--------+--------+--------+--------+--------+--------+

魔术字：0xEB90
最小长度：32字节
CRC算法：CRC-16/CCITT-FALSE
```

### 2. 命令字定义
```erlang
% 命令字常量定义
-define(CMD_HEARTBEAT, 16#01).      % 心跳包
-define(CMD_STATUS_REPORT, 16#02).  % 状态上报
-define(CMD_CONTROL, 16#03).        % 控制指令
-define(CMD_CONFIG, 16#04).         % 配置指令
-define(CMD_DATA_UPLOAD, 16#05).    % 数据上传
-define(CMD_ALARM, 16#06).          % 告警信息
```

## 开发命令

### 1. 热编译和热加载
```bash
# 无人机插件热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_drone).'

# 无人机插件热加载
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_drone).'

# 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_drone:test().'
```

### 2. 测试命令
```bash
# 运行无人机协议测试框架
cd apps/dgiot_drone/test && ./start_simulation.sh

# 智能扫描工具
cd apps/dgiot_drone/test && python3 smart_scanner.py --list

# 报文模拟器
cd apps/dgiot_drone/test && python3 packet_simulator.py --mode single --target 127.0.0.1:8001
```

## 测试数据管理

### 1. 数据目录结构
```
apps/dgiot_drone/
├── baowen/                    # 地测口抓包文件
│   ├── drone_packet_001.bin
│   ├── drone_packet_002.bin
│   └── README.md
├── priv/
│   └── capture/              # Wireshark抓包文件
│       ├── wireshark/
│       │   ├── drone_capture_001.pcapng
│       │   ├── drone_capture_002.pcapng
│       │   └── drone_capture_003.pcapng
│       └── parsed/           # 解析后的JSON文件
└── test/                     # 测试框架
    ├── test_data/           # 测试数据
    ├── packet_simulator.py  # 报文模拟器
    └── unified_parser.py    # 统一解析器
```

### 2. Wireshark报文解析工具
```bash
# 解析Wireshark文件并列出所有报文
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --list

# 解析并生成测试用例
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --generate-test

# 解析并重放报文
python3 test/parse_wireshark.py priv/capture/wireshark/drone_capture_001.pcapng --replay 127.0.0.1:8001
```

## 模拟发包流程

### 1. 报文模拟器功能
```python
# 支持的模式
--mode single      # 单包发送
--mode sequence    # 序列发送
--mode replay      # 重放模式
--mode generate    # 生成模式

# 目标地址
--target 127.0.0.1:8001  # 默认目标

# 发送间隔
--interval 1000    # 毫秒间隔
```

### 2. 快速测试命令
```bash
# 启动完整测试框架
cd apps/dgiot_drone/test && ./start_simulation.sh

# 运行智能扫描
cd apps/dgiot_drone/test && python3 smart_scanner.py --list

# 模拟心跳包
cd apps/dgiot_drone/test && python3 packet_simulator.py --mode single --packet-type heartbeat

# 重放Wireshark抓包
cd apps/dgiot_drone/test && python3 packet_simulator.py --mode replay --source priv/capture/wireshark/drone_capture_001.pcapng
```

## 代码规范

### 1. 模块结构
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_drone 模块 - 无人机协议解析
%%%
%%% 支持协议版本：V1.0
%%% 支持命令字：0x01-0x06
%%% 支持数据包类型：心跳、状态、控制、配置、数据、告警
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_drone).

%% API
-export([parse_packet/1, encode_packet/1, validate_crc/1]).

%% 内部函数
-export([]).

-include("dgiot_drone.hrl").
-include_lib("dgiot/include/dgiot.hrl").
```

### 2. 解析函数规范
```erlang
%% @doc 解析无人机协议数据包
%% @spec parse_packet(binary()) -> {ok, map()} | {error, term()}
parse_packet(<<?MAGIC_NUMBER:16, Length:16, Cmd:8, Seq:32, Data:Length/binary, CRC:16>>) ->
    case validate_crc(<<?MAGIC_NUMBER:16, Length:16, Cmd:8, Seq:32, Data:Length/binary>>, CRC) of
        true ->
            ParsedData = parse_command(Cmd, Data),
            {ok, #{
                magic => ?MAGIC_NUMBER,
                length => Length,
                command => Cmd,
                sequence => Seq,
                data => ParsedData,
                crc => CRC
            }};
        false ->
            {error, crc_mismatch}
    end;
parse_packet(_) ->
    {error, invalid_format}.
```

### 3. 日志格式
```erlang
% 无人机特定日志格式
log_drone_event(Event, Data) ->
    io:format("~s ~p [DRONE] ~p = ~p.~n", [?FILE, ?LINE, Event, Data]).
```

## 测试规范

### 1. 单元测试结构
```erlang
-module(dgiot_drone_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_drone.hrl").

parse_heartbeat_test() ->
    % 生成心跳包测试数据
    HeartbeatPacket = generate_heartbeat_packet(),
    ?assertMatch({ok, #{command := ?CMD_HEARTBEAT}}, dgiot_drone:parse_packet(HeartbeatPacket)).

validate_crc_test() ->
    Packet = <<16#EB, 16#90, 0, 32, 16#01, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0>>,
    ?assertEqual(true, dgiot_drone:validate_crc(Packet)).
```

### 2. 集成测试
```bash
# 端到端测试脚本
#!/bin/bash
# start_integration_test.sh

echo "启动无人机插件集成测试..."
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_drone).'

echo "2. 加载插件..."
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_drone).'

echo "3. 运行模拟器..."
cd apps/dgiot_drone/test && python3 packet_simulator.py --mode sequence --count 10

echo "4. 验证结果..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_drone:verify_test_results().'
```

## 故障排除

### 1. 常见问题
```bash
# 插件加载失败
# 检查依赖：确保所有依赖模块已编译
# 检查导出函数：确保API函数正确导出

# 报文解析失败
# 检查魔术字：确保魔术字为0xEB90
# 检查CRC：使用validate_crc函数验证
# 检查长度：确保数据包长度正确

# 模拟器无法连接
# 检查目标地址：确保127.0.0.1:8001可访问
# 检查防火墙：确保端口开放
```

### 2. 调试命令
```erlang
% 启用详细日志
dgiot_drone:set_log_level(debug).

% 手动测试解析
Packet = <<16#EB, 16#90, 0, 32, 16#01, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0>>,
dgiot_drone:parse_packet(Packet).

% 查看插件状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:status(dgiot_drone).'
```

## 最佳实践

### 1. 开发流程
1. **分析协议文档**：明确协议格式和字段定义
2. **创建测试数据**：准备Wireshark抓包和地测口报文
3. **编写解析模块**：实现parse_packet/1和encode_packet/1
4. **搭建测试框架**：创建自动化测试脚本
5. **集成验证**：与DG-IoT平台集成测试

### 2. 测试策略
- **单元测试**：覆盖所有解析函数
- **集成测试**：验证端到端流程
- **性能测试**：测试高并发场景
- **兼容性测试**：验证不同版本协议

### 3. 文档要求
- 每个函数必须有完整的@doc注释
- 协议格式必须有详细说明
- 测试用例必须有预期结果
- 故障排除必须有具体步骤

## 更新记录

- 2025-12-03：创建无人机插件规则文档
- 基于现有测试框架和协议分析

## 相关资源

- [无人机协议文档]：`apps/dgiot_drone/docs/protocol.md`
- [测试框架架构]：`apps/dgiot_drone/test/TEST_FRAMEWORK_ARCHITECTURE.md`
- [Wireshark解析工具]：`apps/dgiot_drone/test/parse_wireshark.py`
