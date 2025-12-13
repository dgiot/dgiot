# Modbus插件开发规则

## 概述

本文件定义了DG-IoT Modbus插件的开发规则和最佳实践，专门针对Modbus协议解析和测试。

## 开发命令

### 1. 热编译和热加载
```bash
# Modbus插件热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# Modbus插件热加载
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_modbus).'

# 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus:test().'

# 全量编译调试
make run
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
io:format("~s ~p [MODBUS] ~p = ~p.~n", [?FILE, ?LINE, Action, Data])
```

### 2. 错误处理
```erlang
% 使用try-catch处理异常
try
    do_modbus_operation()
catch
    error:Reason ->
        io:format("~s ~p Modbus Error: ~p~n", [?FILE, ?LINE, Reason]),
        {error, Reason}
end.
```

## API调试

### 1. Schema更新
```bash
# 新增API后更新Schema
dgiot_parse_utils:update_schemas_json().
```

## 本地笔记管理

### 1. 笔记存储
- **插件工程相关笔记都存到工程下面的ReadMe文件**
- 保持README.md文件更新，记录开发过程中的重要发现
- 添加代码注释，解释复杂的逻辑和算法
- 创建API文档，方便其他开发者使用

## Modbus协议规范

### 1. 协议格式
```
Modbus RTU协议格式：
+--------+--------+--------+--------+--------+--------+
| 地址   | 功能码 | 数据   | CRC    |
| 1字节  | 1字节  | N字节  | 2字节  |
+--------+--------+--------+--------+

常用功能码：
- 0x01: 读取线圈状态
- 0x03: 读取保持寄存器
- 0x06: 写单个寄存器
- 0x10: 写多个寄存器
```

### 2. CRC算法
```erlang
% Modbus CRC-16算法实现
calculate_crc(Data) ->
    calculate_crc(Data, 16#FFFF).

calculate_crc(<<>>, CRC) ->
    CRC;
calculate_crc(<<Byte:8, Rest/binary>>, CRC) ->
    NewCRC = CRC bxor Byte,
    calculate_crc_loop(Rest, NewCRC, 8).

calculate_crc_loop(Data, CRC, 0) ->
    calculate_crc(Data, CRC);
calculate_crc_loop(Data, CRC, Count) ->
    case (CRC band 1) of
        1 -> NewCRC = (CRC bsr 1) bxor 16#A001;
        0 -> NewCRC = CRC bsr 1
    end,
    calculate_crc_loop(Data, NewCRC, Count - 1).
```

## 测试规范

### 1. 单元测试
```erlang
-module(dgiot_modbus_test).

-include_lib("eunit/include/eunit.hrl").

parse_modbus_frame_test() ->
    Frame = <<1, 3, 0, 0, 0, 1, 0x84, 0x0A>>,
    ?assertMatch({ok, #{address := 1, function := 3}}, dgiot_modbus:parse_frame(Frame)).

calculate_crc_test() ->
    Data = <<1, 3, 0, 0, 0, 1>>,
    ?assertEqual(16#840A, dgiot_modbus:calculate_crc(Data)).
```

### 2. 集成测试
```bash
# 端到端测试脚本
#!/bin/bash
# test_modbus_integration.sh

echo "启动Modbus插件集成测试..."
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

echo "2. 加载插件..."
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_modbus).'

echo "3. 运行测试..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus:test_integration().'

echo "4. 验证结果..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_modbus:verify_test_results().'
```

## 代码规范

### 1. 模块结构
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_modbus 模块 - Modbus协议解析
%%%
%%% 支持协议类型：RTU, TCP
%%% 支持功能码：0x01, 0x03, 0x06, 0x10
%%% 支持数据格式：线圈、寄存器
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_modbus).

%% API
-export([parse_frame/1, encode_frame/1, validate_crc/1]).

%% 内部函数
-export([]).

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot.hrl").
```

### 2. 解析函数规范
```erlang
%% @doc 解析Modbus帧
%% @spec parse_frame(binary()) -> {ok, map()} | {error, term()}
parse_frame(<<Address:8, Function:8, Data/binary>>) ->
    case validate_crc(<<Address:8, Function:8, Data/binary>>) of
        true ->
            ParsedData = parse_function(Function, Data),
            {ok, #{
                address => Address,
                function => Function,
                data => ParsedData
            }};
        false ->
            {error, crc_mismatch}
    end;
parse_frame(_) ->
    {error, invalid_format}.
```

## 故障排除

### 1. 常见问题
```bash
# 插件加载失败
# 检查依赖：确保所有依赖模块已编译
# 检查导出函数：确保API函数正确导出

# CRC校验失败
# 检查CRC算法：确保使用正确的Modbus CRC-16算法
# 检查数据格式：确保数据字节顺序正确

# 通信超时
# 检查串口配置：波特率、数据位、停止位、校验位
# 检查设备地址：确保地址正确
```

### 2. 调试命令
```erlang
% 启用详细日志
dgiot_modbus:set_log_level(debug).

% 手动测试解析
Frame = <<1, 3, 0, 0, 0, 1, 0x84, 0x0A>>,
dgiot_modbus:parse_frame(Frame).

% 查看插件状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:status(dgiot_modbus).'
```

## 最佳实践

### 1. 开发流程
1. **分析协议文档**：明确Modbus协议格式和功能码定义
2. **创建测试数据**：准备Modbus设备模拟数据
3. **编写解析模块**：实现parse_frame/1和encode_frame/1
4. **搭建测试框架**：创建自动化测试脚本
5. **集成验证**：与真实Modbus设备集成测试

### 2. 测试策略
- **单元测试**：覆盖所有解析函数和CRC算法
- **集成测试**：验证端到端通信流程
- **性能测试**：测试高并发读取场景
- **兼容性测试**：验证不同设备厂商的协议差异

### 3. 文档要求
- 每个函数必须有完整的@doc注释
- 协议格式必须有详细说明
- 测试用例必须有预期结果
- 故障排除必须有具体步骤

## 更新记录

- 2025-12-03：创建Modbus插件规则文档
- 基于现有规则和Modbus协议规范

## 相关资源

- [Modbus协议规范]：`apps/dgiot_modbus/docs/protocol.md`
- [测试框架]：`apps/dgiot_modbus/test/`
- [API文档]：`apps/dgiot_modbus/README.md`
