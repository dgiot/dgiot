# 工位配置更正说明

## 问题

之前的工位配置错误：
- ❌ 错误：1700 = 告警检测工位
- ❌ 错误：1200 = 磁航向工位
- ❌ 错误：1500、1600 = 总测工位

## 正确配置（根据实际代码 dgiot_uav_plc_tcp_channel.erl）

### 工位映射表

| 工位ID | 工位名称 | PLC基地址 | 设备组合 | 说明 |
|--------|----------|-----------|----------|------|
| **1100** | 桁行架工位 | D1100 | 舵面×5+单片机+地测口+无人机 | 桁架机械手测试 |
| **1200** | 拷机1工位 | D1200 | 舵面×5+单片机+地测口+无人机 | 拷机准备+空速标定 |
| **1300** | 拷机2工位 | D1300 | 舵面×5+单片机+地测口+无人机 | 拷机测试 |
| **1500** | 总测1工位 | D1500 | 舵面×5+单片机+地测口+无人机 | 机器人手臂测试（含动力检测） |
| **1600** | 总测2工位 | D1600 | 舵面×5+单片机+地测口+无人机 | 机器人手臂测试（含动力检测） |
| **1700** | 磁航向工位 | D1700 | 地测口+扫码枪 | 磁航向校准和测试 |

### 虚拟工位

| 工位ID | 工位名称 | PLC基地址 | 说明 |
|--------|----------|-----------|------|
| 5000 | 磁航向PLC监控 | D5000 | 虚拟告警检测工位（独立地址，不冲突） |
| 5001 | 共享PLC监控 | D5001 | 虚拟心跳检测工位（独立地址，不冲突） |

### 源代码定义

```erlang
%% 来自 dgiot_uav_config.hrl
-define(BASE_MAGNETIC, 1700).      % D1700 磁航向工位
-define(BASE_TOTAL_TEST_2, 1600).  % D1600
-define(BASE_TOTAL_TEST_1, 1500).  % D1500
-define(BASE_BURN_IN_2, 1300).     % D1300
-define(BASE_BURN_IN_1, 1200).     % D1200
-define(BASE_GANTRY, 1100).        % D1100 桁行架工位

-define(BASE_VIRTUAL_ALARM, 5000).     % D5000 虚拟告警检测工位（独立地址，不冲突）
-define(BASE_VIRTUAL_HEARTBEAT, 5001). % D5001 虚拟心跳检测工位（独立地址，不冲突）
```

### PLC通道配置详情

```erlang
%% 来自 dgiot_uav_plc_tcp_channel.erl
%% 工位配置
[
    {
        station_id => ?BASE_GANTRY,              % 1100
        station_name => <<"桁行架"/utf8>>,
        ip => ?IP_SHARED,                      % 192.168.100.40
        port => ?PORT_MODBUS,                   % 502
        base_address => ?BASE_GANTRY,            % D1100
        fixture_address => ?FIXTURE_GANTRY,       % 7
        instruction_set => <<"桁行架"/utf8>>,
        commands => [
            {<<"58e0d17e22_1">>, 1},
            {<<"58e0d17e22_2">>, 2}
        ],
        command_interval => 1000
    },
    {
        station_id => ?BASE_BURN_IN_1,           % 1200
        station_name => <<"拷机"/utf8>>,
        ip => ?IP_SHARED,                       % 192.168.100.40
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_BURN_IN_1,           % D1200
        fixture_address => ?FIXTURE_BURN_IN_1,     % 6
        instruction_set => <<"拷机测试"/utf8>>,
        commands => [
            {<<"b377b6e364">>, 1},            % 拷机准备
            {<<"ff197f0670">>, 2}             % 空速标定
        ],
        command_interval => 1500
    },
    {
        station_id => ?BASE_BURN_IN_2,           % 1300
        station_name => <<"拷机"/utf8>>,
        ip => ?IP_SHARED,                       % 192.168.100.40
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_BURN_IN_2,           % D1300
        fixture_address => ?FIXTURE_BURN_IN_2,     % 5
        instruction_set => <<"拷机测试"/utf8>>,
        commands => [],
        command_interval => 1000
    },
    {
        station_id => ?BASE_TOTAL_TEST_1,         % 1500
        station_name => <<"总测"/utf8>>,
        ip => ?IP_SHARED,                       % 192.168.100.40
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_TOTAL_TEST_1,         % D1500
        fixture_address => ?FIXTURE_TOTAL_TEST_1,   % 3
        fixture_address1 => ?FIXTURE_TOTAL_TEST_1_POWER,  % 4
        instruction_set => <<"机器人手臂"/utf8>>,
        commands => [
            {<<"bb896ba543_1">>, 1},          % 飞控版本号检查
            {<<"ce7d8a050c_2">>, 2},          % 弹翼开关与引信通信调试
            {<<"7e6c8a5125_3">>, 3},          % 空速调试
            {<<"7e6155207c_4">>, 4},          % 舵电故障调试
            {<<"4950ffcc3a_5">>, 5},          % 引信24V供电调试
            {<<"082099bb72_6">>, 6}           % 帧频检查
        ],
        command_interval => 1200
    },
    {
        station_id => ?BASE_MAGNETIC,             % 1700
        station_name => <<"磁航向测试"/utf8>>,
        ip => ?IP_MAGNETIC_PLC,                % 192.168.100.20
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_MAGNETIC,           % D1700
        fixture_address => ?FIXTURE_MAGNETIC,     % 0
        instruction_set => <<"磁航向"/utf8>>,
        commands => [
            {<<"58e0d17e22">>, 1},           % 磁航向校准
            {<<"eef47bcea7">>, 2}            % 磁航向测试
        ],
        command_interval => 1000
    },
    {
        station_id => ?BASE_TOTAL_TEST_2,         % 1600
        station_name => <<"总测"/utf8>>,
        ip => ?IP_SHARED,                       % 192.168.100.40
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_TOTAL_TEST_2,         % D1600
        fixture_address => ?FIXTURE_TOTAL_TEST_2,   % 1
        fixture_address1 => ?FIXTURE_TOTAL_TEST_2_POWER,  % 2
        instruction_set => <<"机器人手臂"/utf8>>,
        commands => [],
        command_interval => 1000
    },
    {
        station_id => ?BASE_VIRTUAL_ALARM,         % 5000
        station_name => <<"磁航向PLC监控"/utf8>>,
        ip => ?IP_VIRTUAL_ALARM,                % 虚拟地址
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_VIRTUAL_ALARM,       % D1700（与PLC模拟器地址范围1700-1799对齐）
        fixture_address => ?FIXTURE_VIRTUAL_ALARM, % 8
        instruction_set => <<"PLC监控"/utf8>>,
        commands => [],                           % 无指令，只做PLC状态监控
        command_interval => 1000
    },
    {
        station_id => ?BASE_VIRTUAL_HEARTBEAT,      % 5001
        station_name => <<"共享PLC监控"/utf8>>,
        ip => ?IP_VIRTUAL_HEARTBEAT,            % 虚拟地址
        port => ?PORT_MODBUS,                    % 502
        base_address => ?BASE_VIRTUAL_HEARTBEAT,  % D1100（与PLC模拟器地址范围1100-1199对齐）
        fixture_address => ?FIXTURE_VIRTUAL_HEARTBEAT, % 9
        instruction_set => <<"PLC监控"/utf8>>,
        commands => [],                           % 无指令，只做PLC状态监控
        command_interval => 1000
    }
]
```

## 修正要点

### 1. 工位名称更正

- ✅ **1100** = 桁行架工位（不是桁架工位）
- ✅ **1200** = 拷机1工位（不是磁航向工位）
- ✅ **1300** = 拷机2工位（新增）
- ✅ **1500** = 总测1工位（不是总测工位）
- ✅ **1600** = 总测2工位（不是拷机工位）
- ✅ **1700** = 磁航向工位（不是告警检测工位）

### 2. PLC配置更正

- **1100**: 基地址 D1100, IP 192.168.100.40
- **1200**: 基地址 D1200, IP 192.168.100.40
- **1300**: 基地址 D1300, IP 192.168.100.40
- **1500**: 基地址 D1500, IP 192.168.100.40
- **1600**: 基地址 D1600, IP 192.168.100.40
- **1700**: 基地址 D1700, IP 192.168.100.20（独立的磁航向PLC）

### 3. 设备组合更正

- **1100-1600**: 舵面×5 + 单片机 + 地测口 + 无人机
- **1700**: 地测口 + 扫码枪（磁航向专用）

### 4. 治具地址映射

| 工位 | 治具地址 | 说明 |
|------|----------|------|
| 1100 | 7 | 桁行架治具 |
| 1200 | 6 | 拷机1治具 |
| 1300 | 5 | 拷机2治具 |
| 1500 | 3,4 | 总测1治具（主+动力检测） |
| 1600 | 1,2 | 总测2治具（主+动力检测） |
| 1700 | 0 | 磁航向治具 |

## 下一步

需要更新以下文件中的工位配置：

1. `one_click_production_test.py` - 主测试脚本
2. `README_ONE_CLICK_TEST.md` - 使用文档
3. `ONE_CLICK_TEST_SUMMARY.md` - 系统总结
4. `QUICK_REFERENCE.md` - 快速参考

## 参考文档

- **源代码**: `/root/gitee/dgiot/apps/dgiot_uav/src/channel/dgiot_uav_plc_tcp_channel.erl`
- **配置文件**: `/root/gitee/dgiot/apps/dgiot_uav/include/dgiot_uav_config.hrl`

---

**创建日期**: 2026-03-25
**作者**: CodeBuddy AI Assistant
**版本**: v1.1.0（修正版）
