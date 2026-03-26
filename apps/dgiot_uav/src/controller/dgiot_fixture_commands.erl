%%--------------------------------------------------------------------
%% @doc 治具命令构建模块
%%
%% 负责构建治具单片机的各种Modbus-RTU控制命令
%%
%% == 通信协议 ==
%% - 从站ID: 2
%% - 功能码: 0x03(读寄存器), 0x05(写线圈), 0x06(写寄存器)
%%--------------------------------------------------------------------
-module(dgiot_fixture_commands).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav_config.hrl").

%% API
-export([
    %% 电源控制
    control_power_relay_on/1, control_power_relay_off/1,
    start_drone/1, stop_drone/1,
    %% 风管控制
    block_wind_tube/1, open_wind_tube/1,
    %% 测试命令
    test_fuse_9_10_resistance/1, test_fuse_7_8_resistance/1,
    test_fuse_7_wing_nail_resistance/1, test_fuse_8_wing_nail_resistance/1,
    test_battery_port_resistance/1,
    test_fuse_5_ground_voltage/1, test_fuse_1_ground_voltage/1,
    %% 信息读取
    read_station_info/1, build_read_station_info/1,
    %% 通讯检测和测试控制
    check_communication/1, start_test/1, end_test/1
]).

%%====================================================================
%% 寄存器/线圈地址定义
%%====================================================================
-define(COIL_POWER_RELAY,  16#0000).   % 大继电器控制
-define(COIL_DRONE_POWER,  16#0001).   % 无人机电源控制
-define(COIL_WIND_TUBE,    16#0002).   % 风管控制
-define(COIL_COMM_CHECK,   3).         % 通讯检测线圈

-define(REG_FUSE_9_10_RESISTANCE,      16#0000).
-define(REG_FUSE_7_8_RESISTANCE,       16#0002).
-define(REG_FUSE_7_WING_NAIL,          16#0004).
-define(REG_FUSE_8_WING_NAIL,          16#0006).
-define(REG_BATTERY_PORT_RES,          16#0008).
-define(REG_FUSE_5_GROUND_VOLT,        16#000A).
-define(REG_FUSE_1_GROUND_VOLT,        16#000B).
-define(REG_STATION_INFO,              16#000D).

-define(VAL_TEST_START, 16#03).
-define(VAL_TEST_END,   16#09).

%%====================================================================
%% 电源控制
%%====================================================================
control_power_relay_on(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_POWER_RELAY, true, "大继电器上电").

control_power_relay_off(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_POWER_RELAY, false, "大继电器断电").

start_drone(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_DRONE_POWER, true, "启动无人机").

stop_drone(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_DRONE_POWER, false, "关闭无人机").

%%====================================================================
%% 风管控制
%%====================================================================
block_wind_tube(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_WIND_TUBE, true, "堵上风管").

open_wind_tube(SlaveId) -> 
    build_coil_cmd(SlaveId, ?COIL_WIND_TUBE, false, "打开风管").

%%====================================================================
%% 测试命令
%%====================================================================
test_fuse_9_10_resistance(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_9_10_RESISTANCE, 2, "引信9,10点电阻").

test_fuse_7_8_resistance(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_7_8_RESISTANCE, 2, "引信7,8点电阻").

test_fuse_7_wing_nail_resistance(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_7_WING_NAIL, 2, "引信7+翼钉电阻").

test_fuse_8_wing_nail_resistance(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_8_WING_NAIL, 2, "引信8+翼钉电阻").

test_battery_port_resistance(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_BATTERY_PORT_RES, 2, "电池端口电阻").

test_fuse_5_ground_voltage(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_5_GROUND_VOLT, 1, "引信5对地电压").

test_fuse_1_ground_voltage(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_FUSE_1_GROUND_VOLT, 1, "引信1对地电压").

%%====================================================================
%% 信息读取
%%====================================================================
read_station_info(SlaveId) -> 
    build_read_cmd(SlaveId, ?REG_STATION_INFO, 1, "工位信息").

build_read_station_info(SlaveId) -> read_station_info(SlaveId).

%%====================================================================
%% 通讯检测和测试控制
%%====================================================================
check_communication(SlaveId) -> 
    Cmd = dgiot_modbus_client:build_coil_command(SlaveId, ?COIL_COMM_CHECK, true),
    log_cmd("通讯检测", SlaveId, Cmd),
    {ok, Cmd}.

start_test(SlaveId) -> 
    build_write_cmd(SlaveId, ?REG_STATION_INFO, ?VAL_TEST_START, "启动测试").

end_test(SlaveId) -> 
    build_write_cmd(SlaveId, ?REG_STATION_INFO, ?VAL_TEST_END, "结束测试").

%%====================================================================
%% 内部函数
%%====================================================================
build_coil_cmd(SlaveId, Address, On, Desc) ->
    Cmd = dgiot_modbus_client:build_coil_command(SlaveId, Address, On),
    log_cmd(Desc, SlaveId, Cmd),
    Cmd.

build_read_cmd(SlaveId, Address, Quantity, Desc) ->
    Cmd = dgiot_modbus_client:build_read_command(SlaveId, Address, Quantity),
    log_cmd(Desc, SlaveId, Cmd),
    Cmd.

build_write_cmd(SlaveId, Address, Value, Desc) ->
    Cmd = dgiot_modbus_client:build_write_command(SlaveId, Address, Value),
    log_cmd(Desc, SlaveId, Cmd),
    Cmd.

log_cmd(Desc, SlaveId, Cmd) ->
    ?LOG(info, "[CMD] ~s - SlaveId:~p, Hex:~s", [Desc, SlaveId, dgiot_utils:binary_to_hex(Cmd)]).
