%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_config.hrl - 无人机系统配置相关的宏定义和工位配置
%%%
%%% 包含默认配置、工位配置记录、PLC寄存器键定义、
%%% 工位ID常量、IP地址常量、端口常量、基地址常量、
%%% 治具地址常量、传感器编码常量、设备类型常量、IP类型常量等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(DGIOT_UAV_CONFIG_HRL).
-define(DGIOT_UAV_CONFIG_HRL, true).

%% 默认配置宏
-define(DEFAULT_SLAVE_ID, 16#02).
-define(AGGREGATE_TIMEOUT, 1000).
-define(SURFACE_INTERVAL, 5000).
-define(MAX_SURFACE_RETRY, 3).           % 舵面读取最大重试次数

%% ===================================================================
%% ETS表定义
%% ===================================================================

%% 治具状态表
-define(FIXTURE_STATE_TABLE, fixture_state_table).

%% ===================================================================
%% 工位配置
%% ===================================================================

%% 工位配置记录
-record(station, {
    id :: integer(),               % 工位ID
    name :: binary(),              % 工位名称
    ip :: binary() | undefined,    % PLC IP地址（undefined表示无IP连接）
    dtu_ip :: binary() | undefined,% DTU IP地址（仅磁航向工位需要）
    port :: integer(),             % 端口
    base_address :: integer(),     % 基地址（相对地址的基准）
    fixture_address :: integer(),  % 治具地址
    description :: binary()        % 描述
}).

%% PLC寄存器键定义
-define(REG_WAIT_COMMAND, wait_command).      % 等待命令
-define(REG_RECEIVE_REPLY, receive_reply).    % 回复寄存器
-define(REG_MACHINE_STATUS, machine_status).  % 机台状态
-define(REG_ALARM_ID, alarm_id).              % 设备告警ID
-define(REG_HEARTBEAT_49, heartbeat_49).      % 通讯心跳
-define(REG_SEND_COMMAND, send_command).      % 发送指令区
-define(REG_TEST_COMMAND, test_command).      % 测试指令
-define(REG_JUDGE_AREA, judge_area).          % 判定区
-define(REG_TEST_RESULT, test_result).        % 测试回复
-define(REG_Y_COORDINATE, y_coordinate).      % Y执行坐标
-define(REG_Z_COORDINATE, z_coordinate).      % Z执行坐标
-define(REG_Q_COORDINATE, q_coordinate).      % Q执行坐标
-define(REG_BETA_COORDINATE, beta_coordinate).% β执行坐标
-define(REG_HEARTBEAT_99, heartbeat_99).      % 通讯心跳

%% 工位ID常量定义
-define(STATION_MAGNETIC, 1).      % 磁航向工位
-define(STATION_LOADING, 2).       % 上料台
-define(STATION_TOTAL_TEST_2, 3).  % 总测工位2
-define(STATION_TOTAL_TEST_2_POWER, 4).  % 总测工位2-动力检测
-define(STATION_TOTAL_TEST_1, 5).  % 总测工位1
-define(STATION_TOTAL_TEST_1_POWER, 6).  % 总测工位1-动力检测
-define(STATION_BURN_IN_2, 7).     % 拷机工位2
-define(STATION_BURN_IN_1, 8).     % 拷机工位1
-define(STATION_GANTRY, 9).        % 桁行架
-define(STATION_VIRTUAL_ALARM, 10).     % 磁航向PLC监控工位
-define(STATION_VIRTUAL_HEARTBEAT, 11). % 共享PLC监控工位

%% ===================================================================
%% 地址体系说明（两套独立体系，不可混淆）
%% ===================================================================
%% 1) PLC Station ID: PLC内部工位编号(1-11)
%%    用于PLC通道中的工位标识
%%    示例：STATION_MAGNETIC=1, STATION_LOADING=2, ..., STATION_GANTRY=9
%%
%% 2) 治具 station_addr: 治具在PLC寄存器中的地址编号(0-7, 255)
%%    用于PLC Modbus寄存器基地址映射
%%    示例：FIXTURE_MAGNETIC=0, FIXTURE_TOTAL_TEST_2=1, ..., FIXTURE_GANTRY=7
%%
%% 两者关系示例：
%%   STATION_BURN_IN_2 (PLC ID=7) 对应 FIXTURE_BURN_IN_1 (station_addr=7)
%%   STATION_GANTRY (PLC ID=9) 对应 FIXTURE_GANTRY (station_addr=7)
%%   这些数字不同是因为两套体系独立编号，使用时必须明确区分
%%
%% 参考映射表：
%%   station_addr | PLC基地址  | 工位名称         | PLC Station ID
%%   -------------|------------|------------------|---------------
%%   0            | D1700      | 磁航向工位       | STATION_MAGNETIC(1)
%%   1            | D1600      | 总测工位2        | STATION_TOTAL_TEST_2(3)
%%   2            | D1600      | 总测工位2-动力检测| STATION_TOTAL_TEST_2_POWER(4)
%%   3            | D1500      | 总测工位1        | STATION_TOTAL_TEST_1(5)
%%   4            | D1500      | 总测工位1-动力检测| STATION_TOTAL_TEST_1_POWER(6)
%%   5            | D1300      | 拷机工位2        | STATION_BURN_IN_2(7)
%%   6            | D1200      | 拷机工位1        | STATION_BURN_IN_1(8)
%%   7            | D1100      | 桁行架           | STATION_GANTRY(9)
%%   255          | 未知       | 上料台           | STATION_LOADING(2)
%% ===================================================================


%% IP地址常量
-define(IP_MAGNETIC_PLC, <<"192.168.100.20">>).      % 磁航向PLC，负责磁航向工位的告警和心跳检测
-define(IP_MAGNETIC_DTU, <<"192.168.100.21">>).      % 磁航向DTU
-define(IP_SHARED, <<"192.168.100.40">>).            % 共享PLC，负责其他工位的告警和心跳检测
-define(IP_VIRTUAL_ALARM, <<"192.168.100.20">>).     % 磁航向PLC监控工位（与磁航向PLC共用IP）
-define(IP_VIRTUAL_HEARTBEAT, <<"192.168.100.40">>). % 共享PLC监控工位（与共享PLC共用IP）
-define(IP_FIXTURE_PREFIX, <<"192.168.100.X">>).

%% 端口常量
-define(PORT_MODBUS, 502).

%% 从机地址常量（PLC默认为502）
-define(SLAVE_ADDRESS_PLC, 502).   % PLC默认从机地址
-define(SLAVE_ADDRESS_DEVICE_1, 1). % 设备1从机地址
-define(SLAVE_ADDRESS_DEVICE_2, 2). % 设备2从机地址
-define(SLAVE_ADDRESS_BROADCAST, 255). % 广播地址

%% 基地址常量
-define(BASE_MAGNETIC, 1700).      % D1700 磁航向工位
-define(BASE_TOTAL_TEST_2, 1600).  % D1600
-define(BASE_TOTAL_TEST_1, 1500).  % D1500
-define(BASE_BURN_IN_2, 1300).     % D1300
-define(BASE_BURN_IN_1, 1200).     % D1200
-define(BASE_GANTRY, 1100).        % D1100 桁行架工位

%% 虚拟工位地址（需要与真实工位地址区分开）
-define(BASE_VIRTUAL_ALARM, 5000).     % D5000 虚拟告警检测工位（独立地址，不冲突）
-define(BASE_VIRTUAL_HEARTBEAT, 5001). % D5001 虚拟心跳检测工位（独立地址，不冲突）

%% 原始虚拟工位对应的PLC地址（用于监控读取）
-define(PLC_ADDR_VIRTUAL_ALARM, 1730).     % PLC模拟器中的告警地址
-define(PLC_ADDR_VIRTUAL_HEARTBEAT, 49).   % PLC模拟器中的心跳地址

%% 治具地址常量
%% 注意：这是治具在PLC寄存器中的地址编号(0-7, 255)，用于PLC Modbus寄存器基地址映射
%% 与PLC Station ID（工位ID常量）是两套独立的编号体系，不要混淆
-define(FIXTURE_MAGNETIC, 0).      % 磁航向工位
-define(FIXTURE_LOADING, 255).     % 上料台
-define(FIXTURE_TOTAL_TEST_2, 1).  % 总测工位2
-define(FIXTURE_TOTAL_TEST_2_POWER, 2).  % 总测工位2-动力检测
-define(FIXTURE_TOTAL_TEST_1, 3).  % 总测工位1
-define(FIXTURE_TOTAL_TEST_1_POWER, 4).  % 总测工位1-动力检测
-define(FIXTURE_BURN_IN_2, 5).     % 拷机工位2 (station_addr=5, 对应PLC基地址D1300)
-define(FIXTURE_BURN_IN_1, 6).     % 拷机工位1 (station_addr=6, 对应PLC基地址D1200)
-define(FIXTURE_GANTRY, 7).        % 桁行架 (station_addr=7, 对应PLC基地址D1100)
-define(FIXTURE_VIRTUAL_ALARM, 8). % 虚拟告警检测工位
-define(FIXTURE_VIRTUAL_HEARTBEAT, 9). % 虚拟心跳检测工位

%% 传感器编码常量
-define(SENSOR_MAGNETIC, 1).       % 磁航向工位
-define(SENSOR_LOADING, 2).        % 上料台
-define(SENSOR_TOTAL_TEST_2, 3).   % 总测工位2
-define(SENSOR_TOTAL_TEST_2_POWER, 4).  % 总测工位2-动力检测
-define(SENSOR_TOTAL_TEST_1, 5).   % 总测工位1
-define(SENSOR_TOTAL_TEST_1_POWER, 6).  % 总测工位1-动力检测
-define(SENSOR_BURN_IN_2, 7).      % 拷机工位2
-define(SENSOR_BURN_IN_1, 8).      % 拷机工位1
-define(SENSOR_GANTRY, 9).         % 桁行架

%% 设备类型常量
-define(DEVICE_FIXTURE_BASE, <<"治具基准舵面">>).
-define(DEVICE_LEFT_FRONT_WING, <<"左前翼舵面">>).
-define(DEVICE_RIGHT_FRONT_WING, <<"右前翼舵面">>).
-define(DEVICE_LEFT_SIDE_WING, <<"左侧翼舵面">>).
-define(DEVICE_RIGHT_SIDE_WING, <<"右侧翼舵面">>).
-define(DEVICE_GROUND_TEST, <<"地测口">>).
-define(DEVICE_MICROCONTROLLER, <<"单片机">>).

%% IP类型常量
-define(IP_TYPE_EXCLUSIVE, exclusive).  % 独享IP
-define(IP_TYPE_SHARED, shared).        % 共享IP
-define(IP_TYPE_FIXTURE, fixture).      % 治具IP
-define(IP_TYPE_UNKNOWN, unknown).      % 未知IP

%% 连接类型常量
-define(CONN_TYPE_PLC, plc).            % PLC连接
-define(CONN_TYPE_DTU, dtu).            % DTU连接
-define(CONN_TYPE_FIXTURE, fixture).    % 治具连接
-define(CONN_TYPE_GROUND_TEST, ground_test). % 地测口连接

-endif.