%%%-------------------------------------------------------------------
%%% @doc
%%% types.hrl - UAV协议公共类型定义
%%%
%%% 包含所有协议层共享的宏定义和记录：
%%% - 同步头、最小帧大小、载荷子帧头、默认地址、平台类型、数据类型低4位
%%% - 命令ID（遥测）、控制命令码
%%% - 帧类型原子
%%% - uav_frame 和 encode_params 记录
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(TYPES_HRL).
-define(TYPES_HRL, true).

%% 同步头
-define(SYNC_HEADER, 16#EB90).

%% 最小帧大小
-define(MIN_FRAME_SIZE, 13).

%% 载荷子帧头
-define(PAYLOAD_SUB_HEADER, 16#A55A).

%% 默认地址
-define(DEFAULT_DEST_ADDR, 16#0024).
-define(DEFAULT_SRC_ADDR, 16#0000).

%% 平台类型
-define(PLATFORM_206, 16#01).
-define(PLATFORM_206A, 16#02).
-define(PLATFORM_205, 16#03).

%% 数据类型低4位
-define(TELEMETRY_DATA_TYPE_LOW, 16#01).      % 飞控遥测数据
-define(TELEMETRY_DATA_TYPE_LOW_ALT, 16#0C).  % 飞控遥测数据（替代类型，平台类型0x6C低四位为0x0C）
-define(CONTROL_DATA_TYPE_LOW, 16#00).         % 遥控指令（低四位为0）
-define(PAYLOAD_DATA_TYPE_LOW, 16#02).         % 载荷数据
-define(LINK_CONTROL_TYPE_LOW, 16#0E).          % 链路遥测/申请信令
-define(EXTENDED_DATA_TYPE_LOW, 16#0F).        % 扩展数据类型/保留数据类型

%% 命令ID（遥测）
-define(CMD_D1, 16#D1).
-define(CMD_D2, 16#D2).
-define(CMD_D3, 16#D3).
-define(CMD_BATTERY, 16#A1).
-define(CMD_VERSION, 16#C1).
-define(CMD_WAYPOINT, 16#B4).
-define(CMD_SURFACE_CALIB, 16#DE).

%% 控制命令码
-define(CMD_SWITCH, 16#F0).
-define(CMD_WAYPOINT_SWITCH, 16#07).
-define(CMD_PAYLOAD_CONTROL, 16#FD).
-define(CMD_PAYLOAD_CONTINUOUS, 16#FE).
-define(CMD_ROUTE_UPLOAD, 16#C3).
-define(CMD_FLIGHT_TIME, 16#D4).
-define(CMD_RUDDER_CALIB, 16#DC).
-define(CMD_AIRSPEED_CALIB, 16#D3).
-define(CMD_ET_ROUTE, 16#D7).
-define(CMD_ID_SETTING, 16#D8).

%% 链路层平台类型
-define(REMOTE_CONTROL_TYPE, 16#00).   % 遥控帧
-define(TELEMETRY_TYPE, 16#6C).        % 遥测帧（原标准）
-define(TELEMETRY_TYPE_ALT, 16#1F).    % 新增：实际接收到的平台类型 31

%% 无人机产品ID
-define(UAV_PRODUCT_ID, <<"6235befb62">>).

%% 帧类型原子
-define(FRAME_TYPE_TELEMETRY, telemetry).
-define(FRAME_TYPE_CONTROL, control).
-define(FRAME_TYPE_PAYLOAD, payload).
-define(FRAME_TYPE_LINK_CONTROL, link_control).

%% 记录定义
-record(uav_frame, {
    sync_header = ?SYNC_HEADER,
    dest_addr = 0,
    src_addr = 0,
    data_type = 0,
    frame_count = 0,
    key = 0,
    crc = 0,
    payload = <<>>,
    frame_size = 0,
    timestamp = 0,
    frame_type = unknown
}).

-record(encode_params, {
    dest_addr = ?DEFAULT_DEST_ADDR,
    src_addr = ?DEFAULT_SRC_ADDR,
    platform_type = ?PLATFORM_206,
    data_type_low = ?TELEMETRY_DATA_TYPE_LOW,
    frame_count = 1,
    key = 0,
    payload = <<>>,
    timestamp = 0
}).

-endif.