%%%-------------------------------------------------------------------
%%% @doc
%%% extra_commands.hrl - 额外命令的数据记录定义
%%%
%%% 包含版本、航点、舵面校准、电池、链路状态等记录。
%%% 对应协议中的额外命令帧。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(EXTRA_COMMANDS_HRL).
-define(EXTRA_COMMANDS_HRL, true).

%% 版本记录（命令字 0xC1）
-record(version_info, {
    frame_length :: integer(),
    drone_model :: integer(),
    drone_id :: integer(),
    version_string :: binary(),
    crc :: integer()
}).

%% 航点信息记录（命令字 0xB4）
-record(waypoint_info, {
    latitude :: float(),
    longitude :: float(),
    altitude :: integer(),
    total_count :: integer(),
    waypoint_index :: integer(),
    crc :: integer()
}).

%% 舵面校准记录（命令字 0xDE）
-record(surface_calibration, {
    channel :: integer(),
    pwm_center :: float(),
    up_ratio :: float(),
    down_ratio :: float(),
    crc :: integer()
}).

%% 电池状态记录（对应 CAN 帧 0x000000A1）
-record(battery_status, {
    status_byte        :: integer(),          % 字节0：电池状态信息
    voltage            :: float(),             % 字节1-2：电压（0.01V）
    activate_state     :: integer(),           % 字节3：激活状态（0x1A/0xA1）
    temperature1       :: integer() | invalid, % 字节4：温度1（℃）
    temp2_or_count     :: integer(),           % 字节5：温度2 或 飞行次数
    sequence           :: integer(),           % 字节6：通信序列编号
    cmd_result         :: integer()            % 字节7：指令执行结果
}).

%% 链路状态记录（对应链路遥测帧）
-record(link_status, {
    network_access_flag :: integer(),          % 入网申请有效标志 (0xAA/0x00)
    node_address :: integer(),                 % 本节点地址
    granted_nodes_count :: integer(),          % 获得许可节点数
    denied_nodes_count :: integer(),           % 被拒绝节点数
    online_nodes :: list(integer()),           % 在线节点地址列表
    latest_denied_node :: integer(),           % 最近被拒绝节点
    crc1 :: integer(),                         % CRC1
    crc2 :: integer()                           % CRC2
}).

-endif.