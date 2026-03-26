%%%-------------------------------------------------------------------
%%% @doc
%%% d1_data.hrl - D1 遥测数据状态记录定义
%%%
%%% 包含飞行模式、攻击模式、故障状态等宏定义，
%%% 以及 drone_status_d1 记录，对应协议 D1.docx 中的所有字段。
%%%
%%% 修改：增加电池状态、温度1、温度2字段，添加缺失的 reserved3 字段
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(D1_DATA_HRL).
-define(D1_DATA_HRL, true).

%% 飞行模式宏定义
-define(FLIGHT_MODE_ALTITUDE_HOLD, 16#12).
-define(FLIGHT_MODE_RETURN_HOME,   16#13).
-define(FLIGHT_MODE_CIRCLE,        16#14).
-define(FLIGHT_MODE_NAVIGATION,    16#15).
-define(FLIGHT_MODE_TAKEOFF,       16#16).
-define(FLIGHT_MODE_LANDING,       16#17).
-define(FLIGHT_MODE_GO_AROUND,     16#19).
-define(FLIGHT_MODE_ATTACK,        16#1B).
-define(FLIGHT_MODE_BARREL_ROLL,   16#1C).

%% 攻击模式宏定义
-define(ATTACK_MODE_IMAGE_GUIDANCE,  1).
-define(ATTACK_MODE_POSITION_GUIDANCE, 2).

%% 电池加热标志
-define(BATTERY_HEATING_NORMAL, 0).
-define(BATTERY_HEATING_HEATING, 1).

%% 复位类型
-define(RESET_TYPE_POWER_ON,   0).
-define(RESET_TYPE_WATCHDOG,   1).

%% 起爆供电状态
-define(DETONATION_POWER_OFF, 0).
-define(DETONATION_POWER_ON,  1).

%% 数据装订指令回报
-define(DATA_BINDING_PAYLOAD_CONTROL, 16#FD).

%% 飞控开关指令回报
-define(FLIGHT_CONTROL_SWITCH_GO_AROUND, 16#B9).

%% 故障状态位宏（按备注1）
-define(FAULT_REMOTE_LINK_BIT,      0).
-define(FAULT_GPS_POSITION_BIT,     1).
-define(FAULT_IMU_POSITION_BIT,     2).
-define(FAULT_AIRSPEED_BIT,         3).
-define(FAULT_BARO_ALTITUDE_BIT,    4).
-define(FAULT_RESERVED5_BIT,        5).
-define(FAULT_PAYLOAD_BIT,          6).
-define(FAULT_BEIDOU_BIT,           7).
-define(FAULT_ROTATION_SPEED_BIT,   8).
-define(FAULT_RESERVED9_BIT,        9).
-define(FAULT_FERROELECTRIC_BIT,    10).
-define(FAULT_BATTERY_VOLTAGE_BIT,  11).
-define(FAULT_WARHEAD_BIT,          12).
-define(FAULT_LAUNCH_TUBE_BIT,      13).

%% 警告标识位宏（按备注2）
-define(WARNING_RECOVERY_ALERT_BIT,             0).
-define(WARNING_EMERGENCY_DATA_UNBOUND_BIT,      1).
-define(WARNING_GROUND_STATION_POS_UNBOUND_BIT,  2).
-define(WARNING_CRUISE_ROUTE_UNBOUND_BIT,        3).
-define(WARNING_LINK_FAILURE_MODE_UNBOUND_BIT,   4).
-define(WARNING_ET_ROUTE_UNBOUND_BIT,            5).

%% 铁电故障位宏（完整定义）
-define(FERROELECTRIC_FAULT_BIT_0,  1 bsl 0).
-define(FERROELECTRIC_FAULT_BIT_1,  1 bsl 1).
-define(FERROELECTRIC_FAULT_BIT_2,  1 bsl 2).
-define(FERROELECTRIC_FAULT_BIT_3,  1 bsl 3).
-define(FERROELECTRIC_FAULT_BIT_4,  1 bsl 4).
-define(FERROELECTRIC_FAULT_BIT_5,  1 bsl 5).
-define(FERROELECTRIC_FAULT_BIT_6,  1 bsl 6).
-define(FERROELECTRIC_FAULT_BIT_7,  1 bsl 7).
-define(FERROELECTRIC_FAULT_BIT_8,  1 bsl 8).
-define(FERROELECTRIC_FAULT_BIT_9,  1 bsl 9).
-define(FERROELECTRIC_FAULT_BIT_10, 1 bsl 10).
-define(FERROELECTRIC_FAULT_BIT_11, 1 bsl 11).
-define(FERROELECTRIC_FAULT_BIT_12, 1 bsl 12).
-define(FERROELECTRIC_FAULT_BIT_13, 1 bsl 13).
-define(FERROELECTRIC_FAULT_BIT_14, 1 bsl 14).
-define(FERROELECTRIC_FAULT_BIT_15, 1 bsl 15).

%% D1 状态记录
-record(drone_status_d1, {
    latitude                :: float(),
    longitude               :: float(),
    heading                 :: float(),
    pitch                   :: float(),
    roll                    :: float(),
    relative_altitude       :: float(),
    gps_altitude            :: float(),
    baro_altitude           :: float(),
    airspeed                :: float(),
    east_velocity           :: float(),
    north_velocity          :: float(),
    elevator_angle          :: float(),
    rudder_angle            :: float(),
    aileron_angle           :: float(),
    throttle_angle          :: float(),
    flight_time             :: integer(),
    battery_heating_flag    :: integer(),
    reset_type              :: integer(),
    reset_count             :: integer(),
    detonation_power_status :: integer(),
    target_heading          :: float(),
    target_waypoint         :: integer(),
    battery_voltage         :: float(),
    climb_rate              :: float(),
    data_binding_executed   :: integer(),
    flight_mode             :: integer(),
    year                    :: integer(),
    month                   :: integer(),
    day                     :: integer(),
    hour                    :: integer(),
    minute                  :: integer(),
    second                  :: integer(),
    gps_horizontal_accuracy :: float(),
    flight_control_switch_command :: integer(),
    gps_satellite_count     :: integer(),
    fault_status            :: integer(),
    warning_flag            :: integer(),
    ferroelectric_fault     :: integer(),
    reserved1               :: binary(),
    reserved2               :: binary(),
    reserved3               :: binary(),   % 新增：对应 parse 中的 Reserved3
    attack_mode             :: integer(),
    soft_command_overload   :: integer(),
    soft_command_path_inclination :: float(),
    path_inclination        :: float(),
    soft_command_path_deflection  :: float(),
    combined_north_velocity :: float(),
    combined_east_velocity  :: float(),
    combined_vertical_velocity :: float(),
    gps_position_flag       :: integer(),
    longitudinal_miss_distance :: float(),
    lateral_miss_distance   :: float(),
    %% 新增电池字段
    battery_status          :: integer(),   % 字节124：电池状态
    battery_temp1           :: integer(),   % 字节125：一次电池温度1
    battery_temp2           :: integer(),   % 字节126：一次电池温度2
    crc                     :: integer()
}).

-endif.