%%%-------------------------------------------------------------------
%%% @doc
%%% d2_data.hrl - D2遥测数据解析模块头文件（修正版）
%%%
%%% 包含北斗状态、飞行模式位定义，
%%% 以及 drone_status_d2 记录，对应协议 D2.docx 中的所有字段。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(D2_DATA_HRL).
-define(D2_DATA_HRL, true).

%% 北斗状态位定义
-define(BEIDOU_POSITION_VALID, 16#04).  %% 位置有效标志位
-define(BEIDOU_CODE_TYPE_MASK, 16#03).  %% 码型掩码

%% 飞行模式位定义
-define(FLIGHT_MODE_BIT_1, 16#0001).    %% 模式1
-define(FLIGHT_MODE_BIT_2, 16#0002).    %% 模式2
-define(FLIGHT_MODE_BIT_3, 16#0004).    %% 模式3
-define(FLIGHT_MODE_BIT_4, 16#0008).    %% 模式4
-define(FLIGHT_MODE_BIT_5, 16#0010).    %% 模式5
-define(FLIGHT_MODE_BIT_6, 16#0020).    %% 模式6
-define(FLIGHT_MODE_BIT_7, 16#0040).    %% 模式7
-define(FLIGHT_MODE_BIT_8, 16#0080).    %% 模式8
-define(FLIGHT_MODE_BIT_9, 16#0100).    %% 模式9
-define(FLIGHT_MODE_BIT_10, 16#0200).   %% 模式10
-define(FLIGHT_MODE_BIT_11, 16#0400).   %% 模式11
-define(FLIGHT_MODE_BIT_12, 16#0800).   %% 模式12
-define(FLIGHT_MODE_BIT_13, 16#1000).   %% 模式13
-define(FLIGHT_MODE_BIT_14, 16#2000).   %% 模式14
-define(FLIGHT_MODE_BIT_15, 16#4000).   %% 模式15
-define(FLIGHT_MODE_BIT_16, 16#8000).   %% 模式16

%% 定义drone_status_d2记录（修正版）
-record(drone_status_d2, {
    frame_length = 0 :: integer(),           %% 帧长度
    drone_model = 0 :: integer(),            %% 无人机型号
    drone_id = 0 :: integer(),               %% 无人机ID
    command_id = 0 :: integer(),             %% 命令字
    
    warhead_frame_freq = 0 :: integer(),     %% 弹头帧频率
    roll_angle_given = 0.0 :: float(),       %% 横滚角指令
    pitch_angle_given = 0.0 :: float(),      %% 俯仰角指令
    airspeed_given = 0 :: integer(),         %% 空速指令
    onboard_nav_status = 0 :: integer(),     %% 机载导航状态
    
    origin_distance = 0 :: integer(),        %% 原点距离
    origin_azimuth = 0.0 :: float(),         %% 原点方位角
    distance_to_go = 0 :: integer(),         %% 剩余距离
    lateral_deviation = 0.0 :: float(),      %% 横向偏差
    command_altitude = 0.0 :: float(),       %% 指令高度
    
    rotation_speed = 0 :: integer(),         %% 转速
    total_flight_time = 0 :: integer(),      %% 总飞行时间
    flight_sortie = 0 :: integer(),          %% 架次
    remote_frame_freq = 0 :: integer(),      %% 遥控帧频率
    launch_tube_frame_freq = 0 :: integer(), %% 发射管帧频率
    payload_frame_freq = 0 :: integer(),     %% 载荷帧频率
    magnetic_heading_frame_freq = 0 :: integer(), %% 磁航向帧频率
    gps_frame_freq = 0 :: integer(),         %% GPS帧频率
    beidou_frame_freq = 0 :: integer(),      %% 北斗帧频率
    
    gps_relative_altitude = 0.0 :: float(),  %% GPS相对高度
    baro_relative_altitude = 0.0 :: float(), %% 气压相对高度
    
    flight_mode_bits = 0 :: integer(),       %% 飞行模式位
    beidou_status = 0 :: integer(),          %% 北斗状态
    launch_flag = 0 :: integer(),            %% 发射标志
    pull_up_height = 0 :: integer(),         %% 拉起高度
    
    angular_rate_x = 0.0 :: float(),         %% X轴角速率
    angular_rate_y = 0.0 :: float(),         %% Y轴角速率
    angular_rate_z = 0.0 :: float(),         %% Z轴角速率
    acceleration_x = 0.0 :: float(),         %% X轴加速度
    acceleration_y = 0.0 :: float(),         %% Y轴加速度
    acceleration_z = 0.0 :: float(),         %% Z轴加速度
    
    pitch_calibration = 0.0 :: float(),      %% 俯仰校准
    roll_calibration = 0.0 :: float(),       %% 横滚校准
    heading_calibration = 0.0 :: float(),    %% 航向校准
    elevator_calibration = 0.0 :: float(),   %% 升降舵校准
    aileron_calibration = 0.0 :: float(),    %% 副翼校准
    rudder_calibration = 0.0 :: float(),     %% 方向舵校准
    
    airspeed_calibration_coef = 0.0 :: float(),   %% 空速校准系数
    airspeed_calibration_offset = 0.0 :: float(), %% 空速校准偏移
    airspeed_zero_offset = 0.0 :: float(),        %% 空速零偏
    
    pitch_integral = 0.0 :: float(),         %% 俯仰积分
    altitude_integral = 0.0 :: float(),      %% 高度积分
    glide_altitude_integral = 0.0 :: float(), %% 滑翔高度积分
    airspeed_to_throttle_integral = 0.0 :: float(), %% 空速-油门积分
    roll_integral = 0.0 :: float(),          %% 横滚积分
    acceleration_integral = 0.0 :: float(),   %% 加速度积分
    airspeed_to_pitch_integral = 0.0 :: float(), %% 空速-俯仰积分
    
    lateral_deviation_correction = 0 :: integer(), %% 横向偏差修正
    reserved1 = 0 :: integer(),              %% 保留字段1
    
    payload_status0 = 0 :: integer(),        %% 载荷状态0
    payload_status1 = 0 :: integer(),        %% 载荷状态1
    payload_target_relative_height = 0 :: integer(), %% 载荷目标相对高度
    target_relative_height_flag = 0 :: integer(), %% 目标相对高度标志
    
    reserved2 = <<>> :: binary(),            %% 保留字段2（修正：字节94-101）
    control_surface_status = 0 :: integer(), %% 控制面状态
    reserved3 = <<>> :: binary(),            %% 保留字段3（修正：字节103-110）
    reserved4 = <<>> :: binary(),            %% 保留字段4（修正：字节111-112）
    crc = 0 :: integer()                     %% CRC校验
    }
).
-endif.
