%%%-------------------------------------------------------------------
%%% @doc
%%% d3_data.hrl - D3遥测数据解析器头文件（拆分版本）
%%%
%%% 包含 drone_status_d3 记录，对应协议 D3.docx 中的所有字段。
%%% 包括地速方向、信噪比、磁强数据、位置信息、导引头角度、
%%% 战斗部状态、电压、风速、载荷信息等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(D3_DATA_HRL).
-define(D3_DATA_HRL, true).

%% D3 状态记录
-record(drone_status_d3, {
    %% 帧头信息（由parse_complete函数添加）
    frame_header = <<>>,                 % 帧头 A5 5A
    frame_length = 0,                    %% 帧长度
    drone_model = 0,                     %% 无人机型号
    drone_id = 0,                         %% 无人机ID
    command_id = 0,                       %% 命令字
    
    %% 地速方向
    ground_speed_direction = 0.0,        %% 地速方向 (0.1°)
    reserved1 = <<>>,                     %% 预留1 (6字节)
    
    %% 信噪比统计（北斗）
    beidou_snr_gt46_count = 0,            %% 北斗信噪比>46的卫星数
    beidou_snr_gt44_count = 0,            %% 北斗信噪比>44的卫星数
    beidou_snr_gt42_count = 0,            %% 北斗信噪比>42的卫星数
    beidou_snr_gt40_count = 0,            %% 北斗信噪比>40的卫星数
    beidou_snr_gt38_count = 0,            %% 北斗信噪比>38的卫星数
    beidou_snr_gt35_count = 0,            %% 北斗信噪比>35的卫星数
    
    %% 信噪比统计（卫导/GPS）
    gps_snr_gt46_count = 0,               %% GPS信噪比>46的卫星数
    gps_snr_gt44_count = 0,               %% GPS信噪比>44的卫星数
    gps_snr_gt42_count = 0,               %% GPS信噪比>42的卫星数
    gps_snr_gt40_count = 0,               %% GPS信噪比>40的卫星数
    gps_snr_gt38_count = 0,               %% GPS信噪比>38的卫星数
    gps_snr_gt35_count = 0,               %% GPS信噪比>35的卫星数
    
    %% 磁力计数据（根据选择标识分为误差或值）
    magnetic_error_x = 0.0,               %% X轴磁强误差 (0.1)
    magnetic_error_y = 0.0,               %% Y轴磁强误差 (0.1)
    magnetic_error_z = 0.0,               %% Z轴磁强误差 (0.1)
    magnetic_value_x = 0.0,               %% X轴磁强值 (0.1)
    magnetic_value_y = 0.0,               %% Y轴磁强值 (0.1)
    magnetic_value_z = 0.0,               %% Z轴磁强值 (0.1)
    
    %% 系统状态
    reserved2 = 0,                        %% 预留2
    beidou_self_destruct_status = 0,      %% 北斗自毁状态
    data_select_flag = 0,                 %% 数据选择标志（字节43）
    reserved3 = 0,                        %% 预留3
    beidou_pdop = 0.0,                    %% 北斗PDOP值 (0.2)
    main_loop_time = 0.0,                 %% 主循环时间 (0.1ms)
    
    %% 位置信息（卫导/GPS）
    gps_altitude = 0.0,                   %% GPS高度 (H*0.1-500)
    gps_latitude = 0.0,                   %% GPS纬度 (10^7)
    gps_longitude = 0.0,                  %% GPS经度 (10^7)
    
    %% 位置信息（北斗）
    beidou_altitude = 0.0,                 %% 北斗高度 (H*0.1-500)
    beidou_latitude = 0.0,                 %% 北斗纬度 (10^7)
    beidou_longitude = 0.0,                %% 北斗经度 (10^7)
    
    %% 卫星信息
    gps_satellite_count = 0,               %% GPS解算星数
    beidou_satellite_count = 0,            %% 北斗解算星数
    
    %% 开关指令
    payload_switch_command = 0,            %% 载荷开关指令
    warhead_switch_command = 0,            %% 战斗部开关指令
    launch_tube_command = 0,               %% 发射筒指令
    
    %% GPS和磁力计信息
    gps_pdop = 0.0,                        %% GPS PDOP值 (0.2)
    magnetic_heading = 0.0,                %% 磁航向 (0.1°)
    magnetic_calibration_status = 0,       %% 磁力计校准状态
    launch_tube_switch_command = 0,        %% 发射筒开关指令
    
    %% 导引头信息
    seeker_elevation_angle = 0.0,          %% 导引头俯仰角 (0.1°)
    seeker_azimuth_angle = 0.0,            %% 导引头方位角 (0.1°)
    seeker_elevation_rate = 0.0,           %% 导引头俯仰角速率 (0.1°/s)
    seeker_azimuth_rate = 0.0,             %% 导引头方位角速率 (0.1°/s)
    line_of_sight_elevation = 0.0,         %% 视线俯仰角 (0.1°)
    line_of_sight_azimuth = 0.0,           %% 视线方位角 (0.1°)
    
    %% 温度信息
    flight_control_temp1 = 0.0,            %% 飞控温度1 (0.1°C)
    flight_control_temp2 = 0.0,            %% 飞控温度2 (0.1°C)
    
    %% 战斗部状态
    warhead_status0 = 0,                   %% 战斗部状态字0
    warhead_status1 = 0,                   %% 战斗部状态字1
    warhead_acceleration = 0,              %% 战斗部加速度 (2g)
    laser_range_value = 0.0,               %% 激光测距值 (0.2m)
    touch_detonation_voltage = 0.0,        %% 碰炸电压 (0.1V)
    
    %% 发射筒状态
    launch_tube_status = 0,                %% 发射筒状态
    launch_tube_ignition_voltage = 0.0,    %% 发射筒点火电压 (V)
    
    %% 电压信息
    warhead_voltage = 0.0,                 %% 战斗部电压 (0.1V)
    payload_voltage = 0.0,                 %% 载荷电压 (0.1V)
    night_flight_voltage = 0.0,            %% 夜航电压 (0.1V)
    power_5v2 = 0.0,                       %% 5.2V电源 (0.1V)
    power_5v0 = 0.0,                       %% 5.0V电源 (0.1V)
    power_8v4_1 = 0.0,                     %% 8.4V电源1 (0.1V)
    power_8v4_2 = 0.0,                     %% 8.4V电源2 (0.1V)
    hard_switch_voltage = 0.0,             %% 硬开关电压 (0.1V)
    
    %% 开关状态
    switch_status = 0,                     %% 开关状态
    reserved4 = 0,                         %% 预留4
    
    %% 引信信息
    fuze_charging_voltage = 0,             %% 引信充电电压
    
    %% 预留字段
    reserved5 = <<>>,                       %% 预留5 (7字节)
    
    %% 导引和风速
    guidance_stabilization_coef = 0,       %% 导引稳定系数
    wind_speed1 = 0.0,                     %% 风速1 (0.2m/s)
    wind_direction1 = 0.0,                 %% 风向1 (0.1°/s)
    wind_speed2 = 0.0,                     %% 风速2 (0.2m/s)
    wind_direction2 = 0.0,                  %% 风向2 (0.1°/s)
    
    %% 预留字段
    reserved6 = <<>>,                       %% 预留6 (3字节)
    
    %% 载荷信息
    payload_electronic_zoom = 0,           %% 载荷电子变焦
    softened_payload_tracking_flag = 0.0,  %% 软化后的载荷跟踪标志 (0.01)
    payload_tracking_flag = 0,             %% 载荷跟踪标志
    drone_type = 0,                         %% 无人机类型
    sight_azimuth_heading_deviation = 0,   %% 视场方位角与航向偏差 (-180°~180°)
    
    %% 预留字段
    reserved7 = <<>>,                       %% 预留7 (2字节)
    
    %% CRC
    crc = 0                                 %% CRC校验
}).

%% 宏定义保持不变
-define(DRONE_TYPE_MISSION, 2).
-define(DRONE_TYPE_TRAINING, 4).

%% 数据选择标识定义
-define(DATA_SELECT_SNR_GPS, 0).
-define(DATA_SELECT_SNR_BEIDOU, 1).
-define(DATA_SELECT_POS_GPS, 0).
-define(DATA_SELECT_POS_BEIDOU, 1).
-define(DATA_SELECT_MAG_ERROR, 0).
-define(DATA_SELECT_MAG_VALUE, 1).

%% 北斗自毁状态定义
-define(BEIDOU_SELF_DESTRUCT_OFF, 0).
-define(BEIDOU_SELF_DESTRUCT_ON, 1).

%% 载荷开关指令定义
-define(PAYLOAD_SWITCH_CENTER, 16#45).

%% 引战开关指令定义
-define(WARHEAD_SWITCH_OFF, 0).
-define(WARHEAD_SWITCH_ON, 1).

%% 发射筒指令定义
-define(LAUNCH_TUBE_OFF, 0).
-define(LAUNCH_TUBE_ON, 1).

%% 磁航向标校状态定义
-define(MAGNETIC_CALIBRATION_OFF, 0).
-define(MAGNETIC_CALIBRATION_ON, 1).

%% 引战设备状态字0位定义
-define(WARHEAD_STATUS0_DEVICE_NORMAL, 16#01).
-define(WARHEAD_STATUS0_SELF_TEST_COMPLETE, 16#02).
-define(WARHEAD_STATUS0_FIRST_SAFETY_RELEASED, 16#08).
-define(WARHEAD_STATUS0_SECOND_SAFETY_RELEASED, 16#10).
-define(WARHEAD_STATUS0_CAPACITOR_CHARGED, 16#20).
-define(WARHEAD_STATUS0_ATTACK_MODE_ENTERED, 16#40).
-define(WARHEAD_STATUS0_SELF_DESTRUCT_MODE, 16#80).

%% 引战设备状态字1位定义
-define(WARHEAD_STATUS1_CONDUCTIVE_MEMBRANE_VALID, 16#01).
-define(WARHEAD_STATUS1_DETONATION_FLAG, 16#02).
-define(WARHEAD_STATUS1_ISOLATION_STATUS, 16#20).
-define(WARHEAD_STATUS1_WING_DEPLOYED, 16#40).

%% 开关状态位定义
-define(SWITCH_STATUS_SOFT_SWITCH1, 16#01).
-define(SWITCH_STATUS_SOFT_SWITCH2, 16#02).
-define(SWITCH_STATUS_LEFT_WING_SWITCH, 16#04).
-define(SWITCH_STATUS_RIGHT_WING_SWITCH, 16#08).
-define(SWITCH_STATUS_HARD_SWITCH_MEASUREMENT, 16#10).

%% 载荷电子变倍数定义
-define(PAYLOAD_ZOOM_1X, 1).
-define(PAYLOAD_ZOOM_2X, 2).
-define(PAYLOAD_ZOOM_3X, 3).
-define(PAYLOAD_ZOOM_4X, 4).
-define(PAYLOAD_ZOOM_5X, 5).

%% 载荷跟踪态标志定义
-define(PAYLOAD_TRACKING_OFF, 0).
-define(PAYLOAD_TRACKING_ON, 1).

%% 视线方位-航向偏差范围
-define(SIGHT_DEVIATION_MIN, -180).
-define(SIGHT_DEVIATION_MAX, 180).

%% D3帧头定义
-define(D3_FRAME_HEADER, 16#A55A).
-define(D3_COMMAND_ID, 16#D3).

-endif.