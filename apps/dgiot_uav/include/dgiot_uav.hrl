%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav.hrl - 无人机测试系统头文件
%%%
%%% 包含 UAV 测试相关的记录定义：uav_test, uav_step, uav_indication,
%%% uav_power, uav_data, 以及载荷协议记录：fc_to_payload, aircraft_params,
%%% payload_to_fc_basic, payload_to_fc_extended, data_terminal_frame,
%%% composite_data, 和 UAV TCP 工作进程状态记录 uav_state。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(DGIOT_UAV_HRL).
-define(DGIOT_UAV_HRL, true).

%% DGIOT系统宏定义
-define(DGIOT_DATASOURCE, dgiot_datasource).
-define(DGIOT_SERVICE, dgiot_service).

%% 测试步骤定义
-define(TEST_STEPS, [
    {1, <<"备检并获取编码"/utf8>>},
    {2, <<"机身静态测试前检查"/utf8>>},
    {3, <<"机身及螺旋桨安装情况检查"/utf8>>},
    {4, <<"电压测量检查"/utf8>>},
    {5, <<"链路功能检查"/utf8>>},
    {6, <<"上电参数检查"/utf8>>},
    {7, <<"夜航灯测试"/utf8>>},
    {8, <<"气压高度检测"/utf8>>},
    {9, <<"系统电磁兼容性功能检查"/utf8>>},
    {10, <<"航线加载及载荷功能检查"/utf8>>}
]).

%% 电源状态
-define(POWER_OFF, off).
-define(POWER_ON, on).

%% 测试状态
-define(TEST_NOT_STARTED, not_started).
-define(TEST_RUNNING, running).
-define(TEST_COMPLETED, completed).
-define(TEST_FAILED, failed).

%% 测试结果
-define(RESULT_PASSED, passed).
-define(RESULT_FAILED, failed).
-define(RESULT_UNTESTED, untested).

%% 记录定义
-record(uav_test, {
    id :: binary(),
    device_id :: binary(),
    operator :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status = ?TEST_NOT_STARTED :: atom(),
    steps = [] :: list(),
    report_id :: binary() | undefined
}).

-record(uav_step, {
    id :: integer(),
    name :: binary(),
    indications = [] :: list(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    status = ?TEST_NOT_STARTED :: atom()
}).

-record(uav_indication, {
    id :: integer(),
    name :: binary(),
    description :: binary() | undefined,
    is_manual = false :: boolean(),
    refer_value :: number() | undefined,
    test_value :: number() | undefined,
    qualified :: boolean() | undefined,
    unit :: binary() | undefined
}).

-record(uav_power, {
    status = ?POWER_OFF :: atom(),
    voltage = 0 :: integer(),
    current = 0 :: integer(),
    max_voltage = 5000 :: integer(),
    max_current = 2000 :: integer()
}).

-record(uav_data, {
    timestamp :: integer(),
    pitch_angle = 0.0 :: float(),
    roll_angle = 0.0 :: float(),
    yaw_angle = 0.0 :: float(),
    altitude = 0.0 :: float(),
    voltage = 0.0 :: float(),
    current = 0.0 :: float(),
    temperature = 0.0 :: float(),
    gps_satellites = 0 :: integer(),
    signal_strength = 0 :: integer()
}).

%% C#数据模型对应的Erlang记录定义
%% 判据模型 (对应CriterionModel)
-record(uav_criterion, {
    id :: integer(),
    name :: binary(),
    expression :: binary(),
    min_value :: float() | undefined,
    max_value :: float() | undefined,
    unit :: binary() | undefined
}).

%% 测试分项模型 (对应SubItemModel)
-record(uav_sub_item, {
    id :: integer(),
    name :: binary(),
    is_enable = true :: boolean(),
    criterions = [] :: list(#uav_criterion{})
}).

%% 测试指标组模型 (对应IndicationGroupModel)
-record(uav_indication_group, {
    id :: integer(),
    name :: binary(),
    indication_ids = [] :: list(integer()),
    indications = [] :: list(#uav_indication{})
}).

%% 测试项目模型 (对应TestItemModel)
-record(uav_test_item, {
    id :: integer(),
    name :: binary(),
    sub_items = [] :: list(#uav_sub_item{}),
    indications = [] :: list(#uav_indication{}),
    indication_groups = [] :: list(#uav_indication_group{})
}).

%% 测试步骤模型 (对应TestStepModel)
-record(uav_test_step, {
    id :: integer(),
    title :: binary(),
    test_item_id :: integer() | undefined,
    control_type :: binary() | undefined,
    exec_func :: fun() | undefined,
    status = ?TEST_NOT_STARTED :: atom(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    indications = [] :: list(#uav_indication{})
}).

%% 测试配置数据 (对应ConfigData)
-record(uav_config, {
    test_items = [] :: list(#uav_test_item{}),
    test_steps = [] :: list(#uav_test_step{}),
    version :: binary(),
    last_modified :: integer()
}).

%% ===================================================================
%% 载荷协议记录定义（唯一版本）
%% ===================================================================

%% FC到Payload命令帧
-record(fc_to_payload, {
    command_code = 0 :: integer(),
    param_c = 0 :: integer(),
    param_d = 0 :: integer(),
    aircraft_params = <<0:256>> :: binary(),  % 32字节
    checksum = 0 :: integer()
}).

%% 飞机参数记录（用于解析aircraft_params）
-record(aircraft_params, {
    aircraft_id = 0 :: integer(),
    nav_status = #{} :: map(),
    latitude = 0.0 :: float(),
    longitude = 0.0 :: float(),
    altitude_gps = 0.0 :: float(),
    ground_speed = 0.0 :: float(),
    pitch = 0.0 :: float(),
    roll = 0.0 :: float(),
    heading_magnetic = 0.0 :: float(),
    airspeed = 0.0 :: float(),
    altitude_baro = 0.0 :: float(),
    track_angle = 0.0 :: float(),
    satellite_count = 0 :: integer()
}).

%% Payload到FC基本状态帧
-record(payload_to_fc_basic, {
    status0 = 0 :: integer(),
    status1 = 0 :: integer(),
    payload_type_zoom = 0 :: integer(),
    elevation_real = 0 :: integer(),
    azimuth_real = 0 :: integer(),
    target_offset_x = 0 :: integer(),
    target_offset_y = 0 :: integer(),
    elevation_target = 0 :: integer(),
    azimuth_target = 0 :: integer(),
    aircraft_pitch = 0 :: integer(),
    aircraft_roll = 0 :: integer(),
    aircraft_yaw = 0 :: integer(),
    tracking_flag = 0 :: integer(),
    frame_count = 0 :: integer(),
    status2 = 0 :: integer(),
    elevation_rate = 0 :: integer(),
    azimuth_rate = 0 :: integer(),
    reserved1 = <<0:136>> :: binary(),  % 17字节
    received_command = 0 :: integer(),
    crc16 = 0 :: integer()
}).

%% Payload到FC扩展状态帧（包含MEMS数据）
-record(payload_to_fc_extended, {
    status0 = 0 :: integer(),
    status1 = 0 :: integer(),
    payload_type_zoom = 0 :: integer(),
    elevation_real = 0 :: integer(),
    azimuth_real = 0 :: integer(),
    target_offset_x = 0 :: integer(),
    target_offset_y = 0 :: integer(),
    debug_data = <<0:32>> :: binary(),  % 4字节
    accel_x = 0 :: integer(),
    accel_y = 0 :: integer(),
    accel_z = 0 :: integer(),
    tracking_flag = 0 :: integer(),
    frame_count = 0 :: integer(),
    status2 = 0 :: integer(),
    gyro_x = 0 :: integer(),
    gyro_y = 0 :: integer(),
    gyro_z = 0 :: integer(),
    imu_temp = 0 :: integer(),
    reserved2 = <<0:80>> :: binary(),   % 10字节
    servo_timestamp = 0 :: integer(),
    image_timestamp = 0 :: integer(),
    received_command = 0 :: integer(),
    crc16 = 0 :: integer()
}).

%% 数据终端帧
-record(data_terminal_frame, {
    data_type = 0 :: integer(),
    frame_number = 0 :: integer(),
    payload_data = <<0:920>> :: binary()  % 115字节
}).

%% 复合数据帧（用于数据终端）
-record(composite_data, {
    payload_data = <<0:368>> :: binary(),  % 46字节
    aircraft_data = <<0:296>> :: binary(), % 37字节
    crc16 = 0 :: integer()
}).

%% ===================================================================
%% 新增：UAV TCP 工作进程状态记录
%% ===================================================================
%% 无人机状态记录
-record(uav_state, {
    id :: binary(),
    port :: integer(),
    product_id :: binary(),
    devaddr :: binary(),
    ip_bin :: binary(),
    device_id :: binary(),
    protocol_state :: term(),
    drone_powered :: boolean(),
    station_addr :: integer() | undefined,
    timer_ref :: reference() | undefined,   % 舵面定时器引用
    retry_count :: integer()                % 失败重试计数器
}).

%% ===================================================================
%% 告警地址段映射定义
%% ===================================================================

%% 告警地址段范围定义
%% 格式: {工位ID, 地址段起始, 地址段结束, PLC设备名称, 工位名称}
-define(ALARM_ADDRESS_RANGES, [
    %% 测试线PLC工位 (工位1)
    {1, 1130, 1189, <<"测试线PLC"/utf8>>, <<"测试线PLC工位"/utf8>>},
    
    %% 磁航向工位 (工位2)
    {2, 1730, 1789, <<"磁航向PLC"/utf8>>, <<"磁航向工位"/utf8>>},
    
    %% 机器人1工位 (工位3)
    {3, 1530, 1589, <<"机器人1"/utf8>>, <<"机器人1工位"/utf8>>},
    
    %% 机器人2工位 (工位4)
    {4, 1630, 1689, <<"机器人2"/utf8>>, <<"机器人2工位"/utf8>>},
    
    %% 留待扩展的地址段
    %% 工位5: 1800-1899
    %% 工位6: 1900-1999
    %% 工位7: 2000-2099
    %% 工位8: 2100-2199
    %% 虚拟工位使用特殊地址段
    {10, 9990, 9999, <<"虚拟告警检测"/utf8>>, <<"虚拟告警检测工位"/utf8>>},
    {11, 9998, 9999, <<"虚拟心跳检测"/utf8>>, <<"虚拟心跳检测工位"/utf8>>}
]).

%% 地址段映射记录
-record(address_range_mapping, {
    station_id :: integer(),           % 工位ID
    range_start :: integer(),          % 地址段起始
    range_end :: integer(),            % 地址段结束
    device_name :: binary(),           % PLC设备名称
    station_name :: binary(),          % 工位名称
    description :: binary()            % 地址段描述
}).

-endif. % DGIOT_UAV_HRL