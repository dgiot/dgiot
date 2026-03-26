%%%-------------------------------------------------------------------
%%% @doc
%%% uav_thing_model_d3 - D3帧物模型转换模块
%%% 将 D3 状态记录转换为物模型 Map（展开数据选择、战斗部状态、开关状态）
%%% @end
%%%-------------------------------------------------------------------
-module(uav_thing_model_d3).

-export([convert/1]).

-include("d3_data.hrl").
-include_lib("dgiot/include/logger.hrl").

%% @doc 将 D3 状态记录转换为物模型 Map
-spec convert(#drone_status_d3{}) -> map().
convert(#drone_status_d3{
    ground_speed_direction = GroundSpeedDirection,
    beidou_snr_gt46_count = BeidouSnrGt46Count,
    beidou_snr_gt44_count = BeidouSnrGt44Count,
    beidou_snr_gt42_count = BeidouSnrGt42Count,
    beidou_snr_gt40_count = BeidouSnrGt40Count,
    beidou_snr_gt38_count = BeidouSnrGt38Count,
    beidou_snr_gt35_count = BeidouSnrGt35Count,
    gps_snr_gt46_count = GpsSnrGt46Count,
    gps_snr_gt44_count = GpsSnrGt44Count,
    gps_snr_gt42_count = GpsSnrGt42Count,
    gps_snr_gt40_count = GpsSnrGt40Count,
    gps_snr_gt38_count = GpsSnrGt38Count,
    gps_snr_gt35_count = GpsSnrGt35Count,
    magnetic_error_x = MagneticErrorX,
    magnetic_error_y = MagneticErrorY,
    magnetic_error_z = MagneticErrorZ,
    magnetic_value_x = MagneticValueX,
    magnetic_value_y = MagneticValueY,
    magnetic_value_z = MagneticValueZ,
    beidou_self_destruct_status = BeidouSelfDestructStatus,
    data_select_flag = DataSelectFlag,
    beidou_pdop = BeidouPdop,
    main_loop_time = MainLoopTime,
    gps_altitude = GPSAltitude,
    gps_latitude = GPSLatitude,
    gps_longitude = GPSLongitude,
    beidou_altitude = BeidouAltitude,
    beidou_latitude = BeidouLatitude,
    beidou_longitude = BeidouLongitude,
    gps_satellite_count = GPSSatelliteCount,
    beidou_satellite_count = BeidouSatelliteCount,
    payload_switch_command = PayloadSwitchCommand,
    warhead_switch_command = WarheadSwitchCommand,
    launch_tube_command = LaunchTubeCommand,
    gps_pdop = GPSPdop,
    magnetic_heading = MagneticHeading,
    magnetic_calibration_status = MagneticCalibrationStatus,
    launch_tube_switch_command = LaunchTubeSwitchCommand,
    seeker_elevation_angle = SeekerElevationAngle,
    seeker_azimuth_angle = SeekerAzimuthAngle,
    seeker_elevation_rate = SeekerElevationRate,
    seeker_azimuth_rate = SeekerAzimuthRate,
    line_of_sight_elevation = LineOfSightElevation,
    line_of_sight_azimuth = LineOfSightAzimuth,
    flight_control_temp1 = FlightControlTemp1,
    flight_control_temp2 = FlightControlTemp2,
    warhead_status0 = WarheadStatus0,
    warhead_status1 = WarheadStatus1,
    warhead_acceleration = WarheadAcceleration,
    laser_range_value = LaserRangeValue,
    touch_detonation_voltage = TouchDetonationVoltage,
    launch_tube_status = LaunchTubeStatus,
    launch_tube_ignition_voltage = LaunchTubeIgnitionVoltage,
    warhead_voltage = WarheadVoltage,
    payload_voltage = PayloadVoltage,
    night_flight_voltage = NightFlightVoltage,
    power_5v2 = Power5V2,
    power_5v0 = Power5V0,
    power_8v4_1 = Power8V4_1,
    power_8v4_2 = Power8V4_2,
    hard_switch_voltage = HardSwitchVoltage,
    switch_status = SwitchStatus,
    fuze_charging_voltage = FuzeChargingVoltage,
    guidance_stabilization_coef = GuidanceStabilizationCoef,
    wind_speed1 = WindSpeed1,
    wind_direction1 = WindDirection1,
    wind_speed2 = WindSpeed2,
    wind_direction2 = WindDirection2,
    payload_electronic_zoom = PayloadElectronicZoom,
    softened_payload_tracking_flag = SoftenedPayloadTrackingFlag,
    payload_tracking_flag = PayloadTrackingFlag,
    drone_type = DroneType,
    sight_azimuth_heading_deviation = SightAzimuthHeadingDeviation
}) ->
    %% 解析数据选择标识（字节43）
    SnrSource      = DataSelectFlag band 1,
    PosSource      = (DataSelectFlag bsr 1) band 1,
    MagType        = (DataSelectFlag bsr 2) band 1,

    %% 解析战斗部状态字0（备注6）
    WarheadSelfDestruct   = (WarheadStatus0 bsr 7) band 1,
    WarheadAttackMode     = (WarheadStatus0 bsr 6) band 1,
    WarheadCapCharged     = (WarheadStatus0 bsr 5) band 1,
    WarheadSecondSafety   = (WarheadStatus0 bsr 4) band 1,
    WarheadFirstSafety    = (WarheadStatus0 bsr 3) band 1,
    _ReservedW0           = (WarheadStatus0 bsr 2) band 1,
    WarheadSelfTest       = (WarheadStatus0 bsr 1) band 1,
    WarheadDeviceNormal   = WarheadStatus0 band 1,

    %% 解析战斗部状态字1（备注7）
    _ReservedW7           = (WarheadStatus1 bsr 7) band 1,
    WingDeployed          = (WarheadStatus1 bsr 6) band 1,
    IsolationStatus       = (WarheadStatus1 bsr 5) band 1,
    _ReservedW4_2         = (WarheadStatus1 bsr 2) band 16#07,
    DetonationFlag        = (WarheadStatus1 bsr 1) band 1,
    ConductiveMembraneValid = WarheadStatus1 band 1,

    %% 解析开关状态（字节98）
    SoftSwitch1      = (SwitchStatus bsr 0) band 1,
    SoftSwitch2      = (SwitchStatus bsr 1) band 1,
    LeftWingSwitch   = (SwitchStatus bsr 2) band 1,
    RightWingSwitch  = (SwitchStatus bsr 3) band 1,
    HardSwitchMeasure= (SwitchStatus bsr 4) band 1,

    %% 综合警告标志：基于战斗部状态字0和1的关键警告位
    WarningFlag = case (WarheadSelfDestruct =:= 1) orelse
                         (DetonationFlag =:= 1) orelse
                         (WarheadDeviceNormal =:= 0) of
                     true -> 1;
                     false -> 0
                 end,

    %% 航姿角映射（根据用户要求添加中文含义字段）
    %% 航姿俯仰角 -> seeker_elevation_angle (导引头俯仰角)
    %% 航姿转角角 -> line_of_sight_elevation (视线俯仰角)，也可能是滚转角，但D3协议中无直接滚转字段
    %% 航姿航向角 -> magnetic_heading (磁航向)
    AttitudePitchAngle = SeekerElevationAngle,        % 航姿俯仰角
    AttitudeRollAngle = LineOfSightElevation,         % 航姿转角角（使用视线俯仰角作为滚转角近似）
    AttitudeHeadingAngle = MagneticHeading,           % 航姿航向角

    %% 引信开关指令：使用warhead_switch_command作为引信开关指令（根据协议字节60）
    FuzeSwitchCommand = WarheadSwitchCommand,

    %% 相对高度字段：目前协议中只有绝对高度，相对高度暂设为0
    GPSRelativeAltitude = 0.0,
    BeidouRelativeAltitude = 0.0,

    #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"ground_speed_direction">> => GroundSpeedDirection,
        %% 北斗信噪比
        <<"beidou_snr_gt46_count">> => BeidouSnrGt46Count,
        <<"beidou_snr_gt44_count">> => BeidouSnrGt44Count,
        <<"beidou_snr_gt42_count">> => BeidouSnrGt42Count,
        <<"beidou_snr_gt40_count">> => BeidouSnrGt40Count,
        <<"beidou_snr_gt38_count">> => BeidouSnrGt38Count,
        <<"beidou_snr_gt35_count">> => BeidouSnrGt35Count,
        %% 卫导信噪比
        <<"gps_snr_gt46_count">> => GpsSnrGt46Count,
        <<"gps_snr_gt44_count">> => GpsSnrGt44Count,
        <<"gps_snr_gt42_count">> => GpsSnrGt42Count,
        <<"gps_snr_gt40_count">> => GpsSnrGt40Count,
        <<"gps_snr_gt38_count">> => GpsSnrGt38Count,
        <<"gps_snr_gt35_count">> => GpsSnrGt35Count,
        %% 磁强误差/值（两套独立存储）
        <<"magnetic_error_x">> => MagneticErrorX,
        <<"magnetic_error_y">> => MagneticErrorY,
        <<"magnetic_error_z">> => MagneticErrorZ,
        <<"magnetic_value_x">> => MagneticValueX,
        <<"magnetic_value_y">> => MagneticValueY,
        <<"magnetic_value_z">> => MagneticValueZ,
        %% 系统状态
        <<"beidou_self_destruct_status">> => BeidouSelfDestructStatus,
        <<"snr_source">> => SnrSource,
        <<"position_source">> => PosSource,
        <<"magnetic_type">> => MagType,
        <<"beidou_pdop">> => BeidouPdop,
        <<"main_loop_time">> => MainLoopTime,
        %% 位置信息（GPS和北斗已拆分）
        <<"gps_altitude">> => GPSAltitude,
        <<"gps_latitude">> => GPSLatitude,
        <<"gps_longitude">> => GPSLongitude,
        <<"beidou_altitude">> => BeidouAltitude,
        <<"beidou_latitude">> => BeidouLatitude,
        <<"beidou_longitude">> => BeidouLongitude,
        %% 相对高度信息（新增字段）
        <<"gps_relative_altitude">> => GPSRelativeAltitude,
        <<"beidou_relative_altitude">> => BeidouRelativeAltitude,
        %% 卫星信息
        <<"gps_satellite_count">> => GPSSatelliteCount,
        <<"beidou_satellite_count">> => BeidouSatelliteCount,
        %% 开关指令
        <<"payload_switch_command">> => PayloadSwitchCommand,
        <<"warhead_switch_command">> => WarheadSwitchCommand,
        <<"launch_tube_command">> => LaunchTubeCommand,
        <<"fuze_switch_command">> => FuzeSwitchCommand,  % 新增：引信开关指令
        <<"gps_pdop">> => GPSPdop,
        <<"magnetic_heading">> => MagneticHeading,
        <<"magnetic_calibration_status">> => MagneticCalibrationStatus,
        <<"launch_tube_switch_command">> => LaunchTubeSwitchCommand,
        %% 导引头信息
        <<"seeker_elevation_angle">> => SeekerElevationAngle,
        <<"seeker_azimuth_angle">> => SeekerAzimuthAngle,
        <<"seeker_elevation_rate">> => SeekerElevationRate,
        <<"seeker_azimuth_rate">> => SeekerAzimuthRate,
        <<"line_of_sight_elevation">> => LineOfSightElevation,
        <<"line_of_sight_azimuth">> => LineOfSightAzimuth,
        %% 航姿角信息（新增中文含义字段）
        <<"attitude_pitch_angle">> => AttitudePitchAngle,      % 航姿俯仰角
        <<"attitude_roll_angle">> => AttitudeRollAngle,        % 航姿转角角
        <<"attitude_heading_angle">> => AttitudeHeadingAngle,  % 航姿航向角
        %% 温度信息
        <<"flight_control_temp1">> => FlightControlTemp1,
        <<"flight_control_temp2">> => FlightControlTemp2,
        %% 警告标志（新增综合警告字段）
        <<"warning_flag">> => WarningFlag,                      % 综合警告标志
        %% 战斗部状态展开
        <<"warhead_self_destruct">> => WarheadSelfDestruct,
        <<"warhead_attack_mode">> => WarheadAttackMode,
        <<"warhead_capacitor_charged">> => WarheadCapCharged,
        <<"warhead_second_safety_released">> => WarheadSecondSafety,
        <<"warhead_first_safety_released">> => WarheadFirstSafety,
        <<"warhead_self_test_complete">> => WarheadSelfTest,
        <<"warhead_device_normal">> => WarheadDeviceNormal,
        <<"wing_deployed">> => WingDeployed,
        <<"isolation_status">> => IsolationStatus,
        <<"detonation_flag">> => DetonationFlag,
        <<"conductive_membrane_valid">> => ConductiveMembraneValid,
        <<"warhead_acceleration">> => WarheadAcceleration,
        <<"laser_range_value">> => LaserRangeValue,
        <<"touch_detonation_voltage">> => TouchDetonationVoltage,
        %% 发射筒状态
        <<"launch_tube_status">> => LaunchTubeStatus,
        <<"launch_tube_ignition_voltage">> => LaunchTubeIgnitionVoltage,
        %% 电压信息
        <<"warhead_voltage">> => WarheadVoltage,
        <<"payload_voltage">> => PayloadVoltage,
        <<"night_flight_voltage">> => NightFlightVoltage,
        <<"power_5v2">> => Power5V2,
        <<"power_5v0">> => Power5V0,
        <<"power_8v4_1">> => Power8V4_1,
        <<"power_8v4_2">> => Power8V4_2,
        <<"hard_switch_voltage">> => HardSwitchVoltage,
        %% 开关状态展开
        <<"soft_switch1">> => SoftSwitch1,
        <<"soft_switch2">> => SoftSwitch2,
        <<"left_wing_switch">> => LeftWingSwitch,
        <<"right_wing_switch">> => RightWingSwitch,
        <<"hard_switch_measure">> => HardSwitchMeasure,
        <<"fuze_charging_voltage">> => FuzeChargingVoltage,
        <<"guidance_stabilization_coef">> => GuidanceStabilizationCoef,
        <<"wind_speed1">> => WindSpeed1,
        <<"wind_direction1">> => WindDirection1,
        <<"wind_speed2">> => WindSpeed2,
        <<"wind_direction2">> => WindDirection2,
        %% 载荷信息
        <<"payload_electronic_zoom">> => PayloadElectronicZoom,
        <<"softened_payload_tracking_flag">> => SoftenedPayloadTrackingFlag,
        <<"payload_tracking_flag">> => PayloadTrackingFlag,
        <<"drone_type">> => DroneType,
        <<"sight_azimuth_heading_deviation">> => SightAzimuthHeadingDeviation
    }.