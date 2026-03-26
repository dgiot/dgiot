%%%-------------------------------------------------------------------
%%% @doc
%%% uav_thing_model_d1 - D1帧物模型转换模块
%%% 将 D1 状态记录转换为物模型 Map（展开故障和警告）
%%% @end
%%%-------------------------------------------------------------------
-module(uav_thing_model_d1).

-export([convert/1]).

-include("d1_data.hrl").
-include_lib("dgiot/include/logger.hrl").

%% @doc 将 D1 状态记录转换为物模型 Map
-spec convert(#drone_status_d1{}) -> map().
convert(#drone_status_d1{
    latitude = Latitude,
    longitude = Longitude,
    heading = Heading,
    pitch = Pitch,
    roll = Roll,
    relative_altitude = RelativeAltitude,
    gps_altitude = GPSAltitude,
    baro_altitude = BaroAltitude,
    airspeed = Airspeed,
    east_velocity = EastVelocity,
    north_velocity = NorthVelocity,
    elevator_angle = ElevatorAngle,
    rudder_angle = RudderAngle,
    aileron_angle = AileronAngle,
    throttle_angle = ThrottleAngle,
    flight_time = FlightTime,
    battery_heating_flag = BatteryHeatingFlag,
    reset_type = ResetType,
    reset_count = ResetCount,
    detonation_power_status = DetonationPowerStatus,
    target_heading = TargetHeading,
    target_waypoint = TargetWaypoint,
    battery_voltage = BatteryVoltage,
    climb_rate = ClimbRate,
    data_binding_executed = DataBindingExecuted,
    flight_mode = FlightMode,
    year = Year,
    month = Month,
    day = Day,
    hour = Hour,
    minute = Minute,
    second = Second,
    gps_horizontal_accuracy = GPSHorizontalAccuracy,
    flight_control_switch_command = FlightControlSwitchCommand,
    gps_satellite_count = GPSSatelliteCount,
    fault_status = FaultStatus,
    warning_flag = WarningFlag,
    ferroelectric_fault = FerroelectricFault,
    attack_mode = AttackMode,
    soft_command_overload = SoftCommandOverload,
    soft_command_path_inclination = SoftCommandPathInclination,
    path_inclination = PathInclination,
    soft_command_path_deflection = SoftCommandPathDeflection,
    combined_north_velocity = CombinedNorthVelocity,
    combined_east_velocity = CombinedEastVelocity,
    combined_vertical_velocity = CombinedVerticalVelocity,
    gps_position_flag = GPSPositionFlag,
    longitudinal_miss_distance = LongitudinalMissDistance,
    lateral_miss_distance = LateralMissDistance,
    battery_status = BatteryStatus,
    battery_temp1 = BatteryTemp1,
    battery_temp2 = BatteryTemp2
}) ->
    % 将 D1 中的年月日时分秒合并为一个 Unix 毫秒时间戳
    CreatedAt = case {Year, Month, Day, Hour, Minute, Second} of
        {0,0,0,0,0,0} -> erlang:system_time(millisecond);
        _ -> 
            try
                {{Y, M, D}, {H, Mn, S}} = {{Year, Month, Day}, {Hour, Minute, Second}},
                Seconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Mn, S}}) - 62167219200,
                Seconds * 1000
            catch
                _:_ -> erlang:system_time(millisecond)
            end
    end,

    %% 展开故障状态（32位）
    FaultRemoteLink   = (FaultStatus bsr 0) band 1,
    FaultGpsPosition   = (FaultStatus bsr 1) band 1,
    FaultImuPosition   = (FaultStatus bsr 2) band 1,
    FaultAirspeed      = (FaultStatus bsr 3) band 1,
    FaultBaroAltitude  = (FaultStatus bsr 4) band 1,
    _FaultReserved5    = (FaultStatus bsr 5) band 1,
    FaultPayload       = (FaultStatus bsr 6) band 1,
    FaultBeidou        = (FaultStatus bsr 7) band 1,
    FaultRotationSpeed = (FaultStatus bsr 8) band 1,
    _FaultReserved9    = (FaultStatus bsr 9) band 1,
    FaultFerroelectric = (FaultStatus bsr 10) band 1,
    FaultBatteryVoltage= (FaultStatus bsr 11) band 1,
    FaultWarhead       = (FaultStatus bsr 12) band 1,
    FaultLaunchTube    = (FaultStatus bsr 13) band 1,

    %% 展开警告标识（16位）
    WarningRecoveryAlert         = (WarningFlag bsr 0) band 1,
    WarningEmergencyDataUnbound  = (WarningFlag bsr 1) band 1,
    WarningGroundStationPosUnbound = (WarningFlag bsr 2) band 1,
    WarningCruiseRouteUnbound    = (WarningFlag bsr 3) band 1,
    WarningLinkFailureModeUnbound= (WarningFlag bsr 4) band 1,
    WarningEtRouteUnbound        = (WarningFlag bsr 5) band 1,

    %% 展开铁电故障（16位）
    FerroelectricFaultBit0  = (FerroelectricFault bsr 0) band 1,
    FerroelectricFaultBit1  = (FerroelectricFault bsr 1) band 1,
    FerroelectricFaultBit2  = (FerroelectricFault bsr 2) band 1,
    FerroelectricFaultBit3  = (FerroelectricFault bsr 3) band 1,
    FerroelectricFaultBit4  = (FerroelectricFault bsr 4) band 1,
    FerroelectricFaultBit5  = (FerroelectricFault bsr 5) band 1,
    FerroelectricFaultBit6  = (FerroelectricFault bsr 6) band 1,
    FerroelectricFaultBit7  = (FerroelectricFault bsr 7) band 1,
    FerroelectricFaultBit8  = (FerroelectricFault bsr 8) band 1,
    FerroelectricFaultBit9  = (FerroelectricFault bsr 9) band 1,
    FerroelectricFaultBit10 = (FerroelectricFault bsr 10) band 1,
    FerroelectricFaultBit11 = (FerroelectricFault bsr 11) band 1,
    FerroelectricFaultBit12 = (FerroelectricFault bsr 12) band 1,
    FerroelectricFaultBit13 = (FerroelectricFault bsr 13) band 1,
    FerroelectricFaultBit14 = (FerroelectricFault bsr 14) band 1,
    FerroelectricFaultBit15 = (FerroelectricFault bsr 15) band 1,

    %% 攻击模式文本
    AttackModeText = case AttackMode of
        ?ATTACK_MODE_IMAGE_GUIDANCE -> <<"图像制导"/utf8>>;
        ?ATTACK_MODE_POSITION_GUIDANCE -> <<"位置制导"/utf8>>;
        _ -> <<"未知攻击模式"/utf8>>
    end,

    #{
        <<"createdat">> => CreatedAt,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,
        <<"heading">> => Heading,
        <<"pitch">> => Pitch,
        <<"roll">> => Roll,
        <<"relative_altitude">> => RelativeAltitude,
        <<"gps_altitude">> => GPSAltitude,
        <<"baro_altitude">> => BaroAltitude,
        <<"airspeed">> => Airspeed,
        <<"east_velocity">> => EastVelocity,
        <<"north_velocity">> => NorthVelocity,
        <<"elevator_angle">> => ElevatorAngle,
        <<"rudder_angle">> => RudderAngle,
        <<"aileron_angle">> => AileronAngle,
        <<"throttle_angle">> => ThrottleAngle,
        <<"flight_time">> => FlightTime,
        <<"battery_heating_flag">> => BatteryHeatingFlag,
        <<"reset_type">> => ResetType,
        <<"reset_count">> => ResetCount,
        <<"detonation_power_status">> => DetonationPowerStatus,
        <<"target_heading">> => TargetHeading,
        <<"target_waypoint">> => TargetWaypoint,
        <<"battery_voltage">> => BatteryVoltage,
        <<"climb_rate">> => ClimbRate,
        <<"data_binding_executed">> => DataBindingExecuted,
        <<"flight_mode">> => FlightMode,
        <<"gps_horizontal_accuracy">> => GPSHorizontalAccuracy,
        <<"flight_control_switch_command">> => FlightControlSwitchCommand,
        <<"gps_satellite_count">> => GPSSatelliteCount,
        %% 故障展开字段
        <<"fault_remote_link">> => FaultRemoteLink,
        <<"fault_gps_position">> => FaultGpsPosition,
        <<"fault_imu_position">> => FaultImuPosition,
        <<"fault_airspeed">> => FaultAirspeed,
        <<"fault_baro_altitude">> => FaultBaroAltitude,
        <<"fault_payload">> => FaultPayload,
        <<"fault_beidou">> => FaultBeidou,
        <<"fault_rotation_speed">> => FaultRotationSpeed,
        <<"fault_ferroelectric">> => FaultFerroelectric,
        <<"fault_battery_voltage">> => FaultBatteryVoltage,
        <<"fault_warhead">> => FaultWarhead,
        <<"fault_launch_tube">> => FaultLaunchTube,
        %% 警告展开字段
        <<"warning_recovery_alert">> => WarningRecoveryAlert,
        <<"warning_emergency_data_unbound">> => WarningEmergencyDataUnbound,
        <<"warning_ground_station_pos_unbound">> => WarningGroundStationPosUnbound,
        <<"warning_cruise_route_unbound">> => WarningCruiseRouteUnbound,
        <<"warning_link_failure_mode_unbound">> => WarningLinkFailureModeUnbound,
        <<"warning_et_route_unbound">> => WarningEtRouteUnbound,
        %% 铁电故障位展开字段
        <<"ferroelectric_fault_bit0">> => FerroelectricFaultBit0,
        <<"ferroelectric_fault_bit1">> => FerroelectricFaultBit1,
        <<"ferroelectric_fault_bit2">> => FerroelectricFaultBit2,
        <<"ferroelectric_fault_bit3">> => FerroelectricFaultBit3,
        <<"ferroelectric_fault_bit4">> => FerroelectricFaultBit4,
        <<"ferroelectric_fault_bit5">> => FerroelectricFaultBit5,
        <<"ferroelectric_fault_bit6">> => FerroelectricFaultBit6,
        <<"ferroelectric_fault_bit7">> => FerroelectricFaultBit7,
        <<"ferroelectric_fault_bit8">> => FerroelectricFaultBit8,
        <<"ferroelectric_fault_bit9">> => FerroelectricFaultBit9,
        <<"ferroelectric_fault_bit10">> => FerroelectricFaultBit10,
        <<"ferroelectric_fault_bit11">> => FerroelectricFaultBit11,
        <<"ferroelectric_fault_bit12">> => FerroelectricFaultBit12,
        <<"ferroelectric_fault_bit13">> => FerroelectricFaultBit13,
        <<"ferroelectric_fault_bit14">> => FerroelectricFaultBit14,
        <<"ferroelectric_fault_bit15">> => FerroelectricFaultBit15,
        <<"attack_mode">> => AttackMode,
        <<"attack_mode_text">> => AttackModeText,
        <<"soft_command_overload">> => SoftCommandOverload,
        <<"soft_command_path_inclination">> => SoftCommandPathInclination,
        <<"path_inclination">> => PathInclination,
        <<"soft_command_path_deflection">> => SoftCommandPathDeflection,
        <<"combined_north_velocity">> => CombinedNorthVelocity,
        <<"combined_east_velocity">> => CombinedEastVelocity,
        <<"combined_vertical_velocity">> => CombinedVerticalVelocity,
        <<"gps_position_flag">> => GPSPositionFlag,
        <<"longitudinal_miss_distance">> => LongitudinalMissDistance,
        <<"lateral_miss_distance">> => LateralMissDistance,
        <<"battery_status">> => BatteryStatus,
        <<"battery_temp1">> => BatteryTemp1,
        <<"battery_temp2">> => BatteryTemp2
    }.