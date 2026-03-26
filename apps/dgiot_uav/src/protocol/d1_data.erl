%%%-------------------------------------------------------------------
%%% @doc
%%% d1_data.erl - D1遥测数据解析器
%%%
%%% 根据协议文档 D1.docx 解析无人机状态信息（0xD1）
%%% 包括：纬度、经度、姿态、舵面、电源、故障状态等。
%%% 数据从命令标识符之后开始，共115字节，按小端字节序解析。
%%% 每个字段按照协议表格中的缩放因子转换为物理量。
%%%
%%% 修改：增加电池状态、温度1、温度2字段（字节124-126）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(d1_data).

-export([parse/1, format/1]).

-include("d1_data.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 解析115字节的D1数据（从命令标识符之后开始）
%% 按照协议 D1.docx 表格逐字段解析，注意小端字节序
parse(<<  
        LatitudeRaw:32/little-signed,   % 纬度 ×10^7
        LongitudeRaw:32/little-signed,  % 经度 ×10^7
        HeadingRaw:16/little-signed,    % 航向角 ×10
        PitchRaw:16/little-signed,      % 俯仰角 ×100
        RollRaw:16/little-signed,       % 横滚角 ×100
        RelativeAltitudeRaw:16/little-signed, % 相对高度 ×10
        AirspeedRaw:16/little,          % 空速 ×100
        ElevatorAngleRaw:16/little-signed, % 升降舵角 ×100
        RudderAngleRaw:16/little-signed,   % 方向舵角 ×100
        AileronAngleRaw:16/little-signed,  % 副翼舵角 ×100
        ThrottleAngleRaw:16/little,        % 油门舵角 ×100
        GPSAltitudeRaw:16/little,          % 卫导高度 (H+500)*5
        BaroAltitudeRaw:16/little,         % 气压高度 (H+500)*5
        EastVelocityRaw:16/little-signed,  % 东向速度 ×100
        NorthVelocityRaw:16/little-signed, % 北向速度 ×100
        FlightTimeRaw:16/little,           % 飞行时间 S
        BatteryHeatingFlag:8,              % 电池加热标志
        ResetType:8,                       % 复位类型
        ResetCount:8,                      % 复位次数
        DetonationPowerStatus:8,           % 起爆供电状态
        TargetHeadingRaw:16/little-signed, % 目标航向 ×10
        TargetWaypoint:8,                  % 目标航点
        BatteryVoltageRaw:8,                % 电池电压 ×5
        ClimbRateRaw:16/little-signed,      % 爬升率 ×100
        DataBindingExecuted:8,              % 已执行的数据装订
        FlightMode:8,                       % 飞行模式
        Year:8, Month:8, Day:8, Hour:8, Minute:8, Second:8, % 时间
        GPSHorizontalAccuracyRaw:8,         % 卫导水平定位精度 ×10
        FlightControlSwitchCommand:8,       % 飞控开关指令回报
        GPSSatelliteCount:8,                % 卫导可用星数
        FaultStatusRaw:32/little,           % 故障状态（4字节）
        WarningFlagRaw:16/little,           % 警告标识（2字节）
        FerroelectricFaultRaw:16/little,    % 铁电故障（2字节）
        Reserved1:5/binary,                 % 预留 86-90字节
        Reserved2:15/binary,                % 预留 91-105字节？实际需核对
        AttackMode:8,                       % 攻击模式
        SoftCommandOverload:8,               % 软化指令过载
        SoftCommandPathInclination:8/signed, % 软化指令航迹倾角
        PathInclination:8/signed,            % 航迹倾角
        SoftCommandPathDeflectionRaw:16/little-signed, % 软化指令航迹偏角 ×0.02
        CombinedNorthVelocityRaw:16/little-signed, % 组合北向速度 ×0.01
        CombinedEastVelocityRaw:16/little-signed,  % 组合东向速度 ×0.01
        CombinedVerticalVelocityRaw:16/little-signed, % 组合天向速度 ×0.01
        GPSPositionFlag:8,                     % 卫导定位标志
        LongitudinalMissDistanceRaw:16/little-signed, % 纵向脱靶量 ×0.1
        LateralMissDistanceRaw:16/little-signed,      % 横向脱靶量 ×0.1
        % 字节124-126：电池状态、温度1、温度2
        BatteryStatus:8/signed,
        BatteryTemp1:8/signed,
        BatteryTemp2:8/signed,
        CRC:16/little,      % CRC校验
        _Rest/binary>>) ->
         FullYear = Year + 2000,
        {YearAdj, MonthAdj, DayAdj, HourAdj, MinuteAdj, SecondAdj} = adjust_to_beijing_time(FullYear, Month, Day, Hour, Minute, Second),
         Status = #drone_status_d1{
        latitude = LatitudeRaw / 10000000.0,
        longitude = LongitudeRaw / 10000000.0,
        heading = HeadingRaw / 10.0,
        pitch = PitchRaw / 100.0,
        roll = RollRaw / 100.0,
        relative_altitude = RelativeAltitudeRaw / 10.0,
        gps_altitude = (GPSAltitudeRaw / 5.0) - 500,
        baro_altitude = (BaroAltitudeRaw / 5.0) - 500,
        airspeed = AirspeedRaw / 100.0,
        east_velocity = EastVelocityRaw / 100.0,
        north_velocity = NorthVelocityRaw / 100.0,
        elevator_angle = ElevatorAngleRaw / 100.0,
        rudder_angle = RudderAngleRaw / 100.0,
        aileron_angle = AileronAngleRaw / 100.0,
        throttle_angle = ThrottleAngleRaw / 100.0,
        flight_time = FlightTimeRaw,
        battery_heating_flag = BatteryHeatingFlag,
        reset_type = ResetType,
        reset_count = ResetCount,
        detonation_power_status = DetonationPowerStatus,
        target_heading = TargetHeadingRaw / 10.0,
        target_waypoint = TargetWaypoint,
        battery_voltage = BatteryVoltageRaw / 5.0,
        climb_rate = ClimbRateRaw / 100.0,
        data_binding_executed = DataBindingExecuted,
        flight_mode = FlightMode,
        year = YearAdj,
        month = MonthAdj,
        day = DayAdj,
        hour = HourAdj,
        minute = MinuteAdj,
        second = SecondAdj,
        gps_horizontal_accuracy = GPSHorizontalAccuracyRaw / 10.0,
        flight_control_switch_command = FlightControlSwitchCommand,
        gps_satellite_count = GPSSatelliteCount,
        fault_status = FaultStatusRaw,
        warning_flag = WarningFlagRaw,
        ferroelectric_fault = FerroelectricFaultRaw,
        reserved1 = Reserved1,
        reserved2 = Reserved2,
        reserved3 = <<>>,   % 原数据无对应，置空
        attack_mode = AttackMode,
        soft_command_overload = SoftCommandOverload,
        soft_command_path_inclination = SoftCommandPathInclination * 0.5,
        path_inclination = PathInclination * 0.5,
        soft_command_path_deflection = SoftCommandPathDeflectionRaw * 0.02,
        combined_north_velocity = CombinedNorthVelocityRaw * 0.01,
        combined_east_velocity = CombinedEastVelocityRaw * 0.01,
        combined_vertical_velocity = CombinedVerticalVelocityRaw * 0.01,
        gps_position_flag = GPSPositionFlag,
        longitudinal_miss_distance = LongitudinalMissDistanceRaw * 0.1,
        lateral_miss_distance = LateralMissDistanceRaw * 0.1,
        battery_status = BatteryStatus,
        battery_temp1 = BatteryTemp1,
        battery_temp2 = BatteryTemp2,
        crc = CRC
    },
    {ok, Status};
            
parse(_) ->
    {error, invalid_packet_length}.

%% 时区调整函数：UTC 转北京时间（UTC+8）
adjust_to_beijing_time(Year, Month, Day, Hour, Minute, Second) ->
    case calendar:valid_date(Year, Month, Day) of
        true ->
            try
                %% 转换为格林威治秒数
                Seconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
                %% 增加8小时（28800秒）
                AdjustedSeconds = Seconds + 8 * 3600,
                {{Y, M, D}, {H, Mn, S}} = calendar:gregorian_seconds_to_datetime(AdjustedSeconds),
                {Y, M, D, H, Mn, S}
            catch
                _:_ -> {Year, Month, Day, Hour, Minute, Second}
            end;
        false ->
            {Year, Month, Day, Hour, Minute, Second}
    end.

%%%===================================================================
%%% 格式化输出
%%%===================================================================

format(Status) when is_record(Status, drone_status_d1) ->
    FlightModeText = get_flight_mode_text(Status#drone_status_d1.flight_mode),
    AttackModeText = get_attack_mode_text(Status#drone_status_d1.attack_mode),
    BatteryHeatingText = get_battery_heating_text(Status#drone_status_d1.battery_heating_flag),
    ResetTypeText = get_reset_type_text(Status#drone_status_d1.reset_type),
    DetonationPowerText = get_detonation_power_text(Status#drone_status_d1.detonation_power_status),
    DataBindingText = get_data_binding_text(Status#drone_status_d1.data_binding_executed),
    FlightControlSwitchText = get_flight_control_switch_text(Status#drone_status_d1.flight_control_switch_command),
    FaultStatusText = parse_fault_status(Status#drone_status_d1.fault_status),
    WarningFlagText = parse_warning_flag(Status#drone_status_d1.warning_flag),
    FerroelectricFaultText = parse_ferroelectric_fault(Status#drone_status_d1.ferroelectric_fault),
    
    FormatString = <<"D1状态:~n"
                   "  位置信息: 纬度=~.7f°, 经度=~.7f°, 航向=~.1f°, 俯仰=~.2f°, 横滚=~.2f°~n"
                   "  高度信息: 相对高度=~.1fm, 卫导高度=~.1fm, 气压高度=~.1fm~n"
                   "  速度信息: 空速=~.2fm/s, 东向速度=~.2fm/s, 北向速度=~.2fm/s~n"
                   "  舵角信息: 升降舵=~.2f°, 方向舵=~.2f°, 副翼舵=~.2f°, 油门舵=~.2f°~n"
                   "  时间信息: 飞行时间=~ps, 日期时间=~p-~p-~p ~p:~p:~p~n"
                   "  系统状态: 电池加热标志=~ts (0x~2.16.0B), 复位类型=~ts (0x~2.16.0B), 复位次数=~p, 起爆供电状态=~ts (0x~2.16.0B)~n"
                   "  导航信息: 目标航向=~.1f°, 目标航点=~p, 电池电压=~.1fV, 爬升率=~.2fm/s, 数据装订=~ts (0x~2.16.0B)~n"
                   "  飞行模式: ~ts (0x~2.16.0B)~n"
                   "  定位信息: 卫导水平精度=~.1fm, 飞控开关指令=~ts (0x~2.16.0B), 卫导可用星数=~p~n"
                   "  故障状态: ~ts (0x~8.16.0B)~n"
                   "  警告标识: ~ts (0x~4.16.0B)~n"
                   "  铁电故障: ~ts (0x~4.16.0B)~n"
                   "  攻击模式: ~ts (0x~2.16.0B)~n"
                   "  软化指令: 过载=~p, 航迹倾角=~.1f°, 航迹偏角=~.2f°~n"
                   "  组合速度: 北向=~.2fm/s, 东向=~.2fm/s, 天向=~.2fm/s~n"
                   "  定位标志: 卫导定位标志=0x~2.16.0B, 纵向脱靶量=~.1f, 横向脱靶量=~.1f~n"
                   "  电池状态: 状态字节=~p (0x~2.16.0B), 温度1=~p°C, 温度2=~p°C~n"
                   "  CRC校验: 0x~4.16.0B~n"/utf8>>,
    
    FormattedString = io_lib:format(FormatString,
                  [ Status#drone_status_d1.latitude,
                    Status#drone_status_d1.longitude,
                    Status#drone_status_d1.heading,
                    Status#drone_status_d1.pitch,
                    Status#drone_status_d1.roll,
                    Status#drone_status_d1.relative_altitude,
                    Status#drone_status_d1.gps_altitude,
                    Status#drone_status_d1.baro_altitude,
                    Status#drone_status_d1.airspeed,
                    Status#drone_status_d1.east_velocity,
                    Status#drone_status_d1.north_velocity,
                    Status#drone_status_d1.elevator_angle,
                    Status#drone_status_d1.rudder_angle,
                    Status#drone_status_d1.aileron_angle,
                    Status#drone_status_d1.throttle_angle,
                    Status#drone_status_d1.flight_time,
                    Status#drone_status_d1.year,
                    Status#drone_status_d1.month,
                    Status#drone_status_d1.day,
                    Status#drone_status_d1.hour,
                    Status#drone_status_d1.minute,
                    Status#drone_status_d1.second,
                    BatteryHeatingText,
                    Status#drone_status_d1.battery_heating_flag,
                    ResetTypeText,
                    Status#drone_status_d1.reset_type,
                    Status#drone_status_d1.reset_count,
                    DetonationPowerText,
                    Status#drone_status_d1.detonation_power_status,
                    Status#drone_status_d1.target_heading,
                    Status#drone_status_d1.target_waypoint,
                    Status#drone_status_d1.battery_voltage,
                    Status#drone_status_d1.climb_rate,
                    DataBindingText,
                    Status#drone_status_d1.data_binding_executed,
                    FlightModeText,
                    Status#drone_status_d1.flight_mode,
                    Status#drone_status_d1.gps_horizontal_accuracy,
                    FlightControlSwitchText,
                    Status#drone_status_d1.flight_control_switch_command,
                    Status#drone_status_d1.gps_satellite_count,
                    FaultStatusText,
                    Status#drone_status_d1.fault_status,
                    WarningFlagText,
                    Status#drone_status_d1.warning_flag,
                    FerroelectricFaultText,
                    Status#drone_status_d1.ferroelectric_fault,
                    AttackModeText,
                    Status#drone_status_d1.attack_mode,
                    Status#drone_status_d1.soft_command_overload,
                    Status#drone_status_d1.soft_command_path_inclination,
                    Status#drone_status_d1.soft_command_path_deflection,
                    Status#drone_status_d1.combined_north_velocity,
                    Status#drone_status_d1.combined_east_velocity,
                    Status#drone_status_d1.combined_vertical_velocity,
                    Status#drone_status_d1.gps_position_flag,
                    Status#drone_status_d1.longitudinal_miss_distance,
                    Status#drone_status_d1.lateral_miss_distance,
                    Status#drone_status_d1.battery_status,
                    Status#drone_status_d1.battery_status,
                    Status#drone_status_d1.battery_temp1,
                    Status#drone_status_d1.battery_temp2,
                    Status#drone_status_d1.crc
                  ]),
    D1 = unicode:characters_to_binary(FormattedString, utf8, utf8),
    io:format("~ts", [D1]),
    D1.

%%%===================================================================
%%% 辅助函数
%%%===================================================================

get_flight_mode_text(Mode) ->
    case Mode of
        ?FLIGHT_MODE_ALTITUDE_HOLD -> <<"高度保持模式"/utf8>>;
        ?FLIGHT_MODE_RETURN_HOME -> <<"返航模式"/utf8>>;
        ?FLIGHT_MODE_CIRCLE -> <<"盘旋模式"/utf8>>;
        ?FLIGHT_MODE_NAVIGATION -> <<"导航模式"/utf8>>;
        ?FLIGHT_MODE_TAKEOFF -> <<"起飞模式"/utf8>>;
        ?FLIGHT_MODE_LANDING -> <<"降落模式"/utf8>>;
        ?FLIGHT_MODE_GO_AROUND -> <<"复飞模式"/utf8>>;
        ?FLIGHT_MODE_ATTACK -> <<"攻击模式"/utf8>>;
        ?FLIGHT_MODE_BARREL_ROLL -> <<"桶滚模式"/utf8>>;
        _ -> <<"未知模式"/utf8>>
    end.

get_attack_mode_text(Mode) ->
    case Mode of
        ?ATTACK_MODE_IMAGE_GUIDANCE -> <<"图像制导"/utf8>>;
        ?ATTACK_MODE_POSITION_GUIDANCE -> <<"位置制导"/utf8>>;
        _ -> <<"未知攻击模式"/utf8>>
    end.

get_battery_heating_text(Flag) ->
    case Flag of
        ?BATTERY_HEATING_NORMAL -> <<"正常"/utf8>>;
        ?BATTERY_HEATING_HEATING -> <<"加热中"/utf8>>;
        _ -> <<"未知状态"/utf8>>
    end.

get_reset_type_text(Type) ->
    case Type of
        ?RESET_TYPE_POWER_ON -> <<"上电复位"/utf8>>;
        ?RESET_TYPE_WATCHDOG -> <<"看门狗复位"/utf8>>;
        _ -> <<"未知复位类型"/utf8>>
    end.

get_detonation_power_text(Status) ->
    case Status of
        ?DETONATION_POWER_OFF -> <<"未供电"/utf8>>;
        ?DETONATION_POWER_ON -> <<"已供电"/utf8>>;
        _ -> <<"未知状态"/utf8>>
    end.

get_data_binding_text(Command) ->
    case Command of
        ?DATA_BINDING_PAYLOAD_CONTROL -> <<"载荷控制"/utf8>>;
        _ -> <<"未知指令"/utf8>>
    end.

get_flight_control_switch_text(Command) ->
    case Command of
        ?FLIGHT_CONTROL_SWITCH_GO_AROUND -> <<"复飞"/utf8>>;
        _ -> <<"未知指令"/utf8>>
    end.

parse_fault_status(FaultStatus) ->
    Faults = [
        {1 bsl ?FAULT_REMOTE_LINK_BIT,      <<"遥控链路故障"/utf8>>},
        {1 bsl ?FAULT_GPS_POSITION_BIT,     <<"卫导定位故障"/utf8>>},
        {1 bsl ?FAULT_IMU_POSITION_BIT,     <<"IMU定位故障"/utf8>>},
        {1 bsl ?FAULT_AIRSPEED_BIT,         <<"空速故障"/utf8>>},
        {1 bsl ?FAULT_BARO_ALTITUDE_BIT,    <<"气压高度故障"/utf8>>},
        {1 bsl ?FAULT_RESERVED5_BIT,        <<"预留位5"/utf8>>},
        {1 bsl ?FAULT_PAYLOAD_BIT,          <<"载荷故障"/utf8>>},
        {1 bsl ?FAULT_BEIDOU_BIT,           <<"北斗故障"/utf8>>},
        {1 bsl ?FAULT_ROTATION_SPEED_BIT,   <<"转速故障"/utf8>>},
        {1 bsl ?FAULT_RESERVED9_BIT,        <<"预留位9"/utf8>>},
        {1 bsl ?FAULT_FERROELECTRIC_BIT,    <<"铁电故障"/utf8>>},
        {1 bsl ?FAULT_BATTERY_VOLTAGE_BIT,  <<"电池电压故障"/utf8>>},
        {1 bsl ?FAULT_WARHEAD_BIT,          <<"引战故障"/utf8>>},
        {1 bsl ?FAULT_LAUNCH_TUBE_BIT,      <<"发射筒故障"/utf8>>}
    ],
    ActiveFaults = lists:filtermap(fun({Mask, Name}) ->
        case FaultStatus band Mask of 0 -> false; _ -> {true, Name} end
    end, Faults),
    case ActiveFaults of [] -> <<"无故障"/utf8>>; _ -> binary:list_to_bin(lists:join(<<", "/utf8>>, ActiveFaults)) end.

parse_warning_flag(WarningFlag) ->
    Warnings = [
        {1 bsl ?WARNING_RECOVERY_ALERT_BIT,           <<"进入回收警示"/utf8>>},
        {1 bsl ?WARNING_EMERGENCY_DATA_UNBOUND_BIT,   <<"应急数据未装订"/utf8>>},
        {1 bsl ?WARNING_GROUND_STATION_POS_UNBOUND_BIT, <<"地面站位置未装订"/utf8>>},
        {1 bsl ?WARNING_CRUISE_ROUTE_UNBOUND_BIT,     <<"巡飞航线未装订"/utf8>>},
        {1 bsl ?WARNING_LINK_FAILURE_MODE_UNBOUND_BIT,<<"链路失效模式未装订"/utf8>>},
        {1 bsl ?WARNING_ET_ROUTE_UNBOUND_BIT,         <<"ET航线未装订"/utf8>>}
    ],
    ActiveWarnings = lists:filtermap(fun({Mask, Name}) ->
        case WarningFlag band Mask of 0 -> false; _ -> {true, Name} end
    end, Warnings),
    case ActiveWarnings of [] -> <<"无警告"/utf8>>; _ -> binary:list_to_bin(lists:join(<<", "/utf8>>, ActiveWarnings)) end.

parse_ferroelectric_fault(Fault) ->
    %% 简单处理，仅输出原始值
    <<"铁电故障原始值: ", (integer_to_binary(Fault))/binary>>.