%%%-------------------------------------------------------------------
%%% @doc
%%% d3_data.erl - D3遥测数据解析器（拆分版本）
%%%
%%% 根据协议文档 D3.docx 解析无人机状态信息（0xD3）
%%% 根据字节43（数据选择标识）将数据分配到对应的北斗或卫导字段：
%%% - 信噪比统计（字节29-34）根据 snr_source 存入 beidou_snr_gt* 或 gps_snr_gt*
%%% - 磁强数据（字节35-40）根据 mag_type 存入 magnetic_error_* 或 magnetic_value_*
%%% - 位置信息（字节47-56）根据 pos_source 存入 beidou_* 或 gps_*
%%%
%%% 修正：使用位语法解析保留字段，避免 binary_to_integer/1 错误。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(d3_data).

-export([parse/1, format/1, format_detailed/1, parse_complete/1, parse_full/1,
         get_data_select_details/1, get_magnetic_calibration_details/1,
         get_switch_status_details/1, get_warhead_status0_details/1,
         get_warhead_status1_details/1, get_flight_mode_details/1]).

%% 包含头文件
-include("d3_data.hrl").

%%%===================================================================
%%% API 函数 - 解析D3数据（不含帧头）
%%%===================================================================

parse(<<
        %% 字节21-22：地速方向 (0.1°)
        GroundSpeedDirectionRaw:16/little,
        %% 字节23-28：预留6字节
        Reserved1:6/binary,
        %% 字节29-34：信噪比统计（6个UINT8，与源选择无关，先读取）
        SnrGt46:8,
        SnrGt44:8,
        SnrGt42:8,
        SnrGt40:8,
        SnrGt38:8,
        SnrGt35:8,
        %% 字节35-40：磁强数据（6字节，2字节有符号小端×3轴，先读取）
        MagXRaw:16/little-signed,
        MagYRaw:16/little-signed,
        MagZRaw:16/little-signed,
        %% 字节41：预留
        Reserved2:8,
        %% 字节42：北斗自毁状态
        BeidouSelfDestructStatus:8,
        %% 字节43：数据选择标识
        DataSelectFlag:8,
        %% 字节44：预留
        Reserved3:8,
        %% 字节45：北斗PDOP (0.2)
        BeidouPdopRaw:8,
        %% 字节46：主循环时间 (0.1ms)
        MainLoopTimeRaw:8,
        %% 字节47-48：高度 (H*0.1-500)
        AltitudeRaw:16/little,
        %% 字节49-52：纬度 (10^7)
        LatitudeRaw:32/little-signed,
        %% 字节53-56：经度 (10^7)
        LongitudeRaw:32/little-signed,
        %% 字节57：卫导解算星数
        GPSSatelliteCount:8,
        %% 字节58：北斗解算星数
        BeidouSatelliteCount:8,
        %% 字节59：载荷开关指令回报
        PayloadSwitchCommand:8,
        %% 字节60：引战开关指令回报
        WarheadSwitchCommand:8,
        %% 字节61：发射筒指令回报
        LaunchTubeCommand:8,
        %% 字节62：卫导PDOP (0.2)
        GPSPdopRaw:8,
        %% 字节63-64：磁航向 (0.1°)
        MagneticHeadingRaw:16/little,
        %% 字节65：磁航向校准状态
        MagneticCalibrationStatus:8,
        %% 字节66：发射筒开关指令
        LaunchTubeSwitchCommand:8,
        %% 字节67-68：导引头高低角 (0.1°)
        SeekerElevationAngleRaw:16/little-signed,
        %% 字节69-70：导引头方位角 (0.1°)
        SeekerAzimuthAngleRaw:16/little-signed,
        %% 字节71-72：导引头高低角速度 (0.1°/s)
        SeekerElevationRateRaw:16/little-signed,
        %% 字节73-74：导引头方位角速度 (0.1°/s)
        SeekerAzimuthRateRaw:16/little-signed,
        %% 字节75-76：视线高低角 (0.1°)
        LineOfSightElevationRaw:16/little-signed,
        %% 字节77-78：视线方位角 (0.1°)
        LineOfSightAzimuthRaw:16/little-signed,
        %% 字节79-80：飞控温度1 (0.1°C)
        FlightControlTemp1Raw:16/little-signed,
        %% 字节81-82：飞控温度2 (0.1°C)
        FlightControlTemp2Raw:16/little-signed,
        %% 字节83：引战设备状态字0
        WarheadStatus0:8,
        %% 字节84：引战设备状态字1
        WarheadStatus1:8,
        %% 字节85：引战设备加速度值 (2g)
        WarheadAcceleration:8,
        %% 字节86：激光测距值 (0.2m)
        LaserRangeValueRaw:8,
        %% 字节87：触炸导电膜电平 (0.1V)
        TouchDetonationVoltageRaw:8,
        %% 字节88：发射筒状态字
        LaunchTubeStatus:8,
        %% 字节89：发射筒点火电压 (V)
        LaunchTubeIgnitionVoltageRaw:8,
        %% 字节90：引战电压 (0.1V)
        WarheadVoltageRaw:8,
        %% 字节91：载荷电压 (0.1V)
        PayloadVoltageRaw:8,
        %% 字节92：夜航电压 (0.1V)
        NightFlightVoltageRaw:8,
        %% 字节93：5.2V电源 (0.1V)
        Power5V2Raw:8,
        %% 字节94：5.0V电源 (0.1V)
        Power5V0Raw:8,
        %% 字节95：8.4V电源1 (0.1V)
        Power8V4_1Raw:8,
        %% 字节96：8.4V电源2 (0.1V)
        Power8V4_2Raw:8,
        %% 字节97：硬开关电压 (0.1V)
        HardSwitchVoltageRaw:8,
        %% 字节98：开关状态
        SwitchStatus:8,
        %% 字节99-100：预留2字节
        Reserved4Raw:16/little,
        %% 字节101：引信充电电压
        FuzeChargingVoltage:8,
        %% 字节102-108：预留7字节
        Reserved5:7/binary,
        %% 字节109：制导镇定系数
        GuidanceStabilizationCoef:8,
        %% 字节110：风速1 (0.2m/s)
        WindSpeed1Raw:8,
        %% 字节111-112：风向1 (0.1°/s)
        WindDirection1Raw:16/little,
        %% 字节113：风速2 (0.2m/s)
        WindSpeed2Raw:8,
        %% 字节114-115：风向2 (0.1°/s)
        WindDirection2Raw:16/little,
        %% 字节116-118：预留3字节
        Reserved6:3/binary,
        %% 字节119：载荷电子变倍数
        PayloadElectronicZoom:8,
        %% 字节120：软化的载荷跟踪态标志 (0.01)
        SoftenedPayloadTrackingFlagRaw:8,
        %% 字节121：载荷跟踪态标志
        PayloadTrackingFlag:8,
        %% 字节122：飞机类型 (2=任务机, 4=训练机)
        DroneType:8,
        %% 字节123：视线方位-航向偏差 (1°)
        SightAzimuthHeadingDeviationRaw:8/signed,
        %% 字节124-126：预留3字节
        Reserved7:3/binary,
        %% 字节127-128：CRC校验 (小端)
        CRC:16/little,
        _Rest/binary>> ) ->

    %% 解析数据选择标识
    SnrSource = DataSelectFlag band 1,
    PosSource = (DataSelectFlag bsr 1) band 1,
    MagType   = (DataSelectFlag bsr 2) band 1,

    %% 信噪比统计：根据 SnrSource 分配到北斗或卫导
    {BeidouSnrGt46, BeidouSnrGt44, BeidouSnrGt42, BeidouSnrGt40, BeidouSnrGt38, BeidouSnrGt35,
     GpsSnrGt46,   GpsSnrGt44,   GpsSnrGt42,   GpsSnrGt40,   GpsSnrGt38,   GpsSnrGt35} =
        case SnrSource of
            0 -> % 卫导信噪比
                {0,0,0,0,0,0,
                 SnrGt46, SnrGt44, SnrGt42, SnrGt40, SnrGt38, SnrGt35};
            1 -> % 北斗信噪比
                {SnrGt46, SnrGt44, SnrGt42, SnrGt40, SnrGt38, SnrGt35,
                 0,0,0,0,0,0}
        end,

    %% 磁强数据：根据 MagType 分配到误差或值
    MagX = MagXRaw / 10.0,
    MagY = MagYRaw / 10.0,
    MagZ = MagZRaw / 10.0,
    {MagneticErrorX, MagneticErrorY, MagneticErrorZ, MagneticValueX, MagneticValueY, MagneticValueZ} =
        case MagType of
            0 -> % 磁强误差
                {MagX, MagY, MagZ, 0.0, 0.0, 0.0};
            1 -> % 磁强值
                {0.0, 0.0, 0.0, MagX, MagY, MagZ}
        end,

    %% 位置信息：根据 PosSource 分配到北斗或卫导
    Altitude = (AltitudeRaw * 0.1) - 500,
    Latitude = LatitudeRaw / 10000000.0,
    Longitude = LongitudeRaw / 10000000.0,
    {GPSAltitude, GPSLatitude, GPSLongitude, BeidouAltitude, BeidouLatitude, BeidouLongitude} =
        case PosSource of
            0 -> % 卫导位置
                {Altitude, Latitude, Longitude, 0.0, 0.0, 0.0};
            1 -> % 北斗位置
                {0.0, 0.0, 0.0, Altitude, Latitude, Longitude}
        end,

    %% 物理量转换
    GroundSpeedDirection = GroundSpeedDirectionRaw / 10.0,
    BeidouPdop = BeidouPdopRaw / 5.0,
    MainLoopTime = MainLoopTimeRaw / 10.0,
    GPSPdop = GPSPdopRaw / 5.0,
    MagneticHeading = MagneticHeadingRaw / 10.0,
    SeekerElevationAngle = SeekerElevationAngleRaw / 10.0,
    SeekerAzimuthAngle = SeekerAzimuthAngleRaw / 10.0,
    SeekerElevationRate = SeekerElevationRateRaw / 10.0,
    SeekerAzimuthRate = SeekerAzimuthRateRaw / 10.0,
    LineOfSightElevation = LineOfSightElevationRaw / 10.0,
    LineOfSightAzimuth = LineOfSightAzimuthRaw / 10.0,
    FlightControlTemp1 = FlightControlTemp1Raw / 10.0,
    FlightControlTemp2 = FlightControlTemp2Raw / 10.0,
    LaserRangeValue = LaserRangeValueRaw * 0.2,
    TouchDetonationVoltage = TouchDetonationVoltageRaw / 10.0,
    LaunchTubeIgnitionVoltage = LaunchTubeIgnitionVoltageRaw,
    WarheadVoltage = WarheadVoltageRaw / 10.0,
    PayloadVoltage = PayloadVoltageRaw / 10.0,
    NightFlightVoltage = NightFlightVoltageRaw / 10.0,
    Power5V2 = Power5V2Raw / 10.0,
    Power5V0 = Power5V0Raw / 10.0,
    Power8V4_1 = Power8V4_1Raw / 10.0,
    Power8V4_2 = Power8V4_2Raw / 10.0,
    HardSwitchVoltage = HardSwitchVoltageRaw / 10.0,
    WindSpeed1 = WindSpeed1Raw * 0.2,
    WindDirection1 = WindDirection1Raw / 10.0,
    WindSpeed2 = WindSpeed2Raw * 0.2,
    WindDirection2 = WindDirection2Raw / 10.0,
    SoftenedPayloadTrackingFlag = SoftenedPayloadTrackingFlagRaw / 100.0,
    SightAzimuthHeadingDeviation = SightAzimuthHeadingDeviationRaw,

    %% 创建状态记录
    Status = #drone_status_d3{
        ground_speed_direction = GroundSpeedDirection,
        reserved1 = Reserved1,
        %% 北斗信噪比
        beidou_snr_gt46_count = BeidouSnrGt46,
        beidou_snr_gt44_count = BeidouSnrGt44,
        beidou_snr_gt42_count = BeidouSnrGt42,
        beidou_snr_gt40_count = BeidouSnrGt40,
        beidou_snr_gt38_count = BeidouSnrGt38,
        beidou_snr_gt35_count = BeidouSnrGt35,
        %% 卫导信噪比
        gps_snr_gt46_count = GpsSnrGt46,
        gps_snr_gt44_count = GpsSnrGt44,
        gps_snr_gt42_count = GpsSnrGt42,
        gps_snr_gt40_count = GpsSnrGt40,
        gps_snr_gt38_count = GpsSnrGt38,
        gps_snr_gt35_count = GpsSnrGt35,
        %% 磁强误差/值
        magnetic_error_x = MagneticErrorX,
        magnetic_error_y = MagneticErrorY,
        magnetic_error_z = MagneticErrorZ,
        magnetic_value_x = MagneticValueX,
        magnetic_value_y = MagneticValueY,
        magnetic_value_z = MagneticValueZ,
        %% 系统状态
        reserved2 = Reserved2,
        beidou_self_destruct_status = BeidouSelfDestructStatus,
        data_select_flag = DataSelectFlag,
        reserved3 = Reserved3,
        beidou_pdop = BeidouPdop,
        main_loop_time = MainLoopTime,
        %% 位置信息
        gps_altitude = GPSAltitude,
        gps_latitude = GPSLatitude,
        gps_longitude = GPSLongitude,
        beidou_altitude = BeidouAltitude,
        beidou_latitude = BeidouLatitude,
        beidou_longitude = BeidouLongitude,
        %% 卫星信息
        gps_satellite_count = GPSSatelliteCount,
        beidou_satellite_count = BeidouSatelliteCount,
        %% 开关指令
        payload_switch_command = PayloadSwitchCommand,
        warhead_switch_command = WarheadSwitchCommand,
        launch_tube_command = LaunchTubeCommand,
        %% GPS和磁力计信息
        gps_pdop = GPSPdop,
        magnetic_heading = MagneticHeading,
        magnetic_calibration_status = MagneticCalibrationStatus,
        launch_tube_switch_command = LaunchTubeSwitchCommand,
        %% 导引头信息
        seeker_elevation_angle = SeekerElevationAngle,
        seeker_azimuth_angle = SeekerAzimuthAngle,
        seeker_elevation_rate = SeekerElevationRate,
        seeker_azimuth_rate = SeekerAzimuthRate,
        line_of_sight_elevation = LineOfSightElevation,
        line_of_sight_azimuth = LineOfSightAzimuth,
        %% 温度信息
        flight_control_temp1 = FlightControlTemp1,
        flight_control_temp2 = FlightControlTemp2,
        %% 战斗部状态
        warhead_status0 = WarheadStatus0,
        warhead_status1 = WarheadStatus1,
        warhead_acceleration = WarheadAcceleration,
        laser_range_value = LaserRangeValue,
        touch_detonation_voltage = TouchDetonationVoltage,
        %% 发射筒状态
        launch_tube_status = LaunchTubeStatus,
        launch_tube_ignition_voltage = LaunchTubeIgnitionVoltage,
        %% 电压信息
        warhead_voltage = WarheadVoltage,
        payload_voltage = PayloadVoltage,
        night_flight_voltage = NightFlightVoltage,
        power_5v2 = Power5V2,
        power_5v0 = Power5V0,
        power_8v4_1 = Power8V4_1,
        power_8v4_2 = Power8V4_2,
        hard_switch_voltage = HardSwitchVoltage,
        %% 开关状态
        switch_status = SwitchStatus,
        reserved4 = Reserved4Raw,
        %% 引信信息
        fuze_charging_voltage = FuzeChargingVoltage,
        reserved5 = Reserved5,
        %% 导引和风速
        guidance_stabilization_coef = GuidanceStabilizationCoef,
        wind_speed1 = WindSpeed1,
        wind_direction1 = WindDirection1,
        wind_speed2 = WindSpeed2,
        wind_direction2 = WindDirection2,
        %% 预留字段
        reserved6 = Reserved6,
        %% 载荷信息
        payload_electronic_zoom = PayloadElectronicZoom,
        softened_payload_tracking_flag = SoftenedPayloadTrackingFlag,
        payload_tracking_flag = PayloadTrackingFlag,
        drone_type = DroneType,
        sight_azimuth_heading_deviation = SightAzimuthHeadingDeviation,
        %% 预留字段
        reserved7 = Reserved7,
        %% CRC
        crc = CRC
    },
    {ok, Status};

parse(_) ->
    {error, invalid_packet_length}.

%%%===================================================================
%%% 格式化函数
%%%===================================================================

-spec format(#drone_status_d3{}) -> binary().
format(Status) when is_record(Status, drone_status_d3) ->
    DroneTypeText = get_drone_type_text(Status#drone_status_d3.drone_type),
    SwitchStatusDetails = format_switch_status(Status),
    
    %% 根据数据选择标识决定显示哪组磁强数据
    MagType = (Status#drone_status_d3.data_select_flag bsr 2) band 1,
    {MagX, MagY, MagZ} = case MagType of
        0 -> {Status#drone_status_d3.magnetic_error_x,
              Status#drone_status_d3.magnetic_error_y,
              Status#drone_status_d3.magnetic_error_z};
        1 -> {Status#drone_status_d3.magnetic_value_x,
              Status#drone_status_d3.magnetic_value_y,
              Status#drone_status_d3.magnetic_value_z}
    end,
    
    %% 根据位置源选择显示哪组位置
    PosSource = (Status#drone_status_d3.data_select_flag bsr 1) band 1,
    {Lat, Lon, Alt} = case PosSource of
        0 -> {Status#drone_status_d3.gps_latitude,
              Status#drone_status_d3.gps_longitude,
              Status#drone_status_d3.gps_altitude};
        1 -> {Status#drone_status_d3.beidou_latitude,
              Status#drone_status_d3.beidou_longitude,
              Status#drone_status_d3.beidou_altitude}
    end,
    
    FormatString = "D3状态:~n"
                   "  地速方向: ~.1f°~n"
                   "  北斗信噪比统计: >46:~p, >44:~p, >42:~p, >40:~p, >38:~p, >35:~p~n"
                   "  卫导信噪比统计: >46:~p, >44:~p, >42:~p, >40:~p, >38:~p, >35:~p~n"
                   "  磁力计(当前类型): X=~.1f, Y=~.1f, Z=~.1f~n"
                   "  位置信息(当前源): 纬度=~.7f°, 经度=~.7f°, 高度=~.1fm~n"
                   "  卫星信息: GPS=~p, 北斗=~p, GPS PDOP=~.1f, 北斗 PDOP=~.1f~n"
                   "  磁航向: ~.1f°, 磁力计校准状态: 0x~2.16.0B~n"
                   "  导引头角度: 俯仰=~.1f°, 方位=~.1f°, 视线俯仰=~.1f°, 视线方位=~.1f°~n"
                   "  飞控温度: 温度1=~.1f°C, 温度2=~.1f°C~n"
                   "  电压信息: 战斗部=~.1fV, 载荷=~.1fV, 夜航=~.1fV, 5.2V=~.1fV, 5.0V=~.1fV, 8.4V_1=~.1fV, 8.4V_2=~.1fV~n"
                   "  开关状态: ~ts~n"
                   "  无人机类型: ~ts~n"
                   "  风速风向: 风速1=~.1fm/s, 风向1=~.1f°, 风速2=~.1fm/s, 风向2=~.1f°~n"
                   "  载荷信息: 电子变焦=~p, 跟踪标志=~p, 视线方位航向偏差=~p°~n",
    
    FormattedString = io_lib:format(FormatString,
        [
         Status#drone_status_d3.ground_speed_direction,
         Status#drone_status_d3.beidou_snr_gt46_count,
         Status#drone_status_d3.beidou_snr_gt44_count,
         Status#drone_status_d3.beidou_snr_gt42_count,
         Status#drone_status_d3.beidou_snr_gt40_count,
         Status#drone_status_d3.beidou_snr_gt38_count,
         Status#drone_status_d3.beidou_snr_gt35_count,
         Status#drone_status_d3.gps_snr_gt46_count,
         Status#drone_status_d3.gps_snr_gt44_count,
         Status#drone_status_d3.gps_snr_gt42_count,
         Status#drone_status_d3.gps_snr_gt40_count,
         Status#drone_status_d3.gps_snr_gt38_count,
         Status#drone_status_d3.gps_snr_gt35_count,
         MagX, MagY, MagZ,
         Lat, Lon, Alt,
         Status#drone_status_d3.gps_satellite_count,
         Status#drone_status_d3.beidou_satellite_count,
         Status#drone_status_d3.gps_pdop,
         Status#drone_status_d3.beidou_pdop,
         Status#drone_status_d3.magnetic_heading,
         Status#drone_status_d3.magnetic_calibration_status,
         Status#drone_status_d3.seeker_elevation_angle,
         Status#drone_status_d3.seeker_azimuth_angle,
         Status#drone_status_d3.line_of_sight_elevation,
         Status#drone_status_d3.line_of_sight_azimuth,
         Status#drone_status_d3.flight_control_temp1,
         Status#drone_status_d3.flight_control_temp2,
         Status#drone_status_d3.warhead_voltage,
         Status#drone_status_d3.payload_voltage,
         Status#drone_status_d3.night_flight_voltage,
         Status#drone_status_d3.power_5v2,
         Status#drone_status_d3.power_5v0,
         Status#drone_status_d3.power_8v4_1,
         Status#drone_status_d3.power_8v4_2,
         SwitchStatusDetails,
         DroneTypeText,
         Status#drone_status_d3.wind_speed1,
         Status#drone_status_d3.wind_direction1,
         Status#drone_status_d3.wind_speed2,
         Status#drone_status_d3.wind_direction2,
         Status#drone_status_d3.payload_electronic_zoom,
         Status#drone_status_d3.payload_tracking_flag,
         Status#drone_status_d3.sight_azimuth_heading_deviation
        ]),
    D3 = unicode:characters_to_binary(FormattedString, utf8, utf8),
    io:format("~ts", [D3]),
    D3.

-spec format_detailed(#drone_status_d3{}) -> binary().
format_detailed(Status) when is_record(Status, drone_status_d3) ->
    % 获取各个详细描述
    DataSelectDetails = get_data_select_details(Status),
    MagneticCalibrationDetails = get_magnetic_calibration_details(Status),
    SwitchStatusDetails = get_switch_status_details(Status),
    WarheadStatus0Details = get_warhead_status0_details(Status),
    WarheadStatus1Details = get_warhead_status1_details(Status),
    FlightModeDetails = get_flight_mode_details(0), % 默认值，实际应用中需从Status中提取FlightModeBits
    
    FormatString = "=== D3遥测数据详细报告 ===~n~n"
                   "1. 数据选择标志:~n~ts~n~n"
                   "2. 磁力计校准状态:~n~ts~n~n"
                   "3. 开关状态:~n~ts~n~n"
                   "4. 战斗部状态字0:~n~ts~n~n"
                   "5. 战斗部状态字1:~n~ts~n~n"
                   "6. 飞行模式:~n~ts~n~n"
                   "=== 报告结束 ===",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   DataSelectDetails,
                   MagneticCalibrationDetails,
                   SwitchStatusDetails,
                   WarheadStatus0Details,
                   WarheadStatus1Details,
                   FlightModeDetails
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

%%%===================================================================
%%% 详细解析辅助函数
%%%===================================================================

-spec get_data_select_details(#drone_status_d3{}) -> binary().
get_data_select_details(Status) when is_record(Status, drone_status_d3) ->
    Flag = Status#drone_status_d3.data_select_flag,
    
    SnrSource = case (Flag bsr 0) band 1 of
        0 -> <<"GPS信噪比"/utf8>>;
        1 -> <<"北斗信噪比"/utf8>>;
        _ -> <<"未知信噪比源"/utf8>>
    end,
    
    PosSource = case (Flag bsr 1) band 1 of
        0 -> <<"GPS位置"/utf8>>;
        1 -> <<"北斗位置"/utf8>>;
        _ -> <<"未知位置源"/utf8>>
    end,
    
    MagType = case (Flag bsr 2) band 1 of
        0 -> <<"磁力计误差"/utf8>>;
        1 -> <<"磁力计值"/utf8>>;
        _ -> <<"未知磁力计类型"/utf8>>
    end,
    
    FormatString = "数据选择标志 (0x~2.16.0B):~n"
                   "  信噪比源: ~ts~n"
                   "  位置源: ~ts~n"
                   "  磁力计类型: ~ts~n"
                   "  原始值: 0x~2.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [Flag, SnrSource, PosSource, MagType, Flag]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

-spec get_magnetic_calibration_details(#drone_status_d3{}) -> binary().
get_magnetic_calibration_details(Status) when is_record(Status, drone_status_d3) ->
    StatusByte = Status#drone_status_d3.magnetic_calibration_status,
    CalibrationStatus = StatusByte band 16#0F,
    CalibrationCommand = (StatusByte bsr 4) band 16#0F,
    
    CalibrationStatusText = case CalibrationStatus of
        0 -> <<"未校准"/utf8>>;
        1 -> <<"正在校准"/utf8>>;
        2 -> <<"校准完成"/utf8>>;
        3 -> <<"校准失败"/utf8>>;
        _ -> <<"未知校准状态"/utf8>>
    end,
    
    CalibrationCommandText = case CalibrationCommand of
        0 -> <<"无命令"/utf8>>;
        1 -> <<"开始校准"/utf8>>;
        2 -> <<"停止校准"/utf8>>;
        _ -> <<"未知命令"/utf8>>
    end,
    
    FormatString = "磁力计校准状态 (0x~2.16.0B):~n"
                   "  校准状态: ~ts (0x~2.16.0B)~n"
                   "  校准命令: ~ts (0x~2.16.0B)~n"
                   "  是否校准中: ~ts~n"
                   "  原始值: 0x~2.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   StatusByte,
                   CalibrationStatusText, CalibrationStatus,
                   CalibrationCommandText, CalibrationCommand,
                   case CalibrationStatus =:= 1 of true -> <<"是"/utf8>>; false -> <<"否"/utf8>> end,
                   StatusByte
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

-spec get_switch_status_details(#drone_status_d3{}) -> binary().
get_switch_status_details(Status) when is_record(Status, drone_status_d3) ->
    StatusByte = Status#drone_status_d3.switch_status,
    
    SoftSwitch1 = (StatusByte bsr 0) band 1,
    SoftSwitch2 = (StatusByte bsr 1) band 1,
    LeftWingSwitch = (StatusByte bsr 2) band 1,
    RightWingSwitch = (StatusByte bsr 3) band 1,
    HardSwitchMeasurement = (StatusByte bsr 4) band 1,
    
    BoolToText = fun(1) -> <<"开"/utf8>>; (0) -> <<"关"/utf8>>; (_) -> <<"未知"/utf8>> end,
    
    FormatString = "开关状态 (0x~2.16.0B):~n"
                   "  软开关1: ~ts (位0)~n"
                   "  软开关2: ~ts (位1)~n"
                   "  左机翼开关: ~ts (位2)~n"
                   "  右机翼开关: ~ts (位3)~n"
                   "  硬开关测量: ~ts (位4)~n"
                   "  原始值: 0x~2.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   StatusByte,
                   BoolToText(SoftSwitch1),
                   BoolToText(SoftSwitch2),
                   BoolToText(LeftWingSwitch),
                   BoolToText(RightWingSwitch),
                   BoolToText(HardSwitchMeasurement),
                   StatusByte
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

-spec get_warhead_status0_details(#drone_status_d3{}) -> binary().
get_warhead_status0_details(Status) when is_record(Status, drone_status_d3) ->
    StatusByte = Status#drone_status_d3.warhead_status0,
    
    DeviceNormal = (StatusByte bsr 0) band 1,
    SelfTestComplete = (StatusByte bsr 1) band 1,
    Reserved = (StatusByte bsr 2) band 1,
    FirstSafetyReleased = (StatusByte bsr 3) band 1,
    SecondSafetyReleased = (StatusByte bsr 4) band 1,
    CapacitorCharged = (StatusByte bsr 5) band 1,
    AttackModeEntered = (StatusByte bsr 6) band 1,
    SelfDestructMode = (StatusByte bsr 7) band 1,
    
    BoolToText = fun(1) -> <<"是"/utf8>>; (0) -> <<"否"/utf8>>; (_) -> <<"未知"/utf8>> end,
    
    FormatString = "战斗部状态字0 (0x~2.16.0B):~n"
                   "  设备正常: ~ts (位0)~n"
                   "  自检完成: ~ts (位1)~n"
                   "  预留位: ~ts (位2)~n"
                   "  第一道保险解除: ~ts (位3)~n"
                   "  第二道保险解除: ~ts (位4)~n"
                   "  电容充电: ~ts (位5)~n"
                   "  进入攻击模式: ~ts (位6)~n"
                   "  自毁模式: ~ts (位7)~n"
                   "  原始值: 0x~2.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   StatusByte,
                   BoolToText(DeviceNormal),
                   BoolToText(SelfTestComplete),
                   BoolToText(Reserved),
                   BoolToText(FirstSafetyReleased),
                   BoolToText(SecondSafetyReleased),
                   BoolToText(CapacitorCharged),
                   BoolToText(AttackModeEntered),
                   BoolToText(SelfDestructMode),
                   StatusByte
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

-spec get_warhead_status1_details(#drone_status_d3{}) -> binary().
get_warhead_status1_details(Status) when is_record(Status, drone_status_d3) ->
    StatusByte = Status#drone_status_d3.warhead_status1,
    
    ConductiveMembraneValid = (StatusByte bsr 0) band 1,
    DetonationFlag = (StatusByte bsr 1) band 1,
    Reserved1 = (StatusByte bsr 2) band 1,
    Reserved2 = (StatusByte bsr 3) band 1,
    Reserved3 = (StatusByte bsr 4) band 1,
    IsolationStatus = (StatusByte bsr 5) band 1,
    WingDeployed = (StatusByte bsr 6) band 1,
    Reserved4 = (StatusByte bsr 7) band 1,
    
    BoolToText = fun(1) -> <<"是"/utf8>>; (0) -> <<"否"/utf8>>; (_) -> <<"未知"/utf8>> end,
    
    FormatString = "战斗部状态字1 (0x~2.16.0B):~n"
                   "  导电膜有效: ~ts (位0)~n"
                   "  起爆标志: ~ts (位1)~n"
                   "  预留位1: ~ts (位2)~n"
                   "  预留位2: ~ts (位3)~n"
                   "  预留位3: ~ts (位4)~n"
                   "  隔离状态: ~ts (位5)~n"
                   "  开翼状态: ~ts (位6)~n"
                   "  预留位4: ~ts (位7)~n"
                   "  原始值: 0x~2.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   StatusByte,
                   BoolToText(ConductiveMembraneValid),
                   BoolToText(DetonationFlag),
                   BoolToText(Reserved1),
                   BoolToText(Reserved2),
                   BoolToText(Reserved3),
                   BoolToText(IsolationStatus),
                   BoolToText(WingDeployed),
                   BoolToText(Reserved4),
                   StatusByte
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

-spec get_flight_mode_details(integer()) -> binary().
get_flight_mode_details(FlightModeBits) ->
    ThrottleMode = (FlightModeBits bsr 13) band 16#07,
    LongitudinalMode = (FlightModeBits bsr 10) band 16#07,
    LateralMode = (FlightModeBits bsr 7) band 16#07,
    Reserved = (FlightModeBits bsr 6) band 1,
    InAirFlag = (FlightModeBits bsr 5) band 1,
    CircleFlag = (FlightModeBits bsr 4) band 1,
    NavigationMode = FlightModeBits band 16#0F,
    
    ThrottleModeText = get_throttle_mode_text(ThrottleMode),
    LongitudinalModeText = get_longitudinal_mode_text(LongitudinalMode),
    LateralModeText = get_lateral_mode_text(LateralMode),
    NavigationModeText = get_navigation_mode_text(NavigationMode),
    
    BoolToText = fun(1) -> <<"是"/utf8>>; (0) -> <<"否"/utf8>>; (_) -> <<"未知"/utf8>> end,
    
    FormatString = "飞行模式 (0x~4.16.0B):~n"
                   "  油门模式: ~ts (位13-15: 0x~2.16.0B)~n"
                   "  纵向模式: ~ts (位10-12: 0x~2.16.0B)~n"
                   "  横向模式: ~ts (位7-9: 0x~2.16.0B)~n"
                   "  预留位: ~ts (位6)~n"
                   "  空中标志: ~ts (位5)~n"
                   "  盘旋标志: ~ts (位4)~n"
                   "  导航模式: ~ts (位0-3: 0x~2.16.0B)~n"
                   "  原始值: 0x~4.16.0B",
    
    FormattedString = io_lib:format(FormatString,
                  [
                   FlightModeBits,
                   ThrottleModeText, ThrottleMode,
                   LongitudinalModeText, LongitudinalMode,
                   LateralModeText, LateralMode,
                   BoolToText(Reserved),
                   BoolToText(InAirFlag),
                   BoolToText(CircleFlag),
                   NavigationModeText, NavigationMode,
                   FlightModeBits
                  ]),
    
    unicode:characters_to_binary(FormattedString, utf8, utf8).

%% 辅助文本函数
get_drone_type_text(Type) ->
    case Type of
        2 -> <<"任务型"/utf8>>;
        4 -> <<"训练型"/utf8>>;
        _ -> <<"未知类型"/utf8>>
    end.

format_switch_status(Status) when is_record(Status, drone_status_d3) ->
    Byte = Status#drone_status_d3.switch_status,
    SoftSwitch1 = (Byte bsr 0) band 1,
    SoftSwitch2 = (Byte bsr 1) band 1,
    LeftWingSwitch = (Byte bsr 2) band 1,
    RightWingSwitch = (Byte bsr 3) band 1,
    HardSwitchMeasure = (Byte bsr 4) band 1,
    BoolToText = fun(1) -> <<"开"/utf8>>; (0) -> <<"关"/utf8>>; (_) -> <<"未知"/utf8>> end,
    io_lib:format("软开关1:~ts, 软开关2:~ts, 左机翼开关:~ts, 右机翼开关:~ts, 硬开关测量:~ts",
                  [BoolToText(SoftSwitch1), BoolToText(SoftSwitch2),
                   BoolToText(LeftWingSwitch), BoolToText(RightWingSwitch),
                   BoolToText(HardSwitchMeasure)]).

get_throttle_mode_text(0) -> <<"手动油门"/utf8>>;
get_throttle_mode_text(1) -> <<"自动油门"/utf8>>;
get_throttle_mode_text(2) -> <<"高度保持"/utf8>>;
get_throttle_mode_text(_) -> <<"未知油门模式"/utf8>>.

get_longitudinal_mode_text(0) -> <<"手动俯仰"/utf8>>;
get_longitudinal_mode_text(1) -> <<"姿态保持"/utf8>>;
get_longitudinal_mode_text(2) -> <<"高度保持"/utf8>>;
get_longitudinal_mode_text(3) -> <<"速度保持"/utf8>>;
get_longitudinal_mode_text(4) -> <<"导航模式"/utf8>>;
get_longitudinal_mode_text(5) -> <<"返航模式"/utf8>>;
get_longitudinal_mode_text(6) -> <<"降落模式"/utf8>>;
get_longitudinal_mode_text(_) -> <<"未知纵向模式"/utf8>>.

get_lateral_mode_text(0) -> <<"手动横滚"/utf8>>;
get_lateral_mode_text(1) -> <<"姿态保持"/utf8>>;
get_lateral_mode_text(2) -> <<"航向保持"/utf8>>;
get_lateral_mode_text(3) -> <<"导航模式"/utf8>>;
get_lateral_mode_text(4) -> <<"返航模式"/utf8>>;
get_lateral_mode_text(_) -> <<"未知横向模式"/utf8>>.

get_navigation_mode_text(0) -> <<"手动模式"/utf8>>;
get_navigation_mode_text(1) -> <<"GPS导航"/utf8>>;
get_navigation_mode_text(2) -> <<"航点导航"/utf8>>;
get_navigation_mode_text(3) -> <<"返航模式"/utf8>>;
get_navigation_mode_text(4) -> <<"降落模式"/utf8>>;
get_navigation_mode_text(5) -> <<"跟随模式"/utf8>>;
get_navigation_mode_text(6) -> <<"盘旋模式"/utf8>>;
get_navigation_mode_text(_) -> <<"未知导航模式"/utf8>>.

%%%===================================================================
%%% 完整解析函数
%%%===================================================================

-spec parse_complete(binary()) -> {ok, #drone_status_d3{}} | {error, term()}.
parse_complete(<<
    ?D3_FRAME_HEADER:16/little,
    FrameLength:8,
    DroneModel:8,
    DroneId:16/little,
    CommandId:8,
    Data/binary
>>) ->
    ExpectedDataLength = FrameLength - 7,
    case byte_size(Data) of
        ExpectedDataLength ->
            case parse(Data) of
                {ok, Status} ->
                    UpdatedStatus = Status#drone_status_d3{
                        frame_length = FrameLength,
                        drone_model = DroneModel,
                        drone_id = DroneId,
                        command_id = CommandId
                    },
                    {ok, UpdatedStatus};
                Error ->
                    Error
            end;
        _ ->
            {error, {invalid_data_length, {expected, ExpectedDataLength}, {actual, byte_size(Data)}}}
    end;
parse_complete(_) ->
    {error, invalid_frame_header}.

-spec parse_full(binary()) -> {ok, binary()} | {error, term()}.
parse_full(Packet) ->
    case parse_complete(Packet) of
        {ok, Status} ->
            DetailedReport = format_detailed(Status),
            {ok, DetailedReport};
        Error ->
            Error
    end.