%%%-------------------------------------------------------------------
%%% @doc
%%% d2_data.erl - D2遥测数据解析器
%%%
%%% 根据协议文档 D2.docx 解析无人机状态信息（0xD2）
%%% 包括：滚转角给定、俯仰角给定、角速率、加速度、校准参数、载荷状态等。
%%% 数据从命令标识符之后开始，共115字节，按小端字节序解析。
%%% 每个字段按照协议表格中的缩放因子转换为物理量。
%%%
%%% 主要功能：
%%% - parse/1: 解析D2数据，返回 #drone_status_d2 记录
%%% - format/1: 格式化输出带中文描述的状态信息
%%% - 内部函数：解析载荷状态字、飞行模态等
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(d2_data).
-export([parse/1,  format/1, get_flight_mode_details/1]).

-include_lib("dgiot_uav/include/d2_data.hrl").

%% @doc 解析115字节的D2数据（从命令标识符之后开始）
parse(<<
    
    %% 字节8：引战帧频 (1字节) - 表格字节21
    WarheadFrameFreq:8,
    
    %% 字节9-10：滚转角给定×10 (2字节，有符号小端) - 表格字节22-23
    RollAngleGivenRaw:16/little-signed,
    
    %% 字节11-12：俯仰角给定×10 (2字节，有符号小端) - 表格字节24-25
    PitchAngleGivenRaw:16/little-signed,
    
    %% 字节13：空速给定 (1字节) - 表格字节26
    AirspeedGiven:8,
    
    %% 字节14：板载导航状态 (1字节) - 表格字节27
    OnboardNavStatus:8,
    
    %% 字节15-16：原点距离 (2字节，小端) - 表格字节28-29
    OriginDistance:16/little,
    
    %% 字节17-18：原点方位×10 (2字节，小端) - 表格字节30-31
    OriginAzimuthRaw:16/little,
    
    %% 字节19-20：待飞距 (2字节，小端) - 表格字节32-33
    DistanceToGo:16/little,
    
    %% 字节21-22：侧偏距×10 (2字节，有符号小端) - 表格字节34-35
    LateralDeviationRaw:16/little-signed,
    
    %% 字节23-24：指令高度×10 (2字节，有符号小端) - 表格字节36-37
    CommandAltitudeRaw:16/little-signed,
    
    %% 字节25-26：转速 (2字节，小端) - 表格字节38-39
    RotationSpeed:16/little,
    
    %% 字节27-28：飞行总时间 (2字节，小端) - 表格字节40-41
    TotalFlightTime:16/little,
    
    %% 字节29：飞行架次 (1字节) - 表格字节42
    FlightSortie:8,
    
    %% 字节30：遥控帧频 (1字节) - 表格字节43
    RemoteFrameFreq:8,
    
    %% 字节31：发射筒帧频 (1字节) - 表格字节44
    LaunchTubeFrameFreq:8,
    
    %% 字节32：载荷帧频 (1字节) - 表格字节45
    PayloadFrameFreq:8,
    
    %% 字节33：磁航向帧频 (1字节) - 表格字节46
    MagneticHeadingFrameFreq:8,
    
    %% 字节34：卫导帧频 (1字节) - 表格字节47
    GPSFrameFreq:8,
    
    %% 字节35：北斗帧频 (1字节) - 表格字节48
    BeidouFrameFreq:8,
    
    %% 字节36-37：卫导相对高度×10 (2字节，有符号小端) - 表格字节49-50
    GPSRelativeAltitudeRaw:16/little-signed,
    
    %% 字节38-39：气压相对高度×10 (2字节，有符号小端) - 表格字节51-52
    BaroRelativeAltitudeRaw:16/little-signed,
    
    %% 字节40-41：飞行模态 (2字节，小端) - 表格字节53-54
    FlightModeBits:16/little,
    
    %% 字节42：北斗定位状态及军民使用状态 (1字节) - 表格字节55
    BeidouStatus:8,
    
    %% 字节43：发射标志 (1字节) - 表格字节56
    LaunchFlag:8,
    
    %% 字节44：拉起高度 (1字节) - 表格字节57
    PullUpHeight:8,
    
    %% 字节45-46：角速率x×100 (2字节，有符号小端) - 表格字节58-59
    AngularRateXRaw:16/little-signed,
    
    %% 字节47-48：角速率y×100 (2字节，有符号小端) - 表格字节60-61
    AngularRateYRaw:16/little-signed,
    
    %% 字节49-50：角速率z×100 (2字节，有符号小端) - 表格字节62-63
    AngularRateZRaw:16/little-signed,
    
    %% 字节51-52：加速度x×100 (2字节，有符号小端) - 表格字节64-65
    AccelerationXRaw:16/little-signed,
    
    %% 字节53-54：加速度y×100 (2字节，有符号小端) - 表格字节66-67
    AccelerationYRaw:16/little-signed,
    
    %% 字节55-56：加速度z×100 (2字节，有符号小端) - 表格字节68-69
    AccelerationZRaw:16/little-signed,
    
    %% 字节57：俯仰角校准值×10 (1字节，有符号) - 表格字节70
    PitchCalibrationRaw:8/signed,
    
    %% 字节58：滚转角校准值×10 (1字节，有符号) - 表格字节71
    RollCalibrationRaw:8/signed,
    
    %% 字节59：航向角校准值×10 (1字节，有符号) - 表格字节72
    HeadingCalibrationRaw:8/signed,
    
    %% 字节60：升降舵校准值×10 (1字节，有符号) - 表格字节73
    ElevatorCalibrationRaw:8/signed,
    
    %% 字节61：副翼校准值×10 (1字节，有符号) - 表格字节74
    AileronCalibrationRaw:8/signed,
    
    %% 字节62：方向舵校准值×10 (1字节，有符号) - 表格字节75
    RudderCalibrationRaw:8/signed,
    
    %% 字节63-64：空速校准系数×1000 (2字节，小端) - 表格字节76-77
    AirspeedCalibrationCoefRaw:16/little,
    
    %% 字节65-66：空速校准偏移量×10 (2字节，小端) - 表格字节78-79
    AirspeedCalibrationOffsetRaw:16/little,
    
    %% 字节67：空速零偏×10 (1字节，有符号) - 表格字节80
    AirspeedZeroOffsetRaw:8/signed,
    
    %% 字节68：俯仰角积分×10 (1字节，有符号) - 表格字节81
    PitchIntegralRaw:8/signed,
    
    %% 字节69：高度积分×10 (1字节，有符号) - 表格字节82
    AltitudeIntegralRaw:8/signed,
    
    %% 字节70：下滑段高度积分×10 (1字节，有符号) - 表格字节83
    GlideAltitudeIntegralRaw:8/signed,
    
    %% 字节71：空速到油门积分×100 (1字节，有符号) - 表格字节84
    AirspeedToThrottleIntegralRaw:8/signed,
    
    %% 字节72：滚转角积分×10 (1字节，有符号) - 表格字节85
    RollIntegralRaw:8/signed,
    
    %% 字节73：加速度积分×10 (1字节，有符号) - 表格字节86
    AccelerationIntegralRaw:8/signed,
    
    %% 字节74：空速到俯仰角积分×10 (1字节，有符号) - 表格字节87
    AirspeedToPitchIntegralRaw:8/signed,
    
    %% 字节75：侧偏距修正 (1字节) - 表格字节88
    LateralDeviationCorrection:8,
    
    %% 字节76：预留 (1字节) - 表格字节89
    Reserved1:8,
    
    %% 字节77：载荷状态字0 (1字节) - 表格字节90
    PayloadStatus0:8,
    
    %% 字节78：载荷状态字1 (1字节) - 表格字节91
    PayloadStatus1:8,
    
    %% 字节79-80：载荷目标相对高 (2字节，小端) - 表格字节92-93
    PayloadTargetRelativeHeight:16/little,
    
    %% 字节81：目标相对高标志 (1字节) - 表格字节94
    TargetRelativeHeightFlag:8,
    
    %% 字节82-101：预留 (20字节) - 表格字节95-114
    Reserved2:20/binary,
    
    %% 字节102：舵面动作状态 (1字节) - 表格字节115
    ControlSurfaceStatus:8,
    
    %% 字节103-110：预留 (8字节) - 表格字节116-123
    Reserved3:8/binary,
    
    %% 字节111-112：预留 (2字节) - 表格字节124-125
    Reserved4:2/binary,
    
    %% 字节113-114：CRC校验 (2字节，小端) - 表格字节127-128
    CRC:16/little,
    _Rest/binary>> ) ->
    
    % 计算实际值（根据表格的缩放因子）
    RollAngleGiven = RollAngleGivenRaw / 10.0,
    PitchAngleGiven = PitchAngleGivenRaw / 10.0,
    OriginAzimuth = OriginAzimuthRaw / 10.0,
    LateralDeviation = LateralDeviationRaw / 10.0,
    CommandAltitude = CommandAltitudeRaw / 10.0,
    GPSRelativeAltitude = GPSRelativeAltitudeRaw / 10.0,
    BaroRelativeAltitude = BaroRelativeAltitudeRaw / 10.0,
    AngularRateX = AngularRateXRaw / 100.0,
    AngularRateY = AngularRateYRaw / 100.0,
    AngularRateZ = AngularRateZRaw / 100.0,
    AccelerationX = AccelerationXRaw / 100.0,
    AccelerationY = AccelerationYRaw / 100.0,
    AccelerationZ = AccelerationZRaw / 100.0,
    PitchCalibration = PitchCalibrationRaw / 10.0,
    RollCalibration = RollCalibrationRaw / 10.0,
    HeadingCalibration = HeadingCalibrationRaw / 10.0,
    ElevatorCalibration = ElevatorCalibrationRaw / 10.0,
    AileronCalibration = AileronCalibrationRaw / 10.0,
    RudderCalibration = RudderCalibrationRaw / 10.0,
    AirspeedCalibrationCoef = AirspeedCalibrationCoefRaw / 1000.0,
    AirspeedCalibrationOffset = AirspeedCalibrationOffsetRaw / 10.0,
    AirspeedZeroOffset = AirspeedZeroOffsetRaw / 10.0,
    PitchIntegral = PitchIntegralRaw / 10.0,
    AltitudeIntegral = AltitudeIntegralRaw / 10.0,
    GlideAltitudeIntegral = GlideAltitudeIntegralRaw / 10.0,
    AirspeedToThrottleIntegral = AirspeedToThrottleIntegralRaw / 100.0,
    RollIntegral = RollIntegralRaw / 10.0,
    AccelerationIntegral = AccelerationIntegralRaw / 10.0,
    AirspeedToPitchIntegral = AirspeedToPitchIntegralRaw / 10.0,
    
    % 创建状态记录
    Status = #drone_status_d2{
        warhead_frame_freq = WarheadFrameFreq,
        roll_angle_given = RollAngleGiven,
        pitch_angle_given = PitchAngleGiven,
        airspeed_given = AirspeedGiven,
        onboard_nav_status = OnboardNavStatus,
        
        origin_distance = OriginDistance,
        origin_azimuth = OriginAzimuth,
        distance_to_go = DistanceToGo,
        lateral_deviation = LateralDeviation,
        command_altitude = CommandAltitude,
        
        rotation_speed = RotationSpeed,
        total_flight_time = TotalFlightTime,
        flight_sortie = FlightSortie,
        remote_frame_freq = RemoteFrameFreq,
        launch_tube_frame_freq = LaunchTubeFrameFreq,
        payload_frame_freq = PayloadFrameFreq,
        magnetic_heading_frame_freq = MagneticHeadingFrameFreq,
        gps_frame_freq = GPSFrameFreq,
        beidou_frame_freq = BeidouFrameFreq,
        
        gps_relative_altitude = GPSRelativeAltitude,
        baro_relative_altitude = BaroRelativeAltitude,
        
        flight_mode_bits = FlightModeBits,
        beidou_status = BeidouStatus,
        launch_flag = LaunchFlag,
        pull_up_height = PullUpHeight,
        
        angular_rate_x = AngularRateX,
        angular_rate_y = AngularRateY,
        angular_rate_z = AngularRateZ,
        acceleration_x = AccelerationX,
        acceleration_y = AccelerationY,
        acceleration_z = AccelerationZ,
        
        pitch_calibration = PitchCalibration,
        roll_calibration = RollCalibration,
        heading_calibration = HeadingCalibration,
        elevator_calibration = ElevatorCalibration,
        aileron_calibration = AileronCalibration,
        rudder_calibration = RudderCalibration,
        
        airspeed_calibration_coef = AirspeedCalibrationCoef,
        airspeed_calibration_offset = AirspeedCalibrationOffset,
        airspeed_zero_offset = AirspeedZeroOffset,
        
        pitch_integral = PitchIntegral,
        altitude_integral = AltitudeIntegral,
        glide_altitude_integral = GlideAltitudeIntegral,
        airspeed_to_throttle_integral = AirspeedToThrottleIntegral,
        roll_integral = RollIntegral,
        acceleration_integral = AccelerationIntegral,
        airspeed_to_pitch_integral = AirspeedToPitchIntegral,
        
        lateral_deviation_correction = LateralDeviationCorrection,
        reserved1 = Reserved1,
        
        payload_status0 = PayloadStatus0,
        payload_status1 = PayloadStatus1,
        payload_target_relative_height = PayloadTargetRelativeHeight,
        target_relative_height_flag = TargetRelativeHeightFlag,
        
        reserved2 = Reserved2,
        control_surface_status = ControlSurfaceStatus,
        reserved3 = Reserved3,
        reserved4 = Reserved4,
        crc = CRC
    },
    
    {ok, Status};

%% @doc 处理数据格式错误
parse(Binary) ->
    io:format("~ts: 无法解析数据，前16字节: ~p~n", 
              [<<"D2数据"/utf8>>, binary:part(Binary, 0, min(16, byte_size(Binary)))]),
    {error, invalid_data_format}.

%% ==================== 新增：载荷状态字解析函数 ====================
%% @doc 获取载荷状态字0的详细描述（备注4）
get_payload_status0_details(Byte) ->
    PayloadType = case (Byte bsr 5) band 16#07 of
        16#05 -> <<"平台式可见光"/utf8>>;
        16#06 -> <<"平台式红外白热"/utf8>>;
        16#07 -> <<"平台式红外黑热"/utf8>>;
        _ -> <<"未知载荷类型"/utf8>>
    end,
    Compression = case (Byte bsr 3) band 16#03 of
        0 -> <<"压缩模式0"/utf8>>;
        _ -> <<"未知压缩模式"/utf8>>
    end,
    ImageStab = case (Byte bsr 2) band 1 of
        0 -> <<"稳像关"/utf8>>;
        1 -> <<"稳像开"/utf8>>
    end,
    WorkState = case Byte band 16#03 of
        0 -> <<"载荷休眠"/utf8>>;
        1 -> <<"工作(手动调节)"/utf8>>;
        2 -> <<"工作(自动调节)"/utf8>>;
        3 -> <<"故障"/utf8>>
    end,
    io_lib:format("载荷类型:~ts, 压缩模式:~ts, 稳像:~ts, 工作状态:~ts",
                  [PayloadType, Compression, ImageStab, WorkState]).

%% @doc 获取载荷状态字1的详细描述（备注5）
get_payload_status1_details(Byte) ->
    IrZoom = case (Byte bsr 6) band 16#03 of
        0 -> <<"红外预留"/utf8>>;
        1 -> <<"红外1x"/utf8>>;
        2 -> <<"红外2x"/utf8>>;
        3 -> <<"红外3x"/utf8>>
    end,
    VisZoom = case (Byte bsr 3) band 16#07 of
        0 -> <<"可见光预置"/utf8>>;
        1 -> <<"可见光1x"/utf8>>;
        2 -> <<"可见光2x"/utf8>>;
        3 -> <<"可见光3x"/utf8>>;
        4 -> <<"可见光4x"/utf8>>;
        5 -> <<"可见光5x"/utf8>>;
        _ -> <<"可见光未知"/utf8>>
    end,
    ImgEnhance = case (Byte bsr 1) band 16#03 of
        0 -> <<"不增强"/utf8>>;
        _ -> <<"增强未知"/utf8>>
    end,
    Protect = case Byte band 1 of
        0 -> <<"工作态"/utf8>>;
        1 -> <<"保护态"/utf8>>
    end,
    io_lib:format("红外变倍:~ts, 可见光变倍:~ts, 图像增强:~ts, 载荷保护:~ts",
                  [IrZoom, VisZoom, ImgEnhance, Protect]).

%% ==================== 新增：飞行模态详细解析（备注3） ====================
%% @doc 获取飞行模态详细描述（按备注3）
get_flight_mode_details(FlightModeBits) ->
    ThrottleMode = case (FlightModeBits bsr 13) band 16#07 of
        0 -> <<"遥控"/utf8>>;
        1 -> <<"空速控制"/utf8>>;
        2 -> <<"着陆油门"/utf8>>;
        _ -> <<"未知油门模态"/utf8>>
    end,
    LongitudinalMode = case (FlightModeBits bsr 10) band 16#07 of
        0 -> <<"起飞控制"/utf8>>;
        1 -> <<"俯冲"/utf8>>;
        2 -> <<"高度控制"/utf8>>;
        3 -> <<"爬升"/utf8>>;
        4 -> <<"高度斜坡控制"/utf8>>;
        5 -> <<"攻击导引"/utf8>>;
        6 -> <<"遥控"/utf8>>;
        _ -> <<"未知纵向模态"/utf8>>
    end,
    LateralMode = case (FlightModeBits bsr 7) band 16#07 of
        0 -> <<"遥控"/utf8>>;
        1 -> <<"滚转角控制"/utf8>>;
        2 -> <<"航向控制"/utf8>>;
        3 -> <<"航迹控制"/utf8>>;
        4 -> <<"攻击控制"/utf8>>;
        _ -> <<"未知横向模态"/utf8>>
    end,
    InAir = case (FlightModeBits bsr 5) band 1 of
        1 -> <<"在空中"/utf8>>;
        0 -> <<"在地上"/utf8>>
    end,
    Circle = case (FlightModeBits bsr 4) band 1 of
        1 -> <<"盘旋"/utf8>>;
        0 -> <<"不盘旋"/utf8>>
    end,
    NavMode = case FlightModeBits band 16#0F of
        0 -> <<"无导引"/utf8>>;
        1 -> <<"自主起飞"/utf8>>;
        2 -> <<"航线导引"/utf8>>;
        3 -> <<"绕点左盘导"/utf8>>;
        4 -> <<"自动回收降落导引"/utf8>>;
        5 -> <<"攻击导引"/utf8>>;
        6 -> <<"复飞导引"/utf8>>;
        _ -> <<"未知导航模态"/utf8>>
    end,
    io_lib:format("油门:~ts, 纵向:~ts, 横向:~ts, ~ts, ~ts, 导航:~ts",
                  [ThrottleMode, LongitudinalMode, LateralMode, InAir, Circle, NavMode]).

%% ==================== 修改点：format函数中调用新增解析 ====================
%% @doc 格式化状态信息
format(Status) when is_record(Status, drone_status_d2) ->
    % 解析北斗状态
    BeidouPositionValid = (Status#drone_status_d2.beidou_status band ?BEIDOU_POSITION_VALID) =/= 0,
    BeidouCodeType = Status#drone_status_d2.beidou_status band 16#03,

    % 获取飞行模态详细描述
    FlightModeInfo = get_flight_mode_details(Status#drone_status_d2.flight_mode_bits),

    % 获取载荷状态字详细描述
    PayloadStatus0Info = get_payload_status0_details(Status#drone_status_d2.payload_status0),
    PayloadStatus1Info = get_payload_status1_details(Status#drone_status_d2.payload_status1),

    % 简化格式化输出，使用更简单的格式
    FormattedString = io_lib:format("D2状态:~n"
                  "  角度: 横滚=~.1f°, 俯仰=~.1f°, 空速=~pm/s~n"
                  "  导航状态: 0x~2.16.0B~n"
                  "  导航信息: 原点距离=~pm, 原点方位=~.1f°, 剩余距离=~pm~n"
                  "            横向偏差=~.1fm, 指令高度=~.1fm~n"
                  "  飞行信息: 转速=~pRPM, 总飞行时间=~pmin, 架次=~p~n"
                  "  帧频率: 遥控=~p, GPS=~p, 北斗=~p~n"
                  "  高度: GPS高度=~.1fm, 气压高度=~.1fm~n"
                  "  飞行模态: ~ts~n"
                  "  北斗状态: 有效=~p, 码型=~p~n"
                  "  发射状态: 标志=~p, 拉起高度=~pm~n"
                  "  角速率 (X,Y,Z)=~.2f,~.2f,~.2f°/s~n"
                  "  加速度 (X,Y,Z)=~.2f,~.2f,~.2fm/s²~n"
                  "  载荷状态0: ~ts~n"
                  "  载荷状态1: ~ts~n"
                  "  控制面状态: ~p~n",
                  [
                   Status#drone_status_d2.roll_angle_given,
                   Status#drone_status_d2.pitch_angle_given,
                   Status#drone_status_d2.airspeed_given,
                   Status#drone_status_d2.onboard_nav_status,
                   Status#drone_status_d2.origin_distance,
                   Status#drone_status_d2.origin_azimuth,
                   Status#drone_status_d2.distance_to_go,
                   Status#drone_status_d2.lateral_deviation,
                   Status#drone_status_d2.command_altitude,
                   Status#drone_status_d2.rotation_speed,
                   Status#drone_status_d2.total_flight_time,
                   Status#drone_status_d2.flight_sortie,
                   Status#drone_status_d2.remote_frame_freq,
                   Status#drone_status_d2.gps_frame_freq,
                   Status#drone_status_d2.beidou_frame_freq,
                   Status#drone_status_d2.gps_relative_altitude,
                   Status#drone_status_d2.baro_relative_altitude,
                   FlightModeInfo,
                   BeidouPositionValid,
                   BeidouCodeType,
                   Status#drone_status_d2.launch_flag,
                   Status#drone_status_d2.pull_up_height,
                   Status#drone_status_d2.angular_rate_x,
                   Status#drone_status_d2.angular_rate_y,
                   Status#drone_status_d2.angular_rate_z,
                   Status#drone_status_d2.acceleration_x,
                   Status#drone_status_d2.acceleration_y,
                   Status#drone_status_d2.acceleration_z,
                   PayloadStatus0Info,
                   PayloadStatus1Info,
                   Status#drone_status_d2.control_surface_status
                  ]),
    % 转换为UTF-8字符串
    D2 = unicode:characters_to_list(FormattedString, utf8),
    io:format("~ts ~n ",[D2]),
    D2.