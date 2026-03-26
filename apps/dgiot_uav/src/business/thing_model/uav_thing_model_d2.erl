%%%-------------------------------------------------------------------
%%% @doc
%%% D2帧物模型转换模块
%%% 将 D2 状态记录转换为物模型 Map（展开飞行模态、北斗状态、载荷状态）
%%% @end
%%%-------------------------------------------------------------------
-module(uav_thing_model_d2).

-export([convert/1]).

-include("d2_data.hrl").
-include_lib("dgiot/include/logger.hrl").

%% @doc 将 D2 状态记录转换为物模型 Map
-spec convert(#drone_status_d2{}) -> map().
convert(#drone_status_d2{
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
    payload_status0 = PayloadStatus0,
    payload_status1 = PayloadStatus1,
    payload_target_relative_height = PayloadTargetRelativeHeight,
    target_relative_height_flag = TargetRelativeHeightFlag,
    control_surface_status = ControlSurfaceStatus
}) ->
    %% 解析飞行模态位（备注3）
    ThrottleMode    = (FlightModeBits bsr 13) band 16#07,
    LongitudinalMode= (FlightModeBits bsr 10) band 16#07,
    LateralMode     = (FlightModeBits bsr 7)  band 16#07,
    InAir           = (FlightModeBits bsr 5)  band 1,
    CircleMode      = (FlightModeBits bsr 4)  band 1,
    NavigationMode  = FlightModeBits band 16#0F,

    %% 解析北斗状态（字节55） - 北斗定位状态及军民使用状态
    BeidouCodeType        = BeidouStatus band 16#03,      % 位0-1: 码型
    BeidouReservedBit2    = (BeidouStatus bsr 2) band 1,  % 位2: 预留
    BeidouPosValid        = (BeidouStatus bsr 3) band 1,  % 位3: 位置有效
    BeidouMilitaryCivil   = (BeidouStatus bsr 4) band 16#0F, % 位4-7: 军民使用状态

    %% 解析载荷状态字0（备注4）
    PayloadType       = (PayloadStatus0 bsr 5) band 16#07,
    PayloadCompMode   = (PayloadStatus0 bsr 3) band 16#03,
    PayloadImageStab  = (PayloadStatus0 bsr 2) band 1,
    PayloadWorkState  = PayloadStatus0 band 16#03,

    %% 解析载荷状态字1（备注5）
    IrZoom            = (PayloadStatus1 bsr 6) band 16#03,
    VisZoom           = (PayloadStatus1 bsr 3) band 16#07,
    ImageEnhance      = (PayloadStatus1 bsr 1) band 16#03,
    PayloadProtect    = PayloadStatus1 band 1,

    %% 飞行模态文本描述
    FlightModeText = unicode:characters_to_binary(d2_data:get_flight_mode_details(FlightModeBits)),

    #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"warhead_frame_freq">> => WarheadFrameFreq,
        <<"roll_angle_given">> => RollAngleGiven,
        <<"pitch_angle_given">> => PitchAngleGiven,
        <<"airspeed_given">> => AirspeedGiven,
        <<"onboard_nav_status">> => OnboardNavStatus,
        <<"origin_distance">> => OriginDistance,
        <<"origin_azimuth">> => OriginAzimuth,
        <<"distance_to_go">> => DistanceToGo,
        <<"lateral_deviation">> => LateralDeviation,
        <<"command_altitude">> => CommandAltitude,
        <<"rotation_speed">> => RotationSpeed,
        <<"total_flight_time">> => TotalFlightTime,
        <<"flight_sortie">> => FlightSortie,
        <<"remote_frame_freq">> => RemoteFrameFreq,
        <<"launch_tube_frame_freq">> => LaunchTubeFrameFreq,
        <<"payload_frame_freq">> => PayloadFrameFreq,
        <<"magnetic_heading_frame_freq">> => MagneticHeadingFrameFreq,
        <<"gps_frame_freq">> => GPSFrameFreq,
        <<"beidou_frame_freq">> => BeidouFrameFreq,
        <<"gps_relative_altitude">> => GPSRelativeAltitude,
        <<"baro_relative_altitude">> => BaroRelativeAltitude,
        <<"throttle_mode">> => ThrottleMode,
        <<"longitudinal_mode">> => LongitudinalMode,
        <<"lateral_mode">> => LateralMode,
        <<"in_air">> => InAir,
        <<"circle_mode">> => CircleMode,
        <<"navigation_mode">> => NavigationMode,
        <<"flight_mode_text">> => FlightModeText,
        <<"beidou_code_type">> => BeidouCodeType,
        <<"beidou_reserved_bit2">> => BeidouReservedBit2,
        <<"beidou_position_valid">> => BeidouPosValid,
        <<"beidou_military_civil">> => BeidouMilitaryCivil,
        <<"launch_flag">> => LaunchFlag,
        <<"pull_up_height">> => PullUpHeight,
        <<"angular_rate_x">> => AngularRateX,
        <<"angular_rate_y">> => AngularRateY,
        <<"angular_rate_z">> => AngularRateZ,
        <<"acceleration_x">> => AccelerationX,
        <<"acceleration_y">> => AccelerationY,
        <<"acceleration_z">> => AccelerationZ,
        <<"pitch_calibration">> => PitchCalibration,
        <<"roll_calibration">> => RollCalibration,
        <<"heading_calibration">> => HeadingCalibration,
        <<"elevator_calibration">> => ElevatorCalibration,
        <<"aileron_calibration">> => AileronCalibration,
        <<"rudder_calibration">> => RudderCalibration,
        <<"airspeed_calibration_coef">> => AirspeedCalibrationCoef,
        <<"airspeed_calibration_offset">> => AirspeedCalibrationOffset,
        <<"airspeed_zero_offset">> => AirspeedZeroOffset,
        <<"pitch_integral">> => PitchIntegral,
        <<"altitude_integral">> => AltitudeIntegral,
        <<"glide_altitude_integral">> => GlideAltitudeIntegral,
        <<"airspeed_to_throttle_integral">> => AirspeedToThrottleIntegral,
        <<"roll_integral">> => RollIntegral,
        <<"acceleration_integral">> => AccelerationIntegral,
        <<"airspeed_to_pitch_integral">> => AirspeedToPitchIntegral,
        <<"lateral_deviation_correction">> => LateralDeviationCorrection,
        <<"payload_type">> => PayloadType,
        <<"payload_compression_mode">> => PayloadCompMode,
        <<"payload_image_stabilization">> => PayloadImageStab,
        <<"payload_work_state">> => PayloadWorkState,
        <<"ir_zoom">> => IrZoom,
        <<"vis_zoom">> => VisZoom,
        <<"image_enhance">> => ImageEnhance,
        <<"payload_protect_state">> => PayloadProtect,
        <<"payload_target_relative_height">> => PayloadTargetRelativeHeight,
        <<"target_relative_height_flag">> => TargetRelativeHeightFlag,
        <<"control_surface_status">> => ControlSurfaceStatus
    }.