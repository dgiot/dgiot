%%%-------------------------------------------------------------------
%%% @doc
%%% payload_to_fc.erl - 载荷到飞控（Payload-to-FC）协议处理模块
%%%
%%% 本模块负责编码和解码从载荷发送给飞控的状态数据帧。
%%% 协议对应：Payload.docx 中的“任务载荷数据发送给飞控导航系统的帧格式”
%%% 有两个版本：基础版（50字节，20220601之前）和扩展版（50字节，增加MEMS数据，20220701之后）
%%% 帧头 AA 55，后跟状态字、角度、脱靶量、MEMS数据、时间戳、CRC等。
%%%
%%% 主要功能：
%%% - encode_basic/1: 编码基础版载荷状态帧
%%% - encode_extended/1: 编码扩展版载荷状态帧
%%% - decode/1: 解码（自动识别基础版或扩展版）
%%% - parse_status0/1: 解析状态字0（按表格9）
%%% - parse_status1/1: 解析状态字1（按表格10）
%%% - convert_angle/2: 将原始值转换为角度
%%% - convert_rate/2: 将原始值转换为角速率
%%% - convert_accel/2: 将原始值转换为加速度
%%% - convert_imu_temp/1: 将原始值转换为温度
%%% - validate_data_range/1: 验证数据范围是否有效
%%% - default_basic_status/0: 返回默认的基础版状态记录
%%% - default_extended_status/0: 返回默认的扩展版状态记录
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(payload_to_fc).

%% 基本类型定义
-define(UINT8, 8/unsigned-little-integer).
-define(INT16, 16/signed-little-integer).
-define(UINT16, 16/unsigned-little-integer).

%% 同步字节
-define(PL_SYNC_BYTES, <<16#AA, 16#55>>).

%% 错误码
-define(ERR_INVALID_SYNC, invalid_sync).
-define(ERR_CRC_MISMATCH, crc_mismatch).

%% 状态0位掩码
-define(STATUS0_PLATFORM_MASK, 16#E0).      %% 1110 0000
-define(STATUS0_COMPRESS_MASK, 16#18).      %% 0001 1000
-define(STATUS0_STABILIZE_MASK, 16#04).     %% 0000 0100
-define(STATUS0_WORK_MODE_MASK, 16#03).     %% 0000 0011

%% 状态1位掩码
-define(STATUS1_IR_ZOOM_MASK, 16#C0).       %% 1100 0000
-define(STATUS1_VIS_ZOOM_MASK, 16#38).      %% 0011 1000
-define(STATUS1_ENHANCE_MASK, 16#02).       %% 0000 0010
-define(STATUS1_PROTECTION_MASK, 16#01).    %% 0000 0001

%% 平台类型
-define(PLATFORM_VISIBLE, 0).
-define(PLATFORM_IR_WHITE, 1).
-define(PLATFORM_IR_BLACK, 2).

%% 工作模式
-define(WORK_MODE_SLEEP, 0).
-define(WORK_MODE_MANUAL, 1).
-define(WORK_MODE_AUTO, 2).
-define(WORK_MODE_FAULT, 3).

%% 载荷类型
-define(PAYLOAD_TYPE_VISIBLE, 0).
-define(PAYLOAD_TYPE_IR, 1).

%% 跟踪标志
-define(TRACKING_OFF, 0).
-define(TRACKING_ON, 1).

%% 伺服状态
-define(SERVO_UNCALIBRATED, 0).
-define(SERVO_CALIBRATED, 1).

%% 命令类型
-define(CMD_NULL, 0).
-define(CMD_START_TRACKING, 1).
-define(CMD_STOP_TRACKING, 2).
-define(CMD_SET_TARGET, 3).

%% 记录定义
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
    reserved1 = <<0:136>> :: binary(),  %% 17 bytes
    received_command = 0 :: integer(),
    crc16 = 0 :: integer()
}).

-record(payload_to_fc_extended, {
    status0 = 0 :: integer(),
    status1 = 0 :: integer(),
    payload_type_zoom = 0 :: integer(),
    elevation_real = 0 :: integer(),
    azimuth_real = 0 :: integer(),
    target_offset_x = 0 :: integer(),
    target_offset_y = 0 :: integer(),
    debug_data = <<0:32>> :: binary(),  %% 4 bytes
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
    reserved2 = <<0:80>> :: binary(),   %% 10 bytes
    servo_timestamp = 0 :: integer(),
    image_timestamp = 0 :: integer(),
    received_command = 0 :: integer(),
    crc16 = 0 :: integer()
}).

-export([encode_basic/1, encode_extended/1, 
         decode/1, decode_extended/1,
         parse_status0/1, parse_status1/1,
         convert_angle/2, convert_rate/2, convert_accel/2,
         convert_imu_temp/1, validate_data_range/1,
         default_basic_status/0, default_extended_status/0,
         test_encode_decode/0]).





%% @spec encode_basic(Record::#payload_to_fc_basic{}) -> binary()
encode_basic(#payload_to_fc_basic{} = Status) ->
    % 
    Status0 = Status#payload_to_fc_basic.status0,
    Status1 = Status#payload_to_fc_basic.status1,
    PayloadTypeZoom = Status#payload_to_fc_basic.payload_type_zoom,
    ElevReal = Status#payload_to_fc_basic.elevation_real,
    AzReal = Status#payload_to_fc_basic.azimuth_real,
    TargetX = Status#payload_to_fc_basic.target_offset_x,
    TargetY = Status#payload_to_fc_basic.target_offset_y,
    ElevTarget = Status#payload_to_fc_basic.elevation_target,
    AzTarget = Status#payload_to_fc_basic.azimuth_target,
    AircraftPitch = Status#payload_to_fc_basic.aircraft_pitch,
    AircraftRoll = Status#payload_to_fc_basic.aircraft_roll,
    AircraftYaw = Status#payload_to_fc_basic.aircraft_yaw,
    TrackingFlag = Status#payload_to_fc_basic.tracking_flag,
    FrameCount = Status#payload_to_fc_basic.frame_count,
    Status2 = Status#payload_to_fc_basic.status2,
    ElevRate = Status#payload_to_fc_basic.elevation_rate,
    AzRate = Status#payload_to_fc_basic.azimuth_rate,
    Reserved1 = Status#payload_to_fc_basic.reserved1,
    ReceivedCmd = Status#payload_to_fc_basic.received_command,
    
    % （3-48）
    Data = <<
        Status0:?UINT8,
        Status1:?UINT8,
        PayloadTypeZoom:?UINT8,
        ElevReal:?INT16,
        AzReal:?UINT16,
        TargetX:?INT16,
        TargetY:?INT16,
        ElevTarget:?INT16,
        AzTarget:?UINT16,
        AircraftPitch:?INT16,
        AircraftRoll:?INT16,
        AircraftYaw:?UINT16,
        TrackingFlag:?UINT8,
        FrameCount:?UINT8,
        Status2:?UINT8,
        ElevRate:?INT16,
        AzRate:?INT16,
        Reserved1/binary,
        ReceivedCmd:?UINT8
    >>,
    
    % CRC16（3-48）
    CRC16 = uav_payload_checksum:calculate_crc16(Data),
    
    % 
    <<
        ?PL_SYNC_BYTES/binary,
        Data/binary,
        CRC16:?UINT16
    >>.


%% @spec encode_extended(Record::#payload_to_fc_extended{}) -> binary()
encode_extended(#payload_to_fc_extended{} = Status) ->
    % 
    Status0 = Status#payload_to_fc_extended.status0,
    Status1 = Status#payload_to_fc_extended.status1,
    PayloadTypeZoom = Status#payload_to_fc_extended.payload_type_zoom,
    ElevReal = Status#payload_to_fc_extended.elevation_real,
    AzReal = Status#payload_to_fc_extended.azimuth_real,
    TargetX = Status#payload_to_fc_extended.target_offset_x,
    TargetY = Status#payload_to_fc_extended.target_offset_y,
    DebugData = Status#payload_to_fc_extended.debug_data,
    AccelX = Status#payload_to_fc_extended.accel_x,
    AccelY = Status#payload_to_fc_extended.accel_y,
    AccelZ = Status#payload_to_fc_extended.accel_z,
    TrackingFlag = Status#payload_to_fc_extended.tracking_flag,
    FrameCount = Status#payload_to_fc_extended.frame_count,
    Status2 = Status#payload_to_fc_extended.status2,
    GyroX = Status#payload_to_fc_extended.gyro_x,
    GyroY = Status#payload_to_fc_extended.gyro_y,
    GyroZ = Status#payload_to_fc_extended.gyro_z,
    ImuTemp = Status#payload_to_fc_extended.imu_temp,
    Reserved2 = Status#payload_to_fc_extended.reserved2,
    ServoTime = Status#payload_to_fc_extended.servo_timestamp,
    ImageTime = Status#payload_to_fc_extended.image_timestamp,
    ReceivedCmd = Status#payload_to_fc_extended.received_command,
    
    % （3-48）
    Data = <<
        Status0:?UINT8,
        Status1:?UINT8,
        PayloadTypeZoom:?UINT8,
        ElevReal:?INT16,
        AzReal:?UINT16,
        TargetX:?INT16,
        TargetY:?INT16,
        DebugData/binary,
        AccelX:?INT16,
        AccelY:?INT16,
        AccelZ:?INT16,
        TrackingFlag:?UINT8,
        FrameCount:?UINT8,
        Status2:?UINT8,
        GyroX:?INT16,
        GyroY:?INT16,
        GyroZ:?INT16,
        ImuTemp:?UINT8,
        Reserved2/binary,
        ServoTime:?UINT16,
        ImageTime:?UINT16,
        ReceivedCmd:?UINT8
    >>,
    
    % CRC16（3-48）
    CRC16 = uav_payload_checksum:calculate_crc16(Data),
    
    % 
    <<
        ?PL_SYNC_BYTES/binary,
        Data/binary,
        CRC16:?UINT16
    >>.




%% @spec decode(Data::binary()) -> {ok, Record, Version} | {error, Reason}
decode(Data) when is_binary(Data) ->
    case Data of
        % 
        <<16#AA, 16#55, _/binary>> ->
            case byte_size(Data) of
                50 -> decode_basic(Data);
                _ -> decode_extended(Data)
            end;
        _ ->
            {error, ?ERR_INVALID_SYNC}
    end.


decode_basic(<<16#AA, 16#55, Status0:?UINT8, Status1:?UINT8,
               PayloadTypeZoom:?UINT8, ElevReal:?INT16, AzReal:?UINT16,
               TargetX:?INT16, TargetY:?INT16, ElevTarget:?INT16,
               AzTarget:?UINT16, AircraftPitch:?INT16, AircraftRoll:?INT16,
               AircraftYaw:?UINT16, TrackingFlag:?UINT8, FrameCount:?UINT8,
               Status2:?UINT8, ElevRate:?INT16, AzRate:?INT16,
               Reserved1:17/binary, ReceivedCmd:?UINT8, CRC16:?UINT16>>) ->
    
    % CRC16
    Data = <<Status0:?UINT8, Status1:?UINT8, PayloadTypeZoom:?UINT8,
             ElevReal:?INT16, AzReal:?UINT16, TargetX:?INT16, TargetY:?INT16,
             ElevTarget:?INT16, AzTarget:?UINT16, AircraftPitch:?INT16,
             AircraftRoll:?INT16, AircraftYaw:?UINT16, TrackingFlag:?UINT8,
             FrameCount:?UINT8, Status2:?UINT8, ElevRate:?INT16, AzRate:?INT16,
             Reserved1/binary, ReceivedCmd:?UINT8>>,
    
    case uav_payload_checksum:calculate_crc16(Data) of
        CRC16 ->
            Record = #payload_to_fc_basic{
                status0 = Status0,
                status1 = Status1,
                payload_type_zoom = PayloadTypeZoom,
                elevation_real = ElevReal,
                azimuth_real = AzReal,
                target_offset_x = TargetX,
                target_offset_y = TargetY,
                elevation_target = ElevTarget,
                azimuth_target = AzTarget,
                aircraft_pitch = AircraftPitch,
                aircraft_roll = AircraftRoll,
                aircraft_yaw = AircraftYaw,
                tracking_flag = TrackingFlag,
                frame_count = FrameCount,
                status2 = Status2,
                elevation_rate = ElevRate,
                azimuth_rate = AzRate,
                received_command = ReceivedCmd,
                crc16 = CRC16
            },
            {ok, Record, basic};
        _ ->
            {error, ?ERR_CRC_MISMATCH}
    end.


decode_extended(<<16#AA, 16#55, Status0:?UINT8, Status1:?UINT8,
                  PayloadTypeZoom:?UINT8, ElevReal:?INT16, AzReal:?UINT16,
                  TargetX:?INT16, TargetY:?INT16, DebugData:4/binary,
                  AccelX:?INT16, AccelY:?INT16, AccelZ:?INT16,
                  TrackingFlag:?UINT8, FrameCount:?UINT8, Status2:?UINT8,
                  GyroX:?INT16, GyroY:?INT16, GyroZ:?INT16,
                  ImuTemp:?UINT8, Reserved2:10/binary,
                  ServoTime:?UINT16, ImageTime:?UINT16,
                  ReceivedCmd:?UINT8, CRC16:?UINT16>>) ->
    
    % CRC16
    Data = <<Status0:?UINT8, Status1:?UINT8, PayloadTypeZoom:?UINT8,
             ElevReal:?INT16, AzReal:?UINT16, TargetX:?INT16, TargetY:?INT16,
             DebugData/binary, AccelX:?INT16, AccelY:?INT16, AccelZ:?INT16,
             TrackingFlag:?UINT8, FrameCount:?UINT8, Status2:?UINT8,
             GyroX:?INT16, GyroY:?INT16, GyroZ:?INT16, ImuTemp:?UINT8,
             Reserved2/binary, ServoTime:?UINT16, ImageTime:?UINT16,
             ReceivedCmd:?UINT8>>,
    
    case uav_payload_checksum:calculate_crc16(Data) of
        CRC16 ->
            Record = #payload_to_fc_extended{
                status0 = Status0,
                status1 = Status1,
                payload_type_zoom = PayloadTypeZoom,
                elevation_real = ElevReal,
                azimuth_real = AzReal,
                target_offset_x = TargetX,
                target_offset_y = TargetY,
                debug_data = DebugData,
                accel_x = AccelX,
                accel_y = AccelY,
                accel_z = AccelZ,
                tracking_flag = TrackingFlag,
                frame_count = FrameCount,
                status2 = Status2,
                gyro_x = GyroX,
                gyro_y = GyroY,
                gyro_z = GyroZ,
                imu_temp = ImuTemp,
                servo_timestamp = ServoTime,
                image_timestamp = ImageTime,
                received_command = ReceivedCmd,
                crc16 = CRC16
            },
            {ok, Record, extended};
        _ ->
            {error, ?ERR_CRC_MISMATCH}
    end.




%% @spec parse_status0(Status0::integer()) -> map()
parse_status0(Status0) ->
    PlatformType = (Status0 band ?STATUS0_PLATFORM_MASK) bsr 5,
    CompressMode = (Status0 band ?STATUS0_COMPRESS_MASK) bsr 3,
    StabilizeMode = (Status0 band ?STATUS0_STABILIZE_MASK) bsr 2,
    WorkMode = Status0 band ?STATUS0_WORK_MODE_MASK,
    
    #{
        platform_type => get_platform_name(PlatformType),
        compress_mode => get_compress_mode(CompressMode),
        stabilize_mode => StabilizeMode =:= 1,
        work_mode => get_work_mode(WorkMode),
        raw_value => Status0
    }.


%% @spec parse_status1(Status1::integer()) -> map()
parse_status1(Status1) ->
    IrZoom = (Status1 band ?STATUS1_IR_ZOOM_MASK) bsr 6,
    VisZoom = (Status1 band ?STATUS1_VIS_ZOOM_MASK) bsr 3,
    EnhanceMode = (Status1 band ?STATUS1_ENHANCE_MASK) bsr 1,
    ProtectionMode = Status1 band ?STATUS1_PROTECTION_MASK,
    
    #{
        ir_zoom_level => IrZoom + 1,  % 1-4
        visible_zoom_level => VisZoom + 1,  % 1-8
        enhance_mode => EnhanceMode =:= 1,
        protection_mode => ProtectionMode =:= 1,
        raw_value => Status1
    }.


get_platform_name(?PLATFORM_VISIBLE) -> "";
get_platform_name(?PLATFORM_IR_WHITE) -> "";
get_platform_name(?PLATFORM_IR_BLACK) -> "";
get_platform_name(_) -> "".


get_compress_mode(0) -> "00: ";
get_compress_mode(1) -> "01: ";
get_compress_mode(2) -> "10: ";
get_compress_mode(3) -> "11: ".


get_work_mode(?WORK_MODE_SLEEP) -> "";
get_work_mode(?WORK_MODE_MANUAL) -> "";
get_work_mode(?WORK_MODE_AUTO) -> "";
get_work_mode(?WORK_MODE_FAULT) -> "";
get_work_mode(_) -> "".




%% @spec convert_angle(RawValue::integer(), Resolution::float()) -> float()
convert_angle(RawValue, Resolution) ->
    RawValue * Resolution.


%% @spec convert_rate(RawValue::integer(), Resolution::float()) -> float()
convert_rate(RawValue, Resolution) ->
    RawValue * Resolution.


%% @spec convert_accel(RawValue::integer(), Resolution::float()) -> float()
convert_accel(RawValue, Resolution) ->
    RawValue * Resolution.


%% @spec convert_imu_temp(RawValue::integer()) -> float()
convert_imu_temp(RawValue) ->
    RawValue - 80.0.  % +80




%% @spec validate_data_range(Record) -> {ok} | {error, Field, Value, Range}
validate_data_range(#payload_to_fc_basic{} = Status) ->
    Checks = [
        {elevation_real, Status#payload_to_fc_basic.elevation_real, -6000, 3000},
        {azimuth_real, Status#payload_to_fc_basic.azimuth_real, 0, 36000},
        {elevation_rate, Status#payload_to_fc_basic.elevation_rate, -30000, 30000},
        {azimuth_rate, Status#payload_to_fc_basic.azimuth_rate, -30000, 30000}
    ],
    validate_ranges(Checks);
validate_data_range(#payload_to_fc_extended{} = Status) ->
    Checks = [
        {elevation_real, Status#payload_to_fc_extended.elevation_real, -6000, 3000},
        {azimuth_real, Status#payload_to_fc_extended.azimuth_real, 0, 36000},
        {accel_x, Status#payload_to_fc_extended.accel_x, -32768, 32767},
        {gyro_x, Status#payload_to_fc_extended.gyro_x, -32768, 32767}
    ],
    validate_ranges(Checks).

validate_ranges([]) -> ok;
validate_ranges([{Field, Value, Min, Max} | Rest]) ->
    if
        Value >= Min, Value =< Max ->
            validate_ranges(Rest);
        true ->
            {error, Field, Value, {Min, Max}}
    end.




%% @spec default_basic_status() -> #payload_to_fc_basic{}
default_basic_status() ->
    #payload_to_fc_basic{
        status0 = (5 bsl 5) bor ?WORK_MODE_MANUAL,  % ，
        status1 = (0 bsl 6) bor (0 bsl 3) bor (0 bsl 1),  % ，
        payload_type_zoom = (?PAYLOAD_TYPE_VISIBLE bsl 4) bor 1,  % ，1
        elevation_real = 0,
        azimuth_real = 0,
        target_offset_x = 0,
        target_offset_y = 0,
        elevation_target = 0,
        azimuth_target = 0,
        aircraft_pitch = 0,
        aircraft_roll = 0,
        aircraft_yaw = 0,
        tracking_flag = ?TRACKING_OFF,
        frame_count = 0,
        status2 = ?SERVO_UNCALIBRATED,
        elevation_rate = 0,
        azimuth_rate = 0,
        received_command = ?CMD_NULL
    }.


%% @spec default_extended_status() -> #payload_to_fc_extended{}
default_extended_status() ->
    #payload_to_fc_extended{
        status0 = (5 bsl 5) bor ?WORK_MODE_MANUAL,
        status1 = (0 bsl 6) bor (0 bsl 3) bor (0 bsl 1),
        payload_type_zoom = (?PAYLOAD_TYPE_VISIBLE bsl 4) bor 1,
        elevation_real = 0,
        azimuth_real = 0,
        target_offset_x = 0,
        target_offset_y = 0,
        accel_x = 0,
        accel_y = 0,
        accel_z = 9800,  % 9.8 m/s²
        tracking_flag = ?TRACKING_OFF,
        frame_count = 0,
        status2 = ?SERVO_UNCALIBRATED,
        gyro_x = 0,
        gyro_y = 0,
        gyro_z = 0,
        imu_temp = 25 + 80,  % 25°C
        servo_timestamp = 0,
        image_timestamp = 0,
        received_command = ?CMD_NULL
    }.



test_encode_decode() ->
    % 
    BasicStatus = default_basic_status(),
    BasicFrame = encode_basic(BasicStatus),
    io:format("Basic frame size: ~p bytes~n", [byte_size(BasicFrame)]),
    
    case decode(BasicFrame) of
        {ok, _DecodedBasic, basic} ->
            io:format("Decoded basic frame OK~n");
        {error, Reason} ->
            io:format("Decode basic error: ~p~n", [Reason])
    end,
    
    % 
    ExtendedStatus = default_extended_status(),
    ExtendedFrame = encode_extended(ExtendedStatus),
    io:format("Extended frame size: ~p bytes~n", [byte_size(ExtendedFrame)]),
    
    case decode(ExtendedFrame) of
        {ok, DecodedExtended, extended} ->
            % 
            Status0Info = parse_status0(DecodedExtended#payload_to_fc_extended.status0),
            Status1Info = parse_status1(DecodedExtended#payload_to_fc_extended.status1),
            io:format("Status0: ~p~n", [Status0Info]),
            io:format("Status1: ~p~n", [Status1Info]);
        {error, Reason2} ->
            io:format("Decode extended error: ~p~n", [Reason2])
    end,
    
    ok.