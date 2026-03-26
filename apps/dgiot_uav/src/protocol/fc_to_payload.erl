%%%-------------------------------------------------------------------
%%% @doc
%%% fc_to_payload.erl - 飞控到载荷（FC-to-Payload）协议处理模块
%%%
%%% 本模块负责构建和解析从飞控发送给载荷的控制命令帧。
%%% 协议对应：Payload.docx 中的“飞控导航系统发送给任务载荷数据帧格式”
%%% 帧格式：EB 90 + 命令码 + 参数C/D + 飞机参数（62字节） + 校验和
%%% 开关指令需连续发送三次有效命令，再发两次空命令（协议要求）。
%%%
%%% 主要功能：
%%% - encode/2: 编码命令（仅命令码+飞机参数）
%%% - encode/5: 编码完整命令帧（含参数C/D，可选择计算校验和）
%%% - decode/1: 解码命令帧
%%% - decode_aircraft_params/1: 解码飞机参数（62字节 -> 记录）
%%% - send_command/2: 发送命令（自动处理开关指令的重复发送）
%%% - send_switch_sequence/2: 发送多个开关指令序列
%%% - validate_command/1: 验证命令码是否有效
%%% - get_command_name/1: 获取命令名称
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fc_to_payload).

-export([
    encode/2, encode/5,
    decode/1,
    decode_aircraft_params/1,
    send_command/2,
    send_switch_sequence/2,
    validate_command/1,
    get_command_name/1,
    test_encode_decode/0
]).

-include_lib("dgiot_uav/include/dgiot_uav.hrl").
-include_lib("dgiot/include/logger.hrl").

%% 基本类型定义
-define(UINT8, 8/unsigned-little-integer).
-define(UINT16, 16/unsigned-little-integer).
-define(INT16, 16/signed-little-integer).
-define(INT32, 32/signed-little-integer).

%% 同步字节
-define(FC_SYNC_BYTES, <<16#EB, 16#90>>).

%% 飞机参数大小
-define(AIRCRAFT_PARAMS_SIZE, 32).

%% 角度分辨率
-define(RES_ANGLE, 0.01).

%% 命令码定义（根据载荷协议表格2）
-define(CMD_PAYLOAD_WORK, 16#D1).   % 任务设备工作
-define(CMD_PAYLOAD_SLEEP, 16#D2).  % 任务设备休眠
-define(CMD_PAYLOAD_PROTECT, 16#D3).% 保护态
-define(CMD_VISIBLE_LIGHT, 16#31).  % 切换可见光
-define(CMD_INFRARED, 16#33).       % 切换红外
-define(CMD_ZOOM_IN, 16#CA).        % 电子变倍加
-define(CMD_ZOOM_OUT, 16#56).       % 电子变倍减
-define(CMD_WHITE_HOT, 16#4D).      % 极性/白热
-define(CMD_BLACK_HOT, 16#4E).      % 极性/黑热
-define(CMD_BITRATE_2M, 16#80).     % 2M码率
-define(CMD_BITRATE_4M, 16#81).     % 4M码率
-define(CMD_NULL, 16#00).           % 空指令（协议表2中序号1？实际为0xFF？这里按文档）

%% 错误码
-define(ERR_INVALID_SYNC, invalid_sync).
-define(ERR_CHECKSUM_MISMATCH, checksum_mismatch).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @spec encode(Command::command_code(), AircraftParams::binary()) -> #fc_to_payload{}
%% 返回记录，不编码为二进制。
encode(Command, AircraftParams) when is_binary(AircraftParams),
                                      byte_size(AircraftParams) =:= ?AIRCRAFT_PARAMS_SIZE ->
    #fc_to_payload{
        command_code = Command,
        param_c = 0,
        param_d = 0,
        aircraft_params = AircraftParams,
        checksum = 0
    }.

%% @spec encode(Command, ParamC, ParamD, AircraftParams, CalcChecksum) -> binary()
%% 编码为二进制帧。如果 CalcChecksum 为 true，则计算校验和；否则校验和置0。
encode(Command, ParamC, ParamD, AircraftParams, true) when
      is_binary(AircraftParams), byte_size(AircraftParams) =:= ?AIRCRAFT_PARAMS_SIZE ->
    % 构建数据部分（不含同步头）
    Data = <<
        Command:?UINT8,
        ParamC:?UINT16,
        ParamD:?UINT16,
        AircraftParams/binary
    >>,
    % 计算校验和（同步头 + 数据）
    ChecksumData = <<?FC_SYNC_BYTES/binary, Data/binary>>,
    Checksum = uav_payload_checksum:calculate_checksum(ChecksumData),
    % 构建完整帧
    <<
        ?FC_SYNC_BYTES/binary,
        Data/binary,
        Checksum:?UINT8
    >>;
encode(Command, ParamC, ParamD, AircraftParams, false) ->
    <<
        ?FC_SYNC_BYTES/binary,
        Command:?UINT8,
        ParamC:?UINT16,
        ParamD:?UINT16,
        AircraftParams/binary,
        0:?UINT8
    >>.

%% @spec decode(Data::binary()) -> {ok, #fc_to_payload{}} | {error, Reason}
decode(<<16#EB, 16#90, Command:?UINT8, ParamC:?UINT16, ParamD:?UINT16,
         AircraftParams:?AIRCRAFT_PARAMS_SIZE/binary, Checksum:?UINT8>>) ->
    FrameData = <<?FC_SYNC_BYTES/binary, Command:?UINT8,
                  ParamC:?UINT16, ParamD:?UINT16, AircraftParams/binary>>,
    case uav_payload_checksum:verify_checksum(FrameData, Checksum) of
        true ->
            Frame = #fc_to_payload{
                command_code = Command,
                param_c = ParamC,
                param_d = ParamD,
                aircraft_params = AircraftParams,
                checksum = Checksum
            },
            {ok, Frame};
        false ->
            {error, ?ERR_CHECKSUM_MISMATCH}
    end;
decode(_) ->
    {error, ?ERR_INVALID_SYNC}.

%% @spec decode_aircraft_params(Data::binary()) -> #aircraft_params{}
%% 解析62字节的飞机参数，返回 #aircraft_params 记录。
decode_aircraft_params(Data) when is_binary(Data), byte_size(Data) =:= ?AIRCRAFT_PARAMS_SIZE ->
    <<
        AircraftId:?INT16,
        NavStatus:?UINT8,
        _Reserved1:7/binary,
        Latitude:?INT32,
        Longitude:?INT32,
        AltitudeGPS:?INT16,
        GroundSpeed:?INT16,
        Pitch:?INT16,
        Roll:?INT16,
        HeadingMagnetic:?UINT16,
        Airspeed:?UINT16,
        AltitudeBaro:?INT16,
        TrackAngle:?UINT16,
        SatelliteCount:?UINT8,
        _Reserved2:2/binary
    >> = Data,
    #aircraft_params{
        aircraft_id = AircraftId,
        nav_status = decode_nav_status(NavStatus),
        latitude = Latitude / 10000000.0,
        longitude = Longitude / 10000000.0,
        altitude_gps = AltitudeGPS * 0.2,
        ground_speed = GroundSpeed * 0.2,
        pitch = Pitch * ?RES_ANGLE,
        roll = Roll * ?RES_ANGLE,
        heading_magnetic = HeadingMagnetic * ?RES_ANGLE,
        airspeed = Airspeed * 0.5,
        altitude_baro = AltitudeBaro * 0.2,
        track_angle = TrackAngle * 0.1,
        satellite_count = SatelliteCount
    }.

%% @spec send_command(Command::command_code(), AircraftParams::binary()) -> [binary()]
%% 发送命令。如果是开关命令，生成3次有效命令 + 2次空命令；否则只生成1次。
send_command(Command, AircraftParams) ->
    EffectiveFrame = encode(Command, 0, 0, AircraftParams, true),
    case is_switch_command(Command) of
        true ->
            % 开关命令：发送3次有效命令 + 2次空命令
            EffectiveFrames = lists:duplicate(3, EffectiveFrame),
            NullFrame = encode(?CMD_NULL, 0, 0, AircraftParams, true),
            NullFrames = lists:duplicate(2, NullFrame),
            EffectiveFrames ++ NullFrames;
        false ->
            [EffectiveFrame]
    end.

%% @spec send_switch_sequence(Commands::[command_code()], AircraftParams::binary()) -> [binary()]
%% 发送多个开关指令序列，每个指令按 send_command 规则生成。
send_switch_sequence(Commands, AircraftParams) ->
    lists:flatmap(
        fun(Command) -> send_command(Command, AircraftParams) end,
        Commands
    ).

%% @spec validate_command(Command::integer()) -> boolean()
validate_command(Command) ->
    ValidCommands = [
        ?CMD_PAYLOAD_WORK, ?CMD_PAYLOAD_SLEEP, ?CMD_PAYLOAD_PROTECT,
        ?CMD_VISIBLE_LIGHT, ?CMD_INFRARED, ?CMD_ZOOM_IN, ?CMD_ZOOM_OUT,
        ?CMD_WHITE_HOT, ?CMD_BLACK_HOT, ?CMD_BITRATE_2M, ?CMD_BITRATE_4M,
        ?CMD_NULL
    ],
    lists:member(Command, ValidCommands).

%% @spec get_command_name(Command::integer()) -> string()
get_command_name(?CMD_PAYLOAD_WORK) -> "Payload Work";
get_command_name(?CMD_PAYLOAD_SLEEP) -> "Payload Sleep";
get_command_name(?CMD_PAYLOAD_PROTECT) -> "Payload Protect";
get_command_name(?CMD_VISIBLE_LIGHT) -> "Visible Light";
get_command_name(?CMD_INFRARED) -> "Infrared";
get_command_name(?CMD_ZOOM_IN) -> "Zoom In";
get_command_name(?CMD_ZOOM_OUT) -> "Zoom Out";
get_command_name(?CMD_WHITE_HOT) -> "White Hot";
get_command_name(?CMD_BLACK_HOT) -> "Black Hot";
get_command_name(?CMD_BITRATE_2M) -> "Bitrate 2M";
get_command_name(?CMD_BITRATE_4M) -> "Bitrate 4M";
get_command_name(?CMD_NULL) -> "Null";
get_command_name(_) -> "Unknown".

%%%===================================================================
%%% 内部函数
%%%===================================================================

is_switch_command(Command) ->
    SwitchCommands = [
        ?CMD_PAYLOAD_WORK, ?CMD_PAYLOAD_SLEEP, ?CMD_PAYLOAD_PROTECT,
        ?CMD_VISIBLE_LIGHT, ?CMD_INFRARED, ?CMD_ZOOM_IN, ?CMD_ZOOM_OUT,
        ?CMD_WHITE_HOT, ?CMD_BLACK_HOT
    ],
    lists:member(Command, SwitchCommands).

decode_nav_status(Status) ->
    #{
        bd_position => (Status band 1) =/= 0,
        gps_position => (Status band 2) =/= 0,
        integrated_nav => (Status band 4) =/= 0
    }.

%%%===================================================================
%%% 测试
%%%===================================================================

test_encode_decode() ->
    AircraftParams = <<
        0:?INT16,               % ID
        0:?UINT8,               % 导航状态
        0:56,                   % 7字节保留
        0:?INT32,               % 纬度
        0:?INT32,               % 经度
        0:?INT16,               % GPS高度
        0:?INT16,               % 地速
        0:?INT16,               % 俯仰
        0:?INT16,               % 横滚
        0:?UINT16,              % 磁航向
        0:?UINT16,              % 空速
        0:?INT16,               % 气压高度
        0:?UINT16,              % 航迹角
        0:?UINT8,               % 卫星数
        0:16                    % 2字节保留
    >>,
    Frame1 = encode(?CMD_PAYLOAD_WORK, AircraftParams),
    ?LOG(info, "Encoded frame size: ~p bytes", [byte_size(Frame1)]),
    case decode(Frame1) of
        {ok, DecodedFrame} ->
            ?LOG(info, "Decoded command: ~p (~s)",
                 [DecodedFrame#fc_to_payload.command_code,
                  get_command_name(DecodedFrame#fc_to_payload.command_code)]);
        {error, Reason} ->
            ?LOG(error, "Decode error: ~p", [Reason])
    end,
    SwitchFrames = send_command(?CMD_VISIBLE_LIGHT, AircraftParams),
    ?LOG(info, "Switch command frames: ~p frames", [length(SwitchFrames)]),
    ok.