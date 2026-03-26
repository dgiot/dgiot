%%%-------------------------------------------------------------------
%%% @doc
%%% control_protocol.erl - 无人机控制协议处理模块
%%%
%%% 本模块负责解析和构建控制命令帧，包括：
%%% - 开关命令、航点切换、载荷控制等
%%% - 遥控指令（EB90 帧）的构建和名称映射
%%%
%%% 协议对应：procotol.docx 中的遥控指令表（开关指令、载荷控制等）
%%% 命令标识符见协议表2。载荷部分以 A5 5A 开头，包含飞机型号、ID、
%%% 命令ID、命令数据和CRC。
%%%
%%% 主要功能：
%%% - build_remote_control_frame/3,4: 构建 EB90 遥控指令帧（调用 eb90_link_protocol）
%%% - remote_command_name/1: 获取遥控指令中文名称（委托给 uav_protocol_utils）
%%% - parse_control_frame/1: 解析完整控制帧（先由 frame_decoder 解码，再解析载荷）
%%% - build_control_frame/2: 构建控制帧（根据类型和参数生成载荷，再由 frame_encoder 编码）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(control_protocol).

%% API
-export([
    parse_control_frame/1,                 % 解析完整控制帧
    build_control_frame/2,                  % 构建控制帧
    build_remote_control_frame/3,            % 构建 EB90 遥控指令帧（3参数）
    build_remote_control_frame/4,            % 构建 EB90 遥控指令帧（4参数，带指令值）
    remote_command_name/1,                   % 遥控指令中文名称
    %% 以下为原有工具函数（转发至 utils 模块）
    parse_platform_type/1,
    switch_command_str/1,
    waypoint_str/1,
    payload_sub_command_str/1,
    payload_continuous_command_str/1,
    rudder_channel_str/1
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").

%%%===================================================================
%%% 外部依赖：链路层帧构建
%%%===================================================================
%% 注意：实际 EB90 帧构建由 eb90_link_protocol 模块负责
%% 这里通过调用其接口实现

%%%===================================================================
%%% 遥控指令相关
%%%===================================================================

%% @doc 构建 EB90 遥控指令帧（标准版）
-spec build_remote_control_frame(DestAddr :: integer(), SrcAddr :: integer(), CmdCode :: integer()) ->
    binary() | {error, term()}.
build_remote_control_frame(DestAddr, SrcAddr, CmdCode) ->
    build_remote_control_frame(DestAddr, SrcAddr, CmdCode, 0).

%% @doc 构建 EB90 遥控指令帧（带指令值）
-spec build_remote_control_frame(DestAddr :: integer(), SrcAddr :: integer(), CmdCode :: integer(), Value :: integer()) ->
    binary() | {error, term()}.
build_remote_control_frame(DestAddr, SrcAddr, CmdCode, Value) ->
    eb90_link_protocol:build_remote_control_frame(DestAddr, SrcAddr, CmdCode, Value).

%% @doc 获取遥控指令中文名称 - 委托给工具模块
-spec remote_command_name(integer()) -> binary().
remote_command_name(Code) ->
    uav_protocol_utils:remote_command_name(Code).

%%%===================================================================
%%% 控制帧解析
%%%===================================================================

parse_control_frame(Data) when is_binary(Data) ->
    case frame_decoder:decode_frame(Data) of
        {ok, Frame, _Rest} ->
            case frame_decoder:validate_frame(Frame) of
                {ok, ValidFrame} ->
                    DataTypeLow = (ValidFrame#uav_frame.data_type) band 16#0F,
                    if DataTypeLow =:= ?CONTROL_DATA_TYPE_LOW ->
                        parse_control_payload(ValidFrame#uav_frame.payload,
                                              ValidFrame#uav_frame.dest_addr,
                                              ValidFrame#uav_frame.src_addr,
                                              (ValidFrame#uav_frame.data_type) bsr 4);
                       true ->
                        {error, {invalid_control_type, DataTypeLow}}
                    end;
                {error, Reason} ->
                    {error, {crc_validation_failed, Reason}}
            end;
        {incomplete, _Buffer} ->
            {error, incomplete_frame};
        {error, Reason} ->
            {error, {decode_error, Reason}}
    end.

parse_control_payload(<<?PAYLOAD_SUB_HEADER:16, PlaneType:8,
                        PlaneID:16, CommandID:8, CommandData/binary>>,
                      DestAddr, SrcAddr, PlatformType) ->
    Size = byte_size(CommandData),
    if Size >= 2 ->
        DataWithoutCRC = binary:part(CommandData, 0, Size - 2),
        parse_control_by_id(CommandID, DataWithoutCRC, DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID);
       true ->
        {error, command_data_too_short}
    end;
parse_control_payload(_, _, _, _) ->
    {error, invalid_payload_header}.

parse_control_by_id(?CMD_SWITCH, <<SwitchCommand:8, _Reserved:12/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => switch_command,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           switch_command => SwitchCommand,
           command_name => uav_protocol_utils:switch_command_str(SwitchCommand)}};
parse_control_by_id(?CMD_WAYPOINT_SWITCH, <<_Reserved1:8, WaypointIndex:8, _Reserved2:11/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => waypoint_switch,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           waypoint_index => WaypointIndex,
           waypoint_desc => uav_protocol_utils:waypoint_str(WaypointIndex)}};
parse_control_by_id(?CMD_PAYLOAD_CONTROL, <<SubCommand:8, _Reserved:14/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => payload_control,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           sub_command => SubCommand,
           sub_command_name => uav_protocol_utils:payload_sub_command_str(SubCommand)}};
parse_control_by_id(?CMD_PAYLOAD_CONTINUOUS, <<SubCommand:8, _Reserved1:8, Elevation:16/little-signed,
                                                Azimuth:16/little-signed, _Reserved2:9/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => payload_continuous_control,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           sub_command => SubCommand,
           sub_command_name => uav_protocol_utils:payload_continuous_command_str(SubCommand),
           elevation => Elevation * 0.1,
           azimuth => Azimuth * 0.1}};
parse_control_by_id(?CMD_ROUTE_UPLOAD, <<Latitude:32/little-signed, Longitude:32/little-signed,
                                          _Reserved:16, Altitude:16/little-signed,
                                          TotalWaypoints:8, WaypointSeq:8, _Reserved2:2/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => route_upload,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           latitude => Latitude / 10000000,
           longitude => Longitude / 10000000,
           altitude => Altitude,
           total_waypoints => TotalWaypoints,
           waypoint_sequence => WaypointSeq}};
parse_control_by_id(?CMD_FLIGHT_TIME, <<TotalTime:16/little-signed, SortieCount:8,
                                          _Reserved:12/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => flight_time_sortie,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           total_flight_time => TotalTime,
           sortie_count => SortieCount}};
parse_control_by_id(?CMD_RUDDER_CALIB, <<Channel:8/signed, _Reserved1:16, PWMCenter:16/little-signed,
                                          _Reserved2:16, UpRatio:8/signed, DownRatio:8/signed,
                                          _Reserved3:8/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => rudder_calibration,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           channel => Channel,
           channel_name => uav_protocol_utils:rudder_channel_str(Channel),
           pwm_center => PWMCenter,
           up_ratio => UpRatio * 0.02,
           down_ratio => DownRatio * 0.02}};
parse_control_by_id(?CMD_AIRSPEED_CALIB, <<Scale:16/little-signed, Offset:16/little-signed,
                                            _Reserved:10/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => airspeed_calibration,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           scale => Scale * 0.001,
           offset => Offset * 0.1}};
parse_control_by_id(?CMD_ET_ROUTE, <<Latitude:32/little-signed, Longitude:32/little-signed,
                                      _Reserved:16, Altitude:16/little-signed,
                                      TotalWaypoints:8, WaypointSeq:8, _Reserved2:2/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => et_route,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           latitude => Latitude / 10000000,
           longitude => Longitude / 10000000,
           altitude => Altitude,
           total_waypoints => TotalWaypoints,
           waypoint_sequence => WaypointSeq}};
parse_control_by_id(?CMD_ID_SETTING, <<NewPlaneType:8, NewPlaneID:16/little,
                                        _Reserved:11/binary>>,
                    DestAddr, SrcAddr, PlatformType, PlaneType, PlaneID) ->
    {ok, #{type => id_setting,
           dest_addr => DestAddr,
           src_addr => SrcAddr,
           platform_type => uav_protocol_utils:parse_platform_type(PlatformType),
           plane_type => PlaneType,
           plane_id => PlaneID,
           new_plane_type => NewPlaneType,
           new_plane_id => NewPlaneID}};
parse_control_by_id(CmdID, _Data, _DestAddr, _SrcAddr, _PlatformType, _PlaneType, _PlaneID) ->
    {error, {unsupported_command_id, CmdID}}.

%%%===================================================================
%%% 控制帧构建
%%%===================================================================

build_control_frame(Type, Params) ->
    try
        Payload = build_command_payload(Type, Params),
        EncodeParams = #encode_params{
            dest_addr = maps:get(dest_addr, Params, ?DEFAULT_DEST_ADDR),
            src_addr = maps:get(src_addr, Params, ?DEFAULT_SRC_ADDR),
            platform_type = maps:get(platform_type, Params, ?PLATFORM_206),
            data_type_low = ?CONTROL_DATA_TYPE_LOW,
            frame_count = maps:get(frame_count, Params, 1),
            key = maps:get(key, Params, 0),
            payload = Payload,
            timestamp = erlang:system_time(millisecond)
        },
        frame_encoder:encode_frame(EncodeParams)
    catch
        _:Exception -> {error, {build_error, Exception}}
    end.

%% ==================== 构建命令载荷（内部） ====================
-spec build_command_payload(atom(), map()) -> binary().
build_command_payload(switch_command, #{plane_type := PlaneType, plane_id := PlaneID,
                                        switch_cmd := SwitchCmd}) ->
    CommandData = <<SwitchCmd:8, 0:96>>,
    build_payload_with_header(?CMD_SWITCH, PlaneType, PlaneID, CommandData);

build_command_payload(waypoint_switch, #{plane_type := PlaneType, plane_id := PlaneID,
                                          waypoint_index := WaypointIndex}) ->
    CommandData = <<0:8, WaypointIndex:8, 0:88>>,
    build_payload_with_header(?CMD_WAYPOINT_SWITCH, PlaneType, PlaneID, CommandData);

build_command_payload(payload_control, #{plane_type := PlaneType, plane_id := PlaneID,
                                          sub_command := SubCommand}) ->
    CommandData = <<SubCommand:8, 0:96>>,
    build_payload_with_header(?CMD_PAYLOAD_CONTROL, PlaneType, PlaneID, CommandData);

build_command_payload(payload_continuous_control, #{plane_type := PlaneType, plane_id := PlaneID,
                                                     sub_command := SubCommand, elevation := Elevation,
                                                     azimuth := Azimuth}) ->
    ElevInt = trunc(Elevation * 10),
    AzInt = trunc(Azimuth * 10),
    CommandData = <<SubCommand:8, 0:8, ElevInt:16/little-signed, AzInt:16/little-signed, 0:56>>,
    build_payload_with_header(?CMD_PAYLOAD_CONTINUOUS, PlaneType, PlaneID, CommandData);

build_command_payload(route_upload, #{plane_type := PlaneType, plane_id := PlaneID,
                                       latitude := Lat, longitude := Lon, altitude := Alt,
                                       total_waypoints := Total, waypoint_sequence := Seq}) ->
    LatInt = trunc(Lat * 10000000),
    LonInt = trunc(Lon * 10000000),
    CommandData = <<LatInt:32/little-signed, LonInt:32/little-signed, 0:16, Alt:16/little-signed,
                    Total:8, Seq:8, 0:16>>,
    build_payload_with_header(?CMD_ROUTE_UPLOAD, PlaneType, PlaneID, CommandData);

build_command_payload(flight_time_sortie, #{plane_type := PlaneType, plane_id := PlaneID,
                                             total_flight_time := TotalTime, sortie_count := Sortie}) ->
    CommandData = <<TotalTime:16/little-signed, Sortie:8, 0:88>>,
    build_payload_with_header(?CMD_FLIGHT_TIME, PlaneType, PlaneID, CommandData);

build_command_payload(rudder_calibration, #{plane_type := PlaneType, plane_id := PlaneID,
                                             channel := Channel, pwm_center := PWMCenter,
                                             up_ratio := UpRatio, down_ratio := DownRatio}) ->
    UpInt = trunc(UpRatio / 0.02),
    DownInt = trunc(DownRatio / 0.02),
    CommandData = <<Channel:8/signed, 0:16, PWMCenter:16/little-signed, 0:16,
                    UpInt:8/signed, DownInt:8/signed, 0:64>>,
    build_payload_with_header(?CMD_RUDDER_CALIB, PlaneType, PlaneID, CommandData);

build_command_payload(airspeed_calibration, #{plane_type := PlaneType, plane_id := PlaneID,
                                               scale := Scale, offset := Offset}) ->
    ScaleInt = trunc(Scale / 0.001),
    OffsetInt = trunc(Offset / 0.1),
    CommandData = <<ScaleInt:16/little-signed, OffsetInt:16/little-signed, 0:80>>,
    build_payload_with_header(?CMD_AIRSPEED_CALIB, PlaneType, PlaneID, CommandData);

build_command_payload(et_route, Params) ->
    % 与 route_upload 相同
    build_command_payload(route_upload, Params);

build_command_payload(id_setting, #{plane_type := PlaneType, plane_id := PlaneID,
                                      new_plane_type := NewType, new_plane_id := NewID}) ->
    CommandData = <<NewType:8, NewID:16/little, 0:88>>,
    build_payload_with_header(?CMD_ID_SETTING, PlaneType, PlaneID, CommandData).

%% 构建完整载荷（包含子帧头 + 命令数据）
build_payload_with_header(CmdID, PlaneType, PlaneID, CommandData) ->
    PayloadSize = byte_size(CommandData) + 1,  % 命令ID占1字节
    <<?PAYLOAD_SUB_HEADER:16, PayloadSize:8, PlaneType:8, PlaneID:16,
      CmdID:8, CommandData/binary>>.

%%%===================================================================
%%% 工具函数转发（保持原有接口）
%%%===================================================================
parse_platform_type(Code) -> uav_protocol_utils:parse_platform_type(Code).
switch_command_str(Cmd) -> uav_protocol_utils:switch_command_str(Cmd).
waypoint_str(Idx) -> uav_protocol_utils:waypoint_str(Idx).
payload_sub_command_str(Cmd) -> uav_protocol_utils:payload_sub_command_str(Cmd).
payload_continuous_command_str(Cmd) -> uav_protocol_utils:payload_continuous_command_str(Cmd).
rudder_channel_str(Ch) -> uav_protocol_utils:rudder_channel_str(Ch).