%%%-------------------------------------------------------------------
%%% @doc
%%% telemetry_protocol.erl - 无人机遥测协议解析器模块（应用层）
%%% 修正：D1 数据增加 drone_id = PlaneID
%%%-------------------------------------------------------------------
-module(telemetry_protocol).

%% API
-export([
    parse_telemetry_frame/1,        % 解析完整遥测帧（入口）
    parse_telemetry_payload/4,       % 解析载荷部分（供框架层调用）
    parse_d1_data/6,                 % 解析 D1 数据
    parse_d2_data/6,                  % 解析 D2 数据
    parse_d3_data/6,                   % 解析 D3 数据
    parse_battery_data/6,             % 解析电池数据
    parse_version_data/6,             % 解析版本数据
    parse_waypoint_data/6,            % 解析航点数据
    parse_surface_calibration_data/6  % 解析舵面校准数据
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").
-include_lib("dgiot_uav/include/d1_data.hrl").
-include_lib("dgiot_uav/include/d2_data.hrl").
-include_lib("dgiot_uav/include/d3_data.hrl").

%% 命令字已经在 types.hrl 中定义
%% ?CMD_D1, ?CMD_D2, ?CMD_D3, ?CMD_BATTERY, ?CMD_VERSION, ?CMD_WAYPOINT, ?CMD_SURFACE_CALIB

%%%===================================================================
%%% 公开 API
%%%===================================================================

%% @doc 解析完整的遥测帧（外部调用入口）
-spec parse_telemetry_frame(binary()) -> {ok, map()} | {error, term()}.
parse_telemetry_frame(Data) when is_binary(Data) ->
    % 1. 使用框架层解码帧结构
    case frame_decoder:decode_frame(Data) of
        {ok, Frame, _Rest} ->
            % 2. 验证 CRC（可选，框架层 decode 已包含 CRC 校验，但可再次验证）
            case frame_decoder:validate_frame(Frame) of
                {ok, ValidFrame} ->
                    % 3. 检查是否为遥测数据类型（根据 data_type）
                    DataType = ValidFrame#uav_frame.data_type,
                    case (DataType band 16#0F) =:= ?TELEMETRY_DATA_TYPE_LOW of
                        true ->
                            % 4. 解析载荷
                            parse_telemetry_payload(ValidFrame#uav_frame.payload,
                                                    ValidFrame#uav_frame.dest_addr,
                                                    ValidFrame#uav_frame.src_addr,
                                                    ValidFrame#uav_frame.frame_count);
                        false ->
                            {error, {invalid_data_type, DataType}}
                    end;
                {error, Reason} ->
                    {error, {crc_validation_failed, Reason}}
            end;
        {incomplete, _Buffer} ->
            {error, incomplete_frame};
        {error, Reason} ->
            {error, {decode_error, Reason}}
    end.

%% @doc 解析遥测载荷（供框架层内部调用，也可被其他模块复用）
-spec parse_telemetry_payload(binary(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_telemetry_payload(<<?PAYLOAD_SUB_HEADER:16, _Len:8, PlaneType:8,
                          PlaneID:16/little, CmdID:8, Data/binary>>,
                        DestAddr, SrcAddr, FrameCount) ->
    case CmdID of
        ?CMD_D1 ->
            parse_d1_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_D2 ->
            parse_d2_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_D3 ->
            parse_d3_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_BATTERY ->
            parse_battery_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_VERSION ->
            parse_version_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_WAYPOINT ->
            parse_waypoint_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        ?CMD_SURFACE_CALIB ->
            parse_surface_calibration_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID);
        _ ->
            ?LOG(error, "[TELEMETRY] 不支持的命令ID: 0x~2.16.0B", [CmdID]),
            {error, {unsupported_command_id, CmdID}}
    end;
parse_telemetry_payload(Payload, _DestAddr, _SrcAddr, _FrameCount) ->
    ?LOG(error, "[TELEMETRY] 载荷头解析失败, payload大小=~p, hex=~p",
        [byte_size(Payload), dgiot_utils:binary_to_hex(Payload)]),
    {error, invalid_payload_header}.

%% @doc 解析 D1 数据（修正：加入 drone_id = PlaneID）
-spec parse_d1_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_d1_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case d1_data:parse(Data) of
        {ok, Status} ->
            ThingModel = uav_thing_model:convert_d1_to_thing_model(Status),
            {ok, #{
                type => d1_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                drone_id => PlaneID,   % 关键修正：使用 PlaneID 作为无人机标识
                command_id => ?CMD_D1,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            ?LOG(error, "[TELEMETRY] D1解析失败: ~p", [Reason]),
            {error, {d1_parse_error, Reason}}
    end.

%% @doc 解析 D2 数据
-spec parse_d2_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_d2_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case d2_data:parse(Data) of
        {ok, Status} ->
            %% 根据协议文档，无人机ID即为帧头中的 PlaneID
            DroneID = PlaneID,
            ThingModel = uav_thing_model:convert_d2_to_thing_model(Status),
            {ok, #{
                type => d2_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                drone_id => DroneID,
                command_id => ?CMD_D2,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            ?LOG(error, "[TELEMETRY] D2解析失败: ~p", [Reason]),
            {error, {d2_parse_error, Reason}}
    end.

%% @doc 解析 D3 数据
-spec parse_d3_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_d3_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case d3_data:parse(Data) of
        {ok, Status} ->
            DroneID = PlaneID,
            ThingModel = uav_thing_model:convert_d3_to_thing_model(Status),
            {ok, #{
                type => d3_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                drone_id => DroneID,
                command_id => ?CMD_D3,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            ?LOG(error, "[TELEMETRY] D3解析失败: ~p", [Reason]),
            {error, {d3_parse_error, Reason}}
    end.

%% @doc 解析电池数据 (0xA1)
-spec parse_battery_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_battery_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case uav_extra_commands:parse_battery(Data) of
        {ok, Status} ->
            ThingModel = uav_thing_model:convert_battery_to_thing_model(Status),
            {ok, #{
                type => battery_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                command_id => ?CMD_BATTERY,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {error, {battery_parse_error, Reason}}
    end.

%% @doc 解析版本数据 (0xC1)
-spec parse_version_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_version_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case uav_extra_commands:parse_version(Data) of
        {ok, Status} ->
            ThingModel = uav_thing_model:convert_version_to_thing_model(Status),
            {ok, #{
                type => version_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                command_id => ?CMD_VERSION,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {error, {version_parse_error, Reason}}
    end.

%% @doc 解析航点数据 (0xB4)
-spec parse_waypoint_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_waypoint_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case uav_extra_commands:parse_waypoint(Data) of
        {ok, Status} ->
            ThingModel = uav_thing_model:convert_waypoint_to_thing_model(Status),
            {ok, #{
                type => waypoint_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                command_id => ?CMD_WAYPOINT,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {error, {waypoint_parse_error, Reason}}
    end.

%% @doc 解析舵面校准数据 (0xDE)
-spec parse_surface_calibration_data(binary(), integer(), integer(), integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
parse_surface_calibration_data(Data, DestAddr, SrcAddr, FrameCount, PlaneType, PlaneID) ->
    case uav_extra_commands:parse_surface_calibration(Data) of
        {ok, Status} ->
            ThingModel = uav_thing_model:convert_surface_calibration_to_thing_model(Status),
            {ok, #{
                type => surface_calib_telemetry,
                dest_addr => DestAddr,
                src_addr => SrcAddr,
                frame_count => FrameCount,
                plane_type => PlaneType,
                plane_id => PlaneID,
                command_id => ?CMD_SURFACE_CALIB,
                data => Status,
                thing_model => ThingModel,
                timestamp => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {error, {surface_calib_parse_error, Reason}}
    end.