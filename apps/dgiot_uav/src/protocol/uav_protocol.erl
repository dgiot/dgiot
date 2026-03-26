%%%-------------------------------------------------------------------
%%% @doc
%%% uav_protocol.erl - 无人机(UAV)通信协议处理统一入口
%%%
%%% 完整解析版本：调用 eb90_link_protocol 解析链路帧，然后根据帧类型分发。
%%% 修改：扩展帧（extended）尝试直接从原始帧提取载荷并解析为飞控遥测。
%%% 修正：载荷提取偏移量从 12 改为 13（跳过前13个字节，即索引13开始）。
%%%
%%% 主要功能：
%%% - start_hook/0, stop_hook/0: 注册/注销协议钩子
%%% - parse/2: 协议解析入口
%%% - encode/2: 协议编码入口
%%% - parse_raw_data/3: 主入口，解析 EB90 帧
%%% - encode_control_command/2: 编码控制命令
%%% - validate_crc/1: 验证 CRC
%%% - get_protocol_info/0: 获取协议信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(uav_protocol).

%% API
-export([
    start_hook/0,
    stop_hook/0,
    parse/2,
    encode/2,
    parse_raw_data/3,
    encode_control_command/2,
    validate_crc/1,
    get_protocol_info/0,
    get_datasource/1
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").
-include_lib("dgiot_uav/include/types.hrl").
-include_lib("dgiot_uav/include/extra_commands.hrl").

%%%===================================================================
%%% API
%%%===================================================================

get_datasource(_ProductId) ->
    #{
        protocol => <<"uav">>,
        description => <<"无人机通信协议"/utf8>>,
        frame_format => #{
            sync_header => ?SYNC_HEADER,
            frame_types => [
                #{type => remote_control, description => <<"遥控指令帧"/utf8>>},
                #{type => fc_telemetry, description => <<"飞控遥测数据帧(D1/D2/D3)"/utf8>>},
                #{type => link_telemetry, description => <<"链路遥测数据帧"/utf8>>}
            ]
        }
    }.

start_hook() ->
    ?LOG(info, "Registering UAV protocol hooks"),
    dgiot_hook:add(one_for_one, {dgiot_datasource, <<"uav">>}, fun get_datasource/1),
    dgiot_hook:add(one_for_one, {dgiot_raw_data_parser, <<"uav">>}, fun parse_raw_data/3),
    dgiot_hook:add(one_for_one, {dgiot_control_encoder, <<"uav">>}, fun encode_control_command/2),
    ok.

stop_hook() ->
    ?LOG(info, "Unregistering UAV protocol hooks"),
    dgiot_hook:remove({dgiot_datasource, <<"uav">>}),
    dgiot_hook:remove({dgiot_raw_data_parser, <<"uav">>}),
    dgiot_hook:remove({dgiot_control_encoder, <<"uav">>}),
    ok.

parse(<<"uav">>, Data) -> parse_raw_data(undefined, undefined, Data);
parse(_Protocol, _Data) -> {error, unsupported_protocol}.

encode(<<"uav">>, #{command := Command, params := Params}) -> encode_control_command(Command, Params);
encode(_Protocol, _Data) -> {error, unsupported_protocol}.

%% 主入口：解析 EB90 帧
parse_raw_data(ProductId, DevAddr, Data) when is_binary(Data) ->
    case eb90_link_protocol:parse_link_frame(Data) of
        {ok, FullFrame, ParsedMap, _Rest} ->
            handle_parsed_frame(ProductId, DevAddr, FullFrame, ParsedMap);
        {error, Reason} ->
            ?LOG(error, "[UAV_PROTO] Frame parsing failed: ~p", [Reason]),
            {error, Reason}
    end.

%% 根据帧类型处理解析结果
handle_parsed_frame(_ProductId, _DevAddr, FullFrame, ParsedMap) ->
    FrameType = maps:get(frame_type, ParsedMap, unknown),
    case FrameType of
        remote_control ->
            {ok, [#{
                type => remote_control,
                data => ParsedMap,
                raw_frame => FullFrame,
                timestamp => erlang:system_time(millisecond)
            }]};
        fc_telemetry ->
            Payload = maps:get(payload, ParsedMap),
            DestAddr = maps:get(dest_addr, ParsedMap),
            SrcAddr = maps:get(src_addr, ParsedMap),
            FrameNo = maps:get(frame_no, ParsedMap),
            case telemetry_protocol:parse_telemetry_payload(Payload, DestAddr, SrcAddr, FrameNo) of
                {ok, TelemetryMap} ->
                    Result = maps:merge(ParsedMap, TelemetryMap),
                    TelemetryType = maps:get(type, TelemetryMap, fc_telemetry),
                    %% 注意：D1 遥测数据不需要在这里调用 save_thing_model_data
                    %% 数据在 dgiot_eb90_protocol:handle_parsed_result/1 中通过汇聚机制处理
                    {ok, [#{
                        type => TelemetryType,
                        data => Result,
                        raw_frame => FullFrame,
                        timestamp => erlang:system_time(millisecond)
                    }]};
                {error, Reason} ->
                    {error, {fc_telemetry_parse_error, Reason}}
            end;
        link_telemetry ->
            LinkStatus = extract_link_status(ParsedMap),
            ThingModel = uav_thing_model:convert_link_to_thing_model(LinkStatus),
            %% 注意：链路遥测数据不需要在这里调用 save_thing_model_data
            %% 数据在 dgiot_eb90_protocol:handle_parsed_result/1 中处理
            {ok, [#{
                type => link_telemetry,
                data => ParsedMap,
                thing_model => ThingModel,
                raw_frame => FullFrame,
                timestamp => erlang:system_time(millisecond)
            }]};
        extended ->
            %% 扩展帧：直接尝试从原始帧提取载荷（字节14-128，共115字节）
            %% 飞控遥测帧结构：字节0-7 EB90头，字节8-10密钥，字节11-12 CRC1，字节13-127载荷（115字节）
            case byte_size(FullFrame) >= 128 of
                true ->
                    %% 提取载荷部分（跳过前13个字节，取115字节）
                    Payload = binary:part(FullFrame, 13, 115),
                    DestAddr = maps:get(dest_addr, ParsedMap, 0),
                    SrcAddr = maps:get(src_addr, ParsedMap, 0),
                    FrameNo = maps:get(frame_no, ParsedMap, 0),
                    %% 打印载荷前两个字节（按大端显示，便于对照协议）
                    case byte_size(Payload) >= 2 of
                        true ->
                            <<_:16/big, _/binary>> = Payload;
                        false ->
                            ok
                    end,
                    case telemetry_protocol:parse_telemetry_payload(Payload, DestAddr, SrcAddr, FrameNo) of
                        {ok, TelemetryMap} ->
                            Result = maps:merge(ParsedMap, TelemetryMap),
                            TelemetryType = maps:get(type, TelemetryMap, fc_telemetry),
                            ?LOG(debug, "[UAV_PROTO] 遥测解析成功: dest=0x~4.16.0B, src=0x~4.16.0B, frame_no=~p, 类型=~p",
                                [DestAddr, SrcAddr, FrameNo, TelemetryType]),
                            %% 注意：D2/D3 遥测数据不需要在这里调用 save_thing_model_data
                            %% 数据在 dgiot_eb90_protocol:handle_parsed_result/1 中通过汇聚机制处理
                            {ok, [#{
                                type => TelemetryType,
                                data => Result,
                                raw_frame => FullFrame,
                                timestamp => erlang:system_time(millisecond)
                            }]};
                        {error, Reason} ->
                            ?LOG(error, "[UAV_PROTO] Payload parse failed: ~p", [Reason]),
                            ?LOG(error, "[UAV_PROTO] Payload hex: ~p", [dgiot_utils:binary_to_hex(Payload)]),
                            {ok, [#{
                                type => extended,
                                data => ParsedMap,
                                raw_frame => FullFrame,
                                timestamp => erlang:system_time(millisecond)
                            }]}
                    end;
                false ->
                    ?LOG(error, "[UAV_PROTO] Frame too short for telemetry extraction"),
                    {ok, [#{
                        type => extended,
                        data => ParsedMap,
                        raw_frame => FullFrame,
                        timestamp => erlang:system_time(millisecond)
                    }]}
            end;
        extended_data ->
            %% 扩展数据帧类型，直接返回原始数据
            ?LOG(info, "[UAV_PROTO] 处理扩展数据帧: ~p", [maps:size(ParsedMap)]),
            {ok, [#{
                type => extended_data,
                data => ParsedMap,
                raw_frame => FullFrame,
                timestamp => erlang:system_time(millisecond)
            }]};
        unknown ->
            {error, {unexpected_frame_type, maps:get(frame_type, ParsedMap)}}
    end.

%% 从解析映射中提取链路状态记录
-spec extract_link_status(map()) -> #link_status{}.
extract_link_status(ParsedMap) ->
    NetworkApply = maps:get(network_apply, ParsedMap, #{}),
    NetworkStatus = maps:get(network_status, ParsedMap, #{}),
    #link_status{
        network_access_flag = case maps:get(valid, NetworkApply, false) of
            true -> 16#AA;
            false -> 16#00
        end,
        node_address = maps:get(address, NetworkApply, 0),
        granted_nodes_count = maps:get(allowed_count, NetworkStatus, 0),
        denied_nodes_count = maps:get(rejected_count, NetworkStatus, 0),
        online_nodes = maps:get(online_nodes, ParsedMap, []),
        latest_denied_node = maps:get(reject_node, ParsedMap, 0),
        crc1 = maps:get(crc1, ParsedMap, 0),
        crc2 = maps:get(crc2, ParsedMap, 0)
    }.

%% 编码控制命令
encode_control_command(Command, Params) ->
    ?LOG(debug, "Encoding control command: ~p, Params=~p", [Command, Params]),
    CmdCode = command_to_code(Command),
    ControlParams = #{
        dest_addr => maps:get(dest_addr, Params, ?DEFAULT_DEST_ADDR),
        src_addr => maps:get(src_addr, Params, ?DEFAULT_SRC_ADDR),
        platform_type => maps:get(platform_type, Params, ?PLATFORM_206),
        plane_type => maps:get(plane_type, Params, 1),
        plane_id => maps:get(plane_id, Params, 1),
        command_id => CmdCode,
        switch_cmd => maps:get(switch_cmd, Params, 0),
        waypoint_index => maps:get(waypoint_index, Params, 0),
        sub_command => maps:get(sub_command, Params, 0),
        elevation => maps:get(elevation, Params, 0),
        azimuth => maps:get(azimuth, Params, 0),
        latitude => maps:get(latitude, Params, 0.0),
        longitude => maps:get(longitude, Params, 0.0),
        altitude => maps:get(altitude, Params, 0),
        total_waypoints => maps:get(total_waypoints, Params, 0),
        waypoint_sequence => maps:get(waypoint_sequence, Params, 0),
        total_flight_time => maps:get(total_flight_time, Params, 0),
        sortie_count => maps:get(sortie_count, Params, 0),
        channel => maps:get(channel, Params, 0),
        pwm_center => maps:get(pwm_center, Params, 0),
        up_ratio => maps:get(up_ratio, Params, 0.0),
        down_ratio => maps:get(down_ratio, Params, 0.0),
        scale => maps:get(scale, Params, 1.0),
        offset => maps:get(offset, Params, 0.0),
        new_plane_type => maps:get(new_plane_type, Params, 0),
        new_plane_id => maps:get(new_plane_id, Params, 0)
    },
    control_protocol:build_control_frame(CmdCode, ControlParams).

validate_crc(Data) when is_binary(Data) ->
    case frame_decoder:decode_frame(Data) of
        {ok, Frame, _Rest} ->
            case frame_decoder:validate_frame(Frame) of
                {ok, _} -> true;
                {error, _} -> false
            end;
        _ -> false
    end.

get_protocol_info() ->
    #{
        protocol_name => <<"uav"/utf8>>,
        protocol_version => <<"2.0.0"/utf8>>,
        sync_header => ?SYNC_HEADER,
        frame_types => [
            #{type => remote_control, description => <<"遥控指令帧"/utf8>>},
            #{type => fc_telemetry, description => <<"飞控遥测数据帧(D1/D2/D3)"/utf8>>},
            #{type => link_telemetry, description => <<"链路遥测数据帧"/utf8>>}
        ],
        supported_commands => [
            <<"takeoff">>, <<"land">>, <<"move">>, <<"rotate">>,
            <<"start_mission">>, <<"stop_mission">>,
            <<"return_home">>, <<"emergency_stop">>, <<"reboot">>,
            <<"query_status">>, <<"query_params">>, <<"set_params">>, <<"calibrate">>
        ],
        timestamp => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 内部函数
%%%===================================================================

command_to_code(<<"takeoff">>) -> 16#A2;
command_to_code(<<"land">>) -> 16#E6;
command_to_code(<<"return_home">>) -> 16#3D;
command_to_code(<<"hover">>) -> 16#E9;
command_to_code(<<"start_mission">>) -> 16#65;
command_to_code(<<"stop_mission">>) -> 16#67;
command_to_code(<<"emergency_stop">>) -> 16#FC;
command_to_code(<<"reboot">>) -> 16#F5;
command_to_code(_) -> 16#00.