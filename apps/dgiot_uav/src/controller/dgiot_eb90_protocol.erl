%%%-------------------------------------------------------------------
%%% @doc EB90 协议处理模块（用于地测口）
%%% @version 1.0.0
%%% @author dgiot_uav_team
%%% @doc 负责处理地测口的EB90协议数据，包括帧解析、设备注册、遥测数据处理
%%%-------------------------------------------------------------------
-module(dgiot_eb90_protocol).
-author("dgiot_uav_team").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include("dgiot_uav.hrl").


%% API 导出
-export([
    init_state/0,
    handle_tcp_data/2,
    handle_port_data/4,
    remote_command_name/1,
    drone_id_to_name/1,
    get_command_example/1,
    build_remote_command/3,
    build_remote_command/4,
    build_remote_command_with_params/5,
    get_station_by_port/1,
    create_dynamic_station_mapping/3
]).

%% 内部状态
-record(state, {
    packet_count = 0,
    station_id = undefined,
    drone_id = undefined,
    ip_bin = <<>>,
    port = 0,
    product_id = <<>>,
    devaddr = <<>>,
    protocol_state = #{}
}).

%% 帧类型定义
-define(FRAME_TYPE_REMOTE,   16#00).   % 遥控帧 (66字节)
-define(FRAME_TYPE_TELEMETRY, 16#0E).   % 链路遥测帧 (128字节)

%% 帧长度定义
-define(FRAME_HEADER_SIZE, 8).
-define(FRAME_REMOTE_SIZE, 66).
-define(FRAME_TELEMETRY_SIZE, 128).

%% 帧头定义
-define(SYNC_HEADER, <<16#EB, 16#90>>).

%%====================================================================
%% 初始化
%%====================================================================
-spec init_state() -> #state{}.
init_state() ->
    #state{}.

%%====================================================================
%% TCP数据处理（用于数据汇聚）
%%====================================================================
-spec handle_tcp_data(binary(), #state{}) -> {ok, binary(), #state{}} | {error, term()}.
handle_tcp_data(Buffer, State) ->
    handle_tcp_data_loop(Buffer, State, []).

handle_tcp_data_loop(<<>>, State, Acc) ->
    lists:foreach(fun handle_frame/1, lists:reverse(Acc)),
    {ok, <<>>, State#state{packet_count = length(Acc)}};
handle_tcp_data_loop(Buffer, State, Acc) ->
    case extract_frame(Buffer) of
        {ok, Frame, Rest} ->
            ?LOG(info, "[EB90 RAW] 收到EB90帧: ~p", [dgiot_utils:binary_to_hex(Frame)]),
            handle_tcp_data_loop(Rest, State#state{packet_count = State#state.packet_count + 1}, [Frame | Acc]);
        {error, incomplete} ->
            lists:foreach(fun handle_frame/1, lists:reverse(Acc)),
            {ok, Buffer, State};
        {error, invalid} ->
            %% 丢弃无效字节，尝试重新同步
            <<_:8, Rest/binary>> = Buffer,
            handle_tcp_data_loop(Rest, State, Acc)
    end.

%% 提取完整EB90帧
extract_frame(<<16#EB, 16#90, DstH, DstL, SrcH, SrcL, Type, FrameNo, Rest/binary>>) ->
    FrameLen = get_frame_length(Type),
    DataLen = FrameLen - ?FRAME_HEADER_SIZE,
    
    case byte_size(Rest) >= DataLen of
        true ->
            <<FrameData:DataLen/binary, Next/binary>> = Rest,
            Frame = <<16#EB, 16#90, DstH, DstL, SrcH, SrcL, Type, FrameNo, FrameData/binary>>,
            {ok, Frame, Next};
        false ->
            {error, incomplete}
    end;
extract_frame(_) ->
    {error, incomplete}.

get_frame_length(?FRAME_TYPE_REMOTE) -> ?FRAME_REMOTE_SIZE;
get_frame_length(?FRAME_TYPE_TELEMETRY) -> ?FRAME_TELEMETRY_SIZE;
get_frame_length(_) -> 128.  % 遥控遥测默认128字节

%% Payload子帧头定义
-define(PAYLOAD_SUB_HEADER, 16#A55A).

%% 处理单个完整帧
handle_frame(Frame) ->
    %% 解析EB90帧
    case eb90_link_protocol:parse_link_frame(Frame) of
        {ok, _FullFrame, ParsedMap, _Rest} ->
            SrcAddr = maps:get(src_addr, ParsedMap, 0),
            FrameType = maps:get(frame_type, ParsedMap, unknown),
            
            ?LOG(error, "[EB90] 接收到帧，长度: ~p, 类型: ~p, 源地址: ~p", [byte_size(Frame), FrameType, SrcAddr]),
            
            %% 从Payload中提取飞机ID（PlaneID）
            %% Payload格式: A5 5A + len(1) + PlaneType(1) + PlaneID(2) + CmdID(1) + Data
            Payload = maps:get(payload, ParsedMap, <<>>),
            ?LOG(error, "[EB90] Payload长度: ~p, 内容: ~s", [byte_size(Payload), dgiot_utils:binary_to_hex(Payload)]),
            
            case Payload of
                <<?PAYLOAD_SUB_HEADER:16, _Len:8, _PlaneType:8, PlaneID:16/little, _CmdID:8, _Data/binary>> ->
                    DroneId = integer_to_binary(PlaneID),
                    ?LOG(info, "~n========================================~n"
                                "【Step 5/7】Extract PlaneID~n"
                                "========================================~n"
                                "  Payload Header: A5 5A~n"
                                "  PlaneType: ~p~n"
                                "  PlaneID: ~p (0x~4.16.0B)~n"
                                "  DroneID: ~s~n"
                                "  Status: OK - PlaneID extracted from EB90 frame~n"
                                "========================================", 
                         [_PlaneType, PlaneID, PlaneID, DroneId]),
                    
                    %% 检查是否需要创建无人机设备（首次收到EB90帧且提取到PlaneID）
                    case get(drone_device_created) of
                        undefined when PlaneID > 0 ->
                            ?LOG(info, "~n========================================~n"
                                        "【Step 6/7】Create device~n"
                                        "========================================~n"
                                        "  DevAddr: ~s (PlaneID)~n"
                                        "  ProductID: ~s~n"
                                        "  Status: Start creating drone device...~n"
                                        "========================================", 
                                 [DroneId, get(product_id)]),
                            %% 首次收到有效的EB90帧，创建无人机设备
                            create_drone_device(DroneId),
                            put(drone_device_created, true);
                        AlreadyCreated ->
                            ?LOG(info, "[EB90] Device already created flag: ~p", [AlreadyCreated]),
                            ok
                    end;
                Other ->
                    ?LOG(info, "[EB90] ERROR - Payload format mismatch or empty, cannot extract PlaneID, actual value: ~p", [Other])
            end,
            
            %% 处理解析后的数据
            handle_parsed_result(ParsedMap, Frame),
            ok;
        {error, Reason} ->
            ?LOG(warning, "[EB90] 帧解析失败: ~p", [Reason]),
            ok;
        {more, NeedBytes} ->
            ?LOG(debug, "[EB90] 数据不完整，需要更多字节: ~p", [NeedBytes]),
            ok
    end.

%% 处理解析结果
handle_parsed_result(#{frame_type := FrameType} = ParsedMap, _FullFrame) ->
    DestAddr = maps:get(dest_addr, ParsedMap, 0),
    SrcAddr = maps:get(src_addr, ParsedMap, 0),
    
    case FrameType of
        fc_telemetry ->
            ?LOG(error, "[EB90] 飞控遥测帧 | 目的:0x~4.16.0B | 源:0x~4.16.0B | 数据字段数:~p",
                 [DestAddr, SrcAddr, map_size(ParsedMap)]);
        _ ->
            ?LOG(error, "[EB90] 帧类型: ~p", [FrameType])
    end,
    ok;
handle_parsed_result(ParsedMap, _FullFrame) ->
    ?LOG(debug, "[EB90] 解析结果: ~p", [ParsedMap]),
    ok.

%%====================================================================
%% 端口数据处理（地测口）
%%====================================================================
-spec handle_port_data(binary(), #tcp{}, #uav_state{}, inet:socket()) -> 
    {#tcp{}, #uav_state{}, noreply}.
handle_port_data(NewBuf, TCPState, UavState, Socket) ->
    %% 转换为内部状态
    Eb90State = convert_uav_state_to_eb90_state(UavState),
    
    case NewBuf of
        <<"wrj_danpianji\n", Rest/binary>> ->
            ?LOG(error, "[EB90] Received fixture MCU registration, raw data: ~s", [dgiot_utils:binary_to_hex(NewBuf)]),
            handle_registration_port(Rest, TCPState, UavState, Socket, Eb90State);
        <<"wrj_dicekou\n", Rest/binary>> ->
            ?LOG(error, "[EB90] Received ground test port UAV registration, raw data: ~s", [dgiot_utils:binary_to_hex(NewBuf)]),
            handle_registration_port(Rest, TCPState, UavState, Socket, Eb90State);
        _ ->
            handle_normal_port_data(NewBuf, TCPState, UavState, Socket, Eb90State)
    end.

%% 转换UAV状态为EB90状态
convert_uav_state_to_eb90_state(UavState) ->
    #state{
        product_id = UavState#uav_state.product_id,
        ip_bin = UavState#uav_state.ip_bin,
        port = UavState#uav_state.port,
        devaddr = UavState#uav_state.devaddr,
        protocol_state = UavState#uav_state.protocol_state,
        station_id = UavState#uav_state.station_addr,
        drone_id = UavState#uav_state.device_id
    }.

%% 处理注册端口数据
handle_registration_port(Rest, TCPState, UavState, Socket, Eb90State) ->
    IpBin = UavState#uav_state.ip_bin,
    Port = UavState#uav_state.port,
    ProductId = UavState#uav_state.product_id,
    
    %% 存储上下文
    put(ip_bin, IpBin),
    put(port, Port),
    put(product_id, ProductId),
    
    %% 获取工位信息
    StationId = get_or_create_station(IpBin, Port),
    
    ?LOG(error, "~n========================================~n"
                "【Step 3/7】Ground test port sends registration~n"
                "========================================~n"
                "  IP:Port: ~s:~p~n"
                "  Register: wrj_dicekou~n"
                "  StationID: ~p~n"
                "  Status: OK - Device type identified~n"
                "========================================", [IpBin, Port, StationId]),
    
    %% 发送注册成功响应
    gen_tcp:send(Socket, <<"OK\n">>),
    DeviceType = case Port of
        10007 -> <<"地测口无人机"/utf8>>;
        _ -> <<"治具单片机"/utf8>>
    end,
    ?LOG(info, "[EB90] DeviceType = ~p", [DeviceType]),
    
    %% 【修复】如果有剩余数据（EB90帧），立即处理
    case Rest of
        <<>> -> 
            %% 没有剩余数据
            {TCPState#tcp{clientid = Eb90State#state.devaddr, buff = <<>>}, 
             UavState#uav_state{station_addr = StationId}, 
             noreply};
        _ ->
            %% 有剩余数据（EB90帧），立即处理
            ?LOG(error, "~n========================================~n"
                        "【Step 4/7】发送EB90飞控遥测帧~n"
                        "========================================~n"
                        "  帧长度: ~p 字节~n"
                        "  帧类型: 飞控遥测帧~n"
                        "  状态: ✅ 开始解析EB90帧~n"
                        "========================================", [byte_size(Rest)]),
            NewTCPState = TCPState#tcp{clientid = Eb90State#state.devaddr, buff = <<>>},
            NewUavState = UavState#uav_state{station_addr = StationId},
            handle_normal_port_data(Rest, NewTCPState, NewUavState, Socket, Eb90State)
    end.

%% 处理正常端口数据
handle_normal_port_data(NewBuf, TCPState, UavState, Socket, Eb90State) ->
    log_received_data(NewBuf, Eb90State),

    Result = case handle_tcp_data(NewBuf, Eb90State) of
    {ok, RestBuf, NewEb90State} ->
        NewUavState = convert_eb90_state_to_uav_state(NewEb90State, UavState),
        {TCPState#tcp{buff = ensure_binary(RestBuf), state = NewUavState}, NewUavState, noreply};
        {error, incomplete} ->
            {TCPState#tcp{buff = NewBuf}, UavState, noreply};
        {error, Reason} ->
            ?LOG(warning, "[EB90] 解析失败: ~p", [Reason]),
            {TCPState#tcp{buff = <<>>}, UavState, noreply}
    end,

    inet:setopts(Socket, [{active, once}]),

    Result.

%% 转换EB90状态为UAV状态
convert_eb90_state_to_uav_state(#state{
    product_id = ProductId,
    ip_bin = IpBin,
    port = Port,
    devaddr = DevAddr,
    protocol_state = ProtocolState,
    station_id = StationId,
    drone_id = DroneId
}, UavState) ->
    _StationId = StationId,
    _DroneId = DroneId,
    UavState#uav_state{
        product_id = ProductId,
        ip_bin = IpBin,
        port = Port,
        devaddr = DevAddr,
        protocol_state = ProtocolState,
        station_addr = StationId,
        device_id = DroneId
    }.

%%====================================================================
%% 工位管理
%%====================================================================
-spec get_or_create_station(binary(), integer()) -> integer().
get_or_create_station(IpBin, Port) ->
    case dgiot_uav_business_service:get_station_by_ip(IpBin) of
        {ok, StationId} -> StationId;
        {error, not_find} -> create_dynamic_station_mapping(IpBin, Port, get_station_by_port(Port));
        {error, _} -> get_station_by_port(Port)
    end.

%% 根据端口获取工位ID
-spec get_station_by_port(integer()) -> integer().
get_station_by_port(10007) -> 1700;  % 磁航向
get_station_by_port(10006) -> 1500;  % 总测1
get_station_by_port(10005) -> 1600;  % 总测2
get_station_by_port(10004) -> 1200;  % 拷机1
get_station_by_port(10003) -> 1300;  % 拷机2
get_station_by_port(10002) -> 1100;  % 桁架
get_station_by_port(_Port) -> 0.  % 默认未知工位

%% 创建动态工位映射
-spec create_dynamic_station_mapping(binary(), integer(), integer()) -> ok.
create_dynamic_station_mapping(IpBin, Port, StationId) ->
    Mapping = #{
        <<"fixture_address">> => StationId,
        <<"station_id">> => StationId,
        <<"station_name">> => <<"地测口-动态工位"/utf8>>,
        <<"bound_at">> => erlang:system_time(millisecond),
        <<"ip">> => IpBin,
        <<"port">> => Port,
        <<"dynamic">> => true
    },
    dgiot_data:insert(uav_ip_station_mapping, IpBin, Mapping),
    ?LOG(error, "[EB90] Dynamic station mapping created - IP:~s, Port:~p, Station:~p", [IpBin, Port, StationId]),
    ok.

%%====================================================================
%% 辅助函数
%%====================================================================
log_received_data(Data, _State) ->
    ?LOG(error, "[EB90] 收到数据: ~p字节", [byte_size(Data)]),
    ?LOG(error, "[EB90] 数据十六进制: ~s", [dgiot_utils:binary_to_hex(Data)]),
    ok.

ensure_binary(undefined) -> <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(_) -> <<>>.

%%====================================================================
%% 遥控命令函数
%%====================================================================

%% 遥控命令名称
remote_command_name(<<"F0A2">>) -> <<"筒内状态"/utf8>>;
remote_command_name(<<"F0B9">>) -> <<"复飞"/utf8>>;
remote_command_name(<<"F0FB">>) -> <<"舵面中位"/utf8>>;
remote_command_name(<<"F0F3">>) -> <<"舵面使能"/utf8>>;
remote_command_name(<<"DC01">>) -> <<"左副翼"/utf8>>;
remote_command_name(<<"DC02">>) -> <<"右副翼"/utf8>>;
remote_command_name(Other) -> Other.

%% 无人机ID转名称
drone_id_to_name(0) -> <<"未设置"/utf8>>;
drone_id_to_name(DroneId) when is_integer(DroneId) ->
    <<(integer_to_binary(DroneId))/binary, "号无人机"/utf8>>;
drone_id_to_name(DroneIdBin) when is_binary(DroneIdBin) ->
    DroneIdBin.

%% 获取命令示例
get_command_example(<<"F0FB">>) ->
    #{<<"command">> => <<"F0FB">>, <<"description">> => <<"舵面中位"/utf8>>};
get_command_example(<<"F0F3">>) ->
    #{<<"command">> => <<"F0F3">>, <<"description">> => <<"舵面使能"/utf8>>};
get_command_example(_) ->
    #{<<"command">> => <<"unknown">>, <<"description">> => <<"未知命令"/utf8>>}.

%% 构建遥控命令（4参数版本）
build_remote_command(DstId, SrcId, Command) ->
    build_remote_command(DstId, SrcId, Command, []).

%% 构建遥控命令（5参数版本）
build_remote_command(DstId, SrcId, Command, Params) ->
    build_remote_command_with_params(DstId, SrcId, Command, Params, 0).

%% 构建带参数的遥控命令
build_remote_command_with_params(DstId, SrcId, Command, Params, FrameNo) ->
    %% 帧头: EB90 + 目的地址(2) + 源地址(2) + 平台类型(1) + 帧号(1)
    DstH = (DstId band 16#FF00) bsr 8,
    DstL = DstId band 16#FF,
    SrcH = (SrcId band 16#FF00) bsr 8,
    SrcL = SrcId band 16#FF,

    Header = <<16#EB, 16#90, DstH, DstL, SrcH, SrcL, 0, FrameNo>>,

    %% 遥控密钥
    Key = <<16#A5, 16#5A>>,

    %% 载荷: 命令码 + 参数
    Payload = case Params of
        [] -> Command;
        _ -> <<Command/binary, (iolist_to_binary(Params))/binary>>
    end,

    %% 计算校验和
    Checksum = lists:foldl(fun(B, Acc) -> Acc + B end, 0, binary_to_list(Payload)),

    %% 构建完整帧
    Frame = <<Header/binary, Key/binary, Payload/binary, Checksum:8>>,

    ?LOG(debug, "[EB90] 构建遥控命令: Dst=~p, Src=~p, Cmd=~p, FrameNo=~p", [DstId, SrcId, Command, FrameNo]),

    Frame.

%%====================================================================
%% 无人机设备创建
%%====================================================================
%% @doc 创建无人机设备
%% DevAddr = 飞机ID（从Payload帧头提取的PlaneID）
%% 设备名称 = 扫码枪二维码序号（serial_no）
-spec create_drone_device(binary()) -> {ok, binary()} | {error, term()}.
create_drone_device(DroneId) ->
    %% 无人机产品ID（超近距无人机）
    DroneProductId = <<"6235befb62">>,
    
    %% 获取IP和端口信息
    IpBin = get(ip_bin),
    Port = get(port),
    
    %% 从二维码缓存获取serial_no作为设备名称
    DeviceName = case dgiot_scanner_protocol:get_cached_qrcode(magnetic_station) of
        {ok, ParsedData} ->
            SerialNo = maps:get(<<"serial_no">>, ParsedData, DroneId),
            ?LOG(error, "~n========================================~n"
                        "【Step 6/7】创建设备（续）~n"
                        "========================================~n"
                        "  DevAddr: ~s (PlaneID)~n"
                        "  DeviceName: ~s (SerialNo)~n"
                        "  ProductID: ~s~n"
                        "  IP:Port: ~s:~p~n"
                        "  Status: OK - Get device name from QR cache~n"
                        "========================================", 
                 [DroneId, SerialNo, DroneProductId, IpBin, Port]),
            SerialNo;
        {error, _} ->
            ?LOG(error, "~n========================================~n"
                        "【Step 6/7】创建设备（续）~n"
                        "========================================~n"
                        "  DevAddr: ~s (PlaneID)~n"
                        "  DeviceName: ~s (默认)~n"
                        "  ProductID: ~s~n"
                        "  IP:Port: ~s:~p~n"
                        "  Status: WARNING - No QR cache, use DroneId as device name~n"
                        "========================================", 
                 [DroneId, DroneId, DroneProductId, IpBin, Port]),
            DroneId
    end,
    
    %% 创建设备
    case dgiot_uav_device_manager:create_device(<<"uav">>, DroneProductId, DroneId, IpBin, DeviceName) of
        {ok, ObjectId} ->
            ?LOG(error, "~n========================================~n"
                        "【Step 6/7】创建设备（完成）~n"
                        "========================================~n"
                        "  DevAddr: ~s~n"
                        "  DeviceName: ~s~n"
                        "  ObjectId: ~s~n"
                        "  状态: ✅ 设备创建成功~n"
                        "========================================", 
                 [DroneId, DeviceName, ObjectId]),
            
            %% MES上报
            ?LOG(info, "~n========================================~n"
                        "【Step 7/7】MES Report~n"
                        "========================================~n"
                        "  DevAddr: ~s~n"
                        "  DeviceName: ~s~n"
                        "  Status: Start MES report...~n"
                        "========================================", 
                 [DroneId, DeviceName]),
            report_to_mes_after_create(DroneId, DeviceName),
            
            {ok, ObjectId};
        {error, Reason} ->
            ?LOG(error, "Device creation failed: ~p", [Reason]),
            {error, Reason}
    end.

%% MES上报辅助函数
report_to_mes_after_create(DroneId, DeviceName) ->
    case code:is_loaded(dgiot_uav_mes_api) of
        false ->
            ?LOG(error, "[MES] MES module not loaded, skip MES report");
        true ->
            %% 获取二维码数据用于MES上报
            case dgiot_scanner_protocol:get_cached_qrcode(magnetic_station) of
                {ok, ParsedData} ->
                    TestId = maps:get(<<"test_id">>, ParsedData, <<>>),
                    StationId = maps:get(<<"station_id">>, ParsedData, <<>>),
                    MaterialCode = maps:get(<<"material_code">>, ParsedData, <<>>),
                    
                    MesData = #{
                        <<"test_id">> => TestId,
                        <<"station_id">> => StationId,
                        <<"material_code">> => MaterialCode,
                        <<"serial_no">> => DeviceName
                    },
                    
                    case dgiot_uav_mes_api:report_device_status(<<"ALM_MES">>, StationId, DroneId, MesData) of
                        {ok, _Response} ->
                            ?LOG(error, "~n========================================~n"
                                        "【Step 7/7】MES Report (Done)~n"
                                        "========================================~n"
                                        "  DevAddr: ~s~n"
                                        "  StationID: ~s~n"
                                        "  TestID: ~s~n"
                                        "  MaterialCode: ~s~n"
                                        "  Status: OK - MES report success~n"
                                        "========================================", 
                                 [DroneId, StationId, TestId, MaterialCode]);
                        {error, Error} ->
                            ?LOG(error, "MES report failed: ~p", [Error])
                    end;
                {error, _} ->
                    ?LOG(warning, "[MES] No QR data, skip MES report")
            end
    end.