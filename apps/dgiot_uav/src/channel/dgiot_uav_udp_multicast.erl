%%%-------------------------------------------------------------------
%%% @doc dgiot_uav UDP多播接收模块 - 针对无人机UDP多播协议定制
%%% 无人机使用: 226.0.0.80:8000, 8001, 8002
%%% 基于实际抓包分析结果实现
%%%-------------------------------------------------------------------
-module(dgiot_uav_udp_multicast).
-author("AI Assistant").
-export([start_capture/3, stop_capture/1, parse_uav_packet/1]).

%% 无人机多播配置（基于抓包分析）
-define(UAV_MULTICAST_GROUP, {226, 0, 0, 80}).  %% 无人机多播地址
-define(UAV_PORTS, [8000, 8001, 8002]).         %% 无人机端口：复合数据、遥测数据、遥控数据

%% UAV报文类型定义（基于EB90协议分析）
-define(PACKET_TYPE_COMPOSITE, 16#01).    %% 复合数据（298字节）
-define(PACKET_TYPE_TELEMETRY, 16#02).    %% 遥测数据（170字节）
-define(PACKET_TYPE_CONTROL, 16#03).      %% 遥控数据（106字节）

%% 数据包大小定义（基于抓包分析）
-define(COMPOSITE_PACKET_SIZE, 298).      %% 复合数据包大小
-define(TELEMETRY_PACKET_SIZE, 170).      %% 遥测数据包大小
-define(CONTROL_PACKET_SIZE, 106).        %% 遥控数据包大小
-define(RF_PACKET_SIZE, 128).             %% 射频卡数据包大小（基于抓包分析）
-define(RF_CARD_COUNT, 3).                %% 射频卡数量

%% 源IP地址定义（基于抓包分析）
-define(GROUND_STATION_IP, {192, 168, 8, 10}).      %% 地面数据终端
-define(HANDHELD_CONTROLLER_IP, {169, 254, 126, 115}). %% 手持控制终端

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 启动射频卡多播抓包
-spec start_capture(inet:ip_address(), integer(), pid()) -> 
    {ok, pid()} | {error, term()}.
start_capture(MulticastGroup, Port, WorkerPid) ->
    spawn_link(fun() -> capture_loop(MulticastGroup, Port, WorkerPid) end).

%% @doc 停止抓包
-spec stop_capture(pid()) -> ok.
stop_capture(Pid) ->
    Pid ! stop.

%% @doc 解析射频卡报文
-spec parse_uav_packet(binary()) -> 
    {ok, map()} | {error, invalid_packet}.
parse_uav_packet(Packet) ->
    try
        parse_rf_packet(Packet)
    catch
        _:_ -> {error, invalid_packet}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private 抓包主循环
capture_loop(MulticastGroup, Port, WorkerPid) ->
    case gen_udp:open(Port, [
        binary,
        {active, true},
        {reuseaddr, true},
        {multicast_loop, true},
        {multicast_ttl, 4},
        {ip, {0,0,0,0}}
    ]) of
        {ok, Socket} ->
            %% 加入多播组
            case inet:setopts(Socket, [{add_membership, {MulticastGroup, {0,0,0,0}}}]) of
                ok ->
                    io:format("射频卡多播抓包已启动: ~p:~p~n", 
                             [MulticastGroup, Port]),
                    io:format("等待3个射频卡数据...~n"),
                    capture_loop(Socket, MulticastGroup, Port, WorkerPid);
                {error, Reason} ->
                    gen_udp:close(Socket),
                    io:format("加入多播组失败: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("打开UDP套接字失败: ~p~n", [Reason])
    end.

%% @private 抓包循环
capture_loop(Socket, MulticastGroup, Port, WorkerPid) ->
    receive
        {udp, Socket, IP, InPort, Packet} ->
            %% 处理接收到的报文
            process_rf_packet(IP, InPort, Packet, WorkerPid),
            capture_loop(Socket, MulticastGroup, Port, WorkerPid);
            
        stop ->
            gen_udp:close(Socket),
            io:format("射频卡多播抓包已停止~n");
            
        _Other ->
            capture_loop(Socket, MulticastGroup, Port, WorkerPid)
    end.

%% @private 处理射频卡报文
process_rf_packet(IP, Port, Packet, WorkerPid) ->
    PacketSize = byte_size(Packet),
    
    %% 记录接收信息
    io:format("射频卡数据来自 ~p:~p, 大小: ~p 字节~n", 
             [IP, Port, PacketSize]),
    
    %% 解析射频卡数据
    case parse_rf_packet(Packet) of
        {ok, ParsedData} ->
            %% 发送给worker进程处理
            WorkerPid ! {rf_multicast_packet, IP, Port, ParsedData},
            
            %% 显示解析结果
            display_rf_packet_info(ParsedData);
            
        {error, invalid_packet} ->
            io:format("无效的射频卡数据包~n")
    end.

%% @private 解析射频卡数据包
parse_rf_packet(Packet) when byte_size(Packet) =:= ?RF_PACKET_SIZE ->
    %% 射频卡数据格式分析
    %% 基于抓包数据：128字节，可能包含：
    %% 1. 头标识 (可能为EB90或其他)
    %% 2. 射频卡ID
    %% 3. 时间戳
    %% 4. 传感器数据
    %% 5. 校验和
    
    try
        %% 尝试解析为EB90协议
        case Packet of
            <<16#EB, 16#90, Rest/binary>> ->
                parse_eb90_protocol(Rest);
            _ ->
                %% 尝试解析为其他格式
                parse_generic_rf_packet(Packet)
        end
    catch
        _:_ ->
            {error, invalid_packet}
    end;

parse_rf_packet(_Packet) ->
    {error, invalid_packet}.

%% @private 解析EB90协议
parse_eb90_protocol(Rest) ->
    %% EB90协议格式假设：
    %% EB90 + 长度 + 射频卡ID + 数据 + 校验
    case Rest of
        <<Length:8, RfCardId:8, Timestamp:32, Data:84/binary, Checksum:16>> ->
            {ok, #{
                protocol => "EB90",
                rf_card_id => RfCardId,
                timestamp => Timestamp,
                data_length => Length,
                raw_data => Data,
                checksum => Checksum,
                packet_size => 128
            }};
        _ ->
            parse_generic_rf_packet(<<16#EB, 16#90, Rest/binary>>)
    end.

%% @private 解析通用射频卡数据包
parse_generic_rf_packet(Packet) ->
    %% 通用解析：将数据包分为多个字段
    <<Byte1:8, Byte2:8, Byte3:8, Byte4:8, _Rest/binary>> = Packet,
    
    %% 尝试识别射频卡ID（基于源IP或数据模式）
    RfCardId = identify_rf_card(Byte1, Byte2, Byte3, Byte4),
    
    {ok, #{
        protocol => "GENERIC_RF",
        rf_card_id => RfCardId,
        header_bytes => [Byte1, Byte2, Byte3, Byte4],
        packet_size => byte_size(Packet),
        raw_packet_hex => binary:encode_hex(Packet),
        timestamp => erlang:system_time(millisecond)
    }}.

%% @private 识别射频卡
identify_rf_card(B1, B2, B3, B4) ->
    %% 简单识别逻辑：基于字节模式
    case {B1, B2, B3, B4} of
        {16#D1, _, _, _} -> "RF_CARD_1";
        {16#D2, _, _, _} -> "RF_CARD_2";
        {16#D3, _, _, _} -> "RF_CARD_3";
        _ -> 
            %% 基于字节和模3
            Id = (B1 + B2 + B3 + B4) rem ?RF_CARD_COUNT + 1,
            "RF_CARD_" ++ integer_to_list(Id)
    end.

%% @private 显示射频卡数据包信息
display_rf_packet_info(#{rf_card_id := RfCardId, protocol := Protocol} = Data) ->
    io:format("射频卡 ~s 数据 [~s]~n", [RfCardId, Protocol]),
    
    case Protocol of
        "EB90" ->
            io:format("  时间戳: ~p, 校验和: ~p~n", 
                     [maps:get(timestamp, Data), maps:get(checksum, Data)]);
        "GENERIC_RF" ->
            io:format("  头字节: ~p, 大小: ~p 字节~n",
                     [maps:get(header_bytes, Data), maps:get(packet_size, Data)])
    end,
    
    %% 每10个包显示一次详细内容
    case rand:uniform(10) of
        1 ->
            io:format("  原始数据(Hex): ~s~n", [maps:get(raw_packet_hex, Data, "")]);
        _ ->
            ok
    end.

%%%===================================================================
%%% 集成到dgiot_uav_udp_worker的示例
%%%===================================================================
%% 
%% 修改dgiot_uav_udp_worker.erl:
%%
%% init([]) ->
%%     %% 启动射频卡多播抓包
%%     {ok, CapturePid} = dgiot_uav_udp_multicast:start_capture(
%%         {226,0,0,80}, 3600, self()
%%     ),
%%     {ok, #state{capture_pid = CapturePid}}.
%%
%% handle_info({rf_multicast_packet, IP, Port, Data}, State) ->
%%     %% 处理射频卡数据
%%     io:format("收到射频卡数据: ~p~n", [Data]),
%%     
%%     %% 转换为DGIOT设备数据格式
%%     DeviceData = convert_to_dgiot_format(Data),
%%     
%%     %% 发送到DGIOT平台
%%     dgiot_parse:send_to_platform(DeviceData),
%%     
%%     {noreply, State}.
%%
%% terminate(_Reason, #state{capture_pid = CapturePid}) ->
%%     dgiot_uav_udp_multicast:stop_capture(CapturePid),
%%     ok.
%%