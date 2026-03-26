%%%-------------------------------------------------------------------
%%% @doc dgiot_uav_udp_worker.erl - 单端口UDP多播工作器
%%% 每个worker只监听一个端口，直接使用gen_udp，不依赖esockd。
%%% 支持端口：8000(复合数据)、8001(遥测数据)、8002(遥控数据)
%%% 并处理入网申请（遥测帧中 network_apply 字段）以及遥控帧的解析
%%% 增强版：支持普通遥控指令的识别与处理
%%% 日志：入网申请日志添加频率控制，同一设备每10秒最多打印一次，避免高频输出。
%%%-------------------------------------------------------------------
-module(dgiot_uav_udp_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, child_spec/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("dgiot/include/logger.hrl").

-define(MULTICAST_GROUP, {226,0,0,80}).
-define(APPLY_LOG_INTERVAL, 10000).  %% 入网申请日志最小间隔（毫秒）

%% 状态记录
-record(state, {
    socket :: inet:socket(),
    port :: integer(),
    packet_count = 0 :: integer(),
    last_packet_time :: integer() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
    gen_server:start_link({local, list_to_atom("dgiot_uav_udp_worker_" ++ integer_to_list(Port))},
                          ?MODULE, [Port], []).

child_spec(Port, _State) ->
    #{
        id => {?MODULE, Port},
        start => {?MODULE, start_link, [Port]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    ?LOG(info, "dgiot_uav_udp_worker starting on port ~p", [Port]),
    %% 确保 uav_online_nodes ETS 表存在（用于入网管理）
    case ets:info(uav_online_nodes) of
        undefined ->
            ets:new(uav_online_nodes, [set, public, named_table, {keypos, 1}]),
            ?LOG(info, "Created ETS table uav_online_nodes", []);
        _ ->
            ok
    end,
    %% 初始化入网申请日志时间记录
    put(apply_log_times, #{}),
    case open_multicast_socket(Port) of
        {ok, Socket} ->
            ?LOG(info, "UDP multicast socket opened on 226.0.0.80:~p", [Port]),
            {ok, #state{socket = Socket, port = Port, last_packet_time = erlang:system_time(millisecond)}};
        {error, Reason} ->
            ?LOG(error, "Failed to open UDP socket: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, IP, InPort, Packet}, State = #state{socket = Socket, port = LocalPort}) ->
    process_packet(IP, InPort, Packet, LocalPort),
    {noreply, update_stats(State)};
handle_info({udp, IP, InPort, Packet}, State) ->
    process_packet(IP, InPort, Packet, State#state.port),
    {noreply, update_stats(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

open_multicast_socket(Port) ->
    %% 获取所有网络接口并加入多播组
    try
        {ok, Socket} = gen_udp:open(Port, [
            binary,
            {active, true},
            {reuseaddr, true},
            {multicast_loop, true},
            {multicast_ttl, 4},
            {ip, {0,0,0,0}}
        ]),
        %% 获取所有接口的IPv4地址并加入多播组
        {ok, IfAddrs} = inet:getifaddrs(),
        lists:foreach(fun({IfName, Opts}) ->
            case proplists:get_value(addr, Opts) of
                {A,B,C,D} = IP when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
                    case inet:setopts(Socket, [{add_membership, {{226,0,0,80}, IP}}]) of
                        ok ->
                            ?LOG(debug, "Added multicast membership on ~s (~s)", [IfName, inet:ntoa(IP)]);
                        {error, Reason} ->
                            ?LOG(warning, "Failed to add multicast membership on ~s: ~p", [IfName, Reason])
                    end;
                _ ->
                    ok
            end
        end, IfAddrs),
        {ok, Socket}
    catch
        _:Error -> {error, Error}
    end.

%% 根据源地址获取设备标识（工位名称或无人机地址）
-spec get_device_name_by_src(integer()) -> binary().
get_device_name_by_src(16#5D11) -> <<"拷机1">>;
get_device_name_by_src(16#5CC1) -> <<"拷机1">>;
get_device_name_by_src(16#5CD1) -> <<"桁架">>;
get_device_name_by_src(Src) when is_integer(Src) -> 
    try
        % io_lib:format返回字符列表，需要转换为二进制
        CharList = io_lib:format("无人机(0x~4.16.0B)", [Src]),
        unicode:characters_to_binary(CharList, utf8)
    catch
        _:_ -> <<"无人机(未知地址)"/utf8>>
    end;
get_device_name_by_src(_Src) -> 
    <<"无人机(无效地址)"/utf8>>.  %% 处理非整数参数

process_packet(IP, Port, Packet, LocalPort) ->
    %% 头部调试信息降为 debug 级别
    HeadHex = case byte_size(Packet) >= 4 of
        true -> dgiot_utils:binary_to_hex(binary:part(Packet, 0, 4));
        false -> <<"too short">>
    end,
    
    %% UDP报文详细打印（根据用户要求"UDP报文稍微全一些"）
    %% 基本信息使用 info 级别
    ?LOG(info, "UDP报文接收: 端口=~p, 来源=~p:~p, 大小=~p字节, 首字节=~p", 
         [LocalPort, IP, Port, byte_size(Packet), binary:first(Packet)]),
    
    %% 添加头部十六进制信息（debug级别）
    ?LOG(debug, "UDP报文前4字节: ~s", [HeadHex]),
    
    %% 添加更详细的报文分析
    case byte_size(Packet) of
        Size when Size >= 8 ->
            %% 打印前8字节的十六进制
            First8Hex = dgiot_utils:binary_to_hex(binary:part(Packet, 0, min(8, Size))),
            ?LOG(debug, "UDP报文前8字节: ~s", [First8Hex]),
            
            %% 打印同步头判断
            SyncHeader = binary:part(Packet, 0, 2),
            case SyncHeader of
                <<16#EB, 16#90>> ->
                    ?LOG(debug, "UDP报文同步头: EB90 (正确)");
                _ ->
                    ?LOG(debug, "UDP报文同步头: ~s (非EB90)", [dgiot_utils:binary_to_hex(SyncHeader)])
            end;
        _ ->
            ?LOG(debug, "UDP报文过短，无法分析详细内容")
    end,

    %% 完整报文打印保持 debug 级别
    ?LOG(debug, "UDP完整报文十六进制: ~s", [dgiot_utils:binary_to_hex(Packet)]),

    case LocalPort of
        8000 ->
            %% 复合数据（格式未知，暂不处理）
            ?LOG(debug, "Composite data on port 8000, ignoring");
        8001 ->
            %% 遥测数据
            case eb90_link_protocol:parse_telemetry_frame(Packet) of
                {ok, Telemetry} ->
                    %% 只处理链路遥测帧的入网申请
                    case maps:get(frame_type, Telemetry, unknown) of
                        link_telemetry ->
                            %% 检查入网申请，若有效则处理
                            case maps:get(network_apply, Telemetry, undefined) of
                                #{valid := true, address := Addr} ->
                                    SrcAddr = maps:get(src_addr, Telemetry),
                                    DeviceName = get_device_name_by_src(SrcAddr),
                                    Now = erlang:system_time(millisecond),
                                    LogTimes = get(apply_log_times),
                                    %% 检查是否应该打印日志（同一设备10秒内不重复打印）
                                    ShouldLog = case maps:find(Addr, LogTimes) of
                                        {ok, LastLogTime} when Now - LastLogTime < ?APPLY_LOG_INTERVAL ->
                                            false;
                                        _ ->
                                            true
                                    end,
                                    if ShouldLog ->
                                        ?LOG(info, "UDP入网申请: device=~s, addr=0x~4.16.0B, src=0x~4.16.0B, frame_no=~p",
                                             [DeviceName, Addr, SrcAddr, maps:get(frame_no, Telemetry)]),
                                        put(apply_log_times, LogTimes#{Addr => Now});
                                       true ->
                                        %% 不打印日志，但保留 debug 级别的简要记录（可选）
                                        ?LOG(debug, "入网申请(抑制): device=~s, addr=0x~4.16.0B", [DeviceName, Addr])
                                    end;
                                _ ->
                                    %% 入网申请无效或不存在
                                    ?LOG(debug, "入网申请无效: telemetry=~p", [Telemetry])
                            end;
                        fc_telemetry ->
                            %% 飞控遥测数据
                            SrcAddr = maps:get(src_addr, Telemetry),
                            FrameNo = maps:get(frame_no, Telemetry),
                            ?LOG(debug, "收到飞控遥测: src=0x~4.16.0B, frame_no=~p, payload_size=~p",
                                 [SrcAddr, FrameNo, byte_size(maps:get(payload, Telemetry, <<>>))]);
                        remote_control ->
                            %% 遥控数据
                            ?LOG(debug, "收到遥控数据: ~p", [Telemetry]);
                        _ ->
                            %% 其他类型的遥测数据
                            ?LOG(debug, "收到其他遥测: frame_type=~p", [maps:get(frame_type, Telemetry, unknown)])
                    end,
                    handle_telemetry(Telemetry);
                {error, Reason} ->
                    ?LOG(error, "Telemetry parse failed: ~p", [Reason])
            end;
        8002 ->
            %% 遥控数据
            case eb90_link_protocol:parse_remote_control_frame(Packet) of
                {ok, Control} ->
                    handle_control(Control);
                {error, Reason} ->
                    ?LOG(error, "Control parse failed: ~p", [Reason])
            end;
        _ ->
            ?LOG(warning, "Unknown port ~p", [LocalPort])
    end.

handle_telemetry(Telemetry) ->
    ?LOG(debug, "Telemetry from 0x~4.16.0B, dest=0x~4.16.0B",
         [maps:get(src_addr, Telemetry), maps:get(dest_addr, Telemetry)]),

    %% 只有链路遥测帧才有 network_apply 字段
    case maps:get(frame_type, Telemetry, unknown) of
        link_telemetry ->
            %% 关键事件：入网申请已在 process_packet 中记录，此处只执行业务逻辑
            case maps:get(network_apply, Telemetry, undefined) of
                #{valid := true, address := Addr} ->
                    %% 打印入网许可发送日志
                    SrcAddr = maps:get(src_addr, Telemetry),
                    DeviceName = get_device_name_by_src(SrcAddr),
                    ?LOG(info, "发送入网许可: device=~s, addr=0x~4.16.0B, src=0x~4.16.0B",
                         [DeviceName, Addr, SrcAddr]),
                    %% 发送入网许可（可根据业务策略决定允许/拒绝）
                    send_network_response(Addr, allow);
                _ ->
                    ok
            end;
        fc_telemetry ->
            %% 飞控遥测帧，没有 network_apply 字段
            ok;
        _ ->
            %% 其他类型遥测帧
            ok
    end,

    %% 可选：存储遥测数据到物模型（根据实际需求实现 store_telemetry/1）
    %% store_telemetry(Telemetry),
    ok.

handle_control(Control) ->
    ?LOG(debug, "Control frame from 0x~4.16.0B, adjust_command=~p",
         [maps:get(src_addr, Control), maps:get(adjust_command, Control)]),
    %% 关键事件：入网许可/拒绝 -> 提升为 error 级别（遥控帧频率通常不高，无需抑制）
    case maps:get(adjust_command, Control) of
        {network_allow, Addr} ->
            ?LOG(error, "收到入网许可: addr=0x~4.16.0B", [Addr]),
            store_node_info(Addr);
        {network_reject, Addr} ->
            ?LOG(error, "收到入网拒绝: addr=0x~4.16.0B", [Addr]);
        _ ->
            %% 其他遥控指令，可根据需要处理
            ok
    end.

send_network_response(Addr, allow) ->
    %% 构建入网许可遥控帧
    Params = #{
        dest_addr => Addr,
        src_addr => 16#0001,    %% 地面站地址
        frame_no => 1,
        switch_commands => [],   %% 无开关指令
        adjust_command => {network_allow, Addr}
    },
    Frame = eb90_link_protocol:build_remote_control_frame(Params),
    %% 通过UDP发送到多播组226.0.0.80:8002
    case gen_udp:open(0, [binary]) of
        {ok, Socket} ->
            ok = gen_udp:send(Socket, {226,0,0,80}, 8002, Frame),
            gen_udp:close(Socket),
            ?LOG(error, "发送入网许可给 0x~4.16.0B", [Addr]);  %% 关键事件提升为 error
        {error, Reason} ->
            ?LOG(error, "发送入网许可失败: ~p", [Reason])
    end.

store_node_info(Addr) ->
    try
        dgiot_data:insert(uav_online_nodes, Addr, #{
            address => Addr,
            last_seen => erlang:system_time(millisecond)
        })
    catch
        _:_ -> ?LOG(warning, "存储节点信息失败，表未初始化", [])
    end.

update_stats(State) ->
    State#state{
        packet_count = State#state.packet_count + 1,
        last_packet_time = erlang:system_time(millisecond)
    }.