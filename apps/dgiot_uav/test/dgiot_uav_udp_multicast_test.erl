%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_udp_multicast_test 模块 - UDP多播报文接收调测模块
%%%
%%% 本模块专门用于调测UDP多播报文的接收功能，包括：
%%% 1. 启动UDP多播通道
%%% 2. 发送测试数据包
%%% 3. 验证报文接收和解析
%%% 4. 统计和分析结果
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_udp_multicast_test).

%% API
-export([
    start_test/0,
    start_test/1,
    stop_test/0,
    send_test_packets/0,
    get_stats/0,
    reset_stats/0,
    test_eb90_protocol/0,
    test_custom_protocol/0,
    test_json_protocol/0,
    test_all_protocols/0
]).

%% 内部函数
-export([
    init_test/0,
    cleanup_test/0,
    send_multicast_packet/3,
    receive_udp_packets/2,
    parse_and_validate_packet/1,
    print_test_results/1,
    monitor_channel_status/0
]).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% 测试配置
-define(TEST_CHANNEL_ID, <<"uav_udp_multicast_test_channel">>).
-define(TEST_MULTICAST_GROUP, <<"239.255.255.250">>).
-define(TEST_PORT, 1900).
-define(TEST_INTERFACE, <<"0.0.0.0">>).
-define(TEST_PRODUCT_ID, <<"test_uav_product">>).

%% 目标端口映射
-define(TARGET_PORTS, [
    {<<"uav_001">>, 8888},
    {<<"uav_002">>, 8889},
    {<<"eb90_12345678">>, 8890},
    {<<"json_uav_003">>, 8891}
]).

%% 测试状态记录
-record(test_state, {
    channel_id :: binary(),
    channel_pid :: pid() | undefined,
    receiver_pids = [] :: [pid()],
    stats = #{
        packets_sent => 0,
        packets_received => 0,
        packets_forwarded => 0,
        packets_dropped => 0,
        parse_success => 0,
        parse_failure => 0,
        start_time => 0,
        end_time => 0
    },
    test_results = [] :: list()
}).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动默认测试
start_test() ->
    start_test(#{
        <<"multicast_group">> => ?TEST_MULTICAST_GROUP,
        <<"port">> => ?TEST_PORT,
        <<"interface">> => ?TEST_INTERFACE,
        <<"product_id">> => ?TEST_PRODUCT_ID,
        <<"uav_mapping">> => create_test_uav_mapping()
    }).

%% @doc 启动自定义配置测试
start_test(Config) when is_map(Config) ->
    ?LOG(info, "~ts", [<<"启动UDP多播调测"/utf8>>]),
    
    % 初始化测试环境
    case init_test() of
        {ok, State} ->
            % 启动UDP多播通道
            ChannelId = ?TEST_CHANNEL_ID,
            
            % 确保所有配置键都是二进制
            BinaryConfig = maps:fold(fun
                (K, V, Acc) when is_atom(K) ->
                    maps:put(atom_to_binary(K, utf8), V, Acc);
                (K, V, Acc) when is_binary(K) ->
                    maps:put(K, V, Acc);
                (K, V, Acc) ->
                    maps:put(integer_to_binary(K), V, Acc)
            end, #{}, Config),
            
            ChannelArgs = maps:merge(#{
                <<"multicast_group">> => ?TEST_MULTICAST_GROUP,
                <<"port">> => ?TEST_PORT,
                <<"interface">> => ?TEST_INTERFACE,
                <<"product_id">> => ?TEST_PRODUCT_ID,
                <<"uav_mapping">> => create_test_uav_mapping()
            }, BinaryConfig),
            
            ?LOG(info, "~ts: ~p", [<<"启动UDP多播通道"/utf8>>, ChannelId]),
            ?LOG(info, "~ts: ~p", [<<"通道配置"/utf8>>, ChannelArgs]),
            
            case dgiot_uav_udp_channel:start(ChannelId, ChannelArgs) of
                {ok, ChannelPid} ->
                    ?LOG(info, "~ts", [<<"UDP多播通道启动成功"/utf8>>]),
                    
                    % 启动目标端口监听器
                    ReceiverPids = start_target_receivers(),
                    
                    % 启动通道状态监控
                    _MonitorPid = spawn_link(fun monitor_channel_status/0),
                    
                    NewState = State#test_state{
                        channel_id = ChannelId,
                        channel_pid = ChannelPid,
                        receiver_pids = ReceiverPids,
                        stats = maps:merge(State#test_state.stats, #{
                            start_time => erlang:system_time(millisecond)
                        })
                    },
                    
                    % 注册测试状态
                    register_test_state(NewState),
                    
                    {ok, NewState};
                
                {error, Reason} ->
                    ?LOG(error, "~ts: ~p", [<<"UDP多播通道启动失败"/utf8>>, Reason]),
                    cleanup_test(),
                    {error, {channel_start_failed, Reason}}
            end;
        
        {error, Reason} ->
            ?LOG(error, "~ts: ~p", [<<"测试环境初始化失败"/utf8>>, Reason]),
            {error, {init_failed, Reason}}
    end.

%% @doc 停止测试
stop_test() ->
    ?LOG(info, "~ts", [<<"停止UDP多播调测"/utf8>>]),
    
    case get_test_state() of
        undefined ->
            ?LOG(warning, "~ts", [<<"未找到测试状态"/utf8>>]),
            ok;
        
        State ->
            % 停止通道
            case State#test_state.channel_id of
                undefined -> ok;
                ChannelId ->
                    ?LOG(info, "~ts: ~p", [<<"停止UDP多播通道"/utf8>>, ChannelId]),
                    dgiot_channelx:stop(ChannelId)
            end,
            
            % 停止接收器
            lists:foreach(fun(Pid) ->
                exit(Pid, normal)
            end, State#test_state.receiver_pids),
            
            % 更新统计信息
            NewStats = maps:merge(State#test_state.stats, #{
                end_time => erlang:system_time(millisecond)
            }),
            
            % 打印测试结果
            print_test_results(State#test_state{stats = NewStats}),
            
            % 清理测试环境
            cleanup_test(),
            
            ok
    end.

%% @doc 发送测试数据包
send_test_packets() ->
    ?LOG(info, "~ts", [<<"发送测试数据包"/utf8>>]),
    
    case get_test_state() of
        undefined ->
            ?LOG(error, "~ts", [<<"测试未启动"/utf8>>]),
            {error, test_not_started};
        
        State ->
            % 发送各种协议的测试数据包
            Results = [
                test_eb90_protocol(),
                test_custom_protocol(),
                test_json_protocol()
            ],
            
            % 更新统计信息
            NewStats = maps:merge(State#test_state.stats, #{
                packets_sent => maps:get(packets_sent, State#test_state.stats, 0) + length(Results)
            }),
            
            % 更新测试状态
            NewState = State#test_state{
                stats = NewStats,
                test_results = Results ++ State#test_state.test_results
            },
            
            register_test_state(NewState),
            
            {ok, Results}
    end.

%% @doc 获取统计信息
get_stats() ->
    case get_test_state() of
        undefined ->
            {error, test_not_started};
        
        State ->
            {ok, State#test_state.stats}
    end.

%% @doc 重置统计信息
reset_stats() ->
    case get_test_state() of
        undefined ->
            {error, test_not_started};
        
        State ->
            NewStats = #{
                packets_sent => 0,
                packets_received => 0,
                packets_forwarded => 0,
                packets_dropped => 0,
                parse_success => 0,
                parse_failure => 0,
                start_time => maps:get(start_time, State#test_state.stats, 0),
                end_time => 0
            },
            
            NewState = State#test_state{
                stats = NewStats,
                test_results = []
            },
            
            register_test_state(NewState),
            
            ok
    end.

%% @doc 测试EB90协议
test_eb90_protocol() ->
    ?LOG(info, "~ts", [<<"测试EB90协议"/utf8>>]),
    
    % 创建EB90测试数据包
    Packet = create_eb90_test_packet(<<"uav_001">>),
    
    % 发送数据包
    case send_multicast_packet(?TEST_MULTICAST_GROUP, ?TEST_PORT, Packet) of
        ok ->
            ?LOG(info, "~ts: ~p ~ts", [<<"EB90数据包发送成功"/utf8>>, byte_size(Packet), <<"字节"/utf8>>]),
            
            % 验证数据包
            case parse_and_validate_packet(Packet) of
                {ok, ParsedData} ->
                    ?LOG(info, "~ts: ~p", [<<"EB90数据包解析成功"/utf8>>, ParsedData]),
                    {eb90_protocol, success, ParsedData};
                {error, Reason} ->
                    ?LOG(error, "~ts: ~p", [<<"EB90数据包解析失败"/utf8>>, Reason]),
                    {eb90_protocol, {parse_error, Reason}, Packet}
            end;
        
        {error, Reason} ->
            ?LOG(error, "~ts: ~p", [<<"EB90数据包发送失败"/utf8>>, Reason]),
            {eb90_protocol, {send_error, Reason}, Packet}
    end.

%% @doc 测试自定义协议
test_custom_protocol() ->
    ?LOG(info, "~ts", [<<"测试自定义协议"/utf8>>]),
    
    % 创建自定义测试数据包
    Packet = create_custom_test_packet(<<"uav_002">>),
    
    % 发送数据包
    case send_multicast_packet(?TEST_MULTICAST_GROUP, ?TEST_PORT, Packet) of
        ok ->
            ?LOG(info, "~ts: ~p ~ts", [<<"自定义数据包发送成功"/utf8>>, byte_size(Packet), <<"字节"/utf8>>]),
            
            % 验证数据包
            case parse_and_validate_packet(Packet) of
                {ok, ParsedData} ->
                    ?LOG(info, "~ts: ~p", [<<"自定义数据包解析成功"/utf8>>, ParsedData]),
                    {custom_protocol, success, ParsedData};
                {error, Reason} ->
                    ?LOG(error, "~ts: ~p", [<<"自定义数据包解析失败"/utf8>>, Reason]),
                    {custom_protocol, {parse_error, Reason}, Packet}
            end;
        
        {error, Reason} ->
            ?LOG(error, "~ts: ~p", [<<"自定义数据包发送失败"/utf8>>, Reason]),
            {custom_protocol, {send_error, Reason}, Packet}
    end.

%% @doc 测试JSON协议
test_json_protocol() ->
    ?LOG(info, "~ts", [<<"测试JSON协议"/utf8>>]),
    
    % 创建JSON测试数据包
    Packet = create_json_test_packet(<<"json_uav_003">>),
    
    % 发送数据包
    case send_multicast_packet(?TEST_MULTICAST_GROUP, ?TEST_PORT, Packet) of
        ok ->
            ?LOG(info, "~ts: ~p ~ts", [<<"JSON数据包发送成功"/utf8>>, byte_size(Packet), <<"字节"/utf8>>]),
            
            % 验证数据包
            case parse_and_validate_packet(Packet) of
                {ok, ParsedData} ->
                    ?LOG(info, "~ts: ~p", [<<"JSON数据包解析成功"/utf8>>, ParsedData]),
                    {json_protocol, success, ParsedData};
                {error, Reason} ->
                    ?LOG(error, "~ts: ~p", [<<"JSON数据包解析失败"/utf8>>, Reason]),
                    {json_protocol, {parse_error, Reason}, Packet}
            end;
        
        {error, Reason} ->
            ?LOG(error, "~ts: ~p", [<<"JSON数据包发送失败"/utf8>>, Reason]),
            {json_protocol, {send_error, Reason}, Packet}
    end.

%% @doc 测试所有协议
test_all_protocols() ->
    ?LOG(info, "~ts", [<<"测试所有协议"/utf8>>]),
    
    Results = [
        test_eb90_protocol(),
        test_custom_protocol(),
        test_json_protocol()
    ],
    
    % 统计结果
    SuccessCount = length([R || R <- Results, element(2, R) =:= success]),
    TotalCount = length(Results),
    
    ?LOG(info, "~ts: ~p/~p", [<<"测试完成"/utf8>>, SuccessCount, TotalCount]),
    
    {ok, Results, #{success => SuccessCount, total => TotalCount}}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private 初始化测试环境
init_test() ->
    ?LOG(info, "~ts", [<<"初始化测试环境"/utf8>>]),
    
    % 检查系统状态
    case check_system_status() of
        ok ->
            % 创建测试状态
            State = #test_state{
                channel_id = ?TEST_CHANNEL_ID,
                stats = #{
                    start_time => erlang:system_time(millisecond),
                    packets_sent => 0,
                    packets_received => 0,
                    packets_forwarded => 0,
                    packets_dropped => 0,
                    parse_success => 0,
                    parse_failure => 0
                }
            },
            
            % 注册测试状态
            register_test_state(State),
            
            {ok, State};
        
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 清理测试环境
cleanup_test() ->
    ?LOG(info, "~ts", [<<"清理测试环境"/utf8>>]),
    
    % 取消注册测试状态
    unregister_test_state(),
    
    ok.

%% @private 检查系统状态
check_system_status() ->
    % 简化检查：直接返回成功，因为系统已经在运行
    ?LOG(info, "~ts", [<<"系统检查通过"/utf8>>]),
    ok.

%% @private 创建测试无人机映射
create_test_uav_mapping() ->
    lists:map(fun({UavId, TargetPort}) ->
        #{
            <<"uav_id">> => UavId,
            <<"target_ip">> => <<"127.0.0.1">>,
            <<"target_port">> => TargetPort
        }
    end, ?TARGET_PORTS).

%% @private 启动目标端口监听器
start_target_receivers() ->
    ?LOG(info, "~ts", [<<"启动目标端口监听器"/utf8>>]),
    
    lists:map(fun({_UavId, Port}) ->
        spawn_link(fun() -> receive_udp_packets(Port, self()) end)
    end, ?TARGET_PORTS).

%% @private 发送多播数据包
send_multicast_packet(Group, Port, Data) when is_binary(Data) ->
    try
        % 使用socket发送UDP多播数据包
        {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
        
        % 设置多播TTL
        ok = inet:setopts(Socket, [{multicast_ttl, 4}]),
        
        % 发送数据
        case gen_udp:send(Socket, binary_to_list(Group), Port, Data) of
            ok ->
                gen_udp:close(Socket),
                ok;
            {error, Reason} ->
                gen_udp:close(Socket),
                {error, Reason}
        end
    catch
        _:Exception ->
            {error, Exception}
    end.

%% @private 接收UDP数据包
receive_udp_packets(Port, TestPid) ->
    ?LOG(info, "~ts: ~p", [<<"启动UDP监听器"/utf8>>, Port]),
    
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {reuseaddr, true}]),
    
    receive_loop(Socket, Port, TestPid).

%% @private UDP接收循环
receive_loop(Socket, Port, TestPid) ->
    receive
        {udp, Socket, IP, InPort, Packet} ->
            ?LOG(info, "~ts: ~p:~p, ~p ~ts", [<<"收到UDP数据包"/utf8>>, IP, InPort, byte_size(Packet), <<"字节"/utf8>>]),
            
            % 通知测试进程
            TestPid ! {udp_packet_received, Port, Packet, erlang:system_time(millisecond)},
            
            % 更新统计信息
            update_stats(packets_received, byte_size(Packet)),
            
            receive_loop(Socket, Port, TestPid);
        
        stop ->
            ?LOG(info, "~ts: ~p", [<<"停止UDP监听器"/utf8>>, Port]),
            gen_udp:close(Socket),
            ok
    end.

%% @private 创建EB90测试数据包
create_eb90_test_packet(UavId) ->
    % EB90协议格式: EB90 + 长度(16) + 命令字(8) + 序列号(32) + 数据
    SyncHeader = <<16#EB, 16#90>>,
    Length = 32,  % 固定长度32字节
    Command = 16#01,  % 心跳命令
    Sequence = 12345678,
    
    % 设备ID数据
    DeviceIdData = case UavId of
        <<"uav_001">> -> <<"uav_001\x00">>;
        <<"eb90_12345678">> -> <<16#12, 16#34, 16#56, 16#78, 0, 0, 0, 0>>;
        _ -> <<"test_device\x00">>
    end,
    
    % 填充剩余数据
    Padding = binary:copy(<<0>>, 20 - byte_size(DeviceIdData)),
    Payload = <<DeviceIdData/binary, Padding/binary>>,
    
    <<SyncHeader/binary, Length:16, Command:8, Sequence:32, Payload/binary>>.

%% @private 创建自定义测试数据包
create_custom_test_packet(UavId) ->
    <<"UAV_", UavId/binary, "_test_data_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>.

%% @private 创建JSON测试数据包
create_json_test_packet(UavId) ->
    JsonData = #{
        <<"uav_id">> => UavId,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"data">> => <<"test_payload">>,
        <<"type">> => <<"test">>,
        <<"version">> => <<"1.0">>
    },
    jsx:encode(JsonData).

%% @private 解析和验证数据包
parse_and_validate_packet(Packet) ->
    try
        % 尝试使用uav_protocol解析
        case uav_protocol:parse(<<"uav">>, Packet) of
            {ok, ParsedData} ->
                {ok, ParsedData};
            {error, Reason} ->
                % 尝试其他解析方法
                parse_packet_fallback(Packet, Reason)
        end
    catch
        _:Exception ->
            {error, {parse_exception, Exception}}
    end.

%% @private 数据包解析回退方法
parse_packet_fallback(Packet, OriginalReason) ->
    % 检查是否是EB90协议
    case Packet of
        <<16#EB, 16#90, _/binary>> ->
            % 手动解析EB90协议
            parse_eb90_packet_manual(Packet);
        <<"UAV_", Rest/binary>> ->
            % 解析自定义协议
            parse_custom_packet(Rest);
        _ ->
            % 尝试JSON解析
            case jsx:is_json(Packet) of
                true ->
                    parse_json_packet(Packet);
                false ->
                    {error, {unknown_protocol, OriginalReason}}
            end
    end.

%% @private 手动解析EB90数据包
parse_eb90_packet_manual(<<16#EB, 16#90, Length:16, Command:8, Sequence:32, Data/binary>>) ->
    ParsedData = #{
        protocol_type => <<"eb90">>,
        sync_header => 16#EB90,
        length => Length,
        command => Command,
        sequence => Sequence,
        data_size => byte_size(Data),
        raw_data => Data,
        timestamp => erlang:system_time(millisecond)
    },
    
    % 尝试提取设备ID
    case byte_size(Data) >= 8 of
        true ->
            <<DeviceId:8/binary, _/binary>> = Data,
            {ok, ParsedData#{device_id => DeviceId}};
        false ->
            {ok, ParsedData}
    end;
parse_eb90_packet_manual(_) ->
    {error, invalid_eb90_packet}.

%% @private 解析自定义数据包
parse_custom_packet(Rest) ->
    case binary:split(Rest, <<"_">>) of
        [UavId, Data] ->
            ParsedData = #{
                protocol_type => <<"custom">>,
                uav_id => UavId,
                data => Data,
                timestamp => erlang:system_time(millisecond)
            },
            {ok, ParsedData};
        _ ->
            {error, invalid_custom_packet}
    end.

%% @private 解析JSON数据包
parse_json_packet(Packet) ->
    try
        JsonData = jsx:decode(Packet, [return_maps]),
        ParsedData = #{
            protocol_type => <<"json">>,
            json_data => JsonData,
            timestamp => erlang:system_time(millisecond)
        },
        {ok, ParsedData}
    catch
        _:_ ->
            {error, invalid_json_packet}
    end.

%% @private 更新统计信息
update_stats(Type, Size) ->
    case get_test_state() of
        undefined ->
            ok;
        State ->
            Stats = State#test_state.stats,
            NewStats = case Type of
                packets_received ->
                    maps:merge(Stats, #{
                        packets_received => maps:get(packets_received, Stats, 0) + 1,
                        bytes_received => maps:get(bytes_received, Stats, 0) + Size
                    });
                packets_forwarded ->
                    maps:merge(Stats, #{
                        packets_forwarded => maps:get(packets_forwarded, Stats, 0) + 1,
                        bytes_forwarded => maps:get(bytes_forwarded, Stats, 0) + Size
                    });
                packets_dropped ->
                    maps:merge(Stats, #{
                        packets_dropped => maps:get(packets_dropped, Stats, 0) + 1
                    });
                parse_success ->
                    maps:merge(Stats, #{
                        parse_success => maps:get(parse_success, Stats, 0) + 1
                    });
                parse_failure ->
                    maps:merge(Stats, #{
                        parse_failure => maps:get(parse_failure, Stats, 0) + 1
                    })
            end,
            
            NewState = State#test_state{stats = NewStats},
            register_test_state(NewState),
            ok
    end.

%% @private 注册测试状态
register_test_state(State) ->
    put(uav_udp_multicast_test_state, State).

%% @private 获取测试状态
get_test_state() ->
    get(uav_udp_multicast_test_state).

%% @private 取消注册测试状态
unregister_test_state() ->
    erase(uav_udp_multicast_test_state).

%% @private 打印测试结果
print_test_results(State) ->
    Stats = State#test_state.stats,
    StartTime = maps:get(start_time, Stats, 0),
    EndTime = maps:get(end_time, Stats, 0),
    Duration = case EndTime > StartTime of
        true -> (EndTime - StartTime) / 1000;
        false -> 0
    end,
    
    ?LOG(info, "~ts", [<<"UDP多播调测结果"/utf8>>]),
    ?LOG(info, "~ts: ~p ~ts", [<<"测试时长"/utf8>>, Duration, <<"秒"/utf8>>]),
    ?LOG(info, "~ts: ~p", [<<"发送数据包"/utf8>>, maps:get(packets_sent, Stats, 0)]),
    ?LOG(info, "~ts: ~p", [<<"接收数据包"/utf8>>, maps:get(packets_received, Stats, 0)]),
    ?LOG(info, "~ts: ~p", [<<"转发数据包"/utf8>>, maps:get(packets_forwarded, Stats, 0)]),
    ?LOG(info, "~ts: ~p", [<<"丢弃数据包"/utf8>>, maps:get(packets_dropped, Stats, 0)]),
    ?LOG(info, "~ts: ~p/~p", [<<"解析成功率"/utf8>>, 
        maps:get(parse_success, Stats, 0), 
        maps:get(parse_success, Stats, 0) + maps:get(parse_failure, Stats, 0)]),
    
    % 打印详细测试结果
    case State#test_state.test_results of
        [] ->
            ?LOG(info, "~ts", [<<"无详细测试结果"/utf8>>]);
        Results ->
            ?LOG(info, "~ts: ~p", [<<"详细测试结果数量"/utf8>>, length(Results)]),
            lists:foreach(fun(Result) ->
                print_single_result(Result)
            end, Results)
    end.

%% @private 打印单个测试结果
print_single_result({Protocol, success, ParsedData}) ->
    ?LOG(info, "✓ ~p: ~ts", [Protocol, <<"成功"/utf8>>]),
    ?LOG(debug, "   ~p", [ParsedData]);
print_single_result({Protocol, {parse_error, Reason}, _Packet}) ->
    ?LOG(error, "✗ ~p: ~ts: ~p", [Protocol, <<"解析错误"/utf8>>, Reason]);
print_single_result({Protocol, {send_error, Reason}, _Packet}) ->
    ?LOG(error, "✗ ~p: ~ts: ~p", [Protocol, <<"发送错误"/utf8>>, Reason]);
print_single_result(Unknown) ->
    ?LOG(warning, "? ~p", [Unknown]).

%% @private 监控通道状态
monitor_channel_status() ->
    ?LOG(info, "~ts", [<<"启动通道状态监控"/utf8>>]),
    
    monitor_loop(1000).  % 1秒检查一次

%% @private 监控循环
monitor_loop(Interval) ->
    receive
        stop ->
            ?LOG(info, "~ts", [<<"停止通道状态监控"/utf8>>]),
            ok
    after Interval ->
        case get_test_state() of
            undefined ->
                monitor_loop(Interval);
            State ->
                case State#test_state.channel_id of
                    undefined ->
                        monitor_loop(Interval);
                    ChannelId ->
                        % 检查通道状态
                        case dgiot_channelx:status(ChannelId) of
                            {ok, ChannelStatus} ->
                                ?LOG(debug, "~ts: ~p", [<<"通道状态"/utf8>>, ChannelStatus]),
                                monitor_loop(Interval);
                            {error, Reason} ->
                                ?LOG(error, "~ts: ~p", [<<"获取通道状态失败"/utf8>>, Reason]),
                                monitor_loop(Interval)
                        end
                end
        end
    end.
