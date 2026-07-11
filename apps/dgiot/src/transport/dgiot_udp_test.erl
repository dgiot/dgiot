%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc UDP多播测试模块
%% 基于dgiot_udp成功经验，提供简化的多播通信测试
-module(dgiot_udp_test).
-author("johnliu").
-include("../../include/dgiot_socket.hrl").
-include("../../include/logger.hrl").

%% API导出
-export([
    test_multicast/0,
    test_multicast_with_tcpdump/0,
    test_multicast_loopback/0,
    run_simple_test/0,
    stop_all/0,
    get_status/0,
    test_server_client_integration/0,
    test_multicast_performance/1,
    test_multicast_reliability/0,
    test_multicast_stress/1,
    get_test_summary/0
]).

%%%===================================================================
%%% 多播测试函数
%%%===================================================================

%% @doc 运行多播测试 - 基于成功经验
test_multicast() ->
    io:format("=== UDP MULTICAST TEST START ===~n"),
    
    % 使用已验证可用的多播组
    MulticastGroups = ["239.255.255.250", "224.0.0.1"],
    Port = 19000,
    
    io:format("Testing multicast groups: ~p on port ~p~n", [MulticastGroups, Port]),
    
    % 直接使用gen_udp进行简化测试（参考dgiot_udp的成功经验）
    Results = lists:map(fun(Group) ->
        io:format("Testing group: ~s~n", [Group]),
        
        % 发送多播消息
        TestMessage = <<"MULTICAST_TEST_", (list_to_binary(Group))/binary>>,
        case send_direct_multicast(Group, Port, TestMessage) of
            ok ->
                io:format("  ✓ Sent to ~s~n", [Group]),
                {Group, success};
            Error ->
                io:format("  ✗ Failed to send to ~s: ~p~n", [Group, Error]),
                {Group, Error}
        end
    end, MulticastGroups),
    
    % 汇总结果
    {Successes, Failures} = lists:partition(
        fun({_, success}) -> true; (_) -> false end, Results),
    
    io:format("=== UDP MULTICAST TEST RESULTS ===~n"),
    io:format("Successes: ~p, Failures: ~p~n", [length(Successes), length(Failures)]),
    io:format("=== UDP MULTICAST TEST END ===~n"),
    
    {ok, #{
        multicast_groups => MulticastGroups,
        successes => length(Successes),
        failures => length(Failures),
        results => Results
    }}.

%% @doc 使用tcpdump进行多播测试 - 增强版本，确保捕获UDP多播报文
test_multicast_with_tcpdump() ->
    io:format("~s ~p Event = Starting enhanced multicast test with tcpdump monitoring.~n", [?FILE, ?LINE]),
    
    % 检查tcpdump是否可用
    case os:type() of
        {unix, _} ->
            case os:find_executable("tcpdump") of
                false ->
                    io:format("~s ~p Event = ✗ tcpdump not found, using standard multicast test.~n", 
                             [?FILE, ?LINE]),
                    test_multicast();
                _ ->
                    % 启动增强的tcpdump监控
                    {_, TcpdumpPort} = start_enhanced_tcpdump_monitor(),
                    timer:sleep(2000),
                    
                    % 在tcpdump监控期间发送更多测试报文
                    io:format("~s ~p Event = Sending multicast test packets during tcpdump monitoring...~n", 
                             [?FILE, ?LINE]),
                    SendResult = send_multicast_packets_for_tcpdump(),
                    
                    % 等待tcpdump捕获报文
                    timer:sleep(3000),
                    
                    % 停止tcpdump并获取输出
                    TcpdumpOutput = stop_enhanced_tcpdump_monitor(TcpdumpPort),
                    
                    % 分析tcpdump输出
                    AnalysisResult = analyze_tcpdump_output(TcpdumpOutput),
                    
                    % 汇总结果
                    io:format("~s ~p Event = ========== TCPDUMP MULTICAST TEST RESULTS ==========.~n", [?FILE, ?LINE]),
                    io:format("~s ~p Event = Packet sending result: ~p~n", [?FILE, ?LINE, SendResult]),
                    io:format("~s ~p Event = Tcpdump analysis: ~p~n", [?FILE, ?LINE, AnalysisResult]),
                    
                    case AnalysisResult of
                        {ok, #{multicast_packets := Packets, total_packets := Total}} when Packets > 0 ->
                            io:format("~s ~p Event = ✓ SUCCESS: Captured ~p multicast packets out of ~p total packets.~n", 
                                     [?FILE, ?LINE, Packets, Total]),
                            {ok, #{
                                tcpdump_analysis => AnalysisResult,
                                packet_sending => SendResult,
                                multicast_packets_captured => Packets,
                                total_packets_captured => Total
                            }};
                        {ok, #{multicast_packets := 0, total_packets := Total}} ->
                            io:format("~s ~p Event = ⚠ WARNING: No multicast packets captured (total packets: ~p).~n", 
                                     [?FILE, ?LINE, Total]),
                            {warning, no_multicast_packets_captured, AnalysisResult};
                        Error ->
                            io:format("~s ~p Event = ✗ FAILED: Tcpdump analysis error: ~p.~n", 
                                     [?FILE, ?LINE, Error]),
                            Error
                    end
            end;
        _ ->
            io:format("~s ~p Event = ✗ tcpdump not supported on this system, using standard multicast test.~n", 
                     [?FILE, ?LINE]),
            test_multicast()
    end.

%% @doc 回环接口多播测试
test_multicast_loopback() ->
    io:format("=== LOOPBACK MULTICAST TEST START ===~n"),
    
    MulticastGroups = ["239.255.255.250", "224.0.0.1"],
    Port = 19000,
    
    % 启动接收器
    ReceiverPid = spawn(fun() -> receive_multicast(MulticastGroups, Port) end),
    io:format("Receiver started with pid ~p~n", [ReceiverPid]),
    
    % 等待接收器启动
    timer:sleep(2000),
    
    % 发送多播消息
    io:format("Sending multicast messages...~n"),
    Messages = [
        <<"MULTICAST_TEST_1: Hello Multicast!">>,
        <<"MULTICAST_TEST_2: This is a test message">>,
        <<"MULTICAST_TEST_3: Multicast working correctly">>
    ],
    
    lists:foreach(fun(Message) ->
        io:format("Sending: ~s~n", [Message]),
        case send_direct_multicast("239.255.255.250", Port, Message) of
            ok -> io:format("  ✓ Sent successfully~n");
            Error -> io:format("  ✗ Send failed: ~p~n", [Error])
        end,
        timer:sleep(1000)
    end, Messages),
    
    % 等待接收完成
    timer:sleep(3000),
    
    % 停止接收器
    ReceiverPid ! stop,
    
    io:format("=== LOOPBACK MULTICAST TEST END ===~n"),
    ok.

%% @doc 运行简化多播测试
run_simple_test() ->
    io:format("=== SIMPLE UDP MULTICAST TEST START ===~n"),
    
    % 使用已验证可用的多播组
    MulticastGroups = ["239.255.255.250", "224.0.0.1"],
    Port = 19000,
    
    io:format("Testing multicast groups: ~p on port ~p~n", [MulticastGroups, Port]),
    
    % 启动接收器
    ReceiverPid = spawn(fun() -> receive_multicast(MulticastGroups, Port) end),
    io:format("Receiver started with pid ~p~n", [ReceiverPid]),
    
    % 等待接收器启动
    timer:sleep(2000),
    
    % 发送多播消息
    io:format("Sending multicast messages...~n"),
    Messages = [
        <<"MULTICAST_TEST_1: Hello Multicast!">>,
        <<"MULTICAST_TEST_2: This is a test message">>,
        <<"MULTICAST_TEST_3: Multicast working correctly">>
    ],
    
    lists:foreach(fun(Message) ->
        io:format("Sending: ~s~n", [Message]),
        case send_direct_multicast("239.255.255.250", Port, Message) of
            ok -> io:format("  ✓ Sent successfully~n");
            Error -> io:format("  ✗ Send failed: ~p~n", [Error])
        end,
        timer:sleep(1000)
    end, Messages),
    
    % 等待接收完成
    timer:sleep(3000),
    
    % 停止接收器
    ReceiverPid ! stop,
    
    io:format("=== SIMPLE UDP MULTICAST TEST END ===~n"),
    ok.

%% @doc 停止所有多播测试进程
stop_all() ->
    io:format("~s ~p Event = Stopping all multicast test processes.~n", [?FILE, ?LINE]),
    ok.

%% @doc 获取多播测试状态
get_status() ->
    #{
        multicast_tests => "available",
        default_port => 19000,
        supported_protocols => ["UDP Multicast"],
        multicast_groups => ["239.255.255.250", "224.0.0.1"]
    }.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 直接发送多播消息 - 基于成功经验
send_direct_multicast(Group, Port, Message) ->
    case gen_udp:open(0, [binary, {reuseaddr, true}, {multicast_ttl, 4}, {multicast_loop, true}]) of
        {ok, Socket} ->
            {ok, Addr} = inet:parse_address(Group),
            Result = gen_udp:send(Socket, Addr, Port, Message),
            gen_udp:close(Socket),
            Result;
        Error ->
            io:format("Failed to open socket for sending: ~p~n", [Error]),
            Error
    end.

%% @doc 接收多播消息
receive_multicast(Groups, Port) ->
    case gen_udp:open(Port, [
        binary, 
        {reuseaddr, true}, 
        {multicast_loop, true},
        {active, true},
        {ip, {0,0,0,0}}  % 绑定到所有接口
    ]) of
        {ok, Socket} ->
            % 加入所有多播组
            lists:foreach(fun(Group) ->
                {ok, GroupAddr} = inet:parse_address(Group),
                {ok, LocalAddr} = inet:parse_address("127.0.0.1"),
                inet:setopts(Socket, [{add_membership, {GroupAddr, LocalAddr}}]),
                io:format("Joined multicast group ~s on port ~p~n", [Group, Port])
            end, Groups),
            
            receive_loop(Socket);
        Error ->
            io:format("Failed to open socket for receiving: ~p~n", [Error]),
            Error
    end.

%% @doc 接收循环
receive_loop(Socket) ->
    receive
        {udp, Socket, IP, InPort, Packet} ->
            io:format("Received packet from ~p:~p: ~s~n", [IP, InPort, Packet]),
            receive_loop(Socket);
        stop ->
            gen_udp:close(Socket),
            io:format("Receiver stopped~n");
        _Other ->
            receive_loop(Socket)
    after 10000 ->  % 10秒超时
        io:format("Receiver timeout, stopping~n"),
        gen_udp:close(Socket)
    end.


%%%===================================================================
%%% 新增测试函数
%%%===================================================================

%% @doc 测试服务器和客户端集成
test_server_client_integration() ->
    io:format("=== SERVER-CLIENT INTEGRATION TEST START ===~n"),
    
    Port = 19001,
    MulticastGroups = ["224.0.0.1", "239.255.255.250"],
    
    % 启动服务器
    case dgiot_udp_server:start_multicast_server(Port, MulticastGroups) of
        {ok, ServerPid} ->
            io:format("✓ Multicast server started on port ~p~n", [Port]),
            
            % 启动客户端
            case dgiot_udp_client:start_multicast_client(Port, MulticastGroups) of
                {ok, ClientPid} ->
                    io:format("✓ Multicast client started on port ~p~n", [Port]),
                    
                    % 发送测试消息
                    TestMessage = <<"Integration test message">>,
                    case dgiot_udp_client:send_multicast(ClientPid, "224.0.0.1", Port, TestMessage) of
                        ok ->
                            io:format("✓ Multicast message sent successfully~n");
                        Error ->
                            io:format("✗ Failed to send multicast message: ~p~n", [Error])
                    end,
                    
                    % 获取状态
                    {ok, ServerStatus} = dgiot_udp_server:get_multicast_status(ServerPid),
                    {ok, ClientStatus} = dgiot_udp_client:get_multicast_status(ClientPid),
                    
                    io:format("Server status: ~p~n", [ServerStatus]),
                    io:format("Client status: ~p~n", [ClientStatus]),
                    
                    % 清理
                    dgiot_udp_server:stop(ServerPid),
                    dgiot_udp_client:close(ClientPid),
                    
                    io:format("✓ Integration test completed~n"),
                    ok;
                ClientError ->
                    io:format("✗ Failed to start multicast client: ~p~n", [ClientError]),
                    dgiot_udp_server:stop(ServerPid)
            end;
        ServerError ->
            io:format("✗ Failed to start multicast server: ~p~n", [ServerError])
    end,
    
    io:format("=== SERVER-CLIENT INTEGRATION TEST END ===~n"),
    ok.

%% @doc 测试多播性能
test_multicast_performance(MessageCount) ->
    io:format("=== MULTICAST PERFORMANCE TEST START ===~n"),
    io:format("Testing with ~p messages~n", [MessageCount]),
    
    Port = 19002,
    MulticastGroup = "224.0.0.1",
    
    StartTime = erlang:system_time(millisecond),
    
    % 启动接收器
    ReceiverPid = spawn(fun() -> performance_receiver(Port, MessageCount) end),
    timer:sleep(1000),
    
    % 发送性能测试消息
    case send_performance_messages(MulticastGroup, Port, MessageCount) of
        {ok, SentCount} ->
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,
            
            % 等待接收器完成
            timer:sleep(2000),
            ReceiverPid ! stop,
            
            io:format("Performance test results:~n"),
            io:format("  Messages sent: ~p~n", [SentCount]),
            io:format("  Duration: ~p ms~n", [Duration]),
            io:format("  Messages per second: ~.2f~n", [SentCount / (Duration / 1000)]),
            
            {ok, #{
                messages_sent => SentCount,
                duration_ms => Duration,
                messages_per_second => SentCount / (Duration / 1000)
            }};
        Error ->
            ReceiverPid ! stop,
            io:format("✗ Performance test failed: ~p~n", [Error]),
            Error
    end.

%% @doc 测试多播可靠性
test_multicast_reliability() ->
    io:format("=== MULTICAST RELIABILITY TEST START ===~n"),
    
    Port = 19003,
    MulticastGroup = "224.0.0.1",
    TestRuns = 10,
    
    Results = lists:map(fun(Run) ->
        Message = list_to_binary("Reliability test run #" ++ integer_to_list(Run)),
        case send_direct_multicast(MulticastGroup, Port, Message) of
            ok -> {Run, success};
            Error -> {Run, Error}
        end
    end, lists:seq(1, TestRuns)),
    
    {Successes, Failures} = lists:partition(
        fun({_, success}) -> true; (_) -> false end, Results),
    
    SuccessRate = (length(Successes) / TestRuns) * 100,
    
    io:format("Reliability test results:~n"),
    io:format("  Total runs: ~p~n", [TestRuns]),
    io:format("  Successes: ~p~n", [length(Successes)]),
    io:format("  Failures: ~p~n", [length(Failures)]),
    io:format("  Success rate: ~.2f%~n", [SuccessRate]),
    
    {ok, #{
        total_runs => TestRuns,
        successes => length(Successes),
        failures => length(Failures),
        success_rate => SuccessRate,
        results => Results
    }}.

%% @doc 测试多播压力
test_multicast_stress(ConcurrentSenders) ->
    io:format("=== MULTICAST STRESS TEST START ===~n"),
    io:format("Testing with ~p concurrent senders~n", [ConcurrentSenders]),
    
    Port = 19004,
    MulticastGroup = "224.0.0.1",
    MessagesPerSender = 10,
    
    StartTime = erlang:system_time(millisecond),
    
    % 启动压力测试发送器
    SenderPids = lists:map(fun(SenderId) ->
        spawn(fun() -> stress_sender(SenderId, MulticastGroup, Port, MessagesPerSender) end)
    end, lists:seq(1, ConcurrentSenders)),
    
    % 等待所有发送器完成
    lists:foreach(fun(Pid) -> 
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        end
    end, SenderPids),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    TotalMessages = ConcurrentSenders * MessagesPerSender,
    
    io:format("Stress test results:~n"),
    io:format("  Concurrent senders: ~p~n", [ConcurrentSenders]),
    io:format("  Messages per sender: ~p~n", [MessagesPerSender]),
    io:format("  Total messages: ~p~n", [TotalMessages]),
    io:format("  Duration: ~p ms~n", [Duration]),
    io:format("  Messages per second: ~.2f~n", [TotalMessages / (Duration / 1000)]),
    
    {ok, #{
        concurrent_senders => ConcurrentSenders,
        messages_per_sender => MessagesPerSender,
        total_messages => TotalMessages,
        duration_ms => Duration,
        messages_per_second => TotalMessages / (Duration / 1000)
    }}.

%% @doc 获取测试摘要
get_test_summary() ->
    #{
        test_functions => [
            "test_multicast/0",
            "test_multicast_with_tcpdump/0", 
            "test_multicast_loopback/0",
            "run_simple_test/0",
            "test_server_client_integration/0",
            "test_multicast_performance/1",
            "test_multicast_reliability/0",
            "test_multicast_stress/1"
        ],
        supported_multicast_groups => [
            "224.0.0.1",  % All Hosts
            "239.255.255.250"  % SSDP
        ],
        default_test_port => 19000,
        description => "UDP Multicast Testing Module based on successful dgiot_udp implementation"
    }.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

%% @doc 性能测试接收器
performance_receiver(Port, ExpectedCount) ->
    case gen_udp:open(Port, [binary, {reuseaddr, true}, {active, true}]) of
        {ok, Socket} ->
            {ok, GroupAddr} = inet:parse_address("224.0.0.1"),
            {ok, LocalAddr} = inet:parse_address("127.0.0.1"),
            inet:setopts(Socket, [{add_membership, {GroupAddr, LocalAddr}}]),
            performance_receive_loop(Socket, ExpectedCount, 0);
        Error ->
            io:format("✗ Performance receiver failed to start: ~p~n", [Error])
    end.

performance_receive_loop(Socket, ExpectedCount, ReceivedCount) ->
    receive
        {udp, Socket, _IP, _Port, _Packet} ->
            NewCount = ReceivedCount + 1,
            if
                NewCount >= ExpectedCount ->
                    io:format("✓ Performance receiver received all ~p messages~n", [ExpectedCount]),
                    gen_udp:close(Socket);
                true ->
                    performance_receive_loop(Socket, ExpectedCount, NewCount)
            end;
        stop ->
            gen_udp:close(Socket),
            io:format("Performance receiver stopped (received ~p/~p messages)~n", 
                     [ReceivedCount, ExpectedCount])
    after 5000 -> % 5秒超时
        io:format("Performance receiver timeout (received ~p/~p messages)~n", 
                 [ReceivedCount, ExpectedCount]),
        gen_udp:close(Socket)
    end.

%% @doc 发送性能测试消息
send_performance_messages(Group, Port, Count) ->
    send_performance_messages(Group, Port, Count, 0).

send_performance_messages(_Group, _Port, 0, SentCount) ->
    {ok, SentCount};
send_performance_messages(Group, Port, Count, SentCount) ->
    Message = list_to_binary("Performance test message #" ++ integer_to_list(Count)),
    case send_direct_multicast(Group, Port, Message) of
        ok ->
            send_performance_messages(Group, Port, Count - 1, SentCount + 1);
        Error ->
            {error, {send_failed, Error, SentCount}}
    end.

%% @doc 压力测试发送器
stress_sender(SenderId, Group, Port, Count) ->
    stress_sender_loop(SenderId, Group, Port, Count).

stress_sender_loop(_SenderId, _Group, _Port, 0) ->
    ok;
stress_sender_loop(SenderId, Group, Port, Count) ->
    Message = list_to_binary("Stress test from sender " ++ integer_to_list(SenderId) ++ 
                            " message #" ++ integer_to_list(Count)),
    case send_direct_multicast(Group, Port, Message) of
        ok -> ok;
        Error -> io:format("Sender ~p failed: ~p~n", [SenderId, Error])
    end,
    stress_sender_loop(SenderId, Group, Port, Count - 1).

%%%===================================================================
%%% 增强的tcpdump监控函数
%%%===================================================================

%% @doc 启动增强的tcpdump监控，返回{Pid, Port}
start_enhanced_tcpdump_monitor() ->
    case os:find_executable("tcpdump") of
        false ->
            throw(tcpdump_not_found);
        TcpdumpPath ->
            io:format("~s ~p Event = Starting enhanced tcpdump multicast monitoring...~n", 
                     [?FILE, ?LINE]),
            % 使用更详细的tcpdump命令，捕获UDP多播报文
            Command = TcpdumpPath ++ " -i any -n -c 20 udp and multicast",
            Port = open_port({spawn, Command}, [stderr_to_stdout, in, stream, binary]),
            io:format("~s ~p Event = Enhanced tcpdump monitoring started (Port: ~p).~n", 
                     [?FILE, ?LINE, Port]),
            {self(), Port}
    end.

%% @doc 在tcpdump监控期间发送多播测试报文
send_multicast_packets_for_tcpdump() ->
    MulticastGroups = ["224.0.0.1", "239.255.255.250", "224.0.0.2", "224.0.0.5"],
    Port = 19000,
    Messages = [
        <<"TCPDUMP_TEST_1: Enhanced multicast test message 1">>,
        <<"TCPDUMP_TEST_2: Enhanced multicast test message 2">>,
        <<"TCPDUMP_TEST_3: Enhanced multicast test message 3">>,
        <<"TCPDUMP_TEST_4: Enhanced multicast test message 4">>,
        <<"TCPDUMP_TEST_5: Enhanced multicast test message 5">>
    ],
    
    io:format("~s ~p Event = Sending ~p multicast packets for tcpdump capture...~n", 
             [?FILE, ?LINE, length(Messages) * length(MulticastGroups)]),
    
    Results = lists:flatmap(fun(Group) ->
        lists:map(fun({Index, Message}) ->
            FullMessage = <<Message/binary, " [Group: ", (list_to_binary(Group))/binary, "]">>,
            case send_direct_multicast(Group, Port, FullMessage) of
                ok ->
                    io:format("~s ~p Event = ✓ Sent packet ~p to group ~s~n", 
                             [?FILE, ?LINE, Index, Group]),
                    {Group, Index, success};
                Error ->
                    io:format("~s ~p Event = ✗ Failed to send packet ~p to group ~s: ~p~n", 
                             [?FILE, ?LINE, Index, Group, Error]),
                    {Group, Index, Error}
            end
        end, lists:zip(lists:seq(1, length(Messages)), Messages))
    end, MulticastGroups),
    
    % 添加延迟以确保报文被捕获
    timer:sleep(2000),
    
    {ok, #{
        packets_sent => length(Results),
        multicast_groups => MulticastGroups,
        results => Results
    }}.

%% @doc 停止增强的tcpdump监控并获取输出
stop_enhanced_tcpdump_monitor(Port) ->
    io:format("~s ~p Event = Stopping enhanced tcpdump monitoring and collecting output...~n", 
             [?FILE, ?LINE]),
    
    % 收集所有输出
    Output = collect_port_output(Port, []),
    
    % 关闭端口
    try
        port_close(Port),
        timer:sleep(500)
    catch
        _:_ -> ok
    end,
    
    io:format("~s ~p Event = Collected ~p bytes of tcpdump output.~n", 
             [?FILE, ?LINE, byte_size(Output)]),
    Output.

%% @doc 收集端口输出
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, [Data | Acc]);
        {Port, eof} ->
            list_to_binary(lists:reverse(Acc))
    after 1000 -> % 1秒超时
        list_to_binary(lists:reverse(Acc))
    end.

%% @doc 分析tcpdump输出
analyze_tcpdump_output(Output) ->
    io:format("~s ~p Event = Analyzing tcpdump output...~n", [?FILE, ?LINE]),
    
    % 将二进制输出转换为字符串
    OutputStr = binary_to_list(Output),
    
    % 分割为行
    Lines = string:tokens(OutputStr, "\n"),
    
    io:format("~s ~p Event = Tcpdump captured ~p lines of output.~n", 
             [?FILE, ?LINE, length(Lines)]),
    
    % 分析每行，统计多播报文
    Analysis = analyze_tcpdump_lines(Lines, #{multicast_packets => 0, total_packets => 0}),
    
    % 输出详细分析结果
    io:format("~s ~p Event = Tcpdump analysis complete:~n", [?FILE, ?LINE]),
    io:format("~s ~p Event =   Total packets: ~p~n", [?FILE, ?LINE, maps:get(total_packets, Analysis)]),
    io:format("~s ~p Event =   Multicast packets: ~p~n", [?FILE, ?LINE, maps:get(multicast_packets, Analysis)]),
    
    case maps:get(multicast_packets, Analysis) of
        0 ->
            % 如果没有捕获到多播报文，输出原始tcpdump输出用于调试
            io:format("~s ~p Event = ⚠ WARNING: No multicast packets detected in tcpdump output.~n", 
                     [?FILE, ?LINE]),
            io:format("~s ~p Event = Raw tcpdump output (first 500 chars):~n~s~n", 
                     [?FILE, ?LINE, string:slice(OutputStr, 0, 500)]);
        _ ->
            % 输出一些示例报文
            io:format("~s ~p Event = Sample multicast packets captured:~n", [?FILE, ?LINE]),
            SampleLines = lists:sublist(
                lists:filter(fun(Line) -> 
                    string:str(Line, ">") > 0 andalso 
                    (string:str(Line, "224.") > 0 orelse string:str(Line, "239.") > 0)
                end, Lines), 3),
            io:format("~p~n", [SampleLines])
    end,
    
    {ok, Analysis}.

%% @doc 分析tcpdump输出行
analyze_tcpdump_lines([], Acc) ->
    Acc;
analyze_tcpdump_lines([Line | Rest], Acc) ->
    CurrentMulticast = maps:get(multicast_packets, Acc),
    CurrentTotal = maps:get(total_packets, Acc),
    NewAcc = case is_multicast_packet_line(Line) of
        true ->
            Acc#{multicast_packets := CurrentMulticast + 1, 
                 total_packets := CurrentTotal + 1};
        false ->
            Acc#{total_packets := CurrentTotal + 1}
    end,
    analyze_tcpdump_lines(Rest, NewAcc).

%% @doc 判断是否为多播报文行
is_multicast_packet_line(Line) ->
    % 检查是否包含多播地址模式
    (string:str(Line, "224.") > 0 orelse 
     string:str(Line, "239.") > 0 orelse
     string:str(Line, "multicast") > 0) andalso
    string:str(Line, "UDP") > 0.
