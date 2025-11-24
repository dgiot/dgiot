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

%% @doc UDP测试通用工具模块
%% 提供状态管理、进程控制和通用工具函数
-module(dgiot_udp_test_utils).
-author("johnliu").

%% API导出
-export([
    init_state/0,
    update_state/1,
    get_state/0,
    stop_all_processes/0,
    wait_for_connection/1,
    format_log/3,
    count_packets/1,
    check_process_alive/1,
    generate_test_messages/1,
    get_test_stats/0,
    count_test_results/1,
    test_multicast/0,
    test_multicast_with_tcpdump/0,
    get_multicast_groups/0,
    start_multicast_server/2,
    start_multicast_client/2
]).

%% 状态记录定义
-record(state, {
    server_pid = undefined,
    client_pids = [],
    server_port = 0,
    test_messages = []
}).

%% 全局状态存储键
-define(STATE_KEY, udp_test_state).

%%%===================================================================
%%% 状态管理函数
%%%===================================================================

%% @doc 初始化测试状态
%% @returns 初始状态
-spec init_state() -> #state{}.
init_state() ->
    #state{}.

%% @doc 更新测试状态
%% @param Fun 状态更新函数
%% @returns 新状态
-spec update_state(fun((#state{}) -> #state{})) -> #state{}.
update_state(Fun) ->
    CurrentState = get_state(),
    NewState = Fun(CurrentState),
    put(?STATE_KEY, NewState),
    NewState.

%% @doc 获取当前测试状态
%% @returns 当前状态
-spec get_state() -> #state{}.
get_state() ->
    case get(?STATE_KEY) of
        undefined -> init_state();
        State -> State
    end.

%%%===================================================================
%%% 进程控制函数
%%%===================================================================

%% @doc 停止所有测试进程
%% @returns ok
-spec stop_all_processes() -> ok.
stop_all_processes() ->
    State = get_state(),
    
    % 停止所有客户端进程
    lists:foreach(fun({Pid, ChannelId, ClientId}) ->
        case check_process_alive(Pid) of
            true -> 
                try
                    dgiot_client:stop(ChannelId, ClientId)
                catch
                    _:_ -> ok
                end,
                io:format("~s ~p Event = Stopped client ~p:~p.~n", [?FILE, ?LINE, ChannelId, ClientId]);
            false -> ok
        end
    end, State#state.test_messages),
    
    % 停止服务器进程
    case State#state.server_pid of
        undefined -> ok;
        ServerPid when is_pid(ServerPid) ->
            case check_process_alive(ServerPid) of
                true -> 
                    try
                        gen_server:stop(ServerPid)
                    catch
                        _:_ -> ok
                    end,
                    io:format("~s ~p Event = Stopped server ~p.~n", [?FILE, ?LINE, ServerPid]);
                false -> ok
            end;
        _ -> ok
    end,
    
    % 清除状态
    put(?STATE_KEY, init_state()),
    io:format("~s ~p Event = Stopped all test processes.~n", [?FILE, ?LINE]).

%% @doc 检查进程是否存活
%% @param Pid 进程ID
%% @returns 是否存活
-spec check_process_alive(pid()) -> boolean().
check_process_alive(Pid) ->
    try
        is_pid(Pid) andalso erlang:is_process_alive(Pid)
    catch
        _:_ -> false
    end.

%%%===================================================================
%%% 通用工具函数
%%%===================================================================

%% @doc 等待连接建立
%% @param Timeout 超时时间（毫秒）
%% @returns ok
-spec wait_for_connection(integer()) -> ok.
wait_for_connection(Timeout) ->
    io:format("~s ~p Event = Waiting for connection establishment (~p ms)...~n", 
             [?FILE, ?LINE, Timeout]),
    timer:sleep(Timeout),
    io:format("~s ~p Event = Connection wait completed.~n", [?FILE, ?LINE]).

%% @doc 格式化日志输出
%% @param Level 日志级别
%% @param Format 格式字符串
%% @param Args 参数列表
%% @returns ok
-spec format_log(atom(), string(), [any()]) -> ok.
format_log(Level, Format, Args) ->
    case Level of
        info -> 
            io:format("~s ~p Event = " ++ Format ++ "~n", [?FILE, ?LINE | Args]);
        warning -> 
            io:format("~s ~p WARNING = " ++ Format ++ "~n", [?FILE, ?LINE | Args]);
        error -> 
            io:format("~s ~p ERROR = " ++ Format ++ "~n", [?FILE, ?LINE | Args]);
        success -> 
            io:format("~s ~p SUCCESS = " ++ Format ++ "~n", [?FILE, ?LINE | Args]);
        _ -> 
            io:format("~s ~p [" ++ atom_to_list(Level) ++ "] = " ++ Format ++ "~n", 
                     [?FILE, ?LINE | Args])
    end.

%% @doc 统计抓包数量
%% @param PcapOutput tcpdump输出
%% @returns 包数量
-spec count_packets(string()) -> integer().
count_packets(PcapOutput) ->
    Lines = string:split(PcapOutput, "\n", all),
    Packets = [L || L <- Lines, string:str(L, "UDP") > 0],
    length(Packets).

%%%===================================================================
%%% 测试消息生成函数
%%%===================================================================

%% @doc 生成普通UDP测试消息列表
%% @param Count 消息数量
%% @returns 消息列表
-spec generate_test_messages(integer()) -> [binary()].
generate_test_messages(Count) ->
    lists:map(fun(I) ->
        list_to_binary("Test message " ++ integer_to_list(I) ++ " from UDP test")
    end, lists:seq(1, Count)).

%%%===================================================================
%%% 统计信息函数
%%%===================================================================

%% @doc 获取测试统计信息
%% @returns 统计信息映射
-spec get_test_stats() -> #{}.
get_test_stats() ->
    State = get_state(),
    #{
        server_pid => State#state.server_pid,
        server_port => State#state.server_port,
        client_count => length(State#state.client_pids),
        client_pids => State#state.client_pids,
        test_messages => State#state.test_messages
    }.

%% @doc 统计测试结果
%% @param Results 测试结果列表
%% @returns 统计结果
-spec count_test_results([{atom(), any()}]) -> #{passed => integer(), failed => integer(), total => integer()}.
count_test_results(Results) ->
    {Passed, Failed} = lists:partition(fun
        ({_, {ok, _}}) -> true;
        (_) -> false
    end, Results),
    
    #{
        total => length(Results),
        passed => length(Passed),
        failed => length(Failed)
    }.

%%%===================================================================
%%% 多播测试工具函数
%%%===================================================================

%% @doc 测试多播通信 - 基于dgiot_udp成功经验
test_multicast() ->
    io:format("=== UDP MULTICAST TEST START ===~n"),
    
    % 使用已验证可用的多播组（基于dgiot_udp成功经验）
    MulticastGroups = ["239.255.255.250", "224.0.0.251"],
    ServerPort = 19000,
    
    io:format("Testing multicast groups: ~p~n", [MulticastGroups]),
    
    % 启动接收器进程（基于成功经验）
    ReceiverPids = lists:map(fun(Group) ->
        Pid = spawn_link(fun() -> receive_multicast(Group, ServerPort) end),
        {Group, Pid}
    end, MulticastGroups),
    
    io:format("Started ~p multicast receivers~n", [length(ReceiverPids)]),
    
    % 等待接收器启动
    timer:sleep(2000),
    
    % 发送多播消息（基于成功经验）
    Results = lists:map(fun({Group, _Pid}) ->
        io:format("Testing group: ~s~n", [Group]),
        
        % 发送多个测试消息
        Messages = [
            <<"MULTICAST_TEST_1: Hello from ", (list_to_binary(Group))/binary>>,
            <<"MULTICAST_TEST_2: Second message to ", (list_to_binary(Group))/binary>>,
            <<"MULTICAST_TEST_3: Final test for ", (list_to_binary(Group))/binary>>
        ],
        
        SendResults = lists:map(fun(Message) ->
            io:format("  Sending: ~s~n", [Message]),
            case send_direct_multicast(Group, ServerPort, Message) of
                ok ->
                    io:format("    ✓ Sent successfully~n"),
                    success;
                Error ->
                    io:format("    ✗ Send failed: ~p~n", [Error]),
                    Error
            end
        end, Messages),
        
        % 等待消息传输
        timer:sleep(2000),
        
        {Group, SendResults}
    end, ReceiverPids),
    
    % 停止接收器
    lists:foreach(fun({_Group, Pid}) ->
        Pid ! stop
    end, ReceiverPids),
    
    % 汇总结果
    AllSuccesses = lists:flatten([Successes || {_, Successes} <- Results]),
    SuccessCount = length([R || R <- AllSuccesses, R =:= success]),
    FailureCount = length(AllSuccesses) - SuccessCount,
    
    io:format("=== UDP MULTICAST TEST RESULTS ===~n"),
    io:format("Total messages sent: ~p~n", [length(AllSuccesses)]),
    io:format("Successes: ~p, Failures: ~p~n", [SuccessCount, FailureCount]),
    io:format("=== UDP MULTICAST TEST END ===~n"),
    
    {ok, #{
        multicast_groups => MulticastGroups,
        total_messages => length(AllSuccesses),
        successes => SuccessCount,
        failures => FailureCount,
        results => Results
    }}.

%% @doc 使用tcpdump进行多播测试
test_multicast_with_tcpdump() ->
    io:format("~s ~p Event = Starting multicast test with tcpdump monitoring.~n", [?FILE, ?LINE]),
    
    % 检查tcpdump是否可用
    case os:type() of
        {unix, _} ->
            case os:find_executable("tcpdump") of
                false ->
                    io:format("~s ~p Event = ✗ tcpdump not found, using standard multicast test.~n", 
                             [?FILE, ?LINE]),
                    test_multicast();
                _ ->
                    % 启动tcpdump监控
                    TcpdumpPid = start_tcpdump_monitor(),
                    timer:sleep(1000),
                    
                    % 运行标准多播测试
                    TestResult = test_multicast(),
                    
                    % 停止tcpdump
                    stop_tcpdump_monitor(TcpdumpPid),
                    
                    % 汇总结果
                    case TestResult of
                        {ok, _} ->
                            io:format("~s ~p Event = ✓ Multicast test with tcpdump completed successfully.~n", 
                                     [?FILE, ?LINE]);
                        Error ->
                            io:format("~s ~p Event = ✗ Multicast test with tcpdump failed: ~p.~n", 
                                     [?FILE, ?LINE, Error])
                    end,
                    TestResult
            end;
        _ ->
            io:format("~s ~p Event = ✗ tcpdump not supported on this system, using standard multicast test.~n", 
                     [?FILE, ?LINE]),
            test_multicast()
    end.

%% @doc 获取多播组列表
get_multicast_groups() ->
    % 返回常见的多播组地址
    [
        "224.0.0.1",  % All Systems on this Subnet
        "224.0.0.2",  % All Routers on this Subnet
        "224.0.0.5",  % OSPF
        "224.0.0.9",  % RIP
        "224.0.0.18", % VRMP
        "224.0.0.22"  % IGMP
    ].

%% @doc 启动多播服务器
start_multicast_server(Port, MulticastGroups) ->
    dgiot_udp_server:start_multicast_server(Port, MulticastGroups).

%% @doc 启动多播客户端
start_multicast_client(Port, MulticastGroups) ->
    dgiot_udp_client:start_multicast_client(Port, MulticastGroups).

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

%% @doc 启动tcpdump监控进程
start_tcpdump_monitor() ->
    case os:find_executable("tcpdump") of
        false ->
            undefined;
        TcpdumpPath ->
            io:format("~s ~p Event = Starting tcpdump multicast monitoring...~n", 
                     [?FILE, ?LINE]),
            Command = TcpdumpPath ++ " -i any multicast -n -c 10",
            Port = open_port({spawn, Command}, [stderr_to_stdout, in, stream, binary]),
            io:format("~s ~p Event = tcpdump monitoring started (Port: ~p).~n", 
                     [?FILE, ?LINE, Port]),
            Port
    end.

%% @doc 停止tcpdump监控
stop_tcpdump_monitor(undefined) ->
    ok;
stop_tcpdump_monitor(Port) ->
    io:format("~s ~p Event = Stopping tcpdump monitoring...~n", 
             [?FILE, ?LINE]),
    try
        port_close(Port),
        timer:sleep(500),
        io:format("~s ~p Event = tcpdump monitoring stopped.~n", 
                 [?FILE, ?LINE])
    catch
        _:_ ->
            io:format("~s ~p Event = tcpdump monitoring already stopped.~n", 
                     [?FILE, ?LINE])
    end.

%% @doc 接收多播消息 - 基于dgiot_udp成功经验
receive_multicast(Group, Port) ->
    case gen_udp:open(Port, [
        binary, 
        {reuseaddr, true}, 
        {multicast_loop, true},
        {active, true},
        {ip, {0,0,0,0}}  % 绑定到所有接口
    ]) of
        {ok, Socket} ->
            % 加入多播组
            {ok, GroupAddr} = inet:parse_address(Group),
            {ok, LocalAddr} = inet:parse_address("127.0.0.1"),
            inet:setopts(Socket, [{add_membership, {GroupAddr, LocalAddr}}]),
            
            io:format("Receiver: Joined multicast group ~s on port ~p~n", [Group, Port]),
            receive_loop(Socket, Group);
        Error ->
            io:format("Failed to open socket for receiving: ~p~n", [Error]),
            Error
    end.

%% @doc 接收循环
receive_loop(Socket, Group) ->
    receive
        {udp, Socket, IP, InPort, Packet} ->
            io:format("Receiver: Got packet from ~p:~p: ~s~n", [IP, InPort, Packet]),
            receive_loop(Socket, Group);
        stop ->
            gen_udp:close(Socket),
            io:format("Receiver: Stopped~n");
        _Other ->
            receive_loop(Socket, Group)
    after 10000 ->  % 10秒超时
        io:format("Receiver: Timeout, stopping~n"),
        gen_udp:close(Socket)
    end.
