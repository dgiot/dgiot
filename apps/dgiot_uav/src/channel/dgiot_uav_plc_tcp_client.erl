%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_plc_tcp_client 模块 - PLC通信层（主模块）
%%% 负责 TCP 连接管理、消息路由、对外 API
%%% 步骤执行逻辑委托给 dgiot_uav_plc_step_executor
%%% 工具函数委托给 dgiot_uav_plc_utils
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_tcp_client).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

%% callback
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%% API
-export([
    start_continuous_test/2,
    stop_continuous_test/1,
    get_continuous_test_status/1,
    write/3,
    read/3,
    send_single_command/3,
    send_single_command/5,
    test/0,
    test_7step/0,  %% 七步校验演示函数
    report_plc_result/4,   %% 供步骤执行器调用
    %% High-level API (Auto relative address)
    read_alarms/1,
    read_heartbeat/1,
    read_station_status/1,
    read_operation_mode/1
]).

%% 注意: 不使用 -import，而是在运行时直接调用模块函数
%% 这样可以避免模块加载顺序问题

-export([handle_virtual_station_ready/4, handle_virtual_station_disconnected/2]).

%%%===================================================================
%%% 在线调试测试函数
%%%===================================================================

%% @doc 在线调试测试函数
test() ->
    dgiot_uav_plc_utils:test_client().

%% @doc 测试七步校验日志（演示函数）
test_7step() ->
    io:format("~n========================================~n", []),
    io:format("🎯 【PLC七步校验】演示日志输出~n", []),
    io:format("========================================~n~n", []),
    
    %% 模拟测试参数
    StationId = 1700,
    Code = 100,
    
    %% 模拟七步流程
    Steps = [
        {1, "读取工位就绪状态", read, {0, 1}},
        {2, "写入测试命令码", write, {51, Code}},
        {3, "读取测试确认状态", read, {10, 1}},
        {4, "复位工位状态", write, {0, 0}},
        {5, "清除测试确认", write, {10, 0}},
        {6, "写入完成确认码", write, {60, Code}},
        {7, "触发完成信号", write, {61, 1}}
    ],
    
    lists:foreach(fun({StepId, Desc, OpType, Args}) ->
        io:format("~n----------------------------------------~n", []),
        io:format("📌 Step ~p/7: ~s~n", [StepId, Desc]),
        io:format("----------------------------------------~n", []),
        
        case OpType of
            read -> 
                {Addr, Count} = Args,
                io:format("操作类型: READ~n"),
                io:format("相对地址: D+~p~n", [Addr]),
                io:format("寄存器数量: ~p~n", [Count]),
                io:format("绝对地址: D~p~n", [StationId + Addr]),
                io:format("✅ 读取成功，返回值: [1]~n");
            write ->
                {Addr, Value} = Args,
                io:format("操作类型: WRITE~n"),
                io:format("相对地址: D+~p~n", [Addr]),
                io:format("写入值: ~p~n", [Value]),
                io:format("绝对地址: D~p~n", [StationId + Addr]),
                io:format("✅ 写入成功~n")
        end,
        
        timer:sleep(500)
    end, Steps),
    
    io:format("~n========================================~n", []),
    io:format("✅ 【PLC七步校验】全部完成~n", []),
    io:format("========================================~n~n", []),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% 回调函数
%%%===================================================================

init(#dclient{channel = ChannelId, client = ClientId, child = Args} = State) ->
    ?LOG(info, "UAV PLC通信层初始化: ChannelId=~p, ClientId=~p", [ChannelId, ClientId]),

    StationId = maps:get(station_id, Args, 1),
    StationName = maps:get(station_name, Args, <<"未知工位"/utf8>>),
    Ip = maps:get(ip, Args, <<"192.168.100.40">>),
    Port = maps:get(port, Args, 502),
    DefaultCommands = maps:get(commands, Args, []),
    CommandInterval = maps:get(command_interval, Args, 1000),

    ?LOG(info, "PLC客户端初始化: StationId=~p, StationName=~ts, IP=~ts, Port=~p",
          [StationId, StationName, Ip, Port]),

    IsVirtualStation = dgiot_uav_plc_business:is_virtual_station(StationId),

    {InitialState, StationType} = case IsVirtualStation of
        true ->
            VirtualStationTypeText = case StationId of
                ?BASE_VIRTUAL_ALARM -> <<"告警检测"/utf8>>;
                ?BASE_VIRTUAL_HEARTBEAT -> <<"心跳检测"/utf8>>;
                _ -> <<"虚拟监控"/utf8>>
            end,
            {<<"monitoring">>, VirtualStationTypeText};
        false ->
            {<<"idle">>, <<"测试工位"/utf8>>}
    end,

    ChildState = #{
        station_id => StationId,
        station_name => StationName,
        station_type => StationType,
        is_virtual => IsVirtualStation,
        base_address => dgiot_uav_plc_utils:get_base_address(StationId),
        ip => Ip,
        port => Port,
        connection_status => <<"initializing">>,
        last_heartbeat => erlang:system_time(millisecond),
        heartbeat_timer => undefined,
        error_count => 0,
        response_queue => [],
        command_list => DefaultCommands,
        command_interval => CommandInterval,
        current_command_index => 1,
        current_step_index => 1,
        state_machine_state => InitialState,
        last_sent_time => 0,
        response_timeout_timer => undefined,
        step_retry_count => 0,
        command_retry_count => 0,
        command_timeout_timer => undefined,
        last_state_change => erlang:system_time(millisecond),
        state_history => [],
        execution_lock => false,
        fixture_address => maps:get(fixture_address, Args, undefined),
        reply_to => undefined,
        test_item_id => undefined,
        step_index => undefined
    },

    HeartbeatTimer = erlang:send_after(30000, self(), heartbeat),
    ReadStationTimer = erlang:send_after(60000, self(), read_station_info),
    UpdatedChildState = ChildState#{
        heartbeat_timer => HeartbeatTimer,
        read_station_timer => ReadStationTimer
    },

    ?LOG(info, "PLC通信层配置完成: StationId=~p, StationName=~ts, Ip=~ts, Port=~p", [
        StationId, StationName, Ip, Port
    ]),

    %% 启动TCP客户端
    TcpClientArgs = #{
        channel => ChannelId,
        client => ClientId,
        ip => Ip,
        port => Port,
        mod => ?MODULE
    },
    StartResult = dgiot_tcp_client:start_link(TcpClientArgs),
    case StartResult of
        ok -> ?LOG(info, "TCP客户端已存在且存活，连接到 ~ts:~p", [Ip, Port]);
        ignore -> ?LOG(warning, "TCP客户端启动被忽略，连接到 ~ts:~p", [Ip, Port]);
        {ok, _TcpPid} -> ?LOG(info, "TCP客户端启动成功，连接到 ~ts:~p", [Ip, Port]);
        {error, _Reason1} -> ?LOG(error, "TCP客户端启动失败: ~p", [_Reason1]);
        Other -> ?LOG(warning, "TCP客户端启动返回未知结果: ~p", [Other])
    end,

    case global:register_name({plc, StationId}, self()) of
        yes -> ?LOG(info, "全局名称注册成功: {plc, ~p}", [StationId]);
        no  -> ?LOG(warning, "全局名称 {plc, ~p} 已被占用", [StationId])
    end,

    dgiot_uav_business_service:register_station_plc(StationId, self()),

    {ok, State#dclient{child = UpdatedChildState}}.

%% 异步单指令下发
handle_cast({send_single_command, Addr, Value, TestItemId, StepIndex}, #dclient{child = ChildState} = State) ->
    StationId = maps:get(station_id, ChildState),
    ?LOG(info, "【PLC_ASYNC】收到异步单指令下发: StationId=~p, Addr=~p, Value=~p, TestItemId=~p, StepIndex=~p",
         [StationId, Addr, Value, TestItemId, StepIndex]),

    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "【PLC_ASYNC】虚拟工位 ~p 拒绝执行测试指令，只做告警/心跳监控", [StationId]),
            {noreply, State};
        false ->
            case dgiot_uav_plc_step_executor:acquire_execution_lock(ChildState) of
                {ok, LockedState} ->
                    CmdList = [{<<"single">>, Value}],
                    NewState = LockedState#{
                        command_list => CmdList,
                        current_command_index => 1,
                        current_step_index => 1,
                        state_machine_state => <<"executing">>,
                        reply_to => undefined,
                        test_item_id => TestItemId,
                        step_index => StepIndex
                    },
                    self() ! {step, 1, 0},
                    {noreply, State#dclient{child = NewState}};
                {error, busy} ->
                    ?LOG(warning, "【PLC_ASYNC】工位 ~p 忙碌，无法执行指令", [StationId]),
                    {noreply, State}
            end
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% 同步调用处理
handle_call({start_continuous_test, Values}, _From, #dclient{child = ChildState} = State) ->
    StationId = maps:get(station_id, ChildState),
    ?LOG(info, "PLC系统: 收到连续测试启动请求: StationId=~p, Values=~p", [StationId, Values]),

    NormalizedValues = dgiot_uav_plc_utils:normalize_command_list(Values),
    ?LOG(debug, "PLC系统: 规范化后的命令列表: ~p", [NormalizedValues]),

    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "PLC系统: 虚拟工位 ~p 拒绝执行连续测试指令，只做告警/心跳监控", [StationId]),
            {reply, {error, virtual_station_cannot_execute}, State};
        false ->
            case dgiot_uav_plc_step_executor:acquire_execution_lock(ChildState) of
                {ok, LockedState} ->
                    ?LOG(info, "PLC系统: 获取执行锁成功，启动连续指令测试: StationId=~p", [StationId]),
                    NewChildState = LockedState#{
                        command_list => NormalizedValues,
                        current_command_index => 1,
                        current_step_index => 1,
                        state_machine_state => <<"executing">>,
                        command_retry_count => 0,
                        step_retry_count => 0,
                        reply_to => undefined,
                        test_item_id => undefined,
                        step_index => undefined
                    },
                    self() ! {step, 1, 0},
                    {reply, ok, State#dclient{child = NewChildState}};
                {error, busy} ->
                    ?LOG(warning, "PLC系统: 工位~p正在执行中，拒绝新的连续指令测试请求", [StationId]),
                    {reply, {error, busy}, State}
            end
    end;

handle_call(stop_continuous_test, _From, #dclient{child = ChildState} = State) ->
    StationId = maps:get(station_id, ChildState),
    ?LOG(info, "PLC系统: 停止连续指令测试: StationId=~p", [StationId]),
    NewChildState = dgiot_uav_plc_step_executor:release_execution_lock(ChildState#{
        state_machine_state => <<"idle">>,
        current_step_index => 1,
        current_command_index => 1
    }),
    {reply, ok, State#dclient{child = NewChildState}};

handle_call(get_continuous_test_status, _From, #dclient{child = ChildState} = State) ->
    Status = #{
        station_id => maps:get(station_id, ChildState),
        state_machine_state => maps:get(state_machine_state, ChildState, <<"idle">>),
        current_command_index => maps:get(current_command_index, ChildState, 1),
        current_step_index => maps:get(current_step_index, ChildState, 1),
        command_list => maps:get(command_list, ChildState, []),
        command_retry_count => maps:get(command_retry_count, ChildState, 0),
        step_retry_count => maps:get(step_retry_count, ChildState, 0)
    },
    {reply, {ok, Status}, State};

handle_call(get_connection_status, _From, #dclient{child = ChildState} = State) ->
    Status = maps:get(connection_status, ChildState, <<"unknown">>),
    {reply, {ok, Status}, State};

handle_call(get_responses, _From, #dclient{child = ChildState} = State) ->
    Responses = maps:get(response_queue, ChildState, []),
    {reply, {ok, Responses}, State};

handle_call({send_data, Data}, _From, #dclient{child = ChildState, channel = ChannelId, client = ClientId} = State) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    
    ?LOG(info, "~n========================================", []),
    ?LOG(info, "📤 【PLC TCP发送】发送数据到PLC", []),
    ?LOG(info, "----------------------------------------", []),
    ?LOG(info, "StationId: ~p", [StationId]),
    ?LOG(info, "StationName: ~ts", [StationName]),
    ?LOG(info, "报文长度: ~p 字节", [byte_size(Data)]),
    ?LOG(info, "报文内容(hex): ~s", [lists:flatten([io_lib:format("~2.16.0B ", [B]) || B <- binary:bin_to_list(Data)])]),
    ?LOG(info, "报文内容(binary): ~p", [Data]),
    ?LOG(info, "========================================~n", []),
    
    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "虚拟工位 ~p 拒绝发送数据请求 (只做告警/心跳监控)", [StationId]),
            {reply, {error, virtual_station_cannot_send_data}, State};
        false ->
            ?LOG(info, "收到发送数据请求到工位~p: ~p", [StationId, dgiot_utils:binary_to_hex(Data)]),
            try
                dgiot_tcp_client:send(ChannelId, ClientId, Data),
                ?LOG(info, "✅ 数据发送成功", []),
                {reply, ok, State}
            catch
                _:Error ->
                    ?LOG(error, "❌ 发送数据到工位~p失败: ~p", [StationId, Error]),
                    {reply, {error, Error}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% handle_info 主入口（已拆分为独立函数）
handle_info(connection_ready, #dclient{child = ChildState, channel = ChannelId, client = ClientId} = Dclient) ->
    handle_connection_ready(ChildState, ChannelId, ClientId, Dclient);

handle_info({step, StepId, _Code}, #dclient{child = ChildState} = Dclient) ->
    handle_step(StepId, ChildState, Dclient);

handle_info({tcp, Buff}, #dclient{child = ChildState} = Dclient) ->
    handle_tcp_data(Buff, ChildState, Dclient);

handle_info(disconnected, #dclient{child = ChildState} = Dclient) ->
    handle_disconnected(ChildState, Dclient);

handle_info(heartbeat, #dclient{child = ChildState, channel = ChannelId, client = ClientId} = Dclient) ->
    handle_heartbeat(ChildState, ChannelId, ClientId, Dclient);

handle_info({tcp_response, _Data}, Dclient) ->
    {noreply, Dclient};

handle_info(read_station_info, #dclient{channel = ChannelId, client = ClientId, child = ChildState} = Dclient) ->
    handle_read_station_info(ChildState, ChannelId, ClientId, Dclient);

handle_info({tcp_closed, Socket}, #dclient{channel = ChannelId, client = ClientId, child = ChildState} = Dclient) ->
    handle_tcp_closed(Socket, ChildState, ChannelId, ClientId, Dclient);

handle_info({tcp_error, Socket, Reason}, #dclient{channel = ChannelId, client = ClientId, child = ChildState} = Dclient) ->
    handle_tcp_error(Socket, Reason, ChildState, ChannelId, ClientId, Dclient);

handle_info({simple_plc_command, Addr, Value, _TestItemId, _StepIndex, FromPid}, #dclient{child = ChildState, channel = ChannelId, client = ClientId} = Dclient) ->
    handle_simple_plc_command(Addr, Value, FromPid, ChildState, ChannelId, ClientId, Dclient);

handle_info(_Info, Dclient) ->
    case _Info of
        next_time -> {noreply, Dclient};
        _Other ->
            ?LOG(error, "PLC系统: 收到未知消息: ~p", [_Other]),
            {noreply, Dclient}
    end.

terminate(Reason, #dclient{channel = ChannelId, client = ClientId, child = #{station_id := StationId}}) ->
    case Reason of
        normal -> ?LOG(info, "PLC系统: UAV PLC通信层正常终止: ChannelId=~p, ClientId=~p", [ChannelId, ClientId]);
        shutdown -> ?LOG(info, "PLC系统: UAV PLC通信层关闭终止: ChannelId=~p, ClientId=~p", [ChannelId, ClientId]);
        {shutdown, _} -> ?LOG(info, "PLC系统: UAV PLC通信层有序关闭: ChannelId=~p, ClientId=~p", [ChannelId, ClientId]);
        _ -> ?LOG(error, "PLC系统: UAV PLC通信层异常终止: Reason=~p, ChannelId=~p, ClientId=~p", [Reason, ChannelId, ClientId])
    end,
    global:unregister_name({plc, StationId}),
    dgiot_uav_business_service:unregister_station_plc(StationId),
    dgiot_client:stop(ChannelId, ClientId),
    ok.

code_change(_OldVsn, Dclient, _Extra) ->
    {ok, Dclient}.

%%%===================================================================
%%% API 函数
%%%===================================================================

write(StationId, InstructionAddress, Code) ->
    BaseAddress = dgiot_uav_plc_utils:get_base_address(StationId),
    AbsoluteAddress = BaseAddress + InstructionAddress,
    ModbusFrame = dgiot_uav_plc_parser:build_modbus_write_request(1, AbsoluteAddress, Code, 16#06),
    
    ?LOG(debug, "[PLC] Write D~p = ~p | Hex: ~s", [AbsoluteAddress, Code, dgiot_utils:binary_to_hex(ModbusFrame)]),
    
    ChannelId = dgiot_uav_plc_utils:get_channel_id(),
    ClientId = dgiot_uav_plc_utils:get_client_id(StationId),
    Result = dgiot_tcp_client:send(ChannelId, ClientId, ModbusFrame),
    Result.

read(StationId, InstructionAddress, RegisterCount) ->
    BaseAddress = dgiot_uav_plc_utils:get_base_address(StationId),
    AbsoluteAddress = BaseAddress + InstructionAddress,
    ModbusFrame = dgiot_uav_plc_parser:build_modbus_read_request(1, AbsoluteAddress, RegisterCount),
    
    ?LOG(debug, "[PLC] Read D~p (~p) | Hex: ~s", [AbsoluteAddress, RegisterCount, dgiot_utils:binary_to_hex(ModbusFrame)]),
    
    ChannelId = dgiot_uav_plc_utils:get_channel_id(),
    ClientId = dgiot_uav_plc_utils:get_client_id(StationId),
    Result = dgiot_tcp_client:send(ChannelId, ClientId, ModbusFrame),
    Result.

start_continuous_test(StationId, Values) ->
    ChannelId = dgiot_uav_plc_utils:get_channel_id(),
    ClientId = dgiot_uav_plc_utils:get_client_id(StationId),
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} -> gen_server:call(Pid, {start_continuous_test, Values});
        _ -> {error, client_not_find}
    end.

stop_continuous_test(StationId) ->
    ChannelId = dgiot_uav_plc_utils:get_channel_id(),
    ClientId = dgiot_uav_plc_utils:get_client_id(StationId),
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} -> gen_server:call(Pid, stop_continuous_test);
        _ -> {error, client_not_find}
    end.

get_continuous_test_status(StationId) ->
    ChannelId = dgiot_uav_plc_utils:get_channel_id(),
    ClientId = dgiot_uav_plc_utils:get_client_id(StationId),
    case dgiot_client:get(ChannelId, ClientId) of
        {ok, Pid} -> gen_server:call(Pid, get_continuous_test_status);
        _ -> {error, client_not_find}
    end.

send_single_command(Pid, Addr, Value) ->
    send_single_command(Pid, Addr, Value, undefined, undefined).

send_single_command(Pid, Addr, Value, TestItemId, StepIndex) ->
    gen_server:cast(Pid, {send_single_command, Addr, Value, TestItemId, StepIndex}).

report_plc_result(TestItemId, StepIndex, Code, _ChildState) ->
    ?LOG(info, "PLC指令完成: TestItemId=~s, StepIndex=~p, Code=~p", [TestItemId, StepIndex, Code]),
    ok.

%%%===================================================================
%%% 拆分后的 handle_info 辅助函数
%%%===================================================================

handle_connection_ready(ChildState, ChannelId, ClientId, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    IpBin = maps:get(ip, ChildState),
    Port = maps:get(port, ChildState, 502),
    UpdatedChildState = ChildState#{connection_status => <<"connected">>},
    ?LOG(info, "UAV PLC连接就绪: StationId=~p, StationName=~ts, IP=~ts, Port=~p, ChannelId=~p, ClientId=~p",
          [StationId, StationName, IpBin, Port, ChannelId, ClientId]),
    dgiot_uav_plc_business:handle_virtual_station_ready(StationId, StationName, IpBin, UpdatedChildState),
    dgiot_uav_plc_business:notify_worker_connected(StationId),
    {noreply, Dclient#dclient{child = UpdatedChildState}}.

handle_step(StepId, ChildState, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "!!! 虚拟工位 ~p 收到步骤指令 StepId=~p (不应发生！) !!!", [StationId, StepId]),
            {noreply, Dclient};
        false ->
            CmdId = maps:get(current_command_index, ChildState, 1),
            List = maps:get(command_list, ChildState, []),
            Interval = maps:get(command_interval, ChildState, 1000),
            CurrentCode = case CmdId =< length(List) of
                true -> 
                    case lists:nth(CmdId, List) of
                        {_DeviceId, Code} -> Code;  %% 元组格式 [{DeviceId, Code}]
                        Code when is_integer(Code) -> Code  %% 整数格式 [Code]
                    end;
                false -> 0
            end,
            
            %% 七步校验流程开始标志
            case StepId of
                1 ->
                    ?LOG(info, "~n~n========================================", []),
                    ?LOG(info, "🎯 【PLC七步校验】开始执行", []),
                    ?LOG(info, "========================================", []),
                    ?LOG(info, "Station ID: ~p", [StationId]),
                    ?LOG(info, "Command Index: ~p", [CmdId]),
                    ?LOG(info, "Command Code: ~p", [CurrentCode]),
                    ?LOG(info, "Interval: ~p ms", [Interval]),
                    ?LOG(info, "========================================~n", []);
                7 ->
                    ?LOG(info, "~n========================================", []),
                    ?LOG(info, "🎯 【PLC七步校验】最后一步 (Step 7/7)", []),
                    ?LOG(info, "========================================~n", []);
                _ ->
                    ok
            end,
            
            %% 步骤执行日志
            ?LOG(info, "~n----------------------------------------", []),
            ?LOG(info, "📌 Step ~p/7: ~s", [StepId, get_step_description(StepId, CurrentCode)]),
            ?LOG(info, "----------------------------------------", []),
            
            timer:sleep(Interval),
            
            %% 执行具体步骤
            case StepId of
                1 -> read(StationId, 0, 1);
                2 -> write(StationId, 51, CurrentCode);
                3 -> read(StationId, 10, 1);
                4 -> write(StationId, 0, 0);
                5 -> write(StationId, 10, 0);
                6 -> write(StationId, 60, CurrentCode);
                7 -> 
                    write(StationId, 61, 1),
                    ?LOG(info, "~n========================================", []),
                    ?LOG(info, "✅ 【PLC七步校验】全部完成", []),
                    ?LOG(info, "========================================~n", []);
                _ -> ?LOG(error, "非法步骤ID: ~p", [StepId])
            end,
            {noreply, Dclient}
    end.

handle_tcp_data(Buff, ChildState, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    CmdId = maps:get(current_command_index, ChildState, 1),
    StepId = maps:get(current_step_index, ChildState, 1),
    List = maps:get(command_list, ChildState, []),
    Code = case CmdId =< length(List) of 
        true -> 
            case lists:nth(CmdId, List) of
                {_DeviceId, CodeValue} -> CodeValue;  %% 元组格式 [{DeviceId, Code}]
                CodeValue when is_integer(CodeValue) -> CodeValue  %% 整数格式 [Code]
            end;
        false -> 0 
    end,
    
    %% 七步校验响应日志
    ?LOG(info, "~n~n========================================", []),
    ?LOG(info, "📥 【PLC TCP响应】收到Modbus响应报文", []),
    ?LOG(info, "========================================", []),
    ?LOG(info, "Station ID: ~p", [StationId]),
    ?LOG(info, "Step ID: ~p/7", [StepId]),
    ?LOG(info, "Command Index: ~p", [CmdId]),
    ?LOG(info, "Command Code: ~p", [Code]),
    ?LOG(info, "----------------------------------------", []),
    ?LOG(info, "响应报文 (~p bytes):", [byte_size(Buff)]),
    ?LOG(info, "  Hex: ~s", [dgiot_utils:binary_to_hex(Buff)]),
    ?LOG(info, "  Binary: ~p", [Buff]),
    print_modbus_response_log(Buff),
    ?LOG(info, "========================================~n", []),
    
    case dgiot_uav_plc_parser:parse_modbus_response(Buff) of
        {ok, #{function_code := _FCode, data := Data}} ->
            ?LOG(info, "✅ Modbus响应解析成功: ~p", [Data]),
            case is_sync_read_response(StepId) of
                true -> send_response_to_waiting_process(Data);
                false -> ok
            end,
            case StepId of
                1 -> dgiot_uav_plc_step_executor:handle_step1_response(Data, StationId, Code, ChildState, Dclient);
                3 -> dgiot_uav_plc_step_executor:handle_step3_response(Data, StationId, Code, ChildState, Dclient);
                _ -> dgiot_uav_plc_step_executor:handle_write_response(Data, StationId, CmdId, StepId, Code, undefined, ChildState, Dclient)
            end;
        {error, Reason} ->
            ?LOG(error, "❌ Modbus响应解析失败: ~p", [Reason]),
            {noreply, Dclient}
    end.

handle_disconnected(ChildState, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    UpdatedChildState = maps:put(connection_status, <<"disconnected">>, ChildState),
    ?LOG(error, "PLC系统: UAV PLC连接断开: StationId=~p, StationName=~ts", [StationId, StationName]),
    dgiot_uav_plc_business:handle_virtual_station_disconnected(StationId, StationName),
    dgiot_uav_plc_business:notify_worker_disconnected(StationId),
    {noreply, Dclient#dclient{child = UpdatedChildState}}.

handle_heartbeat(ChildState, ChannelId, ClientId, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    ConnectionStatus = maps:get(connection_status, ChildState, <<"unknown">>),
    StateMachineState = maps:get(state_machine_state, ChildState, <<"idle">>),
    ?LOG(info, "💓 【PLC心跳】StationId=~p, Name=~ts, Status=~p, State=~p",
         [StationId, StationName, ConnectionStatus, StateMachineState]),
    case {ConnectionStatus, StateMachineState} of
        {<<"connected">>, <<"idle">>} ->
            HeartbeatReadFrame = dgiot_uav_plc_parser:build_modbus_read_request(1, dgiot_uav_plc_utils:get_base_address(StationId), 1),
            case dgiot_tcp_client:send(ChannelId, ClientId, HeartbeatReadFrame) of
                ok -> ?LOG(debug, "心跳读取请求发送成功");
                {send, _} -> ?LOG(debug, "心跳读取请求发送成功（已排队）");
                {error, Reason} -> ?LOG(warning, "心跳读取请求发送失败: ~p", [Reason])
            end;
        _ -> ok
    end,
    OldTimer = maps:get(heartbeat_timer, ChildState, undefined),
    if OldTimer =/= undefined -> erlang:cancel_timer(OldTimer); true -> ok end,
    NewTimer = erlang:send_after(30000, self(), heartbeat),
    NewChildState = ChildState#{
        heartbeat_timer => NewTimer,
        last_heartbeat => erlang:system_time(millisecond)
    },
    {noreply, Dclient#dclient{child = NewChildState}}.

handle_read_station_info(ChildState, ChannelId, ClientId, Dclient) ->
    ?LOG(debug, "【PLC周期性读取】开始周期性读取治具工位信息", []),
    ReadCmd = dgiot_uav_plc_parser:build_modbus_read_request(1, 0, 1),
    case dgiot_tcp_client:send(ChannelId, ClientId, ReadCmd) of
        ok -> ?LOG(debug, "【PLC周期性读取】发送读取工位信息命令成功");
        {send, _} -> ?LOG(debug, "【PLC周期性读取】发送读取工位信息命令成功（已排队）");
        {error, Reason} -> ?LOG(error, "【PLC周期性读取】发送读取工位信息命令失败: ~p", [Reason]);
        Other -> ?LOG(warning, "【PLC周期性读取】未知返回值: ~p", [Other])
    end,
    OldTimer = maps:get(read_station_timer, ChildState, undefined),
    if OldTimer =/= undefined -> erlang:cancel_timer(OldTimer); true -> ok end,
    NewTimer = erlang:send_after(60000, self(), read_station_info),
    NewChildState = ChildState#{read_station_timer => NewTimer},
    {noreply, Dclient#dclient{child = NewChildState}}.

handle_tcp_closed(Socket, ChildState, _ChannelId, _ClientId, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    %% 注意: dgiot_tcp_client没有get_socket/2函数，直接处理tcp_closed事件
    ?LOG(warning, "PLC系统: TCP连接断开: StationId=~p, StationName=~ts, Socket=~p", [StationId, StationName, Socket]),
    UpdatedChildState = maps:put(connection_status, <<"disconnected">>, ChildState),
    {noreply, Dclient#dclient{child = UpdatedChildState}}.

handle_tcp_error(Socket, Reason, ChildState, _ChannelId, _ClientId, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    StationName = maps:get(station_name, ChildState, <<"未知工位"/utf8>>),
    %% 注意: dgiot_tcp_client没有get_socket/2函数，直接处理tcp_error事件
    ?LOG(error, "PLC系统: TCP连接错误: StationId=~p, StationName=~ts, Socket=~p, Reason=~p", [StationId, StationName, Socket, Reason]),
    UpdatedChildState = maps:put(connection_status, <<"error">>, ChildState),
    ErrorCount = maps:get(error_count, ChildState, 0) + 1,
    UpdatedChildState2 = UpdatedChildState#{error_count => ErrorCount},
    {noreply, Dclient#dclient{child = UpdatedChildState2}}.

handle_simple_plc_command(Addr, Value, FromPid, ChildState, ChannelId, ClientId, Dclient) ->
    StationId = maps:get(station_id, ChildState),
    ?LOG(info, "[SIMPLE_PLC] 收到简化指令消息: 工位=~p, 地址=~p, 值=~p, 发送者=~p", [StationId, Addr, Value, FromPid]),
    %% 使用dgiot_uav_plc_parser构建Modbus写入帧（RTU格式由parser内部处理）
    WriteCmd = dgiot_uav_plc_parser:build_modbus_write_request(1, Addr, Value, 16#06),
    case dgiot_tcp_client:send(ChannelId, ClientId, WriteCmd) of
        ok ->
            ?LOG(info, "[SIMPLE_PLC] 简化指令发送成功"),
            FromPid ! {simple_plc_response, ok},
            {noreply, Dclient};
        {send, _} ->
            ?LOG(info, "[SIMPLE_PLC] 简化指令发送成功（已排队）"),
            FromPid ! {simple_plc_response, ok},
            {noreply, Dclient};
        {error, Reason} ->
            ?LOG(error, "[SIMPLE_PLC] 简化指令发送失败: ~p", [Reason]),
            FromPid ! {simple_plc_response, {error, Reason}},
            {noreply, Dclient}
    end.

%%%===================================================================
%%% 同步读取辅助函数
%%%===================================================================

-spec is_sync_read_response(StepId :: integer()) -> boolean().
is_sync_read_response(StepId) ->
    StepId < 1 orelse StepId > 7.

-spec send_response_to_waiting_process(Data :: map()) -> ok.
send_response_to_waiting_process(Data) ->
    case Data of
        #{registers := Registers} when is_list(Registers) ->
            self() ! {tcp_response, Registers},
            ok;
        _ ->
            ok
    end.

%%%===================================================================
%%% 虚拟工位处理函数
%%%===================================================================

-spec handle_virtual_station_ready(StationId :: integer(), StationName :: binary(),
                                   Ip :: binary(), ChildState :: map()) -> ok.
handle_virtual_station_ready(StationId, StationName, Ip, ChildState) ->
    dgiot_uav_plc_business:handle_virtual_station_ready(StationId, StationName, Ip, ChildState).

-spec handle_virtual_station_disconnected(StationId :: integer(), StationName :: binary()) -> ok.
handle_virtual_station_disconnected(StationId, StationName) ->
    dgiot_uav_plc_business:handle_virtual_station_disconnected(StationId, StationName).

% %%%===================================================================
% %%% Modbus帧打印辅助函数
% %%%===================================================================
% 
% %% @doc 打印Modbus帧的字节分解（控制台版本）
% print_modbus_frame_console(Frame) when is_binary(Frame) ->
%     _ = Frame,
%     Bytes = binary:bin_to_list(Frame),
%     case Bytes of
%         [T1, T2, P1, P2, L1, L2, Slave, Func | Data] ->
%             io:format("  Transaction ID: ~4.16.0B ~2.16.0B~n", [T1, T2]),
%             io:format("  Protocol ID:    ~4.16.0B ~2.16.0B~n", [P1, P2]),
%             io:format("  Length:         ~4.16.0B ~2.16.0B (~p bytes)~n", [L1, L2, L1*256+L2]),
%             io:format("  Slave ID:       ~2.16.0B (~p)~n", [Slave, Slave]),
%             io:format("  Function Code:  ~2.16.0B (~s)~n", [Func, get_function_name(Func)]),
%             print_data_bytes_console(Data, Func);
%         _ ->
%             io:format("  Raw Data: ~p~n", [Bytes])
%     end;
% print_modbus_frame_console(_) ->
%     ok.
% 
% %% @doc 打印数据字节（控制台版本）
% print_data_bytes_console([Addr1, Addr2, Value1, Value2], 16#06) ->
%     _ = [Addr1, Addr2, Value1, Value2],
%     Addr = Addr1 * 256 + Addr2,
%     Value = Value1 * 256 + Value2,
%     io:format("  Register Addr:  ~4.16.0B ~2.16.0B (~p)~n", [Addr1, Addr2, Addr]),
%     io:format("  Value:          ~4.16.0B ~2.16.0B (~p)~n", [Value1, Value2, Value]);
% print_data_bytes_console([Addr1, Addr2, Count1, Count2], 16#03) ->
%     Addr = Addr1 * 256 + Addr2,
%     Count = Count1 * 256 + Count2,
%     io:format("  Register Addr:  ~4.16.0B ~2.16.0B (~p)~n", [Addr1, Addr2, Addr]),
%     io:format("  Register Count: ~4.16.0B ~2.16.0B (~p)~n", [Count1, Count2, Count]);
% print_data_bytes_console(Data, _Func) ->
%     io:format("  Data: ~p~n", [Data]).
% 
% %%%===================================================================
%%% High-Level API Functions (Auto Relative Address)
%%%===================================================================

%% @doc 读取告警状态（自动使用相对地址30）
%% @spec read_alarms(StationId :: integer()) -> {send, binary()} | {error, Reason}
%% @param StationId 工位ID (如1700、5000)
%% @returns 发送结果
%% @example
%%   dgiot_uav_plc_tcp_client:read_alarms(1700).  % 自动读取D1730告警区域
%%   dgiot_uav_plc_tcp_client:read_alarms(5000).  % 自动读取D5030告警区域
read_alarms(StationId) ->
    RelativeAddr = 30,  % 告警区域相对地址固定为30
    WordCount = 60,     % 读取60个寄存器
    io:format("~n~ts [PLC High-Level API] Read Alarms~n", [<<"🔍"/utf8>>]),
    io:format("Station ~p: Auto reading alarms at relative addr ~p~n", [StationId, RelativeAddr]),
    read(StationId, RelativeAddr, WordCount).

%% @doc 读取心跳状态（自动使用相对地址49）
%% @spec read_heartbeat(StationId :: integer()) -> {send, binary()} | {error, Reason}
%% @param StationId 工位ID
%% @returns 发送结果
%% @example
%%   dgiot_uav_plc_tcp_client:read_heartbeat(1700).  % 自动读取D1749心跳寄存器
read_heartbeat(StationId) ->
    RelativeAddr = 49,  % 心跳寄存器相对地址固定为49
    WordCount = 1,      % 读取1个寄存器
    io:format("~n~ts [PLC High-Level API] Read Heartbeat~n", [<<"💓"/utf8>>]),
    io:format("Station ~p: Auto reading heartbeat at relative addr ~p~n", [StationId, RelativeAddr]),
    read(StationId, RelativeAddr, WordCount).

%% @doc 读取工位状态（自动使用相对地址48）
%% @spec read_station_status(StationId :: integer()) -> {send, binary()} | {error, Reason}
%% @param StationId 工位ID
%% @returns 发送结果
%% @example
%%   dgiot_uav_plc_tcp_client:read_station_status(1700).  % 自动读取D1748工位状态
read_station_status(StationId) ->
    RelativeAddr = 48,  % 工位状态寄存器相对地址固定为48
    WordCount = 1,      % 读取1个寄存器
    io:format("~n~ts [PLC High-Level API] Read Station Status~n", [<<"📊"/utf8>>]),
    io:format("Station ~p: Auto reading status at relative addr ~p~n", [StationId, RelativeAddr]),
    read(StationId, RelativeAddr, WordCount).

%% @doc 读取运行模式（自动使用相对地址50）
%% @spec read_operation_mode(StationId :: integer()) -> {send, binary()} | {error, Reason}
%% @param StationId 工位ID
%% @returns 发送结果
%% @example
%%   dgiot_uav_plc_tcp_client:read_operation_mode(1700).  % 自动读取D1750运行模式
read_operation_mode(StationId) ->
    RelativeAddr = 50,  % 运行模式寄存器相对地址固定为50
    WordCount = 1,      % 读取1个寄存器
    io:format("~n~ts [PLC High-Level API] Read Operation Mode~n", [<<"⚙️"/utf8>>]),
    io:format("Station ~p: Auto reading mode at relative addr ~p~n", [StationId, RelativeAddr]),
    read(StationId, RelativeAddr, WordCount).

%% @doc 获取功能码名称
get_function_name(16#01) -> "Read Coils";
get_function_name(16#02) -> "Read Discrete Inputs";
get_function_name(16#03) -> "Read Holding Registers";
get_function_name(16#04) -> "Read Input Registers";
get_function_name(16#05) -> "Write Single Coil";
get_function_name(16#06) -> "Write Single Register";
get_function_name(16#0F) -> "Write Multiple Coils";
get_function_name(16#10) -> "Write Multiple Registers";
get_function_name(Code) -> io_lib:format("Unknown (~2.16.0B)", [Code]).

%% @doc 获取步骤描述（用于七步校验日志）
get_step_description(1, _Code) ->
    <<"读取工位就绪状态 (Read D+0, 1 register)"/utf8>>;
get_step_description(2, Code) ->
    io_lib:format(<<"写入测试命令码 ~p (Write D+51)"/utf8>>, [Code]);
get_step_description(3, _Code) ->
    <<"读取测试确认状态 (Read D+10, 1 register)"/utf8>>;
get_step_description(4, _Code) ->
    <<"复位工位状态 (Write D+0 = 0)"/utf8>>;
get_step_description(5, _Code) ->
    <<"清除测试确认 (Write D+10 = 0)"/utf8>>;
get_step_description(6, Code) ->
    io_lib:format(<<"写入完成确认码 ~p (Write D+60)"/utf8>>, [Code]);
get_step_description(7, _Code) ->
    <<"触发完成信号 (Write D+61 = 1)"/utf8>>;
get_step_description(_, _Code) ->
    <<"未知步骤"/utf8>>.

%% @doc 打印Modbus响应报文详细字段（LOG版本）
print_modbus_response_log(Frame) when is_binary(Frame) ->
    Bytes = binary:bin_to_list(Frame),
    case Bytes of
        [T1, T2, P1, P2, L1, L2, Slave, Func | Data] ->
            ?LOG(info, "  Transaction ID: ~4.16.0B ~2.16.0B", [T1, T2]),
            ?LOG(info, "  Protocol ID:    ~4.16.0B ~2.16.0B", [P1, P2]),
            ?LOG(info, "  Length:         ~4.16.0B ~2.16.0B (~p bytes)", [L1, L2, L1*256+L2]),
            ?LOG(info, "  Slave ID:       ~2.16.0B (~p)", [Slave, Slave]),
            ?LOG(info, "  Function Code:  ~2.16.0B (~s)", [Func, get_function_name(Func)]),
            print_response_data_log(Data, Func);
        _ ->
            ?LOG(info, "  Raw Data: ~p", [Bytes])
    end;
print_modbus_response_log(_) ->
    ok.

%% @doc 打印响应数据字段（LOG版本）
print_response_data_log([ByteCount | RegisterData], 16#03) when length(RegisterData) >= ByteCount ->
    ?LOG(info, "  Byte Count:     ~p", [ByteCount]),
    Registers = parse_registers(RegisterData, []),
    ?LOG(info, "  Registers:      ~p", [Registers]);
print_response_data_log([Addr1, Addr2, Value1, Value2], 16#06) ->
    Addr = Addr1 * 256 + Addr2,
    Value = Value1 * 256 + Value2,
    ?LOG(info, "  Register Addr:  ~p", [Addr]),
    ?LOG(info, "  Written Value:  ~p", [Value]);
print_response_data_log(Data, _Func) ->
    ?LOG(info, "  Data: ~p", [Data]).

%% @doc 解析寄存器数据（小端格式）
parse_registers([], Acc) ->
    lists:reverse(Acc);
parse_registers([High, Low | Rest], Acc) ->
    Value = High * 256 + Low,
    parse_registers(Rest, [Value | Acc]);
parse_registers([_], Acc) ->
    lists:reverse(Acc).