%%%-------------------------------------------------------------------
%%% @doc 无人机测试项指令下发链路跟踪器
%%%
%%% 在消息发送链路的每个关键节点添加详细日志，用于调试和监控
%%% 支持三种指令类型：PLC指令、治具指令、无人机指令
%%% 支持两个TCP通道和一个UDP通道
%%%
%%% 消息发送链路：
%%% 1. 测试项加载器 → 命令调度器
%%% 2. 命令调度器 → 命令管理器
%%% 3. 命令管理器 → 具体通道（PLC TCP / 治具 TCP / 无人机 UDP）
%%% 4. 通道 → 网络发送
%%% 5. 模拟设备接收
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_command_tracer).
-author("johnliu").

-include_lib("dgiot/include/logger.hrl").

%% 模块初始化
-on_load(init/0).

%% API
-export([
    %% 链路跟踪
    trace_command_flow/5,
    trace_command_result/4,
    
    %% 日志记录
    log_command_start/5,
    log_command_send/6,
    log_command_receive/5,
    log_command_complete/5,
    log_command_error/6,
    
    %% 链路状态查询
    get_command_trace/1,
    get_all_traces/0,
    clear_traces/0,
    
    %% 测试工具
    test_plc_command_flow/0,
    test_fixture_command_flow/0,
    test_uav_command_flow/0,
    test_all_flows/0
]).

%% ETS表定义
-define(TABLE_COMMAND_TRACES, uav_command_traces).

%% 跟踪记录
-record(command_trace, {
    trace_id :: binary(),
    command_type :: atom(),      % plc | fixture | uav
    station_id :: integer(),
    target_addr :: integer(),
    command_code :: integer(),
    command_value :: integer(),
    test_item_id :: binary() | undefined,
    step_index :: integer() | undefined,
    
    %% 链路节点状态
    nodes = [] :: list(),        % [{node, timestamp, status, details}]
    
    %% 整体状态
    status = pending :: atom(),  % pending | sending | sent | received | completed | failed
    start_time :: integer(),
    end_time :: integer() | undefined,
    error_reason :: term() | undefined
}).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 跟踪命令流程
-spec trace_command_flow(atom(), integer(), integer(), integer(), map()) -> {ok, binary()} | {error, term()}.
trace_command_flow(CommandType, StationId, TargetAddr, CommandCode, Params) ->
    ?LOG(info, "【链路跟踪】开始跟踪命令流程: Type=~p, StationId=~p, TargetAddr=~p, Code=~p", 
         [CommandType, StationId, TargetAddr, CommandCode]),
    
    %% 创建跟踪ID
    TraceId = generate_trace_id(CommandType, StationId, TargetAddr, CommandCode),
    
    %% 提取参数
    TestItemId = maps:get(test_item_id, Params, undefined),
    StepIndex = maps:get(step_index, Params, undefined),
    CommandValue = maps:get(value, Params, 0),
    
    %% 创建跟踪记录
    Trace = #command_trace{
        trace_id = TraceId,
        command_type = CommandType,
        station_id = StationId,
        target_addr = TargetAddr,
        command_code = CommandCode,
        command_value = CommandValue,
        test_item_id = TestItemId,
        step_index = StepIndex,
        start_time = erlang:system_time(millisecond),
        nodes = [
            {command_start, erlang:system_time(millisecond), pending, 
             #{type => CommandType, station_id => StationId, target_addr => TargetAddr}}
        ]
    },
    
    %% 存储跟踪记录
    store_trace(Trace),
    
    ?LOG(info, "【链路跟踪】跟踪ID创建: ~s", [TraceId]),
    {ok, TraceId}.

%% @doc 跟踪命令结果
-spec trace_command_result(binary(), atom(), term(), map()) -> ok.
trace_command_result(TraceId, Status, Result, Details) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            %% 更新节点状态
            Node = {command_result, erlang:system_time(millisecond), Status, 
                   #{result => Result, details => Details}},
            UpdatedNodes = Trace#command_trace.nodes ++ [Node],
            
            %% 更新跟踪记录
            UpdatedTrace = Trace#command_trace{
                status = Status,
                end_time = case Status of
                    completed -> erlang:system_time(millisecond);
                    failed -> erlang:system_time(millisecond);
                    _ -> undefined
                end,
                nodes = UpdatedNodes,
                error_reason = case Status of
                    failed -> Result;
                    _ -> undefined
                end
            },
            
            store_trace(UpdatedTrace),
            
            ?LOG(info, "【链路跟踪】命令结果: TraceId=~s, Status=~p, Result=~p", 
                 [TraceId, Status, Result]),
            ok;
            
        {error, not_found} ->
            ?LOG(warning, "【链路跟踪】未找到跟踪记录: ~s", [TraceId]),
            ok
    end.

%% @doc 记录命令开始
-spec log_command_start(atom(), integer(), integer(), integer(), map()) -> binary().
log_command_start(CommandType, StationId, TargetAddr, CommandCode, Params) ->
    case trace_command_flow(CommandType, StationId, TargetAddr, CommandCode, Params) of
        {ok, TraceId} ->
            ?LOG(info, "【命令开始】Type=~p, StationId=~p, TargetAddr=~p, Code=~p, Value=~p, TraceId=~s", 
                 [CommandType, StationId, TargetAddr, CommandCode, 
                  maps:get(value, Params, 0), TraceId]),
            TraceId;
        {error, Reason} ->
            ?LOG(error, "【命令开始】跟踪创建失败: ~p", [Reason]),
            <<"trace_failed">>
    end.

%% @doc 记录命令发送
-spec log_command_send(binary(), atom(), term(), integer(), integer(), map()) -> ok.
log_command_send(TraceId, NodeType, NodeName, DataSize, ChannelType, Details) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            Node = {command_send, erlang:system_time(millisecond), sending,
                   #{node_type => NodeType, node_name => NodeName, 
                     data_size => DataSize, channel_type => ChannelType,
                     details => Details}},
            UpdatedNodes = Trace#command_trace.nodes ++ [Node],
            UpdatedTrace = Trace#command_trace{
                status = sending,
                nodes = UpdatedNodes
            },
            store_trace(UpdatedTrace),
            
            ?LOG(info, "【命令发送】TraceId=~s, Node=~p:~p, Size=~p, Channel=~p", 
                 [TraceId, NodeType, NodeName, DataSize, ChannelType]),
            ok;
            
        {error, not_found} ->
            ?LOG(warning, "【命令发送】未找到跟踪记录: ~s", [TraceId]),
            ok
    end.

%% @doc 记录命令接收
-spec log_command_receive(binary(), atom(), term(), integer(), map()) -> ok.
log_command_receive(TraceId, NodeType, NodeName, DataSize, Details) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            Node = {command_receive, erlang:system_time(millisecond), received,
                   #{node_type => NodeType, node_name => NodeName, 
                     data_size => DataSize, details => Details}},
            UpdatedNodes = Trace#command_trace.nodes ++ [Node],
            UpdatedTrace = Trace#command_trace{
                status = received,
                nodes = UpdatedNodes
            },
            store_trace(UpdatedTrace),
            
            ?LOG(info, "【命令接收】TraceId=~s, Node=~p:~p, Size=~p", 
                 [TraceId, NodeType, NodeName, DataSize]),
            ok;
            
        {error, not_found} ->
            ?LOG(warning, "【命令接收】未找到跟踪记录: ~s", [TraceId]),
            ok
    end.

%% @doc 记录命令完成
-spec log_command_complete(binary(), atom(), term(), integer(), map()) -> ok.
log_command_complete(TraceId, NodeType, NodeName, Duration, Details) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            Node = {command_complete, erlang:system_time(millisecond), completed,
                   #{node_type => NodeType, node_name => NodeName, 
                     duration_ms => Duration, details => Details}},
            UpdatedNodes = Trace#command_trace.nodes ++ [Node],
            UpdatedTrace = Trace#command_trace{
                status = completed,
                end_time = erlang:system_time(millisecond),
                nodes = UpdatedNodes
            },
            store_trace(UpdatedTrace),
            
            ?LOG(info, "【命令完成】TraceId=~s, Node=~p:~p, Duration=~pms", 
                 [TraceId, NodeType, NodeName, Duration]),
            ok;
            
        {error, not_found} ->
            ?LOG(warning, "【命令完成】未找到跟踪记录: ~s", [TraceId]),
            ok
    end.

%% @doc 记录命令错误
-spec log_command_error(binary(), atom(), term(), term(), integer(), map()) -> ok.
log_command_error(TraceId, NodeType, NodeName, Error, Duration, Details) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            Node = {command_error, erlang:system_time(millisecond), failed,
                   #{node_type => NodeType, node_name => NodeName, 
                     error => Error, duration_ms => Duration, details => Details}},
            UpdatedNodes = Trace#command_trace.nodes ++ [Node],
            UpdatedTrace = Trace#command_trace{
                status = failed,
                end_time = erlang:system_time(millisecond),
                error_reason = Error,
                nodes = UpdatedNodes
            },
            store_trace(UpdatedTrace),
            
            ?LOG(error, "【命令错误】TraceId=~s, Node=~p:~p, Error=~p, Duration=~pms", 
                 [TraceId, NodeType, NodeName, Error, Duration]),
            ok;
            
        {error, not_found} ->
            ?LOG(warning, "【命令错误】未找到跟踪记录: ~s", [TraceId]),
            ok
    end.

%% @doc 获取命令跟踪详情
-spec get_command_trace(binary()) -> {ok, map()} | {error, term()}.
get_command_trace(TraceId) ->
    case get_trace(TraceId) of
        {ok, Trace} ->
            TraceMap = #{
                trace_id => Trace#command_trace.trace_id,
                command_type => Trace#command_trace.command_type,
                station_id => Trace#command_trace.station_id,
                target_addr => Trace#command_trace.target_addr,
                command_code => Trace#command_trace.command_code,
                command_value => Trace#command_trace.command_value,
                test_item_id => Trace#command_trace.test_item_id,
                step_index => Trace#command_trace.step_index,
                status => Trace#command_trace.status,
                start_time => Trace#command_trace.start_time,
                end_time => Trace#command_trace.end_time,
                error_reason => Trace#command_trace.error_reason,
                nodes => Trace#command_trace.nodes,
                duration_ms => case Trace#command_trace.end_time of
                    undefined -> undefined;
                    EndTime -> EndTime - Trace#command_trace.start_time
                end
            },
            {ok, TraceMap};
            
        {error, not_found} ->
            {error, trace_not_found}
    end.

%% @doc 获取所有跟踪记录
-spec get_all_traces() -> {ok, [map()]}.
get_all_traces() ->
    Traces = ets:foldl(fun
        ({_TraceId, Trace}, Acc) ->
            TraceMap = #{
                trace_id => Trace#command_trace.trace_id,
                command_type => Trace#command_trace.command_type,
                station_id => Trace#command_trace.station_id,
                status => Trace#command_trace.status,
                start_time => Trace#command_trace.start_time,
                end_time => Trace#command_trace.end_time,
                node_count => length(Trace#command_trace.nodes)
            },
            [TraceMap | Acc]
    end, [], ?TABLE_COMMAND_TRACES),
    
    {ok, lists:reverse(Traces)}.

%% @doc 清空跟踪记录
-spec clear_traces() -> ok.
clear_traces() ->
    ets:delete_all_objects(?TABLE_COMMAND_TRACES),
    ?LOG(info, "【链路跟踪】所有跟踪记录已清空"),
    ok.

%%%===================================================================
%%% 测试工具函数
%%%===================================================================

%% @doc 测试PLC命令流程
-spec test_plc_command_flow() -> ok.
test_plc_command_flow() ->
    ?LOG(info, "【测试】开始测试PLC命令流程"),
    
    %% 模拟PLC命令发送
    TraceId = log_command_start(plc, 1100, 51, 16#0001, #{
        value => 1,
        test_item_id => <<"test_plc_item">>,
        step_index => 1
    }),
    
    %% 模拟命令发送节点
    log_command_send(TraceId, command_scheduler, dgiot_uav_command_scheduler, 10, tcp, #{
        module => "dgiot_uav_command_scheduler",
        function => "handle_plc_command"
    }),
    
    %% 模拟命令管理器节点
    log_command_send(TraceId, command_manager, dgiot_uav_command_manager, 8, tcp, #{
        module => "dgiot_uav_command_manager",
        function => "send_plc_single"
    }),
    
    %% 模拟TCP通道节点
    log_command_send(TraceId, tcp_channel, dgiot_uav_plc_tcp_client, 6, tcp, #{
        module => "dgiot_uav_plc_tcp_client",
        function => "send_single_command"
    }),
    
    %% 模拟网络发送
    log_command_send(TraceId, network, tcp_socket, 4, tcp, #{
        protocol => "modbus_tcp",
        dest_ip => "192.168.100.40",
        dest_port => 502
    }),
    
    %% 模拟设备接收
    timer:sleep(100),
    log_command_receive(TraceId, device, plc_device, 4, #{
        device_type => "PLC",
        response_code => 16#0001
    }),
    
    %% 命令完成
    log_command_complete(TraceId, complete, command_complete, 150, #{
        result => "success",
        response_data => <<16#01, 16#00, 16#01>>
    }),
    
    ?LOG(info, "【测试】PLC命令流程测试完成，TraceId=~s", [TraceId]),
    ok.

%% @doc 测试治具命令流程
-spec test_fixture_command_flow() -> ok.
test_fixture_command_flow() ->
    ?LOG(info, "【测试】开始测试治具命令流程"),
    
    %% 模拟治具命令发送
    TraceId = log_command_start(fixture, 1100, 10006, 16#0002, #{
        value => 1,
        test_item_id => <<"test_fixture_item">>,
        step_index => 2
    }),
    
    %% 模拟命令发送节点
    log_command_send(TraceId, command_scheduler, dgiot_uav_command_scheduler, 10, tcp, #{
        module => "dgiot_uav_command_scheduler",
        function => "handle_fixture_command"
    }),
    
    %% 模拟命令管理器节点
    log_command_send(TraceId, command_manager, dgiot_uav_command_manager, 8, tcp, #{
        module => "dgiot_uav_command_manager",
        function => "send_fixture_single"
    }),
    
    %% 模拟TCP通道节点
    log_command_send(TraceId, tcp_channel, dgiot_fixture_controller, 6, tcp, #{
        module => "dgiot_fixture_controller",
        function => "build_write_command"
    }),
    
    %% 模拟网络发送
    log_command_send(TraceId, network, tcp_socket, 4, tcp, #{
        protocol => "modbus_tcp",
        dest_ip => "192.168.100.41",
        dest_port => 502
    }),
    
    %% 模拟设备接收
    timer:sleep(100),
    log_command_receive(TraceId, device, fixture_device, 4, #{
        device_type => "Fixture",
        response_code => 16#0002
    }),
    
    %% 命令完成
    log_command_complete(TraceId, complete, command_complete, 120, #{
        result => "success",
        response_data => <<16#02, 16#00, 16#01>>
    }),
    
    ?LOG(info, "【测试】治具命令流程测试完成，TraceId=~s", [TraceId]),
    ok.

%% @doc 测试无人机命令流程
-spec test_uav_command_flow() -> ok.
test_uav_command_flow() ->
    ?LOG(info, "【测试】开始测试无人机命令流程"),
    
    %% 模拟无人机命令发送
    TraceId = log_command_start(uav, 1100, 10007, 16#0003, #{
        value => 1,
        test_item_id => <<"test_uav_item">>,
        step_index => 3
    }),
    
    %% 模拟命令发送节点
    log_command_send(TraceId, command_scheduler, dgiot_uav_command_scheduler, 10, udp, #{
        module => "dgiot_uav_command_scheduler",
        function => "handle_uav_command"
    }),
    
    %% 模拟命令管理器节点
    log_command_send(TraceId, command_manager, dgiot_uav_command_manager, 8, udp, #{
        module => "dgiot_uav_command_manager",
        function => "send_uav_single"
    }),
    
    %% 模拟协议构建节点
    log_command_send(TraceId, protocol, eb90_link_protocol, 12, udp, #{
        module => "eb90_link_protocol",
        function => "build_remote_control_frame"
    }),
    
    %% 模拟UDP多播发送
    log_command_send(TraceId, network, udp_multicast, 106, udp, #{
        protocol => "eb90",
        multicast_group => "226.0.0.80",
        port => 8002
    }),
    
    %% 模拟设备接收
    timer:sleep(100),
    log_command_receive(TraceId, device, uav_device, 106, #{
        device_type => "UAV",
        protocol => "eb90"
    }),
    
    %% 命令完成
    log_command_complete(TraceId, complete, command_complete, 80, #{
        result => "success",
        response_data => <<16#90, 16#EB, 16#03, 16#00>>
    }),
    
    ?LOG(info, "【测试】无人机命令流程测试完成，TraceId=~s", [TraceId]),
    ok.

%% @doc 测试所有命令流程
-spec test_all_flows() -> ok.
test_all_flows() ->
    ?LOG(info, "【测试】开始测试所有命令流程"),
    
    %% 测试PLC命令流程
    test_plc_command_flow(),
    
    %% 测试治具命令流程
    test_fixture_command_flow(),
    
    %% 测试无人机命令流程
    test_uav_command_flow(),
    
    %% 显示所有跟踪记录
    case get_all_traces() of
        {ok, Traces} ->
            ?LOG(info, "【测试】所有跟踪记录统计:"),
            lists:foreach(fun(Trace) ->
                ?LOG(info, "  Trace: ~p", [Trace])
            end, Traces),
            ?LOG(info, "【测试】总计 ~p 条跟踪记录", [length(Traces)]);
        {error, Reason} ->
            ?LOG(error, "【测试】获取跟踪记录失败: ~p", [Reason])
    end,
    
    ?LOG(info, "【测试】所有命令流程测试完成"),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 初始化ETS表
-spec init_ets() -> ok.
init_ets() ->
    case ets:info(?TABLE_COMMAND_TRACES) of
        undefined ->
            ets:new(?TABLE_COMMAND_TRACES, [
                named_table, public, set,
                {keypos, 2},  % 使用trace_id作为键
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ?LOG(info, "【链路跟踪】ETS表创建成功: ~p", [?TABLE_COMMAND_TRACES]);
        _ ->
            ?LOG(info, "【链路跟踪】ETS表已存在: ~p", [?TABLE_COMMAND_TRACES])
    end,
    ok.

%% @doc 生成跟踪ID
-spec generate_trace_id(atom(), integer(), integer(), integer()) -> binary().
generate_trace_id(CommandType, StationId, TargetAddr, CommandCode) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(10000),
    <<(atom_to_binary(CommandType, utf8))/binary, "_", 
      (integer_to_binary(StationId))/binary, "_",
      (integer_to_binary(TargetAddr))/binary, "_",
      (integer_to_binary(CommandCode))/binary, "_",
      (integer_to_binary(Timestamp))/binary, "_",
      (integer_to_binary(Random))/binary>>.

%% @doc 存储跟踪记录
-spec store_trace(#command_trace{}) -> ok.
store_trace(Trace) ->
    init_ets(),
    ets:insert(?TABLE_COMMAND_TRACES, {Trace#command_trace.trace_id, Trace}),
    ok.

%% @doc 获取跟踪记录
-spec get_trace(binary()) -> {ok, #command_trace{}} | {error, not_found}.
get_trace(TraceId) ->
    case ets:lookup(?TABLE_COMMAND_TRACES, TraceId) of
        [{TraceId, Trace}] ->
            {ok, Trace};
        [] ->
            {error, not_found}
    end.

%% @doc 启动函数（模块初始化）
-spec start() -> ok.
start() ->
    init_ets(),
    ?LOG(info, "【链路跟踪】命令跟踪器已启动"),
    ok.

%% @doc 停止函数（模块清理）
init() ->
    start(),
    ok.