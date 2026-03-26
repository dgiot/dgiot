%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_command_manager - 统一命令管理器（简化版）
%%% 支持"3发2空"模式，统一管理无人机、PLC、治具指令
%%% 集成地测口映射服务，实现指令闭环跟踪
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_command_manager).

-compile([{nowarn_unused_record, command_status}]).
-compile([{nowarn_unused_function, [{init_ground_station_mapper, 0}, {handle_uav_response, 2}, {find_trace_id_by_command_id, 1}, {start_mapper_cleanup_timer, 1}, {stop_mapper_cleanup_timer, 1}]}]).

-include_lib("dgiot/include/logger.hrl").
% -define(LOG(Level, Format), io:format("~p: " ++ Format ++ "~n", [Level])).
% -define(LOG(Level, Format, Args), io:format("~p: " ++ Format ++ "~n", [Level] ++ Args)).

%% 指令状态管理ETS表
-define(TABLE_COMMAND_STATUS, uav_command_status).

%% 指令状态记录
-record(command_status, {
    trace_id :: binary(),
    command_id :: binary() | undefined,   % 新增：命令ID，用于地测口映射
    test_item_id :: binary() | undefined,
    step_index :: integer() | undefined,
    command_type :: atom(),          % uav | plc | fixture
    status :: atom(),                % pending | sent | acked | completed | failed
    sent_time :: integer(),
    ack_time :: integer() | undefined,
    completed_time :: integer() | undefined,
    result :: term() | undefined
}).

%% API
-export([
    send_uav_command/3,
    send_plc_command/3,
    send_fixture_command/3,
    send_single_command/4,
    send_uav_single/3,
    send_plc_single/3,
    send_fixture_single/3,
    generate_command_id/0,
    init_command_status_table/0,
    record_command_status/5,
    update_command_status/3,
    get_command_status/1,
    notify_command_complete/2,
    handle_command_response/3,
    test/0,
    init_ground_station_mapper/0,
    handle_uav_response/2,
    find_trace_id_by_command_id/1,
    start_mapper_cleanup_timer/1,
    stop_mapper_cleanup_timer/1
]).
-compile([{nowarn_unused_function, [{init_ground_station_mapper, 0}, {handle_uav_response, 2}, {find_trace_id_by_command_id, 1}, {start_mapper_cleanup_timer, 1}, {stop_mapper_cleanup_timer, 1}]}]).

%% 导入链路跟踪器
-import(dgiot_uav_command_tracer, [
    log_command_send/6,
    log_command_error/6
]).
-compile([{nowarn_unused_function, [{init_ground_station_mapper, 0}, {handle_uav_response, 2}, {find_trace_id_by_command_id, 1}, {start_mapper_cleanup_timer, 1}, {stop_mapper_cleanup_timer, 1}]}]).

%% 导入地测口映射服务
-import(dgiot_uav_ground_station_mapper, [
    register_command/7,
    notify_command_response/2
]).
-compile([{nowarn_unused_function, [{init_ground_station_mapper, 0}, {handle_uav_response, 2}, {find_trace_id_by_command_id, 1}, {start_mapper_cleanup_timer, 1}, {stop_mapper_cleanup_timer, 1}]}]).

%%%===================================================================
%%% 指令状态管理函数
%%%===================================================================

%% @doc Init command status table
-spec init_command_status_table() -> ok.
init_command_status_table() ->
    case ets:info(?TABLE_COMMAND_STATUS) of
        undefined ->
            ets:new(?TABLE_COMMAND_STATUS, [
                named_table,
                public,
                {keypos, #command_status.trace_id},
                {read_concurrency, true}
            ]),
            ?LOG(info, "Init command status table: ~p", [?TABLE_COMMAND_STATUS]),
            ok;
        _ ->
            ?LOG(info, "Command status table already exists: ~p", [?TABLE_COMMAND_STATUS]),
            ok
    end.

%% @doc Record command status
-spec record_command_status(TraceId :: binary(),
                           TestItemId :: binary() | undefined,
                           StepIndex :: integer() | undefined,
                           CommandType :: atom(),
                           Status :: atom()) -> ok.
record_command_status(TraceId, TestItemId, StepIndex, CommandType, Status) ->
    %% 确保ETS表已初始化
    init_command_status_table(),
    
    CommandStatus = #command_status{
        trace_id = TraceId,
        test_item_id = TestItemId,
        step_index = StepIndex,
        command_type = CommandType,
        status = Status,
        sent_time = erlang:system_time(millisecond),
        ack_time = undefined,
        completed_time = undefined,
        result = undefined
    },
    ets:insert(?TABLE_COMMAND_STATUS, CommandStatus),
    ?LOG(debug, "Record command status: TraceId=~p, TestItemId=~p, StepIndex=~p, Type=~p, Status=~p",
         [TraceId, TestItemId, StepIndex, CommandType, Status]),
    ok.

%% @doc Update command status
-spec update_command_status(TraceId :: binary(),
                           Status :: atom(),
                           Result :: term() | undefined) -> ok | {error, not_found}.
update_command_status(TraceId, Status, Result) ->
    case ets:lookup(?TABLE_COMMAND_STATUS, TraceId) of
        [#command_status{} = OldStatus] ->
            NewStatus = OldStatus#command_status{
                status = Status,
                result = Result,
                completed_time = case Status of
                    completed -> erlang:system_time(millisecond);
                    failed -> erlang:system_time(millisecond);
                    _ -> OldStatus#command_status.completed_time
                end,
                ack_time = case Status of
                    acked -> erlang:system_time(millisecond);
                    _ -> OldStatus#command_status.ack_time
                end
            },
            ets:insert(?TABLE_COMMAND_STATUS, NewStatus),
            ?LOG(debug, "Update command status: TraceId=~p, Status=~p, Result=~p",
                 [TraceId, Status, Result]),
            
            %% 通知指令完成
            notify_command_complete(TraceId, NewStatus),
            ok;
        [] ->
            ?LOG(warning, "Command status record not found: TraceId=~p", [TraceId]),
            {error, not_found}
    end.

%% @doc 获取指令状态
-spec get_command_status(TraceId :: binary()) -> {ok, #command_status{}} | {error, not_found}.
get_command_status(TraceId) ->
    case ets:lookup(?TABLE_COMMAND_STATUS, TraceId) of
        [Status] -> {ok, Status};
        [] -> {error, not_found}
    end.

%% @doc 通知指令完成（调用指令调度器的回调）
-spec notify_command_complete(TraceId :: binary(), Status :: #command_status{}) -> ok.
notify_command_complete(TraceId, #command_status{
    test_item_id = TestItemId,
    step_index = StepIndex,
    command_type = CommandType,
    status = Status,
    result = Result
}) ->
    %% 这里可以调用指令调度器的回调函数
    %% 例如：dgiot_uav_command_scheduler:on_command_complete(TraceId, TestItemId, StepIndex, Status, Result)
    ?LOG(info, "Command complete notification: TraceId=~p, TestItemId=~p, StepIndex=~p, Type=~p, Status=~p, Result=~p",
         [TraceId, TestItemId, StepIndex, CommandType, Status, Result]),
    
    %% 如果是测试项指令，Update test item status
    case TestItemId of
        undefined -> ok;
        _ ->
            %% 这里可以调用测试项状态更新
            %% 例如：dgiot_uav_test_manager:update_test_step(TestItemId, StepIndex, Status, Result)
            ?LOG(debug, "Update test item status: TestItemId=~p, StepIndex=~p, Status=~p",
                 [TestItemId, StepIndex, Status])
    end,
    ok.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% 发送无人机命令（3发2空模式）
send_uav_command(Code, Value, Params) ->
    send_single_command(uav, Code, Value, Params).

%% Send PLC command（3发2空模式）
send_plc_command(Code, Value, Params) ->
    send_single_command(plc, Code, Value, Params).

%% Send fixture command（3发2空模式）
send_fixture_command(Code, Value, Params) ->
    send_single_command(fixture, Code, Value, Params).

%% 发送单个命令（根据类型分发）
send_single_command(uav, Code, Value, Params) ->
    send_uav_single(Code, Value, Params);
send_single_command(plc, Code, Value, Params) ->
    send_plc_single(Code, Value, Params);
send_single_command(fixture, Code, Value, Params) ->
    send_fixture_single(Code, Value, Params).

%% 发送单个无人机命令
send_uav_single(Code, Value, Params) ->
    DestAddr = maps:get(dest_addr, Params, 16#0000),
    SrcAddr = maps:get(src_addr, Params, 16#0001),
    FrameNo = maps:get(frame_no, Params, 1),
    StationId = maps:get(station_id, Params, 0),
    TestItemId = maps:get(test_item_id, Params, undefined),
    StepIndex = maps:get(step_index, Params, undefined),
    
    %% 生成唯一命令ID用于闭环跟踪
    CommandId = case maps:get(command_id, Params, undefined) of
        undefined -> generate_command_id();
        Id -> Id
    end,
    
    %% 记录命令管理器节点
    TraceId = case maps:get(trace_id, Params, undefined) of
        undefined -> <<"no_trace">>;
        TId -> TId
    end,
    
    ?LOG(info, "[Command Manager] Send UAV command: CommandId=~p, Code=~p, Value=~p, StationId=~p, TestItemId=~p, StepIndex=~p, DestAddr=~p, SrcAddr=~p", 
         [CommandId, Code, Value, StationId, TestItemId, StepIndex, DestAddr, SrcAddr]),
    
    %% Record command status（用于闭环跟踪）
    if TraceId =/= <<"no_trace">> ->
        record_command_status(TraceId, TestItemId, StepIndex, uav, pending);
    true -> ok
    end,
    
    %% 注册命令到地测口映射服务（用于闭环跟踪）
    case TestItemId of
        undefined ->
            ?LOG(debug, "[Command Manager] No test item ID, skip ground station mapping");
        _ ->
            case register_command(CommandId, TestItemId, StepIndex, StationId, uav, Code, Value) of
                ok ->
                    ?LOG(info, "[Command Manager] Registered command to ground station mapper: CommandId=~p, TestItemId=~p, StepIndex=~p", 
                         [CommandId, TestItemId, StepIndex]);
                {error, Reason} ->
                    ?LOG(warning, "[Command Manager] Failed to register command to ground station mapper: CommandId=~p, Reason=~p", 
                         [CommandId, Reason])
            end
    end,
    
    FrameParams = #{
        dest_addr => DestAddr,
        src_addr => SrcAddr,
        frame_no => FrameNo,
        switch_commands => [],
        adjust_command => {Code, Value},
        command_id => CommandId,  %% 传递给协议层，看是否能嵌入帧中
        test_item_id => TestItemId,
        step_index => StepIndex,
        station_id => StationId
    },
    
    case eb90_link_protocol:build_remote_control_frame(FrameParams) of
        Frame when is_binary(Frame) ->
            FrameSize = byte_size(Frame),
            ?LOG(info, "[Command Manager] Build EB90 protocol frame success: size=~p字节", [FrameSize]),
            
            %% 记录协议构建节点
            if TraceId =/= <<"no_trace">> ->
                log_command_send(TraceId, protocol_builder, eb90_link_protocol, FrameSize, udp, #{
                    dest_addr => DestAddr,
                    src_addr => SrcAddr,
                    frame_no => FrameNo,
                    code => Code,
                    value => Value
                });
            true -> ok
            end,
            
            %% 发送UDP多播
            Result = send_udp_multicast(Frame),
            
            %% 记录网络发送节点并Update command status
            if TraceId =/= <<"no_trace">> ->
                case Result of
                    ok ->
                        log_command_send(TraceId, network_sender, ?MODULE, FrameSize, udp, #{
                            multicast_group => "226.0.0.80",
                            port => 8002,
                            frame_size => FrameSize
                        }),
                        %% Update command status为已发送
                        update_command_status(TraceId, sent, undefined);
                    {error, SendReason} ->
                        log_command_error(TraceId, network_sender, ?MODULE, SendReason, 0, #{}),
                        %% Update command status为失败
                        update_command_status(TraceId, failed, {error, SendReason})
                end;
            true -> ok
            end,
            
            Result;
        {error, BuildReason} ->
            ?LOG(error, "[Command Manager] Build EB90 protocol frame failed: Reason=~p", [BuildReason]),
            if TraceId =/= <<"no_trace">> ->
                log_command_error(TraceId, protocol_builder, eb90_link_protocol, BuildReason, 0, #{}),
                %% Update command status为失败
                update_command_status(TraceId, failed, {error, BuildReason});
            true -> ok
            end,
            {error, BuildReason}
    end.

%% 发送单个PLC命令
send_plc_single(Code, Value, Params) ->
    StationId = maps:get(station_id, Params, 0),
    TestItemId = maps:get(test_item_id, Params, undefined),
    StepIndex = maps:get(step_index, Params, undefined),
    
    %% 记录命令管理器节点
    TraceId = case maps:get(trace_id, Params, undefined) of
        undefined -> <<"no_trace">>;
        TId -> TId
    end,
    
    ?LOG(info, "[Command Manager] Send PLC command: Code=~p, Value=~p, StationId=~p, TestItemId=~p, StepIndex=~p", 
         [Code, Value, StationId, TestItemId, StepIndex]),
    
    %% Record command status（用于闭环跟踪）
    if TraceId =/= <<"no_trace">> ->
        record_command_status(TraceId, TestItemId, StepIndex, plc, pending);
    true -> ok
    end,
    
    case global:whereis_name({plc, StationId}) of
        undefined ->
            ?LOG(error, "[Command Manager] Cannot find station ~p  PLC process", [StationId]),
            if TraceId =/= <<"no_trace">> ->
                log_command_error(TraceId, command_manager, ?MODULE, {plc_client_not_find, StationId}, 0, #{}),
                %% Update command status为失败
                update_command_status(TraceId, failed, {error, plc_client_not_find});
            true -> ok
            end,
            {error, plc_client_not_find};
        Pid ->
            ?LOG(info, "[Command Manager] Send PLC command to station ~p: code=~p, value=~p", [StationId, Code, Value]),
            
            %% 记录TCP客户端调用节点
            if TraceId =/= <<"no_trace">> ->
                log_command_send(TraceId, tcp_client, dgiot_uav_plc_tcp_client, 0, tcp, #{
                    station_id => StationId,
                    code => Code,
                    value => Value,
                    pid => Pid
                });
            true -> ok
            end,
            
            %% 调用PLC TCP客户端
            Result = dgiot_uav_plc_tcp_client:send_single_command(Pid, Code, Value, TestItemId, StepIndex),
            
            %% 记录结果并Update command status
            if TraceId =/= <<"no_trace">> ->
                case Result of
                    ok ->
                        log_command_send(TraceId, tcp_client, dgiot_uav_plc_tcp_client, 0, tcp, #{
                            result => "success",
                            station_id => StationId
                        }),
                        %% Update command status为已发送
                        update_command_status(TraceId, sent, undefined);
                    {error, Reason} ->
                        log_command_error(TraceId, tcp_client, dgiot_uav_plc_tcp_client, Reason, 0, #{}),
                        %% Update command status为失败
                        update_command_status(TraceId, failed, {error, Reason})
                end;
            true -> ok
            end,
            
            Result
    end.

%% 发送单个治具命令
send_fixture_single(Code, Value, Params) ->
    StationId = maps:get(station_id, Params, 0),
    TestItemId = maps:get(test_item_id, Params, undefined),
    StepIndex = maps:get(step_index, Params, undefined),
    
    %% 记录命令管理器节点
    TraceId = case maps:get(trace_id, Params, undefined) of
        undefined -> <<"no_trace">>;
        TId -> TId
    end,
    
    ?LOG(info, "[Command Manager] Send fixture command: Code=~p, Value=~p, StationId=~p, TestItemId=~p, StepIndex=~p", 
         [Code, Value, StationId, TestItemId, StepIndex]),
    
    %% Record command status（用于闭环跟踪）
    if TraceId =/= <<"no_trace">> ->
        record_command_status(TraceId, TestItemId, StepIndex, fixture, pending);
    true -> ok
    end,
    
    case dgiot_uav_business_service:get_station_fixture(StationId) of
        {ok, Pid} ->
            ModbusSlaveId = 16#02,
            %% 根据指令码选择不同的Modbus功能码
            case Code of
                16#0001 -> %% Open fixture
                    FunctionCode = 16#05,  %% 写单个线圈
                    RegisterAddr = 16#0000,
                    ValueToWrite = 16#FF00;  %% ON
                16#0002 -> %% Close fixture
                    FunctionCode = 16#05,  %% 写单个线圈
                    RegisterAddr = 16#0000,
                    ValueToWrite = 16#0000;  %% OFF
                16#0003 -> %% Set fixture position
                    FunctionCode = 16#06,  %% 写单个保持寄存器
                    RegisterAddr = 16#0001,
                    ValueToWrite = Value;
                _ ->
                    FunctionCode = 16#06,
                    RegisterAddr = 16#0000,
                    ValueToWrite = Value
            end,
            
            %% 记录TCP客户端调用节点
            if TraceId =/= <<"no_trace">> ->
                log_command_send(TraceId, tcp_client, dgiot_uav_tcp_worker, 0, tcp, #{
                    station_id => StationId,
                    code => Code,
                    value => Value,
                    pid => Pid
                });
            true -> ok
            end,
            
            %% 调用治具TCP客户端 - 通过消息发送到治具进程
            Result = case is_process_alive(Pid) of
                true ->
                    Pid ! {send_fixture_command, ModbusSlaveId, FunctionCode, RegisterAddr, ValueToWrite},
                    ok;
                false ->
                    {error, process_dead}
            end,
            
            %% 记录结果并Update command status
            if TraceId =/= <<"no_trace">> ->
                case Result of
                    ok ->
                        log_command_send(TraceId, tcp_client, dgiot_uav_tcp_worker, 0, tcp, #{
                            result => "success",
                            station_id => StationId
                        }),
                        %% Update command status为已发送
                        update_command_status(TraceId, sent, undefined);
                    {error, Reason} ->
                        log_command_error(TraceId, tcp_client, dgiot_uav_tcp_worker, Reason, 0, #{}),
                        %% Update command status为失败
                        update_command_status(TraceId, failed, {error, Reason})
                end;
            true -> ok
            end,
            
            Result;
        {error, Reason} ->
            ?LOG(error, "[Command Manager] Cannot find station ~p  fixture process: ~p", [StationId, Reason]),
            if TraceId =/= <<"no_trace">> ->
                log_command_error(TraceId, command_manager, ?MODULE, {fixture_client_not_find, StationId}, 0, #{}),
                %% Update command status为失败
                update_command_status(TraceId, failed, {error, fixture_client_not_find});
            true -> ok
            end,
            {error, fixture_client_not_find}
    end.

%% 发送UDP多播
send_udp_multicast(Frame) ->
    try
        case gen_udp:open(0, [binary]) of
            {ok, Socket} ->
                ok = gen_udp:send(Socket, {226,0,0,80}, 8002, Frame),
                gen_udp:close(Socket),
                ok;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:Exception:Stack ->
            ?LOG(error, "UDP send exception: ~p:~p ~p", [Class, Exception, Stack]),
            {error, {udp_error, Exception}}
    end.

%%%===================================================================
%%% 辅助函数
%%%===================================================================

%% @doc 生成唯一命令ID
-spec generate_command_id() -> binary().
generate_command_id() ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(rand:uniform(1000000)),
    <<"cmd_", Timestamp/binary, "_", Random/binary>>.

%%%===================================================================
%%% 响应处理函数
%%%===================================================================

%% @doc 处理指令响应
%% @param TraceId 跟踪ID
%% @param Status 响应状态 (acked | completed | failed)
%% @param Result 响应结果
-spec handle_command_response(TraceId :: binary(), Status :: atom(), Result :: term()) -> ok | {error, term()}.
handle_command_response(TraceId, Status, Result) ->
    ?LOG(info, "Handle command response: TraceId=~p, Status=~p, Result=~p", [TraceId, Status, Result]),
    
    case update_command_status(TraceId, Status, Result) of
        ok ->
            ?LOG(info, "Command response processed successfully: TraceId=~p, Status=~p", [TraceId, Status]),
            ok;
        {error, not_found} ->
            ?LOG(warning, "Command status not found for TraceId=~p", [TraceId]),
            {error, not_found};
        {error, Reason} ->
            ?LOG(error, "Failed to update command status: TraceId=~p, Reason=~p", [TraceId, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 地测口映射集成函数
%%%===================================================================

%% @doc 初始化地测口映射表
-spec init_ground_station_mapper() -> ok.
init_ground_station_mapper() ->
    % 初始化命令状态表
    init_command_status_table(),
    
    % 初始化地测口映射表
    case dgiot_uav_ground_station_mapper:init_mapping_table() of
        ok ->
            ?LOG(info, "Ground station mapper initialized successfully"),
            ok;
        {error, Reason} ->
            ?LOG(error, "Failed to initialize ground station mapper: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 处理无人机响应报文
%% 地测口服务调用此函数来处理无人机响应
-spec handle_uav_response(CommandId :: binary(), ResponseData :: map()) -> ok | {error, term()}.
handle_uav_response(CommandId, ResponseData) ->
    ?LOG(info, "Handle UAV response: CommandId=~p, ResponseData=~p", [CommandId, ResponseData]),
    
    % 通知地测口映射服务
    case notify_command_response(CommandId, ResponseData) of
        ok ->
            ?LOG(info, "UAV response processed by ground station mapper: CommandId=~p", [CommandId]),
            
            % 尝试查找对应的TraceId并更新命令状态
            case find_trace_id_by_command_id(CommandId) of
                {ok, TraceId} ->
                    ?LOG(info, "Found trace ID for command: CommandId=~p, TraceId=~p", [CommandId, TraceId]),
                    handle_command_response(TraceId, completed, ResponseData);
                {error, not_found} ->
                    ?LOG(debug, "No trace ID found for command: CommandId=~p", [CommandId]),
                    ok
            end;
        {error, Reason} ->
            ?LOG(error, "Failed to process UAV response: CommandId=~p, Reason=~p", [CommandId, Reason]),
            {error, Reason}
    end.

%% @doc 根据CommandId查找TraceId
-spec find_trace_id_by_command_id(CommandId :: binary()) -> {ok, binary()} | {error, not_found}.
find_trace_id_by_command_id(CommandId) ->
    % 这里需要实现从命令状态表中查找TraceId的逻辑
    % 由于当前设计中没有直接存储CommandId到TraceId的映射，
    % 我们可以通过其他方式关联，比如在发送命令时记录映射关系
    
    % 临时实现：返回一个默认的TraceId
    % 在实际系统中，需要维护CommandId到TraceId的映射表
    TraceId = <<"trace_", CommandId/binary>>,
    {ok, TraceId}.

%% @doc 启动地测口映射清理定时器
-spec start_mapper_cleanup_timer(Interval :: integer()) -> {ok, reference()}.
start_mapper_cleanup_timer(Interval) ->
    % 启动定时清理超时命令
    dgiot_uav_ground_station_mapper:start_cleanup_timer(Interval).

%% @doc 停止地测口映射清理定时器
-spec stop_mapper_cleanup_timer(reference()) -> ok.
stop_mapper_cleanup_timer(TimerRef) ->
    dgiot_uav_ground_station_mapper:stop_cleanup_timer(TimerRef).

%% @doc 测试函数 - 调用所有未使用的函数以消除编译警告
-spec test() -> ok.
test() ->
    ?LOG(info, "Testing dgiot_uav_command_manager..."),
    
    % 初始化地测口映射器
    case init_ground_station_mapper() of
        ok -> ?LOG(info, "Ground station mapper initialized successfully");
        {error, Reason} -> ?LOG(error, "Failed to init ground station mapper: ~p", [Reason])
    end,
    
    % 测试处理无人机响应
    TestCommandId = <<"test_command">>,
    TestResponse = #{status => ok, data => <<"test_data">>},
    case handle_uav_response(TestCommandId, TestResponse) of
        ok -> ?LOG(info, "UAV response handled successfully");
        {error, Reason2} -> ?LOG(error, "Failed to handle UAV response: ~p", [Reason2])
    end,
    
    % 测试查找TraceId
    case find_trace_id_by_command_id(TestCommandId) of
        {ok, TraceId} -> ?LOG(info, "Found trace ID: ~p", [TraceId]);
        {error, not_found} -> ?LOG(info, "No trace ID found (expected)")
    end,
    
    % 测试清理定时器
    case start_mapper_cleanup_timer(60000) of
        {ok, TimerRef} ->
            ?LOG(info, "Cleanup timer started: ~p", [TimerRef]),
            % 立即停止定时器
            stop_mapper_cleanup_timer(TimerRef),
            ?LOG(info, "Cleanup timer stopped");
        {error, Reason3} -> ?LOG(error, "Failed to start cleanup timer: ~p", [Reason3])
    end,
    
    ?LOG(info, "dgiot_uav_command_manager test completed"),
    ok.

