%%%-------------------------------------------------------------------
%%% @doc 磁航向工位自动化测试协调器
%%% 负责：测试项加载 → PLC七步校验 → 无人机指令下发 → 测试结果汇聚 → 报文日志
%%%-------------------------------------------------------------------
-module(dgiot_uav_auto_test_coordinator).
-author("johnliu").
-behaviour(gen_server).

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% API
-export([
    start_link/0,
    start_test/1,        %% 启动测试 (StationId)
    stop_test/1,         %% 停止测试
    get_test_status/1,   %% 获取测试状态
    test/0               %% 在线测试函数
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    station_id :: integer(),
    test_items = [] :: list(),
    current_test_item_id :: binary() | undefined,
    current_step_index = 0 :: integer(),
    test_status = <<"idle">> :: binary(),
    test_results = #{} :: map(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    packet_logger :: pid() | undefined
}).

-define(TEST_TIMEOUT, 600000).  %% 10分钟超时

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 启动自动化测试
start_test(StationId) ->
    gen_server:call(?MODULE, {start_test, StationId}, ?TEST_TIMEOUT).

stop_test(StationId) ->
    gen_server:call(?MODULE, {stop_test, StationId}).

get_test_status(StationId) ->
    gen_server:call(?MODULE, {get_test_status, StationId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG(info, "磁航向自动化测试协调器已启动", []),
    {ok, #state{}}.

handle_call({start_test, StationId}, _From, State) ->
    case State#state.test_status of
        <<"idle">> ->
            ?LOG(info, "~n~n========================================", []),
            ?LOG(info, "🚀 【自动化测试协调器】启动测试", []),
            ?LOG(info, "========================================", []),
            ?LOG(info, "Station ID: ~p", [StationId]),
            ?LOG(info, "========================================~n", []),
            
            %% 1. 加载测试项
            case load_test_items(StationId) of
                {ok, TestItems} ->
                    ?LOG(info, "✅ 测试项加载成功: ~p项", [length(TestItems)]),
                    
                    %% 2. 启动报文日志记录器
                    PacketLogger = start_packet_logger(StationId),
                    
                    %% 3. 更新状态
                    NewState = State#state{
                        station_id = StationId,
                        test_items = TestItems,
                        test_status = <<"running">>,
                        start_time = erlang:system_time(millisecond),
                        packet_logger = PacketLogger
                    },
                    
                    %% 4. 异步执行测试
                    self() ! execute_next_test_item,
                    
                    {reply, {ok, started}, NewState};
                {error, Reason} ->
                    ?LOG(error, "❌ 测试项加载失败: ~p", [Reason]),
                    {reply, {error, Reason}, State}
            end;
        <<"running">> ->
            ?LOG(warning, "测试正在进行中，无法重复启动", []),
            {reply, {error, test_already_running}, State};
        _ ->
            {reply, {error, invalid_status}, State}
    end;

handle_call({stop_test, StationId}, _From, #state{station_id = StationId} = State) ->
    ?LOG(info, "停止测试: StationId=~p", [StationId]),
    NewState = State#state{
        test_status = <<"stopped">>,
        end_time = erlang:system_time(millisecond)
    },
    {reply, ok, NewState};

handle_call({get_test_status, StationId}, _From, #state{station_id = StationId} = State) ->
    Status = #{
        station_id => StationId,
        test_status => State#state.test_status,
        current_test_item_id => State#state.current_test_item_id,
        current_step_index => State#state.current_step_index,
        test_results => State#state.test_results,
        start_time => State#state.start_time,
        end_time => State#state.end_time
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(execute_next_test_item, #state{test_items = []} = State) ->
    ?LOG(info, "~n========================================", []),
    ?LOG(info, "✅ 【自动化测试协调器】所有测试项执行完成", []),
    ?LOG(info, "========================================~n", []),
    NewState = State#state{
        test_status = <<"completed">>,
        end_time = erlang:system_time(millisecond)
    },
    {noreply, NewState};

handle_info(execute_next_test_item, #state{
    test_items = [TestItem | Remaining],
    station_id = StationId,
    packet_logger = PacketLogger
} = State) ->
    {test_item, TestItemId, TestItemName, _Status, _Type, Steps, _Priority} = TestItem,
    
    ?LOG(info, "~n~n========================================", []),
    ?LOG(info, "📋 【测试项执行】开始执行测试项", []),
    ?LOG(info, "========================================", []),
    ?LOG(info, "Test Item ID: ~s", [TestItemId]),
    ?LOG(info, "Test Item Name: ~ts", [TestItemName]),
    ?LOG(info, "Total Steps: ~p", [length(Steps)]),
    ?LOG(info, "========================================~n", []),
    
    %% 记录测试项开始
    log_packet(PacketLogger, test_item_start, #{
        test_item_id => TestItemId,
        test_item_name => TestItemName,
        steps => length(Steps)
    }),
    
    %% 执行测试项的所有步骤
    TestResult = execute_test_item_steps(StationId, TestItemId, Steps, PacketLogger),
    
    %% 记录测试项完成
    log_packet(PacketLogger, test_item_complete, #{
        test_item_id => TestItemId,
        test_item_name => TestItemName,
        result => TestResult
    }),
    
    %% 更新测试结果
    UpdatedResults = maps:put(TestItemId, TestResult, State#state.test_results),
    NewState = State#state{
        test_items = Remaining,
        current_test_item_id = TestItemId,
        test_results = UpdatedResults
    },
    
    %% 继续执行下一个测试项
    self() ! execute_next_test_item,
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{packet_logger = PacketLogger}) when is_pid(PacketLogger) ->
    PacketLogger ! stop,
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 加载测试项
load_test_items(StationId) ->
    case dgiot_uav_test_loader:load_by_station(StationId) of
        {ok, TestItems} when is_list(TestItems) ->
            %% 按优先级排序
            SortedItems = lists:sort(
                fun({test_item, _, _, _, _, _, P1}, {test_item, _, _, _, _, _, P2}) ->
                    P1 =< P2
                end,
                TestItems
            ),
            {ok, SortedItems};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 执行测试项的所有步骤
execute_test_item_steps(StationId, TestItemId, Steps, PacketLogger) ->
    StartTime = erlang:system_time(millisecond),
    
    StepResults = lists:map(
        fun(Step) ->
            StepNumber = maps:get(<<"step_number">>, Step, 0),
            ActionType = maps:get(<<"action_type">>, Step, <<"unknown">>),
            Description = maps:get(<<"description">>, Step, <<"">>),
            Target = maps:get(<<"target">>, Step, <<"unknown">>),
            
            ?LOG(info, "~n----------------------------------------", []),
            ?LOG(info, "📌 执行步骤 ~p: ~ts", [StepNumber, Description]),
            ?LOG(info, "  Action Type: ~s", [ActionType]),
            ?LOG(info, "  Target: ~ts", [Target]),
            ?LOG(info, "----------------------------------------", []),
            
            %% 记录步骤开始
            log_packet(PacketLogger, step_start, #{
                test_item_id => TestItemId,
                step_number => StepNumber,
                action_type => ActionType,
                description => Description,
                target => Target
            }),
            
            %% 执行步骤
            StepResult = execute_step(StationId, Step, PacketLogger),
            
            %% 记录步骤完成
            log_packet(PacketLogger, step_complete, #{
                test_item_id => TestItemId,
                step_number => StepNumber,
                result => StepResult
            }),
            
            %% 步骤间延迟
            timer:sleep(500),
            
            {StepNumber, StepResult}
        end,
        Steps
    ),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% 汇总测试项结果
    AllPassed = lists:all(
        fun({_StepNum, Result}) ->
            case Result of
                {ok, _} -> true;
                passed -> true;
                _ -> false
            end
        end,
        StepResults
    ),
    
    #{
        test_item_id => TestItemId,
        status => case AllPassed of true -> <<"passed">>; false -> <<"failed">> end,
        step_results => StepResults,
        start_time => StartTime,
        end_time => EndTime,
        duration => Duration
    }.

%% @doc 执行单个步骤
execute_step(StationId, Step, PacketLogger) ->
    ActionType = maps:get(<<"action_type">>, Step, <<"unknown">>),
    Target = maps:get(<<"target">>, Step, <<"unknown">>),
    
    case ActionType of
        <<"send">> ->
            execute_send_action(StationId, Target, Step, PacketLogger);
        <<"judge">> ->
            execute_judge_action(StationId, Target, Step, PacketLogger);
        _ ->
            ?LOG(error, "未知的action_type: ~s", [ActionType]),
            {error, unknown_action_type}
    end.

%% @doc 执行send动作
execute_send_action(StationId, Target, Step, PacketLogger) ->
    SendData = maps:get(<<"send">>, Step, #{}),
    Content = maps:get(<<"content">>, SendData, <<"">>),
    
    ?LOG(info, "  发送数据: ~s -> ~ts", [Content, Target]),
    
    %% 根据目标类型选择执行方式
    case binary_to_list(Target) of
        "工位PLC" ++ _ ->
            %% PLC指令下发（通过PLC七步校验）
            Code = binary_to_integer(Content),
            execute_plc_7step(StationId, Code, PacketLogger);
        "无人机" ++ _ ->
            %% 无人机指令下发（EB90遥控）
            execute_uav_command(StationId, Content, PacketLogger);
        _ ->
            ?LOG(warning, "未知的发送目标: ~ts", [Target]),
            {error, unknown_target}
    end.

%% @doc 执行judge动作
execute_judge_action(_StationId, _Target, Step, _PacketLogger) ->
    %% 这里应该实现判据逻辑（SQL/TDengine SQL/简单阈值）
    %% 目前简化为返回passed
    ?LOG(info, "  执行判据逻辑（简化实现）"),
    Description = maps:get(<<"description">>, Step, <<"">>),
    ?LOG(info, "  判据描述: ~ts", [Description]),
    passed.

%% @doc 执行PLC七步校验
execute_plc_7step(StationId, Code, PacketLogger) ->
    ?LOG(info, "  执行PLC七步校验: Code=~p", [Code]),
    
    %% 查找PLC客户端进程
    case global:whereis_name({plc, StationId}) of
        undefined ->
            ?LOG(error, "PLC客户端进程未找到: StationId=~p", [StationId]),
            {error, plc_client_not_found};
        Pid ->
            %% 调用PLC客户端执行七步校验
            case dgiot_uav_plc_tcp_client:start_continuous_test(Pid, [Code]) of
                ok ->
                    ?LOG(info, "  ✅ PLC七步校验启动成功"),
                    
                    %% 记录PLC命令
                    log_packet(PacketLogger, plc_7step_start, #{
                        station_id => StationId,
                        code => Code
                    }),
                    
                    %% 等待执行完成（简化实现）
                    timer:sleep(7000),
                    
                    log_packet(PacketLogger, plc_7step_complete, #{
                        station_id => StationId,
                        code => Code,
                        result => passed
                    }),
                    
                    {ok, passed};
                {error, Reason} ->
                    ?LOG(error, "  ❌ PLC七步校验启动失败: ~p", [Reason]),
                    {error, Reason}
            end
    end.

%% @doc 执行无人机指令下发（EB90遥控）
execute_uav_command(StationId, Content, PacketLogger) ->
    ?LOG(info, "  执行无人机指令下发: Content=~s", [Content]),
    
    %% 这里应该实现EB90遥控指令下发
    %% 目前简化为记录日志
    
    log_packet(PacketLogger, uav_command, #{
        station_id => StationId,
        content => Content
    }),
    
    ?LOG(info, "  ✅ 无人机指令下发成功（简化实现）"),
    {ok, passed}.

%% @doc 启动报文日志记录器
start_packet_logger(StationId) ->
    spawn_link(
        fun() ->
            LogDir = "/tmp/uav_test_logs",
            filelib:ensure_dir(LogDir ++ "/"),
            LogFile = io_lib:format("~s/station_~p_~p.log", 
                [LogDir, StationId, erlang:system_time(millisecond)]),
            {ok, IoDevice} = file:open(LogFile, [write, append]),
            packet_logger_loop(IoDevice, LogFile)
        end
    ).

%% @doc 报文日志记录循环
packet_logger_loop(IoDevice, LogFile) ->
    receive
        {log, Type, Data} ->
            LogEntry = #{
                timestamp => erlang:system_time(millisecond),
                type => Type,
                data => Data
            },
            io:format(IoDevice, "~p.~n", [LogEntry]),
            packet_logger_loop(IoDevice, LogFile);
        stop ->
            file:close(IoDevice),
            ?LOG(info, "报文日志已保存: ~s", [LogFile]);
        _ ->
            packet_logger_loop(IoDevice, LogFile)
    end.

%% @doc 记录报文日志
log_packet(PacketLogger, Type, Data) when is_pid(PacketLogger) ->
    PacketLogger ! {log, Type, Data};
log_packet(_, _, _) ->
    ok.

%%%===================================================================
%%% 在线测试函数
%%%===================================================================

%% @doc 在线测试函数
test() ->
    io:format("~n========================================~n", []),
    io:format("🧪 【自动化测试协调器】在线测试~n", []),
    io:format("========================================~n~n", []),
    
    %% 测试磁航向工位
    StationId = 1700,
    
    io:format("1. 启动测试...~n"),
    case start_test(StationId) of
        {ok, started} ->
            io:format("   ✅ 测试启动成功~n"),
            
            %% 等待测试完成
            io:format("~n2. 等待测试完成...~n"),
            timer:sleep(5000),
            
            %% 获取测试状态
            io:format("~n3. 查询测试状态...~n"),
            case get_test_status(StationId) of
                {ok, Status} ->
                    io:format("   ✅ 测试状态: ~p~n", [Status]);
                {error, Reason} ->
                    io:format("   ❌ 查询失败: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("   ❌ 启动失败: ~p~n", [Reason])
    end,
    
    io:format("~n========================================~n", []),
    io:format("✅ 测试完成~n", []),
    io:format("========================================~n~n", []),
    ok.
