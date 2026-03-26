%%%-------------------------------------------------------------------
%%% @doc 无人机测试执行器（统一版本）
%%% 负责执行测试流程，不包含加载和存储逻辑
%%%-------------------------------------------------------------------
-module(dgiot_uav_test_executor).
-author("johnliu").

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% 内部记录定义（避免跨模块记录引用问题）
-record(test_item, {
    id = <<>>,
    name = <<>>,
    station_id = 0,
    station_name = <<>>,
    steps = [],
    order = 0
}).

%% API
-export([
    start/2,
    start/3,
    stop/1,
    pause/1,
    resume/1,
    get_progress/1,
    get_status/1,
    execute_step/3,
    test/0
]).

%% 内部记录
-record(execution, {
    test_id :: binary(),
    test_item_id :: binary(),
    device_id :: binary(),
    station_id :: integer(),
    steps = [] :: list(),
    current_step = 0 :: integer(),
    status = pending :: pending | running | paused | completed | failed,
    start_time :: integer() | undefined,
    end_time :: integer() | undefined,
    step_results = [] :: list(),
    context = #{} :: map()
}).

%% ETS表配置
-define(EXECUTION_TABLE, execution_table).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动测试执行
-spec start(binary(), integer()) -> {ok, binary()} | {error, term()}.
start(TestItemId, StationId) ->
    start(TestItemId, StationId, #{}).

-spec start(binary(), integer(), map()) -> {ok, binary()} | {error, term()}.
start(TestItemId, StationId, Options) ->
    ?LOG(info, "[EXEC] 启动测试 - TestItemId:~s, StationId:~p", [TestItemId, StationId]),
    
    %% 1. 加载测试项
    case dgiot_uav_test_loader:load(TestItemId) of
        {ok, TestItem} ->
            %% 2. 创建执行记录
            TestId = generate_test_id(),
            Execution = #execution{
                test_id = TestId,
                test_item_id = TestItemId,
                device_id = maps:get(device_id, Options, TestItemId),
                station_id = StationId,
                steps = get_steps_from_test_item(TestItem),
                status = running,
                start_time = erlang:system_time(millisecond),
                context = maps:get(context, Options, #{})
            },
            
            %% 3. 存储执行记录
            store_execution(Execution),
            
            %% 4. 异步执行
            spawn(fun() -> execute_loop(Execution) end),
            
            ?LOG(info, "[EXEC] 测试启动成功 - TestId:~s", [TestId]),
            {ok, TestId};
        {error, Reason} ->
            ?LOG(error, "[EXEC] 加载测试项失败 - ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 停止测试
-spec stop(binary()) -> ok | {error, term()}.
stop(TestId) ->
    case get_execution(TestId) of
        {ok, Execution} ->
            Updated = Execution#execution{
                status = completed,
                end_time = erlang:system_time(millisecond)
            },
            store_execution(Updated),
            ?LOG(info, "[EXEC] 测试停止 - TestId:~s", [TestId]),
            ok;
        {error, not_found} ->
            {error, test_not_found}
    end.

%% @doc 暂停测试
-spec pause(binary()) -> ok | {error, term()}.
pause(TestId) ->
    case get_execution(TestId) of
        {ok, Execution = #execution{status = running}} ->
            Updated = Execution#execution{status = paused},
            store_execution(Updated),
            ?LOG(info, "[EXEC] 测试暂停 - TestId:~s", [TestId]),
            ok;
        {ok, _} ->
            {error, not_running};
        {error, not_found} ->
            {error, test_not_found}
    end.

%% @doc 恢复测试
-spec resume(binary()) -> ok | {error, term()}.
resume(TestId) ->
    case get_execution(TestId) of
        {ok, Execution = #execution{status = paused, current_step = Step}} ->
            Updated = Execution#execution{status = running},
            store_execution(Updated),
            spawn(fun() -> continue_loop(Updated, Step) end),
            ?LOG(info, "[EXEC] 测试恢复 - TestId:~s", [TestId]),
            ok;
        {ok, _} ->
            {error, not_paused};
        {error, not_found} ->
            {error, test_not_found}
    end.

%% @doc 获取测试进度
-spec get_progress(binary()) -> {ok, map()} | {error, term()}.
get_progress(TestId) ->
    case get_execution(TestId) of
        {ok, #execution{
            test_id = Id,
            status = Status,
            current_step = Current,
            steps = Steps,
            start_time = Start,
            end_time = End
        }} ->
            Progress = #{
                test_id => Id,
                status => Status,
                current_step => Current,
                total_steps => length(Steps),
                progress_percent => if length(Steps) > 0 -> Current * 100 div length(Steps); true -> 0 end,
                start_time => Start,
                end_time => End
            },
            {ok, Progress};
        {error, not_found} ->
            {error, test_not_found}
    end.

%% @doc 获取测试状态
-spec get_status(binary()) -> {ok, map()} | {error, term()}.
get_status(TestId) ->
    case get_execution(TestId) of
        {ok, #execution{
            status = Status,
            current_step = Current,
            step_results = Results
        }} ->
            {ok, #{status => Status, current_step => Current, results => Results}};
        {error, not_found} ->
            {error, test_not_found}
    end.

%% @doc 手动执行单个步骤（用于调试）
-spec execute_step(binary(), integer(), map()) -> {ok, map()} | {error, term()}.
execute_step(TestId, StepIndex, Context) ->
    case get_execution(TestId) of
        {ok, Execution = #execution{steps = Steps, status = running}} ->
            try
                Step = lists:nth(StepIndex + 1, Steps),
                execute_single_step(Execution, Step, StepIndex, Context)
            catch
                error:badarg ->
                    {error, step_index_out_of_range}
            end;
        {ok, _} ->
            {error, not_running};
        {error, not_found} ->
            {error, test_not_found}
    end.

%%====================================================================
%% 内部执行函数
%%====================================================================

%% 执行循环
execute_loop(Execution = #execution{steps = Steps, current_step = StartStep}) ->
    execute_loop(Execution, StartStep, Steps, []).

execute_loop(_Execution, _Step, [], _Results) ->
    ok;
execute_loop(#execution{test_id = TestId, status = Status}, _Step, _Steps, _Results)
        when Status =:= paused; Status =:= completed; Status =:= failed ->
    ?LOG(debug, "[EXEC] 测试已停止或暂停 - TestId:~s, Status:~p", [TestId, Status]),
    ok;
execute_loop(Execution = #execution{test_id = TestId, context = Context, steps = Steps}, Step, Steps, Results) ->
    %% 获取当前步骤
    CurrentStep = lists:nth(Step + 1, Steps),
    
    %% 更新当前步骤
    update_current_step(TestId, Step),
    
    %% 执行步骤
    case execute_single_step(Execution, CurrentStep, Step, Context) of
        {ok, NewContext, StepResult} ->
            NewResults = Results ++ [StepResult],
            NextStep = Step + 1,
            
            %% 检查是否完成
            case NextStep >= length(Steps) of
                true ->
                    finish_test(TestId, NewResults, NewContext);
                false ->
                    Updated = Execution#execution{
                        current_step = NextStep,
                        context = NewContext,
                        step_results = NewResults
                    },
                    store_execution(Updated),
                    execute_loop(Updated, NextStep, Steps, NewResults)
            end;
        {error, Reason, StepResult} ->
            %% 步骤失败
            NewResults = Results ++ [StepResult],
            fail_test(TestId, Reason, NewResults, Context)
    end.

%% 继续执行（恢复暂停后）
continue_loop(Execution = #execution{steps = Steps}, StartStep) ->
    execute_loop(Execution, StartStep, Steps, Execution#execution.step_results).

%% 执行单个步骤
execute_single_step(Execution, Step, StepIndex, Context) ->
    ActionType = maps:get(<<"action_type">>, Step, maps:get(<<"type">>, Step, undefined)),
    
    case ActionType of
        <<"send">> ->
            execute_send_step(Execution, Step, StepIndex, Context);
        <<"judge">> ->
            execute_judge_step(Execution, Step, StepIndex, Context);
        <<"receive_data">> ->
            execute_receive_step(Execution, Step, StepIndex, Context);
        <<"wait">> ->
            execute_wait_step(Execution, Step, StepIndex, Context);
        _ ->
            {error, {unknown_action, ActionType}, create_step_result(StepIndex, Step, failed, Context)}
    end.

%% 发送步骤
execute_send_step(#execution{station_id = StationId}, Step, StepIndex, Context) ->
    Target = maps:get(<<"target">>, Step, <<"plc">>),
    SendMap = maps:get(<<"send">>, Step, #{}),
    ValueStr = maps:get(<<"content">>, SendMap, maps:get(<<"value">>, SendMap, <<"0">>)),
    
    try binary_to_integer(ValueStr) of
        Value ->
            case send_command(Target, StationId, Value) of
                {ok, Response} ->
                    NewContext = Context#{<<"last_command">> => Value, <<"response">> => Response},
                    StepResult = create_step_result(StepIndex, Step, passed, NewContext),
                    {ok, NewContext, StepResult};
                {error, Reason} ->
                    StepResult = create_step_result(StepIndex, Step, failed, Context, Reason),
                    {error, Reason, StepResult}
            end
    catch
        error:badarg ->
            StepResult = create_step_result(StepIndex, Step, failed, Context, {invalid_value, ValueStr}),
            {error, {invalid_value, ValueStr}, StepResult}
    end.

%% 判据步骤
execute_judge_step(#execution{test_id = TestId, station_id = StationId}, Step, StepIndex, Context) ->
    StepName = maps:get(<<"step_name">>, Step, <<"unknown">>),
    Expected = maps:get(<<"expected">>, Step, #{}),
    JudgeRule = maps:get(<<"judge_rule">>, Step, #{}),
    JudgeType = maps:get(<<"type">>, JudgeRule, <<"simple">>),
    
    %% 构建判据上下文
    JudgeContext = Context#{
        <<"step_name">> => StepName,
        <<"station_id">> => StationId,
        <<"test_id">> => TestId,
        <<"metrics">> => maps:get(<<"metrics">>, Context, #{})
    },
    
    case dgiot_uav_judge_engine:evaluate(JudgeType, JudgeRule, Expected, JudgeContext) of
        {ok, #{result := pass, details := Details}} ->
            StepResult = create_step_result(StepIndex, Step, passed, Context, Details),
            {ok, Context, StepResult};
        {ok, #{result := fail, details := Details}} ->
            StepResult = create_step_result(StepIndex, Step, failed, Context, Details),
            {error, {judge_failed, Details}, StepResult};
        {error, Reason} ->
            StepResult = create_step_result(StepIndex, Step, failed, Context, Reason),
            {error, {judge_error, Reason}, StepResult}
    end.

%% 接收步骤
execute_receive_step(_Execution, Step, StepIndex, Context) ->
    WaitTime = maps:get(<<"wait">>, Step, 1000),
    Metrics = maps:get(<<"metrics">>, Step, []),
    
    timer:sleep(WaitTime),
    
    %% 从物模型获取数据（简化实现）
    ReceivedData = receive_metrics(Metrics, Context),
    NewContext = Context#{<<"received_data">> => ReceivedData},
    StepResult = create_step_result(StepIndex, Step, passed, NewContext),
    {ok, NewContext, StepResult}.

%% 等待步骤
execute_wait_step(_Execution, Step, StepIndex, Context) ->
    Delay = maps:get(<<"wait">>, Step, 1000),
    timer:sleep(Delay),
    StepResult = create_step_result(StepIndex, Step, passed, Context),
    {ok, Context, StepResult}.

%%====================================================================
%% 命令发送
%%====================================================================
send_command(<<"plc">>, StationId, Value) ->
    Addr = station_base_address(StationId),
    dgiot_uav_simple_plc_command:send_command(StationId, Addr, Value, <<"auto">>, 0);
send_command(<<"fixture">>, StationId, Value) ->
    dgiot_uav_command_scheduler:send_command(
        <<"D", (integer_to_binary(StationId))/binary>>, 10006, Value, 0, <<"auto">>, 0);
send_command(<<"uav">>, StationId, Value) ->
    dgiot_uav_command_scheduler:send_command(
        <<"D", (integer_to_binary(StationId))/binary>>, 10007, Value, 0, <<"auto">>, 0);
send_command(_, _, _) ->
    {ok, #{status => sent}}.

station_base_address(1700) -> 1700;  % 磁航向
station_base_address(1500) -> 1500;  % 总测1
station_base_address(1600) -> 1600;  % 总测2
station_base_address(1200) -> 1200;  % 拷机1
station_base_address(1300) -> 1300;  % 拷机2
station_base_address(1100) -> 1100;  % 桁架
station_base_address(_) -> 0.

receive_metrics(Metrics, Context) ->
    %% 简化实现：从上下文中获取
    maps:fold(fun(Metric, _Acc, Map) ->
        Value = maps:get(Metric, Context, undefined),
        Map#{Metric => Value}
    end, #{}, Metrics).

%%====================================================================
%% 辅助函数
%%====================================================================
get_steps_from_test_item(TestItem) ->
    case TestItem of
        #test_item{steps = Steps} -> Steps;
        #{steps := Steps} -> Steps;
        _ -> []
    end.

generate_test_id() ->
    Timestamp = erlang:system_time(millisecond),
    Rand = rand:uniform(9999),
    list_to_binary(io_lib:format("test_~p_~4..0B", [Timestamp, Rand])).

update_current_step(TestId, Step) ->
    case get_execution(TestId) of
        {ok, Execution} ->
            Updated = Execution#execution{current_step = Step},
            store_execution(Updated);
        _ -> ok
    end.

create_step_result(StepIndex, Step, Status, Context) ->
    create_step_result(StepIndex, Step, Status, Context, undefined).

create_step_result(StepIndex, Step, Status, Context, Details) ->
    #{
        step_index => StepIndex,
        step_name => maps:get(<<"step_name">>, Step, maps:get(<<"name">>, Step, <<"unknown">>)),
        action_type => maps:get(<<"action_type">>, Step, maps:get(<<"type">>, Step, <<"unknown">>)),
        status => Status,
        timestamp => erlang:system_time(millisecond),
        context => maps:with([<<"last_command">>, <<"received_data">>, <<"judge_details">>], Context),
        details => Details
    }.

finish_test(TestId, Results, Context) ->
    case get_execution(TestId) of
        {ok, Execution} ->
            Updated = Execution#execution{
                status = completed,
                end_time = erlang:system_time(millisecond),
                step_results = Results,
                context = Context
            },
            store_execution(Updated),
            
            %% 保存测试结果
            dgiot_uav_test_storage:save(Updated),
            
            ?LOG(info, "[EXEC] 测试完成 - TestId:~s, 步骤数:~p", [TestId, length(Results)]),
            ok;
        _ -> ok
    end.

fail_test(TestId, Reason, Results, Context) ->
    case get_execution(TestId) of
        {ok, Execution} ->
            Updated = Execution#execution{
                status = failed,
                end_time = erlang:system_time(millisecond),
                step_results = Results,
                context = Context
            },
            store_execution(Updated),
            
            ?LOG(warning, "[EXEC] 测试失败 - TestId:~s, Reason:~p", [TestId, Reason]),
            ok;
        _ -> ok
    end.

%%====================================================================
%% 存储管理
%%====================================================================
store_execution(Execution) ->
    put({execution, Execution#execution.test_id}, Execution).

get_execution(TestId) ->
    case get({execution, TestId}) of
        undefined -> {error, not_found};
        Exec -> {ok, Exec}
    end.

%%====================================================================
%% 测试函数
%%====================================================================
-spec test() -> ok.
test() ->
    io:format("~n========== 测试执行器测试 ==========~n", []),
    
    %% 测试加载磁航向测试项
    case dgiot_uav_test_loader:load_by_station(<<"磁航向"/utf8>>) of
        {ok, Items} when length(Items) > 0 ->
            TestItem = hd(Items),
            TestItemId = case TestItem of
                #test_item{id = Id} -> Id;
                #{<<"objectId">> := Id} -> Id;
                _ -> <<>>
            end,
            
            io:format("测试项ID: ~s~n", [TestItemId]),
            
            %% 启动测试
            case start(TestItemId, 1700) of
                {ok, TestId} ->
                    io:format("✓ 测试启动成功: ~s~n", [TestId]),
                    timer:sleep(2000),
                    
                    case get_progress(TestId) of
                        {ok, Progress} ->
                            io:format("测试进度: ~p~n", [Progress]);
                        {error, Reason} ->
                            io:format("获取进度失败: ~p~n", [Reason])
                    end;
                {error, Reason} ->
                    io:format("✗ 测试启动失败: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("加载测试项失败: ~p~n", [Reason])
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.
