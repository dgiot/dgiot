%%--------------------------------------------------------------------
%% @doc 治具测试流程执行模块
%%--------------------------------------------------------------------
-module(dgiot_fixture_test_executor).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav_config.hrl").
-include("dgiot_fixture_state.hrl").

%% API
-export([
    start_test/1, stop_test/1, pause_test/1, resume_test/1,
    execute_test_step/2, execute_all_steps/1,
    get_test_status/1, get_test_progress/1,
    get_test_results/1, get_test_summary/1,
    create_test_result/3, validate_test_value/2, test/0
]).

-define(STEPS, [
    {1, "备检并获取编码", required},
    {2, "机身静态测试前检查", required},
    {3, "机身及螺旋桨安装检查", required},
    {4, "电压测量检查", required},
    {5, "链路功能检查", required},
    {6, "上电参数检查", required},
    {7, "夜航灯测试", optional},
    {8, "气压高度检测", optional},
    {9, "电磁兼容性检查", optional},
    {10, "航线加载及载荷检查", optional}
]).

%%====================================================================
%% 测试流程控制
%%====================================================================
start_test(Addr) ->
    ?LOG(info, "[TEST] 启动测试流程 - 工位~p", [Addr]),
    dgiot_fixture_state_manager:set_test_state(Addr, testing),
    dgiot_fixture_state_manager:set_test_step(Addr, 1),
    spawn(fun() -> execute_all_steps(Addr) end),
    {ok, running}.

stop_test(Addr) ->
    ?LOG(info, "[TEST] 停止测试流程 - 工位~p", [Addr]),
    dgiot_fixture_state_manager:set_test_state(Addr, failed),
    dgiot_fixture_state_manager:set_test_step(Addr, 0),
    {ok, stopped}.

pause_test(Addr) ->
    dgiot_fixture_state_manager:set_test_state(Addr, paused),
    {ok, paused}.

resume_test(Addr) ->
    dgiot_fixture_state_manager:set_test_state(Addr, testing),
    spawn(fun() -> execute_all_steps(Addr) end),
    {ok, running}.

%%====================================================================
%% 测试步骤执行
%%====================================================================
execute_all_steps(Addr) ->
    case dgiot_fixture_state_manager:get_test_state(Addr) of
        {ok, testing} -> execute_steps(Addr, 1, length(?STEPS));
        _ -> ?LOG(warning, "[TEST] 工位~p未处于测试状态", [Addr])
    end.

execute_steps(_Addr, Step, Total) when Step > Total ->
    ?LOG(info, "[TEST] 所有测试步骤完成"),
    dgiot_fixture_state_manager:set_test_state(_Addr, completed);
    
execute_steps(Addr, Step, Total) ->
    case execute_test_step(Addr, Step) of
        {ok, Result} ->
            dgiot_fixture_state_manager:add_test_result(Addr, Step, Result),
            execute_steps(Addr, Step + 1, Total);
        {error, {step_failed, Step, Reason}} ->
            ?LOG(error, "[TEST] 必需步骤~p失败: ~s", [Step, Reason]),
            dgiot_fixture_state_manager:set_test_state(Addr, failed),
            {error, test_failed}
    end.

execute_test_step(Addr, Step) ->
    case lists:keyfind(Step, 1, ?STEPS) of
        {Step, Name, Required} ->
            Result = create_test_result(Step, Name, running),
            case execute_step(Addr, Step) of
                {ok, Value, Unit} ->
                    Final = Result#test_result{status = passed, value = Value, unit = Unit},
                    {ok, Final};
                {error, Reason} when Required =:= required ->
                    _Final = Result#test_result{status = failed, error_reason = Reason},
                    {error, {step_failed, Step, Reason}};
                {error, Reason} ->
                    Final1 = Result#test_result{status = failed, error_reason = Reason},
                    {ok, Final1}
            end;
        false ->
            {error, invalid_step}
    end.

execute_step(_Addr, Step) ->
    case Step of
        1 -> {ok, undefined, <<>>};
        2 -> {ok, undefined, <<>>};
        3 -> {ok, undefined, <<>>};
        4 -> {ok, undefined, <<"V">>};
        5 -> {ok, undefined, <<>>};
        6 -> {ok, undefined, <<>>};
        7 -> {ok, undefined, <<>>};
        8 -> {ok, undefined, <<"m">>};
        9 -> {ok, undefined, <<>>};
        10 -> {ok, undefined, <<>>};
        _ -> {error, <<"未知步骤">>}
    end.

%%====================================================================
%% 状态查询
%%====================================================================
get_test_status(Addr) -> dgiot_fixture_state_manager:get_test_state(Addr).

get_test_progress(Addr) ->
    Total = length(?STEPS),
    case dgiot_fixture_state_manager:get_test_step(Addr) of
        {ok, Step} -> {ok, Step, Total, Step * 100 div Total};
        {error, _} -> {error, not_found}
    end.

get_test_results(Addr) ->
    case dgiot_fixture_state_manager:get_fixture_state(Addr) of
        {ok, #fixture_state{test_results = R}} -> {ok, R};
        {error, _} -> {error, not_found}
    end.

get_test_summary(Addr) ->
    case get_test_results(Addr) of
        {ok, Results} ->
            Passed = length([R || R <- Results, R#test_result.status =:= passed]),
            Failed = length([R || R <- Results, R#test_result.status =:= failed]),
            Total = length(Results),
            {ok, #{total => Total, passed => Passed, failed => Failed,
                   progress => if Total > 0 -> Passed * 100 div Total; true -> 0 end}};
        {error, _} -> {error, not_found}
    end.

%%====================================================================
%% 工具函数
%%====================================================================
create_test_result(Step, Name, Status) ->
    #test_result{
        step = Step,
        test_name = Name,
        status = Status,
        start_time = erlang:system_time(millisecond)
    }.

validate_test_value(Value, Threshold) when is_number(Value), is_number(Threshold) ->
    Value >= Threshold;
validate_test_value(_, _) -> false.

test() -> ok.
