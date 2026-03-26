%%--------------------------------------------------------------------
%% @doc 治具状态管理模块
%%
%% 负责管理治具单片机的各种状态，包括：
%% - 电源状态（大继电器、无人机电源）
%% - 测试状态（未开始、进行中、完成、失败）
%% - 当前测试步骤
%% - 测试结果数据
%%
%% @end
%%--------------------------------------------------------------------
-module(dgiot_fixture_state_manager).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav_config.hrl").
-include("dgiot_fixture_state.hrl").

%% API
-export([
    %% 状态查询
    get_power_state/1,
    get_drone_power_state/1,
    get_test_state/1,
    get_test_step/1,
    get_test_result/2,
    get_communication_state/1,

    %% 状态更新
    set_power_state/2,
    set_drone_power_state/2,
    set_test_state/2,
    set_test_step/2,
    set_communication_state/2,
    add_test_result/3,

    %% 定时器管理
    start_communication_check/2,
    stop_communication_check/1,

    %% 状态重置
    reset_fixture_state/1,

    %% 完整状态查询
    get_fixture_state/1,
    init_ets_table/0
]).

%%%===================================================================
%%% 状态查询
%%%===================================================================
%%%===================================================================

%% @doc 获取治具电源状态
get_power_state(StationAddr) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            {ok, State#fixture_state.power_relay};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取无人机电源状态
get_drone_power_state(StationAddr) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            {ok, State#fixture_state.drone_power};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取测试状态
get_test_state(StationAddr) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            {ok, State#fixture_state.test_state};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取当前测试步骤
get_test_step(StationAddr) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            {ok, State#fixture_state.test_step};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取测试结果
get_test_result(StationAddr, Step) ->
    case get_fixture_state(StationAddr) of
        {ok, #fixture_state{test_results = Results}} ->
            case lists:keyfind(Step, #test_result.step, Results) of
                false ->
                    {error, not_found};
                Result ->
                    {ok, Result}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取通讯状态
get_communication_state(StationAddr) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            {ok, State#fixture_state.comm_state};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 获取完整的治具状态
get_fixture_state(StationAddr) ->
    case ets:lookup(?FIXTURE_STATE_TABLE, StationAddr) of
        [#fixture_state{} = State] ->
            {ok, State};
        [] ->
            {error, not_found}
    end.

%%%===================================================================
%%% 状态更新
%%%===================================================================

%% @doc 设置治具电源状态
set_power_state(StationAddr, PowerState) when PowerState =:= on orelse PowerState =:= off ->
    update_fixture_state(StationAddr, fun(State) ->
        State#fixture_state{power_relay = PowerState}
    end).

%% @doc 设置无人机电源状态
set_drone_power_state(StationAddr, PowerState) when PowerState =:= on orelse PowerState =:= off ->
    update_fixture_state(StationAddr, fun(State) ->
        State#fixture_state{drone_power = PowerState}
    end).

%% @doc 设置测试状态
set_test_state(StationAddr, TestState) when TestState =:= not_started orelse
                                            TestState =:= testing orelse
                                            TestState =:= completed orelse
                                            TestState =:= failed ->
    update_fixture_state(StationAddr, fun(State) ->
        State#fixture_state{test_state = TestState}
    end).

%% @doc 设置当前测试步骤
set_test_step(StationAddr, Step) when is_integer(Step), Step >= 1, Step =< 7 ->
    update_fixture_state(StationAddr, fun(State) ->
        State#fixture_state{test_step = Step}
    end).

%% @doc 添加测试结果
add_test_result(StationAddr, Step, Result) when is_record(Result, test_result) ->
    update_fixture_state(StationAddr, fun(State) ->
        #fixture_state{test_results = Results} = State,
        %% 移除旧的结果（如果有）
        NewResults = lists:keydelete(Step, #test_result.step, Results),
        State#fixture_state{test_results = [Result | NewResults]}
    end).

%% @doc 设置通讯状态
set_communication_state(StationAddr, CommState) when CommState =:= online orelse CommState =:= offline ->
    update_fixture_state(StationAddr, fun(State) ->
        State#fixture_state{comm_state = CommState}
    end).

%%%===================================================================
%%% 定时器管理
%%%===================================================================

%% @doc 启动通讯检测定时器（定时间隔1.5秒）
start_communication_check(StationAddr, SocketPid) ->
    ?LOG(info, "【FIXTURE_STATE】启动通讯检测定时器 - 工位: ~p, 进程: ~p, 间隔: 1500ms", [StationAddr, SocketPid]),
    TimerRef = erlang:send_after(1500, SocketPid, {communication_check, StationAddr}),
    update_fixture_state(StationAddr, fun(State) ->
        %% 先停止旧定时器（如果存在）
        case State#fixture_state.comm_check_timer of
            undefined -> ok;
            OldTimer -> erlang:cancel_timer(OldTimer)
        end,
        State#fixture_state{comm_check_timer = TimerRef}
    end),
    {ok, TimerRef}.

%% @doc 停止通讯检测定时器
stop_communication_check(StationAddr) ->
    ?LOG(info, "【FIXTURE_STATE】停止通讯检测定时器 - 工位: ~p", [StationAddr]),
    update_fixture_state(StationAddr, fun(State) ->
        case State#fixture_state.comm_check_timer of
            undefined -> ok;
            OldTimer ->
                erlang:cancel_timer(OldTimer),
                ?LOG(debug, "【FIXTURE_STATE】已取消通讯检测定时器 - 工位: ~p", [StationAddr])
        end,
        State#fixture_state{comm_check_timer = undefined}
    end),
    ok.

%%%===================================================================
%%% 状态重置
%%%===================================================================

%% @doc 重置治具状态
reset_fixture_state(StationAddr) ->
    %% 先停止通讯检测定时器
    stop_communication_check(StationAddr),
    InitialState = #fixture_state{
        station_addr = StationAddr,
        power_relay = off,
        drone_power = off,
        test_state = not_started,
        test_step = 0,
        test_results = [],
        start_time = undefined,
        end_time = undefined,
        comm_state = offline,
        comm_check_timer = undefined
    },
    ets:insert(?FIXTURE_STATE_TABLE, InitialState),
    ?LOG(info, "【FIXTURE_STATE】治具状态已重置 - 工位: ~p", [StationAddr]),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 更新治具状态
update_fixture_state(StationAddr, UpdateFun) ->
    case get_fixture_state(StationAddr) of
        {ok, State} ->
            NewState = UpdateFun(State),
            ets:insert(?FIXTURE_STATE_TABLE, NewState),
            ok;
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc 初始化ETS表
init_ets_table() ->
    try
        ets:new(?FIXTURE_STATE_TABLE, [
            named_table,
            set,
            {keypos, #fixture_state.station_addr},
            public,
            {read_concurrency, true},
            {write_concurrency, true}
        ]),
        ?LOG(info, "【FIXTURE_STATE】ETS表初始化成功: ~p", [?FIXTURE_STATE_TABLE]),
        ok
    catch
        error:badarg ->
            ?LOG(warning, "【FIXTURE_STATE】ETS表已存在: ~p", [?FIXTURE_STATE_TABLE]),
            ok
    end.
