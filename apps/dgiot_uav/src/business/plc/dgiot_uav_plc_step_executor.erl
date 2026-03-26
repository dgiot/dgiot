%%%-------------------------------------------------------------------
%%% @doc UAV PLC 7步校验流程执行器
%%% 处理 Modbus 响应，管理重试、锁、步骤跳转
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_step_executor).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% API
-export([
    handle_step1_response/5,
    handle_step3_response/5,
    handle_write_response/8,
    handle_step_retry_action/6,
    acquire_execution_lock/1,
    release_execution_lock/1,
    reset_step_retry_count/1,
    record_command_failure/3
]).

%% 内部导出
-export([
    handle_step_retry/5,
    handle_write_success/8,
    handle_write_failure/8,
    handle_write_failure_aftermath/6
]).

%%%===================================================================
%%% 步骤响应处理
%%%===================================================================

handle_step1_response(Data, StationId, _Code, ChildState, Dclient) ->
    CmdId = maps:get(current_command_index, ChildState, 1),
    List = maps:get(command_list, ChildState, []),
    {_DeviceId, CurrentCode} = case CmdId =< length(List) of
        true -> lists:nth(CmdId, List);
        false -> {undefined, 0}
    end,
    case Data of
        #{registers := [Value]} when Value == 1 ->
            ?LOG(debug, "PLC就绪，执行步骤2"),
            NewChildState = reset_step_retry_count(ChildState),
            self() ! {step, 2, CurrentCode},
            {noreply, Dclient#dclient{child = NewChildState#{current_step_index => 2}}};
        #{registers := [Value]} ->
            ?LOG(debug, "PLC未就绪(寄存器值=~p)，等待1秒后重试", [Value]),
            handle_step_retry_action(1, StationId, CurrentCode, ChildState, Dclient, "PLC未就绪");
        _ ->
            ?LOG(warning, "读取D0响应数据格式错误，等待1秒后重试"),
            handle_step_retry_action(1, StationId, CurrentCode, ChildState, Dclient, "响应格式错误")
    end.

handle_step3_response(Data, StationId, _Code, ChildState, Dclient) ->
    CmdId = maps:get(current_command_index, ChildState, 1),
    List = maps:get(command_list, ChildState, []),
    {_DeviceId, CurrentCode} = case CmdId =< length(List) of
        true -> lists:nth(CmdId, List);
        false -> {undefined, 0}
    end,
    case Data of
        #{registers := [Value]} when Value == CurrentCode ->
            ?LOG(debug, "指令接收符合预期(期望~p)，执行步骤4", [CurrentCode]),
            NewChildState = reset_step_retry_count(ChildState),
            self() ! {step, 4, CurrentCode},
            {noreply, Dclient#dclient{child = NewChildState#{current_step_index => 4}}};
        #{registers := [Value]} ->
            ?LOG(warning, "指令接收错误(期望~p, 实际值 ~p)，等待1秒后重试步骤3", [CurrentCode, Value]),
            handle_step_retry_action(3, StationId, CurrentCode, ChildState, Dclient, "指令接收错误");
        _ ->
            ?LOG(warning, "读取D10响应数据格式错误，等待1秒后重试"),
            handle_step_retry_action(3, StationId, CurrentCode, ChildState, Dclient, "响应格式错误")
    end.

handle_write_response(Data, StationId, CmdId, StepId, Code, DeviceId, ChildState, Dclient) ->
    BaseAddress = maps:get(base_address, ChildState, dgiot_uav_plc_utils:get_base_address(StationId)),
    ExpectedAddress = BaseAddress + dgiot_uav_plc_utils:get_address_by_step(StepId),
    CurrentCode = Code,
    case Data of
        #{address := Address, value := Value} when Address == ExpectedAddress ->
            ?LOG(debug, "步骤~p写操作成功，地址=~p, 值=~p", [StepId, Address, Value]),
            NewChildState = reset_step_retry_count(ChildState),
            handle_write_success(StepId, StationId, CmdId, CurrentCode, DeviceId, ChildState, Dclient, NewChildState);
        _ ->
            ?LOG(warning, "步骤~p写操作响应异常，等待1秒后重试", [StepId]),
            handle_write_failure(StepId, StationId, CmdId, CurrentCode, DeviceId, ChildState, Dclient, "写操作响应异常")
    end.

%%%===================================================================
%%% 重试辅助
%%%===================================================================

handle_step_retry(StepId, _StationId, _Code, ChildState, Reason) ->
    MaxRetries = 150,
    CurrentRetryCount = maps:get(step_retry_count, ChildState, 0),
    if
        CurrentRetryCount < MaxRetries ->
            NewRetryCount = CurrentRetryCount + 1,
            NewState = ChildState#{step_retry_count => NewRetryCount},
            {retry, NewState, NewRetryCount};
        true ->
            ErrorReason = lists:flatten(io_lib:format("步骤~p ~ts (重试~p次)",
                [StepId, Reason, CurrentRetryCount])),
            NewState = ChildState#{
                step_retry_count => 0,
                state_machine_state => <<"step_failed">>
            },
            {failed, NewState, ErrorReason}
    end.

reset_step_retry_count(ChildState) ->
    ChildState#{step_retry_count => 0}.

handle_step_retry_action(StepId, StationId, Code, ChildState, Dclient, Reason) ->
    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "虚拟工位 ~p 收到步骤 ~p 重试动作，忽略 (原因: ~ts)", [StationId, StepId, Reason]),
            {noreply, Dclient};
        false ->
            case handle_step_retry(StepId, StationId, Code, ChildState, Reason) of
                {retry, NewState, RetryCount} ->
                    ?LOG(info, "PLC系统: 步骤~p重试 ~p/150 - ~ts, 工位:~p", [StepId, RetryCount, Reason, StationId]),
                    self() ! {step, StepId, Code},
                    {noreply, Dclient#dclient{child = NewState}};
                {failed, NewState, ErrorReason} ->
                    ?LOG(error, "PLC系统: 步骤~p失败 - ~ts, 工位:~p", [StepId, ErrorReason, StationId]),
                    UpdatedState = record_command_failure(NewState, StepId, ErrorReason),
                    self() ! {step, 7, 0},
                    {noreply, Dclient#dclient{child = UpdatedState#{current_step_index => 7}}}
            end
    end.

%%%===================================================================
%%% 成功/失败后续处理
%%%===================================================================

handle_write_success(2, _StationId, _CmdId, Code, _DeviceId, _ChildState, Dclient, NewChildState) ->
    self() ! {step, 3, Code},
    {noreply, Dclient#dclient{child = NewChildState#{current_step_index => 3}}};

handle_write_success(4, _StationId, _CmdId, Code, _DeviceId, _ChildState, Dclient, NewChildState) ->
    self() ! {step, 5, Code},
    {noreply, Dclient#dclient{child = NewChildState#{current_step_index => 5}}};

handle_write_success(5, _StationId, _CmdId, Code, _DeviceId, _ChildState, Dclient, NewChildState) ->
    self() ! {step, 6, Code},
    {noreply, Dclient#dclient{child = NewChildState#{current_step_index => 6}}};

handle_write_success(6, _StationId, _CmdId, Code, _DeviceId, ChildState, Dclient, NewChildState) ->
    ?LOG(debug, "指令完成，执行步骤7（写D61=1）"),
    TestItemId = maps:get(test_item_id, ChildState, undefined),
    StepIndex = maps:get(step_index, ChildState, undefined),
    %% 上报结果（调用主模块的 report_plc_result，需要导出或由主模块回调）
    dgiot_uav_plc_tcp_client:report_plc_result(TestItemId, StepIndex, Code, ChildState),
    UpdatedState = NewChildState#{current_step_index => 7},
    self() ! {step, 7, Code},
    {noreply, Dclient#dclient{child = UpdatedState}};

handle_write_success(7, StationId, CmdId, _Code, _DeviceId, ChildState, Dclient, NewChildState) ->
    List = maps:get(command_list, ChildState, []),
    case CmdId < length(List) of
        true ->
            NextCmdId = CmdId + 1,
            ?LOG(debug, "指令~p完成并恢复D0，开始下一条指令~p", [CmdId, NextCmdId]),
            UpdatedState = NewChildState#{
                current_command_index => NextCmdId,
                current_step_index => 1,
                command_retry_count => 0
            },
            self() ! {step, 1, 0},
            {noreply, Dclient#dclient{child = UpdatedState}};
        false ->
            ?LOG(info, "连续指令测试完成: StationId=~p", [StationId]),
            FinalChildState = release_execution_lock(NewChildState#{
                state_machine_state => <<"completed">>,
                current_step_index => 1,
                current_command_index => 1
            }),
            case maps:get(reply_to, ChildState, undefined) of
                undefined -> ok;
                From -> gen_server:reply(From, ok)
            end,
            {noreply, Dclient#dclient{child = FinalChildState#{reply_to => undefined}}}
    end.

handle_write_failure(StepId, StationId, CmdId, Code, DeviceId, ChildState, Dclient, Reason) ->
    case dgiot_uav_plc_business:is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "虚拟工位 ~p 收到步骤 ~p 写失败，忽略 (原因: ~ts)", [StationId, StepId, Reason]),
            {noreply, Dclient};
        false ->
            case handle_step_retry(StepId, StationId, Code, ChildState, Reason) of
                {retry, NewState, RetryCount} ->
                    ?LOG(info, "PLC系统: 步骤~p重试 ~p/150 - ~ts, 工位:~p", [StepId, RetryCount, Reason, StationId]),
                    self() ! {step, StepId, Code},
                    {noreply, Dclient#dclient{child = NewState}};
                {failed, NewState, ErrorReason} ->
                    ?LOG(error, "PLC系统: 步骤~p失败 - ~ts, 工位:~p", [StepId, ErrorReason, StationId]),
                    UpdatedState = record_command_failure(NewState, StepId, ErrorReason),
                    handle_write_failure_aftermath(StepId, CmdId, DeviceId, UpdatedState, Dclient, StationId)
            end
    end.

handle_write_failure_aftermath(6, CmdId, _DeviceId, UpdatedState, Dclient, _StationId) ->
    List = maps:get(command_list, UpdatedState, []),
    case CmdId < length(List) of
        true ->
            NextCmdId = CmdId + 1,
            ?LOG(warning, "指令~p失败，尝试下一个指令~p", [CmdId, NextCmdId]),
            RetryState = UpdatedState#{
                current_command_index => NextCmdId,
                current_step_index => 1
            },
            self() ! {step, 1, 0},
            {noreply, Dclient#dclient{child = RetryState}};
        false ->
            FinalChildState = release_execution_lock(UpdatedState#{state_machine_state => <<"failed">>}),
            case maps:get(reply_to, UpdatedState, undefined) of
                undefined -> ok;
                From -> gen_server:reply(From, {error, step_failed})
            end,
            {noreply, Dclient#dclient{child = FinalChildState#{reply_to => undefined}}}
    end;

handle_write_failure_aftermath(_, _, _, UpdatedState, Dclient, _) ->
    FinalChildState = release_execution_lock(UpdatedState#{state_machine_state => <<"failed">>}),
    case maps:get(reply_to, UpdatedState, undefined) of
        undefined -> ok;
        From -> gen_server:reply(From, {error, step_failed})
    end,
    {noreply, Dclient#dclient{child = FinalChildState#{reply_to => undefined}}}.

%%%===================================================================
%%% 执行锁
%%%===================================================================

acquire_execution_lock(ChildState) ->
    case maps:get(execution_lock, ChildState, false) of
        false -> {ok, ChildState#{execution_lock => true}};
        true -> {error, busy}
    end.

release_execution_lock(ChildState) ->
    ChildState#{execution_lock => false}.

%%%===================================================================
%%% 错误记录
%%%===================================================================

record_command_failure(ChildState, StepId, Reason) ->
    CmdId = maps:get(current_command_index, ChildState, 1),
    FailureRecord = #{
        timestamp => erlang:system_time(millisecond),
        command_index => CmdId,
        step_id => StepId,
        reason => Reason,
        station_id => maps:get(station_id, ChildState)
    },
    History = maps:get(state_history, ChildState, []),
    ChildState#{
        state_machine_state => <<"failed">>,
        state_history => [FailureRecord | History]
    }.