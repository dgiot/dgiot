-module(dgiot_quick_diagnosis).
-author("dgiot").
-export([
    run_all_diagnostics/0,
    diagnose_data_convergence/0,
    diagnose_command_dispatch/0,
    diagnose_criteria_execution/0,
    diagnose_report_generation/0,
    test/0
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% @doc 运行所有诊断
%% @spec run_all_diagnostics() -> #{atom() => map()}
run_all_diagnostics() ->
    ?LOG(info, "==================== 快速诊断开始 ====================", []),

    Results = #{
        timestamp => dgiot_datetime:format_timestamp(os:timestamp(), <<>>),
        data_convergence => diagnose_data_convergence(),
        command_dispatch => diagnose_command_dispatch(),
        criteria_execution => diagnose_criteria_execution(),
        report_generation => diagnose_report_generation()
    },

    print_summary(Results),
    Results.

%% @doc 诊断1：数据汇聚
%% 检查点：
%% - 治具设备是否在线
%% - 治具设备是否注册到工位
%% - 舵面数据是否正确解析
diagnose_data_convergence() ->
    ?LOG(info, "【诊断1】数据汇聚验证", []),

    Checks = [
        check_fixture_online(),
        check_fixture_station_binding(),
        check_surface_data()
    ],

    Summary = #{passed => count_passed(Checks), total => length(Checks)},
    print_diagnostic_result("数据汇聚", Checks, Summary),

    #{status => get_status(Summary), checks => Checks, summary => Summary}.

%% @doc 诊断2：指令下发
%% 检查点：
%% - PLC客户端是否在线
%% - 指令下发函数是否可调用
%% - 指令发送日志是否存在
diagnose_command_dispatch() ->
    ?LOG(info, "【诊断2】指令下发验证", []),

    Checks = [
        check_plc_client_online(1500),
        check_plc_find_function(),
        check_command_logs()
    ],

    Summary = #{passed => count_passed(Checks), total => length(Checks)},
    print_diagnostic_result("指令下发", Checks, Summary),

    #{status => get_status(Summary), checks => Checks, summary => Summary}.

%% @doc 诊断3：判据执行
%% 检查点：
%% - 测试项是否加载
%% - 判据类型是否支持
%% - 判据执行状态
diagnose_criteria_execution() ->
    ?LOG(info, "【诊断3】判据执行验证", []),

    Checks = [
        check_test_item_loaded(),
        check_criteria_config(),
        check_executor_running()
    ],

    Summary = #{passed => count_passed(Checks), total => length(Checks)},
    print_diagnostic_result("判据执行", Checks, Summary),

    #{status => get_status(Summary), checks => Checks, summary => Summary}.

%% @doc 诊断4：报告生成
%% 检查点：
%% - 测试结果是否存储
%% - Parse Server数据完整性
%% - TDengine时序数据
diagnose_report_generation() ->
    ?LOG(info, "【诊断4】报告生成验证", []),

    Checks = [
        check_parse_result(),
        check_tdengine_data(),
        check_report_logs()
    ],

    Summary = #{passed => count_passed(Checks), total => length(Checks)},
    print_diagnostic_result("报告生成", Checks, Summary),

    #{status => get_status(Summary), checks => Checks, summary => Summary}.

%%%===================================================================
%%% 数据汇聚检查函数
%%%===================================================================

%% 检查治具设备在线状态
check_fixture_online() ->
    DeviceId = <<"wrj_danpianji">>,
    Expected = <<"online">>,
    Actual = case dgiot_device:lookup(DeviceId) of
        {ok, Device} ->
            maps:get(<<"status">>, Device, <<"unknown">>);
        {error, _} ->
            <<"not_found">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  1.1 治具设备在线: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"治具设备在线状态">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%% 检查治具工位绑定状态
check_fixture_station_binding() ->
    DeviceId = <<"wrj_danpianji">>,
    Actual = case dgiot_device:lookup(DeviceId) of
        {ok, Device} ->
            Content = maps:get(<<"content">>, Device, #{}),
            maps:get(<<"station_id">>, Content, <<"not_bound">>);
        {error, _} ->
            <<"not_found">>
    end,
    Passed = (Actual =/= <<"not_bound">>) andalso (Actual =/= <<"not_found">>),
    ?LOG(info, "  1.2 工位绑定: ~p ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"治具工位绑定">>,
        expected => <<"工位ID">>,
        actual => Actual,
        passed => Passed
    }.

%% 检查舵面数据解析状态
check_surface_data() ->
    DeviceId = <<"wrj_dm_zhj">>,
    Actual = case ets:lookup(fixture_devices, DeviceId) of
        [{_, DeviceState}] ->
            case maps:find(<<"latest_data">>, DeviceState) of
                {ok, _Data} -> <<"has_data">>;
                error -> <<"no_data">>
            end;
        [] ->
            <<"not_registered">>
    end,
    Passed = Actual =:= <<"has_data">>,
    ?LOG(info, "  1.3 舵面数据: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"舵面数据解析">>,
        expected => <<"has_data">>,
        actual => Actual,
        passed => Passed
    }.

%%%===================================================================
%%% 指令下发检查函数
%%%===================================================================

%% 检查PLC客户端在线状态
check_plc_client_online(StationId) ->
    Expected = <<"online">>,
    Actual = case global:whereis_name({plc, StationId}) of
        undefined -> <<"offline">>;
        _Pid -> <<"online">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  2.1 PLC客户端(工位~p): ~s ~s", [StationId, Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"PLC客户端在线状态">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%% 检查PLC查找函数
check_plc_find_function() ->
    Expected = <<"function_ok">>,
    Actual = case dgiot_uav_auto_tester:find_plc_client_for_station(1500) of
        {ok, _Pid} -> <<"function_ok">>;
        {error, Reason} -> <<"error:", (list_to_binary(atom_to_list(Reason)))/binary>>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  2.2 find_plc_client函数: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"find_plc_client函数">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%% 检查指令发送日志
check_command_logs() ->
    LogFile = filename:join([code:priv_dir(dgiot), "..", "..", "..", "_build", "emqx", "rel", "emqx", "log", "emqx.log.1"]),
    Expected = <<"exists">>,
    Actual = case file:read_file_info(LogFile) of
        {ok, _} -> <<"exists">>;
        {error, _} -> <<"not_found">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  2.3 指令日志: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"指令发送日志">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%%%===================================================================
%%% 判据执行检查函数
%%%===================================================================

%% 检查测试项加载状态
check_test_item_loaded() ->
    DeviceId = <<"test_item_001">>,
    Actual = case dgiot_device:lookup(DeviceId) of
        {ok, Item} ->
            maps:get(<<"name">>, Item, <<"unknown">>);
        {error, _} ->
            <<"not_found">>
    end,
    Passed = (Actual =/= <<"not_found">>) andalso (Actual =/= <<"unknown">>),
    ?LOG(info, "  3.1 测试项加载: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"测试项加载">>,
        expected => <<"测试项名称">>,
        actual => Actual,
        passed => Passed
    }.

%% 检查判据配置
check_criteria_config() ->
    DeviceId = <<"test_item_001">>,
    Actual = case dgiot_device:lookup(DeviceId) of
        {ok, Item} ->
            Content = maps:get(<<"content">>, Item, #{}),
            Steps = maps:get(<<"steps">>, Content, []),
            case Steps of
                [] -> <<"no_steps">>;
                _ -> list_to_binary(integer_to_list(length(Steps)) ++ "_steps")
            end;
        {error, _} ->
            <<"not_found">>
    end,
    Passed = (Actual =/= <<"no_steps">>) andalso (Actual =/= <<"not_found">>),
    ?LOG(info, "  3.2 判据步骤: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"判据步骤配置">>,
        expected => <<"包含测试步骤">>,
        actual => Actual,
        passed => Passed
    }.

%% 检查判据执行器
check_executor_running() ->
    Expected = <<"running">>,
    Actual = case whereis(dgiot_test_executor) of
        undefined -> <<"not_running">>;
        _Pid -> <<"running">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  3.3 判据执行器: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"判据执行器进程">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%%%===================================================================
%%% 报告生成检查函数
%%%===================================================================

%% 检查Parse Server测试结果
check_parse_result() ->
    DeviceId = <<"wrj_danpianji">>,
    Expected = <<"has_result">>,
    Actual = case dgiot_device:lookup(DeviceId) of
        {ok, Device} ->
            Content = maps:get(<<"content">>, Device, #{}),
            case maps:find(<<"last_test_result">>, Content) of
                {ok, _} -> <<"has_result">>;
                error -> <<"no_result">>
            end;
        {error, _} ->
            <<"not_found">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  4.1 Parse结果: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"Parse Server测试结果">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%% 检查TDengine时序数据
check_tdengine_data() ->
    Expected = <<"has_records">>,
    %% 注意: dgiot_tdengine:query/1 不存在，需要使用正确的API
    %% TODO: 实现TDengine数据检查逻辑
    Actual = <<"check_not_implemented">>,
    ?LOG(info, "  4.2 TDengine数据: ~s ✗ (检查功能未实现)", [Actual]),
    #{
        name => <<"TDengine测试记录">>,
        expected => Expected,
        actual => Actual,
        passed => false
    }.

%% 检查报告生成日志
check_report_logs() ->
    LogFile = filename:join([code:priv_dir(dgiot), "..", "..", "..", "_build", "emqx", "rel", "emqx", "log", "emqx.log.1"]),
    Expected = <<"exists">>,
    Actual = case file:read_file_info(LogFile) of
        {ok, _} -> <<"exists">>;
        {error, _} -> <<"not_found">>
    end,
    Passed = Actual =:= Expected,
    ?LOG(info, "  4.3 报告日志: ~s ~s", [Actual, if Passed -> "✓"; true -> "✗" end]),
    #{
        name => <<"报告生成日志">>,
        expected => Expected,
        actual => Actual,
        passed => Passed
    }.

%%%===================================================================
%%% 辅助函数
%%%===================================================================

%% 统计通过的检查项
count_passed(Checks) ->
    lists:foldl(
        fun(#{passed := Passed}, Acc) ->
            case Passed of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Checks
    ).

%% 获取诊断状态
get_status(#{passed := Passed, total := Total}) ->
    case Passed of
        Total -> <<"passed">>;
        _ -> <<"failed">>
    end.

%% 打印诊断结果
print_diagnostic_result(DiagnosticName, Checks, Summary) ->
    #{passed := Passed, total := Total} = Summary,
    ?LOG(info, "【~s】结果: ~p/~p 通过", [DiagnosticName, Passed, Total]),
    lists:foreach(
        fun(#{name := Name, actual := Actual, passed := _Passed}) ->
            Status = if _Passed -> "✓"; true -> "✗" end,
            ?LOG(info, "  ~s ~s: ~p", [Status, Name, Actual])
        end,
        Checks
    ).

%% 打印诊断总结
print_summary(Results) ->
    ?LOG(info, "==================== 诊断总结 ====================", []),

    SummaryMaps = maps:fold(
        fun(DiagnosticName, Result, {PassedAcc, TotalAcc}) ->
            #{summary := #{passed := Passed, total := Total}} = Result,
            Status = maps:get(status, Result),
            ?LOG(info, "~s: ~p/~p (~s)", [DiagnosticName, Passed, Total, Status]),
            {PassedAcc + Passed, TotalAcc + Total}
        end,
        {0, 0},
        maps:without([timestamp], Results)
    ),

    {FinalPassed, FinalTotal} = SummaryMaps,
    ?LOG(info, "总计: ~p/~p 项检查通过", [FinalPassed, FinalTotal]),
    ?LOG(info, "==================================================", []).

%% @doc 测试函数
test() ->
    ?LOG(info, "开始快速诊断测试", []),
    _Results = run_all_diagnostics(),
    ?LOG(info, "诊断完成", []),
    ok.
