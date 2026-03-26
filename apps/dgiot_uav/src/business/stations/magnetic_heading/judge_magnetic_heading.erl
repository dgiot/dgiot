%%%-------------------------------------------------------------------
%%% @doc
%%% judge_magnetic_heading - 磁航向工位测试项判据模块
%%% 专门处理磁航向工位的测试项结果判定
%%% 
%%% 判据函数根据物模型指标来判断测试是否通过
%%% @end
%%%-------------------------------------------------------------------
-module(judge_magnetic_heading).

%% 判据接口
-export([
    %% 通用判据函数
    judge_test_item/3,
    judge_test_step/4,
    
    %% 磁航向特定判据
    judge_magnetic_calibration/2,
    judge_magnetic_test/2,
    judge_voltage_check/2,
    judge_system_compatibility/2,
    
    %% 工具函数
    extract_metrics_from_context/1,
    get_metric_value/2,
    is_metric_within_range/3,
    
    %% 在线调试函数（用于热编译）
    test/0
]).

%% 物模型指标定义（磁航向工位）
-define(METRIC_VOLTAGE, <<"voltage">>).          %% 电压 (V)
-define(METRIC_CURRENT, <<"current">>).          %% 电流 (A)
-define(METRIC_MAGNETIC_X, <<"magnetic_x">>).    %% 磁传感器 X 轴
-define(METRIC_MAGNETIC_Y, <<"magnetic_y">>).    %% 磁传感器 Y 轴
-define(METRIC_MAGNETIC_Z, <<"magnetic_z">>).    %% 磁传感器 Z 轴
-define(METRIC_STATUS, <<"status">>).            %% 设备状态 (0=正常, 1=异常)
-define(METRIC_TEMPERATURE, <<"temperature">>).  %% 温度 (°C)

%% 阈值定义
-define(VOLTAGE_NOMINAL, 24.0).      %% 额定电压 24V
-define(VOLTAGE_TOLERANCE, 2.0).     %% 电压容差 ±2V
-define(CURRENT_MAX, 5.0).           %% 最大电流 5A
-define(TEMPERATURE_MAX, 60.0).      %% 最高温度 60°C
-define(MAGNETIC_REFERENCE, 0.0).    %% 磁传感器参考值
-define(MAGNETIC_TOLERANCE, 50.0).   %% 磁传感器容差

-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 通用判据函数
%%%===================================================================

%% @doc 判断测试项是否通过
-spec judge_test_item(binary(), map(), list()) -> {ok, #{passed => boolean(), details => map()}}.
judge_test_item(TestItemId, Context, StepResults) ->
    ?LOG(info, "判断磁航向测试项: ~s", [TestItemId]),
    
    %% 从测试项ID推断测试类型
    TestType = infer_test_type(TestItemId),
    
    %% 调用对应的判据函数
    %% 注意：使用binary_to_list转换以避免Unicode二进制匹配问题
    case binary_to_list(TestType) of
        "磁航向校准" ->
            judge_magnetic_calibration(TestItemId, Context);
        "磁航向测试" ->
            judge_magnetic_test(TestItemId, Context);
        "电压检查" ->
            judge_voltage_check(TestItemId, Context);
        "系统兼容性" ->
            judge_system_compatibility(TestItemId, Context);
        _ ->
            %% 默认判据：检查所有步骤是否都成功
            AllPassed = lists:all(fun(#{passed := P}) -> P end, StepResults),
            Details = #{
                test_type => TestType,
                total_steps => length(StepResults),
                passed_steps => count_passed_steps(StepResults),
                step_details => StepResults
            },
            {ok, #{passed => AllPassed, details => Details}}
    end.

%% @doc 判断测试步骤是否通过
-spec judge_test_step(integer(), binary(), map(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_test_step(StepIndex, StepName, Context, Expected) ->
    ?LOG(debug, "判断磁航向测试步骤: StepIndex=~p, StepName=~s", [StepIndex, StepName]),
    
    %% 从上下文中提取物模型指标
    Metrics = extract_metrics_from_context(Context),
    
    %% 根据步骤名称判断
    %% 注意：使用binary_to_list转换以避免Unicode二进制匹配问题
    case binary_to_list(StepName) of
        "电压测量" ->
            judge_voltage_step(Metrics, Expected);
        "电流测量" ->
            judge_current_step(Metrics, Expected);
        "磁传感器校准" ->
            judge_magnetic_calibration_step(Metrics, Expected);
        "磁传感器测试" ->
            judge_magnetic_test_step(Metrics, Expected);
        "温度检测" ->
            judge_temperature_step(Metrics, Expected);
        "状态检查" ->
            judge_status_step(Metrics, Expected);
        _ ->
            %% 默认判据：检查是否有异常状态
            judge_default_step(Metrics, Expected)
    end.

%%%===================================================================
%%% 磁航向特定判据函数
%%%===================================================================

%% @doc 判断磁航向校准测试
-spec judge_magnetic_calibration(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_magnetic_calibration(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查磁传感器数据是否在合理范围内
    MagX = get_metric_value(?METRIC_MAGNETIC_X, Metrics),
    MagY = get_metric_value(?METRIC_MAGNETIC_Y, Metrics),
    MagZ = get_metric_value(?METRIC_MAGNETIC_Z, Metrics),
    
    IsMagXOk = is_metric_within_range(?METRIC_MAGNETIC_X, MagX, {?MAGNETIC_REFERENCE, ?MAGNETIC_TOLERANCE}),
    IsMagYOk = is_metric_within_range(?METRIC_MAGNETIC_Y, MagY, {?MAGNETIC_REFERENCE, ?MAGNETIC_TOLERANCE}),
    IsMagZOk = is_metric_within_range(?METRIC_MAGNETIC_Z, MagZ, {?MAGNETIC_REFERENCE, ?MAGNETIC_TOLERANCE}),
    
    AllOk = IsMagXOk andalso IsMagYOk andalso IsMagZOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"磁航向校准"/utf8>>,
        magnetic_x => MagX,
        magnetic_y => MagY,
        magnetic_z => MagZ,
        is_magnetic_x_ok => IsMagXOk,
        is_magnetic_y_ok => IsMagYOk,
        is_magnetic_z_ok => IsMagZOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断磁航向测试
-spec judge_magnetic_test(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_magnetic_test(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查电压和电流是否正常
    Voltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    Current = get_metric_value(?METRIC_CURRENT, Metrics),
    Status = get_metric_value(?METRIC_STATUS, Metrics),
    
    IsVoltageOk = is_metric_within_range(?METRIC_VOLTAGE, Voltage, 
                                        {?VOLTAGE_NOMINAL - ?VOLTAGE_TOLERANCE, 
                                         ?VOLTAGE_NOMINAL + ?VOLTAGE_TOLERANCE}),
    IsCurrentOk = Current =< ?CURRENT_MAX,
    IsStatusOk = Status =:= 0,
    
    AllOk = IsVoltageOk andalso IsCurrentOk andalso IsStatusOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"磁航向测试"/utf8>>,
        voltage => Voltage,
        current => Current,
        status => Status,
        is_voltage_ok => IsVoltageOk,
        is_current_ok => IsCurrentOk,
        is_status_ok => IsStatusOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断电压检查测试
-spec judge_voltage_check(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_voltage_check(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    Voltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    IsVoltageOk = is_metric_within_range(?METRIC_VOLTAGE, Voltage,
                                        {?VOLTAGE_NOMINAL - ?VOLTAGE_TOLERANCE,
                                         ?VOLTAGE_NOMINAL + ?VOLTAGE_TOLERANCE}),
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"电压检查"/utf8>>,
        voltage => Voltage,
        nominal_voltage => ?VOLTAGE_NOMINAL,
        tolerance => ?VOLTAGE_TOLERANCE,
        is_ok => IsVoltageOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => IsVoltageOk, details => Details}}.

%% @doc 判断系统兼容性测试
-spec judge_system_compatibility(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_system_compatibility(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查所有关键指标
    Voltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    Current = get_metric_value(?METRIC_CURRENT, Metrics),
    Temperature = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    Status = get_metric_value(?METRIC_STATUS, Metrics),
    
    IsVoltageOk = is_metric_within_range(?METRIC_VOLTAGE, Voltage,
                                        {?VOLTAGE_NOMINAL - ?VOLTAGE_TOLERANCE,
                                         ?VOLTAGE_NOMINAL + ?VOLTAGE_TOLERANCE}),
    IsCurrentOk = Current =< ?CURRENT_MAX,
    IsTemperatureOk = Temperature =< ?TEMPERATURE_MAX,
    IsStatusOk = Status =:= 0,
    
    AllOk = IsVoltageOk andalso IsCurrentOk andalso IsTemperatureOk andalso IsStatusOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"系统兼容性检查"/utf8>>,
        voltage => Voltage,
        current => Current,
        temperature => Temperature,
        status => Status,
        is_voltage_ok => IsVoltageOk,
        is_current_ok => IsCurrentOk,
        is_temperature_ok => IsTemperatureOk,
        is_status_ok => IsStatusOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%%%===================================================================
%%% 步骤级判据函数
%%%===================================================================

%% @doc 判断电压步骤
judge_voltage_step(Metrics, Expected) ->
    ExpectedVoltage = maps:get(<<"expected_voltage">>, Expected, ?VOLTAGE_NOMINAL),
    Tolerance = maps:get(<<"tolerance">>, Expected, ?VOLTAGE_TOLERANCE),
    
    ActualVoltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    IsOk = is_metric_within_range(?METRIC_VOLTAGE, ActualVoltage,
                                 {ExpectedVoltage - Tolerance, ExpectedVoltage + Tolerance}),
    
    Details = #{
        step_name => <<"电压测量"/utf8>>,
        expected_voltage => ExpectedVoltage,
        actual_voltage => ActualVoltage,
        tolerance => Tolerance,
        is_within_range => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断电流步骤
judge_current_step(Metrics, Expected) ->
    MaxCurrent = maps:get(<<"max_current">>, Expected, ?CURRENT_MAX),
    
    ActualCurrent = get_metric_value(?METRIC_CURRENT, Metrics),
    IsOk = ActualCurrent =< MaxCurrent,
    
    Details = #{
        step_name => <<"电流测量"/utf8>>,
        max_current => MaxCurrent,
        actual_current => ActualCurrent,
        is_below_max => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断磁传感器校准步骤
judge_magnetic_calibration_step(Metrics, Expected) ->
    Reference = maps:get(<<"reference">>, Expected, ?MAGNETIC_REFERENCE),
    Tolerance = maps:get(<<"tolerance">>, Expected, ?MAGNETIC_TOLERANCE),
    
    MagX = get_metric_value(?METRIC_MAGNETIC_X, Metrics),
    MagY = get_metric_value(?METRIC_MAGNETIC_Y, Metrics),
    MagZ = get_metric_value(?METRIC_MAGNETIC_Z, Metrics),
    
    IsMagXOk = is_metric_within_range(?METRIC_MAGNETIC_X, MagX, {Reference, Tolerance}),
    IsMagYOk = is_metric_within_range(?METRIC_MAGNETIC_Y, MagY, {Reference, Tolerance}),
    IsMagZOk = is_metric_within_range(?METRIC_MAGNETIC_Z, MagZ, {Reference, Tolerance}),
    
    AllOk = IsMagXOk andalso IsMagYOk andalso IsMagZOk,
    
    Details = #{
        step_name => <<"磁传感器校准"/utf8>>,
        magnetic_x => MagX,
        magnetic_y => MagY,
        magnetic_z => MagZ,
        is_magnetic_x_ok => IsMagXOk,
        is_magnetic_y_ok => IsMagYOk,
        is_magnetic_z_ok => IsMagZOk,
        all_ok => AllOk
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断磁传感器测试步骤
judge_magnetic_test_step(Metrics, Expected) ->
    MinValue = maps:get(<<"min_value">>, Expected, -1000.0),
    MaxValue = maps:get(<<"max_value">>, Expected, 1000.0),
    
    MagX = get_metric_value(?METRIC_MAGNETIC_X, Metrics),
    MagY = get_metric_value(?METRIC_MAGNETIC_Y, Metrics),
    MagZ = get_metric_value(?METRIC_MAGNETIC_Z, Metrics),
    
    IsMagXOk = (MagX >= MinValue) andalso (MagX =< MaxValue),
    IsMagYOk = (MagY >= MinValue) andalso (MagY =< MaxValue),
    IsMagZOk = (MagZ >= MinValue) andalso (MagZ =< MaxValue),
    
    AllOk = IsMagXOk andalso IsMagYOk andalso IsMagZOk,
    
    Details = #{
        step_name => <<"磁传感器测试"/utf8>>,
        magnetic_x => MagX,
        magnetic_y => MagY,
        magnetic_z => MagZ,
        range => #{min => MinValue, max => MaxValue},
        is_magnetic_x_ok => IsMagXOk,
        is_magnetic_y_ok => IsMagYOk,
        is_magnetic_z_ok => IsMagZOk
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断温度步骤
judge_temperature_step(Metrics, Expected) ->
    MaxTemperature = maps:get(<<"max_temperature">>, Expected, ?TEMPERATURE_MAX),
    
    ActualTemperature = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    IsOk = ActualTemperature =< MaxTemperature,
    
    Details = #{
        step_name => <<"温度检测"/utf8>>,
        max_temperature => MaxTemperature,
        actual_temperature => ActualTemperature,
        is_below_max => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断状态步骤
judge_status_step(Metrics, Expected) ->
    ExpectedStatus = maps:get(<<"expected_status">>, Expected, 0),
    
    ActualStatus = get_metric_value(?METRIC_STATUS, Metrics),
    IsOk = ActualStatus =:= ExpectedStatus,
    
    Details = #{
        step_name => <<"状态检查"/utf8>>,
        expected_status => ExpectedStatus,
        actual_status => ActualStatus,
        is_match => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 默认步骤判据
judge_default_step(Metrics, _Expected) ->
    Status = get_metric_value(?METRIC_STATUS, Metrics),
    IsOk = Status =:= 0,
    
    Details = #{
        step_name => <<"默认检查"/utf8>>,
        status => Status,
        is_ok => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%%%===================================================================
%%% 工具函数
%%%===================================================================

%% @doc 从上下文中提取物模型指标
-spec extract_metrics_from_context(map()) -> map().
extract_metrics_from_context(Context) ->
    maps:get(<<"metrics">>, Context, #{}).

%% @doc 获取物模型指标值
-spec get_metric_value(binary(), map()) -> number().
get_metric_value(MetricKey, Metrics) ->
    case maps:get(MetricKey, Metrics, undefined) of
        undefined ->
            %% 返回默认值
            default_metric_value(MetricKey);
        Value when is_number(Value) ->
            Value;
        Value when is_binary(Value) ->
            try binary_to_float(Value) catch _:_ -> binary_to_integer(Value) end;
        _ ->
            default_metric_value(MetricKey)
    end.

%% @doc 检查指标值是否在范围内
-spec is_metric_within_range(binary(), number(), {number(), number()}) -> boolean().
is_metric_within_range(_MetricKey, Value, {Min, Tolerance}) when is_number(Tolerance) ->
    %% 使用容差模式
    (Value >= (Min - Tolerance)) andalso (Value =< (Min + Tolerance));
is_metric_within_range(_MetricKey, Value, {Min, Max}) ->
    %% 使用范围模式
    (Value >= Min) andalso (Value =< Max).

%% @doc 从测试项ID推断测试类型
-spec infer_test_type(binary()) -> binary().
infer_test_type(TestItemId) ->
    case binary:match(TestItemId, <<"校准">>) of
        {_, _} -> <<"磁航向校准"/utf8>>;
        nomatch ->
            case binary:match(TestItemId, <<"电压">>) of
                {_, _} -> <<"电压检查"/utf8>>;
                nomatch ->
                    case binary:match(TestItemId, <<"兼容">>) of
                        {_, _} -> <<"系统兼容性"/utf8>>;
                        nomatch -> <<"磁航向测试"/utf8>>
                    end
            end
    end.

%% @doc 统计通过的步骤数
-spec count_passed_steps(list()) -> integer().
count_passed_steps(StepResults) ->
    lists:foldl(fun(#{passed := P}, Acc) -> 
                   case P of true -> Acc + 1; false -> Acc end 
                end, 0, StepResults).

%% @doc 获取指标默认值
-spec default_metric_value(binary()) -> number().
default_metric_value(?METRIC_VOLTAGE) -> ?VOLTAGE_NOMINAL;
default_metric_value(?METRIC_CURRENT) -> 0.0;
default_metric_value(?METRIC_MAGNETIC_X) -> ?MAGNETIC_REFERENCE;
default_metric_value(?METRIC_MAGNETIC_Y) -> ?MAGNETIC_REFERENCE;
default_metric_value(?METRIC_MAGNETIC_Z) -> ?MAGNETIC_REFERENCE;
default_metric_value(?METRIC_STATUS) -> 0;
default_metric_value(?METRIC_TEMPERATURE) -> 25.0;
default_metric_value(_) -> 0.0.

%%%===================================================================
%%% 在线调试函数
%%%===================================================================

%% @doc 测试判据模块
-spec test() -> ok.
test() ->
    io:format("~n========== 磁航向判据模块测试 ==========~n", []),
    
    %% 测试1: 电压检查判据
    TestContext1 = #{<<"metrics">> => #{
        <<"voltage">> => 24.5,
        <<"current">> => 1.2,
        <<"status">> => 0
    }},
    
    case judge_voltage_check(<<"test_voltage_001">>, TestContext1) of
        {ok, #{passed := true, details := Details1}} ->
            io:format("✓ 电压检查测试通过: ~p~n", [Details1]);
        {ok, #{passed := false, details := Details1}} ->
            io:format("✗ 电压检查测试失败: ~p~n", [Details1]);
        Error1 ->
            io:format("✗ 电压检查测试错误: ~p~n", [Error1])
    end,
    
    %% 测试2: 磁传感器校准判据
    TestContext2 = #{<<"metrics">> => #{
        <<"magnetic_x">> => 5.2,
        <<"magnetic_y">> => -3.8,
        <<"magnetic_z">> => 15.6
    }},
    
    case judge_magnetic_calibration(<<"test_calibration_001">>, TestContext2) of
        {ok, #{passed := true, details := Details2}} ->
            io:format("✓ 磁传感器校准测试通过: ~p~n", [Details2]);
        {ok, #{passed := false, details := Details2}} ->
            io:format("✗ 磁传感器校准测试失败: ~p~n", [Details2]);
        Error2 ->
            io:format("✗ 磁传感器校准测试错误: ~p~n", [Error2])
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.