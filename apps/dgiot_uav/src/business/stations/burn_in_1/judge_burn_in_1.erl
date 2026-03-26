%%%-------------------------------------------------------------------
%%% @doc
%%% judge_burn_in_1 - 拷机1工位测试项判据模块
%%% 专门处理拷机1工位的测试项结果判定
%%% 
%%% 拷机工位主要测试长时间运行稳定性和温升
%%% @end
%%%-------------------------------------------------------------------
-module(judge_burn_in_1).

%% 判据接口
-export([
    %% 通用判据函数
    judge_test_item/3,
    judge_test_step/4,
    
    %% 拷机1特定判据
    judge_burn_in_stability/2,
    judge_temperature_rise/2,
    judge_power_consumption/2,
    judge_runtime_test/2,
    
    %% 工具函数
    extract_metrics_from_context/1,
    get_metric_value/2,
    is_metric_within_range/3,
    
    %% 在线调试函数（用于热编译）
    test/0
]).

%% 物模型指标定义（拷机1工位）
-define(METRIC_VOLTAGE, <<"voltage">>).          %% 电压 (V)
-define(METRIC_CURRENT, <<"current">>).          %% 电流 (A)
-define(METRIC_TEMPERATURE, <<"temperature">>).  %% 温度 (°C)
-define(METRIC_HUMIDITY, <<"humidity">>).        %% 湿度 (%)
-define(METRIC_POWER, <<"power">>).              %% 功率 (W)
-define(METRIC_RUNTIME, <<"runtime">>).          %% 运行时间 (小时)
-define(METRIC_STATUS, <<"status">>).            %% 设备状态 (0=正常, 1=异常)
-define(METRIC_ERROR_COUNT, <<"error_count">>).  %% 错误计数

%% 阈值定义
-define(VOLTAGE_NOMINAL, 24.0).      %% 额定电压 24V
-define(VOLTAGE_TOLERANCE, 2.0).     %% 电压容差 ±2V
-define(CURRENT_MAX, 10.0).          %% 最大电流 10A (拷机工位电流较大)
-define(TEMPERATURE_MAX, 85.0).      %% 最高温度 85°C (拷机工位温度较高)
-define(TEMPERATURE_RISE_MAX, 40.0). %% 最大温升 40°C
-define(HUMIDITY_MAX, 80.0).         %% 最高湿度 80%
-define(POWER_MAX, 240.0).           %% 最大功率 240W
-define(RUNTIME_MIN, 24.0).          %% 最小运行时间 24小时
-define(ERROR_COUNT_MAX, 3).         %% 最大错误计数

-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 通用判据函数
%%%===================================================================

%% @doc 判断测试项是否通过
-spec judge_test_item(binary(), map(), list()) -> {ok, #{passed => boolean(), details => map()}}.
judge_test_item(TestItemId, Context, StepResults) ->
    ?LOG(info, "判断拷机1测试项: ~s", [TestItemId]),
    
    %% 从测试项ID推断测试类型
    TestType = infer_test_type(TestItemId),
    
    %% 调用对应的判据函数
    %% 注意：使用二进制匹配以避免编译器误解
    case TestType of
        <<"拷机稳定性测试"/utf8>> ->
            judge_burn_in_stability(TestItemId, Context);
        <<"温升测试"/utf8>> ->
            judge_temperature_rise(TestItemId, Context);
        <<"功耗测试"/utf8>> ->
            judge_power_consumption(TestItemId, Context);
        <<"运行时间测试"/utf8>> ->
            judge_runtime_test(TestItemId, Context);
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
    ?LOG(debug, "判断拷机1测试步骤: StepIndex=~p, StepName=~s", [StepIndex, StepName]),
    
    %% 从上下文中提取物模型指标
    Metrics = extract_metrics_from_context(Context),
    
    %% 根据步骤名称判断
    %% 注意：使用二进制匹配以避免Unicode二进制匹配问题
    case StepName of
        <<"电压测量"/utf8>> ->
            judge_voltage_step(Metrics, Expected);
        <<"电流测量"/utf8>> ->
            judge_current_step(Metrics, Expected);
        <<"温度测量"/utf8>> ->
            judge_temperature_step(Metrics, Expected);
        <<"湿度测量"/utf8>> ->
            judge_humidity_step(Metrics, Expected);
        <<"功率测量"/utf8>> ->
            judge_power_step(Metrics, Expected);
        <<"运行时间检查"/utf8>> ->
            judge_runtime_step(Metrics, Expected);
        <<"错误计数检查"/utf8>> ->
            judge_error_count_step(Metrics, Expected);
        <<"状态检查"/utf8>> ->
            judge_status_step(Metrics, Expected);
        _ ->
            %% 默认判据：检查是否有异常状态
            judge_default_step(Metrics, Expected)
    end.

%%%===================================================================
%%% 拷机1特定判据函数
%%%===================================================================

%% @doc 判断拷机稳定性测试
-spec judge_burn_in_stability(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_burn_in_stability(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查关键指标是否在稳定范围内
    Voltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    Current = get_metric_value(?METRIC_CURRENT, Metrics),
    Temperature = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    Status = get_metric_value(?METRIC_STATUS, Metrics),
    ErrorCount = get_metric_value(?METRIC_ERROR_COUNT, Metrics),
    
    IsVoltageOk = is_metric_within_range(?METRIC_VOLTAGE, Voltage,
                                        {?VOLTAGE_NOMINAL - ?VOLTAGE_TOLERANCE,
                                         ?VOLTAGE_NOMINAL + ?VOLTAGE_TOLERANCE}),
    IsCurrentOk = Current =< ?CURRENT_MAX,
    IsTemperatureOk = Temperature =< ?TEMPERATURE_MAX,
    IsStatusOk = Status =:= 0,
    IsErrorCountOk = ErrorCount =< ?ERROR_COUNT_MAX,
    
    AllOk = IsVoltageOk andalso IsCurrentOk andalso 
            IsTemperatureOk andalso IsStatusOk andalso IsErrorCountOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"拷机稳定性测试"/utf8>>,
        voltage => Voltage,
        current => Current,
        temperature => Temperature,
        status => Status,
        error_count => ErrorCount,
        is_voltage_ok => IsVoltageOk,
        is_current_ok => IsCurrentOk,
        is_temperature_ok => IsTemperatureOk,
        is_status_ok => IsStatusOk,
        is_error_count_ok => IsErrorCountOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断温升测试
-spec judge_temperature_rise(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_temperature_rise(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查温升是否在允许范围内
    Temperature = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    TemperatureRise = get_temperature_rise(Context),  %% 需要从上下文获取起始温度
    
    IsTemperatureOk = Temperature =< ?TEMPERATURE_MAX,
    IsRiseOk = TemperatureRise =< ?TEMPERATURE_RISE_MAX,
    
    AllOk = IsTemperatureOk andalso IsRiseOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"温升测试"/utf8>>,
        temperature => Temperature,
        temperature_rise => TemperatureRise,
        max_temperature => ?TEMPERATURE_MAX,
        max_temperature_rise => ?TEMPERATURE_RISE_MAX,
        is_temperature_ok => IsTemperatureOk,
        is_rise_ok => IsRiseOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断功耗测试
-spec judge_power_consumption(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_power_consumption(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查功耗是否在允许范围内
    Voltage = get_metric_value(?METRIC_VOLTAGE, Metrics),
    Current = get_metric_value(?METRIC_CURRENT, Metrics),
    Power = get_metric_value(?METRIC_POWER, Metrics),
    
    %% 计算实际功率（如果功率未直接测量）
    CalculatedPower = case Power of
        0.0 -> Voltage * Current;  %% 如果没有直接测量，则计算功率
        _ -> Power
    end,
    
    IsPowerOk = CalculatedPower =< ?POWER_MAX,
    IsVoltageOk = is_metric_within_range(?METRIC_VOLTAGE, Voltage,
                                        {?VOLTAGE_NOMINAL - ?VOLTAGE_TOLERANCE,
                                         ?VOLTAGE_NOMINAL + ?VOLTAGE_TOLERANCE}),
    
    AllOk = IsPowerOk andalso IsVoltageOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"功耗测试"/utf8>>,
        voltage => Voltage,
        current => Current,
        power => CalculatedPower,
        max_power => ?POWER_MAX,
        is_power_ok => IsPowerOk,
        is_voltage_ok => IsVoltageOk,
        timestamp => erlang:system_time(millisecond)
    },
    
    {ok, #{passed => AllOk, details => Details}}.

%% @doc 判断运行时间测试
-spec judge_runtime_test(binary(), map()) -> {ok, #{passed => boolean(), details => map()}}.
judge_runtime_test(TestItemId, Context) ->
    Metrics = extract_metrics_from_context(Context),
    
    %% 检查运行时间是否达到要求
    Runtime = get_metric_value(?METRIC_RUNTIME, Metrics),
    ErrorCount = get_metric_value(?METRIC_ERROR_COUNT, Metrics),
    Status = get_metric_value(?METRIC_STATUS, Metrics),
    
    IsRuntimeOk = Runtime >= ?RUNTIME_MIN,
    IsErrorCountOk = ErrorCount =< ?ERROR_COUNT_MAX,
    IsStatusOk = Status =:= 0,
    
    AllOk = IsRuntimeOk andalso IsErrorCountOk andalso IsStatusOk,
    
    Details = #{
        test_item_id => TestItemId,
        test_type => <<"运行时间测试"/utf8>>,
        runtime => Runtime,
        error_count => ErrorCount,
        status => Status,
        min_runtime => ?RUNTIME_MIN,
        max_error_count => ?ERROR_COUNT_MAX,
        is_runtime_ok => IsRuntimeOk,
        is_error_count_ok => IsErrorCountOk,
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

%% @doc 判断温度步骤
judge_temperature_step(Metrics, Expected) ->
    MaxTemperature = maps:get(<<"max_temperature">>, Expected, ?TEMPERATURE_MAX),
    
    ActualTemperature = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    IsOk = ActualTemperature =< MaxTemperature,
    
    Details = #{
        step_name => <<"温度测量"/utf8>>,
        max_temperature => MaxTemperature,
        actual_temperature => ActualTemperature,
        is_below_max => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断湿度步骤
judge_humidity_step(Metrics, Expected) ->
    MaxHumidity = maps:get(<<"max_humidity">>, Expected, ?HUMIDITY_MAX),
    
    ActualHumidity = get_metric_value(?METRIC_HUMIDITY, Metrics),
    IsOk = ActualHumidity =< MaxHumidity,
    
    Details = #{
        step_name => <<"湿度测量"/utf8>>,
        max_humidity => MaxHumidity,
        actual_humidity => ActualHumidity,
        is_below_max => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断功率步骤
judge_power_step(Metrics, Expected) ->
    MaxPower = maps:get(<<"max_power">>, Expected, ?POWER_MAX),
    
    ActualPower = get_metric_value(?METRIC_POWER, Metrics),
    IsOk = ActualPower =< MaxPower,
    
    Details = #{
        step_name => <<"功率测量"/utf8>>,
        max_power => MaxPower,
        actual_power => ActualPower,
        is_below_max => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断运行时间步骤
judge_runtime_step(Metrics, Expected) ->
    MinRuntime = maps:get(<<"min_runtime">>, Expected, ?RUNTIME_MIN),
    
    ActualRuntime = get_metric_value(?METRIC_RUNTIME, Metrics),
    IsOk = ActualRuntime >= MinRuntime,
    
    Details = #{
        step_name => <<"运行时间检查"/utf8>>,
        min_runtime => MinRuntime,
        actual_runtime => ActualRuntime,
        is_above_min => IsOk
    },
    
    {ok, #{passed => IsOk, details => Details}}.

%% @doc 判断错误计数步骤
judge_error_count_step(Metrics, Expected) ->
    MaxErrorCount = maps:get(<<"max_error_count">>, Expected, ?ERROR_COUNT_MAX),
    
    ActualErrorCount = get_metric_value(?METRIC_ERROR_COUNT, Metrics),
    IsOk = ActualErrorCount =< MaxErrorCount,
    
    Details = #{
        step_name => <<"错误计数检查"/utf8>>,
        max_error_count => MaxErrorCount,
        actual_error_count => ActualErrorCount,
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

%% @doc 计算温升
-spec get_temperature_rise(map()) -> number().
get_temperature_rise(Context) ->
    Metrics = extract_metrics_from_context(Context),
    CurrentTemp = get_metric_value(?METRIC_TEMPERATURE, Metrics),
    StartTemp = get_metric_value(<<"start_temperature">>, Metrics),
    CurrentTemp - StartTemp.

%% @doc 从测试项ID推断测试类型
-spec infer_test_type(binary()) -> binary().
infer_test_type(TestItemId) ->
    case binary:match(TestItemId, <<"稳定性">>) of
        {_, _} -> <<"拷机稳定性测试"/utf8>>;
        nomatch ->
            case binary:match(TestItemId, <<"温升">>) of
                {_, _} -> <<"温升测试"/utf8>>;
                nomatch ->
                    case binary:match(TestItemId, <<"功耗">>) of
                        {_, _} -> <<"功耗测试"/utf8>>;
                        nomatch ->
                            case binary:match(TestItemId, <<"运行时间">>) of
                                {_, _} -> <<"运行时间测试"/utf8>>;
                                nomatch -> <<"拷机测试"/utf8>>
                            end
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
default_metric_value(?METRIC_TEMPERATURE) -> 25.0;
default_metric_value(?METRIC_HUMIDITY) -> 50.0;
default_metric_value(?METRIC_POWER) -> 0.0;
default_metric_value(?METRIC_RUNTIME) -> 0.0;
default_metric_value(?METRIC_STATUS) -> 0;
default_metric_value(?METRIC_ERROR_COUNT) -> 0;
default_metric_value(_) -> 0.0.

%%%===================================================================
%%% 在线调试函数
%%%===================================================================

%% @doc 测试判据模块
-spec test() -> ok.
test() ->
    io:format("~n========== 拷机1判据模块测试 ==========~n", []),
    
    %% 测试1: 稳定性测试判据
    TestContext1 = #{<<"metrics">> => #{
        <<"voltage">> => 23.8,
        <<"current">> => 8.5,
        <<"temperature">> => 65.2,
        <<"status">> => 0,
        <<"error_count">> => 1
    }},
    
    case judge_burn_in_stability(<<"test_stability_001">>, TestContext1) of
        {ok, #{passed := true, details := Details1}} ->
            io:format("✓ 稳定性测试通过: ~p~n", [Details1]);
        {ok, #{passed := false, details := Details1}} ->
            io:format("✗ 稳定性测试失败: ~p~n", [Details1]);
        Error1 ->
            io:format("✗ 稳定性测试错误: ~p~n", [Error1])
    end,
    
    %% 测试2: 温升测试判据
    TestContext2 = #{<<"metrics">> => #{
        <<"temperature">> => 70.5,
        <<"start_temperature">> => 25.0
    }},
    
    case judge_temperature_rise(<<"test_temp_rise_001">>, TestContext2) of
        {ok, #{passed := true, details := Details2}} ->
            io:format("✓ 温升测试通过: ~p~n", [Details2]);
        {ok, #{passed := false, details := Details2}} ->
            io:format("✗ 温升测试失败: ~p~n", [Details2]);
        Error2 ->
            io:format("✗ 温升测试错误: ~p~n", [Error2])
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.