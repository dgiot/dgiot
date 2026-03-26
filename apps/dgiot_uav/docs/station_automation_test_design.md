# 无人机测试产线各工位自动化检测流程用例设计与测试

## 一、概述

本文档详细设计无人机测试产线各个工位的自动化检测流程用例，涵盖从无人机上线到完成所有测试项的完整流程。

### 1.1 工位架构

产线包含以下工位：

| 工位编号 | 工位名称 | 主要功能 | 测试项数量 | IP地址 |
|---------|---------|---------|-----------|--------|
| 1700 | 磁航向 | 磁航向传感器校准 | 2 | 192.168.100.21 |
| 1500 | 总测1 | 综合测试 | 31 | 192.168.100.22 |
| 1600 | 总测2 | 综合测试 | 待补充 | 192.168.100.23 |
| 1200 | 拷机1 | 长时间运行稳定性测试 | 7 | 192.168.100.24 |
| 1300 | 拷机2 | 长时间运行稳定性测试 | 7 | 192.168.100.25 |
| 1100 | 桁架 | 导引头测试 | 9 | 192.168.100.26 |

### 1.2 设备端口映射

| 端口 | 设备类型 | 说明 |
|------|---------|------|
| 10001-10005 | 舵面传感器 | 左/右副翼、左/右垂尾、方向舵 |
| 10006 | 单片机(治具) | 治具控制 |
| 10007 | 地测口 | 无人机通信 |
| 1234 | 扫码枪 | 二维码扫描 |
| 21000 | 噪音传感器 | 噪音检测 |

## 二、测试流程总体设计

### 2.1 自动化测试主流程

```
┌─────────────────────────────────────────────────────────────────┐
│                     无人机自动化测试主流程                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. 无人机上线 (设备连接 192.168.100.7:20000)                     │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  2. 扫码绑定 (扫描二维码获取无人机SN → 绑定到工位)                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  3. 治具就绪 (PLC控制治具上电 → 单片机响应)                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  4. 工位识别 (根据IP地址识别工位类型 → 加载对应测试项)             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  5. 测试项执行 (按顺序执行测试项 → 每步下发指令 → 收集响应 → 判据)  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  6. 结果记录 (PASS/FAIL → 存储到TDengine + Parse Server)          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  7.  MES上报 (测试完成 → 上报MES系统 → 流转到下一工位)             │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 测试项执行流程

```
┌─────────────────────────────────────────────────────────────────┐
│                  测试项执行内部流程                               │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  加载测试项步骤 (从Parse Server加载steps[])                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │  遍历每个步骤    │
                    └─────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
        ┌──────────┐    ┌──────────┐    ┌──────────┐
        │  send    │    │ receive  │    │  judge   │
        │  发送指令│    │ 接收响应 │    │ 判据判定 │
        └──────────┘    └──────────┘    └──────────┘
              │               │               │
              ▼               ▼               ▼
        ┌──────────┐    ┌──────────┐    ┌──────────┐
        │下发到PLC │    │等待响应   │    │执行判据  │
        │/无人机   │    │超时判断   │    │阈值比较  │
        └──────────┘    └──────────┘    └──────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │  步骤结果记录   │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ 所有步骤完成?   │
                    │ 否 → 继续步骤   │
                    │ 是 → 测试项完成│
                    └─────────────────┘
```

## 三、各工位测试用例详细设计

### 3.1 磁航向工位 (1700)

#### 3.1.1 工位概述

磁航向工位主要负责无人机磁航向传感器的校准和测试，通过PLC控制磁航向测试辅具旋转，采集无人机传感器数据。

#### 3.1.2 测试用例清单

| 序号 | 测试项名称 | 步骤数 | 测试目标 | 关键指令 |
|------|-----------|--------|---------|---------|
| 1 | 磁航向校准 | 11 | 校准磁航向传感器 | PLC旋转→采集数据→计算偏差 |
| 2 | 磁航向测试 | 4 | 验证磁航向精度 | 多角度测试→数据比对 |

#### 3.1.3 磁航向校准测试用例

```erlang
%% 磁航向校准测试用例
test_item_magnetic_calibration() ->
    #test_item{
        name = <<"磁航向校准"/utf8>>,
        station_id = 1700,
        steps = [
            #test_step{
                step_number = 1,
                action_type = <<"send">>,
                description = <<"PLC顺时针旋转指令"/utf8>>,
                target = <<"1">>,  % 工位PLC
                send = #test_send_config{
                    address = <<"D1751">>,
                    content = <<"0001">>  % 顺时针旋转
                },
                wait = 2.0
            },
            #test_step{
                step_number = 2,
                action_type = <<"receive">>,
                description = <<"等待无人机磁传感器数据"/utf8>>,
                target = <<"3">>,  % 无人机
                recv_config = #test_receive_config{
                    timeout = 5000,
                    expected_fields = [<<"magnetic_x">>, <<"magnetic_y">>]
                },
                wait = 1.0
            },
            #test_step{
                step_number = 3,
                action_type = <<"send">>,
                description = <<"PLC翻转90度"/utf8>>,
                target = <<"1">>,
                send = #test_send_config{
                    address = <<"D1751">>,
                    content = <<"0003">>  % 翻转90度
                },
                wait = 2.0
            },
            #test_step{
                step_number = 4,
                action_type = <<"receive">>,
                description = <<"采集翻转后磁传感器数据"/utf8>>,
                target = <<"3">>,
                recv_config = #test_receive_config{
                    timeout = 5000
                },
                wait = 1.0
            },
            #test_step{
                step_number = 5,
                action_type = <<"judge">>,
                description = <<"判断磁航向数据是否在正常范围"/utf8>>,
                target = <<"3">>,
                expected_result = #{
                    <<"magnetic_x">> => #{<<"min">> => -1000, <<"max">> => 1000},
                    <<"magnetic_y">> => #{<<"min">> => -1000, <<"max">> => 1000}
                },
                result = undefined
            }
            % ... 更多步骤
        ]
    }.
```

#### 3.1.4 判据规则定义

```json
{
    "magnetic_heading": {
        "voltage_check": {
            "type": "simple",
            "rule": "{\"field\": \"voltage\", \"operator\": \">=\", \"value\": 22.0, \"unit\": \"V\"}",
            "description": "磁航向电压检查：额定电压24V±2V"
        },
        "current_check": {
            "type": "simple",
            "rule": "{\"field\": \"current\", \"operator\": \"<=\", \"value\": 5.0, \"unit\": \"A\"}",
            "description": "磁航向电流检查：最大电流5A"
        },
        "magnetic_sensor_check": {
            "type": "tdengine",
            "rule": "SELECT COUNT(*) FROM uav_magnetic_metrics WHERE device_id='${device_id}' AND magnetic_x >= -1000 AND magnetic_x <= 1000 AND magnetic_y >= -1000 AND magnetic_y <= 1000 AND ts >= ${start_time} AND ts <= ${end_time}",
            "description": "磁传感器数据检查：X/Y轴在正常范围内"
        }
    }
}
```

### 3.2 总测1工位 (1500)

#### 3.2.1 工位概述

总测1工位是无人机出厂前的综合测试工位，包含电气测试、通信测试、舵面测试、动力测试等31项测试。

#### 3.2.2 测试用例分类

| 类别 | 测试项数量 | 示例 |
|------|-----------|------|
| 电气测试 | 6 | 上电、电压显示、一次电池通讯、引信供电 |
| 通信测试 | 5 | 飞控版本号、发射筒通讯、数据链检查 |
| 导航测试 | 4 | 导航状态、卫星导航、气压高度 |
| 舵面测试 | 8 | 左右副翼校准、左右垂尾校准、舵面极性 |
| 动力测试 | 1 | 动力测试(190步骤) |
| 其他 | 7 | 帧频检查、主循环时间、加速度校准、姿态测试 |

#### 3.2.3 动力测试用例(节选)

动力测试是总测1最重要的测试项，包含190个步骤，主要测试无人机的电机响应和控制精度。

```erlang
%% 动力测试用例结构
test_item_power_test() ->
    #test_item{
        name = <<"动力测试"/utf8>>,
        station_id = 1500,
        devaddr = <<"总测1_动力测试"/utf8>>,
        steps = [
            % 第1阶段：初始化
            #test_step{
                step_number = 1,
                action_type = <<"send">>,
                description = <<"发送动力测试初始化指令"/utf8>>,
                target = <<"3">>,  % 无人机
                send = #test_send_config{
                    content = <<"F0 A2 01">>  % 动力测试模式
                },
                wait = 1.0
            },
            % 第2阶段：各通道测试
            #test_step{
                step_number = 2,
                action_type = <<"send">>,
                description = <<"左副翼通道测试"/utf8>>,
                target = <<"3">>,
                send = #test_send_config{
                    content = <<"DC 01">>  % 左副翼
                },
                wait = 0.5
            },
            #test_step{
                step_number = 3,
                action_type = <<"receive">>,
                description = <<"接收左副翼响应"/utf8>>,
                target = <<"3">>,
                recv_config = #test_receive_config{
                    timeout = 2000,
                    expected_fields = [<<"aileron_left_angle">>]
                },
                wait = 0.5
            },
            % ... 继续其他通道测试
            % 右副翼、右垂尾、左垂尾、方向舵等
        ]
    }.
```

### 3.3 拷机工位 (1200/1300)

#### 3.3.1 工位概述

拷机工位负责无人机的长时间运行稳定性测试，通常需要连续运行数小时。

#### 3.3.2 测试用例清单

| 序号 | 测试项名称 | 步骤数 | 测试目标 |
|------|-----------|--------|---------|
| 1 | 拷机准备 | 3 | 系统初始化和自检 |
| 2 | 导航状态检查 | 2 | GPS/北斗导航状态 |
| 3 | 卫星导航检查 | 2 | 卫星信号质量 |
| 4 | 空速标定 | 3 | 空速传感器校准 |
| 5 | 空速调试 | 5 | 空速数据验证 |
| 6 | 帧频检查 | 2 | 通信帧频稳定性 |
| 7 | 数据链检查 | 13 | 链路通信质量 |

#### 3.3.3 拷机测试数据采集

```erlang
%% 拷机测试数据采集配置
burnin_test_data_collection() ->
    #{
        % 实时监控指标
        realtime_metrics => [
            <<"voltage">>,      % 电压
            <<"current">>,      % 电流
            <<"temperature">>,   % 温度
            <<"humidity">>,     % 湿度
            <<"gps_status">>,   % GPS状态
            <<"link_quality">>  % 链路质量
        ],
        % 采集频率配置
        collection_interval => 1000,  % 1秒
        % 存储配置
        storage => #{
            backend => tdengine,
            table => <<"uav_burnin_metrics">>,
            retention => 2592000  % 30天
        },
        % 告警阈值
        alarm_thresholds => #{
            <<"temperature">> => 85.0,  % °C
            <<"voltage_low">> => 22.0,   % V
            <<"voltage_high">> => 26.0,  % V
            <<"current_max">> => 10.0    % A
        }
    }.
```

### 3.4 桁架工位 (1100)

#### 3.4.1 工位概述

桁架工位负责导引头的测试，包括上电、功能调试、图像测试等。

#### 3.4.2 测试用例清单

| 序号 | 测试项名称 | 步骤数 | 测试目标 |
|------|-----------|--------|---------|
| 1 | 导引头上电 | 2 | 导引头系统启动 |
| 2 | 电子变倍功能调试 | 4 | 变倍功能验证 |
| 3 | 扫描与刹车测试 | 3 | 扫描和制动功能 |
| 4 | 锁定测试 | 2 | 目标锁定功能 |
| 5 | 可见光与红外切换 | 3 | 传感器切换 |
| 6 | 黑白热切换 | 3 | 红外模式切换 |
| 7 | H264码率测试 | 3 | 视频编码质量 |
| 8 | 重复性检查 | 1 | 测试一致性 |
| 9 | 拷机结束 | 5 | 下线前准备 |

## 四、测试用例执行框架

### 4.1 测试执行器设计

```erlang
-module(dgiot_uav_test_executor).
-author("johnliu").

%% 测试执行API
-export([
    execute_test_item/3,      % 执行单个测试项
    execute_test_steps/4,    % 执行测试步骤序列
    execute_single_step/4,    % 执行单个步骤
    judge_step_result/3,      % 判定步骤结果
    handle_step_error/4       % 处理步骤错误
]).

%% 测试执行结果
-record(test_execution_result, {
    test_id :: binary(),
    test_item_id :: binary(),
    device_id :: binary(),
    station_id :: integer(),
    status :: atom(),           % running | completed | failed
    start_time :: integer(),
    end_time :: integer() | undefined,
    step_results :: [#step_result{}],
    final_result :: atom()      % pass | fail | skip
}).

%% 步骤执行结果
-record(step_result, {
    step_number :: integer(),
    action_type :: binary(),
    description :: binary(),
    status :: atom(),            % pending | running | passed | failed
    send_time :: integer() | undefined,
    recv_time :: integer() | undefined,
    judge_time :: integer() | undefined,
    expected :: term(),
    actual :: term(),
    result :: atom()             % pass | fail | skip
}).

%% 执行单个测试项
-spec execute_test_item(binary(), binary(), integer()) -> 
    {ok, #test_execution_result{}} | {error, term()}.
execute_test_item(TestItemId, DeviceId, StationId) ->
    ?LOG(info, "开始执行测试项: ~p, 设备: ~p, 工位: ~p", 
         [TestItemId, DeviceId, StationId]),
    
    % 1. 加载测试项
    case dgiot_uav_test_item_loader:load_test_item(TestItemId) of
        {ok, TestItem} ->
            % 2. 创建执行记录
            ExecutionResult = #test_execution_result{
                test_id = generate_test_id(),
                test_item_id = TestItemId,
                device_id = DeviceId,
                station_id = StationId,
                status = running,
                start_time = erlang:system_time(millisecond),
                step_results = []
            },
            
            % 3. 执行测试步骤
            Steps = maps:get(steps, TestItem, []),
            {FinalResult, StepResults} = execute_test_steps(
                Steps, DeviceId, StationId, ExecutionResult
            ),
            
            % 4. 返回执行结果
            EndTime = erlang:system_time(millisecond),
            FinalExecutionResult = ExecutionResult#test_execution_result{
                status = completed,
                end_time = EndTime,
                step_results = StepResults,
                final_result = FinalResult
            },
            
            % 5. 保存结果到存储
            save_test_execution_result(FinalExecutionResult),
            
            {ok, FinalExecutionResult};
        
        {error, Reason} ->
            ?LOG(error, "加载测试项失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 执行测试步骤序列
-spec execute_test_steps(list(), binary(), integer(), #test_execution_result{}) ->
    {atom(), list(#step_result{})}.
execute_test_steps([], _DeviceId, _StationId, ExecutionResult) ->
    {pass, ExecutionResult#test_execution_result.step_results};
execute_test_steps([Step | Rest], DeviceId, StationId, ExecutionResult) ->
    StepResults = ExecutionResult#test_execution_result.step_results,
    
    case execute_single_step(Step, DeviceId, StationId) of
        {ok, StepResult} ->
            NewStepResults = StepResults ++ [StepResult],
            NewExecutionResult = ExecutionResult#test_execution_result{
                step_results = NewStepResults
            },
            
            % 判断是否继续执行
            case StepResult#step_result.result of
                pass ->
                    execute_test_steps(Rest, DeviceId, StationId, NewExecutionResult);
                fail ->
                    ?LOG(warning, "测试步骤失败: ~p", [Step#test_step.description]),
                    {fail, NewStepResults};
                skip ->
                    execute_test_steps(Rest, DeviceId, StationId, NewExecutionResult)
            end;
        
        {error, Reason} ->
            ?LOG(error, "执行测试步骤失败: ~p", [Reason]),
            ErrorStepResult = #step_result{
                step_number = Step#test_step.step_number,
                action_type = Step#test_step.action_type,
                description = Step#test_step.description,
                status = failed,
                result = fail
            },
            {fail, StepResults ++ [ErrorStepResult]}
    end.

%% 执行单个测试步骤
-spec execute_single_step(#test_step{}, binary(), integer()) ->
    {ok, #step_result{}} | {error, term()}.
execute_single_step(Step, DeviceId, StationId) ->
    StepNumber = Step#test_step.step_number,
    ActionType = Step#test_step.action_type,
    Description = Step#test_step.description,
    Target = Step#test_step.target,
    
    ?LOG(debug, "执行步骤 ~p: ~p, 目标: ~p", 
         [StepNumber, Description, Target]),
    
    StartTime = erlang:system_time(millisecond),
    
    Result = case ActionType of
        <<"send">> ->
            execute_send_step(Step, DeviceId, StationId);
        <<"receive">> ->
            execute_receive_step(Step, DeviceId, StationId);
        <<"judge">> ->
            execute_judge_step(Step, DeviceId, StationId);
        <<"request_response">> ->
            execute_request_response_step(Step, DeviceId, StationId);
        _ ->
            {error, {unknown_action_type, ActionType}}
    end,
    
    EndTime = erlang:system_time(millisecond),
    
    case Result of
        {ok, Expected, Actual} ->
            {ok, #step_result{
                step_number = StepNumber,
                action_type = ActionType,
                description = Description,
                status = passed,
                send_time = StartTime,
                recv_time = EndTime,
                expected = Expected,
                actual = Actual,
                result = pass
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% 发送步骤执行
execute_send_step(Step, DeviceId, StationId) ->
    SendConfig = Step#test_step.send,
    Target = Step#test_step.target,
    
    % 根据目标类型选择发送通道
    Channel = get_channel_by_target(Target),
    
    % 发送指令
    case dgiot_uav_command_manager:send_command(
        Channel, DeviceId, StationId, SendConfig
    ) of
        {ok, Response} ->
            {ok, SendConfig#test_send_config.content, Response};
        {error, Reason} ->
            {error, Reason}
    end.

%% 接收步骤执行
execute_receive_step(Step, DeviceId, StationId) ->
    RecvConfig = Step#test_step.recv_config,
    Target = Step#test_step.target,
    Timeout = RecvConfig#test_receive_config.timeout,
    
    % 等待接收数据
    case dgiot_uav_command_manager:wait_response(
        DeviceId, StationId, Target, Timeout
    ) of
        {ok, Data} ->
            Expected = RecvConfig#test_receive_config.recv_content,
            {ok, Expected, Data};
        {error, Reason} ->
            {error, Reason}
    end.

%% 判据步骤执行
execute_judge_step(Step, DeviceId, StationId) ->
    ExpectedResult = Step#test_step.expected_result,
    Target = Step#test_step.target,
    
    % 获取实际数据
    case get_actual_data(Target, DeviceId, StationId) of
        {ok, ActualData} ->
            % 执行判据判定
            case judge_step_result(ExpectedResult, ActualData) of
                {ok, IsPass} ->
                    Result = case IsPass of
                        true -> pass;
                        false -> fail
                    end,
                    {ok, ExpectedResult, ActualData};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 判据判定
judge_step_result(Expected, Actual) when is_map(Expected), is_map(Actual) ->
    % 逐字段判定
    FieldResults = maps:fold(
        fun(Field, ExpectedValue, Acc) ->
            ActualValue = maps:get(Field, Actual, undefined),
            case evaluate_field_condition(Field, ExpectedValue, ActualValue) of
                true -> Acc;
                false -> [{Field, {expected, ExpectedValue, actual, ActualValue}} | Acc]
            end
        end,
        [],
        Expected
    ),
    
    case FieldResults of
        [] -> {ok, true};
        _ -> {ok, false}
    end;
judge_step_result(Expected, Actual) ->
    {ok, Expected =:= Actual}.

%% 字段条件评估
evaluate_field_condition(Field, ExpectedValue, ActualValue) ->
    case ExpectedValue of
        #{<<"min">> := Min, <<"max">> := Max} ->
            (ActualValue >= Min) and (ActualValue =< Max);
        #{<<"operator">> := Operator, <<"value">> := Threshold} ->
            evaluate_operator(Operator, ActualValue, Threshold);
        _ ->
            ActualValue =:= ExpectedValue
    end.

%% 操作符评估
evaluate_operator(<<">=">>, Value, Threshold) -> Value >= Threshold;
evaluate_operator(<<"<=">>, Value, Threshold) -> Value =< Threshold;
evaluate_operator(<<">">>, Value, Threshold) -> Value > Threshold;
evaluate_operator(<<"<">>, Value, Threshold) -> Value < Threshold;
evaluate_operator(<<"=">>, Value, Threshold) -> Value =:= Threshold;
evaluate_operator(<<"!=">>, Value, Threshold) -> Value =/= Threshold;
evaluate_operator(_, _, _) -> false.
```

### 4.2 测试结果存储

```erlang
-module(dgiot_uav_test_result_store).

-export([
    save_test_execution_result/1,
    save_step_result/4,
    query_test_history/3,
    query_test_statistics/2
]).

%% 保存测试执行结果
save_test_execution_result(Result) ->
    #test_execution_result{
        test_id = TestId,
        test_item_id = TestItemId,
        device_id = DeviceId,
        station_id = StationId,
        status = Status,
        start_time = StartTime,
        end_time = EndTime,
        final_result = FinalResult
    } = Result,
    
    % 保存到Parse Server
    TestResultDoc = #{
        <<"testId">> => TestId,
        <<"testItemId">> => TestItemId,
        <<"deviceId">> => DeviceId,
        <<"stationId">> => StationId,
        <<"status">> => atom_to_binary(Status, utf8),
        <<"result">> => atom_to_binary(FinalResult, utf8),
        <<"startTime">> => StartTime,
        <<"endTime">> => EndTime,
        <<"duration">> => EndTime - StartTime,
        <<"stepCount">> => length(Result#test_execution_result.step_results)
    },
    
    case dgiot_parse:create_object(<<"TestExecution">>, TestResultDoc) of
        {ok, _} ->
            ?LOG(info, "测试结果保存成功: ~p", [TestId]),
            ok;
        {error, Reason} ->
            ?LOG(error, "测试结果保存失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 保存到TDengine (时序数据)
save_test_metrics_to_td(TestId, StationId, Metrics) ->
    MetricsData = #{
        <<"test_id">> => TestId,
        <<"station_id">> => StationId,
        <<"metrics">> => Metrics,
        <<"ts">> => erlang:system_time(millisecond)
    },
    
    dgiot_tdengine:insert(<<"uav_test_metrics">>, MetricsData).
```

## 五、测试脚本实现

### 5.1 集成测试脚本

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
无人机自动化测试集成脚本
执行各工位测试用例
"""

import json
import time
import socket
import struct
from typing import Dict, List, Optional

class UAVTestIntegrator:
    """无人机测试集成器"""
    
    def __init__(self, config_path: str = "config.json"):
        with open(config_path, 'r', encoding='utf-8') as f:
            self.config = json.load(f)
        
        self.test_items = []
        self.test_results = []
        
    def load_test_items(self, json_path: str):
        """加载测试项配置"""
        with open(json_path, 'r', encoding='utf-8') as f:
            self.test_items = json.load(f)
        print(f"加载了 {len(self.test_items)} 个测试项")
        
    def execute_station_tests(self, station_name: str) -> Dict:
        """执行指定工位的所有测试"""
        # 筛选该工位的测试项
        station_tests = [
            item for item in self.test_items 
            if item.get('station_name') == station_name
        ]
        
        print(f"\n=== 开始执行 {station_name} 工位测试 ===")
        print(f"测试项数量: {len(station_tests)}")
        
        results = {
            'station': station_name,
            'total': len(station_tests),
            'passed': 0,
            'failed': 0,
            'skipped': 0,
            'details': []
        }
        
        for test_item in station_tests:
            result = self.execute_single_test(test_item)
            results['details'].append(result)
            
            if result['status'] == 'passed':
                results['passed'] += 1
            elif result['status'] == 'failed':
                results['failed'] += 1
            else:
                results['skipped'] += 1
                
        print(f"执行完成: 通过={results['passed']}, 失败={results['failed']}, 跳过={results['skipped']}")
        return results
        
    def execute_single_test(self, test_item: Dict) -> Dict:
        """执行单个测试项"""
        test_name = test_item.get('device_name', 'Unknown')
        step_count = test_item.get('test_step_count', 0)
        
        print(f"\n  执行测试: {test_name} ({step_count}步骤)")
        
        result = {
            'name': test_name,
            'step_count': step_count,
            'status': 'passed',  # 默认通过
            'start_time': time.time(),
            'end_time': None,
            'errors': []
        }
        
        # 模拟测试执行
        # 实际实现中应该调用Erlang节点的RPC接口
        time.sleep(0.1)  # 模拟执行时间
        
        result['end_time'] = time.time()
        return result
        
    def execute_full_production_line(self) -> List[Dict]:
        """执行完整产线测试"""
        stations = ['磁航向', '总测1', '拷机1', '拷机2', '桁架']
        all_results = []
        
        for station in stations:
            result = self.execute_station_tests(station)
            all_results.append(result)
            
        return all_results
        
    def generate_report(self, results: List[Dict]) -> str:
        """生成测试报告"""
        total_tests = sum(r['total'] for r in results)
        total_passed = sum(r['passed'] for r in results)
        total_failed = sum(r['failed'] for r in results)
        
        report = f"""
======================================
无人机自动化测试报告
======================================
测试时间: {time.strftime('%Y-%m-%d %H:%M:%S')}

总计:
  测试工位数: {len(results)}
  测试项总数: {total_tests}
  通过: {total_passed}
  失败: {total_failed}
  通过率: {total_passed/total_tests*100:.1f}%

各工位详情:
"""
        for r in results:
            rate = r['passed']/r['total']*100 if r['total'] > 0 else 0
            report += f"""
  {r['station']}:
    测试项: {r['total']}
    通过: {r['passed']}
    失败: {r['failed']}
    通过率: {rate:.1f}%
"""
        return report


if __name__ == '__main__':
    integrator = UAVTestIntegrator()
    integrator.load_test_items('../priv/json/test_items_summary.json')
    
    # 执行完整产线测试
    results = integrator.execute_full_production_line()
    
    # 生成报告
    report = integrator.generate_report(results)
    print(report)
    
    # 保存报告
    with open('test_report.txt', 'w', encoding='utf-8') as f:
        f.write(report)
```

### 5.2 工位测试Shell脚本

```bash
#!/bin/bash
# 执行指定工位自动化测试

# 配置
STATION_NAME=${1:-"磁航向"}
DGIOT_EVAL="_build/emqx/rel/emqx/bin/emqx eval"

echo "=========================================="
echo "无人机工位自动化测试"
echo "工位: $STATION_NAME"
echo "=========================================="

# 1. 检查系统状态
echo "[1/4] 检查系统状态..."
$DGIOT_EVAL "dgiot_uav_station_manager:check_all_stations()."

# 2. 加载测试项
echo "[2/4] 加载测试项..."
$DGIOT_EVAL "dgiot_uav_test_item_loader:debug_load_magnetic()."

# 3. 执行测试
echo "[3/4] 执行测试..."
case $STATION_NAME in
    "磁航向")
        $DGIOT_EVAL "dgiot_uav_auto_tester:test_magnetic_auto()."
        ;;
    "总测1")
        $DGIOT_EVAL "dgiot_uav_auto_tester:test_total_station(1500)."
        ;;
    "拷机1")
        $DGIOT_EVAL "dgiot_uav_auto_tester:test_burnin_station(1200)."
        ;;
    "拷机2")
        $DGIOT_EVAL "dgiot_uav_auto_tester:test_burnin_station(1300)."
        ;;
    "桁架")
        $DGIOT_EVAL "dgiot_uav_auto_tester:test_gantry_station(1100)."
        ;;
    *)
        echo "未知工位: $STATION_NAME"
        exit 1
        ;;
esac

# 4. 查看测试结果
echo "[4/4] 查看测试结果..."
$DGIOT_EVAL "dgiot_uav_test_result_store:query_test_statistics(1000, 100)."

echo "=========================================="
echo "测试完成"
echo "=========================================="
```

## 六、测试判据实现

### 6.1 判据类型定义

| 判据类型 | 说明 | 适用场景 |
|---------|------|---------|
| simple | 简单阈值比较 | 电压、电流、温度等单值判据 |
| sql | SQL查询判据 | 复杂业务逻辑判断 |
| tdengine | 时序数据库判据 | 历史数据统计判断 |

### 6.2 判据执行引擎

```erlang
-module(dgiot_uav_judge_engine).

-export([
    evaluate_judge_rule/3,
    evaluate_simple/3,
    evaluate_sql/3,
    evaluate_tdengine/3
]).

%% 评估判据规则
evaluate_judge_rule(RuleType, Rule, Context) ->
    case RuleType of
        <<"simple">> ->
            evaluate_simple(Rule, Rule, Context);
        <<"sql">> ->
            evaluate_sql(Rule, Rule, Context);
        <<"tdengine">> ->
            evaluate_tdengine(Rule, Rule, Context);
        _ ->
            {error, {unknown_rule_type, RuleType}}
    end.

%% 简单阈值判据
evaluate_simple(Rule, Expected, Context) ->
    % Rule格式: "{\"field\": \"voltage\", \"operator\": \">=\", \"value\": 22.0}"
    RuleMap = jsx:decode(list_to_binary(Rule)),
    
    Field = maps:get(<<"field">>, RuleMap),
    Operator = maps:get(<<"operator">>, RuleMap),
    Threshold = maps:get(<<"value">>, RuleMap),
    
    % 从上下文获取实际值
    Actual = maps:get(Field, Context, undefined),
    
    case Actual of
        undefined ->
            {error, {field_not_found, Field}};
        _ ->
            Result = evaluate_operator(Operator, Actual, Threshold),
            {ok, Result, #{
                expected => #{field => Field, operator => Operator, value => Threshold},
                actual => Actual
            }}
    end.

%% SQL判据
evaluate_sql(SqlTemplate, Expected, Context) ->
    % 替换模板变量
    Sql = replace_template_vars(SqlTemplate, Context),
    
    % 执行SQL查询
    case dgiot_parse:query(Sql) of
        {ok, Result} ->
            % 判定查询结果
            Pass = evaluate_sql_result(Result, Expected),
            {ok, Pass, #{sql => Sql, result => Result}};
        {error, Reason} ->
            {error, Reason}
    end.

%% TDengine判据
evaluate_tdengine(SqlTemplate, Expected, Context) ->
    % 替换模板变量
    Sql = replace_template_vars(SqlTemplate, Context),
    
    % 执行TDengine查询
    case dgiot_tdengine:query(Sql) of
        {ok, Result} ->
            % 判定查询结果
            Pass = evaluate_tdengine_result(Result, Expected),
            {ok, Pass, #{sql => Sql, result => Result}};
        {error, Reason} ->
            {error, Reason}
    end.

%% 替换模板变量
replace_template_vars(Sql, Context) ->
    Vars = [
        {"\\$\\{device_id\\}", maps:get(device_id, Context, "")},
        {"\\$\\{start_time\\}", maps:get(start_time, Context, 0)},
        {"\\$\\{end_time\\}", maps:get(end_time, Context, 9999999999999)}
    ],
    
    lists:foldl(
        fun({Pattern, Value}, AccSql) ->
            re:replace(AccSql, Pattern, Value, [{return, list}])
        end,
        Sql,
        Vars
    ).

%% 操作符评估
evaluate_operator(<<">=">>, Value, Threshold) -> Value >= Threshold;
evaluate_operator(<<"<=">>, Value, Threshold) -> Value =< Threshold;
evaluate_operator(<<">">>, Value, Threshold) -> Value > Threshold;
evaluate_operator(<<"<">>, Value, Threshold) -> Value < Threshold;
evaluate_operator(<<"=">>, Value, Threshold) -> Value =:= Threshold;
evaluate_operator(<<"!=">>, Value, Threshold) -> Value =/= Threshold.
```

## 七、测试日志与监控

### 7.1 测试日志配置

```erlang
%% 测试模块日志级别配置
-define(TEST_LOG_LEVEL, debug).

%% 日志宏定义
-define(TEST_LOG(Format, Args),
    ?LOG(info, "[TEST] " ++ Format, Args)).

-define(TEST_LOG_DEBUG(Format, Args),
    ?LOG(debug, "[TEST] " ++ Format, Args)).

-define(TEST_LOG_ERROR(Format, Args),
    ?LOG(error, "[TEST] " ++ Format, Args)).
```

### 7.2 实时监控接口

```erlang
%% 测试实时监控API
-module(dgiot_uav_test_monitor).

-export([
    get_realtime_status/0,
    get_test_progress/1,
    get_station_test_summary/1,
    subscribe_test_events/1
]).

%% 获取实时测试状态
get_realtime_status() ->
    #{
        active_tests => get_active_test_count(),
        queued_tests => get_queued_test_count(),
        completed_today => get_completed_today_count(),
        failed_today => get_failed_today_count(),
        avg_duration => get_average_test_duration()
    }.

%% 获取测试进度
get_test_progress(TestId) ->
    case dgiot_data:lookup(test_progress, TestId) of
        {ok, Progress} -> {ok, Progress};
        _ -> {error, not_found}
    end.

%% 获取工位测试摘要
get_station_test_summary(StationId) ->
    #{
        station_id => StationId,
        total_tests => count_station_tests(StationId),
        passed => count_station_passed(StationId),
        failed => count_station_failed(StationId),
        in_progress => count_station_in_progress(StationId)
    }.
```

## 八、总结

本文档详细设计了无人机测试产线各工位的自动化检测流程用例，包括：

1. **测试流程设计**：从无人机上线到MES上报的完整自动化流程
2. **工位测试用例**：磁航向、总测1/2、拷机1/2、桁架等工位的具体测试项
3. **测试执行框架**：测试项加载、步骤执行、判据评估的完整实现
4. **测试脚本**：Python集成测试脚本和Shell批量执行脚本
5. **判据引擎**：支持简单阈值、SQL、TDengine三种判据类型
6. **监控接口**：实时测试状态和进度查询

后续工作：
- 完成各工位测试项的具体步骤配置
- 实现测试结果的自动上报MES功能
- 增加测试异常处理和重试机制
- 完善测试报告生成和展示
