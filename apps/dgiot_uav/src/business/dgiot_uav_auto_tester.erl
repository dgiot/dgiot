%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_auto_tester - 无人机自动化测试器
%%% 负责磁航向工位的自动化测试流程
%%% @version 1.0.0
%%%-------------------------------------------------------------------
-module(dgiot_uav_auto_tester).
-author("dgiot_uav_team").

-include_lib("dgiot/include/logger.hrl").

%% 记录定义
-record(test_item, {
    id :: binary(),
    name :: binary(),
    station_id :: integer(),
    station_name :: binary(),
    steps = [] :: list(),
    order = 0 :: integer()
}).

%% API 导出
-export([
    test/0,
    test_magnetic_auto/0,
    test_station_auto/1,
    auto_test_device/3,
    start_test_for_device/1,
    find_plc_client_for_station/1,
    handle_device_online/1,
    handle_device_offline/1
]).

%%====================================================================
%%% API 函数
%%====================================================================

test() ->
    ?LOG(info, "[AUTO_TEST] 启动自动化测试器测试（简化版）"),
    {ok, "自动化测试器正常工作（简化版）"}.

start_test_for_device(DeviceId) ->
    ?LOG(info, "[AUTO_TEST] 收到设备上线测试请求: DeviceId=~s", [DeviceId]),
    %% 暂时模拟测试成功
    ?LOG(info, "[AUTO_TEST] 模拟测试执行中..."),
    timer:sleep(500),
    ?LOG(info, "[AUTO_TEST] 模拟测试完成"),
    {ok, <<"测试已启动">>}.

test_magnetic_auto() ->
    io:format(standard_error, "~n========== 磁航向自动化测试开始 ==========~n", []),
    ?LOG(error, "[AUTO_TEST] 磁航向自动化测试开始"),
    
    %% 1. 检查PLC客户端
    io:format(standard_error, "[AUTO_TEST] 1. 检查PLC客户端...~n", []),
    ?LOG(error, "[AUTO_TEST] 1. 检查PLC客户端..."),
    case find_plc_client_for_station(1700) of
        {ok, _Pid} ->
            io:format(standard_error, "[AUTO_TEST]   ✓ PLC客户端找到~n", []),
            ?LOG(error, "[AUTO_TEST]   ✓ PLC客户端找到"),
            
            %% 2. 加载磁航向工位测试项
            io:format(standard_error, "[AUTO_TEST] 2. 加载磁航向工位测试项...~n", []),
            ?LOG(error, "[AUTO_TEST] 2. 加载磁航向工位测试项..."),
            case dgiot_uav_test_loader:load_by_station(1700) of
                {ok, TestItems} ->
                    io:format(standard_error, "[AUTO_TEST]   ✓ 测试项数量: ~p~n", [length(TestItems)]),
                    ?LOG(error, "[AUTO_TEST]   ✓ 测试项数量: ~p", [length(TestItems)]),
                    
                    %% 3. 执行测试项
                    execute_magnetic_test_items(TestItems);
                {error, Reason} ->
                    io:format(standard_error, "[AUTO_TEST]   ✗ 加载测试项失败: ~p~n", [Reason]),
                    ?LOG(error, "[AUTO_TEST]   ✗ 加载测试项失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format(standard_error, "[AUTO_TEST]   ✗ PLC客户端查找失败: ~p~n", [Reason]),
            ?LOG(error, "[AUTO_TEST]   ✗ PLC客户端查找失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 执行磁航向测试项
execute_magnetic_test_items([]) ->
    io:format(standard_error, "[AUTO_TEST] 所有测试项执行完成~n", []),
    ?LOG(error, "[AUTO_TEST] 所有测试项执行完成"),
    {ok, <<"磁航向自动化测试完成">>};
execute_magnetic_test_items([TestItem | Rest]) ->
    #test_item{id = TestItemId, name = TestItemName, steps = Steps} = TestItem,
    
    io:format(standard_error, "[AUTO_TEST] 3. 执行测试项: ~s (~s)~n", [TestItemName, TestItemId]),
    ?LOG(error, "[AUTO_TEST] 3. 执行测试项: ~s (~s)", [TestItemName, TestItemId]),
    
    %% 执行测试步骤
    case execute_magnetic_test_steps(Steps, TestItemId, 1) of
        ok ->
            execute_magnetic_test_items(Rest);
        {error, Reason} ->
            ?LOG(error, "[AUTO_TEST] 测试项执行失败: ~p", [Reason]),
            {error, Reason}
    end.

%% 执行磁航向测试步骤
execute_magnetic_test_steps([], _TestItemId, _StepIndex) ->
    ok;
execute_magnetic_test_steps([Step | Rest], TestItemId, StepIndex) ->
    case execute_magnetic_step(Step, TestItemId, StepIndex) of
        ok ->
            %% 等待数据汇聚完成
            timer:sleep(1000),
            execute_magnetic_test_steps(Rest, TestItemId, StepIndex + 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% 执行单个测试步骤（适配test_items_summary.json格式）
execute_magnetic_step(Step, TestItemId, StepIndex) ->
    ActionType = maps:get(<<"action_type">>, Step, <<>>),
    Target = maps:get(<<"target">>, Step, <<>>),
    SendValue = maps:get(<<"send">>, Step, <<"0">>),
    Description = maps:get(<<"description">>, Step, <<>>),
    
    ?LOG(error, "[AUTO_TEST] 步骤~p: ~s [action=~s, target=~s, send=~s]",
         [StepIndex, Description, ActionType, Target, SendValue]),
    
    case ActionType of
        <<"send">> ->
            %% 下发指令
            case Target of
                <<"1">> ->
                    %% 下发PLC指令
                    StationId = 1700,
                    Code = case SendValue of
                        <<"1">> -> 1;  %% 顺时针旋转
                        <<"2">> -> 2;  %% 逆时针旋转
                        <<"0">> -> 0;  %% 停止
                        _ -> 0
                    end,
                    
                    Params = #{
                        station_id => StationId,
                        test_item_id => TestItemId,
                        step_index => StepIndex
                    },
                    
                    ?LOG(error, "[AUTO_TEST] 下发PLC指令: Code=~p, Station=~p", [Code, StationId]),
                    dgiot_uav_command_manager:send_plc_command(Code, Code, Params);
                    
                <<"3">> ->
                    %% 数据汇聚（D1、D2、D3）
                    ?LOG(error, "[AUTO_TEST] 数据汇聚: 等待数据采集完成"),
                    timer:sleep(2000),  %% 等待数据汇聚
                    ok;
                    
                _ ->
                    ?LOG(warning, "[AUTO_TEST] 未知目标: ~p", [Target]),
                    ok
            end;
            
        <<"judge">> ->
            %% 判据步骤（等待数据汇聚完成）
            ?LOG(error, "[AUTO_TEST] 判据步骤: 等待数据汇聚完成"),
            timer:sleep(1000),
            ok;
            
        _ ->
            ?LOG(warning, "[AUTO_TEST] 未知动作类型: ~p", [ActionType]),
            ok
    end.

test_station_auto(StationId) ->
    ?LOG(info, "[AUTO_TEST] 工位自动化测试已屏蔽: StationId=~p", [StationId]),
    {ok, "工位测试已屏蔽"}.

auto_test_device(DeviceId, StationId, Trigger) ->
    ?LOG(info, "[AUTO_TEST] 设备自动化测试已屏蔽: DeviceId=~s, StationId=~p, Trigger=~s",
         [DeviceId, StationId, Trigger]),
    {ok, "设备测试已屏蔽"}.

%% @doc 查找工位的PLC客户端进程
find_plc_client_for_station(StationId) ->
    ?LOG(info, "查找工位 ~p 的PLC客户端进程", [StationId]),
    %% 直接使用方案2：通过进程注册名查找PLC客户端
    %% 跳过方案1（通道查找），因为dgiot_channelx:lookup使用通道类型而非ID
    ?LOG(info, "方案: 通过进程注册名查找客户端...", []),
    case find_plc_client_by_pid(StationId) of
        {ok, Pid} ->
            ?LOG(info, "✅ 成功找到PLC客户端: ~p (StationId=~p)", [Pid, StationId]),
            {ok, Pid};
        {error, FindReason} ->
            ?LOG(warning, "❌ 失败: 无法找到工位 ~p 的PLC客户端, FindReason=~p", [StationId, FindReason]),
            {error, no_plc_client_found}
    end.

%% @doc 通过进程注册名查找PLC客户端
find_plc_client_by_pid(StationId) ->
    %% 尝试全局注册名 {plc, StationId}
    case global:whereis_name({plc, StationId}) of
        undefined ->
            ?LOG(info, "全局注册名 {plc, ~p} 未找到", [StationId]),
            {error, not_found};
        Pid ->
            ?LOG(info, "找到全局注册名 {plc, ~p} -> ~p", [StationId, Pid]),
            {ok, Pid}
    end.

%% @doc 处理设备上线事件
handle_device_online(DeviceId) ->
    ?LOG(info, "[AUTO_TEST] 设备上线事件（简化版）: DeviceId=~s", [DeviceId]),
    ok.

%% @doc 处理设备离线事件
handle_device_offline(DeviceId) ->
    ?LOG(info, "[AUTO_TEST] 设备离线事件（简化版）: DeviceId=~s", [DeviceId]),
    ok.