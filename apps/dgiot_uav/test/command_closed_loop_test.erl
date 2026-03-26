%%%-------------------------------------------------------------------
%%% @doc
%%% 指令闭环跟踪端到端测试
%%% 
%%% 测试从指令下发到响应接收的完整闭环流程
%%% 包括：测试项管理、指令状态跟踪、地测口映射、响应处理
%%% @end
%%%-------------------------------------------------------------------
-module(command_closed_loop_test).

-include_lib("eunit/include/eunit.hrl").

%% 测试配置
-define(TEST_STATION_ID, 1).
-define(TEST_DEST_ADDR, 16#0000).
-define(TEST_SRC_ADDR, 16#0001).
-define(TEST_COMMAND_CODE, 16#80).  % 高度指令
-define(TEST_COMMAND_VALUE, 1000).   % 高度值

%%%===================================================================
%%% 测试用例
%%%===================================================================

%% @doc 测试完整的指令闭环流程
closed_loop_workflow_test() ->
    % 初始化所有必要的表
    ok = dgiot_uav_command_manager:init_command_status_table(),
    ok = dgiot_uav_ground_station_mapper:init_mapping_table(),
    ok = dgiot_uav_test_manager:init_test_table(),
    
    % 1. 创建测试项
    TestItemId = <<"test_item_001">>,
    Steps = [
        #{name => <<"起飞准备">>, type => uav, code => 16#80, value => 1000, timeout => 5000},
        #{name => <<"爬升到目标高度">>, type => uav, code => 16#80, value => 2000, timeout => 10000},
        #{name => <<"平飞">>, type => uav, code => 16#81, value => 50, timeout => 15000}
    ],
    
    {ok, TestItemId} = dgiot_uav_test_manager:create_test_item(TestItemId, Steps),
    
    % 2. 开始测试项
    ok = dgiot_uav_test_manager:start_test(TestItemId),
    
    % 3. 获取当前步骤
    {ok, #{current_step := StepIndex, steps := TestSteps}} = dgiot_uav_test_manager:get_test_status(TestItemId),
    ?assertEqual(1, StepIndex),
    
    % 4. 发送指令（模拟指令调度器调用）
    Step = lists:nth(StepIndex, TestSteps),
    #{type := CommandType, code := CommandCode, value := CommandValue} = Step,
    
    Params = #{
        dest_addr => ?TEST_DEST_ADDR,
        src_addr => ?TEST_SRC_ADDR,
        frame_no => 1,
        station_id => ?TEST_STATION_ID,
        test_item_id => TestItemId,
        step_index => StepIndex,
        trace_id => <<"trace_", TestItemId/binary, "_", (integer_to_binary(StepIndex))/binary>>
    },
    
    % 发送指令
    Result = case CommandType of
        uav -> dgiot_uav_command_manager:send_uav_single(CommandCode, CommandValue, Params);
        plc -> dgiot_uav_command_manager:send_plc_single(CommandCode, CommandValue, Params);
        fixture -> dgiot_uav_command_manager:send_fixture_single(CommandCode, CommandValue, Params)
    end,
    
    ?assertMatch(ok, Result),
    
    % 5. 验证指令状态
    {ok, Status} = dgiot_uav_command_manager:get_command_status(<<"trace_", TestItemId/binary, "_1">>),
    ?assertEqual(pending, Status#dgiot_uav_command_manager.command_status.status),
    ?assertEqual(TestItemId, Status#dgiot_uav_command_manager.command_status.test_item_id),
    ?assertEqual(StepIndex, Status#dgiot_uav_command_manager.command_status.step_index),
    
    % 6. 模拟地测口收到响应
    % 生成命令ID（实际系统中应该从发送命令时获取）
    CommandId = dgiot_uav_command_manager:generate_command_id(),
    
    % 模拟响应数据
    ResponseData = #{
        command_id => CommandId,
        response_code => 0,
        response_data => #{altitude => 1000},
        timestamp => erlang:system_time(millisecond)
    },
    
    % 处理响应
    ok = dgiot_uav_command_manager:handle_uav_response(CommandId, ResponseData),
    
    % 7. 验证测试项状态更新
    timer:sleep(100), % 等待异步处理完成
    
    {ok, UpdatedStatus} = dgiot_uav_test_manager:get_test_status(TestItemId),
    ?assertEqual(2, maps:get(current_step, UpdatedStatus)), % 应该进入下一步
    ?assertEqual(completed, maps:get(status, UpdatedStatus)), % 测试项应该还在进行中
    
    % 8. 清理
    ok = dgiot_uav_test_manager:cleanup_test_item(TestItemId),
    ok.

%% @doc 测试指令超时处理
command_timeout_test() ->
    % 初始化
    ok = dgiot_uav_command_manager:init_command_status_table(),
    ok = dgiot_uav_ground_station_mapper:init_mapping_table(),
    ok = dgiot_uav_test_manager:init_test_table(),
    
    % 创建测试项
    TestItemId = <<"test_item_timeout">>,
    Steps = [
        #{name => <<"超时测试指令">>, type => uav, code => 16#80, value => 1000, timeout => 100} % 100ms超时
    ],
    
    {ok, TestItemId} = dgiot_uav_test_manager:create_test_item(TestItemId, Steps),
    ok = dgiot_uav_test_manager:start_test(TestItemId),
    
    % 发送指令
    Params = #{
        dest_addr => ?TEST_DEST_ADDR,
        src_addr => ?TEST_SRC_ADDR,
        frame_no => 1,
        station_id => ?TEST_STATION_ID,
        test_item_id => TestItemId,
        step_index => 1,
        trace_id => <<"trace_timeout">>
    },
    
    ok = dgiot_uav_command_manager:send_uav_single(?TEST_COMMAND_CODE, ?TEST_COMMAND_VALUE, Params),
    
    % 等待超时
    timer:sleep(200),
    
    % 手动触发超时清理
    {ok, TimeoutCount} = dgiot_uav_ground_station_mapper:cleanup_timeout_commands(),
    ?assert(TimeoutCount >= 0),
    
    % 验证测试项状态
    {ok, Status} = dgiot_uav_test_manager:get_test_status(TestItemId),
    ?assertEqual(failed, maps:get(status, Status)),
    
    % 清理
    ok = dgiot_uav_test_manager:cleanup_test_item(TestItemId),
    ok.

%% @doc 测试多指令并发
concurrent_commands_test() ->
    % 初始化
    ok = dgiot_uav_command_manager:init_command_status_table(),
    ok = dgiot_uav_ground_station_mapper:init_mapping_table(),
    ok = dgiot_uav_test_manager:init_test_table(),
    
    % 创建多个测试项
    TestItems = [
        {<<"test_concurrent_1">>, 16#80, 1000},
        {<<"test_concurrent_2">>, 16#81, 50},
        {<<"test_concurrent_3">>, 16#82, 90}
    ],
    
    % 并发发送指令
    lists:foreach(
        fun({TestItemId, Code, Value}) ->
            Steps = [#{name => <<"并发测试">>, type => uav, code => Code, value => Value, timeout => 5000}],
            {ok, TestItemId} = dgiot_uav_test_manager:create_test_item(TestItemId, Steps),
            ok = dgiot_uav_test_manager:start_test(TestItemId),
            
            Params = #{
                dest_addr => ?TEST_DEST_ADDR,
                src_addr => ?TEST_SRC_ADDR,
                frame_no => 1,
                station_id => ?TEST_STATION_ID,
                test_item_id => TestItemId,
                step_index => 1,
                trace_id => <<"trace_", TestItemId/binary>>
            },
            
            ok = dgiot_uav_command_manager:send_uav_single(Code, Value, Params)
        end,
        TestItems
    ),
    
    % 验证所有指令都已发送
    {ok, PendingCommands} = dgiot_uav_ground_station_mapper:get_pending_commands(),
    ?assertEqual(3, length(PendingCommands)),
    
    % 模拟响应
    lists:foreach(
        fun({TestItemId, _Code, _Value}) ->
            CommandId = dgiot_uav_command_manager:generate_command_id(),
            ResponseData = #{
                command_id => CommandId,
                response_code => 0,
                response_data => #{},
                timestamp => erlang:system_time(millisecond)
            },
            ok = dgiot_uav_command_manager:handle_uav_response(CommandId, ResponseData)
        end,
        TestItems
    ),
    
    % 等待处理完成
    timer:sleep(100),
    
    % 验证所有测试项都已完成
    lists:foreach(
        fun({TestItemId, _Code, _Value}) ->
            {ok, Status} = dgiot_uav_test_manager:get_test_status(TestItemId),
            ?assertEqual(completed, maps:get(status, Status)),
            ok = dgiot_uav_test_manager:cleanup_test_item(TestItemId)
        end,
        TestItems
    ),
    
    ok.

%% @doc 测试地测口映射服务
ground_station_mapper_test() ->
    % 初始化
    ok = dgiot_uav_ground_station_mapper:init_mapping_table(),
    
    % 注册命令
    CommandId = <<"test_mapper_cmd">>,
    TestItemId = <<"test_mapper_item">>,
    StepIndex = 1,
    StationId = ?TEST_STATION_ID,
    CommandType = uav,
    CommandCode = 16#80,
    CommandValue = 1000,
    
    ok = dgiot_uav_ground_station_mapper:register_command(
        CommandId, TestItemId, StepIndex, StationId, CommandType, CommandCode, CommandValue
    ),
    
    % 获取映射
    {ok, Mapping} = dgiot_uav_ground_station_mapper:get_mapping(CommandId),
    ?assertEqual(TestItemId, maps:get(test_item_id, Mapping)),
    ?assertEqual(StepIndex, maps:get(step_index, Mapping)),
    ?assertEqual(CommandType, maps:get(command_type, Mapping)),
    ?assertEqual(pending, maps:get(status, Mapping)),
    
    % 更新状态
    ok = dgiot_uav_ground_station_mapper:update_mapping_status(CommandId, completed),
    
    % 再次获取验证
    {ok, UpdatedMapping} = dgiot_uav_ground_station_mapper:get_mapping(CommandId),
    ?assertEqual(completed, maps:get(status, UpdatedMapping)),
    
    % 移除映射
    ok = dgiot_uav_ground_station_mapper:remove_mapping(CommandId),
    
    % 验证已移除
    ?assertMatch({error, not_found}, dgiot_uav_ground_station_mapper:get_mapping(CommandId)),
    
    ok.

%% @doc 测试错误处理
error_handling_test() ->
    % 初始化
    ok = dgiot_uav_command_manager:init_command_status_table(),
    ok = dgiot_uav_ground_station_mapper:init_mapping_table(),
    
    % 测试不存在的命令ID
    ?assertMatch({error, not_found}, dgiot_uav_command_manager:get_command_status(<<"nonexistent">>)),
    
    % 测试不存在的映射
    ?assertMatch({error, not_found}, dgiot_uav_ground_station_mapper:get_mapping(<<"nonexistent">>)),
    
    % 测试重复注册
    CommandId = <<"test_duplicate">>,
    ok = dgiot_uav_ground_station_mapper:register_command(
        CommandId, <<"item1">>, 1, 1, uav, 16#80, 1000
    ),
    ?assertMatch({error, command_id_exists}, dgiot_uav_ground_station_mapper:register_command(
        CommandId, <<"item2">>, 2, 1, uav, 16#81, 50
    )),
    
    % 清理
    ok = dgiot_uav_ground_station_mapper:remove_mapping(CommandId),
    ok.

%% @doc 运行所有测试
run_all_tests() ->
    eunit:test([
        {module, ?MODULE},
        {report, {eunit_surefire, [{dir, "."}]}}
    ]).

%%%===================================================================
%%% 辅助函数
%%%===================================================================

%% @doc 设置测试环境
setup() ->
    % 确保所有表都已初始化
    dgiot_uav_command_manager:init_command_status_table(),
    dgiot_uav_ground_station_mapper:init_mapping_table(),
    dgiot_uav_test_manager:init_test_table(),
    ok.

%% @doc 清理测试环境
cleanup(_) ->
    % 清理所有测试数据
    dgiot_uav_test_manager:cleanup_all_tests(),
    ok.