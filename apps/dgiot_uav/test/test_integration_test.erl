%%%-------------------------------------------------------------------
%%% @doc
%%% 无人机测试项管理系统集成测试
%%% 
%%% 测试整个系统的集成功能
%%%-------------------------------------------------------------------
-module(test_integration_test).
-author("johnliu").
-include("dgiot_uav_test_item.hrl").

%% API
-export([run_all_tests/0]).

%% 测试配置
-define(TEST_DEVICE_ID, <<"test_device_001">>).
-define(TEST_PRODUCT_ID, <<"343cf21f82">>).
-define(TEST_STATION_ID, 1500).
-define(TEST_STATION_NAME, <<"总测1"/utf8>>).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 运行所有集成测试
run_all_tests() ->
    io:format("=== 开始无人机测试项管理系统集成测试 ===~n~n"),
    
    %% 初始化测试环境
    init_test_environment(),
    
    %% 运行测试用例
    TestResults = [
        test_target_to_slave_addr_mapping(),
        test_test_dispatcher_integration(),
        test_handler_integration(),
        test_device_integration()
    ],
    
    %% 统计测试结果
    Passed = lists:filter(fun({_, Result}) -> Result =:= passed end, TestResults),
    Failed = lists:filter(fun({_, Result}) -> Result =:= failed end, TestResults),
    
    io:format("~n=== 测试结果汇总 ===~n"),
    io:format("总测试数: ~p~n", [length(TestResults)]),
    io:format("通过数: ~p~n", [length(Passed)]),
    io:format("失败数: ~p~n", [length(Failed)]),
    
    case Failed of
        [] ->
            io:format("~n✅ 所有测试通过！~n"),
            ok;
        _ ->
            io:format("~n❌ 有测试失败：~n"),
            lists:foreach(fun({TestName, _}) ->
                io:format("  - ~s~n", [TestName])
            end, Failed),
            {error, "部分测试失败"}
    end.

%%%===================================================================
%%% 测试用例
%%%===================================================================

%% @doc 测试target_type到slave_address的映射
test_target_to_slave_addr_mapping() ->
    io:format("1. 测试target_type到slave_address映射... "),
    try
        %% 测试PLC映射
        51 = dgiot_uav_command_scheduler:target_to_slave_addr(<<"plc">>),
        
        %% 测试治具映射
        52 = dgiot_uav_command_scheduler:target_to_slave_addr(<<"fixture">>),
        
        %% 测试无人机映射
        10007 = dgiot_uav_command_scheduler:target_to_slave_addr(<<"uav">>),
        
        %% 测试默认值
        0 = dgiot_uav_command_scheduler:target_to_slave_addr(<<"unknown">>),
        
        io:format("✅ 通过~n"),
        {target_mapping, passed}
    catch
        _:Error ->
            io:format("❌ 失败: ~p~n", [Error]),
            {target_mapping, failed}
    end.

%% @doc 测试测试调度器集成
test_test_dispatcher_integration() ->
    io:format("2. 测试测试调度器集成... "),
    try
        %% 初始化测试调度器
        dgiot_uav_test_dispatcher:init(),
        
        %% 创建测试项
        _TestItem = #test_item{
            object_id = <<"test_item_001">>,
            device_id = ?TEST_DEVICE_ID,
            devaddr = <<"D1500">>,
            name = <<"集成测试项">>,
            product_id = ?TEST_PRODUCT_ID,
            content = #{
                <<"steps">> => [
                    #{
                        <<"step_number">> => 1,
                        <<"action_type">> => <<"send_command">>,
                        <<"target">> => <<"plc">>,
                        <<"send">> => #{<<"command">> => 1, <<"value">> => 100},
                        <<"expect">> => #{}
                    },
                    #{
                        <<"step_number">> => 2,
                        <<"action_type">> => <<"wait">>,
                        <<"target">> => <<"">>,
                        <<"send">> => #{<<"duration">> => 1000},
                        <<"expect">> => #{}
                    }
                ]
            },
            common_params = #test_item_common_params{
                port = 10007,
                station_name = ?TEST_STATION_NAME,
                station_number = ?TEST_STATION_ID,
                test_station_name = ?TEST_STATION_NAME
            },
            is_test_item_device = true,
            last_updated = erlang:system_time(second),
            test_item_count = 2,
            status = <<"active">>,
            created_at = erlang:system_time(second),
            updated_at = erlang:system_time(second)
        },
        
        %% 启动测试
        {ok, TestId} = dgiot_uav_test_dispatcher:start_test(<<"test_item_001">>, #{
            <<"device_id">> => ?TEST_DEVICE_ID
        }),
        
        %% 获取测试状态
        {ok, Status} = dgiot_uav_test_dispatcher:get_test_status(TestId),
        
        %% 验证状态
        true = is_binary(maps:get(<<"test_id">>, Status)),
        <<"running">> = maps:get(<<"status">>, Status),
        
        %% 停止测试
        ok = dgiot_uav_test_dispatcher:stop_test(TestId),
        
        io:format("✅ 通过~n"),
        {test_dispatcher, passed}
    catch
        _:Error ->
            io:format("❌ 失败: ~p~n", [Error]),
            {test_dispatcher, failed}
    end.

%% @doc 测试handler集成
test_handler_integration() ->
    io:format("3. 测试handler集成... "),
    try
        %% 测试handler函数（模拟调用）
        %% 注意：这里不实际调用handler，只是验证模块存在
        true = is_atom(dgiot_uav_handler),
        
        %% 验证路由路径
        Routes = dgiot_uav_handler:module_info(exports),
        true = lists:member({swagger_uav, 0}, Routes),
        true = lists:member({handle, 4}, Routes),
        
        io:format("✅ 通过~n"),
        {handler_integration, passed}
    catch
        _:Error ->
            io:format("❌ 失败: ~p~n", [Error]),
            {handler_integration, failed}
    end.

%% @doc 测试设备集成
test_device_integration() ->
    io:format("4. 测试设备集成... "),
    try
        %% 初始化集成模块
        dgiot_uav_test_integration:init(),
        
        %% 注册设备测试关联
        TestId = <<"test_execution_001">>,
        dgiot_uav_test_integration:register_device_test(?TEST_DEVICE_ID, TestId),
        
        %% 验证设备测试关联
        {ok, TestId} = dgiot_uav_test_integration:get_test_by_device(?TEST_DEVICE_ID),
        
        %% 存储测试结果
        TestResult = #{
            <<"test_id">> => TestId,
            <<"device_id">> => ?TEST_DEVICE_ID,
            <<"status">> => <<"completed">>,
            <<"results">> => [#{<<"step">> => 1, <<"status">> => <<"passed">>}],
            <<"last_update">> => erlang:system_time(millisecond)
        },
        ok = dgiot_uav_test_integration:store_test_result(TestId, TestResult),
        
        %% 获取设备测试结果
        {ok, Results} = dgiot_uav_test_integration:get_test_results_by_device(?TEST_DEVICE_ID),
        true = is_list(Results),
        
        %% 取消设备测试关联
        ok = dgiot_uav_test_integration:unregister_device_test(?TEST_DEVICE_ID, TestId),
        
        %% 验证关联已取消
        {error, not_found} = dgiot_uav_test_integration:get_test_by_device(?TEST_DEVICE_ID),
        
        io:format("✅ 通过~n"),
        {device_integration, passed}
    catch
        _:Error ->
            io:format("❌ 失败: ~p~n", [Error]),
            {device_integration, failed}
    end.

%%%===================================================================
%%% 辅助函数
%%%===================================================================

%% @doc 初始化测试环境
init_test_environment() ->
    %% 初始化测试调度器
    dgiot_uav_test_dispatcher:init(),
    
    %% 初始化集成模块
    dgiot_uav_test_integration:init(),
    
    %% 初始化测试项缓存
    dgiot_uav_test_item_cache:init(),
    
    io:format("测试环境初始化完成~n~n").

%% @doc 清理测试环境
cleanup_test_environment() ->
    %% 清理ETS表
    ets:delete(test_executions),
    ets:delete(uav_test_results),
    ets:delete(device_test_mapping),
    ets:delete(test_item_cache),
    
    ok.