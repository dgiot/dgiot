%%%-------------------------------------------------------------------
%%% @doc
%%% 验证舵面数据汇聚是否成功的测试脚本
%%% @end
%%%-------------------------------------------------------------------
-module(test_surface_aggregation).
-export([test/0, verify_aggregator_state/0, test_manual_aggregate/0]).

test() ->
    ?LOG(info, "===== 开始验证舵面数据汇聚 ====="),
    verify_aggregator_state(),
    test_manual_aggregate(),
    ?LOG(info, "===== 验证完成 ====="),
    ok.

%% @doc 验证汇聚模块状态
verify_aggregator_state() ->
    ?LOG(info, "1. 检查 aggregator 进程状态"),
    case whereis(dgiot_uav_aggregator) of
        undefined ->
            ?LOG(error, "❌ aggregator 进程未启动！请先重启系统");
        Pid when is_pid(Pid) ->
            ?LOG(info, "✅ aggregator 进程已启动: ~p", [Pid]),
            % 检查ETS表
            case ets:info(uav_aggregate_cache) of
                undefined ->
                    ?LOG(error, "❌ uav_aggregate_cache 表不存在");
                Info ->
                    ?LOG(info, "✅ uav_aggregate_cache 表存在, 大小: ~p", [proplists:get_value(size, Info)]),
                    CacheData = ets:tab2list(uav_aggregate_cache),
                    ?LOG(info, "   缓存数据条数: ~p", [length(CacheData)]),
                    case CacheData of
                        [] -> ?LOG(warning, "   ⚠️  缓存为空，暂无数据汇聚");
                        _ -> ?LOG(info, "   📄 缓存数据示例: ~p", [hd(CacheData)])
                    end
            end,
            case ets:info(uav_aggregate_state) of
                undefined ->
                    ?LOG(error, "❌ uav_aggregate_state 表不存在");
                Info2 ->
                    ?LOG(info, "✅ uav_aggregate_state 表存在, 大小: ~p", [proplists:get_value(size, Info2)]),
                    StateData = ets:tab2list(uav_aggregate_state),
                    ?LOG(info, "   状态数据条数: ~p", [length(StateData)]),
                    case StateData of
                        [] -> ?LOG(warning, "   ⚠️  状态为空，暂无设备状态");
                        _ -> 
                            ?LOG(info, "   📄 状态数据示例: ~p", [hd(StateData)]),
                            lists:foreach(
                                fun({DroneId, _Timestamp, Data}) ->
                                    ?LOG(info, "   设备 ~s 的状态字段数: ~p", [DroneId, maps:size(Data)]),
                                    ?LOG(info, "   状态字段: ~p", [maps:keys(Data)])
                                end,
                                StateData
                            )
                    end
            end
    end,
    ok.

%% @doc 手动测试汇聚功能
test_manual_aggregate() ->
    ?LOG(info, "2. 手动测试数据汇聚"),
    
    % 模拟舵面数据（左前舵）
    TestData = #{
        <<"zqy_acceleration_x">> => 0.5,
        <<"zqy_angular_x">> => 0.01,
        <<"zqy_roll">> => 5.0,
        <<"zqy_temperature">> => 25.5
    },
    
    % 尝试查找一个真实的无人机设备
    case dgiot_device:lookup(<<"wrj_dm_zqy">>) of
        {ok, DeviceInfo} ->
            DeviceId = maps:get(<<"objectId">>, DeviceInfo),
            ProductId = maps:get(<<"productid">>, DeviceInfo),
            ?LOG(info, "找到舵面设备: DeviceId=~s, ProductId=~s", [DeviceId, ProductId]),
            
            % 获取无人机ID（通过工位绑定）
            case dgiot_uav_station_manager:get_drone_by_station(<<"D1">>) of
                {ok, DroneId} ->
                    ?LOG(info, "找到无人机ID: ~s", [DroneId]),
                    
                    % 获取产品ID（从无人机设备）
                    {ok, DroneInfo} = dgiot_device:lookup(DroneId),
                    DroneProductId = maps:get(<<"productid">>, DroneInfo),
                    
                    % 发送汇聚数据
                    ?LOG(info, "发送测试数据到 aggregator..."),
                    dgiot_uav_aggregator:aggregate(DroneId, DroneProductId, TestData, erlang:system_time(millisecond)),
                    
                    ?LOG(info, "✅ 测试数据已发送"),
                    ?LOG(info, "   请稍等1秒后查看缓存表：ets:tab2list(uav_aggregate_cache)"),
                    ?LOG(info, "   查看状态表：ets:tab2list(uav_aggregate_state)"),
                    
                    timer:sleep(1500),
                    
                    % 再次检查状态
                    case dgiot_uav_aggregator:get_latest_state(DroneId) of
                        {ok, LatestData} ->
                            ?LOG(info, "✅ 汇聚成功！最新状态: ~p", [LatestData]);
                        {error, Reason} ->
                            ?LOG(error, "❌ 获取最新状态失败: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?LOG(warning, "⚠️  未找到无人机ID: ~p", [Reason]),
                    ?LOG(info, "   尝试使用测试设备ID..."),
                    TestDroneId = <<"test_drone_001">>,
                    TestProductId = <<"6235befb62">>,
                    dgiot_uav_aggregator:aggregate(TestDroneId, TestProductId, TestData, erlang:system_time(millisecond)),
                    ?LOG(info, "✅ 测试数据已发送（测试设备）")
            end;
        {error, Reason} ->
            ?LOG(warning, "⚠️  未找到舵面设备: ~p", [Reason]),
            ?LOG(info, "   使用测试设备ID..."),
            TestDroneId = <<"test_drone_001">>,
            TestProductId = <<"6235befb62">>,
            dgiot_uav_aggregator:aggregate(TestDroneId, TestProductId, TestData, erlang:system_time(millisecond)),
            ?LOG(info, "✅ 测试数据已发送（测试设备）"),
            timer:sleep(1500),
            case dgiot_uav_aggregator:get_latest_state(TestDroneId) of
                {ok, LatestData} ->
                    ?LOG(info, "✅ 汇聚成功！最新状态: ~p", [LatestData]);
                {error, Reason2} ->
                    ?LOG(error, "❌ 获取最新状态失败: ~p", [Reason2])
            end
    end,
    ok.
