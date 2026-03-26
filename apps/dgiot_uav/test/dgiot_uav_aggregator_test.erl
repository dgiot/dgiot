%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_aggregator_test - 数据汇聚模块测试
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_aggregator_test).

-export([
    test/0,
    test_aggregate/0,
    test_flush/0,
    test_get_latest_state/0
]).

-include_lib("dgiot/include/logger.hrl").

%% @doc 运行所有测试
test() ->
    ?LOG(info, "========================================"),
    ?LOG(info, "开始测试数据汇聚模块"),
    ?LOG(info, "========================================"),
    
    ok = test_aggregate(),
    ok = test_get_latest_state(),
    ok = test_flush(),
    
    ?LOG(info, "========================================"),
    ?LOG(info, "所有测试完成"),
    ?LOG(info, "========================================"),
    ok.

%% @doc 测试数据汇聚功能
test_aggregate() ->
    ?LOG(info, "[TEST] 测试数据汇聚..."),
    
    %% 模拟舵面数据
    DroneId = <<"drone_001">>,
    ProductId = <<"6235befb62">>,
    Timestamp = erlang:system_time(millisecond),
    
    SurfaceData = #{
        <<"zqy_acceleration_x">> => 0.5,
        <<"zqy_acceleration_y">> => 0.3,
        <<"zqy_acceleration_z">> => 9.8,
        <<"zqy_angular_x">> => 0.01,
        <<"zqy_angular_y">> => 0.02,
        <<"zqy_angular_z">> => 0.03,
        <<"zqy_roll">> => 5.0,
        <<"zqy_pitch">> => 3.0,
        <<"zqy_yaw">> => 180.0,
        <<"zqy_temperature">> => 25.5
    },
    
    %% 调用汇聚接口
    dgiot_uav_aggregator:aggregate(DroneId, ProductId, SurfaceData, Timestamp),
    
    ?LOG(info, "[TEST] 数据汇聚完成: DroneId=~s, 字段数=~p", [DroneId, maps:size(SurfaceData)]),
    
    %% 等待一段时间让数据写入缓存
    timer:sleep(100),
    
    %% 检查缓存数量
    CacheCount = dgiot_uav_aggregator:get_cached_count(),
    ?LOG(info, "[TEST] 缓存数量: ~p", [CacheCount]),
    
    ok.

%% @doc 测试获取最新状态
test_get_latest_state() ->
    ?LOG(info, "[TEST] 测试获取最新状态..."),
    
    DroneId = <<"drone_001">>,
    
    case dgiot_uav_aggregator:get_latest_state(DroneId) of
        {ok, Data} ->
            ?LOG(info, "[TEST] 获取到最新状态: 字段数=~p", [maps:size(Data)]),
            ?LOG(debug, "[TEST] 数据内容: ~p", [Data]),
            ok;
        {error, Reason} ->
            ?LOG(error, "[TEST] 获取状态失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 测试手动刷新
test_flush() ->
    ?LOG(info, "[TEST] 测试手动刷新..."),
    
    %% 等待定时刷新
    timer:sleep(1100),
    
    %% 检查缓存是否被清空
    CacheCount = dgiot_uav_aggregator:get_cached_count(),
    ?LOG(info, "[TEST] 刷新后缓存数量: ~p", [CacheCount]),
    
    ok.
