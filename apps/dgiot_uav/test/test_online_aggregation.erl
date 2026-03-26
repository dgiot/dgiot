%%%-------------------------------------------------------------------
%%% @doc
%%% 舵面数据汇聚在线测试脚本
%%% @end
%%%-------------------------------------------------------------------
-module(test_online_aggregation).
-export([test/0]).

test() ->
    io:format("========================================~n", []),
    io:format("舵面数据汇聚在线测试~n", []),
    io:format("========================================~n~n", []),

    % 1. 检查aggregator进程
    io:format("1. 检查 aggregator 进程~n", []),
    case whereis(dgiot_uav_aggregator) of
        undefined ->
            io:format("❌ aggregator 进程未启动~n", []),
            io:format("   需要重启系统: make run~n", []);
        Pid ->
            io:format("✅ aggregator 进程已启动: ~p~n", [Pid]),

            % 2. 测试基本汇聚
            io:format("~n2. 测试基本汇聚功能~n", []),
            io:format("========================================~n", []),
            TestDroneId = <<"test_drone_online">>,
            TestProductId = <<"6235befb62">>,
            TestData = #{
                <<"zqy_acceleration_x">> => 0.123,
                <<"zqy_angular_x">> => 0.045
            },

            io:format("发送测试数据: ~p~n", [TestData]),
            dgiot_uav_aggregator:aggregate(
                TestDroneId,
                TestProductId,
                TestData,
                erlang:system_time(millisecond)
            ),
            io:format("✅ 数据已发送~n", []),

            timer:sleep(1500),

            % 3. 查看缓存
            CacheCount = dgiot_uav_aggregator:get_cached_count(),
            io:format("缓存数量: ~p~n", [CacheCount]),

            % 4. 查看状态
            case dgiot_uav_aggregator:get_latest_state(TestDroneId) of
                {ok, Data} ->
                    io:format("✅ 最新状态: ~p~n", [Data]);
                {error, Reason} ->
                    io:format("❌ 获取状态失败: ~p~n", [Reason])
            end
    end,

    io:format("~n========================================~n", []),
    io:format("测试完成~n", []),
    io:format("========================================~n", []),
    ok.
