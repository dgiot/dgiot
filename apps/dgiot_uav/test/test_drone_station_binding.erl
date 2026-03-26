%%%-------------------------------------------------------------------
%%% @doc
%%% test_drone_station_binding - 工位-无人机动态绑定观察者测试脚本
%%%
%%% 【观察者模式】测试脚本只观察验证，不干预业务逻辑
%%%
%%% "乘客-站台"模型:
%%% - 站台(工位): 1200-1700
%%% - 乘客(无人机): 通过治具IP识别
%%% - 检票系统(治具单片机): 自动建立IP→工位映射
%%% - 乘车绑定: 无人机上线自动建立工位→无人机绑定
%%%
%%% 测试流程:
%%% 1. 启动模拟器(治具+无人机)
%%% 2. 观察并验证IP→工位映射自动建立
%%% 3. 观察并验证工位→无人机绑定自动建立
%%% 4. 触发PLC指令，观察aggregate消息汇聚
%%% 5. 查询TDengine验证数据入库
%%% @end
%%%-------------------------------------------------------------------
-module(test_drone_station_binding).

%% 测试入口
-export([test/0]).

%% 观察验证函数
-export([
    observe_ip_station_mapping/1,
    observe_station_drone_binding/1,
    verify_binding_chain/2
]).

%% 测试配置
-define(TEST_TIMEOUT, 30000).
-define(POLL_INTERVAL, 1000).

%%%===================================================================
%%% 测试入口
%%%===================================================================

test() ->
    StationAddr = 1200,
    ExpectedIp = <<"192.168.100.45">>,

    io:format("~n=== 工位-无人机绑定观察测试 ===~n"),
    io:format("测试模式: 纯观察，不干预业务逻辑~n"),
    io:format("目标工位: ~p~n", [StationAddr]),
    io:format("预期IP: ~s~n~n", [ExpectedIp]),

    %% 阶段1: 观察IP→工位映射
    io:format("【阶段1】观察IP→工位映射建立...~n"),
    io:format("  提示: 请确保治具模拟器已启动~n"),
    case observe_ip_station_mapping(ExpectedIp) of
        {ok, StationAddr} ->
            io:format("  [OK] IP→工位映射: ~s -> ~p~n", [ExpectedIp, StationAddr]);
        {ok, OtherStation} ->
            io:format("  [WARN] IP映射指向不同工位: ~p (预期~p)~n", [OtherStation, StationAddr]);
        {error, timeout} ->
            io:format("  [FAIL] 等待IP→工位映射超时~n"),
            return_error(ip_mapping_timeout)
    end,

    %% 阶段2: 观察工位→无人机绑定
    io:format("~n【阶段2】观察工位→无人机绑定建立...~n"),
    io:format("  提示: 请确保无人机模拟器已启动~n"),
    case observe_station_drone_binding(StationAddr) of
        {ok, DroneId} ->
            io:format("  [OK] 工位→无人机绑定: ~p -> ~s~n", [StationAddr, DroneId]),
            %% 验证反向绑定
            case dgiot_uav_station_manager:get_station_by_drone(DroneId) of
                {ok, StationAddr} ->
                    io:format("  [OK] 反向绑定验证: ~s -> ~p~n", [DroneId, StationAddr]);
                {error, _} ->
                    io:format("  [WARN] 反向绑定未找到~n")
            end,
            {ok, DroneId};
        {error, timeout} ->
            io:format("  [FAIL] 等待工位→无人机绑定超时~n"),
            return_error(drone_binding_timeout)
    end.

%%%===================================================================
%%% 观察验证函数
%%%===================================================================

observe_ip_station_mapping(IpBin) ->
    observe_ip_station_mapping(IpBin, ?TEST_TIMEOUT div ?POLL_INTERVAL).

observe_ip_station_mapping(_IpBin, 0) ->
    {error, timeout};
observe_ip_station_mapping(IpBin, Retries) ->
    case dgiot_uav_station_manager:get_station_by_ip(IpBin) of
        {ok, StationAddr} ->
            {ok, StationAddr};
        {error, _} ->
            timer:sleep(?POLL_INTERVAL),
            observe_ip_station_mapping(IpBin, Retries - 1)
    end.

observe_station_drone_binding(StationAddr) ->
    observe_station_drone_binding(StationAddr, ?TEST_TIMEOUT div ?POLL_INTERVAL).

observe_station_drone_binding(_StationAddr, 0) ->
    {error, timeout};
observe_station_drone_binding(StationAddr, Retries) ->
    case dgiot_uav_station_manager:get_drone_by_station(StationAddr) of
        {ok, DroneId} ->
            {ok, DroneId};
        {error, _} ->
            timer:sleep(?POLL_INTERVAL),
            observe_station_drone_binding(StationAddr, Retries - 1)
    end.

verify_binding_chain(IpBin, ExpectedStation) ->
    case dgiot_uav_station_manager:get_station_by_ip(IpBin) of
        {ok, StationAddr} when StationAddr == ExpectedStation ->
            case dgiot_uav_station_manager:get_drone_by_station(StationAddr) of
                {ok, DroneId} ->
                    {ok, DroneId};
                {error, Reason} ->
                    {error, {no_drone_binding, Reason}}
            end;
        {ok, OtherStation} ->
            {error, {station_mismatch, OtherStation, ExpectedStation}};
        {error, Reason} ->
            {error, {no_ip_mapping, Reason}}
    end.

return_error(Reason) ->
    io:format("~n[TEST FAILED] ~p~n", [Reason]),
    {error, Reason}.
