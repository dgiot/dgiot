%%%-------------------------------------------------------------------
%%% @doc
%%% 舵面数据汇聚在线测试脚本
%%% @end
%%%-------------------------------------------------------------------
-module(test_aggregation_online).
-export([
    test/0,
    test_basic_aggregation/0,
    test_surface_data_flow/0,
    check_aggregator_status/0
]).

test() ->
    io:format("========================================~n", []),
    io:format("开始舵面数据汇聚在线测试~n", []),
    io:format("========================================~n", []),
    
    check_aggregator_status(),
    test_basic_aggregation(),
    test_surface_data_flow(),
    
    io:format("========================================~n", []),
    io:format("测试完成！~n", []),
    io:format("========================================~n", []),
    ok.

%% @doc 检查aggregator状态
check_aggregator_status() ->
    io:format("~n", []),
    io:format("1. 检查 aggregator 进程状态~n", []),
    io:format("========================================~n", []),
    
    case whereis(dgiot_uav_aggregator) of
        undefined ->
            io:format("⚠️  aggregator 进程未启动~n", []),
            io:format("   可能原因：系统需要重启以启动监督树中的进程~n", []),
            io:format("   建议：make run 重启系统~n", []);
        Pid when is_pid(Pid) ->
            io:format("✅ aggregator 进程已启动: ~p~n", [Pid]),
            
            % 检查ETS表
            case ets:info(uav_aggregate_cache) of
                undefined ->
                    io:format("❌ uav_aggregate_cache 表不存在~n", []);
                Info ->
                    io:format("✅ uav_aggregate_cache 表存在~n", []),
                    CacheSize = proplists:get_value(size, Info, 0),
                    io:format("   缓存大小: ~p 条~n", [CacheSize])
            end,
            
            case ets:info(uav_aggregate_state) of
                undefined ->
                    io:format("❌ uav_aggregate_state 表不存在~n", []);
                Info2 ->
                    io:format("✅ uav_aggregate_state 表存在~n", []),
                    StateSize = proplists:get_value(size, Info2, 0),
                    io:format("   状态大小: ~p 条~n", [StateSize])
            end
    end,
    
    ok.

%% @doc 测试基本汇聚功能
test_basic_aggregation() ->
    ?LOG(info, "", []),
    ?LOG(info, "2. 测试基本汇聚功能", []),
    ?LOG(info, "========================================", []),
    
    % 准备测试数据
    TestDroneId = <<"test_drone_online">>,
    TestProductId = <<"6235befb62">>,
    TestData = #{
        <<"zqy_acceleration_x">> => 0.123,
        <<"zqy_angular_x">> => 0.045,
        <<"zqy_roll">> => 5.67,
        <<"zqy_temperature">> => 26.5
    },
    
    ?LOG(info, "发送测试数据到 aggregator", []),
    ?LOG(info, "DroneId: ~s", [TestDroneId]),
    ?LOG(info, "ProductId: ~s", [TestProductId]),
    ?LOG(info, "数据: ~p", [TestData]),
    
    % 发送汇聚数据
    dgiot_uav_aggregator:aggregate(
        TestDroneId,
        TestProductId,
        TestData,
        erlang:system_time(millisecond)
    ),
    
    ?LOG(info, "✅ 数据已发送", []),
    
    % 等待1秒让数据写入
    ?LOG(info, "等待1秒...", []),
    timer:sleep(1500),
    
    % 检查缓存表
    CacheCount = dgiot_uav_aggregator:get_cached_count(),
    ?LOG(info, "缓存数量: ~p", [CacheCount]),
    
    % 检查最新状态
    case dgiot_uav_aggregator:get_latest_state(TestDroneId) of
        {ok, LatestData} ->
            ?LOG(info, "✅ 获取最新状态成功", []),
            ?LOG(info, "状态数据: ~p", [LatestData]);
        {error, Reason} ->
            ?LOG(error, "❌ 获取最新状态失败: ~p", [Reason])
    end,
    
    ok.

%% @doc 测试舵面数据完整流程
test_surface_data_flow() ->
    ?LOG(info, "", []),
    ?LOG(info, "3. 测试舵面数据完整流程", []),
    ?LOG(info, "========================================", []),
    
    % 模拟治具发送的原始数据（Modbus格式）
    % SlaveId: 1, Function: 3, ByteCount: 48, Payload: 96 bytes, CRC: 2 bytes
    <<SlaveId:8, 3:8, 48:8, Payload:96/binary, Crc:16>> = build_modbus_surface_data(),
    
    RawData = <<SlaveId:8, 3:8, 48:8, Payload/binary, Crc:16>>,
    
    ?LOG(info, "模拟治具发送原始数据", []),
    ?LOG(info, "SlaveId: ~p", [SlaveId]),
    ?LOG(info, "Function: 3 (读保持寄存器)"),
    ?LOG(info, "ByteCount: 48"),
    ?LOG(info, "RawData size: ~p bytes", [byte_size(RawData)]),
    ?LOG(info, "RawData (hex): ~p", [binary:encode_hex(RawData)]),
    
    % 调用 surface service 处理
    ProductId = <<"6235befb62">>,
    DevAddr = <<"192.168.100.45_10001">>,
    
    ?LOG(info, "调用 uav_surface_service:handle_surface_data", []),
    ?LOG(info, "ProductId: ~s", [ProductId]),
    ?LOG(info, "DevAddr: ~s", [DevAddr]),
    
    % 设置设备类型ID（用于位置映射）
    put(device_type_id, <<"wrj_dm_zqy">>),
    
    % 处理数据
    uav_surface_service:handle_surface_data(ProductId, DevAddr, RawData),
    
    ?LOG(info, "✅ 数据处理完成", []),
    ?LOG(info, "请查看日志确认完整流程", []),
    
    % 清理
    erase(device_type_id),
    
    ok.

%% @doc 构建Modbus舵面数据
build_modbus_surface_data() ->
    % 模拟舵面传感器的Modbus寄存器数据
    % 地址0x34-0x40，每个地址2字节，共13个寄存器 = 26字节
    % 但实际Payload是96字节，说明有更多数据
    
    % 简化：构造一些测试数据
    Regs = lists:map(fun(_) ->
        % 随机生成寄存器值（0-65535）
        rand:uniform(65535)
    end, lists:seq(1, 48)),  % 48个寄存器 = 96字节
    
    % 将寄存器值转换为二进制
    Payload = list_to_binary([<<Reg:16>> || Reg <- Regs]),
    
    % 计算CRC16
    <<SlaveId:8, Func:8, ByteCount:8>> = <<1:8, 3:8, 48:8>>,
    DataWithoutCrc = <<SlaveId:8, Func:8, ByteCount:8, Payload/binary>>,
    Crc = calculate_crc(DataWithoutCrc),
    
    <<SlaveId:8, Func:8, ByteCount:8, Payload/binary, Crc:16>>.

%% @doc 计算CRC16
calculate_crc(Data) ->
    calculate_crc(Data, 16#FFFF).

calculate_crc(<<>>, Crc) ->
    Crc;
calculate_crc(<<Byte:8, Rest/binary>>, Crc) ->
    Crc1 = Crc bxor Byte,
    Crc2 = calculate_crc_byte(Crc1, 8),
    calculate_crc(Rest, Crc2).

calculate_crc_byte(Crc, 0) ->
    Crc;
calculate_crc_byte(Crc, N) ->
    Crc1 = case Crc band 1 of
        1 -> (Crc bsr 1) bxor 16#A001;
        0 -> Crc bsr 1
    end,
    calculate_crc_byte(Crc1, N - 1).
