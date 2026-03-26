-module(test_message_flow).
-export([test/0]).

test() ->
    io:format("========== 测试消息流 ==========~n"),
    
    % 1. 查找UAVPLC通道
    ChannelType = <<"UAVPLCC">>,
    case dgiot_parse:query_object(<<"Channel">>, #{<<"where">> => #{<<"cType">> => ChannelType}}) of
        {ok, #{<<"results">> := Results}} when is_list(Results) andalso length(Results) > 0 ->
            Channel = lists:nth(1, Results),
            ChannelId = maps:get(<<"objectId">>, Channel),
            ChannelName = maps:get(<<"name">>, Channel, <<"未命名">>),
            io:format("1. 找到UAVPLC通道: ~s (~s)~n", [binary_to_list(ChannelId), binary_to_list(ChannelName)]),
            
            % 2. 创建测试设备
            DeviceId = <<"test_uav_device_001">>,
            DeviceInfo = #{
                <<"name">> => <<"测试无人机设备">>,
                <<"product">> => <<"343cf21f82">>,
                <<"status">> => <<"online">>,
                <<"station_id">> => 1200,
                <<"ip">> => <<"192.168.100.21">>
            },
            
            % 3. 发送设备上线消息
            io:format("2. 发送设备上线消息...~n"),
            Message = {device_online, DeviceId, DeviceInfo},
            case dgiot_channelx:do_message(ChannelType, ChannelId, Message) of
                ok ->
                    io:format("   消息发送成功~n");
                {error, Reason} ->
                    io:format("   消息发送失败: ~p~n", [Reason])
            end,
            
            % 4. 发送测试指令
            io:format("3. 发送测试指令...~n"),
            TestCommand = {send_command, DeviceId, #{
                <<"action">> => <<"start_test">>,
                <<"station_id">> => 1200,
                <<"test_type">> => <<"磁航向测试">>,
                <<"plc_address">> => 1720,
                <<"value">> => 1
            }},
            case dgiot_channelx:do_message(ChannelType, ChannelId, TestCommand) of
                ok ->
                    io:format("   指令发送成功~n");
                {error, Reason2} ->
                    io:format("   指令发送失败: ~p~n", [Reason2])
            end,
            
            % 5. 等待并检查模拟器响应
            io:format("4. 等待模拟器响应...~n"),
            timer:sleep(2000),
            
            % 6. 检查PLC寄存器值
            io:format("5. 检查PLC寄存器...~n"),
            % 这里可以添加检查PLC寄存器值的逻辑
            
            ok;
        {ok, #{<<"results">> := []}} ->
            io:format("1. 未找到UAVPLC通道~n"),
            error(no_channel);
        Error ->
            io:format("1. 查询通道失败: ~p~n", [Error]),
            error(query_failed)
    end.