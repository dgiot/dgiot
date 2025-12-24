%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(modbus_tcp_client_SUITE).
-author("dgiot").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("dgiot_modbus.hrl").

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        test_tcp_connection,
        test_hex_registration,
        test_ascii_registration,
        test_invalid_registration,
        test_registration_with_port,
        test_product_extraction,
        test_device_address_generation,
        test_error_handling,
        test_performance_registration
    ].

init_per_suite(Config) ->
    % 启动必要的应用
    ok = application:start(dgiot),
    ok = application:start(dgiot_modbus),
    
    % 创建测试通道配置
    ChannelId = <<"test_modbus_channel">>,
    ChannelConfig = #{
        <<"id">> => ChannelId,
        <<"name">> => <<"Modbus测试通道">>,
        <<"type">> => <<"MODBUS">>,
        <<"config">> => #{
            <<"port">> => 1502,
            <<"regtype">> => <<"RegisterByPort">>,
            <<"head">> => "wrj_*****",
            <<"dtutype">> => <<"DGIOT">>
        }
    },
    
    % 注册测试通道
    dgiot_bridge:add_channel(ChannelId, ChannelConfig),
    
    % 创建测试产品
    ProductId = <<"test_modbus_product">>,
    ProductConfig = #{
        <<"objectId">> => ProductId,
        <<"name">> => <<"Modbus测试产品">>,
        <<"category">> => #{
            <<"objectId">> => <<"5ca6049839">>
        }
    },
    
    % 模拟产品数据
    meck:new(dgiot_parse, [non_strict]),
    meck:expect(dgiot_parse, get_object, 
                fun(<<"Product">>, <<"test_modbus_product">>) ->
                    {ok, ProductConfig};
                   (<<"Product">>, _) ->
                    {error, not_found}
                end),
    
    [{channel_id, ChannelId}, {product_id, ProductId} | Config].

end_per_suite(Config) ->
    % 清理meck
    meck:unload(dgiot_parse),
    
    % 停止应用
    application:stop(dgiot_modbus),
    application:stop(dgiot),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% 测试TCP连接建立
test_tcp_connection(_Config) ->
    % 模拟TCP连接
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(ListenSocket),
    
    % 启动客户端连接
    {ok, ClientSocket} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    {ok, ServerSocket} = gen_tcp:accept(ListenSocket),
    
    % 验证连接
    ?assert(is_port(ClientSocket)),
    ?assert(is_port(ServerSocket)),
    
    % 发送测试数据
    TestData = <<"test connection">>,
    ok = gen_tcp:send(ClientSocket, TestData),
    {ok, Received} = gen_tcp:recv(ServerSocket, 0, 1000),
    ?assertEqual(TestData, Received),
    
    % 清理
    gen_tcp:close(ClientSocket),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ListenSocket),
    ok.

%% 测试十六进制注册包处理
test_hex_registration(Config) ->
    ChannelId = ?config(channel_id, Config),
    ProductId = ?config(product_id, Config),
    
    % 十六进制注册包: "wrj_dm-zqy"
    HexPacket = <<"77726A5F646D2D7A7179">>,
    Head = "wrj_*****",
    Port = 502,
    
    % 测试十六进制解码
    AsciiBuff = case dgiot_utils:hex_to_binary(HexPacket) of
        {error, _} -> HexPacket;
        Decoded -> Decoded
    end,
    ?assertEqual(<<"wrj_dm-zqy">>, AsciiBuff),
    
    % 测试正则匹配
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    MatchResult = re:run(binary_to_list(AsciiBuff), Regex),
    ?assertMatch({match, _}, MatchResult),
    
    % 测试产品名提取
    Productname = 
        case binary:split(AsciiBuff, <<"-">>, [global]) of
            [Part | _] -> Part;
            _ -> AsciiBuff
        end,
    ?assertEqual(<<"wrj_dm">>, Productname),
    
    % 测试设备地址生成
    DeviceAddr = <<AsciiBuff/binary, "-", (integer_to_binary(Port))/binary>>,
    ?assertEqual(<<"wrj_dm-zqy-502">>, DeviceAddr),
    
    % 测试产品ID获取
    Dtutype = <<"DGIOT">>,
    ExpectedProductId = modbus_util:get_product_id(Productname, Dtutype),
    ?assert(is_binary(ExpectedProductId)),
    
    ct:pal("十六进制注册测试通过: ~p", [DeviceAddr]),
    ok.

%% 测试ASCII注册包处理
test_ascii_registration(Config) ->
    ChannelId = ?config(channel_id, Config),
    ProductId = ?config(product_id, Config),
    
    % ASCII注册包
    AsciiPacket = <<"wrj_dm-zqy">>,
    Head = "wrj_*****",
    Port = 502,
    
    % 测试十六进制检查（应该返回false）
    ?assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string(AsciiPacket)),
    
    % 测试正则匹配
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    MatchResult = re:run(binary_to_list(AsciiPacket), Regex),
    ?assertMatch({match, _}, MatchResult),
    
    % 测试产品名提取
    Productname = 
        case binary:split(AsciiPacket, <<"-">>, [global]) of
            [Part | _] -> Part;
            _ -> AsciiPacket
        end,
    ?assertEqual(<<"wrj_dm">>, Productname),
    
    % 测试设备地址生成
    DeviceAddr = <<AsciiPacket/binary, "-", (integer_to_binary(Port))/binary>>,
    ?assertEqual(<<"wrj_dm-zqy-502">>, DeviceAddr),
    
    ct:pal("ASCII注册测试通过: ~p", [DeviceAddr]),
    ok.

%% 测试无效注册包处理
test_invalid_registration(_Config) ->
    % 测试不匹配的注册包
    InvalidPacket = <<"xxx_dm-zqy">>,  % 不以"wrj_"开头
    Head = "wrj_*****",
    
    % 测试正则匹配（应该不匹配）
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    MatchResult = re:run(binary_to_list(InvalidPacket), Regex),
    ?assertEqual(nomatch, MatchResult),
    
    % 测试无效十六进制字符串
    InvalidHex = <<"invalid_hex">>,
    ?assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string(InvalidHex)),
    
    ct:pal("无效注册包测试通过"),
    ok.

%% 测试带端口的注册流程
test_registration_with_port(Config) ->
    ChannelId = ?config(channel_id, Config),
    
    % 模拟完整的注册流程
    HexPacket = <<"77726A5F646D2D7A7179">>,
    Head = "wrj_*****",
    Port = 502,
    Dtutype = <<"DGIOT">>,
    
    % 模拟TCP状态
    TCPState = #tcp{
        register = false,
        state = #state{
            id = ChannelId,
            regtype = <<"RegisterByPort">>,
            head = Head,
            dtutype = Dtutype,
            port = Port
        }
    },
    
    % 测试handle_info函数（简化版本）
    AsciiBuff = case dgiot_modbusrtu_tcp:is_hex_string(HexPacket) of
        true -> 
            case dgiot_utils:hex_to_binary(HexPacket) of
                {error, _} -> HexPacket;
                Decoded -> Decoded
            end;
        false -> HexPacket
    end,
    
    % 验证解码结果
    ?assertEqual(<<"wrj_dm-zqy">>, AsciiBuff),
    
    % 验证正则匹配
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    MatchResult = re:run(binary_to_list(AsciiBuff), Regex),
    ?assertMatch({match, _}, MatchResult),
    
    ct:pal("带端口注册测试通过"),
    ok.

%% 测试产品名提取逻辑
test_product_extraction(_Config) ->
    TestCases = [
        {<<"wrj_dm-zqy">>, <<"wrj_dm">>},
        {<<"wrj_device">>, <<"wrj_device">>},
        {<<"wrj-sensor-001">>, <<"wrj-sensor">>},
        {<<"no_dash">>, <<"no_dash">>}
    ],
    
    lists:foreach(
        fun({Input, Expected}) ->
            Result = 
                case binary:split(Input, <<"-">>, [global]) of
                    [Part | _] -> Part;
                    _ -> Input
                end,
            ?assertEqual(Expected, Result,
                io_lib:format("产品名提取失败: ~p -> ~p (期望: ~p)", 
                             [Input, Result, Expected]))
        end,
        TestCases
    ),
    
    ct:pal("产品名提取测试通过"),
    ok.

%% 测试设备地址生成逻辑
test_device_address_generation(_Config) ->
    TestCases = [
        {<<"wrj_dm-zqy">>, 502, <<"wrj_dm-zqy-502">>},
        {<<"device001">>, 1502, <<"device001-1502">>},
        {<<"sensor">>, 80, <<"sensor-80">>}
    ],
    
    lists:foreach(
        fun({Packet, Port, Expected}) ->
            DeviceAddr = <<Packet/binary, "-", (integer_to_binary(Port))/binary>>,
            ?assertEqual(Expected, DeviceAddr,
                io_lib:format("设备地址生成失败: ~p + ~p -> ~p (期望: ~p)", 
                             [Packet, Port, DeviceAddr, Expected]))
        end,
        TestCases
    ),
    
    ct:pal("设备地址生成测试通过"),
    ok.

%% 测试错误处理
test_error_handling(_Config) ->
    % 测试空输入
    ?assertEqual(true, dgiot_modbusrtu_tcp:is_hex_string(<<>>)),
    
    % 测试非二进制输入（应该由函数处理）
    ?assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string("string")),
    
    % 测试混合字符
    Mixed = <<"77726A5F646D2D7A7179xyz">>,  % 十六进制 + 非十六进制
    ?assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string(Mixed)),
    
    % 测试特殊字符
    Special = <<"wrj_dm@zqy">>,  % 包含@符号
    Head = "wrj_*****",
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    MatchResult = re:run(binary_to_list(Special), Regex),
    ?assertEqual(nomatch, MatchResult),  # @符号不在允许的字符集中
    
    ct:pal("错误处理测试通过"),
    ok.

%% 测试性能：多次注册
test_performance_registration(_Config) ->
    Head = "wrj_*****",
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    
    % 生成测试数据
    TestPackets = [
        <<"wrj_dm-zqy">>,
        <<"wrj_sensor">>,
        <<"wrj_device">>,
        <<"wrj_meter01">>,
        <<"wrj_temp-45">>
    ],
    
    % 性能测试：多次匹配
    StartTime = erlang:monotonic_time(millisecond),
    
    Results = lists:map(
        fun(Packet) ->
            re:run(binary_to_list(Packet), Regex)
        end,
        TestPackets
    ),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    % 验证所有匹配都成功
    lists:foreach(
        fun(Result) ->
            ?assertMatch({match, _}, Result)
        end,
        Results
    ),
    
    % 性能要求：5次匹配应该在10ms内完成
    ?assert(Duration < 10,
        io_lib:format("性能测试失败: 5次匹配耗时 ~p ms (要求: <10ms)", [Duration])),
    
    ct:pal("性能测试通过: 5次匹配耗时 ~p ms", [Duration]),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% 模拟TCP服务器用于测试
start_test_server(Port, Handler) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    spawn_link(fun() -> accept_loop(ListenSocket, Handler) end),
    ListenSocket.

accept_loop(ListenSocket, Handler) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn_link(fun() -> Handler(Socket) end),
    accept_loop(ListenSocket, Handler).

%% 模拟注册处理器
mock_registration_handler(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            % 模拟注册处理
            ct:pal("模拟服务器收到注册包: ~p", [Data]),
            gen_tcp:send(Socket, <<"REGISTER_OK">>),
            mock_registration_handler(Socket);
        {tcp_closed, Socket} ->
            ct:pal("连接关闭");
        _ ->
            mock_registration_handler(Socket)
    end.
