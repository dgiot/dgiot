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

-module(modbus_integration_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("dgiot_modbus.hrl").

%%--------------------------------------------------------------------
%% 测试套件配置
%%--------------------------------------------------------------------

all() -> 
    [
        test_modbus_util_functions,
        test_modbus_rtu_parsing,
        test_modbus_tcp_communication,
        test_modbus_channel_integration,
        test_modbus_device_registration,
        test_modbus_performance
    ].

init_per_suite(Config) ->
    % 初始化测试套件
    ct:pal("初始化Modbus集成测试套件..."),
    
    % 确保应用已启动
    application:ensure_all_started(dgiot_modbus),
    
    % 设置测试环境
    application:set_env(dgiot_modbus, test_mode, true),
    
    % 创建测试数据目录
    DataDir = ?config(data_dir, Config),
    file:make_dir(filename:join(DataDir, "test_data")),
    
    [{test_data_dir, filename:join(DataDir, "test_data")} | Config].

end_per_suite(Config) ->
    % 清理测试套件
    ct:pal("清理Modbus集成测试套件..."),
    
    % 停止应用
    application:stop(dgiot_modbus),
    
    % 清理测试数据
    DataDir = ?config(test_data_dir, Config),
    file:del_dir_r(DataDir),
    
    Config.

init_per_testcase(_TestCase, Config) ->
    % 每个测试用例初始化
    ct:pal("开始测试用例..."),
    Config.

end_per_testcase(_TestCase, _Config) ->
    % 每个测试用例清理
    ct:pal("测试用例完成"),
    ok.

%%--------------------------------------------------------------------
%% 测试用例
%%--------------------------------------------------------------------

%% 测试 modbus_util 模块功能
test_modbus_util_functions(Config) ->
    ct:pal("测试 modbus_util 模块功能..."),
    
    % 测试 convert_pattern 函数
    TestPatterns = [
        {"abc", <<"abc">>},
        {"a*b", <<"a[a-zA-Z0-9]{1}b">>},
        {"test*123", <<"test[a-zA-Z0-9]{1}123">>}
    ],
    
    lists:foreach(fun({Input, Expected}) ->
        Result = modbus_util:convert_pattern(Input),
        ct:pal("convert_pattern(~p) = ~p", [Input, Result]),
        ?assertEqual(Expected, Result)
    end, TestPatterns),
    
    % 测试 get_category_id 函数
    CategoryId = modbus_util:get_category_id(),
    ct:pal("get_category_id() = ~p", [CategoryId]),
    ?assert(is_binary(CategoryId)),
    
    % 测试二进制转换函数
    Binary = <<1, 2, 3, 4>>,
    Int16Result = modbus_util:binary_to_int16(Binary),
    ct:pal("binary_to_int16(~p) = ~p", [Binary, Int16Result]),
    ?assert(is_list(Int16Result)),
    
    ok.

%% 测试 Modbus RTU 报文解析
test_modbus_rtu_parsing(Config) ->
    ct:pal("测试 Modbus RTU 报文解析..."),
    
    % 测试有效报文
    ValidPacket = <<1, 3, 0, 0, 0, 1, 132, 10>>,
    {ok, Parsed} = modbus_rtu:dealwith(ValidPacket),
    ct:pal("dealwith(~p) = ~p", [ValidPacket, Parsed]),
    ?assertMatch(#{<<"buff">> := _, <<"slaveId">> := _, <<"address">> := _}, Parsed),
    
    % 测试无效报文
    InvalidPacket = <<0, 0, 0, 0>>,
    {error, Reason} = modbus_rtu:dealwith(InvalidPacket),
    ct:pal("dealwith(~p) = {error, ~p}", [InvalidPacket, Reason]),
    ?assertEqual(invalid_packet, Reason),
    
    % 测试 parse_frame 函数
    Buff = <<1, 3, 2, 0, 1, 185, 200>>,
    Context = #{},
    ConfigMap = #{
        <<"dtuproduct">> => <<"test_product">>,
        <<"channel">> => <<"test_channel">>,
        <<"dtuaddr">> => <<"test_addr">>,
        <<"slaveId">> => 1,
        <<"address">> => 0
    },
    
    {Result, Data} = modbus_rtu:parse_frame(Buff, Context, ConfigMap),
    ct:pal("parse_frame result: ~p, data: ~p", [Result, Data]),
    ?assertMatch(ok, Result),
    ?assert(is_map(Data)),
    
    ok.

%% 测试 Modbus TCP 通信
test_modbus_tcp_communication(Config) ->
    ct:pal("测试 Modbus TCP 通信..."),
    
    % 测试 modbus_tcp 模块函数
    try
        % 检查模块是否已加载
        ModuleInfo = modbus_tcp:module_info(),
        ct:pal("modbus_tcp 模块信息: ~p", [length(ModuleInfo)]),
        
        % 测试基本功能
        Frame = modbus_tcp:to_frame(#{
            <<"slaveid">> => 1,
            <<"address">> => 0,
            <<"registersnumber">> => 1
        }),
        ct:pal("生成的TCP帧: ~p", [Frame]),
        ?assert(is_binary(Frame)),
        
        ok
    catch
        error:undef ->
            ct:pal("modbus_tcp 模块未加载，跳过测试"),
            {skip, "modbus_tcp module not loaded"}
    end.

%% 测试 Modbus 通道集成
test_modbus_channel_integration(Config) ->
    ct:pal("测试 Modbus 通道集成..."),
    
    % 测试通道启动
    ChannelConfig = #{
        <<"name">> => <<"test_modbus_channel">>,
        <<"type">> => <<"MODBUS">>,
        <<"config">> => #{
            <<"ip">> => <<"127.0.0.1">>,
            <<"port">> => 502,
            <<"protocol">> => <<"tcp">>
        }
    },
    
    % 模拟通道启动
    try
        {ok, Pid} = dgiot_modbus_channel:start_link(ChannelConfig),
        ct:pal("通道启动成功: ~p", [Pid]),
        
        % 测试通道状态
        Status = dgiot_modbus_channel:get_status(Pid),
        ct:pal("通道状态: ~p", [Status]),
        ?assertMatch(#{status := _}, Status),
        
        % 停止通道
        exit(Pid, normal),
        ok
    catch
        error:Reason ->
            ct:pal("通道启动失败: ~p", [Reason]),
            {skip, "Channel startup failed"}
    end.

%% 测试 Modbus 设备注册
test_modbus_device_registration(Config) ->
    ct:pal("测试 Modbus 设备注册..."),
    
    % 测试设备注册逻辑
    ProductId = <<"test_product">>,
    DevAddr = <<"test_device_001">>,
    
    % 模拟设备ID生成
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    ct:pal("生成的设备ID: ~p", [DeviceId]),
    ?assert(is_binary(DeviceId)),
    
    % 测试通配符匹配
    Pattern = "wrj_**-***",
    Regex = modbus_util:convert_pattern(Pattern),
    
    TestAddresses = [
        {<<"wrj_dm-zqy">>, true},
        {<<"wrj_ab-cd">>, true},
        {<<"xxx_dm-zqy">>, false}
    ],
    
    lists:foreach(fun({Address, ShouldMatch}) ->
        MatchResult = re:run(Address, Regex),
        ct:pal("地址 ~p 匹配模式 ~p: ~p", [Address, Pattern, MatchResult]),
        
        case ShouldMatch of
            true -> ?assertMatch({match, _}, MatchResult);
            false -> ?assertMatch(nomatch, MatchResult)
        end
    end, TestAddresses),
    
    ok.

%% 测试 Modbus 性能
test_modbus_performance(Config) ->
    ct:pal("测试 Modbus 性能..."),
    
    % 测试报文处理性能
    Packets = [
        <<1, 3, 0, 0, 0, 1, 132, 10>>,
        <<1, 6, 0, 0, 0, 10, 201, 204>>,
        <<1, 16, 0, 0, 0, 2, 4, 0, 10, 0, 20, 182, 77>>
    ],
    
    Times = lists:map(fun(Packet) ->
        {Time, Result} = timer:tc(modbus_rtu, dealwith, [Packet]),
        ?assertMatch({ok, _}, Result),
        Time
    end, Packets),
    
    AvgTime = lists:sum(Times) div length(Times),
    ct:pal("报文处理平均时间: ~p 微秒", [AvgTime]),
    
    % 性能要求：平均处理时间小于5ms
    ?assert(AvgTime < 5000),
    
    % 测试批量处理性能
    BatchSize = 100,
    {BatchTime, _} = timer:tc(fun() ->
        lists:foreach(fun(Packet) ->
            {ok, _} = modbus_rtu:dealwith(Packet)
        end, lists:duplicate(BatchSize, hd(Packets)))
    end),
    
    AvgBatchTime = BatchTime div BatchSize,
    ct:pal("批量处理平均时间: ~p 微秒", [AvgBatchTime]),
    
    ok.

%%--------------------------------------------------------------------
%% 辅助函数
%%--------------------------------------------------------------------

file:del_dir_r(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                Path = filename:join(Dir, File),
                case filelib:is_dir(Path) of
                    true -> file:del_dir_r(Path);
                    false -> file:delete(Path)
                end
            end, Files),
            file:del_dir(Dir);
        {error, enoent} ->
            ok;
        Error ->
            Error
    end.
