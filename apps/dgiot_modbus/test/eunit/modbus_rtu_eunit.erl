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

-module(modbus_rtu_eunit).
-author("dgiot").

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_modbus.hrl").

%%%===================================================================
%%% EUnit 测试
%%%===================================================================

%% 测试 dealwith 函数
dealwith_test_() ->
    [
        {"测试有效Modbus RTU报文处理",
            ?_assertMatch({ok, #{<<"buff">> := _, <<"slaveId">> := _, <<"address">> := _}},
                         modbus_rtu:dealwith(<<1, 3, 0, 0, 0, 1, 132, 10>>))},
        {"测试无效Modbus RTU报文处理",
            ?_assertEqual({error, invalid_packet}, modbus_rtu:dealwith(<<0, 0, 0, 0>>))}
    ].

%% 测试 parse_frame 函数
parse_frame_test_() ->
    {setup,
     fun setup_parse_frame/0,
     fun cleanup_parse_frame/1,
     fun test_parse_frame/1}.

setup_parse_frame() ->
    % 准备测试数据
    Buff = <<1, 3, 2, 0, 1, 185, 200>>,
    Context = #{},
    Config = #{
        <<"dtuproduct">> => <<"test_product">>,
        <<"channel">> => <<"test_channel">>,
        <<"dtuaddr">> => <<"test_addr">>,
        <<"slaveId">> => 1,
        <<"address">> => 0
    },
    {Buff, Context, Config}.

cleanup_parse_frame(_State) ->
    ok.

test_parse_frame({Buff, Context, Config}) ->
    [
        {"测试解析有效数据帧",
            ?_assertMatch({_, #{}}, modbus_rtu:parse_frame(Buff, Context, Config))}
    ].

%% 测试 to_frame 函数
to_frame_test_() ->
    [
        {"测试生成Modbus RTU帧",
            ?_assert(is_binary(modbus_rtu:to_frame(#{
                <<"slaveid">> => 1,
                <<"address">> => 0,
                <<"registersnumber">> => 1
            })))}
    ].

%% 测试 process_calculated_properties 函数
process_calculated_properties_test_() ->
    {setup,
     fun setup_calculated_props/0,
     fun cleanup_calculated_props/1,
     fun test_calculated_props/1}.

setup_calculated_props() ->
    % 准备测试数据
    CalculatedProps = [
        #{
            <<"identifier">> => <<"temp">>,
            <<"dataForm">> => #{<<"protocol">> => <<"MODBUSRTU">>, <<"strategy">> => <<"计算值">>},
            <<"dataSource">> => #{
                <<"address">> => <<"0">>,
                <<"registersnumber">> => <<"1">>,
                <<"originaltype">> => <<"raw">>
            }
        }
    ],
    Buff = <<25>>,  % 温度值 25
    ParentId = <<"parent_device">>,
    ParentValue = <<"parent_value">>,
    Acc = [],
    {CalculatedProps, Buff, ParentId, ParentValue, Acc}.

cleanup_calculated_props(_State) ->
    ok.

test_calculated_props({CalculatedProps, Buff, ParentId, ParentValue, Acc}) ->
    [
        {"测试计算值属性处理",
            ?_assertMatch(#{<<"temp">> := _}, 
                         modbus_rtu:process_calculated_properties(CalculatedProps, Buff, ParentId, ParentValue, Acc))}
    ].

%% 测试 set_params 函数
set_params_test_() ->
    [
        {"测试设置参数生成",
            ?_assert(is_list(modbus_rtu:set_params(#{<<"test">> => <<"value">>}, <<"product">>, <<"addr">>)))}
    ].

%% 测试 modbus_rtu_format 模块函数
format_test_() ->
    [
        {"测试格式转换函数",
            ?_assertEqual(<<"010300000001840A">>, modbus_rtu_format:to_hex(<<1, 3, 0, 0, 0, 1, 132, 10>>))}
    ].

%% 测试 modbus_rtu_utils 模块函数
utils_test_() ->
    [
        {"测试CRC16计算",
            ?_assertEqual(<<132, 10>>, modbus_rtu_utils:crc16(<<1, 3, 0, 0, 0, 1>>))},
        {"测试字节交换",
            ?_assertEqual(<<2, 1>>, modbus_rtu_utils:swap_bytes(<<1, 2>>))}
    ].

%% 性能测试：报文处理性能
performance_test_() ->
    {timeout, 30,
        fun() ->
            % 测试 dealwith 性能
            Packets = [
                <<1, 3, 0, 0, 0, 1, 132, 10>>,
                <<1, 6, 0, 0, 0, 10, 201, 204>>,
                <<1, 16, 0, 0, 0, 2, 4, 0, 10, 0, 20, 182, 77>>
            ],
            lists:foreach(fun(Packet) ->
                {Time, Result} = timer:tc(modbus_rtu, dealwith, [Packet]),
                ?assertMatch({ok, _}, Result),
                ?assert(Time < 5000)  % 每个报文处理应小于5ms
            end, Packets)
        end}.

%% 错误处理测试
error_handling_test_() ->
    [
        {"测试空报文处理",
            ?_assertMatch({error, _}, modbus_rtu:dealwith(<<>>))},
        {"测试过短报文处理",
            ?_assertMatch({error, _}, modbus_rtu:dealwith(<<1, 2>>))},
        {"测试CRC错误报文处理",
            ?_assertMatch({error, _}, modbus_rtu:dealwith(<<1, 3, 0, 0, 0, 1, 0, 0>>))}
    ].

%% 边界条件测试
boundary_test_() ->
    [
        {"测试最大地址值",
            ?_assert(is_binary(modbus_rtu:to_frame(#{
                <<"slaveid">> => 255,
                <<"address">> => 65535,
                <<"registersnumber">> => 125
            })))},
        {"测试最小地址值",
            ?_assert(is_binary(modbus_rtu:to_frame(#{
                <<"slaveid">> => 1,
                <<"address">> => 0,
                <<"registersnumber">> => 1
            })))}
    ].
