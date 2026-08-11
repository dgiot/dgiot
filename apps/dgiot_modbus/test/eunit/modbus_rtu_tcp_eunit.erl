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

-module(modbus_rtu_tcp_eunit).
-author("dgiot").

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_modbus.hrl").

%%%===================================================================
%%% EUnit 测试
%%%===================================================================

%% 测试十六进制字符串检查函数
hex_string_check_test_() ->
    [
        {"测试有效十六进制字符串",
            ?_assertEqual(true, dgiot_modbusrtu_tcp:is_hex_string(<<"77726A5F646D2D7A7179">>))},
        {"测试无效十六进制字符串",
            ?_assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string(<<"wrj_dm-zqy">>))},
        {"测试空字符串",
            ?_assertEqual(true, dgiot_modbusrtu_tcp:is_hex_string(<<>>))},
        {"测试混合字符串",
            ?_assertEqual(false, dgiot_modbusrtu_tcp:is_hex_string(<<"77726A5F646D2D7A7179xyz">>))}
    ].

%% 测试十六进制解码函数
hex_decode_test_() ->
    [
        {"测试十六进制字符串解码",
            ?_assertEqual(<<"wrj_dm-zqy">>, 
                         dgiot_utils:hex_to_binary(<<"77726A5F646D2D7A7179">>))},
        {"测试无效十六进制字符串",
            ?_assertMatch({error, _}, 
                         dgiot_utils:hex_to_binary(<<"invalid">>))}
    ].

%% 测试通配符匹配
wildcard_match_test_() ->
    [
        {"测试通配符模式转换",
            ?_assertEqual(<<"wrj_[a-zA-Z0-9\\-_]{5}">>, 
                         modbus_util:convert_pattern("wrj_*****"))},
        {"测试通配符匹配 - 正确匹配",
            fun() ->
                Regex = modbus_util:convert_pattern("wrj_*****"),
                ?assertMatch({match, _}, re:run("wrj_dm-zqy", Regex))
            end},
        {"测试通配符匹配 - 包含连字符",
            fun() ->
                Regex = modbus_util:convert_pattern("wrj_*****"),
                ?assertMatch({match, _}, re:run("wrj_dm-zqy", Regex))
            end},
        {"测试通配符匹配 - 包含下划线",
            fun() ->
                Regex = modbus_util:convert_pattern("wrj_*****"),
                ?assertMatch({match, _}, re:run("wrj_dm_zqy", Regex))
            end},
        {"测试通配符匹配 - 不匹配",
            fun() ->
                Regex = modbus_util:convert_pattern("wrj_*****"),
                ?assertEqual(nomatch, re:run("xxx_dm-zqy", Regex))
            end}
    ].

%% 测试注册包处理逻辑
registration_packet_test_() ->
    {setup,
     fun setup_registration/0,
     fun cleanup_registration/1,
     fun test_registration_logic/1}.

setup_registration() ->
    % 模拟测试数据
    HexPacket = <<"77726A5F646D2D7A7179">>,  % "wrj_dm-zqy"
    Head = "wrj_*****",
    Port = 502,
    Dtutype = <<"DGIOT">>,
    {HexPacket, Head, Port, Dtutype}.

cleanup_registration(_State) ->
    ok.

test_registration_logic({HexPacket, Head, Port, Dtutype}) ->
    [
        {"测试十六进制解码",
            fun() ->
                AsciiBuff = case dgiot_utils:hex_to_binary(HexPacket) of
                    {error, _} -> HexPacket;
                    Decoded -> Decoded
                end,
                ?assertEqual(<<"wrj_dm-zqy">>, AsciiBuff)
            end},
        {"测试正则表达式匹配",
            fun() ->
                AsciiBuff = dgiot_utils:hex_to_binary(HexPacket),
                RegexPattern = modbus_util:convert_pattern(Head),
                Regex = binary_to_list(RegexPattern),
                ?assertMatch({match, _}, re:run(binary_to_list(AsciiBuff), Regex))
            end},
        {"测试设备地址生成",
            fun() ->
                AsciiBuff = dgiot_utils:hex_to_binary(HexPacket),
                DeviceAddr = <<AsciiBuff/binary, "-", (integer_to_binary(Port))/binary>>,
                ?assertEqual(<<"wrj_dm-zqy-502">>, DeviceAddr)
            end}
    ].

%% 测试错误处理
error_handling_test_() ->
    [
        {"测试非十六进制输入处理",
            fun() ->
                NonHex = <<"plain_text">>,
                AsciiBuff = case dgiot_utils:hex_to_binary(NonHex) of
                    {error, _} -> NonHex;
                    Decoded -> Decoded
                end,
                ?assertEqual(NonHex, AsciiBuff)
            end},
        {"测试空输入处理",
            fun() ->
                Empty = <<>>,
                AsciiBuff = case dgiot_utils:hex_to_binary(Empty) of
                    {error, _} -> Empty;
                    Decoded -> Decoded
                end,
                ?assertEqual(Empty, AsciiBuff)
            end}
    ].

%% 测试边界条件
boundary_conditions_test_() ->
    [
        {"测试最大长度匹配",
            fun() ->
                % 测试长字符串匹配
                LongString = "wrj_" ++ lists:duplicate(20, $a),
                Regex = modbus_util:convert_pattern("wrj_********************"),
                ?assertMatch({match, _}, re:run(LongString, Regex))
            end},
        {"测试特殊字符处理",
            fun() ->
                % 测试包含特殊字符的字符串
                SpecialString = "wrj_dm-zqy_123",
                Regex = modbus_util:convert_pattern("wrj_***********"),
                ?assertMatch({match, _}, re:run(SpecialString, Regex))
            end}
    ].

%% 集成测试：完整注册流程
integration_registration_test_() ->
    {timeout, 30,
        fun() ->
            % 模拟完整注册流程
            HexPacket = <<"77726A5F646D2D7A7179">>,  % "wrj_dm-zqy"
            Head = "wrj_*****",
            
            % 1. 解码十六进制
            AsciiBuff = case dgiot_utils:hex_to_binary(HexPacket) of
                {error, _} -> HexPacket;
                Decoded -> Decoded
            end,
            ?assertEqual(<<"wrj_dm-zqy">>, AsciiBuff),
            
            % 2. 正则匹配
            RegexPattern = modbus_util:convert_pattern(Head),
            Regex = binary_to_list(RegexPattern),
            MatchResult = re:run(binary_to_list(AsciiBuff), Regex),
            ?assertMatch({match, _}, MatchResult),
            
            % 3. 产品名提取
            Productname = 
                case binary:split(AsciiBuff, <<"-">>, [global]) of
                    [Part | _] -> Part;
                    _ -> AsciiBuff
                end,
            ?assertEqual(<<"wrj_dm">>, Productname),
            
            % 4. 设备地址生成
            Port = 502,
            DeviceAddr = <<AsciiBuff/binary, "-", (integer_to_binary(Port))/binary>>,
            ?assertEqual(<<"wrj_dm-zqy-502">>, DeviceAddr),
            
            ct:pal("集成测试通过: 解码 -> 匹配 -> 提取 -> 生成地址")
        end}.
