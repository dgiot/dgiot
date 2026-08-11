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

-module(modbus_util_eunit).
-author("dgiot").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% EUnit 测试
%%%===================================================================

%% 测试 convert_pattern 函数
convert_pattern_test_() ->
    [
        {"测试无通配符模式转换",
            ?_assertEqual(<<"abc">>, modbus_util:convert_pattern("abc"))},
        {"测试单个星号模式转换",
            ?_assertEqual(<<"a[a-zA-Z0-9]{1}b">>, modbus_util:convert_pattern("a*b"))},
        {"测试双星号模式转换",
            ?_assertEqual(<<"a[a-zA-Z0-9]{2}b">>, modbus_util:convert_pattern("a**b"))},
        {"测试只有星号模式转换",
            ?_assertEqual(<<"[a-zA-Z0-9]{3}">>, modbus_util:convert_pattern("***"))},
        {"测试中间星号模式转换",
            ?_assertEqual(<<"test[a-zA-Z0-9]{1}123">>, modbus_util:convert_pattern("test*123"))},
        {"测试开头星号模式转换",
            ?_assertEqual(<<"[a-zA-Z0-9]{1}start">>, modbus_util:convert_pattern("*start"))},
        {"测试结尾星号模式转换",
            ?_assertEqual(<<"end[a-zA-Z0-9]{1}">>, modbus_util:convert_pattern("end*"))},
        {"测试多个星号模式转换",
            ?_assertEqual(<<"a[a-zA-Z0-9]{1}b[a-zA-Z0-9]{2}c[a-zA-Z0-9]{1}d">>, 
                         modbus_util:convert_pattern("a*b**c*d"))},
        {"测试二进制输入模式转换",
            ?_assertEqual(<<"test[a-zA-Z0-9]{1}123">>, modbus_util:convert_pattern(<<"test*123">>))}
    ].

%% 测试 get_header 函数
get_header_test_() ->
    [
        {"测试简单头部解析",
            ?_assertEqual({"wrj", 3}, modbus_util:get_header("wrj"))},
        {"测试带分隔符头部解析",
            ?_assertEqual({"wrj**-***", 9}, modbus_util:get_header("wrj**-***"))}
    ].

%% 测试 get_category_id 函数
get_category_id_test_() ->
    [
        {"测试默认分类ID",
            ?_assertEqual(<<"5ca6049839">>, modbus_util:get_category_id())}
    ].

%% 测试二进制转换函数
binary_conversion_test_() ->
    [
        {"测试二进制转16位整数",
            ?_assertEqual([258], modbus_util:binary_to_int16(<<1, 2>>))},
        {"测试二进制转32位整数",
            ?_assertEqual([16909060], modbus_util:binary_to_int32(<<1, 2, 3, 4>>))},
        {"测试二进制转ASCII",
            ?_assertEqual("test", modbus_util:binary_to_ascii(<<"test">>))}
    ].

%% 测试 coils_to_binary 函数
coils_to_binary_test_() ->
    [
        {"测试线圈转二进制",
            ?_assertEqual(<<128>>, modbus_util:coils_to_binary([1,0,0,0,0,0,0,0]))},
        {"测试线圈转二进制（补零）",
            ?_assertEqual(<<128, 0>>, modbus_util:coils_to_binary([1,0,0,0,0,0,0,0, 0]))}
    ].

%% 测试 int16_to_binary 函数
int16_to_binary_test_() ->
    [
        {"测试16位整数转二进制",
            ?_assertEqual(<<1, 2>>, modbus_util:int16_to_binary([258]))}
    ].

%% 集成测试：完整流程测试
integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     fun test_integration_flow/1}.

setup_integration() ->
    % 设置测试环境
    application:set_env(dgiot_modbus, category, "test_category"),
    {ok, #{}}.

cleanup_integration(_State) ->
    % 清理测试环境
    application:unset_env(dgiot_modbus, category),
    ok.

test_integration_flow(_State) ->
    [
        {"测试分类ID获取",
            ?_assertEqual(<<"test_category">>, modbus_util:get_category_id())}
    ].

%% 性能测试
performance_test_() ->
    {timeout, 30,  % 30秒超时
        fun() ->
            % 测试 convert_pattern 性能
            Patterns = ["abc", "a*b", "test*123", "***", "a*b**c*d"],
            lists:foreach(fun(Pattern) ->
                {Time, _} = timer:tc(modbus_util, convert_pattern, [Pattern]),
                ?assert(Time < 1000)  % 每个转换应小于1ms
            end, Patterns)
        end}.
