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

%% @doc dgiot_task模块单元测试
%% 测试任务统计模块的核心功能
-module(dgiot_task_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_task.hrl").

%% 测试集定义
all_test_() ->
    [
        {"数据保存函数测试", fun data_saving_test/0},
        {"物模型函数测试", fun thing_model_test/0},
        {"统计计算函数测试", fun statistics_test/0},
        {"工具函数测试", fun utility_test/0},
        {"协议处理函数测试", fun protocol_test/0},
        {"规则引擎函数测试", fun rule_engine_test/0},
        {"任务编排函数测试", fun task_scheduling_test/0}
    ].

%%%===================================================================
%%% 数据保存函数测试
%%%===================================================================

data_saving_test() ->
    io:format("~s ~p 开始数据保存函数测试...~n", [?FILE, ?LINE]),
    
    % 测试save_td函数
    ProductId = <<"test_product">>,
    DevAddr = <<"test_device">>,
    Ack = #{<<"temperature">> => 25.5, <<"humidity">> => 60.0},
    AppData = #{<<"interval">> => 3},
    
    ?assertMatch(#{}, dgiot_task:save_td(ProductId, DevAddr, Ack, AppData)),
    
    % 测试smart_save_td函数
    ?assertMatch(#{}, dgiot_task:smart_save_td(ProductId, DevAddr, Ack, AppData)),
    
    % 测试save_td_no_match函数
    ?assertMatch(#{}, dgiot_task:save_td_no_match(ProductId, DevAddr, Ack, AppData)),
    
    io:format("~s ~p 数据保存函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 物模型函数测试
%%%===================================================================

thing_model_test() ->
    io:format("~s ~p 开始物模型函数测试...~n", [?FILE, ?LINE]),
    
    ProductId = <<"test_product">>,
    
    % 测试get_props函数
    Props = dgiot_task:get_props(ProductId),
    ?assert(is_list(Props)),
    
    % 测试get_control函数
    ControlResult = dgiot_task:get_control(1, #{<<"value">> => 10}, <<"control_template">>),
    ?assert(is_map(ControlResult)),
    ?assertMatch(#{round := 1, data := #{<<"value">> := 10}, control := <<"control_template">>}, ControlResult),
    
    % 测试get_collection函数
    Payload = #{<<"temperature">> => 25.5},
    Collection = dgiot_task:get_collection(ProductId, [], Payload, Props),
    ?assert(is_map(Collection)),
    
    % 测试get_calculated函数
    Calculated = dgiot_task:get_calculated(ProductId, <<"test_device">>, Collection, Props),
    ?assert(is_map(Calculated)),
    
    % 测试get_instruct函数
    Instruct = dgiot_task:get_instruct(ProductId, 1),
    ?assert(is_list(Instruct)),
    
    % 测试get_storage函数
    Storage = dgiot_task:get_storage(Calculated, Props),
    ?assert(is_map(Storage)),
    
    io:format("~s ~p 物模型函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 统计计算函数测试
%%%===================================================================

statistics_test() ->
    io:format("~s ~p 开始统计计算函数测试...~n", [?FILE, ?LINE]),
    
    % 测试get_statistic函数
    ProductId = <<"test_product">>,
    DevAddr = <<"test_device">>,
    Key = <<"temperature">>,
    Identifier = <<"avg_temperature">>,
    KeyValue = 25,
    DataSource = #{<<"type">> => <<"duration">>},
    Acc = #{},
    
    StatisticResult = dgiot_task:get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc),
    ?assert(is_map(StatisticResult)),
    
    % 测试get_last_value函数
    LastValue = dgiot_task:get_last_value(ProductId, DevAddr, Key, Identifier),
    ?assertMatch(not_find, LastValue),
    
    % 测试compare函数
    ?assertEqual(true, dgiot_task:compare(5, <<"LT">>, 10)),
    ?assertEqual(false, dgiot_task:compare(15, <<"LT">>, 10)),
    ?assertEqual(true, dgiot_task:compare(10, <<"EQ">>, 10)),
    ?assertEqual(false, dgiot_task:compare(10, <<"NE">>, 10)),
    ?assertEqual(true, dgiot_task:compare(15, <<"GT">>, 10)),
    ?assertEqual(true, dgiot_task:compare(10, <<"GE">>, 10)),
    ?assertEqual(true, dgiot_task:compare(5, <<"LE">>, 10)),
    
    io:format("~s ~p 统计计算函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 工具函数测试
%%%===================================================================

utility_test() ->
    io:format("~s ~p 开始工具函数测试...~n", [?FILE, ?LINE]),
    
    % 测试string2value函数
    ?assertEqual(3, dgiot_task:string2value("1+2", <<"int">>)),
    ?assertEqual(7.5, dgiot_task:string2value("5+2.5", <<"float">>)),
    ?assertEqual("test", dgiot_task:string2value("test", <<"TEXT">>)),
    
    % 测试string2value带规格函数
    Specs = #{<<"precision">> => 2},
    ?assertEqual(3.14, dgiot_task:string2value("3.14159", <<"float">>, Specs)),
    
    % 测试边界条件
    ?assertMatch(error, dgiot_task:string2value("1+%", <<"int">>)),
    ?assertMatch(error, dgiot_task:string2value("%test%", <<"TEXT">>)),
    
    io:format("~s ~p 工具函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 协议处理函数测试
%%%===================================================================

protocol_test() ->
    io:format("~s ~p 开始协议处理函数测试...~n", [?FILE, ?LINE]),
    
    % 测试needs_protocol_parsing函数
    ?assertEqual(true, dgiot_task:needs_protocol_parsing(<<"data">>)),
    ?assertEqual(false, dgiot_task:needs_protocol_parsing(<<>>)),
    ?assertEqual(false, dgiot_task:needs_protocol_parsing(#{})),
    
    % 测试call_protocol_hook函数
    ProductId = <<"test_product">>,
    DevAddr = <<"test_device">>,
    Data = <<"raw_data">>,
    Protocol = <<"MODBUSRTU">>,
    
    HookResult = dgiot_task:call_protocol_hook(ProductId, DevAddr, Data, Protocol),
    ?assert(is_tuple(HookResult)),
    
    io:format("~s ~p 协议处理函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 规则引擎函数测试
%%%===================================================================

rule_engine_test() ->
    io:format("~s ~p 开始规则引擎函数测试...~n", [?FILE, ?LINE]),
    
    % 测试rule_engine_transform函数
    ThirdPartyData = #{<<"temp">> => 25, <<"hum">> => 60},
    Protocol = <<"TEST_PROTOCOL">>,
    
    TransformResult = dgiot_task:rule_engine_transform(ThirdPartyData, Protocol),
    ?assert(is_map(TransformResult)),
    
    % 测试register_rule函数
    Rule = #{<<"source">> => <<"temp">>, <<"target">> => <<"temperature">>, <<"transform">> => <<"value">>},
    ?assertEqual(ok, dgiot_task:register_rule(Protocol, Rule)),
    
    % 测试get_rules函数
    Rules = dgiot_task:get_rules(Protocol),
    ?assert(is_list(Rules)),
    
    io:format("~s ~p 规则引擎函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 任务编排函数测试
%%%===================================================================

task_scheduling_test() ->
    io:format("~s ~p 开始任务编排函数测试...~n", [?FILE, ?LINE]),
    
    ProductId = <<"test_product">>,
    
    % 测试schedule_tasks_from_thing_model函数
    ScheduleResult = dgiot_task:schedule_tasks_from_thing_model(ProductId),
    ?assert(is_tuple(ScheduleResult)),
    
    % 测试stop_tasks函数
    ?assertEqual(ok, dgiot_task:stop_tasks(ProductId)),
    
    % 测试parse_task_parameters函数
    Props = [
        #{
            <<"dataForm">> => #{
                <<"strategy">> => <<"采集值"/utf8>>,
                <<"order">> => 1,
                <<"interval">> => 5,
                <<"rounds">> => 10
            },
            <<"identifier">> => <<"temperature">>
        }
    ],
    
    TaskParams = dgiot_task:parse_task_parameters(Props),
    ?assert(is_list(TaskParams)),
    
    io:format("~s ~p 任务编排函数测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 测试辅助函数
%%%===================================================================

%% @doc 生成测试数据
generate_test_data() ->
    #{
        product_id => <<"test_product">>,
        device_addr => <<"test_device">>,
        temperature => 25.5,
        humidity => 60.0,
        timestamp => erlang:system_time()
    }.

%% @doc 验证测试结果
validate_test_result(Result) ->
    case Result of
        ok -> ok;
        {error, Reason} -> {error, Reason};
        _ -> {error, unexpected_result}
    end.
