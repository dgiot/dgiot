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

%% @doc dgiot_task模块集成测试
%% 测试任务统计模块的端到端流程
-module(dgiot_task_integration_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_task.hrl").

%% 集成测试
end_to_end_test_() ->
    {timeout, 30, fun test_end_to_end_workflow/0}.

%%%===================================================================
%%% 端到端工作流测试
%%%===================================================================

test_end_to_end_workflow() ->
    io:format("~s ~p 开始端到端工作流测试...~n", [?FILE, ?LINE]),
    
    % 1. 准备测试数据
    TestData = prepare_test_data(),
    ProductId = maps:get(product_id, TestData),
    DevAddr = maps:get(device_addr, TestData),
    
    % 2. 测试数据保存流程
    io:format("~s ~p 测试数据保存流程...~n", [?FILE, ?LINE]),
    test_data_saving_workflow(ProductId, DevAddr, TestData),
    
    % 3. 测试物模型处理流程
    io:format("~s ~p 测试物模型处理流程...~n", [?FILE, ?LINE]),
    test_thing_model_workflow(ProductId, DevAddr, TestData),
    
    % 4. 测试规则引擎流程
    io:format("~s ~p 测试规则引擎流程...~n", [?FILE, ?LINE]),
    test_rule_engine_workflow(),
    
    % 5. 测试任务编排流程
    io:format("~s ~p 测试任务编排流程...~n", [?FILE, ?LINE]),
    test_task_scheduling_workflow(ProductId),
    
    io:format("~s ~p 端到端工作流测试完成~n", [?FILE, ?LINE]),
    ok.

%%%===================================================================
%%% 数据保存工作流测试
%%%===================================================================

test_data_saving_workflow(ProductId, DevAddr, TestData) ->
    % 准备测试数据
    Ack = #{
        <<"temperature">> => maps:get(temperature, TestData),
        <<"humidity">> => maps:get(humidity, TestData),
        <<"timestamp">> => maps:get(timestamp, TestData)
    },
    AppData = #{<<"interval">> => 3},
    
    % 测试save_td函数
    SaveResult = dgiot_task:save_td(ProductId, DevAddr, Ack, AppData),
    ?assert(is_map(SaveResult)),
    
    % 测试smart_save_td函数
    SmartSaveResult = dgiot_task:smart_save_td(ProductId, DevAddr, Ack, AppData),
    ?assert(is_map(SmartSaveResult)),
    
    % 测试save_td_no_match函数
    NoMatchResult = dgiot_task:save_td_no_match(ProductId, DevAddr, Ack, AppData),
    ?assert(is_map(NoMatchResult)),
    
    ok.

%%%===================================================================
%%% 物模型工作流测试
%%%===================================================================

test_thing_model_workflow(ProductId, DevAddr, TestData) ->
    % 获取物模型属性
    Props = dgiot_task:get_props(ProductId),
    ?assert(is_list(Props)),
    
    % 测试数据采集流程
    Payload = #{
        <<"temperature">> => maps:get(temperature, TestData),
        <<"humidity">> => maps:get(humidity, TestData)
    },
    
    % 采集数据
    Collection = dgiot_task:get_collection(ProductId, [], Payload, Props),
    ?assert(is_map(Collection)),
    ?assert(maps:size(Collection) > 0),
    
    % 计算值
    Calculated = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),
    ?assert(is_map(Calculated)),
    
    % 获取存储值
    Storage = dgiot_task:get_storage(Calculated, Props),
    ?assert(is_map(Storage)),
    
    % 测试控制值
    ControlResult = dgiot_task:get_control(1, #{<<"value">> => 25}, <<"control">>),
    ?assert(is_map(ControlResult)),
    
    % 测试指令生成
    Instruct = dgiot_task:get_instruct(ProductId, 1),
    ?assert(is_list(Instruct)),
    
    ok.

%%%===================================================================
%%% 规则引擎工作流测试
%%%===================================================================

test_rule_engine_workflow() ->
    % 准备测试数据
    ThirdPartyData = #{
        <<"external_temp">> => 25,
        <<"external_hum">> => 60,
        <<"external_pressure">> => 1013
    },
    Protocol = <<"EXTERNAL_PROTOCOL">>,
    
    % 注册转换规则
    Rules = [
        #{<<"source">> => <<"external_temp">>, <<"target">> => <<"temperature">>, <<"transform">> => <<"value">>},
        #{<<"source">> => <<"external_hum">>, <<"target">> => <<"humidity">>, <<"transform">> => <<"value">>},
        #{<<"source">> => <<"external_pressure">>, <<"target">> => <<"pressure">>, <<"transform">> => <<"value/100">>}
    ],
    
    lists:foreach(fun(Rule) ->
        ?assertEqual(ok, dgiot_task:register_rule(Protocol, Rule))
    end, Rules),
    
    % 获取规则
    RetrievedRules = dgiot_task:get_rules(Protocol),
    ?assert(is_list(RetrievedRules)),
    ?assertEqual(length(Rules), length(RetrievedRules)),
    
    % 测试规则转换
    TransformResult = dgiot_task:rule_engine_transform(ThirdPartyData, Protocol),
    ?assert(is_map(TransformResult)),
    ?assert(maps:is_key(<<"temperature">>, TransformResult)),
    ?assert(maps:is_key(<<"humidity">>, TransformResult)),
    ?assert(maps:is_key(<<"pressure">>, TransformResult)),
    
    ok.

%%%===================================================================
%%% 任务编排工作流测试
%%%===================================================================

test_task_scheduling_workflow(ProductId) ->
    % 测试任务调度
    ScheduleResult = dgiot_task:schedule_tasks_from_thing_model(ProductId),
    ?assert(is_tuple(ScheduleResult)),
    
    % 测试任务参数解析
    Props = [
        #{
            <<"dataForm">> => #{
                <<"strategy">> => <<"采集值"/utf8>>,
                <<"order">> => 1,
                <<"interval">> => 5,
                <<"rounds">> => 10,
                <<"timeout">> => 30,
                <<"retry">> => 3
            },
            <<"identifier">> => <<"temperature">>,
            <<"accessMode">> => <<"r">>
        },
        #{
            <<"dataForm">> => #{
                <<"strategy">> => <<"采集值"/utf8>>,
                <<"order">> => 2,
                <<"interval">> => 10,
                <<"rounds">> => 5,
                <<"timeout">> => 20,
                <<"retry">> => 2
            },
            <<"identifier">> => <<"humidity">>,
            <<"accessMode">> => <<"r">>
        }
    ],
    
    TaskParams = dgiot_task:parse_task_parameters(Props),
    ?assert(is_list(TaskParams)),
    ?assertEqual(2, length(TaskParams)),
    
    % 验证任务参数
    lists:foreach(fun(Task) ->
        ?assert(is_map(Task)),
        ?assert(maps:is_key(identifier, Task)),
        ?assert(maps:is_key(interval, Task)),
        ?assert(maps:is_key(rounds, Task))
    end, TaskParams),
    
    % 测试停止任务
    ?assertEqual(ok, dgiot_task:stop_tasks(ProductId)),
    
    ok.

%%%===================================================================
%%% 测试辅助函数
%%%===================================================================

%% @doc 准备测试数据
prepare_test_data() ->
    #{
        product_id => <<"integration_test_product">>,
        device_addr => <<"integration_test_device">>,
        temperature => 25.5,
        humidity => 60.0,
        pressure => 1013.25,
        timestamp => erlang:system_time()
    }.

%% @doc 验证数据格式
validate_data_format(Data) when is_map(Data) ->
    RequiredFields = [<<"temperature">>, <<"humidity">>, <<"timestamp">>],
    lists:all(fun(Field) -> maps:is_key(Field, Data) end, RequiredFields);
validate_data_format(_) -> false.

%% @doc 清理测试环境
cleanup_test_environment() ->
    % 清理可能创建的测试数据
    Protocols = [<<"EXTERNAL_PROTOCOL">>, <<"TEST_PROTOCOL">>],
    lists:foreach(fun(Protocol) ->
        dgiot_data:delete({?RULE_ENGINE_TABLE, Protocol})
    end, Protocols),
    ok.
