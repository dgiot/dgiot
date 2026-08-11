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

%% @doc 任务统计服务层（简洁版）
%% 负责任务统计的核心业务逻辑，遵循简洁高效原则
-module(dgiot_task_service).
-include("dgiot_task.hrl").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

%% API导出 - 只导出核心业务函数
-export([save_td/4, save_td_no_match/4, smart_save_td/4]).
-export([get_props/1, get_control/3, get_collection/4, get_calculated/4, get_instruct/2, get_storage/2]).
-export([get_statistic/7, get_last_value/4, compare/3]).
-export([string2value/2, string2value/3]).
-export([needs_protocol_parsing/1, call_protocol_hook/4]).
-export([rule_engine_transform/2, register_rule/2, get_rules/1]).
-export([schedule_tasks_from_thing_model/1, stop_tasks/1, parse_task_parameters/1]).
-export([execute_task/2, get_current_round/2, update_round/3, execute_round/3]).
-export([generate_collection_command/4, send_collection_command/2]).

%%%===================================================================
%%% 数据保存函数（核心业务）
%%%===================================================================

%% @doc 保存数据到TDengine
save_td(ProductId, DevAddr, Ack, _AppData) ->
    Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
    dgiot_mqttc_channel:send(ProductId, DevAddr, Topic, Ack),
    case maps:size(Ack) of
        0 -> #{};
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = dgiot_product:get_interval(ProductId),
            CacheData = dgiot_task_dao:merge_cache_data(DeviceId, Ack, Interval),
            Props = get_props(ProductId),
            Collection = get_collection(ProductId, [], CacheData, Props),
            AllData = get_calculated(ProductId, DevAddr, Collection, Props),
            Storage = get_storage(AllData, Props),
            dgiot_task_dao:save_cache_data(DeviceId, CacheData),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval)
    end.

%% @doc 智能保存数据
smart_save_td(ProductId, DevAddr, Data, Context) ->
    ?LOG(info, "Smart processing data for ProductId=~p, DevAddr=~p", [ProductId, DevAddr]),
    save_td(ProductId, DevAddr, Data, Context).

%% @doc 保存数据（无匹配模式）
save_td_no_match(ProductId, DevAddr, Ack, AppData) ->
    case length(maps:to_list(Ack)) of
        0 -> #{};
        _ ->
            Props = get_props(ProductId),
            Collection = get_collection(ProductId, [], Ack, Props),
            Calculated = get_calculated(ProductId, DevAddr, Collection, Props),
            Storage = get_storage(Calculated, Props),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = maps:get(<<"interval">>, AppData, 3),
            AllData = dgiot_task_dao:merge_cache_data(DeviceId, Storage, Interval),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval),
            AllData
    end.

%%%===================================================================
%%% 物模型相关函数
%%%===================================================================

%% @doc 获取物模型属性列表
get_props(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} -> Props;
        _Error -> []
    end.

%% @doc 获取采集数据（主动上报模式）
get_collection(ProductId, [], Payload, Props) ->
    lists:foldl(fun(X, Acc2) ->
        case Acc2 of
            error -> Acc2;
            _ ->
                case X of
                    #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                      <<"dataType">> := DataType,
                      <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                        dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                    _ -> Acc2
                end
        end
    end, Payload, Props);

%% @doc 获取采集数据（指定标识符模式）
get_collection(ProductId, Dis, Payload, Props) ->
    lists:foldl(fun(Identifier, Acc1) ->
        lists:foldl(fun(X, Acc2) ->
            case Acc2 of
                error -> Acc2;
                _ ->
                    case X of
                        #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                          <<"dataType">> := DataType,
                          <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                            dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                        _ -> Acc2
                    end
            end
        end, Acc1, Props)
    end, Payload, Dis).

%% @doc 获取计算值
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            error -> Acc;
            _ ->
                case X of
                    #{<<"isaccumulate">> := true,
                      <<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>},
                      <<"dataSource">> := #{<<"key">> := Key} = DataSource} ->
                        %% 统计计算：持续时间、频率等
                        case maps:get(Key, Calculated, not_find) of
                            not_find -> Acc;
                            KeyValue -> get_statistic(ProductId, DevAddr, Key, Identifier, dgiot_utils:to_int(KeyValue), DataSource, Acc)
                        end;
                    #{<<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection}} ->
                        %% 公式计算：使用增强的公式计算器
                        case calculate_formula_with_enhanced_calculator(Collection, Calculated, X) of
                            undefined -> 
                                ?LOG(warning, "公式计算失败: Identifier=~p, Collection=~p", [Identifier, Collection]),
                                maps:without([Identifier], Acc);
                            Value -> Acc#{Identifier => Value}
                        end;
                    _ -> Acc
                end
        end
    end, Calculated, Props).

%% @doc 使用增强公式计算器计算公式
calculate_formula_with_enhanced_calculator(Collection, Calculated, Prop) ->
    try
        %% 1. 提取变量并验证
        RequiredVars = dgiot_formula_calculator:extract_variables(Collection),
        
        %% 2. 获取变量值
        Variables = maps:with(RequiredVars, Calculated),
        
        %% 3. 检查是否有缺失变量
        case maps:size(Variables) =:= length(RequiredVars) of
            true ->
                %% 4. 使用公式计算器计算
                Options = #{
                    precision => 3,
                    timeout => 5000
                },
                dgiot_formula_calculator:calculate_formula(Collection, Variables, Prop, Options);
            false ->
                %% 5. 尝试使用传统方法（向后兼容）
                calculate_formula_with_legacy_method(Collection, Calculated, Prop)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG(error, "增强公式计算器异常: Class=~p, Reason=~p, Stacktrace=~p", 
                 [Class, Reason, Stacktrace]),
            calculate_formula_with_legacy_method(Collection, Calculated, Prop)
    end.

%% @doc 使用传统方法计算公式（向后兼容）
calculate_formula_with_legacy_method(Collection, Calculated, _Prop) ->
    %% 替换变量
    Str1 = maps:fold(fun(K, V, Acc2) ->
        Pattern = dgiot_utils:to_list(<<"%%{", K/binary, "}">>),
        Replacement = dgiot_utils:to_list(V),
        re:replace(Acc2, Pattern, Replacement, [global, {return, list}])
    end, dgiot_utils:to_list(Collection), Calculated),
    
    %% 执行计算
    case string2value(Str1, <<"float">>) of
        error -> undefined;
        Value -> Value
    end.

%% @doc 获取存储值
get_storage(Calculated, Props) ->
    lists:foldl(fun(#{<<"isstorage">> := true, <<"identifier">> := Identifier}, Acc) ->
        case maps:find(Identifier, Calculated) of
            {ok, Value} -> Acc#{Identifier => Value};
            _ -> Acc
        end;
    (_, Acc) -> Acc
    end, #{}, Props).

%% @doc 获取控制值
get_control(Round, Data, Control) ->
    ?LOG(debug, "Getting control value: Round=~p, Data=~p, Control=~p", [Round, Data, Control]),
    #{round => Round, data => Data, control => Control}.

%% @doc 获取指令
get_instruct(ProductId, Round) ->
    ?LOG(debug, "Getting instructions: ProductId=~p, Round=~p", [ProductId, Round]),
    [].

%% @doc 获取上次统计值
get_last_value(ProductId, DevAddr, Key, Identifier) ->
    ?LOG(debug, "Getting last value: ProductId=~p, DevAddr=~p, Key=~p, Identifier=~p", 
         [ProductId, DevAddr, Key, Identifier]),
    not_find.

%%%===================================================================
%%% 协议处理函数
%%%===================================================================

%% @doc 判断数据是否需要协议解析
needs_protocol_parsing(Data) when is_binary(Data) ->
    case Data of
        <<>> -> false;
        _ -> true
    end;
needs_protocol_parsing(_) -> false.

%% @doc 调用协议钩子
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    ?LOG(info, "Calling protocol hook: ProductId=~p, DevAddr=~p, Protocol=~p", [ProductId, DevAddr, Protocol]),
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} -> {parsed, ParsedData};
        _ -> {error, protocol_not_supported}
    end.

%%%===================================================================
%%% 统计计算函数
%%%===================================================================

%% @doc 获取统计值
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"duration">>} = DataSource, Acc) ->
    dgiot_task_utils:handle_duration_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc);
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"frequency">>} = DataSource, Acc) ->
    dgiot_task_utils:handle_frequency_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc);
get_statistic(_, _, _, _, _, _, Acc) -> Acc.

%% @doc 比较两个值
compare(Value1, <<"LT">>, Value2) -> Value1 < Value2;
compare(Value1, <<"LE">>, Value2) -> Value1 =< Value2;
compare(Value1, <<"GT">>, Value2) -> Value1 > Value2;
compare(Value1, <<"GE">>, Value2) -> Value1 >= Value2;
compare(Value1, <<"EQ">>, Value2) -> Value1 == Value2;
compare(Value1, <<"NE">>, Value2) -> Value1 /= Value2;
compare(_Value1, _CompareType, _Value2) -> false.

%%%===================================================================
%%% 工具函数
%%%===================================================================

%% @doc 字符串转值
string2value(Str, <<"TEXT">>) when is_list(Str) ->
    case string:find(Str, "%%") of
        nomatch -> Str;
        _ -> error
    end;
string2value(Str, _) ->
    case string:find(Str, "%%") of
        nomatch ->
            {ok, Tokens, _} = erl_scan:string(Str ++ "."),
            case erl_parse:parse_exprs(Tokens) of
                {error, _} -> error;
                {ok, Exprs} ->
                    Bindings = erl_eval:new_bindings(),
                    case catch erl_eval:exprs(Exprs, Bindings) of
                        {value, Value, _} -> Value;
                        _ -> 0
                    end
            end;
        _ -> error
    end.

%% @doc 字符串转值（带规格）
string2value(Str, Type, Specs) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    case string2value(Str, Type1) of
        error -> error;
        Value ->
            case Type1 of
                <<"INT">> -> round(Value);
                Type2 when Type2 == <<"FLOAT">>; Type2 == <<"DOUBLE">> ->
                    Precision = maps:get(<<"precision">>, Specs, 3),
                    dgiot_utils:to_float(Value, Precision);
                _ -> Value
            end
    end.

%%%===================================================================
%%% 规则引擎函数
%%%===================================================================

%% @doc 规则引擎转换
rule_engine_transform(ThirdPartyData, Protocol) ->
    ?LOG(info, "Transforming third-party data: Protocol=~p", [Protocol]),
    Rules = get_rules(Protocol),
    apply_rules(ThirdPartyData, Rules).

%% @doc 应用转换规则
apply_rules(Data, Rules) when is_map(Data), is_list(Rules) ->
    lists:foldl(fun(Rule, Acc) -> apply_single_rule(Data, Rule, Acc) end, #{}, Rules).

%% @doc 应用单个转换规则
apply_single_rule(Data, #{<<"source">> := Source, <<"target">> := Target, <<"transform">> := Transform}, Acc) ->
    SourceValue = maps:get(Source, Data, undefined),
    TargetValue = apply_transform(SourceValue, Transform),
    Acc#{Target => TargetValue};
apply_single_rule(_, _, Acc) -> Acc.

%% @doc 应用转换函数
apply_transform(Value, <<"value">>) -> Value;
apply_transform(Value, Transform) when is_binary(Transform) ->
    try
        Expression = re:replace(Transform, "value", dgiot_utils:to_list(Value), [global, {return, list}]),
        string2value(Expression, <<"float">>)
    catch _:_ ->
        ?LOG(error, "Transform expression error: Value=~p, Transform=~p", [Value, Transform]),
        Value
    end;
apply_transform(Value, _) -> Value.

%% @doc 注册转换规则
register_rule(Protocol, Rule) ->
    dgiot_data:insert({?RULE_ENGINE_TABLE, Protocol}, Rule).

%% @doc 获取协议规则
get_rules(Protocol) ->
    dgiot_data:get({?RULE_ENGINE_TABLE, Protocol}, []).

%%%===================================================================
%%% 任务编排函数
%%%===================================================================

%% @doc 从物模型调度任务
schedule_tasks_from_thing_model(ProductId) ->
    ?LOG(info, "Scheduling tasks from thing model: ProductId=~p", [ProductId]),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Tasks = parse_task_parameters(Props),
            TaskQueue = create_task_queue(Tasks),
            start_task_scheduler(ProductId, TaskQueue);
        Error ->
            ?LOG(error, "Failed to load product config: ~p", [Error]),
            Error
    end.

%% @doc 解析任务参数
parse_task_parameters(Props) ->
    lists:filtermap(fun(Prop) ->
        case Prop of
            #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
              <<"identifier">> := Identifier} when Strategy =:= <<"采集值"/utf8>> ->
                {true, #{
                    order => maps:get(<<"order">>, DataForm, ?MAX_TASK_ORDER),
                    interval => maps:get(<<"interval">>, DataForm, ?DEFAULT_TASK_INTERVAL),
                    rounds => maps:get(<<"rounds">>, DataForm, ?DEFAULT_TASK_ROUNDS),
                    timeout => maps:get(<<"timeout">>, DataForm, 30),
                    retry => maps:get(<<"retry">>, DataForm, 3),
                    identifier => Identifier,
                    data_form => DataForm,
                    prop => Prop
                }};
            _ -> false
        end
    end, Props).

%% @doc 创建任务队列
create_task_queue(Tasks) ->
    SortedTasks = lists:sort(fun(A, B) ->
        maps:get(order, A, ?MAX_TASK_ORDER) =< maps:get(order, B, ?MAX_TASK_ORDER)
    end, Tasks),
    {StaggeredTasks, _} = lists:foldl(fun(Task, {Acc, Offset}) ->
        NewTask = Task#{start_offset => Offset},
        NewOffset = Offset + maps:get(interval, Task, ?DEFAULT_TASK_INTERVAL) div 2,
        {[NewTask | Acc], NewOffset}
    end, {[], 0}, SortedTasks),
    lists:reverse(StaggeredTasks).

%% @doc 启动任务调度器
start_task_scheduler(ProductId, TaskQueue) ->
    ?LOG(info, "Starting task scheduler: ProductId=~p, TaskCount=~p", [ProductId, length(TaskQueue)]),
    dgiot_data:insert({?TASK_SCHEDULER_TABLE, ProductId}, TaskQueue),
    lists:foreach(fun(Task) -> start_single_task(ProductId, Task) end, TaskQueue),
    ok.

%% @doc 启动单个任务
start_single_task(ProductId, Task) ->
    Identifier = maps:get(identifier, Task),
    Interval = maps:get(interval, Task, ?DEFAULT_TASK_INTERVAL),
    StartOffset = maps:get(start_offset, Task, 0),
    ?LOG(info, "Starting task: ProductId=~p, Identifier=~p, Interval=~p, Offset=~p", 
         [ProductId, Identifier, Interval, StartOffset]),
    {ok, TimerRef} = timer:apply_interval(Interval * 1000, ?MODULE, execute_task, [ProductId, Task]),
    dgiot_data:insert({task_timer, ProductId, Identifier}, TimerRef),
    ok.

%% @doc 执行任务
execute_task(ProductId, Task) ->
    Identifier = maps:get(identifier, Task),
    Rounds = maps:get(rounds, Task, ?DEFAULT_TASK_ROUNDS),
    CurrentRound = get_current_round(ProductId, Identifier),
    case CurrentRound < Rounds of
        true ->
            execute_round(ProductId, Task, CurrentRound),
            update_round(ProductId, Identifier, CurrentRound + 1);
        false ->
            ?LOG(info, "Task completed all rounds: ProductId=~p, Identifier=~p", [ProductId, Identifier]),
            complete_task(ProductId, Identifier)
    end.

%% @doc 获取当前轮次
get_current_round(ProductId, Identifier) ->
    dgiot_data:get({task_round, ProductId, Identifier}, 0).

%% @doc 更新轮次
update_round(ProductId, Identifier, Round) ->
    dgiot_data:insert({task_round, ProductId, Identifier}, Round).

%% @doc 执行轮次
execute_round(ProductId, Task, Round) ->
    Identifier = maps:get(identifier, Task),
    Prop = maps:get(prop, Task),
    ?LOG(info, "Executing round: ProductId=~p, Identifier=~p, Round=~p", [ProductId, Identifier, Round]),
    case Prop of
        #{<<"dataSource">> := DataSource, <<"dataForm">> := DataForm} ->
            Protocol = maps:get(<<"protocol">>, DataForm, <<"MODBUSRTU">>),
            AccessMode = maps:get(<<"accessMode">>, Prop, <<"r">>),
            Command = generate_collection_command(Protocol, AccessMode, DataSource, Round),
            send_collection_command(ProductId, Command);
        _ -> ?LOG(warning, "Invalid task configuration: ~p", [Task])
    end.

%% @doc 生成采集指令
generate_collection_command(Protocol, AccessMode, DataSource, Round) ->
    #{
        protocol => Protocol,
        access_mode => AccessMode,
        data_source => DataSource,
        round => Round,
        timestamp => dgiot_datetime:now_ms()
    }.

%% @doc 发送采集指令
send_collection_command(ProductId, Command) ->
    ?LOG(debug, "Sending collection command: ProductId=~p, Command=~p", [ProductId, Command]),
    ok.

%% @doc 完成任务
complete_task(ProductId, Identifier) ->
    case dgiot_data:get({task_timer, ProductId, Identifier}) of
        not_find -> ok;
        TimerRef ->
            timer:cancel(TimerRef),
            dgiot_data:delete({task_timer, ProductId, Identifier})
    end,
    ?LOG(info, "Task completed: ProductId=~p, Identifier=~p", [ProductId, Identifier]).

%% @doc 停止任务
stop_tasks(ProductId) ->
    ?LOG(info, "Stopping all tasks for product: ~p", [ProductId]),
    case dgiot_data:get({?TASK_SCHEDULER_TABLE, ProductId}) of
        not_find -> ok;
        TaskQueue ->
            lists:foreach(fun(Task) ->
                Identifier = maps:get(identifier, Task),
                complete_task(ProductId, Identifier)
            end, TaskQueue),
            dgiot_data:delete({?TASK_SCHEDULER_TABLE, ProductId}),
            ok
    end.

%% @doc 处理数据
dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, _Interval) ->
    NotificationTopic = <<"$dg/user/alarm/", ProductId/binary, "/", DeviceId/binary, "/properties/report">>,
    dgiot_mqtt:publish(DeviceId, NotificationTopic, dgiot_json:encode(AllData)),
    ChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"DGIOTTOPO">>, <<"TOPO组态通道"/utf8>>),
    try
        dgiot_channelx:do_message(ChannelId, {topo_thing, ProductId, DeviceId, AllData})
    catch _:_ -> pass
    end,
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage),
    dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1),
    Channel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p save td => ProductId ~p DevAddr ~p ~ts ", [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(dgiot_json:encode(Storage))]),
    dgiot_data:insert({last_data, DeviceId}, AllData),
    ?LOG(info, "Updated last_data cache for device ~p", [DeviceId]),
    Storage.
