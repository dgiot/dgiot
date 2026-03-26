%%%-------------------------------------------------------------------
%%% @doc 无人机测试结果存储（统一版本）
%%% 负责测试结果的持久化存储和查询
%%%-------------------------------------------------------------------
-module(dgiot_uav_test_storage).
-author("johnliu").

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    %% 保存
    save/1,
    save_step/2,
    
    %% 查询
    get/1,
    get_by_device/2,
    get_by_station/2,
    get_by_time_range/3,
    get_recent/1,
    
    %% 统计
    get_statistics/2,
    get_pass_rate/2,
    
    %% 清理
    cleanup/1,
    
    %% 测试
    test/0
]).

%% 执行记录（与test_executor共享，无类型标注避免编译错误）
-record(execution, {
    test_id,
    test_item_id,
    device_id,
    station_id,
    steps = [],
    current_step = 0,
    status = pending,
    start_time,
    end_time,
    step_results = [],
    context = #{}
}).

%%====================================================================
%% 保存函数
%%====================================================================

%% @doc 保存测试结果
save(Record) when is_record(Record, execution) ->
    save_execution_to_parse(Record);
save(#{test_id := _} = Map) ->
    save_map_to_parse(Map);
save(_) ->
    {error, invalid_format}.

save_execution_to_parse(#execution{
    test_id = TestId,
    test_item_id = TestItemId,
    device_id = DeviceId,
    station_id = StationId,
    status = Status,
    start_time = StartTime,
    end_time = EndTime,
    step_results = StepResults,
    context = Context
}) ->
    Duration = case EndTime of undefined -> 0; _ -> EndTime - StartTime end,
    TotalSteps = length(StepResults),
    PassedSteps = count_passed(StepResults),
    FailedSteps = count_failed(StepResults),
    
    ParseObject = #{
        <<"test_id">> => TestId,
        <<"test_item_id">> => TestItemId,
        <<"device_id">> => DeviceId,
        <<"station_id">> => StationId,
        <<"status">> => atom_to_binary(Status, utf8),
        <<"start_time">> => StartTime,
        <<"end_time">> => EndTime,
        <<"duration">> => Duration,
        <<"total_steps">> => TotalSteps,
        <<"passed_steps">> => PassedSteps,
        <<"failed_steps">> => FailedSteps,
        <<"steps">> => encode_steps(StepResults),
        <<"context">> => encode_context(Context),
        <<"created_at">> => erlang:system_time(millisecond)
    },
    
    case dgiot_parse:create_object(<<"TestResult">>, ParseObject) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            %% 同时保存到设备content
            update_device_content(DeviceId, ParseObject),
            ?LOG(info, "[STORAGE] 测试结果已保存 - TestId:~s, ObjectId:~s", [TestId, ObjectId]),
            {ok, ObjectId};
        {error, Reason} ->
            ?LOG(error, "[STORAGE] 保存失败 - ~p", [Reason]),
            {error, Reason}
    end.

save_map_to_parse(Map) ->
    ParseObject = maps:fold(fun
        (test_id, V, Acc) -> Acc#{<<"test_id">> => V};
        (test_item_id, V, Acc) -> Acc#{<<"test_item_id">> => V};
        (device_id, V, Acc) -> Acc#{<<"device_id">> => V};
        (station_id, V, Acc) -> Acc#{<<"station_id">> => V};
        (status, V, Acc) -> Acc#{<<"status">> => atom_to_binary(V, utf8)};
        (start_time, V, Acc) -> Acc#{<<"start_time">> => V};
        (end_time, V, Acc) -> Acc#{<<"end_time">> => V};
        (duration, V, Acc) -> Acc#{<<"duration">> => V};
        (steps, V, Acc) -> Acc#{<<"steps">> => encode_steps(V)};
        (_, _, Acc) -> Acc
    end, #{}, Map),
    
    dgiot_parse:create_object(<<"TestResult">>, ParseObject).

%% @doc 保存步骤结果
save_step(TestId, StepResult) ->
    StepObject = #{
        <<"test_id">> => TestId,
        <<"step_index">> => maps:get(step_index, StepResult, 0),
        <<"step_name">> => maps:get(step_name, StepResult, <<>>),
        <<"status">> => atom_to_binary(maps:get(status, StepResult, unknown), utf8),
        <<"timestamp">> => maps:get(timestamp, StepResult, erlang:system_time(millisecond)),
        <<"details">> => maps:get(details, StepResult, #{})
    },
    
    case dgiot_parse:create_object(<<"TestStepResult">>, StepObject) of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {ok, ObjectId};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 查询函数
%%====================================================================

%% @doc 获取测试结果详情
get(TestId) ->
    Query = #{<<"where">> => #{<<"test_id">> => TestId}},
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := [Result]}} ->
            Steps = get_steps_for_test(TestId),
            {ok, Result#{<<"steps">> => Steps}};
        {ok, #{<<"results">> := []}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取设备的测试历史
get_by_device(DeviceId, Options) ->
    Limit = maps:get(limit, Options, 50),
    Skip = maps:get(skip, Options, 0),
    
    Query = #{
        <<"where">> => #{<<"device_id">> => DeviceId},
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => Limit,
        <<"skip">> => Skip
    },
    
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取工位的测试历史
get_by_station(StationId, Options) ->
    Limit = maps:get(limit, Options, 50),
    Skip = maps:get(skip, Options, 0),
    
    Query = #{
        <<"where">> => #{<<"station_id">> => StationId},
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => Limit,
        <<"skip">> => Skip
    },
    
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 按时间范围查询
get_by_time_range(StartTime, EndTime, Options) ->
    Limit = maps:get(limit, Options, 100),
    
    Query = #{
        <<"where">> => #{
            <<"start_time">> => #{<<"$gte">> => StartTime, <<"$lte">> => EndTime}
        },
        <<"order">> => <<"-start_time">>,
        <<"limit">> => Limit
    },
    
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取最近的测试
get_recent(Limit) ->
    Query = #{
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => Limit
    },
    
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 统计函数
%%====================================================================

%% @doc 获取测试统计
get_statistics(StartTime, EndTime) ->
    case get_by_time_range(StartTime, EndTime, #{limit => 1000}) of
        {ok, Results} ->
            Stats = calculate_statistics(Results),
            {ok, Stats};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 获取通过率
get_pass_rate(StartTime, EndTime) ->
    case get_by_time_range(StartTime, EndTime, #{limit => 1000}) of
        {ok, Results} ->
            Total = length(Results),
            Passed = count_passed_results(Results),
            Rate = if Total > 0 -> Passed / Total * 100; true -> 0 end,
            {ok, Rate};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 清理函数
%%====================================================================

%% @doc 清理过期测试数据
cleanup(Days) ->
    ExpireTime = erlang:system_time(millisecond) - Days * 24 * 3600 * 1000,
    
    Query = #{
        <<"where">> => #{<<"createdAt">> => #{<<"$lt">> => ExpireTime}},
        <<"limit">> => 100
    },
    
    case dgiot_parse:query_object(<<"TestResult">>, Query) of
        {ok, #{<<"results">> := Results}} ->
            lists:foreach(fun(Result) ->
                ObjectId = maps:get(<<"objectId">>, Result),
                dgiot_parse:del_object(<<"TestResult">>, ObjectId)
            end, Results),
            ?LOG(info, "[STORAGE] 清理了 ~p 条过期测试记录", [length(Results)]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================
count_passed(StepResults) ->
    length([S || S <- StepResults, maps:get(status, S) =:= passed]).

count_failed(StepResults) ->
    length([S || S <- StepResults, maps:get(status, S) =:= failed]).

encode_steps(Steps) ->
    lists:map(fun(Step) ->
        #{
            <<"step_index">> => maps:get(step_index, Step, 0),
            <<"step_name">> => maps:get(step_name, Step, <<>>),
            <<"status">> => atom_to_binary(maps:get(status, Step, unknown), utf8),
            <<"timestamp">> => maps:get(timestamp, Step, 0),
            <<"details">> => maps:get(details, Step, #{})
        }
    end, Steps).

encode_context(Context) ->
    maps:map(fun(_K, V) -> encode_value(V) end, Context).

encode_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
encode_value(V) when is_number(V) -> V;
encode_value(V) when is_binary(V) -> V;
encode_value(V) when is_map(V) -> encode_context(V);
encode_value(V) -> dgiot_utils:to_binary(V).

update_device_content(DeviceId, TestResult) ->
    try
        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
            {ok, Device} ->
                Content = maps:get(<<"content">>, Device, #{}),
                NewContent = Content#{
                    <<"last_test">> => #{
                        <<"test_id">> => maps:get(<<"test_id">>, TestResult),
                        <<"status">> => maps:get(<<"status">>, TestResult),
                        <<"time">> => erlang:system_time(millisecond)
                    }
                },
                dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"content">> => NewContent});
            _ -> ok
        end
    catch _:_ -> ok end.

get_steps_for_test(TestId) ->
    Query = #{<<"where">> => #{<<"test_id">> => TestId}, <<"order">> => <<"step_index">>},
    case dgiot_parse:query_object(<<"TestStepResult">>, Query) of
        {ok, #{<<"results">> := Results}} -> Results;
        _ -> []
    end.

calculate_statistics(Results) ->
    Total = length(Results),
    Passed = count_passed_results(Results),
    Failed = Total - Passed,
    
    Durations = [maps:get(<<"duration">>, R, 0) || R <- Results, maps:get(<<"duration">>, R, 0) > 0],
    AvgDuration = if length(Durations) > 0 -> lists:sum(Durations) div length(Durations); true -> 0 end,
    MaxDuration = if Durations /= [] -> lists:max(Durations); true -> 0 end,
    MinDuration = if Durations /= [] -> lists:min(Durations); true -> 0 end,
    
    #{
        total => Total,
        passed => Passed,
        failed => Failed,
        pass_rate => if Total > 0 -> Passed / Total * 100; true -> 0 end,
        avg_duration => AvgDuration,
        max_duration => MaxDuration,
        min_duration => MinDuration
    }.

count_passed_results(Results) ->
    length([R || R <- Results, maps:get(<<"status">>, R, <<>>) =:= <<"completed">>]).

%%====================================================================
%% 测试函数
%%====================================================================
test() ->
    io:format("~n========== 测试存储模块测试 ==========~n", []),
    
    %% 测试保存
    TestData = #{
        test_id => <<"test_001">>,
        test_item_id => <<"item_001">>,
        device_id => <<"device_001">>,
        station_id => 1700,
        status => completed,
        start_time => erlang:system_time(millisecond) - 5000,
        end_time => erlang:system_time(millisecond),
        duration => 5000,
        steps => [
            #{step_index => 0, step_name => <<"电压检查"/utf8>>, status => passed, timestamp => 123456, details => #{}},
            #{step_index => 1, step_name => <<"电流检查"/utf8>>, status => passed, timestamp => 123456, details => #{}}
        ]
    },
    
    case save(TestData) of
        {ok, ObjectId} ->
            io:format("✓ 保存成功: ~s~n", [ObjectId]);
        {error, _SaveReason} ->
            io:format("✗ 保存失败~n")
    end,
    
    %% 测试查询
    case get_recent(5) of
        {ok, Results} ->
            io:format("✓ 最近测试数量: ~p~n", [length(Results)]);
        {error, _QueryReason} ->
            io:format("✗ 查询失败~n")
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.
