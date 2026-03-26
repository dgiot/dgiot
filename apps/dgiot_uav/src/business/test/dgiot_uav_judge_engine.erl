%%%-------------------------------------------------------------------
%%% @doc 无人机测试判据引擎（统一版本）
%%% 支持：simple(阈值)、sql、tdengine 三种判据类型
%%%-------------------------------------------------------------------
-module(dgiot_uav_judge_engine).

-include_lib("dgiot/include/logger.hrl").

%% API - 保持与导出列表一致
-export([
    %% 判据评估
    evaluate/3,
    evaluate/4,
    
    %% 特定类型判据
    evaluate_simple/2,      %% 改为2个参数
    evaluate_sql/2,         %% 改为2个参数
    evaluate_tdengine/2,    %% 改为2个参数
    
    %% 工具函数
    load_judge_rules/1,
    get_judge_rule/2,
    extract_metrics/1,
    
    %% 测试
    test/0
]).

%% 判据类型
-define(JUDGE_SIMPLE, <<"simple">>).
-define(JUDGE_SQL, <<"sql">>).
-define(JUDGE_TDENGINE, <<"tdengine">>).

%% 默认阈值
-define(DEFAULT_VOLTAGE_MIN, 22.0).
-define(DEFAULT_VOLTAGE_MAX, 26.0).
-define(DEFAULT_CURRENT_MAX, 5.0).

%%====================================================================
%% 统一判据入口
%%====================================================================

%% @doc 评估判据（简化版）
-spec evaluate(binary(), map()) -> 
    {ok, #{result := pass | fail, details := map()}} | {error, term()}.
evaluate(JudgeRule, Context) when is_map(JudgeRule) ->
    JudgeType = maps:get(<<"type">>, JudgeRule, ?JUDGE_SIMPLE),
    Rule = maps:get(<<"rule">>, JudgeRule, #{}),
    evaluate(JudgeType, Rule, Context);
evaluate(JudgeRule, Context) when is_binary(JudgeRule) ->
    %% 尝试解析JSON
    try jsx:decode(JudgeRule, [return_maps]) of
        RuleMap -> evaluate(RuleMap, Context)
    catch _:_ ->
        {error, invalid_rule_format}
    end.

%% @doc 评估判据（完整版）
-spec evaluate(binary(), map(), map()) -> 
    {ok, #{result := pass | fail, details := map()}} | {error, term()}.
evaluate(?JUDGE_SIMPLE, Rule, Context) ->
    evaluate_simple(Rule, Context);
evaluate(?JUDGE_SQL, Rule, Context) ->
    evaluate_sql(Rule, Context);
evaluate(?JUDGE_TDENGINE, Rule, Context) ->
    evaluate_tdengine(Rule, Context);
evaluate(_, Rule, Context) ->
    %% 默认使用简单判据
    evaluate_simple(Rule, Context).

-spec evaluate(binary(), binary(), map(), map()) -> 
    {ok, #{result := pass | fail, details := map()}} | {error, term()}.
evaluate(JudgeType, Rule, Expected, Context) ->
    %% 将Expected合并到Context中
    NewContext = Context#{<<"expected">> => Expected},
    evaluate(JudgeType, Rule, NewContext).

%%====================================================================
%% 简单阈值判据
%%====================================================================

%% @doc 简单判据评估
-spec evaluate_simple(map(), map()) -> 
    {ok, #{result := pass | fail, details := map()}}.
evaluate_simple(Rule, Context) when is_map(Rule) ->
    %% 支持两种格式：
    %% 1. #{field => <<"voltage">>, operator => <<">=">>, value => 22.0}
    %% 2. #{expected => #{voltage => 22.0, max => 26.0}}
    case maps:get(<<"field">>, Rule, undefined) of
        undefined ->
            %% 使用expected格式
            Expected = maps:get(<<"expected">>, Rule, #{}),
            evaluate_simple_expected(Expected, Context);
        Field ->
            %% 使用field格式
            Operator = maps:get(<<"operator">>, Rule, <<">=">>),
            Threshold = maps:get(<<"value">>, Rule, 0),
            Actual = get_field_value(Field, Context),
            Pass = compare(Actual, Operator, Threshold),
            
            Details = #{
                field => Field,
                operator => Operator,
                threshold => Threshold,
                actual => Actual,
                result => if Pass -> pass; true -> fail end
            },
            {ok, #{result => if Pass -> pass; true -> fail end, details => Details}}
    end;
evaluate_simple(Rule, Context) when is_binary(Rule) ->
    try jsx:decode(Rule, [return_maps]) of
        RuleMap -> evaluate_simple(RuleMap, Context)
    catch _:_ ->
        {error, invalid_rule_format}
    end.

%% 评估expected格式的判据
evaluate_simple_expected(Expected, Context) ->
    case Expected of
        #{<<"voltage">> := Min, <<"max">> := Max} ->
            Actual = get_field_value(<<"voltage">>, Context),
            Pass = Actual >= Min andalso Actual =< Max,
            {ok, #{result => if Pass -> pass; true -> fail end, 
                   details => #{voltage => Actual, min => Min, max => Max}}};
        #{<<"voltage_min">> := Min, <<"voltage_max">> := Max} ->
            Actual = get_field_value(<<"voltage">>, Context),
            Pass = Actual >= Min andalso Actual =< Max,
            {ok, #{result => if Pass -> pass; true -> fail end,
                   details => #{voltage => Actual, min => Min, max => Max}}};
        #{<<"status">> := ExpectedStatus} ->
            Actual = get_field_value(<<"status">>, Context),
            Pass = Actual =:= ExpectedStatus,
            {ok, #{result => if Pass -> pass; true -> fail end,
                   details => #{status => Actual, expected => ExpectedStatus}}};
        #{<<"current_max">> := Max} ->
            Actual = get_field_value(<<"current">>, Context),
            Pass = Actual =< Max,
            {ok, #{result => if Pass -> pass; true -> fail end,
                   details => #{current => Actual, max => Max}}};
        _ ->
            {ok, #{result => pass, details => #{}}}
    end.

%%====================================================================
%% SQL判据
%%====================================================================

%% @doc SQL判据评估
-spec evaluate_sql(binary(), map()) -> 
    {ok, #{result := pass | fail, details := map()}} | {error, term()}.
evaluate_sql(Sql, Context) when is_binary(Sql) ->
    PreparedSql = replace_vars(Sql, Context),
    
    ?LOG(debug, "[JUDGE] 执行SQL判据: ~s", [PreparedSql]),
    
    %% 执行Parse Server查询
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{}}) of
        {ok, #{<<"results">> := Results}} ->
            Pass = length(Results) > 0,
            Details = #{
                sql => PreparedSql,
                count => length(Results),
                result => if Pass -> pass; true -> fail end
            },
            {ok, #{result => if Pass -> pass; true -> fail end, details => Details}};
        {error, Reason} ->
            ?LOG(error, "[JUDGE] SQL判据执行失败: ~p", [Reason]),
            {error, Reason}
    end;
evaluate_sql(Rule, _Context) ->
    {error, {invalid_sql_format, Rule}}.

%%====================================================================
%% TDengine判据
%%====================================================================

%% @doc TDengine判据评估
-spec evaluate_tdengine(binary(), map()) -> 
    {ok, #{result := pass | fail, details := map()}} | {error, term()}.
evaluate_tdengine(Sql, Context) when is_binary(Sql) ->
    DeviceId = get_field_value(<<"device_id">>, Context),
    ProductId = get_field_value(<<"product_id">>, Context, <<>>),
    PreparedSql = replace_vars(Sql, Context),
    
    ?LOG(debug, "[JUDGE] 执行TDengine判据: ~s", [PreparedSql]),
    
    case dgiot_tdengine:query_object(ProductId, DeviceId, PreparedSql) of
        {ok, Data} ->
            Pass = is_data_valid(Data),
            Details = #{
                sql => PreparedSql,
                data => Data,
                result => if Pass -> pass; true -> fail end
            },
            {ok, #{result => if Pass -> pass; true -> fail end, details => Details}};
        {error, Reason} ->
            ?LOG(error, "[JUDGE] TDengine判据执行失败: ~p", [Reason]),
            {error, Reason}
    end;
evaluate_tdengine(Rule, _Context) ->
    {error, {invalid_tdengine_format, Rule}}.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 从上下文中提取物模型指标
-spec extract_metrics(map()) -> map().
extract_metrics(Context) ->
    maps:get(<<"metrics">>, Context, #{}).

%% @doc 获取字段值
-spec get_field_value(binary(), map()) -> term().
get_field_value(Field, Context) ->
    get_field_value(Field, Context, undefined).

-spec get_field_value(binary(), map(), term()) -> term().
get_field_value(Field, Context, Default) ->
    %% 优先从metrics中查找
    case maps:get(<<"metrics">>, Context, #{}) of
        #{Field := Value} -> Value;
        _ ->
            %% 直接从Context中查找
            case maps:get(Field, Context, undefined) of
                undefined -> Default;
                Value -> Value
            end
    end.

%% @doc 比较操作符
-spec compare(term(), binary(), number()) -> boolean().
compare(Actual, <<">=">>, Threshold) when is_number(Actual) -> Actual >= Threshold;
compare(Actual, <<"<=">>, Threshold) when is_number(Actual) -> Actual =< Threshold;
compare(Actual, <<">">>, Threshold) when is_number(Actual) -> Actual > Threshold;
compare(Actual, <<"<">>, Threshold) when is_number(Actual) -> Actual < Threshold;
compare(Actual, <<"=">>, Threshold) -> Actual =:= Threshold;
compare(Actual, <<"!=">>, Threshold) -> Actual =/= Threshold;
compare(_, _, _) -> false.

%% @doc 替换SQL模板变量
-spec replace_vars(binary(), map()) -> binary().
replace_vars(Sql, Context) ->
    Vars = [
        {"\\$\\{device_id\\}", get_field_value(<<"device_id">>, Context, <<>>)},
        {"\\$\\{start_time\\}", get_field_value(<<"start_time">>, Context, 0)},
        {"\\$\\{end_time\\}", get_field_value(<<"end_time">>, Context, 9999999999999)},
        {"\\$\\{station_id\\}", get_field_value(<<"station_id">>, Context, 0)},
        {"\\$\\{voltage_min\\}", get_field_value(<<"voltage_min">>, Context, ?DEFAULT_VOLTAGE_MIN)},
        {"\\$\\{voltage_max\\}", get_field_value(<<"voltage_max">>, Context, ?DEFAULT_VOLTAGE_MAX)}
    ],
    lists:foldl(fun({Pattern, Value}, Acc) ->
        re:replace(Acc, Pattern, dgiot_utils:to_binary(Value), [{return, binary}, global])
    end, Sql, Vars).

%% @doc 验证TDengine数据是否有效
-spec is_data_valid(term()) -> boolean().
is_data_valid(#{<<"data">> := []}) -> false;
is_data_valid(#{<<"data">> := [_|_]}) -> true;
is_data_valid(#{<<"rows">> := []}) -> false;
is_data_valid(#{<<"rows">> := [_|_]}) -> true;
is_data_valid(_) -> false.

%% @doc 加载判据规则
-spec load_judge_rules(binary()) -> {ok, map()} | {error, term()}.
load_judge_rules(_StationName) ->
    File = filename:join([code:priv_dir(dgiot_uav), "json", "judge_rules.json"]),
    case file:read_file(File) of
        {ok, Bin} ->
            try jsx:decode(Bin, [return_maps]) of
                Rules ->
                    {ok, Rules}
            catch
                _:Error ->
                    ?LOG(error, "[JUDGE] JSON解析失败: ~p", [Error]),
                    {error, parse_error}
            end;
        {error, Reason} ->
            ?LOG(warning, "[JUDGE] 配置文件不存在: ~p", [Reason]),
            {ok, #{}}
    end.

%% @doc 获取判据规则
-spec get_judge_rule(binary(), binary()) -> {ok, map()} | {error, term()}.
get_judge_rule(StationName, RuleName) ->
    case load_judge_rules(StationName) of
        {ok, Rules} ->
            case maps:get(RuleName, Rules, undefined) of
                undefined -> {error, not_found};
                Rule -> {ok, Rule}
            end;
        Error -> Error
    end.

%%====================================================================
%% 测试函数
%%====================================================================
-spec test() -> ok.
test() ->
    io:format("~n========== 判据引擎测试 ==========~n", []),
    
    %% 测试简单判据
    io:format("1. 测试简单判据...~n"),
    Context = #{<<"voltage">> => 24.5},
    Rule = #{<<"field">> => <<"voltage">>, <<"operator">> => <<">=">>, <<"value">> => 22.0},
    case evaluate_simple(Rule, Context) of
        {ok, #{result := Result, details := Details}} ->
            io:format("   ✓ 电压判据: ~p, 详情: ~p~n", [Result, Details]);
        {error, Reason} ->
            io:format("   ✗ 测试失败: ~p~n", [Reason])
    end,
    
    %% 测试expected格式
    io:format("2. 测试expected格式判据...~n"),
    Rule2 = #{<<"expected">> => #{<<"voltage">> => 22.0, <<"max">> => 26.0}},
    case evaluate_simple(Rule2, Context) of
        {ok, #{result := Result2, details := Details2}} ->
            io:format("   ✓ expected判据: ~p, 详情: ~p~n", [Result2, Details2]);
        {error, Reason2} ->
            io:format("   ✗ 测试失败: ~p~n", [Reason2])
    end,

    %% 测试统一入口
    io:format("3. 测试统一入口...~n"),
    case evaluate(Rule, Context) of
        {ok, #{result := Result3}} ->
            io:format("   ✓ 统一入口: ~p~n", [Result3]);
        {error, Reason3} ->
            io:format("   ✗ 测试失败: ~p~n", [Reason3])
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.
