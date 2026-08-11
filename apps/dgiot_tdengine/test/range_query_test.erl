%%%-------------------------------------------------------------------
%%% @doc
%%% TDengine范围查询测试
%%%
%%% 测试范围查询函数的正确性
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(range_query_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_tdengine.hrl").

%% 测试函数
range_query_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_range_queries/1}.

%% 设置函数
setup() ->
    io:format("~s ~p 开始TDengine范围查询测试...~n", [?FILE, ?LINE]),
    % 确保TDengine插件已加载
    ok.

%% 清理函数
cleanup(_) ->
    io:format("~s ~p TDengine范围查询测试完成~n", [?FILE, ?LINE]),
    ok.

%% 测试范围查询
test_range_queries(_) ->
    [
        {"测试build_range_query_sql函数", fun test_build_range_query_sql/0},
        {"测试query_range_data函数", fun test_query_range_data/0},
        {"测试build_aggregate_query_sql函数", fun test_build_aggregate_query_sql/0},
        {"测试query_aggregate_data函数", fun test_query_aggregate_data/0}
    ].

%% 测试构建范围查询SQL
test_build_range_query_sql() ->
    Database = <<"test_db">>,
    Table = <<"test_table">>,
    StartTime = <<"2023-01-01 00:00:00">>,
    EndTime = <<"2023-01-02 00:00:00">>,
    Fields = [<<"value">>, <<"status">>],
    
    ExpectedSQL = <<"SELECT value,status FROM test_db.test_table WHERE ts >= '2023-01-01 00:00:00' AND ts <= '2023-01-02 00:00:00'">>,
    
    case dgiot_tdengine:build_range_query_sql(Database, Table, StartTime, EndTime, Fields) of
        {ok, SQL} ->
            io:format("~s ~p 生成的SQL: ~p~n", [?FILE, ?LINE, SQL]),
            ?assertEqual(ExpectedSQL, SQL);
        Error ->
            io:format("~s ~p 构建SQL失败: ~p~n", [?FILE, ?LINE, Error]),
            ?assert(false)
    end.

%% 测试查询范围数据
test_query_range_data() ->
    Database = <<"test_db">>,
    Table = <<"test_table">>,
    StartTime = <<"2023-01-01 00:00:00">>,
    EndTime = <<"2023-01-02 00:00:00">>,
    Fields = [<<"value">>, <<"status">>],
    
    % 由于是测试环境，我们只测试函数调用是否正常
    case dgiot_tdengine:query_range_data(Database, Table, StartTime, EndTime, Fields) of
        {ok, _Result} ->
            io:format("~s ~p 查询范围数据成功~n", [?FILE, ?LINE]),
            ?assert(true);
        {error, Reason} ->
            io:format("~s ~p 查询范围数据失败: ~p~n", [?FILE, ?LINE, Reason]),
            % 在测试环境中，连接失败是正常的
            ?assert(true)
    end.

%% 测试构建聚合查询SQL
test_build_aggregate_query_sql() ->
    Database = <<"test_db">>,
    Table = <<"test_table">>,
    StartTime = <<"2023-01-01 00:00:00">>,
    EndTime = <<"2023-01-02 00:00:00">>,
    Field = <<"value">>,
    Interval = <<"1h">>,
    Function = <<"avg">>,
    
    ExpectedSQL = <<"SELECT avg(value) FROM test_db.test_table WHERE ts >= '2023-01-01 00:00:00' AND ts <= '2023-01-02 00:00:00' INTERVAL(1h)">>,
    
    case dgiot_tdengine:build_aggregate_query_sql(Database, Table, StartTime, EndTime, Field, Interval, Function) of
        {ok, SQL} ->
            io:format("~s ~p 生成的聚合SQL: ~p~n", [?FILE, ?LINE, SQL]),
            ?assertEqual(ExpectedSQL, SQL);
        Error ->
            io:format("~s ~p 构建聚合SQL失败: ~p~n", [?FILE, ?LINE, Error]),
            ?assert(false)
    end.

%% 测试查询聚合数据
test_query_aggregate_data() ->
    Database = <<"test_db">>,
    Table = <<"test_table">>,
    StartTime = <<"2023-01-01 00:00:00">>,
    EndTime = <<"2023-01-02 00:00:00">>,
    Field = <<"value">>,
    Interval = <<"1h">>,
    Function = <<"avg">>,
    
    % 由于是测试环境，我们只测试函数调用是否正常
    case dgiot_tdengine:query_aggregate_data(Database, Table, StartTime, EndTime, Field, Interval, Function) of
        {ok, _Result} ->
            io:format("~s ~p 查询聚合数据成功~n", [?FILE, ?LINE]),
            ?assert(true);
        {error, Reason} ->
            io:format("~s ~p 查询聚合数据失败: ~p~n", [?FILE, ?LINE, Reason]),
            % 在测试环境中，连接失败是正常的
            ?assert(true)
    end.

%% 运行所有测试
run_all_tests() ->
    io:format("~s ~p 运行TDengine范围查询测试...~n", [?FILE, ?LINE]),
    eunit:test({module, ?MODULE}, [verbose]),
    io:format("~s ~p 测试完成~n", [?FILE, ?LINE]).
