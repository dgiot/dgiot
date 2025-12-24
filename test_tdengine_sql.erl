-module(test_tdengine_sql).
-export([run/0]).

run() ->
    io:format("Testing TDengine SQL generation~n"),
    
    % 测试数据
    Channel = <<"24b9b4bc50">>,
    ProductId = <<"1654e224d5">>,
    DeviceId = <<"406ca90ccd">>,
    Tags = #{
        <<"area1">> => <<"B04">>,
        <<"area2">> => <<"B04维保区">>,
        <<"area3">> => <<"随钻电子设备维保区">>
    },
    
    io:format("Input parameters:~n"),
    io:format("  Channel: ~p~n", [Channel]),
    io:format("  ProductId: ~p~n", [ProductId]),
    io:format("  DeviceId: ~p~n", [DeviceId]),
    io:format("  Tags: ~p~n", [Tags]),
    
    % 构建数据库名
    DB = iolist_to_binary(["_", Channel]),
    io:format("~n1. Database name: ~p~n", [DB]),
    
    % 构建表名
    TableName = iolist_to_binary(["_", ProductId, "._", DeviceId]),
    io:format("2. Table name: ~p~n", [TableName]),
    
    % 构建SET TAG子句
    SetClauses = build_set_tag_clauses(Tags),
    io:format("3. SET TAG clauses: ~ts~n", [SetClauses]),
    
    % 构建完整SQL
    Sql = <<"ALTER TABLE ", DB/binary, ".", TableName/binary, " SET TAG ", SetClauses/binary, ";">>,
    io:format("~n4. Full SQL: ~ts~n", [Sql]),
    
    % 验证SQL是否与预期匹配
    ExpectedSql = <<"ALTER TABLE _24b9b4bc50._1654e224d5._406ca90ccd SET TAG area1='B04', area2='B04维保区', area3='随钻电子设备维保区';">>,
    io:format("~n5. Expected SQL: ~ts~n", [ExpectedSql]),
    
    case Sql =:= ExpectedSql of
        true ->
            io:format("✅ SQL generation test PASSED!~n");
        false ->
            io:format("❌ SQL generation test FAILED!~n"),
            io:format("   Generated: ~ts~n", [Sql]),
            io:format("   Expected:  ~ts~n", [ExpectedSql])
    end,
    
    % 测试边界情况：缺少某些area
    io:format("~n6. Testing partial tags:~n"),
    PartialTags = #{
        <<"area1">> => <<"B04">>,
        <<"area3">> => <<"随钻电子设备维保区">>
    },
    PartialSetClauses = build_set_tag_clauses(PartialTags),
    io:format("   Partial tags: ~p~n", [PartialTags]),
    io:format("   SET TAG clauses: ~ts~n", [PartialSetClauses]),
    
    io:format("~nAll tests completed!~n"),
    ok.

% 复制build_set_tag_clauses函数用于测试
build_set_tag_clauses(Tags) ->
    build_set_tag_clauses(maps:to_list(Tags), <<>>).

build_set_tag_clauses([], Acc) ->
    Acc;
build_set_tag_clauses([{<<"area1">>, Value} | Rest], Acc) when Value =/= undefined, Value =/= <<>> ->
    NewAcc = case Acc of
        <<>> -> <<"area1='", Value/binary, "'">>;
        _ -> <<Acc/binary, ", area1='", Value/binary, "'">>
    end,
