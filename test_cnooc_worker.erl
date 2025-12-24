-module(test_cnooc_worker).
-export([run/0]).

run() ->
    io:format("Testing dgiot_cnooc_worker:update_td_tags/1~n"),
    
    % 模拟设备数据
    TestDevice = #{
        <<"objectId">> => <<"test_device_123">>,
        <<"product">> => #{<<"objectId">> => <<"test_product_456">>},
        <<"content">> => #{
            <<"relationList">> => [
                #{<<"id">> => 3, <<"type">> => <<"area3">>, <<"value">> => <<"active_area">>, <<"cname">> => <<"活动区域">>},
                #{<<"id">> => 2, <<"type">> => <<"area2">>, <<"value">> => <<"active_area">>, <<"cname">> => <<"活动区域">>},
                #{<<"id">> => 1, <<"type">> => <<"area1">>, <<"value">> => <<"B04">>, <<"cname">> => <<"B04区域">>},
                #{<<"id">> => 0, <<"type">> => <<"algorithm">>, <<"value">> => <<"fall_down">>}
            ]
        }
    },
    
    io:format("Test device: ~p~n", [TestDevice]),
    
    % 测试extract_areas函数
    io:format("~n1. Testing extract_areas function:~n"),
    Areas = dgiot_cnooc_tools:extract_areas(TestDevice),
    io:format("Extracted areas: ~p~n", [Areas]),
    
    % 测试process_areas函数
    io:format("~n2. Testing process_areas function:~n"),
    ProcessedAreas = process_areas(Areas),
    io:format("Processed areas: ~p~n", [ProcessedAreas]),
    
    % 测试构建SQL
    io:format("~n3. Testing SQL building:~n"),
    SetClauses = build_set_tag_clauses(ProcessedAreas),
    io:format("SET TAG clauses: ~s~n", [SetClauses]),
    
    DB = <<"_testdb">>,
    TableName = <<"_test_device_123">>,
    Sql = <<"ALTER TABLE ", DB/binary, TableName/binary, " SET TAG ", SetClauses/binary, ";">>,
    io:format("Full SQL: ~s~n", [Sql]),
    
    io:format("~n4. Testing update_table_tags function (mock):~n"),
    io:format("Would call: dgiot_cnooc_tools:update_table_tags(~p, ~p, ~p)~n", 
              [<<"test_product_456">>, <<"test_device_123">>, ProcessedAreas]),
    
    io:format("~nAll tests completed!~n"),
    ok.

% 复制process_areas函数用于测试
process_areas(Areas) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Value of
            #{<<"objectId">> := ObjectId} ->
                Acc#{Key => ObjectId};
            _ when is_binary(Value) ->
                Acc#{Key => Value};
            _ ->
                Acc
        end
    end, #{}, Areas).

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
    build_set_tag_clauses(Rest, NewAcc);
build_set_tag_clauses([{<<"area2">>, Value} | Rest], Acc) when Value =/= undefined, Value =/= <<>> ->
    NewAcc = case Acc of
        <<>> -> <<"area2='", Value/binary, "'">>;
        _ -> <<Acc/binary, ", area2='", Value/binary, "'">>
    end,
    build_set_tag_clauses(Rest, NewAcc);
build_set_tag_clauses([{<<"area3">>, Value} | Rest], Acc) when Value =/= undefined, Value =/= <<>> ->
    NewAcc = case Acc of
        <<>> -> <<"area3='", Value/binary, "'">>;
        _ -> <<Acc/binary, ", area3='", Value/binary, "'">>
    end,
    build_set_tag_clauses(Rest, NewAcc);
build_set_tag_clauses([_ | Rest], Acc) ->
    build_set_tag_clauses(Rest, Acc).
