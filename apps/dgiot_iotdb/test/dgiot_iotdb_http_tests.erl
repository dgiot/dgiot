-module(dgiot_iotdb_http_tests).

-include_lib("eunit/include/eunit.hrl").

query_rows_keep_separate_timestamps_test() ->
    Result = dgiot_iotdb_http:normalize_query(#{
        <<"columns">> => [<<"root.dgiot.dbx70.px31.dx31.m74656d70">>],
        <<"dataType">> => [<<"DOUBLE">>],
        <<"timestamps">> => [1700000000000],
        <<"values">> => [[21.5]]
    }),
    ?assertEqual([#{<<"createdat">> => 1700000000000, <<"temp">> => 21.5}],
        maps:get(<<"results">>, Result)).

query_rows_map_time_column_test() ->
    Result = dgiot_iotdb_http:normalize_query(#{
        <<"columns">> => [<<"Time">>, <<"root.dgiot.dbx70.px31.dx31.m74656d70">>],
        <<"dataType">> => [<<"TIMESTAMP">>, <<"DOUBLE">>],
        <<"timestamps">> => [1700000000000],
        <<"values">> => [[21.5]]
    }),
    ?assertEqual([#{<<"createdat">> => 1700000000000, <<"temp">> => 21.5}],
        maps:get(<<"results">>, Result)).

iotdb_2_0_columnar_query_response_test() ->
    Result = dgiot_iotdb_http:normalize_query(#{
        <<"expressions">> => [
            <<"root.dgiot.dbx70.px31.dx31.m74656d70">>,
            <<"root.dgiot.dbx70.px31.dx31.m68756d6964697479">>
        ],
        <<"column_names">> => [],
        <<"data_types">> => [<<"DOUBLE">>, <<"INT32">>],
        <<"timestamps">> => [1700000000000, 1700000060000],
        <<"values">> => [[21.5, 22.5], [40, 41]]
    }),
    ?assertEqual([
        #{<<"createdat">> => 1700000000000, <<"temp">> => 21.5, <<"humidity">> => 40},
        #{<<"createdat">> => 1700000060000, <<"temp">> => 22.5, <<"humidity">> => 41}
    ], maps:get(<<"results">>, Result)),
    ?assertEqual(2, maps:get(<<"rows">>, Result)).

query_alias_and_aggregate_column_test() ->
    ?assertEqual(<<"temp">>, dgiot_iotdb_field:decode_measurement(<<"last_value(m74656d70) AS m74656d70">>)).

iotdb_2_0_duplicate_database_is_idempotent_test() ->
    ?assert(dgiot_iotdb_http:is_duplicate_ddl_error(#{
        <<"code">> => 501,
        <<"message">> => <<"root.dgiot.test has already been created as database">>
    })),
    ?assertNot(dgiot_iotdb_http:is_duplicate_ddl_error(#{
        <<"code">> => 501,
        <<"message">> => <<"database permission denied">>
    })).

iotdb_2_0_duplicate_timeseries_is_idempotent_test() ->
    ?assert(dgiot_iotdb_http:is_duplicate_ddl_error(#{
        <<"code">> => 503,
        <<"message">> => <<"Path [root.dgiot.test.mtemp] already exist">>
    })),
    ?assertNot(dgiot_iotdb_http:is_duplicate_ddl_error(#{
        <<"code">> => 503,
        <<"message">> => <<"some unrelated failure">>
    })).
