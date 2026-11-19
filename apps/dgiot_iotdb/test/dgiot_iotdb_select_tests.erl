-module(dgiot_iotdb_select_tests).

-include_lib("eunit/include/eunit.hrl").

database_segment_test() ->
    ?assertEqual(<<"root.dgiot.dbx412f42">>, dgiot_iotdb_select:database(<<"root.dgiot">>, <<"A/B">>)),
    ?assertEqual(<<"root.dgiot.dbx412f42.px5031.dx4431">>,
        dgiot_iotdb_select:device_path(<<"root.dgiot.dbx412f42">>, <<"P1">>, <<"D1">>)).

unsafe_root_falls_back_test() ->
    ?assertEqual(<<"root.dgiot.dbx7031">>, dgiot_iotdb_select:database(<<"root.dgiot; DROP DATABASE root.x">>, <<"p1">>)).

iotdb_2_0_ddl_syntax_test() ->
    DatabaseSql = dgiot_iotdb_select:create_database(<<"root.dgiot.test">>),
    TimeseriesSql = dgiot_iotdb_select:create_timeseries(
        <<"root.dgiot.test.px31.dx31">>, <<"temp">>, <<"DOUBLE">>),
    ?assertEqual(<<"CREATE DATABASE root.dgiot.test">>, DatabaseSql),
    ?assertEqual(nomatch, binary:match(DatabaseSql, <<"IF NOT EXISTS">>)),
    ?assertEqual(nomatch, binary:match(TimeseriesSql, <<"IF NOT EXISTS">>)).

measurement_roundtrip_test() ->
    Encoded = dgiot_iotdb_field:measurement(<<"temperature.main">>),
    ?assertEqual(<<"temperature.main">>, dgiot_iotdb_field:decode_measurement(Encoded)).

where_values_are_escaped_test() ->
    ?assertEqual(<<"'a''b'">>, dgiot_iotdb_select:quote_literal(<<"a'b">>)),
    ?assertEqual(<<"WHERE m74656d70 > 10">>,
        dgiot_iotdb_select:format_where(#{<<"where">> => #{<<"temp">> => #{<<"$gt">> => 10}}})).

time_range_is_milliseconds_test() ->
    ?assertEqual(<<"WHERE time >= 1700000000000 AND time <= 1700000001000">>,
        dgiot_iotdb_select:format_where(#{<<"starttime">> => 1700000000000,
            <<"endtime">> => 1700000001000})).

interval_and_limit_are_bounded_test() ->
    Sql = dgiot_iotdb_select:select(<<"root.dgiot.dbx70.px31.dx31">>, <<"p1">>,
        #{<<"keys">> => [<<"temperature">>], <<"interval">> => <<"1m) ; DROP DATABASE root.x --">>,
            <<"limit">> => 1000000, <<"skip">> => 100001}),
    ?assertEqual(nomatch, binary:match(Sql, <<"DROP DATABASE">>)),
    ?assertEqual(nomatch, binary:match(Sql, <<"GROUP BY">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"LIMIT 100000">>)),
    ?assertEqual(nomatch, binary:match(Sql, <<"OFFSET">>)).

relative_interval_is_accepted_test() ->
    Sql = dgiot_iotdb_select:select(<<"root.dgiot.dbx70.px31.dx31">>, <<"p1">>,
        #{<<"interval">> => <<"15m">>, <<"starttime">> => 1000000, <<"endtime">> => 2000000}),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"GROUP BY ([1000000000,2000000000), 15m)">>)).

stddev_function_is_preserved_test() ->
    Sql = dgiot_iotdb_select:select(<<"root.dgiot.dbx70.px31.dx31">>, <<"p1">>,
        #{<<"keys">> => [<<"temperature">>], <<"function">> => <<"stddev">>}),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"stddev(m74656d7065726174757265) AS m74656d7065726174757265">>)).

last_row_wildcard_selects_latest_row_test() ->
    Sql = dgiot_iotdb_select:select(<<"root.dgiot.dbx70.px31.dx31">>, <<"p1">>,
        #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1, <<"order">> => <<"-createdat">>}),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"SELECT * FROM">>)),
    ?assertEqual(nomatch, binary:match(Sql, <<"last_row">>)).

time_where_relative_value_is_numeric_test() ->
    Sql = dgiot_iotdb_select:select(<<"root.dgiot.dbx70.px31.dx31">>, <<"p1">>,
        #{<<"where">> => #{<<"createdat">> => #{<<"$gte">> => <<"now - 10d">>}}}),
    ?assertEqual(nomatch, binary:match(Sql, <<"'now - 10d'">>)),
    ?assertNotEqual(nomatch, binary:match(Sql, <<"WHERE time >= ">>)).

custom_root_database_path_is_extracted_test() ->
    Database = dgiot_iotdb_select:database(<<"root.platform">>, <<"p1">>),
    Device = dgiot_iotdb_select:device_path(Database, <<"p1">>, <<"d1">>),
    ?assertEqual({ok, Database}, dgiot_iotdb_select:database_from_device(Device)).

timestamp_seconds_are_normalized_test() ->
    ?assertEqual(1700000000000, dgiot_iotdb_field:timestamp_ms(1700000000)).

relative_timestamp_is_resolved_test() ->
    Before = dgiot_datetime:now_ms(),
    Timestamp = dgiot_iotdb_field:timestamp_ms(<<"now - 10d">>),
    After = dgiot_datetime:now_ms(),
    ?assert(Timestamp >= Before - 864000000),
    ?assert(Timestamp =< After - 864000000).
