-module(dgiot_iotdb_select).
-include("dgiot_iotdb.hrl").

-export([database/2, device_path/3, create_database/1, create_timeseries/3,
    select/3, format_where/1, quote_literal/1, format_limit/1, valid_path/1,
    database_from_device/1]).

database(Root, Id) ->
    SafeRoot = normalize_root(Root),
    <<SafeRoot/binary, ".db", (dgiot_iotdb_field:encode_segment(Id))/binary>>.

device_path(Database, ProductId, DeviceId) ->
    <<Database/binary, ".p", (dgiot_iotdb_field:encode_segment(ProductId))/binary,
        ".d", (dgiot_iotdb_field:encode_segment(DeviceId))/binary>>.

create_database(Database) ->
    <<"CREATE DATABASE ", Database/binary>>.

create_timeseries(Device, Field, Type) ->
    Measurement = dgiot_iotdb_field:measurement(Field),
    Encoding = encoding(Type),
    <<"CREATE TIMESERIES ", Device/binary, ".", Measurement/binary,
        " WITH DATATYPE=", Type/binary, ", ENCODING=", Encoding/binary, ", COMPRESSOR=LZ4">>.

select(Device, ProductId, Query) ->
    case valid_path(Device) of
        false -> <<>>;
        true -> select_path(Device, ProductId, Query)
    end.

select_path(Device, ProductId, Query) ->
    Keys = format_keys(maps:get(<<"keys">>, Query, <<"*">>), ProductId, Query),
    Where = format_where(Query),
    Group = format_group(Query),
    Order = format_order(Query),
    Limit = format_limit(Query),
    Offset = format_offset(Query),
    Clauses = [<<"SELECT ", Keys/binary, " FROM ", Device/binary>>, Where, Group, Order, Limit, Offset],
    Sql = dgiot_utils:join(" ", [Part || Part <- Clauses, Part =/= <<>>]),
    dgiot_utils:to_binary(Sql).

format_keys(<<"last_row(*)">>, _ProductId, _Query) -> <<"*">>;
format_keys(<<"*">>, ProductId, Query) ->
    case aggregate_requested(Query) of
        true -> format_key_list(dgiot_product:get_keys(ProductId), Query);
        false -> <<"*">>
    end;
format_keys(<<>>, ProductId, Query) -> format_keys(<<"*">>, ProductId, Query);
format_keys(Keys, _ProductId, Query) when is_list(Keys) ->
    format_key_list(Keys, Query);
format_keys(Keys, _ProductId, Query) when is_binary(Keys) ->
    format_key_list(re:split(Keys, <<",">>, [trim]), Query).

aggregate_requested(Query) ->
    Functions = maps:get(<<"functions">>, Query, #{}),
    maps:get(<<"function">>, Query, <<>>) =/= <<>> orelse
        maps:get(<<"interval">>, Query, undefined) =/= undefined orelse
        (is_map(Functions) andalso map_size(Functions) > 0).

format_key_list([], _Query) -> <<"*">>;
format_key_list(Keys, Query) ->
    list_to_binary(lists:join(",", [format_key(Key, Query) || Key <- Keys, Key =/= <<>>])).

format_key(<<"createdat">>, _Query) -> <<"Time">>;
format_key(<<"time">>, _Query) -> <<"Time">>;
format_key(Key, Query) ->
    Measurement = dgiot_iotdb_field:measurement(Key),
    Function = function_for(Key, Query),
    case normalize_function(Function) of
        <<>> -> Measurement;
        SafeFunction -> <<SafeFunction/binary, "(", Measurement/binary, ") AS ", Measurement/binary>>
    end.

function_for(Key, Query) ->
    Functions = maps:get(<<"functions">>, Query, #{}),
    case maps:find(Key, Functions) of
        {ok, Function} -> Function;
        error ->
            case maps:get(<<"function">>, Query, <<>>) of
                <<>> ->
                    case maps:get(<<"interval">>, Query, undefined) of
                        undefined -> <<>>;
                        _ -> <<"last">>
                    end;
                Function -> Function
            end
    end.

normalize_function(<<"first">>) -> <<"first_value">>;
normalize_function(<<"last">>) -> <<"last_value">>;
normalize_function(<<"avg">>) -> <<"avg">>;
normalize_function(<<"count">>) -> <<"count">>;
normalize_function(<<"sum">>) -> <<"sum">>;
normalize_function(<<"stddev">>) -> <<"stddev">>;
normalize_function(<<"max">>) -> <<"max_value">>;
normalize_function(<<"min">>) -> <<"min_value">>;
normalize_function(<<"first_value">>) -> <<"first_value">>;
normalize_function(<<"last_value">>) -> <<"last_value">>;
normalize_function(<<"max_value">>) -> <<"max_value">>;
normalize_function(<<"min_value">>) -> <<"min_value">>;
normalize_function(_) -> <<>>.

format_where(Query) ->
    TimeFilters = format_time_range(Query),
    MapFilters = format_map_where(maps:get(<<"where">>, Query, #{})),
    Filters = TimeFilters ++ MapFilters,
    case Filters of
        [] -> <<>>;
        _ -> <<"WHERE ", (list_to_binary(lists:join(" AND ", Filters)))/binary>>
    end.

format_time_range(Query) ->
    Start = maps:get(<<"starttime">>, Query, undefined),
    End = maps:get(<<"endtime">>, Query, undefined),
    Start1 = case Start of undefined -> []; _ -> [<<"time >= ", (integer_to_binary(dgiot_iotdb_field:timestamp_ms(Start)))/binary>>] end,
    case End of undefined -> Start1; _ -> Start1 ++ [<<"time <= ", (integer_to_binary(dgiot_iotdb_field:timestamp_ms(End)))/binary>>] end.

format_map_where(Where) when is_binary(Where) ->
    case jsx:is_json(Where) of
        true -> format_map_where(dgiot_json:decode(Where, [return_maps]));
        false -> []
    end;
format_map_where(Where) when is_map(Where) ->
    lists:filtermap(fun({Key, Value}) -> format_predicate(Key, Value) end, maps:to_list(Where));
format_map_where(_) -> [].

format_predicate(<<"createdat">>, Operators) when is_map(Operators) ->
    format_operator(<<"time">>, Operators);
format_predicate(<<"time">>, Operators) when is_map(Operators) ->
    format_operator(<<"time">>, Operators);
format_predicate(<<"createdat">>, Value) ->
    {true, <<"time = ", (operator_value(<<"time">>, Value))/binary>>};
format_predicate(<<"time">>, Value) ->
    {true, <<"time = ", (operator_value(<<"time">>, Value))/binary>>};
format_predicate(Key, Operators) when is_map(Operators) ->
    format_operator(dgiot_iotdb_field:measurement(Key), Operators);
format_predicate(Key, Value) ->
    {true, <<(dgiot_iotdb_field:measurement(Key))/binary, " = ", (sql_value(Value))/binary>>}.

format_operator(Field, Operators) ->
    Ops = [{<<"$gt">>, <<" > ">>}, {<<"$gte">>, <<" >= ">>}, {<<"$lt">>, <<" < ">>},
        {<<"$lte">>, <<" <= ">>}, {<<"$ne">>, <<" <> ">>}, {<<"$eq">>, <<" = ">>}],
    case [{Op, maps:get(Key, Operators)} || {Key, Op} <- Ops, maps:is_key(Key, Operators)] of
        [] -> false;
        Matches ->
            Predicates = [<<Field/binary, Op/binary, (operator_value(Field, Value))/binary>> || {Op, Value} <- Matches],
            {true, list_to_binary(lists:join(" AND ", Predicates))}
    end.

operator_value(<<"time">>, Value) ->
    dgiot_utils:to_binary(dgiot_iotdb_field:timestamp_ms(Value));
operator_value(_Field, Value) ->
    sql_value(Value).

sql_value(Value) when is_integer(Value); is_float(Value) -> dgiot_utils:to_binary(Value);
sql_value(true) -> <<"true">>;
sql_value(false) -> <<"false">>;
sql_value(null) -> <<"NULL">>;
sql_value(Value) -> quote_literal(dgiot_utils:to_binary(Value)).

quote_literal(Value) ->
    Escaped = binary:replace(Value, <<"'">>, <<"''">>, [global]),
    <<"'", Escaped/binary, "'">>.

format_group(Query) ->
    case maps:get(<<"interval">>, Query, undefined) of
        Interval when is_binary(Interval), Interval =/= <<>> ->
            case format_interval(Interval) of
                {ok, SafeInterval} ->
                    group_clause(Query, SafeInterval);
                error ->
                    <<>>
            end;
        Interval when is_integer(Interval), Interval > 0, Interval =< 31536000000 ->
            {Start, End} = group_bounds(Query),
            <<"GROUP BY ([", (integer_to_binary(Start))/binary, ",", (integer_to_binary(End))/binary,
                "), ", (integer_to_binary(Interval))/binary, "ms)">>;
        _ -> <<>>
    end.

group_clause(Query, Interval) ->
    {Start, End} = group_bounds(Query),
    <<"GROUP BY ([", (integer_to_binary(Start))/binary, ",", (integer_to_binary(End))/binary,
        "), ", Interval/binary, ")">>.

group_bounds(Query) ->
    Now = dgiot_datetime:now_ms(),
    Start = maps:get(<<"starttime">>, Query, Now - 25920000000),
    End = maps:get(<<"endtime">>, Query, Now),
    {dgiot_iotdb_field:timestamp_ms(Start), dgiot_iotdb_field:timestamp_ms(End)}.

format_interval(Interval) ->
    case re:run(Interval, <<"^([1-9][0-9]{0,9})(ms|s|m|h|d|w)$">>, [{capture, [1, 2], binary}]) of
        {match, [Amount, Unit]} ->
            {ok, <<Amount/binary, Unit/binary>>};
        nomatch ->
            error
    end.

format_order(#{<<"order">> := <<"-createdat">>}) -> <<"ORDER BY TIME DESC">>;
format_order(#{<<"order">> := <<"createdat">>}) -> <<"ORDER BY TIME ASC">>;
format_order(#{<<"order">> := <<"-time">>}) -> <<"ORDER BY TIME DESC">>;
format_order(#{<<"order">> := <<"time">>}) -> <<"ORDER BY TIME ASC">>;
format_order(_) -> <<>>.

format_limit(Query) ->
    case maps:get(<<"limit">>, Query, 5000) of
        Limit when is_integer(Limit), Limit > 0 -> <<"LIMIT ", (integer_to_binary(min(Limit, 100000)))/binary>>;
        _ -> <<"LIMIT 5000">>
    end.

format_offset(Query) ->
    case maps:get(<<"skip">>, Query, undefined) of
        Offset when is_integer(Offset), Offset > 0, Offset =< 100000 -> <<"OFFSET ", (integer_to_binary(Offset))/binary>>;
        _ -> <<>>
    end.

normalize_root(Root) ->
    case re:run(Root, <<"^root(?:\\.[A-Za-z_][A-Za-z0-9_]*)*$">>, [{capture, none}]) of
        match -> Root;
        nomatch -> <<"root.dgiot">>
    end.

valid_path(Path) when is_binary(Path) ->
    re:run(Path, <<"^root(?:\\.[A-Za-z_][A-Za-z0-9_]*)+(?:\\.\\*\\*)?$">>, [{capture, none}]) =:= match;
valid_path(_) ->
    false.

database_from_device(Device) when is_binary(Device) ->
    case re:run(Device, <<"^(.*\\.dbx[0-9a-f]*)\\.px[0-9a-f]*\\.dx[0-9a-f]*$">>,
            [{capture, [1], binary}]) of
        {match, [Database]} -> {ok, Database};
        nomatch -> error
    end;
database_from_device(_) ->
    error.

encoding(<<"BOOLEAN">>) -> <<"PLAIN">>;
encoding(<<"INT32">>) -> <<"RLE">>;
encoding(<<"INT64">>) -> <<"TS_2DIFF">>;
encoding(<<"FLOAT">>) -> <<"GORILLA">>;
encoding(<<"DOUBLE">>) -> <<"GORILLA">>;
encoding(_) -> <<"PLAIN">>.
