-module(dgiot_iotdb_http).
-include("dgiot_iotdb.hrl").

-export([query/2, non_query/2, insert_records/2, request/3, normalize_query/1,
    is_duplicate_ddl_error/1]).

-define(DEFAULT_TIMEOUT, 30000).

query(Context, Sql) ->
    case request(Context, <<"query">>, #{<<"sql">> => Sql}) of
        {ok, Body} ->
            case response_code(Body) of
                success -> {ok, normalize_query(Body)};
                {error, Reason} -> {error, Reason}
            end;
        Error ->
            Error
    end.

non_query(Context, Sql) ->
    case request(Context, <<"nonQuery">>, #{<<"sql">> => Sql}) of
        {ok, Body} ->
            case response_code(Body) of
                success -> {ok, Body};
                {error, Reason} ->
                    case is_duplicate_ddl_error(Reason) of
                        true -> {ok, Reason};
                        false -> {error, Reason}
                    end
            end;
        Error ->
            Error
    end.

insert_records(Context, Payload) ->
    case request(Context, <<"insertRecords">>, Payload) of
        {ok, Body} ->
            case response_code(Body) of
                success -> {ok, Body};
                {error, Reason} -> {error, Reason}
            end;
        Error ->
            Error
    end.

request(#{<<"url">> := BaseUrl, <<"username">> := User, <<"password">> := Password} = Context, Endpoint, Payload) ->
    Url = <<(strip_trailing_slash(BaseUrl))/binary, "/rest/v2/", Endpoint/binary>>,
    Authorization = binary_to_list(<<"Basic ", (base64:encode(<<User/binary, ":", Password/binary>>))/binary>>),
    Headers = [{"Authorization", Authorization}, {"Accept", "application/json"}],
    Timeout = maps:get(<<"timeout">>, Context, ?DEFAULT_TIMEOUT),
    Options = [{timeout, Timeout}, {connect_timeout, Timeout}],
    RequestOptions = [{body_format, binary}],
    try
        Body = jsx:encode(Payload),
        case httpc:request(post, {binary_to_list(Url), Headers, "application/json", Body}, Options, RequestOptions) of
            {ok, {{_Version, Status, _Reason}, _ResponseHeaders, ResponseBody}} ->
                decode_response(Status, ResponseBody);
            {error, Reason} ->
                {error, #{<<"reason">> => dgiot_utils:to_binary(Reason)}}
        end
    catch
        Class:ExceptionReason ->
            {error, #{<<"reason">> => dgiot_utils:to_binary({Class, ExceptionReason})}}
    end;
request(_Context, _Endpoint, _Payload) ->
    {error, #{<<"reason">> => <<"invalid_iotdb_context">>}}.

decode_response(Status, Body) when Status >= 200, Status < 300 ->
    try jsx:decode(Body, [{labels, binary}, return_maps]) of
        Decoded when is_map(Decoded) -> {ok, Decoded};
        _ -> {error, #{<<"status">> => Status, <<"body">> => Body}}
    catch
        _:_ -> {error, #{<<"status">> => Status, <<"body">> => Body}}
    end;
decode_response(Status, Body) ->
    {error, #{<<"status">> => Status, <<"body">> => Body}}.

response_code(#{<<"code">> := Code}) when Code =:= 200; Code =:= 0 ->
    success;
response_code(#{<<"code">> := _} = Body) ->
    {error, Body};
response_code(Body) ->
    case maps:is_key(<<"columns">>, Body) orelse maps:is_key(<<"column_names">>, Body) orelse maps:is_key(<<"values">>, Body) of
        true -> success;
        false -> {error, Body}
    end.

is_duplicate_ddl_error(#{<<"code">> := 501, <<"message">> := Message}) when is_binary(Message) ->
    binary:match(Message, <<"has already been created as database">>) =/= nomatch;
is_duplicate_ddl_error(#{<<"code">> := 503, <<"message">> := Message}) when is_binary(Message) ->
    binary:match(Message, <<"already exist">>) =/= nomatch;
is_duplicate_ddl_error(_) ->
    false.

normalize_query(#{<<"columns">> := Columns} = Body) ->
    normalize_query(Body, Columns);
normalize_query(#{<<"expressions">> := Expressions, <<"timestamps">> := Timestamps,
        <<"values">> := ValueColumns} = Body)
        when is_list(Expressions), is_list(Timestamps), is_list(ValueColumns) ->
    normalize_columnar_query(Body, Expressions, Timestamps, ValueColumns);
normalize_query(#{<<"column_names">> := Columns} = Body) ->
    normalize_query(Body, Columns);
normalize_query(Body) ->
    Body#{<<"results">> => maps:get(<<"results">>, Body, [])}.

normalize_columnar_query(Body, Expressions, Timestamps, ValueColumns) ->
    Types = maps:get(<<"data_types">>, Body, maps:get(<<"dataType">>, Body, [])),
    Results = [columnar_row(Index, Timestamp, Expressions, ValueColumns)
        || {Timestamp, Index} <- lists:zip(Timestamps, lists:seq(1, length(Timestamps)))],
    #{<<"results">> => Results,
        <<"column_meta">> => lists:zipwith(fun(Expression, Type) -> [Expression, Type] end,
            Expressions, pad_types(Types, length(Expressions))),
        <<"rows">> => length(Results),
        <<"code">> => maps:get(<<"code">>, Body, 200)}.

columnar_row(Index, Timestamp, Expressions, ValueColumns) ->
    lists:foldl(fun({Expression, Values}, Acc) ->
        case columnar_value(Index, Values) of
            {ok, Value} -> Acc#{dgiot_iotdb_field:decode_measurement(Expression) => Value};
            error -> Acc
        end
    end, #{<<"createdat">> => Timestamp}, lists:zip(Expressions, ValueColumns)).

columnar_value(Index, Values) when is_list(Values), length(Values) >= Index ->
    {ok, lists:nth(Index, Values)};
columnar_value(_Index, _Values) ->
    error.

normalize_query(Body, Columns) ->
    Rows = maps:get(<<"values">>, Body, []),
    Timestamps = maps:get(<<"timestamps">>, Body, []),
    Types = maps:get(<<"dataType">>, Body, maps:get(<<"data_types">>, Body, [])),
    Results = rows_to_maps(Columns, Types, Rows, Timestamps),
    #{<<"results">> => Results,
        <<"column_meta">> => lists:zipwith(fun(Column, Type) -> [Column, Type] end, Columns, pad_types(Types, length(Columns))),
        <<"rows">> => length(Results),
        <<"code">> => maps:get(<<"code">>, Body, 200)}.

rows_to_maps(Columns, Types, Rows, Timestamps) ->
    HasTimeColumn = lists:any(fun is_time_column/1, Columns),
    case HasTimeColumn of
        true ->
            [row_with_time(Columns, Types, Row, timestamp_at(Index, Timestamps)) || {Row, Index} <- lists:zip(Rows, lists:seq(1, length(Rows)))];
        false ->
            [row_to_map(Columns, Types, Row, timestamp_at(Index, Timestamps)) || {Row, Index} <- lists:zip(Rows, lists:seq(1, length(Rows)))]
    end.

row_with_time(Columns, Types, Row, Timestamp) when length(Row) < length(Columns), Timestamp =/= undefined ->
    row_to_map(Columns, Types, align_values(Columns, Row, Timestamp), Timestamp);
row_with_time(Columns, Types, Row, Timestamp) when Timestamp =/= undefined ->
    row_to_map(Columns, Types, replace_time_value(Columns, Row, Timestamp), Timestamp);
row_with_time(Columns, Types, Row, Timestamp) ->
    row_to_map(Columns, Types, Row, Timestamp).

align_values(Columns, Row, Timestamp) ->
    align_values(Columns, Row, Timestamp, []).

align_values([], _Row, _Timestamp, Acc) -> lists:reverse(Acc);
align_values([Column | Columns], Row, Timestamp, Acc) ->
    case is_time_column(Column) of
        true -> align_values(Columns, Row, Timestamp, [Timestamp | Acc]);
        false ->
            case Row of
                [Value | Rest] -> align_values(Columns, Rest, Timestamp, [Value | Acc]);
                [] -> align_values(Columns, [], Timestamp, [null | Acc])
            end
    end.

replace_time_value(Columns, Row, Timestamp) ->
    [case is_time_column(Column) of true -> Timestamp; false -> Value end
        || {Column, Value} <- lists:zip(Columns, Row)] .

row_to_map(Columns, Types, Row, Timestamp) ->
    Values = case length(Row) > length(Columns) of
        true -> lists:nthtail(length(Row) - length(Columns), Row);
        false -> Row
    end,
    Base = lists:foldl(fun({Column, Value}, Acc) ->
        case is_time_column(Column) of
            true -> Acc#{<<"createdat">> => Value};
            false -> Acc#{dgiot_iotdb_field:decode_measurement(Column) => Value}
        end
    end, #{}, lists:zip(Columns, Values)),
    _ = Types,
    case {maps:is_key(<<"createdat">>, Base), Timestamp} of
        {false, undefined} -> Base;
        {false, Time} -> Base#{<<"createdat">> => Time};
        _ -> Base
    end.

timestamp_at(Index, Timestamps) when Index =< length(Timestamps) ->
    lists:nth(Index, Timestamps);
timestamp_at(_Index, _Timestamps) ->
    undefined.

pad_types(Types, Count) when length(Types) >= Count -> lists:sublist(Types, Count);
pad_types(Types, Count) -> Types ++ lists:duplicate(Count - length(Types), <<"">>).

is_time_column(<<"Time">>) -> true;
is_time_column(<<"time">>) -> true;
is_time_column(<<"Timestamp">>) -> true;
is_time_column(_) -> false.

strip_trailing_slash(Url) when byte_size(Url) > 0 ->
    case binary:last(Url) of
        $/ -> strip_trailing_slash(binary:part(Url, 0, byte_size(Url) - 1));
        _ -> Url
    end;
strip_trailing_slash(Url) -> Url.
