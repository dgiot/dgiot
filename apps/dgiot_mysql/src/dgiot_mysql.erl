%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 四月 2020 16:38
%%%-------------------------------------------------------------------
-module(dgiot_mysql).
-author("kenneth").
-include("dgiot_mysql.hrl").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([create_database/1, create_schemas/2, create_object/3, create_user/3, alter_user/3, delete_user/2, query_object/3, query_object/4, batch/2]).
-export([create_database/2, create_schemas/1, create_object/2, create_user/2, alter_user/2, delete_user/1, query_object/2, batch/1]).
-export([query/2,query/3]).
%% 引入执行函数
-import(dgiot_mysql_channel, [transaction/2]).
-export([test/1]).


create_database(DataBase) ->
    create_database(?DEFAULT, DataBase).

create_database(Channel, DataBase) ->
    transaction(Channel,
        fun(Context) ->
            Sql = <<"CREATE DATABASE ", DataBase/binary, ";">>,
            query(Context, Sql)
        end).


create_schemas(Schema) ->
    create_schemas(?DEFAULT, Schema).

create_schemas(Channel, #{<<"tableName">> := TableName, <<"fields">> := Fields0}) ->
    transaction(Channel,
        fun(Context) ->
            Fields = list_to_binary(join(",", lists:foldr(
                fun({FieldName, #{<<"type">> := Type}}, Acc) ->
                    [<<FieldName/binary, " ", Type/binary>> | Acc]
                end, [], Fields0))),
            Sql = <<"CREATE TABLE ", TableName/binary, " (", Fields/binary, ");">>,
            query(Context, Sql)
        end).


%% 插入
create_object(TableName, Object) ->
    create_object(?DEFAULT, TableName, Object).
create_object(Channel, TableName, #{<<"values">> := Values0} = Object) ->
    transaction(Channel,
        fun(Context) ->
            Sql = format_batch(Object#{ <<"tableName">> => TableName, <<"values">> => [Values0]}),
            query(Context, Sql)
        end).


%% 查询
query_object(TableName, Query, SessionToken, from_rest) ->
    Filter = #{<<"where">> => #{<<"cType">> => ?TYPE}},
    case dgiot_parse:query_object(<<"Channel">>, Filter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Result}} when length(Result) == 1 ->
            [#{<<"objectId">> := Channel} |_] = Result,
            query_object(Channel, TableName, Query);
        {ok, #{<<"results">> := Result}} when length(Result) > 1 ->
            {error, <<"too many channel,pls use get_classes_cid_table api">>};
        _ ->
            {error, <<"not find channel">>}
    end.

query_object(TableName, Query) ->
    query_object(?DEFAULT, TableName, Query).
query_object(Channel, TableName, Query) ->
    transaction(Channel,
        fun(Context) ->
            Keys = format_keys(Query),
            Order = format_order(Query),
            Limit = format_limit(Query),
            Offset = format_offset(Query),
            Where = format_where(Query),
            From = format_from(Query),
            Group = format_group(Query),
            Get =
                fun(Keys1) ->
                    Tail = list_to_binary(join(" ", [TableName, From, Where], true)),
                    Sql = <<"SELECT ", Keys1/binary ," FROM ", Tail/binary, ";">>,
                    query(Context, Sql)
                end,
            Fun =
                fun(Keys1) ->
                    Tail = list_to_binary(join(" ", [TableName, From, Where, Order, Limit, Offset, Group], true)),
                    Sql = <<"SELECT ", Keys1/binary, " FROM ", Tail/binary, ";">>,
                    query(Context, Sql)
                end,
            case Keys of
                <<"count(*)">> ->
                    Get(<<"COUNT(*)">>);
                _ ->
                    List = re:split(Keys, <<",">>),
                    case lists:member(<<"count(*)">>, List) of
                        true ->
                            case Get(<<"COUNT(*)">>) of
                                {error, Reason} ->
                                    {error, Reason};
                                {ok,[<<"COUNT(*)">>], [[Count]]} ->
                                    Keys1 = list_to_binary(join(",", lists:delete(<<"count(*)">>, List), true)),
                                    case Fun(Keys1) of
                                        {ok, Columns, Records} ->
                                            Result = format_result(Columns, Records),
                                            {ok, #{ <<"results">> => Result, <<"count">> => Count }};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end
                            end;
                        false ->
                            case Fun(Keys) of
                                {ok, Columns, Records} ->
                                    Result = format_result(Columns, Records),
                                    {ok, #{ <<"results">> => Result }};
                                {error, Reason} ->
                                    {error, Reason}
                            end
                    end
            end
        end).


batch(Batch) ->
    batch(?DEFAULT, Batch).
batch(Channel, Requests) when is_list(Requests) ->
    transaction(Channel,
        fun(Context) ->
            Sql = list_to_binary(join(" ", [format_batch(Request) || Request <- Requests])),
            query(Context, Sql)
        end);
batch(Channel, Batch) ->
    transaction(Channel,
        fun(Context) ->
            Sql = format_batch(Batch),
            query(Context, Sql)
        end).


query(Sql, SessionToken, from_rest) ->
    Filter = #{<<"where">> => #{<<"cType">> => ?TYPE}},
    case dgiot_parse:query_object(<<"Channel">>, Filter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Result}} when length(Result) == 1 ->
            [#{<<"objectId">> := Channel} |_] = Result,
            query(Channel, Sql);
        {ok, #{<<"results">> := Result}} when length(Result) > 1 ->
            {error, <<"too many channel,pls use get_classes_cid_table api">>};
        _ ->
            {error, <<"not find channel">>}
    end.
query(Channel, Sql) when is_binary(Channel) ->
    transaction(Channel, fun(Context) -> query(Context, Sql)  end);
query(Context, Sql) ->
    case mysql:query(Context, Sql) of
        {error, {Code, Code1, Reason}} ->
            {error, list_to_binary(io_lib:format("#~p ~s ~s", [Code, Code1, Reason]))};
        Result ->
            Result
    end.


create_user(UserName, Password) ->
    create_user(?DEFAULT, UserName, Password).
create_user(Channel, UserName, Password) ->
    transaction(Channel,
        fun(Context) ->
            query(Context, <<"CREATE USER ", UserName/binary, " PASS ‘", Password/binary, "’">>)
        end).


delete_user(UserName) ->
    delete_user(?DEFAULT, UserName).
delete_user(Channel, UserName) ->
    transaction(Channel,
        fun(Context) ->
            query(Context, <<"DROP USER ", UserName/binary>>)
        end).

alter_user(UserName, NewPassword) ->
    alter_user(?DEFAULT, UserName, NewPassword).
alter_user(Channel, UserName, NewPassword) ->
    transaction(Channel,
        fun(Context) ->
            query(Context, <<"ALTER USER ", UserName/binary, " PASS ‘", NewPassword/binary, "’">>)
        end).


join(Sep, L) -> join(Sep, L, false).
join(Sep, L, Trip) -> join(Sep, L, Trip, fun to_binary/1).
join(_Sep, [], _, _) -> [];
join(Sep, [<<>> | T], true, F) -> join(Sep, T, true, F);
join(Sep, [H | T], Trip, F) -> [F(H) | join_prepend(Sep, T, Trip, F)].
join_prepend(_Sep, [], _, _) -> [];
join_prepend(Sep, [<<>> | T], true, F) -> join_prepend(Sep, T, true, F);
join_prepend(Sep, [H | T], Trip, F) -> [Sep, F(H) | join_prepend(Sep, T, Trip, F)].


format_value(V) when is_binary(V) -> <<"'", V/binary, "'">>;
format_value(V) -> to_binary(V).

format_column(V) -> to_binary(V).

format_result(Columns, Rows) -> format_result(Columns, Rows, []).
format_result(_, [], Acc) -> lists:reverse(Acc);
format_result(Columns, [Row|Rows], Acc) ->
    format_result(Columns, Rows, [format_row(Columns, Row)|Acc]).

format_row(Columns, Values) -> format_row(Columns, Values, #{}).
format_row([], [], Acc) -> Acc;
format_row([Column|Columns], [Value|Values], Acc) ->
    format_row(Columns, Values, Acc#{ Column => Value }).


format_batch(#{ <<"tableName">> := TableName, <<"fields">> := Fields0, <<"values">> := Values0}) ->
    Fields = list_to_binary(join(",", Fields0, false, fun format_column/1)),
    Values2 = [list_to_binary(binary_to_list(<<"INSERT INTO ", TableName/binary, "(", Fields/binary, ") VALUES (">>) ++ join(",", Values1, false, fun format_value/1) ++ ");") || Values1 <- Values0],
    list_to_binary(join(" ", Values2));
format_batch(#{ <<"tableName">> := TableName, <<"values">> := Values0}) ->
    Values2 = [list_to_binary(binary_to_list(<<"INSERT INTO ", TableName/binary, " VALUES ">>) ++ "(" ++ join(",", Values1, false, fun format_value/1) ++ ");") || Values1 <- Values0],
    list_to_binary(join(" ", Values2)).



format_order([], Acc) -> Acc;
format_order([<<"-", Field/binary>> | Other], Acc) ->
    format_order(Other, Acc ++ [<<Field/binary, " DESC">>]);
format_order([<<"+", Field/binary>> | Other], Acc) ->
    format_order([Field | Other], Acc);
format_order([Field | Other], Acc) ->
    format_order(Other, Acc ++ [<<Field/binary, " ASC">>]).
format_order(#{<<"order">> := Order}) when Order =/= <<>>, Order =/= undefined ->
    Order1 = list_to_binary(join(",", format_order(re:split(Order, <<",">>), []))),
    <<"ORDER BY ", Order1/binary>>;
format_order(_) ->
    <<>>.

format_limit(#{<<"limit">> := Limit}) when is_integer(Limit), Limit =/= undefined ->
    L = integer_to_binary(Limit),
    <<"LIMIT ", L/binary>>;
format_limit(_) ->
    <<"LIMIT 5000">>.

format_offset(#{<<"skip">> := Skip}) when Skip =/= undefined, is_integer(Skip) ->
    S = integer_to_binary(Skip),
    <<"OFFSET ", S/binary>>;
format_offset(_) ->
    <<>>.


format_keys(#{<<"keys">> := Keys}) when Keys =/= undefined, Keys =/= <<>> ->
    Keys;
format_keys(_) ->
    <<"*">>.


format_from(#{<<"from">> := From}) when From =/= <<>>, From =/= undefined ->
    From;
format_from(_) ->
    <<>>.

format_group(#{<<"group">> := Group}) when Group =/= <<>>, Group =/= undefined ->
    <<"GROUP BY ", Group/binary>>;
format_group(_) ->
    <<>>.

format_where([], Acc) ->
    Acc;
format_where([{Field, #{<<"$gt">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " > ", V/binary>> | Acc]);
format_where([{Field, #{<<"$gte">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " >= ", V/binary>> | Acc]);
format_where([{Field, #{<<"$lt">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " < ", V/binary>> | Acc]);
format_where([{Field, #{<<"$gte">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " <= ", V/binary>> | Acc]);
format_where([{Field, #{<<"$ne">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " <> ", V/binary>> | Acc]);
format_where([{Field, #{<<"$regex">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " LIKE '", V/binary, "'">> | Acc]);
format_where([{Field, Value} | Other], Acc) when Value == true; Value == false ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " = ", V/binary, "">> | Acc]);
format_where([{Field, Value} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " = '", V/binary, "'">> | Acc]).




format_where(#{<<"where">> := Where}) when Where =/= undefined, Where =/= <<>> ->
    Where1 =
        case is_list(Where) of
            true ->
                format_where(Where, []);
            false when is_map(Where) ->
                format_where(maps:to_list(Where), [])
        end,
    case list_to_binary(lists:join(" AND ", lists:reverse(Where1))) of
        <<>> ->
            <<>>;
        Where2 ->
            <<"WHERE ", Where2/binary>>
    end;
format_where(_) ->
    <<>>.


to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_float(V) -> list_to_binary(io_lib:format("~p", [V]));
to_binary(V) when is_binary(V) -> V.


test(ChannelId) ->
    %% 创建表
    R = create_schemas(ChannelId, #{
        <<"tableName">> => <<"Product">>,
        <<"fields">> => [
            {<<"config">>, #{
                <<"type">> => <<"INT">>
            }},
            {<<"secret">>, #{
                <<"type">> => <<"FLOAT">>
            }},
            {<<"description">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }},
            {<<"enable">>, #{
                <<"type">> => <<"BOOL">>
            }}
        ]
    }),
    ?LOG(info,"~p", [R]),

    %% 插入设备
    F =
        fun(I) ->
            create_object(ChannelId, <<"Product">>, #{
                <<"values">> => [I, 2.1, <<"aaaaa">>, false]
            })
        end,
    [F(I) || I <- lists:seq(1, 10)],
    R1 = query_object(ChannelId, <<"Product">>, #{
        <<"keys">> => <<"count(*),config,enable">>,
        <<"limit">> => 2,
        <<"skip">> => 0,
        <<"order">> => <<"-config">>,
        <<"where">> => #{
            <<"enable">> => false
        }
    }),
    ?LOG(info,"~p", [R1]),
    %% 创建普通表
    R2 = create_schemas(ChannelId, #{
        <<"tableName">> => <<"Device">>,
        <<"fields">> => [
            {<<"devaddr">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }},
            {<<"enable">>, #{
                <<"type">> => <<"BOOL">>
            }},
            {<<"description">>, #{
                <<"type">> => <<"NCHAR(10)">>
            }}
        ]
    }),
    ?LOG(info,"~p", [R2]),

    %% 插入一条记录
    R4 = create_object(ChannelId, <<"Device">>, #{
        <<"values">> => [<<"00000001">>, true, <<>>]
    }),
    ?LOG(info,"~p", [R4]),

    %% 插入一条记录，数据对应到指定的列
    R5 = create_object(ChannelId, <<"Device">>, #{
        <<"fields">> => [<<"devaddr">>, <<"enable">>, <<"description">>],
        <<"values">> => [<<"00000002">>, true, <<>>]
    }),
    ?LOG(info,"~p", [R5]),

    %% 插入多条记录
    batch(ChannelId, #{
        <<"tableName">> => <<"Device">>,
        <<"values">> => [
            [<<"00000003">>, true, <<>>],
            [<<"00000004">>, true, <<>>],
            [<<"00000005">>, true, <<>>]
        ]
    }),
    %% 按指定的列插入多条记录
    batch(ChannelId, #{
        <<"tableName">> => <<"Device">>,
        <<"fields">> => [<<"devaddr">>, <<"enable">>],
        <<"values">> => [
            [<<"00000006">>, true],
            [<<"00000007">>, true],
            [<<"00000008">>, true]
        ]
    }),
    %% 向多个表插入多条记录
    batch(ChannelId, [
        #{
            <<"tableName">> => <<"Device">>,
            <<"values">> => [
                [<<"00000009">>, true, <<>>],
                [<<"00000010">>, true, <<>>]
            ]},
        #{
            <<"tableName">> => <<"Device">>,
            <<"values">> => [
                [<<"00000001">>, true, <<>>],
                [<<"00000002">>, true, <<>>]
            ]}
    ]),
    %% 同时向多个表按列插入多条记录
    batch(ChannelId, [
        #{
            <<"tableName">> => <<"Device">>,
            <<"fields">> => [<<"devaddr">>, <<"enable">>, <<"description">>],
            <<"values">> => [
                [<<"00000011">>, true, <<>>],
                [<<"00000012">>, true, <<>>],
                [<<"00000013">>, true, <<>>]
            ]},
        #{
            <<"tableName">> => <<"Device">>,
            <<"fields">> => [<<"devaddr">>, <<"enable">>],
            <<"values">> => [
                [<<"00000003">>, true],
                [<<"00000004">>, true],
                [<<"00000005">>, true]
            ]}
    ]).
