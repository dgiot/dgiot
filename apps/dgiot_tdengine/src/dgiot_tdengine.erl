%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_tdengine).
-author("kenneth").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([create_database/3, create_schemas/2, create_object/3, create_user/3, alter_user/3, delete_user/2, query_object/3, batch/2]).
-export([create_database/2, create_schemas/1, create_object/2, create_user/2, alter_user/2, delete_user/1, query_object/2, batch/1, parse_batch/1]).
-export([to_unixtime/1, get_channel/1]).
-export([get_product/2, get_products/2, get_chartdata/3, get_appdata/3]).
-export([get_device/3, get_device/4, get_device/5, get_keys/3, get_reportdata/3]).
%% 引入执行函数
-import(dgiot_tdengine_channel, [run_sql/3, transaction/2]).
-export([test_product/0]).

to_unixtime(Time) when is_integer(Time) ->
    Time;
to_unixtime(Time) ->
    Size = byte_size(Time) - 4,
    <<DateTime:Size/binary, _/binary>> = Time,
    dgiot_datetime:to_unixtime(dgiot_datetime:to_localtime(DateTime)).

test_product() ->
    ProductId = <<"0765bee775">>,
    Query = #{
        <<"keys">> => [<<"last_row(createdat)">>],
        <<"group">> => <<"devaddr">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"createdat">> => #{
                <<"$gte">> => <<"now - 10d">>
            }
        }
    },
    dgiot_tdengine:get_product(ProductId, Query).

get_product(ProductId, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find -> [];
        ChannelId ->
            TableName = ?Table(ProductId),
            case dgiot_tdengine:query_object(ChannelId, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_products(ProductId, ChannelId) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"nodeType">> := 2}} ->
            dgiot_product:load(ProductId),
            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
            case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1, <<"where">> => #{<<"product">> => ProductId}}) of
                {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
                    [#{<<"objectId">> := Devid} | _] = Results,
                    case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1000, <<"keys">> => [<<"product">>], <<"where">> => #{<<"parentId">> => Devid}}) of
                        {ok, #{<<"results">> := R}} ->
                            lists:foldl(fun(#{<<"product">> := Product}, Acc) ->
                                #{<<"objectId">> := SubProductId} = Product,
                                dgiot_product:load(SubProductId),
%%                                ?LOG(info, "SubProductId ~p ChannelId ~p", [SubProductId, ChannelId]),
                                dgiot_data:insert({SubProductId, ?TYPE}, ChannelId),
                                Acc ++ [SubProductId]
                                        end, [ProductId], R);
                        _ -> [ProductId]
                    end;
                _ -> [ProductId]
            end;
        _ ->
            dgiot_product:load(ProductId),
            dgiot_data:insert({ProductId, ?TYPE}, ChannelId),
            [ProductId]
    end.

%% #{<<"keys">> => <<"last_row(*)">>, <<"limit">> => 1} 查询td最新的一条device
get_device(ProductId, DevAddr, Query) ->
    case dgiot_data:get({ProductId, ?TYPE}) of
        not_find ->
            {error, <<"not find channel">>};
        ChannelId ->
            DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
            TableName = ?Table(DeviceId),
            case dgiot_tdengine:query_object(ChannelId, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_device(Channel, ProductId, DevAddr, Query) ->
    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
    TableName = ?Table(DeviceId),
    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    end.

get_device(Channel, ProductId, DeviceId, _DevAddr, #{<<"keys">> := <<"last_row(*)">>, <<"limit">> := 1} = Query) ->
    case dgiot_data:get({td, ProductId, DeviceId}) of
        not_find ->
            case dgiot_data:get({tdengine_os, Channel}) of
                <<"windows">> ->
                    case dgiot_parse:query_object(<<"Timescale">>, Query#{
                        <<"where">> => #{
                            <<"product">> => ProductId,
                            <<"device">> => DeviceId
                        },
                        <<"keys">> => [<<"values">>],
                        <<"order">> => <<"-createdAt">>,
                        <<"limit">> => 1
                    }) of
                        {ok, #{<<"results">> := [Data | _] = Result}} when length(Result) > 0 ->
                            {ok, #{<<"results">> => [maps:get(<<"values">>, Data)]}};
                        {ok, #{<<"results">> := Result}} ->
                            {error, Result};
                        Error ->
                            Error
                    end;
                _ ->
                    TableName = ?Table(DeviceId),
                    case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
                        {ok, Data} ->
                            {ok, Data};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        Value -> {ok, #{<<"results">> => [Value]}}
    end;

get_device(Channel, ProductId, DeviceId, _DevAddr, Query) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            case dgiot_parse:query_object(<<"Timescale">>, Query#{
                <<"where">> => #{
                    <<"product">> => ProductId,
                    <<"device">> => DeviceId
                },
                <<"keys">> => [<<"values">>],
                <<"order">> => <<"-createdAt">>
            }) of
                {ok, #{<<"results">> := Result} = All} when length(Result) > 0 ->
                    {ok, All#{<<"results">> => [maps:get(<<"values">>, X) || X <- Result]}};
                {ok, All} ->
                    {error, All};
                Error ->
                    Error
            end;
        _ ->
            TableName = ?Table(DeviceId),
            case dgiot_tdengine:query_object(Channel, TableName, Query#{<<"db">> => ProductId}) of
                {ok, Data} ->
                    {ok, Data};
                {error, Reason} ->
                    {error, Reason}
            end
    end.


get_channel(Session) ->
    Body = #{
        <<"keys">> => <<"objectId">>,
        <<"order">> => <<"-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"cType">> => <<"TD">>,
            <<"isEnable">> => true
        }
    },
    case dgiot_parse:query_object(<<"Channel">>, Body, [{"X-Parse-Session-Token", Session}], [{from, rest}]) of
        {ok, #{<<"results">> := []}} ->
            {error, <<"not find channel">>};
        {ok, #{<<"results">> := [#{<<"objectId">> := ChannelId}]}} ->
            {ok, ChannelId};
        {error, Reason} ->
            {error, Reason}
    end.

create_database(DataBase, Keep) ->
    create_database(?DEFAULT, DataBase, Keep).

create_database(Channel, DataBase, Keep) ->
    transaction(Channel,
        fun(Context) ->
            KeepTime = integer_to_binary(Keep),
            Sql = <<"CREATE DATABASE IF NOT EXISTS ", DataBase/binary, " KEEP ", KeepTime/binary>>,
            run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).


create_schemas(Schema) ->
    create_schemas(?DEFAULT, Schema).

%% 如果里面有using，则为使用了超级表创建子表
create_schemas(Channel, #{<<"tableName">> := TableName, <<"using">> := STbName, <<"tags">> := Tags}) ->
    transaction(Channel,
        fun(Context) ->
            TagFields = list_to_binary(join(",", Tags, fun format_value/1)),
            DB1 = format_db(TableName),
            Sql = <<"CREATE TABLE IF NOT EXISTS ", DB1/binary, TableName/binary, " USING ", STbName/binary, " TAGS (", TagFields/binary, ");">>,
            run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end);

%% 如果schema里面带有tags则为超级表，没有则为普通表
create_schemas(Channel, #{<<"tableName">> := TableName, <<"fields">> := Fields0} = Schema) ->
    transaction(Channel,
        fun(Context) ->
            DB1 = format_db(TableName),
            Fields = list_to_binary(join(",", ["createdat TIMESTAMP"] ++ lists:foldr(
                fun({FieldName, #{<<"type">> := Type}}, Acc) ->
                    [<<FieldName/binary, " ", Type/binary>> | Acc]
                end, [], Fields0))),
            TagFields = list_to_binary(join(",", lists:foldr(
                fun({TagName, #{<<"type">> := TType}}, Acc) ->
                    [<<TagName/binary, " ", TType/binary>> | Acc]
                end, [], maps:get(<<"tags">>, Schema, [])))),
            Sql =
                case TagFields of
                    <<>> ->
                        <<"CREATE TABLE IF NOT EXISTS ", DB1/binary, TableName/binary, " (", Fields/binary, ");">>;
                    _ ->
                        <<"CREATE TABLE IF NOT EXISTS ", DB1/binary, TableName/binary, " (", Fields/binary, ") TAGS (", TagFields/binary, ");">>
                end,
            alter_table(DB1, TableName, Context, Channel),
            run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).

alter_table(DB1, TableName, Context, Channel) ->
    Sql1 = <<"DESCRIBE ", DB1/binary, TableName/binary, ";">>,
    case run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql1) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            TdColumn =
                lists:foldl(fun(Column, Acc) ->
                    case Column of
                        #{<<"Field">> := Identifier, <<"Type">> := Type} ->
                            Acc#{Identifier => list_to_binary(string:to_lower(binary_to_list(Type)))};
                        _ ->
                            Acc
                    end
                            end, #{}, Results),
%%            ?LOG(info,"TdColumn ~p", [TdColumn]),
            <<"_", ProductId/binary>> = TableName,
            case dgiot_parse:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                    lists:foldl(fun(Prop, _Acc1) ->
                        case Prop of
                            #{<<"dataType">> := #{<<"type">> := Type}, <<"identifier">> := Identifier} ->
                                LowerIdentifier = list_to_binary(string:to_lower(binary_to_list(Identifier))),
                                case maps:find(LowerIdentifier, TdColumn) of
                                    error ->
                                        AddSql =
                                            case Type of
                                                <<"enum">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " INT;">>;
                                                <<"file">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(10);">>;
                                                <<"text">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(10);">>;
                                                <<"url">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(10);">>;
                                                <<"geopoint">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(30);">>;
                                                <<"image">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " BIGINT;">>;
                                                <<"date">> ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " TIMESTAMP;">>;
                                                _ ->
                                                    <<"ALTER TABLE ", DB1/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " ", Type/binary, ";">>
                                            end,
%%                                        ?LOG(info,"AddSql ~p", [AddSql]),
                                        run_sql(Context#{<<"channel">> => Channel}, execute_query, AddSql);
                                    _ ->
%%                                            todo   类型改变
                                        pass
                                end
                        end
                                end, #{}, Props),
                    {ok, #{<<"results">> := Results2}} = run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql1),
                    dgiot_data:insert({ProductId, describe_table}, Results2);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

%% 插入
create_object(TableName, Object) ->
    create_object(?DEFAULT, TableName, Object).
create_object(Channel, TableName, #{<<"values">> := Values0} = Object) ->
    transaction(Channel,
        fun(Context) ->
            Values = format_batch(Object#{<<"tableName">> => TableName, <<"values">> => [Values0]}),
            Sql = <<"INSERT INTO ", Values/binary, ";">>,
            run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).


%% 查询
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
            Interval = format_interval(Query),
            Fill = format_fill(Query),
            From = format_from(Query),
            Group = format_group(Query),
            Database = maps:get(<<"db">>, Query),
            DB = format_db(?Database(Database)),
            Tail = list_to_binary(join(" ", [TableName, From, Where, Interval, Fill, Group, Order, Limit, Offset], true)),
            Sql = <<"SELECT ", Keys/binary, " FROM ", DB/binary, Tail/binary>>,
            run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

%% SELECT max(day_electricity) '时间' ,max(charge_current) '日期' FROM _2d26a94cf8._c5e1093e30 WHERE createdat >= now - 1h INTERVAL(1h) limit 10;
get_chartdata(Channel, TableName, Query) ->
    transaction(Channel,
        fun(Context) ->
            Database = maps:get(<<"db">>, Query),
            Function = maps:get(<<"function">>, Query),
            Keys = maps:get(<<"keys">>, Query),
            Limit = format_limit(Query),
            Interval = maps:get(<<"interval">>, Query),
            Starttime = maps:get(<<"starttime">>, Query),
            Endtime = maps:get(<<"endtime">>, Query),
            {Names, Newkeys} = get_keys(Database, Function, Keys),
            DB = format_db(?Database(Database)),
            Tail = <<" where createdat >= ", Starttime/binary, " AND createdat <= ", Endtime/binary, " INTERVAL(", Interval/binary, ") ", Limit/binary, ";">>,
            Sql = <<"SELECT ", Newkeys/binary, " FROM ", DB/binary, TableName/binary, Tail/binary>>,
            ?LOG(error, "Sql ~s", [Sql]),
            {Names, run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)}
        end).

%% SELECT last(*) FROM _2d26a94cf8._c5e1093e30;
get_appdata(Channel, TableName, Query) ->
    transaction(Channel,
        fun(Context) ->
            Database = maps:get(<<"db">>, Query),
            {_Names, Newkeys} = get_keys(Database, <<"last">>, <<"*">>),
            DB = format_db(?Database(Database)),
            case size(Newkeys) > 0 of
                true ->
                    Sql = <<"SELECT last(createdat) createdat, ", Newkeys/binary, " FROM ", DB/binary, TableName/binary, ";">>,
                    run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql);
                _ ->
                    {error, #{<<"msg">> => <<"无物模型"/utf8>>}}
            end
        end).

get_keys(ProductId, Function, <<"*">>) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, {Names, Acc}) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"name">> := Name} ->
                        case Acc of
                            <<"">> ->
                                {Names, <<Function/binary, "(", Identifier/binary, ") \'", Identifier/binary, "\'">>};
                            _ ->
                                {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", Identifier/binary, ") \'", Identifier/binary, "\'">>}
                        end;
                    _ ->
                        {Names, Acc}
                end
                        end, {[], <<"">>}, Props);
        _Other ->
            ?LOG(info, "_Other ~p", [_Other]),
            {[], <<"">>}
    end;

get_keys(ProductId, Function, Keys) when Keys == undefined; Keys == <<>> ->
    get_keys(ProductId, Function, <<"*">>);

get_keys(ProductId, Function, Keys) ->
    List =
        case is_list(Keys) of
            true -> Keys;
            false -> re:split(Keys, <<",">>)
        end,
    Maps = get_prop(ProductId),
    lists:foldl(fun(X, {Names, Acc}) ->
        case maps:find(X, Maps) of
            error ->
                {Names, Acc};
            Name ->
                case Acc of
                    <<"">> ->
                        {Names, <<Function/binary, "(", X/binary, ") \'", X/binary, "\'">>};
                    _ ->
                        {Names ++ [Name], <<Acc/binary, ", ", Function/binary, "(", X/binary, ") \'", X/binary, "\'">>}
                end
        end
                end, {[], <<"">>}, List).

get_prop(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier, <<"name">> := Name} ->
                        Acc#{Identifier => Name};
                    _ -> Acc
                end
                        end, #{}, Props);
        _ ->
            #{}
    end.

%% select flow, head, effect, power from _09d0bbcf44._47d9172bf1 where createdat >= 1635581932000 AND createdat <= 1638260333000 order by createdat asc;
get_reportdata(Channel, TableName, Query) ->
    transaction(Channel,
        fun(Context) ->
            Database = maps:get(<<"db">>, Query),
            Keys = maps:get(<<"keys">>, Query),
            Starttime = maps:get(<<"starttime">>, Query),
            Endtime = maps:get(<<"endtime">>, Query),
            DB = format_db(?Database(Database)),
            Tail = <<" where createdat >= ", Starttime/binary, " AND createdat <= ", Endtime/binary, " order by createdat asc;">>,
            Sql = <<"SELECT ", Keys/binary, " FROM ", DB/binary, TableName/binary, Tail/binary>>,
            ?LOG(error, "Sql ~s", [Sql]),
            run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

batch(Batch) ->
    batch(?DEFAULT, Batch).
batch(Channel, Requests) when is_list(Requests) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            parse_batch(Requests);
        _ ->
            transaction(Channel,
                fun(Context) ->
                    Request1 = list_to_binary(join(" ", [format_batch(Request) || Request <- Requests])),
                    Sql = <<"INSERT INTO ", Request1/binary, ";">>,
                    run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
                end)
    end;
batch(Channel, Batch) ->
    case dgiot_data:get({tdengine_os, Channel}) of
        <<"windows">> ->
            parse_batch(Batch);
        _ ->
            transaction(Channel,
                fun(Context) ->
                    Values = format_batch(Batch),
                    Sql = <<"INSERT INTO ", Values/binary, ";">>,
                    run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
                end)
    end.

parse_batch(Requests) when is_list(Requests), length(Requests) < 5 ->
    NewRequests =
        lists:foldl(fun(X, Acc) ->
            Acc ++ lists:foldl(fun(Y, Acc1) ->
                Values = maps:from_list(lists:zip(maps:get(<<"fields">>, X), Y)),
                Tags = maps:from_list(lists:zip([<<"devaddr">>], maps:get(<<"tags">>, X))),
                <<"_", ProductId/binary>> = maps:get(<<"db">>, X),
                <<"_", DeviceId/binary>> = maps:get(<<"tableName">>, X),
                Acc1 ++ [#{
                    <<"method">> => <<"POST">>,
                    <<"path">> => <<"/classes/Timescale">>,
                    <<"body">> => #{
                        <<"product">> => #{
                            <<"className">> => <<"Product">>,
                            <<"objectId">> => ProductId,
                            <<"__type">> => <<"Pointer">>
                        },
                        <<"device">> => #{
                            <<"className">> => <<"Device">>,
                            <<"objectId">> => DeviceId,
                            <<"__type">> => <<"Pointer">>
                        },
                        <<"values">> => Values#{<<"createdat">> => dgiot_datetime:nowstamp()},
                        <<"tags">> => Tags
                    }
                }]
                               end, Acc, maps:get(<<"values">>, X))
                    end, [], Requests),
    dgiot_parse:batch(NewRequests);

parse_batch(_Requests) ->
    {ok, _Requests}.

create_user(UserName, Password) ->
    create_user(?DEFAULT, UserName, Password).
create_user(Channel, UserName, Password) ->
    transaction(Channel,
        fun(Context) ->
            run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"CREATE USER ", UserName/binary, " PASS ‘", Password/binary, "’">>)
        end).


delete_user(UserName) ->
    delete_user(?DEFAULT, UserName).
delete_user(Channel, UserName) ->
    transaction(Channel,
        fun(Context) ->
            run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"DROP USER ", UserName/binary>>)
        end).

alter_user(UserName, NewPassword) ->
    alter_user(?DEFAULT, UserName, NewPassword).
alter_user(Channel, UserName, NewPassword) ->
    transaction(Channel,
        fun(Context) ->
            run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"ALTER USER ", UserName/binary, " PASS ‘", NewPassword/binary, "’">>)
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

format_db(<<>>) -> <<>>;
format_db(DB) -> <<DB/binary, ".">>.

format_using(_, <<>>) -> <<>>;
format_using(DB, Table) ->
    DB1 = format_db(DB),
    <<" using ", DB1/binary, Table/binary>>.

format_tags(<<>>) -> <<>>;
format_tags(Tags) -> <<" TAGS (", Tags/binary, ")">>.

format_batch(#{<<"db">> := DB, <<"tableName">> := TableName, <<"fields">> := _Fields0, <<"values">> := Values0} = Batch) ->
    Using = maps:get(<<"using">>, Batch, <<>>),
    DB1 = format_db(DB),
    Using1 = format_using(DB, Using),
    Tags = maps:get(<<"tags">>, Batch, []),
    TagFields = format_tags(list_to_binary(join(",", Tags, false, fun format_value/1))),
    <<DB1/binary, TableName/binary, Using1/binary, TagFields/binary, " VALUES ", Values0/binary>>;
format_batch(#{<<"db">> := DB, <<"tableName">> := TableName, <<"values">> := Values0} = Batch) ->
    Using = maps:get(<<"using">>, Batch, <<>>),
    DB1 = format_db(DB),
    Using1 = format_using(DB, Using),
    Tags = maps:get(<<"tags">>, Batch, []),
    TagFields = format_tags(list_to_binary(join(",", Tags, false, fun format_value/1))),
    <<DB1/binary, TableName/binary, Using1/binary, TagFields/binary, " VALUES ", Values0/binary>>.

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

format_keys(#{<<"keys">> := Keys}) when Keys =/= undefined, Keys =/= <<>>, is_list(Keys) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            <<>> -> X;
            _ -> <<Acc/binary, ",", X/binary>>
        end
                end, <<>>, Keys);
format_keys(#{<<"keys">> := Keys}) when Keys =/= undefined, Keys =/= <<>> ->
    Keys;
format_keys(_) ->
    <<"*">>.

format_interval(#{<<"interval">> := Interval}) when Interval =/= undefined, is_integer(Interval) ->
    I = integer_to_binary(Interval),
    <<"INTERVAL(", I/binary, ")">>;
format_interval(_) ->
    <<>>.

format_fill(#{<<"fill">> := Value}) when Value =/= <<>>, Value =/= undefined ->
    <<"FILL(", Value/binary, ")">>;
format_fill(_) ->
    <<>>.

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
format_where([{Field, #{<<"$lte">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " <= ", V/binary>> | Acc]);
format_where([{Field, #{<<"$ne">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " <> ", V/binary>> | Acc]);
format_where([{Field, #{<<"$regex">> := Value}} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " LIKE '", V/binary, "'">> | Acc]);
format_where([{Field, Value} | Other], Acc) ->
    V = to_binary(Value),
    format_where(Other, [<<Field/binary, " = '", V/binary, "'">> | Acc]).
format_where(#{<<"where">> := Where0}) when Where0 =/= undefined, Where0 =/= <<>> ->
    Where =
        case jsx:is_json(Where0) of
            true -> jsx:decode(Where0, [return_maps]);
            fasle -> Where0
        end,
    Where1 =
        case is_list(Where) of
            true ->
                Where3 =
                    lists:foldl(fun(X, Acc) ->
                        Acc ++ maps:to_list(X)
                                end, [], Where),
                format_where(Where3, []);
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
