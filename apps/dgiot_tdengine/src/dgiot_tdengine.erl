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
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
%% API
-export([get_database/2, create_database/3, create_schemas/2, create_object/3, create_user/3, alter_user/3, delete_user/2, query_object/3, batch/2]).
-export([create_database/2, create_schemas/1, create_object/2, create_user/2, alter_user/2, delete_user/1, query_object/2, batch/1]).
-export([transaction/2, format_data/5]).
-export([export/2, import/2]).
-export([format_sql/3, get_values/3]).

%% dgiot_tdengine:export().
export(ChannelId, #{<<"deviceid">> := DeviceId} = Body) ->
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"productid">> := ProductId}} ->
            export_device_data(ChannelId, #{<<"objectId">> => DeviceId, <<"product">> => #{<<"objectId">> => ProductId}}, Body, []);
        _ ->
            []
    end;

export(ChannelId, _Body) ->
%%    io:format("~s ~p 111Body = ~p.~n", [?FILE, ?LINE, Body]),
    Query = #{
        <<"keys">> => [<<"objectId">>, <<"product">>]
    },
    TdQuery = #{<<"limit">> => 100000, <<"function">> => <<"last">>, <<"interval">> => <<"1m">>},
    case dgiot_parse:query_object(<<"Device">>, Query) of
        {ok, #{<<"results">> := Data}} ->
            lists:foldl(fun(Device, Acc) ->
                export_device_data(ChannelId, Device, TdQuery, Acc)
                        end, [], Data);
        _ ->
            []
    end.

%% dgiot_tdengine:import().
import(ChannelId, Result) ->
    lists:foldl(fun({Name, Bin}, _Acc) ->
        case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
            {'EXIT', _} ->
                pass;
            Data ->
                case binary:split(dgiot_utils:to_binary(Name), <<$/>>, [global, trim]) of
                    [ProductId, <<DeviceId:10/binary, _/binary>>] ->
                        import_device_data(ChannelId, ProductId, DeviceId, Data);
                    _ ->
                        pass
                end
        end
                end, #{}, Result).

export_device_data(ChannelId, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}, Query, NewData) ->
%%    io:format("~s ~p Query = ~p.~n", [?FILE, ?LINE, Query]),
    TableName = ?Table(DeviceId),
    case dgiot_device_tdengine:get_history_data(ChannelId, ProductId, TableName, Query) of
        {_TdNames, {ok, #{<<"results">> := TdResults}}} when length(TdResults) > 0 ->
            NewTdResults =
                lists:foldl(fun(Result, Acc) ->
                    Acc ++ [Result]
                            end, [], TdResults),
            NewData ++ [{dgiot_utils:to_list(<<ProductId/binary, "/", DeviceId/binary, ".json">>), unicode:characters_to_binary(jsx:encode(#{<<"results">> => NewTdResults}))}];
        _ ->
            NewData
    end.

%% INSERT INTO _010e2df351._da06aff7f0 using _010e2df351._010e2df351 TAGS ('_844425383144878') VALUES  (now,null,6756.5,null,null,null);
import_device_data(ChannelId, ProductId, DeviceId, TdData) ->
    case TdData of
        #{<<"results">> := TdResults} ->
            transaction(ChannelId,
                fun(_Context) ->
                    case dgiot_device:lookup(DeviceId) of
                        {ok, #{<<"devaddr">> := DevAddr}} ->
                            case dgiot_bridge:get_product_info(ProductId) of
                                {ok, #{<<"thing">> := Properties}} ->
                                    lists:foldl(fun
                                                    (#{<<"createdat">> := V} = Data, _Acc) ->
                                                        NewV =
                                                            case binary:split(V, <<$.>>, [global, trim]) of
                                                                [NewV1, _] ->
                                                                    NewV1;
                                                                _ ->
                                                                    V
                                                            end,
                                                        Createdat = dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(NewV)) * 1000,
                                                        Object = dgiot_tdengine:format_data(ChannelId, ProductId, DevAddr, Properties, Data#{<<"createdat">> => Createdat}),
                                                        dgiot_tdengine:batch(ChannelId, Object);
                                                    (Data, _Acc) ->
                                                        Object = dgiot_tdengine:format_data(ChannelId, ProductId, DevAddr, Properties, Data),
                                                        dgiot_tdengine:batch(ChannelId, Object)
                                                end, [], TdResults);
                                _ ->
                                    pass
                            end;
                        _ ->
                            pass
                    end
                end);
        _ ->
            pass
    end.

transaction(Channel, Fun) ->
    case dgiot_channelx:call(?TYPE, Channel, config) of
        {ok, Context} ->
            Fun(Context);
        {error, Reason} ->
            {error, Reason}
    end.

get_database(ChannelId, ProductId) ->
    case dgiot_data:get({tdengine_db, ChannelId, ProductId}) of
        not_find ->
            dgiot_tdengine_select:format_db(?Database(ProductId));
        DbName ->
            DbName
    end.

create_database(DataBase, Keep) ->
    create_database(?DEFAULT, DataBase, Keep).

create_database(Channel, DataBase, Keep) ->
    transaction(Channel,
        fun(Context) ->
            Sql = dgiot_tdengine_schema:create_database(#{<<"database">> => DataBase, <<"keep">> => Keep}),
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).


create_schemas(Schema) ->
    create_schemas(?DEFAULT, Schema).

%% 如果里面有using，则为使用了超级表创建子表
create_schemas(Channel, #{<<"using">> := _STbName} = Query) ->
    transaction(Channel,
        fun(Context) ->
            Sql = dgiot_tdengine_schema:create_table(Query, Context#{<<"channel">> => Channel}),
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end);

%% 如果schema里面带有tags则为超级表，没有则为普通表
create_schemas(Channel, Query) ->
    transaction(Channel,
        fun(Context) ->
            Sql = dgiot_tdengine_schema:create_table(Query, Context#{<<"channel">> => Channel}),
            R = dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql),
            dgiot_tdengine_schema:alter_table(Query, Context#{<<"channel">> => Channel}),
            R
        end).

%% 插入
create_object(TableName, Object) ->
    create_object(?DEFAULT, TableName, Object).
create_object(Channel, TableName, #{<<"values">> := Values0} = Object) ->
    transaction(Channel,
        fun(Context) ->
            DB = dgiot_tdengine:get_database(Channel, maps:get(<<"db">>, Object, <<"">>)),
            Values = dgiot_tdengine_select:format_batch(Object#{<<"db">> => DB, <<"tableName">> => TableName, <<"values">> => [Values0]}),
            Sql = <<"INSERT INTO ", Values/binary, ";">>,
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).

%% 查询
query_object(TableName, Query) ->
    query_object(?DEFAULT, TableName, Query).
query_object(Channel, TableName, Query) ->
    transaction(Channel,
        fun(Context) ->
            Database = dgiot_tdengine:get_database(Channel, maps:get(<<"db">>, Query, <<"">>)),
            Sql = dgiot_tdengine_select:select(TableName, Query#{<<"channel">> => Channel, <<"db">> => Database}),
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end).

%% 批处理
batch(Batch) ->
    batch(?DEFAULT, Batch).
batch(Channel, Requests) when is_list(Requests) ->
    transaction(Channel,
        fun(Context) ->
            Request1 = list_to_binary(dgiot_utils:join(" ", [dgiot_tdengine_select:format_batch(Request) || Request <- Requests])),
            Sql = <<"INSERT INTO ", Request1/binary, ";">>,
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end);
batch(Channel, Batch) ->
    transaction(Channel,
        fun(Context) ->
            Values = dgiot_tdengine_select:format_batch(Batch),
            Sql = <<"INSERT INTO ", Values/binary, ";">>,
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).

create_user(UserName, Password) ->
    create_user(?DEFAULT, UserName, Password).
create_user(Channel, UserName, Password) ->
    transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"CREATE USER ", UserName/binary, " PASS ‘", Password/binary, "’">>)
        end).


delete_user(UserName) ->
    delete_user(?DEFAULT, UserName).
delete_user(Channel, UserName) ->
    transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"DROP USER ", UserName/binary>>)
        end).

alter_user(UserName, NewPassword) ->
    alter_user(?DEFAULT, UserName, NewPassword).
alter_user(Channel, UserName, NewPassword) ->
    transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, <<"ALTER USER ", UserName/binary, " PASS ‘", NewPassword/binary, "’">>)
        end).


%% 产品，设备地址与数据分离，推荐
format_data(ChannelId, ProductId, DevAddr, Properties, Data) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    Values = dgiot_tdengine_field:check_fields(Data, Properties),
    Fields = get_fields(ProductId),
    dgiot_data:insert({td, ProductId, DeviceId}, Values#{<<"createdat">> => dgiot_datetime:nowstamp()}),
    Now = maps:get(<<"createdat">>, Data, now),
    NewValues = get_values(ProductId, Values, Now),
    dgiot_data:insert(?DGIOT_TD_THING_ETS, DeviceId, Values),
    DB = get_database(ChannelId, ProductId),
    #{
        <<"db">> => DB,
        <<"tableName">> => ?Table(DeviceId),
        <<"using">> => ?Table(ProductId),
        <<"tags">> => [?Table(DevAddr)],
        <<"fields">> => Fields,
        <<"values">> => NewValues
    }.


%% INSERT INTO _173acf2f85._af6d16f9ba using _173acf2f85._173acf2f85 TAGS ('_KOHbyuiJilnsdD') VALUES (now,null,null,null,null);
get_values(Table, Values, Now) ->
    Values0 =
        case dgiot_data:get({Table, ?TABLEDESCRIBE}) of
            Results when length(Results) > 0 ->
                lists:foldl(fun(Column, Acc) ->
                    case Column of
                        #{<<"Note">> := <<"TAG">>} ->
                            Acc;
                        #{<<"Field">> := <<"createdat">>} ->
                            Acc ++ dgiot_utils:to_list(Now);
                        #{<<"Field">> := Field} ->
                            Value = maps:get(Field, Values, null),
                            case Value of
                                {NewValue, text} ->
                                    Acc ++ ",\'" ++ dgiot_utils:to_list(NewValue) ++ "\'";
                                _ ->
                                    Acc ++ "," ++ dgiot_utils:to_list(Value)
                            end;
                        _ ->
                            Acc
                    end
                            end, " (", Results);
            _ ->
                " "
        end,
    list_to_binary(Values0 ++ ")").

get_fields(Table) ->
    case dgiot_data:get({Table, ?TABLEDESCRIBE}) of
        Results when length(Results) > 0 ->
            lists:foldl(fun(Column, Acc) ->
                case Column of
                    #{<<"Field">> := Field} ->
                        Acc ++ [Field];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end.


format_sql(ProductId, DevAddr, Data) ->
    case dgiot_bridge:get_product_info(ProductId) of
        {ok, #{<<"thing">> := Properties}} ->
            NewValues =
                case dgiot_data:get({ProductId, ?TABLEDESCRIBE}) of
                    Results when length(Results) > 0 ->
                        get_sqls(Data, ProductId, Properties, Results);
                    _ ->
                        " "
                end,
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            TdChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
            DB = dgiot_tdengine:get_database(TdChannelId, ProductId),
            TableName = ?Table(DeviceId),
            Using1 = <<" using ", DB/binary, "_", ProductId/binary>>,
            TagFields = <<" TAGS ('_", DevAddr/binary, "')">>,
            <<"INSERT INTO ", DB/binary, TableName/binary, Using1/binary, TagFields/binary, " VALUES", NewValues/binary, ";">>;
        _ ->
            <<"show database;">>
    end.


get_sqls(Data, ProductId, Properties, Results) ->
    get_sqls(Data, ProductId, Properties, Results, <<"">>).

get_sqls([], _ProductId, _Properties, _Results, Acc) ->
    Acc;

get_sqls([Data | Rest], ProductId, Properties, Results, Acc) ->
    Now = maps:get(<<"createdat">>, Data, now),
    Sql = get_sql(Results, Data, Now),
    get_sqls(Rest, ProductId, Properties, Results, <<Acc/binary, Sql/binary>>).

get_sql(Results, Values, Now) ->
    get_sql(Results, Values, Now, " (").

get_sql([], _Values, _Now, Acc) ->
    list_to_binary(Acc ++ ")");

get_sql([Column | Results], Values, Now, Acc) ->
    NewAcc =
        case Column of
            #{<<"Note">> := <<"TAG">>} ->
                Acc;
            #{<<"Field">> := <<"createdat">>} ->
                Acc ++ dgiot_utils:to_list(Now);
            #{<<"Field">> := Field} ->
                Value = maps:get(Field, Values, null),
                case Value of
                    {NewValue, text} ->
                        Acc ++ ",\'" ++ dgiot_utils:to_list(NewValue) ++ "\'";
                    _ ->
                        Acc ++ "," ++ dgiot_utils:to_list(Value)
                end;
            _ ->
                Acc
        end,
    get_sql(Results, Values, Now, NewAcc).
