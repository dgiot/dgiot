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
-export([transaction/2, format_data/4]).
-export([format_sql/3, get_values/3]).
-export([save_fields/2, get_fields/1]).
-export([batch_sql/2]).
-export([get_channel/1]).

transaction(Channel, Fun) ->
    case dgiot_data:get({?TYPE, Channel, config}) of
        not_find ->
            pass;
        Context ->
            Fun(Context)
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
            dgiot_tdengine_pool:insert_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end);
batch(Channel, Batch) ->
    transaction(Channel,
        fun(Context) ->
            Values = dgiot_tdengine_select:format_batch(Batch),
            Sql = <<"INSERT INTO ", Values/binary, ";">>,
            dgiot_tdengine_pool:insert_sql(Context#{<<"channel">> => Channel}, execute_update, Sql)
        end).

batch_sql(Channel, Sql) ->
    transaction(Channel,
        fun(Context) ->
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
format_data(ChannelId, ProductId, DevAddr, Data) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    Fields = get_fields(ProductId),
    NewValues = get_values(Fields, ProductId, Data),
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
get_values(Fields, ProductId, Data) ->
    Values0 =
        lists:foldl(fun(Field, Acc) ->
            case Field of
                <<"createdat">> ->
                    Acc ++ dgiot_utils:to_list(maps:get(<<"createdat">>, Data, now));
                Field ->
                    Value = dgiot_tdengine_field:check_value(maps:get(Field, Data, null), ProductId, Field),
                    case Value of
                        {NewValue, text} ->
                            Acc ++ ",\'" ++ dgiot_utils:to_list(NewValue) ++ "\'";
                        _ ->
                            Acc ++ "," ++ dgiot_utils:to_list(Value)
                    end
            end
                    end, " (", Fields),
    list_to_binary(Values0 ++ ")").

save_fields(ProductId, Results) ->
    dgiot_data:insert({ProductId, ?TABLEDESCRIBE}, Results),
    Fields =
        lists:foldl(fun(Column, Acc) ->
            case Column of
                #{<<"Note">> := <<"TAG">>} ->
                    Acc;
                #{<<"note">> := <<"TAG">>} ->
                    Acc;
                #{<<"Field">> := Field} ->
                    Acc ++ [Field];
                #{<<"field">> := Field} ->
                    Acc ++ [Field];
                _ ->
                    Acc
            end
                    end, [], Results),
    dgiot_data:insert({ProductId, ?TABLEFIELDS}, Fields).

get_fields(Table) ->
    case dgiot_data:get({Table, ?TABLEFIELDS}) of
        not_find ->
            [];
        Fields ->
            Fields
    end.

format_sql(ProductId, DevAddr, Data) ->
    {TagFields, ValueFields} =
        case dgiot_data:get({ProductId, ?TABLEDESCRIBE}) of
            Results when length(Results) > 0 ->
                get_sqls(Data, ProductId, {unicode:characters_to_binary(unicode:characters_to_list((dgiot_utils:to_binary(DevAddr)))), text}, Results);
            _ ->
                {<<" ">>, <<" ">>}
        end,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
    TdChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"TD">>, <<"TD资源通道"/utf8>>),
    DB = dgiot_tdengine:get_database(TdChannelId, ProductId),
    TableName = ?Table(DeviceId),
    Using1 = <<" using ", DB/binary, "_", ProductId/binary>>,
    <<"INSERT INTO ", DB/binary, TableName/binary, Using1/binary, " TAGS", TagFields/binary, " VALUES", ValueFields/binary, ";">>.

get_sqls(Data, ProductId, DevAddr, Results) ->
    get_sqls(Data, ProductId, DevAddr, Results, {<<"">>, <<"">>}).

get_sqls([], _ProductId, _DevAddr, _Results, Acc) ->
    Acc;

get_sqls([Data | Rest], ProductId, DevAddr, Results, {_, Acc}) ->
    Now = maps:get(<<"createdat">>, Data, now),
    {TagSql, Sql} = get_sql(Results, ProductId, Data#{<<"devaddr">> => DevAddr}, Now),
    get_sqls(Rest, ProductId, DevAddr, Results, {TagSql, <<Acc/binary, Sql/binary>>}).

get_sql(Results, ProductId, Values, Now) ->
    get_sql(Results, ProductId, Values, Now, {"(", "("}).

get_sql([], _ProductId, _Values, _Now, {TagAcc, Acc}) ->
    {list_to_binary(TagAcc ++ ")"), list_to_binary(Acc ++ ")")};

get_sql([Column | Results], ProductId, Values, Now, {TagAcc, Acc}) ->
    NewAcc =
        case Column of
            #{<<"Field">> := Field, <<"Note">> := <<"TAG">>} ->
                {get_value(Field, Values, ProductId, TagAcc), Acc};
            #{<<"field">> := Field, <<"note">> := <<"TAG">>} ->
                {get_value(Field, Values, ProductId, TagAcc), Acc};
            #{<<"Field">> := <<"createdat">>} ->
                {TagAcc, Acc ++ dgiot_utils:to_list(Now)};
            #{<<"field">> := <<"createdat">>} ->
                {TagAcc, Acc ++ dgiot_utils:to_list(Now)};
            #{<<"Field">> := Field} ->
                {TagAcc, get_value(Field, Values, ProductId, Acc)};
            #{<<"field">> := Field} ->
                {TagAcc, get_value(Field, Values, ProductId, Acc)};
            _ ->
                {TagAcc, Acc}
        end,
    get_sql(Results, ProductId, Values, Now, NewAcc).


get_value(Field, Values, ProductId, Acc) ->
    Value = dgiot_tdengine_field:check_value(maps:get(Field, Values, null), ProductId, Field),
    case Value of
        {NewValue, text} ->
            case Acc of
                "(" ->
                    Acc ++ "\'" ++ dgiot_utils:to_list(NewValue) ++ "\'";
                _ ->
                    Acc ++ ",\'" ++ dgiot_utils:to_list(NewValue) ++ "\'"
            end;
        _ ->
            case Acc of
                "(" ->
                    Acc ++ dgiot_utils:to_list(Value);
                _ ->
                    Acc ++ "," ++ dgiot_utils:to_list(Value)
            end
    end.

get_channel(Product) ->
    case dgiot_data:lookup({Product, ?TYPE}) of
        {ok, Channel} ->
            {ok, Channel};
        {error, not_find} ->
            {error, not_find_tdengine}
    end.
