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

-module(dgiot_tdengine_schema).
-author("jonliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([get_schema/2, create_database/1, create_table/2, alter_table/2, get_addSql/4]).
<<<<<<< HEAD
-export([extract_columns/1, create_stable_by_columns/5]).

%% 从产品物模型中提取所有字段（已清洗、去重，不含 devaddr 标签）
extract_columns(Product) ->
    Thing = maps:get(<<"thing">>, Product, #{}),
    Properties = maps:get(<<"properties">>, Thing, []),
    Tags = maps:get(<<"tags">>, Thing, []),
    
    % 收集所有字段，以清洗后的名称为键，类型为值，用 maps 自动去重
    AllFieldsMap = lists:foldl(fun(Prop, Acc) ->
        case dgiot_tdengine_field:get_field(Prop) of
            pass -> Acc;
            {Name, Type} -> Acc#{Name => Type}
        end
    end, #{}, Properties ++ Tags),
    
    % 转换为列表，并过滤掉 devaddr
    lists:filter(fun({Name, _}) -> Name =/= <<"devaddr">> end, maps:to_list(AllFieldsMap)).

%% 逐字段创建超级表（5参数版本）
create_stable_by_columns(ChannelId, ProductId, Database, TableName, AllColumns) ->
    ?LOG(info, ">>> create_stable_by_columns: Database=~s, Table=~s, total columns=~p", 
         [Database, TableName, length(AllColumns)]),
    {FinalColumns, _FreshFlag} = case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, LatestProduct} ->
            ?LOG(info, ">>> Fetched latest product from Parse, extracting columns"),
            NewCols = extract_columns(LatestProduct),
            if length(NewCols) =/= length(AllColumns) ->
                ?LOG(info, ">>> Cache mismatch: passed ~p columns, fresh ~p columns, using fresh", 
                     [length(AllColumns), length(NewCols)]),
                {NewCols, true};
               true ->
                {AllColumns, false}
            end;
        {error, Reason} ->
            ?LOG(error, ">>> Failed to fetch latest product from Parse: ~p, using passed columns", [Reason]),
            {AllColumns, false}
    end,
    UniqueColumns = lists:foldl(fun({Name, Type}, Acc) ->
        Acc#{Name => Type}
    end, #{}, FinalColumns),
    UniqueList0 = maps:to_list(UniqueColumns),
    UniqueList = lists:filter(fun({Name, _}) -> Name =/= <<"createdat">> end, UniqueList0),
    ColNames = [Name || {Name,_} <- UniqueList],
    ?LOG(info, ">>> UniqueList column names (count=~p): ~p", [length(ColNames), ColNames]),
    ContainsCreatedat = lists:keymember(<<"createdat">>, 1, UniqueList0),
    ?LOG(info, ">>> UniqueList originally contained createdat? ~p", [ContainsCreatedat]),
    if length(UniqueList) =/= length(FinalColumns) ->
        ?LOG(info, ">>> Duplicate columns detected and removed, original ~p, unique ~p", [length(FinalColumns), length(UniqueList)]);
       true -> ok
    end,
    case table_exists(ChannelId, Database, TableName) of
        true ->
            ExistingColumns = get_existing_columns(ChannelId, Database, TableName),
            NewColumnsToAdd = [Col || {Name, _}=Col <- UniqueList, not lists:keymember(Name, 1, ExistingColumns)],
            ?LOG(debug, ">>> Table exists, existing: ~p, new: ~p", [length(ExistingColumns), length(NewColumnsToAdd)]),
            add_columns(ChannelId, Database, TableName, NewColumnsToAdd);
        false ->
            ?LOG(info, ">>> Table does not exist, creating base table..."),
            % 选择第一列作为基表的附加列
            [FirstColTuple | _] = UniqueList,
            BaseColumnsDef = list_columns_def([FirstColTuple]),
            BaseSql = <<"CREATE STABLE IF NOT EXISTS ", TableName/binary,
                        " (createdat TIMESTAMP, ", BaseColumnsDef/binary, ") TAGS (devaddr NCHAR(64));">>,
            case dgiot_tdengine:batch_sql(ChannelId, Database, BaseSql) of
                {ok, _Result} ->
                    ?LOG(info, ">>> Base table created successfully, now adding remaining columns..."),
                    % 添加剩余列（不包括第一列）
                    RemainingColumns = lists:filter(fun({Name, _}) -> Name =/= element(1, FirstColTuple) end, UniqueList),
                    add_columns(ChannelId, Database, TableName, RemainingColumns);
                {error, _Reason} ->
                    ?LOG(error, ">>> Base table creation failed"),
                    {error, _Reason}
            end
    end.

list_columns_def(Columns) ->
    lists:foldr(fun({Name, #{<<"type">> := Type}}, Acc) ->
        case Acc of
            <<>> -> <<Name/binary, " ", Type/binary>>;
            _ -> <<Name/binary, " ", Type/binary, ", ", Acc/binary>>
        end
    end, <<>>, Columns).

add_columns(_ChannelId, _Database, _TableName, []) ->
    ?LOG(info, ">>> All columns added, no more columns to process"),
    ok;
add_columns(ChannelId, Database, TableName, [{Name, #{<<"type">> := Type}} | Rest]) ->
    ?LOG(debug, ">>> Adding column ~s (type ~s) to table ~s", [Name, Type, TableName]),
    AlterSql = <<"ALTER STABLE ", TableName/binary, " ADD COLUMN ", Name/binary, " ", Type/binary, ";">>,
    case dgiot_tdengine:batch_sql(ChannelId, Database, AlterSql) of
        {ok, Result} ->
            ?LOG(debug, ">>> Column ~s added successfully, result: ~p", [Name, Result]),
            add_columns(ChannelId, Database, TableName, Rest);
        {error, #{<<"code">> := Code}} when Code == 904; Code == 875 ->
            ?LOG(debug, ">>> Column ~s already exists (code ~p), skipping", [Name, Code]),
            add_columns(ChannelId, Database, TableName, Rest);
        {error, Reason} ->
            ?LOG(error, ">>> Failed to add column ~s: ~p", [Name, Reason]),
            add_columns(ChannelId, Database, TableName, Rest)
    end.

%% 检查表是否存在（通过 SHOW STABLES LIKE）
table_exists(ChannelId, Database, TableName) ->
    Sql = <<"SHOW STABLES LIKE '", TableName/binary, "';">>,
    case dgiot_tdengine:batch_sql(ChannelId, Database, Sql) of
        {ok, #{<<"results">> := [_|_]}} ->
            true;
        {ok, _} ->
            false;
        {error, #{<<"code">> := Code}} when Code == 1850 ->
            %% Query memory exhausted - 临时故障，不应触发建表，等下一次数据上报时重试
            ?LOG(warning, ">>> TDengine内存不足(code=1850)，跳过表检查: ~s", [TableName]),
            {error, memory_exhausted};
        {error, Reason} ->
            ?LOG(warning, ">>> Error checking table existence: ~p", [Reason]),
            {error, Reason}
    end.

%% 获取现有列名（通过 DESCRIBE，TDengine 3.x 返回的键是小写）
get_existing_columns(ChannelId, Database, TableName) ->
    ?LOG(debug, ">>> Fetching existing columns for ~s.~s", [Database, TableName]),
    Sql = <<"DESCRIBE ", TableName/binary, ";">>,
    case dgiot_tdengine:batch_sql(ChannelId, Database, Sql) of
        {ok, #{<<"results">> := Rows}} ->
            Cols = [maps:get(<<"field">>, Row) || Row <- Rows, maps:get(<<"note">>, Row, <<"">>) =/= <<"TAG">>],
            ?LOG(debug, ">>> Existing columns: ~p", [Cols]),
            Cols;
        {error, Reason} ->
            ?LOG(error, ">>> Failed to fetch columns: ~p", [Reason]),
            []
    end.

%% 以下为原有函数（未修改，仅保持原样）
=======

%% TDengine参数限制与保留关键字
%% https://www.taosdata.com/docs/cn/v2.0/administrator#keywords
>>>>>>> origin/dgaiot-plugins
get_schema(_ChannelId, Schema) ->
    case maps:get(<<"thing">>, Schema, <<>>) of
        <<>> ->
            ignore;
        Thing ->
            {Columns, Tags} = get_field_tag(Thing),
            case length(Columns) of
                0 ->
                    ignore;
                _ ->
                    #{<<"fields">> => Columns, <<"tags">> => Tags}
            end
    end.

get_field_tag(Thing) ->
    Properties = maps:get(<<"properties">>, Thing, []),
    Tags = maps:get(<<"tags">>, Thing, []),
    Columns =
        lists:foldl(fun(Property, Acc) ->
            case dgiot_tdengine_field:get_field(Property) of
                pass ->
                    Acc;
                V ->
                    Acc ++ [V]
            end
                    end, [], Properties),
    NewTags =
        lists:foldl(fun(Tag, Acc) ->
            case dgiot_tdengine_field:get_field(Tag) of
                pass ->
                    Acc;
                V ->
                    Acc ++ [V]
            end
                    end, [], Tags),
    NewTags1 =
        case proplists:get_value(<<"devaddr">>, NewTags) of
            undefined ->
                NewTags ++ [{<<"devaddr">>, #{<<"type">> => <<"NCHAR(50)">>}}];
            _ ->
                NewTags
        end,
    {lists:flatten(Columns), lists:flatten(dgiot_utils:unique_1(NewTags1))}.

create_database(Query) ->
    DataBase = maps:get(<<"database">>, Query),
    KeepTime = format_keep(Query),
    <<"CREATE DATABASE IF NOT EXISTS ", DataBase/binary, " KEEP ", KeepTime/binary>>.

format_keep(Query) ->
    Keep = maps:get(<<"keep">>, Query, 10),
    dgiot_utils:to_binary(Keep).

<<<<<<< HEAD
=======

>>>>>>> origin/dgaiot-plugins
create_table(#{<<"tableName">> := TableName, <<"using">> := STbName, <<"tags">> := Tags} = _Query, #{<<"channel">> := Channel} = _Context) ->
    TagFields =
        list_to_binary(dgiot_utils:join(",", lists:foldr(
            fun({TagName, #{<<"type">> := TType}}, Acc) ->
                [<<TagName/binary, " ", TType/binary>> | Acc]
            end, [], Tags))),
    <<"_", ProductId/binary>> = TableName,
    DataBase = dgiot_tdengine:get_database(Channel, ProductId),
    <<"CREATE TABLE IF NOT EXISTS ", DataBase/binary, TableName/binary, " USING ", STbName/binary, " TAGS (", TagFields/binary, ");">>;

create_table(#{<<"tableName">> := TableName, <<"fields">> := Fields0} = Query, #{<<"channel">> := Channel} = _Context) ->
    <<"_", ProductId/binary>> = TableName,
    DataBase = dgiot_tdengine:get_database(Channel, ProductId),
    Fields =
        list_to_binary(dgiot_utils:join(",", ["createdat TIMESTAMP"] ++ lists:foldr(
            fun({FieldName, #{<<"type">> := Type}}, Acc) ->
                [<<FieldName/binary, " ", Type/binary>> | Acc]
            end, [], Fields0))),
    TagFields =
        list_to_binary(dgiot_utils:join(",", lists:foldr(
            fun({TagName, #{<<"type">> := TType}}, Acc) ->
                [<<TagName/binary, " ", TType/binary>> | Acc]
            end, [], maps:get(<<"tags">>, Query, [])))),
    case TagFields of
        <<>> ->
            <<"CREATE TABLE IF NOT EXISTS ", DataBase/binary, TableName/binary, " (", Fields/binary, ");">>;
        _ ->
            <<"CREATE TABLE IF NOT EXISTS ", DataBase/binary, TableName/binary, " (", Fields/binary, ") TAGS (", TagFields/binary, ");">>
    end.

alter_table(#{<<"tableName">> := TableName}, #{<<"channel">> := Channel} = Context) ->
    <<"_", ProductId/binary>> = TableName,
    Database = dgiot_tdengine:get_database(Channel, ProductId),
    Sql1 = <<"DESCRIBE ", Database/binary, TableName/binary, ";">>,
    case dgiot_tdengine_pool:run_sql(Context, execute_query, Sql1) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            TdColumn =
                lists:foldl(fun
                                (#{<<"Field">> := Identifier, <<"Type">> := Type}, Acc) ->
                                    Acc#{Identifier => list_to_binary(string:to_lower(binary_to_list(Type)))};
                                (#{<<"field">> := Identifier, <<"type">> := Type}, Acc) ->
                                    Acc#{Identifier => list_to_binary(string:to_lower(binary_to_list(Type)))};
                                (_, Acc) ->
                                    Acc
                            end, #{}, Results),
            AddSqls = dgiot_tdengine_schema:get_addSql(ProductId, TdColumn, Database, TableName),
            lists:map(fun(AddSql) ->
                dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, AddSql)
                      end, AddSqls),
            case dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql1) of
                {ok, #{<<"results">> := Results2}} ->
                    dgiot_tdengine:save_fields(ProductId, Results2);
                _ ->
                    pass
            end;
        _ ->
            pass
    end.

<<<<<<< HEAD
=======
%% ALTER TABLE  _24b9b4bc50._5392ccb3d7 drop COLUMN status;
>>>>>>> origin/dgaiot-plugins
get_addSql(ProductId, TdColumn, Database, TableName) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing}} ->
            Tags = maps:get(<<"tags">>, Thing, []),
            lists:foldl(fun(Prop, Acc) ->
                case Prop of
                    #{<<"dataType">> := #{<<"type">> := Type} = DataType, <<"identifier">> := Identifier, <<"moduleType">> := ModuleType, <<"isstorage">> := true} ->
<<<<<<< HEAD
                        LowerIdentifier = dgiot_tdengine_field:sanitize_name(Identifier),
=======
                        LowerIdentifier = list_to_binary(string:to_lower(binary_to_list(Identifier))),
>>>>>>> origin/dgaiot-plugins
                        LowerType = dgiot_tdengine_field:get_field_type(Type),
                        FieldType = get_fieldtype(ModuleType),
                        case maps:find(LowerIdentifier, TdColumn) of
                            error ->
                                Acc ++ [dgiot_tdengine_field:add_field(DataType, Database, TableName, LowerIdentifier, FieldType)];
                            {ok, LowerType} ->
                                Acc;
                            _ ->
<<<<<<< HEAD
=======
                                %% 类型改变, 先删除列, 再重新添加
>>>>>>> origin/dgaiot-plugins
                                DROP = <<"ALTER TABLE ", Database/binary, TableName/binary, " DROP ", FieldType/binary, " ", LowerIdentifier/binary, ";">>,
                                ADD = dgiot_tdengine_field:add_field(DataType, Database, TableName, LowerIdentifier, FieldType),
                                Acc ++ [DROP, ADD]
                        end;
                    _ ->
                        Acc
                end
                        end, [], Props ++ Tags);
        _ ->
            []
    end.

get_fieldtype(<<"tags">>) ->
    <<"TAG">>;
get_fieldtype(_) ->
<<<<<<< HEAD
    <<"COLUMN">>.
=======
    <<"COLUMN">>.

>>>>>>> origin/dgaiot-plugins
