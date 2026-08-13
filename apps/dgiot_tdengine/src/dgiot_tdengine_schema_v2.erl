%% dgiot_tdengine_schema v2 — Batch ALTER TABLE
%% Fix #3: 合并多列变更为单条 ALTER TABLE ADD COLUMN (a,b,c)

%% ——— alter_table v2: batch instead of per-column ———
alter_table(#{<<"tableName">> := TableName}, #{<<"channel">> := Channel} = Context) ->
    <<"_", ProductId/binary>> = TableName,
    Database = dgiot_tdengine:get_database(Channel, ProductId),
    Sql1 = <<"DESCRIBE ", Database/binary, TableName/binary, ";">>,

    case dgiot_tdengine_pool:run_sql(Context, execute_query, Sql1) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            TdColumn = build_column_map(Results),

            %% v2: 收集所有需要 ADD 的列, 合并为一条 SQL
            {AddFields, DropFields} = collect_changes(ProductId, TdColumn, Database, TableName),

            %% 先批量 DROP
            lists:foreach(fun(DropSql) ->
                dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, DropSql)
            end, DropFields),

            %% 再批量 ADD (合并为一条!)
            case AddFields of
                [] -> ok;
                [_|_] ->
                    BatchAdd = iolist_to_binary([
                        <<"ALTER TABLE ">>, Database/binary, TableName/binary,
                        <<" ADD COLUMN (">>,
                        lists:join(<<", ">>, AddFields),
                        <<");">>
                    ]),
                    dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_update, BatchAdd)
            end,

            %% 刷新缓存
            case dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql1) of
                {ok, #{<<"results">> := Results2}} ->
                    dgiot_tdengine:save_fields(ProductId, Results2);
                _ -> pass
            end;
        _ -> pass
    end.

%% ——— 收集变更 (不立即执行) ———
collect_changes(ProductId, TdColumn, Database, TableName) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing}} ->
            Tags = maps:get(<<"tags">>, Thing, []),
            lists:foldl(fun(Prop, {AddAcc, DropAcc}) ->
                case Prop of
                    #{<<"dataType">> := #{<<"type">> := Type},
                      <<"identifier">> := Identifier,
                      <<"isstorage">> := true} ->
                        LowerId = list_to_binary(string:to_lower(binary_to_list(Identifier))),
                        LowerType = dgiot_tdengine_field:get_field_type(Type),
                        case maps:find(LowerId, TdColumn) of
                            error ->
                                %% 新列 → 收集用于批量ADD
                                FieldDef = iolist_to_binary([LowerId, <<" ">>, LowerType]),
                                {[FieldDef|AddAcc], DropAcc};
                            {ok, LowerType} ->
                                {AddAcc, DropAcc};  %% 匹配, 不变
                            _ ->
                                %% 类型变了 → DROP + ADD
                                Drop = <<"ALTER TABLE ", Database/binary, TableName/binary,
                                         " DROP COLUMN ", LowerId/binary, ";">>,
                                FieldDef = iolist_to_binary([LowerId, <<" ">>, LowerType]),
                                {[FieldDef|AddAcc], [Drop|DropAcc]}
                        end;
                    _ -> {AddAcc, DropAcc}
                end
            end, {[], []}, Props ++ Tags);
        _ -> {[], []}
    end.

build_column_map(Results) ->
    lists:foldl(fun
        (#{<<"Field">> := F, <<"Type">> := T}, Acc) ->
            Acc#{F => list_to_binary(string:to_lower(binary_to_list(T)))};
        (#{<<"field">> := F, <<"type">> := T}, Acc) ->
            Acc#{F => list_to_binary(string:to_lower(binary_to_list(T)))};
        (_, Acc) -> Acc
    end, #{}, Results).
