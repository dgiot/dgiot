-module(dgiot_iotdb).
-include("dgiot_iotdb.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([transaction/2, get_database/2, create_database/2, create_schemas/2,
    create_object/3, query_object/3, query_object/4, batch/2, get_channel/1, get_fields/1]).
-export([format_data/4, insert_records/2, batch_sql/2, get_device_path/3]).

transaction(ChannelId, Fun) when is_function(Fun, 1) ->
    case dgiot_data:get({?TYPE, ChannelId, config}) of
        Config when is_map(Config) -> Fun(Config);
        _ -> {error, channel_not_found}
    end.

get_database(ChannelId, ProductId) ->
    Config = channel_config(ChannelId),
    Root = maps:get(<<"root">>, Config, ?DEFAULT_ROOT),
    DatabaseId = case maps:get(<<"db">>, Config, <<"ProductId">>) of
        <<"ChannelId">> -> ChannelId;
        _ -> ProductId
    end,
    dgiot_iotdb_select:database(Root, DatabaseId).

get_device_path(ChannelId, ProductId, DeviceId) ->
    Database = get_database(ChannelId, ProductId),
    dgiot_iotdb_select:device_path(Database, ProductId, DeviceId).

create_database(ChannelId, Database) ->
    transaction(ChannelId, fun(Config) ->
        create_database_with_context(Config, Database)
    end).

create_schemas(ChannelId, Schema) when is_map(Schema) ->
    transaction(ChannelId, fun(Config) ->
        case schema_target(ChannelId, Config, Schema) of
            {ok, Device, Database, Fields} ->
                create_schemas_with_context(Config, Device, Database, Fields);
            Error -> Error
        end
    end);
create_schemas(_ChannelId, _Schema) ->
    {error, invalid_schema}.

create_object(ChannelId, Device, Data) when is_binary(Device), is_map(Data) ->
    insert_records(ChannelId, [record_for_device(Device, Data)]);
create_object(_ChannelId, _Device, _Data) ->
    {error, invalid_record}.

query_object(ChannelId, Device, Query) when is_binary(Device), is_map(Query) ->
    query_object(ChannelId, maps:get(<<"productId">>, Query, <<>>), Device, Query);
query_object(_ChannelId, _Device, _Query) ->
    {error, invalid_query}.

query_object(ChannelId, ProductId, Device, Query) when is_binary(Device), is_map(Query) ->
    case dgiot_iotdb_select:valid_path(Device) of
        true -> transaction(ChannelId, fun(Config) ->
            Sql = dgiot_iotdb_select:select(Device, ProductId, Query),
            dgiot_iotdb_http:query(Config, Sql)
        end);
        false -> {error, invalid_device_path}
    end;
query_object(_ChannelId, _ProductId, _Device, _Query) ->
    {error, invalid_query}.

batch(ChannelId, Records) ->
    insert_records(ChannelId, Records).

insert_records(ChannelId, Payload) when is_map(Payload) ->
    case maps:is_key(<<"devices">>, Payload) of
        true ->
            transaction(ChannelId, fun(Config) ->
                dgiot_iotdb_http:insert_records(Config, Payload)
            end);
        false -> insert_records(ChannelId, [Payload])
    end;
insert_records(ChannelId, Records) when is_list(Records) ->
    transaction(ChannelId, fun(Config) ->
        case normalize_records(ChannelId, Records, []) of
            {ok, []} -> {ok, #{<<"code">> => 200, <<"message">> => <<"no records">>}};
            {ok, Normalized} ->
                case ensure_record_schemas(ChannelId, Config, Normalized) of
                    ok ->
                        dgiot_iotdb_http:insert_records(Config, records_payload(Normalized));
                    Error -> Error
                end;
            Error -> Error
        end
    end);
insert_records(_ChannelId, _Records) ->
    {error, invalid_records}.

batch_sql(ChannelId, Sql) when is_binary(Sql) ->
    transaction(ChannelId, fun(Config) -> dgiot_iotdb_http:non_query(Config, Sql) end);
batch_sql(_ChannelId, _Sql) ->
    {error, invalid_sql}.

get_channel(ProductId) ->
    case dgiot_data:lookup({ProductId, ?TYPE}) of
        {ok, ChannelId} -> {ok, ChannelId};
        {error, not_find} -> {error, not_find_iotdb}
    end.

get_fields(ProductId) when is_binary(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := Thing}} -> dgiot_iotdb_field:get_fields(Thing);
        _ -> []
    end;
get_fields(#{<<"thing">> := Thing}) ->
    dgiot_iotdb_field:get_fields(Thing);
get_fields(Thing) when is_map(Thing) ->
    dgiot_iotdb_field:get_fields(Thing);
get_fields(_) ->
    [].

format_data(ChannelId, ProductId, DevAddr, Data) when is_map(Data) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, dgiot_utils:to_binary(DevAddr)),
    Device = get_device_path(ChannelId, ProductId, DeviceId),
    Fields = get_fields(ProductId),
    record_for_device(Device, Data, Fields);
format_data(_ChannelId, _ProductId, _DevAddr, _Data) ->
    {error, invalid_record}.

channel_config(ChannelId) ->
    case dgiot_data:get({?TYPE, ChannelId, config}) of
        Config when is_map(Config) -> Config;
        _ -> #{}
    end.

create_database_with_context(Config, Database) ->
    Root = maps:get(<<"root">>, Config, ?DEFAULT_ROOT),
    DatabasePath = normalize_database(Root, Database),
    dgiot_iotdb_http:non_query(Config, dgiot_iotdb_select:create_database(DatabasePath)).

normalize_database(Root, Database) when is_binary(Database) ->
    SafeRoot = safe_root(Root),
    Prefix = <<SafeRoot/binary, ".">>,
    case {binary:match(Database, Prefix), re:run(Database, <<"^root(?:\\.[A-Za-z_][A-Za-z0-9_]*)+$">>, [{capture, none}])} of
        {{0, _}, match} -> Database;
        _ -> dgiot_iotdb_select:database(SafeRoot, Database)
    end;
normalize_database(Root, Database) ->
    dgiot_iotdb_select:database(Root, dgiot_utils:to_binary(Database)).

safe_root(Root) ->
    case re:run(Root, <<"^root(?:\\.[A-Za-z_][A-Za-z0-9_]*)*$">>, [{capture, none}]) of
        match -> Root;
        nomatch -> ?DEFAULT_ROOT
    end.

schema_target(ChannelId, Config, Schema) ->
    Fields = case maps:find(<<"fields">>, Schema) of
        {ok, FieldList} -> normalize_fields(FieldList);
        error ->
            case maps:find(<<"thing">>, Schema) of
                {ok, Thing} -> dgiot_iotdb_field:get_fields(Thing);
                error -> []
            end
    end,
    case maps:find(<<"device">>, Schema) of
        {ok, Device} when is_binary(Device) ->
            Database = database_for_schema(ChannelId, Config, Schema, Device),
            {ok, Device, Database, Fields};
        _ ->
            case {maps:find(<<"productId">>, Schema), maps:find(<<"deviceId">>, Schema)} of
                {{ok, ProductId}, {ok, DeviceId}} ->
                    Database = case maps:find(<<"database">>, Schema) of
                        {ok, Db} -> normalize_database(maps:get(<<"root">>, Config, ?DEFAULT_ROOT), Db);
                        error -> get_database(ChannelId, ProductId)
                    end,
                    Device = dgiot_iotdb_select:device_path(Database, ProductId, DeviceId),
                    {ok, Device, Database, Fields};
                _ -> {error, missing_device_path}
            end
    end.

database_for_schema(ChannelId, Config, Schema, Device) ->
    case maps:find(<<"database">>, Schema) of
        {ok, Database} -> normalize_database(maps:get(<<"root">>, Config, ?DEFAULT_ROOT), Database);
        error ->
            case dgiot_iotdb_select:database_from_device(Device) of
                {ok, Database} -> Database;
                error -> get_database(ChannelId, maps:get(<<"productId">>, Schema, <<"unknown">>))
            end
    end.

create_schemas_with_context(Config, Device, Database, Fields0) ->
    Fields = normalize_fields(Fields0),
    case dgiot_iotdb_select:valid_path(Device) of
        false -> {error, invalid_device_path};
        true ->
            case dgiot_iotdb_select:database_from_device(Device) of
                {ok, _} -> create_schemas_with_valid_path(Config, Device, Database, Fields);
                error -> {error, invalid_device_path}
            end
    end.

create_schemas_with_valid_path(Config, Device, Database, Fields) ->
    case dgiot_iotdb_http:non_query(Config, dgiot_iotdb_select:create_database(Database)) of
        {ok, _} -> create_timeseries(Config, Device, Fields, []);
        Error -> Error
    end.

create_timeseries(_Config, _Device, [], Results) ->
    {ok, lists:reverse(Results)};
create_timeseries(Config, Device, [{Field, Type} | Rest], Results) ->
    Sql = dgiot_iotdb_select:create_timeseries(Device, Field, Type),
    case dgiot_iotdb_http:non_query(Config, Sql) of
        {ok, Result} -> create_timeseries(Config, Device, Rest, [Result | Results]);
        Error -> Error
    end.

normalize_fields(Fields) when is_map(Fields) ->
    normalize_fields(maps:to_list(Fields));
normalize_fields(Fields) when is_list(Fields) ->
    lists:filtermap(fun
        ({Field, Type}) ->
            normalize_field(Field, Type);
        (#{<<"identifier">> := Field, <<"dataType">> := Type} = Spec) ->
            case maps:get(<<"isstorage">>, Spec, true) of
                false -> false;
                0 -> false;
                _ -> normalize_field(Field, Type)
            end;
        (Field) when is_binary(Field) -> normalize_field(Field, dgiot_iotdb_field:infer_type(null));
        (_) -> false
    end, Fields);
normalize_fields(_) ->
    [].

normalize_field(Field0, Type0) ->
    Field = dgiot_utils:to_binary(Field0),
    case Field of
        <<>> -> false;
        <<"createdat">> -> false;
        <<"time">> -> false;
        _ -> {true, {Field, dgiot_iotdb_field:get_type(Type0)}}
    end.

record_for_device(Device, Data) ->
    record_for_device(Device, Data, []).

record_for_device(Device, Data, Fields0) ->
    FieldValues = case maps:find(<<"fields">>, Data) of
        {ok, NestedFields} when is_map(NestedFields) -> NestedFields;
        _ -> Data
    end,
    Declared = normalize_fields(Fields0),
    Fields = case Declared of
        [] -> inferred_fields(FieldValues);
        _ -> [Pair || {Field, _Type} = Pair <- Declared, maps:is_key(Field, FieldValues)]
    end,
    Values = maps:from_list([{Field, maps:get(Field, FieldValues)} || {Field, _Type} <- Fields]),
    Timestamp = dgiot_iotdb_field:timestamp_ms(maps:get(<<"createdat">>, Data, maps:get(<<"time">>, Data, now))),
    #{<<"device">> => Device, <<"timestamp">> => Timestamp,
        <<"fields">> => Values, <<"schema">> => Fields}.

inferred_fields(Data) ->
    lists:filtermap(fun({Field, Value}) ->
        case is_storage_value(Field) of
            true -> {true, {Field, dgiot_iotdb_field:infer_type(Value)}};
            false -> false
        end
    end, maps:to_list(Data)).

is_storage_value(<<>>) -> false;
is_storage_value(<<"createdat">>) -> false;
is_storage_value(<<"time">>) -> false;
is_storage_value(<<"timestamp">>) -> false;
is_storage_value(<<"addr">>) -> false;
is_storage_value(<<"devaddr">>) -> false;
is_storage_value(<<"dtuaddr">>) -> false;
is_storage_value(<<"product">>) -> false;
is_storage_value(<<"productId">>) -> false;
is_storage_value(<<"productid">>) -> false;
is_storage_value(<<"device">>) -> false;
is_storage_value(<<"deviceId">>) -> false;
is_storage_value(_) -> true.

normalize_records(_ChannelId, [], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_records(ChannelId, [Record | Rest], Acc) ->
    case normalize_record(ChannelId, Record) of
        {ok, Normalized} -> normalize_records(ChannelId, Rest, [Normalized | Acc]);
        Error -> Error
    end.

normalize_record(ChannelId, {ProductId, DevAddr, Data}) when is_map(Data) ->
    normalize_formatted(format_data(ChannelId, ProductId, DevAddr, Data));
normalize_record(ChannelId, #{<<"productId">> := ProductId, <<"devaddr">> := DevAddr, <<"data">> := Data}) when is_map(Data) ->
    normalize_formatted(format_data(ChannelId, ProductId, DevAddr, Data));
normalize_record(ChannelId, #{<<"product">> := ProductId, <<"addr">> := DevAddr} = Data) ->
    normalize_formatted(format_data(ChannelId, ProductId, DevAddr, Data));
normalize_record(_ChannelId, #{<<"device">> := Device} = Record) ->
    Data = maps:get(<<"fields">>, Record, #{}),
    Fields = maps:get(<<"schema">>, Record, []),
    {ok, (record_for_device(Device, Data, Fields))#{
        <<"timestamp">> => dgiot_iotdb_field:timestamp_ms(maps:get(<<"timestamp">>, Record, maps:get(<<"createdat">>, Record, now)))
    }};
normalize_record(_ChannelId, _Record) ->
    {error, invalid_record}.

normalize_formatted({error, _} = Error) -> Error;
normalize_formatted(Record) when is_map(Record) -> {ok, Record}.

ensure_record_schemas(_ChannelId, _Config, []) ->
    ok;
ensure_record_schemas(ChannelId, Config, [Record | Rest]) ->
    Device = maps:get(<<"device">>, Record),
    Fields = maps:get(<<"schema">>, Record, []),
    Database = database_from_device(Device),
    case ensure_database(ChannelId, Config, Database) of
        ok -> ensure_device_schemas(ChannelId, Config, Device, Database, Fields, Rest);
        Error -> Error
    end.

ensure_database(ChannelId, Config, Database) ->
    case dgiot_data:get({iotdb_database, ChannelId, Database}) of
        true -> ok;
        _ ->
            case dgiot_iotdb_http:non_query(Config, dgiot_iotdb_select:create_database(Database)) of
                {ok, _} ->
                    dgiot_data:insert({iotdb_database, ChannelId, Database}, true),
                    ok;
                Error -> Error
            end
    end.

ensure_device_schemas(ChannelId, Config, Device, Database, Fields, Rest) ->
    Existing = case dgiot_data:get({iotdb_schema, ChannelId, Device}) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end,
    NewFields = [{Field, Type} || {Field, Type} <- Fields, maps:get(Field, Existing, undefined) =/= Type],
    case NewFields of
        [] -> ensure_record_schemas(ChannelId, Config, Rest);
        _ ->
            case create_schemas_with_context(Config, Device, Database, NewFields) of
                {ok, _} ->
                    dgiot_data:insert({iotdb_schema, ChannelId, Device},
                        lists:foldl(fun({Field, Type}, Acc) -> Acc#{Field => Type} end, Existing, NewFields)),
                    ensure_record_schemas(ChannelId, Config, Rest);
                Error -> Error
            end
    end.

records_payload(Records) ->
    Rows = [record_row(Record) || Record <- Records],
    #{
        <<"timestamps">> => [Timestamp || {Timestamp, _, _, _, _} <- Rows],
        <<"measurements_list">> => [Measurements || {_, Measurements, _, _, _} <- Rows],
        <<"data_types_list">> => [Types || {_, _, Types, _, _} <- Rows],
        <<"values_list">> => [Values || {_, _, _, Values, _} <- Rows],
        <<"devices">> => [Device || {_, _, _, _, Device} <- Rows],
        <<"is_aligned">> => false
    }.

record_row(Record) ->
    Device = maps:get(<<"device">>, Record),
    Fields = maps:get(<<"fields">>, Record),
    Schema = [{Field, Type} || {Field, Type} <- maps:get(<<"schema">>, Record, []), maps:is_key(Field, Fields)],
    Measurements = [dgiot_iotdb_field:measurement(Field) || {Field, _Type} <- Schema],
    Types = [Type || {_Field, Type} <- Schema],
    Values = [dgiot_iotdb_field:encode_value(Type, maps:get(Field, Fields)) || {Field, Type} <- Schema],
    {maps:get(<<"timestamp">>, Record), Measurements, Types, Values, Device}.

database_from_device(Device) ->
    case dgiot_iotdb_select:database_from_device(Device) of
        {ok, Database} -> Database;
        error -> <<"root.dgiot">>
    end.
