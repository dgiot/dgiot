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

-module(dgiot_tdengine_field).
-author("jonliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([add_field/5, get_field/1, check_fields/2, check_fields/3, get_time/2, check_value/3, get_field_type/1, check_validate/2]).

%% 新增导出：sanitize_name/1（若需外部调用）
-export([sanitize_name/1]).

%% 字段名清洗：转为小写，非法字符替换为下划线，若首字符为数字则加前缀 'f_'
sanitize_name(Identifier) ->
    Lower = string:lowercase(Identifier),
    Clean = re:replace(Lower, "[^a-z0-9_]", "_", [global, {return, binary}]),
    case Clean of
        <<C, _/binary>> when C >= $0, C =< $9 ->
            <<"f_", Clean/binary>>;
        _ ->
            Clean
    end.

add_field(#{<<"type">> := <<"enum">>}, Database, TableName, LowerIdentifier, FieldType) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " INT;">>;
add_field(#{<<"type">> := <<"file">>} = Spec, Database, TableName, LowerIdentifier, FieldType) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 999)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"text">>} = Spec, Database, TableName, LowerIdentifier, FieldType) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 999)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"url">>} = Spec, Database, TableName, LowerIdentifier, FieldType) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 999)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " NCHAR((", Size/binary, ");">>;
add_field(#{<<"type">> := <<"geopoint">>} = Spec, Database, TableName, LowerIdentifier, FieldType) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 999)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"image">>}, Database, TableName, LowerIdentifier, FieldType) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " BIGINT;">>;
add_field(#{<<"type">> := <<"date">>}, Database, TableName, LowerIdentifier, FieldType) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " TIMESTAMP;">>;
add_field(#{<<"type">> := <<"long">>}, Database, TableName, LowerIdentifier, FieldType) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " BIGINT;">>;
add_field(#{<<"type">> := Type}, Database, TableName, LowerIdentifier, FieldType) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD ", FieldType/binary, " ", LowerIdentifier/binary, " ", Type/binary, ";">>.

get_field(#{<<"isstorage">> := false}) ->
    pass;
get_field(#{<<"isstorage">> := true} = Property) ->
    get_field_(Property);
get_field(#{<<"isstorage">> := Isstorage} = Property) when Isstorage > 0 ->
    get_field_(Property);
get_field(#{<<"isshow">> := true} = Property) ->
    get_field_(Property);
get_field(_) ->
    pass.

%% 所有字段名都经过 sanitize_name 处理
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"int">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"INT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"image">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"BIGINT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"long">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"BIGINT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"float">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"FLOAT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"date">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"TIMESTAMP">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"bool">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"BOOL">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"double">>}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"DOUBLE">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"string">>} = Spec}) ->
    Size = maps:get(<<"size">>, Spec, 64),
    SizeBin = integer_to_binary(min(Size, 999)),
    {sanitize_name(Field), #{<<"type">> => <<"NCHAR(", SizeBin/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"text">>} = Spec}) ->
    Size = maps:get(<<"size">>, Spec, 64),
    SizeBin = integer_to_binary(min(Size, 999)),
    {sanitize_name(Field), #{<<"type">> => <<"NCHAR(", SizeBin/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"geopoint">>} = Spec}) ->
    Size = maps:get(<<"size">>, Spec, 64),
    SizeBin = integer_to_binary(min(Size, 999)),
    {sanitize_name(Field), #{<<"type">> => <<"NCHAR(", SizeBin/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"enum">>, <<"specs">> := _Specs}}) ->
    {sanitize_name(Field), #{<<"type">> => <<"INT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"struct">>, <<"specs">> := SubFields}}) ->
    [get_field(SubField#{<<"identifier">> => ?Struct(Field, Field1)}) || #{<<"identifier">> := Field1} = SubField <- SubFields];
get_field_(_) ->
    pass.

check_value(Value, ProductId, Field) ->
    case dgiot_product:get_product_identifier(ProductId, Field) of
        not_find ->
            get_type_value(<<>>, Value, #{});
        #{<<"dataType">> := #{<<"type">> := Type} = DataType} ->
            Specs = maps:get(<<"specs">>, DataType, #{}),
            Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
            NewValue = get_type_value(Type1, Value, Specs),
            NewValue
    end.

check_fields(Data, #{<<"properties">> := Props}) ->
    check_fields(Data, Props);
check_fields(Data, Props) -> check_fields(Data, Props, #{}).
check_fields(Data, Props, Acc) when Data == []; Props == [] -> Acc;
check_fields(Data, [#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType} = Prop | Other], Acc) ->
    LowerField = sanitize_name(Field),
    case check_field(Data, Prop) of
        undefined ->
            check_fields(Data, Other, Acc);
        Value ->
            case list_to_binary(string:to_upper(binary_to_list(Type))) of
                <<"STRUCT">> ->
                    #{<<"specs">> := SubFields} = DataType,
                    Acc2 = lists:foldl(
                        fun(#{<<"identifier">> := Field1} = SubField, Acc1) ->
                            case check_field(Value, SubField) of
                                undefined ->
                                    Acc1;
                                Value1 ->
                                    LowerField1 = sanitize_name(Field1),
                                    Acc1#{?Struct(LowerField, LowerField1) => Value1}
                            end
                        end, Acc, SubFields),
                    check_fields(Data, Other, Acc2);
                _ ->
                    check_fields(Data, Other, Acc#{LowerField => Value})
            end
    end.

check_field(Data, #{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType}) ->
    Specs = maps:get(<<"specs">>, DataType, #{}),
    case maps:get(Field, Data, undefined) of
        undefined ->
            undefined;
        Value ->
            Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
            NewValue = get_type_value(Type1, Value, Specs),
            NewValue
    end;
check_field(_, _) ->
    undefined.

check_validate({_, text}, _) ->
    true;
check_validate(null, _) ->
    true;
check_validate(Value, #{<<"max">> := Max, <<"min">> := Min}) when is_integer(Max), is_integer(Min) ->
    Value =< Max andalso Value >= Min;
check_validate(Value, #{<<"max">> := Max}) when is_integer(Max) ->
    Value =< Max;
check_validate(Value, #{<<"min">> := Min}) when is_integer(Min) ->
    Value >= Min;
check_validate(_, _) ->
    true.

get_time(V, Interval) ->
    NewV =
        case binary:split(V, <<$T>>, [global, trim]) of
            [_, _] ->
                V;
            _ ->
                case binary:split(V, <<$.>>, [global, trim]) of
                    [NewV1, _] ->
                        NewV1;
                    [<<T:10/binary, _/binary>> | _] ->
                        T;
                    _ ->
                        V
                end
        end,
    Size = erlang:size(Interval) - 1,
    <<_:Size/binary, Type/binary>> = Interval,
    case Type of
        <<"a">> ->
            NewV;
        <<"s">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"DD HH:NN:SS">>);
        <<"m">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"MM-DD HH:NN">>);
        <<"h">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"MM-DD HH">>);
        <<"d">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"YY-MM-DD">>);
        <<"y">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"YY">>);
        <<"H">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"HH">>);
        <<"D">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"DD">>);
        <<"M">> ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"MM">>);
        _ ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"YY-MM-DD HH:NN:SS">>)
    end.

get_type_value(_, {Value, text}, _) ->
    {Value, text};
get_type_value(_, null, _) ->
    null;
get_type_value(Type, Value, _Specs) when Type == <<"INT">>; Type == <<"DATE">>; Type == <<"SHORT">>; Type == <<"LONG">>; Type == <<"ENUM">>, is_list(Value) ->
    round(dgiot_utils:to_int(Value));
get_type_value(Type, Value, _Specs) when Type == <<"INT">>; Type == <<"DATE">>, is_float(Value) ->
    round(Value);
get_type_value(Type, Value, _Specs) when Type == <<"INT">>; Type == <<"DATE">> ->
    Value;
get_type_value(Type, Value, Specs) when Type == <<"FLOAT">>; Type == <<"DOUBLE">> ->
    Precision = maps:get(<<"precision">>, Specs, 3),
    case size(dgiot_utils:to_binary(Value)) of
        0 ->
            0;
        _ ->
            dgiot_utils:to_float(Value, Precision)
    end;
get_type_value(<<"BOOL">>, Value, _Specs) ->
    Value;
get_type_value(<<"TEXT">>, Value, _Specs) ->
    {unicode:characters_to_binary(unicode:characters_to_list((dgiot_utils:to_binary(Value)))), text};
get_type_value(<<"GEOPOINT">>, Value, _Specs) ->
    {unicode:characters_to_binary(unicode:characters_to_list((Value))), text};
get_type_value(<<"STRUCT">>, Value, _Specs) ->
    Value;
get_type_value(<<"IMAGE">>, Value, _Specs) ->
    round(dgiot_utils:to_int(Value));
get_type_value(_, Value, _Specs) ->
    Value.

get_field_type(<<"enum">>) ->
    <<"int">>;
get_field_type(<<"file">>) ->
    <<"nchar">>;
get_field_type(<<"text">>) ->
    <<"nchar">>;
get_field_type(<<"url">>) ->
    <<"nchar">>;
get_field_type(<<"geopoint">>) ->
    <<"nchar">>;
get_field_type(<<"image">>) ->
    <<"bigint;">>;
get_field_type(<<"date">>) ->
    <<"timestamp">>;
get_field_type(<<"long">>) ->
    <<"bigint">>;
get_field_type(Type) ->
    list_to_binary(string:to_lower(binary_to_list(Type))).