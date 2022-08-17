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

-export([add_field/4, get_field/1, check_fields/2, check_fields/3, get_time/2]).

add_field(#{<<"type">> := <<"enum">>}, Database, TableName, LowerIdentifier) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " INT;">>;
add_field(#{<<"type">> := <<"file">>} = Spec, Database, TableName, LowerIdentifier) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"text">>} = Spec, Database, TableName, LowerIdentifier) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"url">>} = Spec, Database, TableName, LowerIdentifier) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR((", Size/binary, ");">>;
add_field(#{<<"type">> := <<"geopoint">>} = Spec, Database, TableName, LowerIdentifier) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " NCHAR(", Size/binary, ");">>;
add_field(#{<<"type">> := <<"image">>}, Database, TableName, LowerIdentifier) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " BIGINT;">>;
add_field(#{<<"type">> := <<"date">>}, Database, TableName, LowerIdentifier) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " TIMESTAMP;">>;
add_field(#{<<"type">> := <<"long">>}, Database, TableName, LowerIdentifier) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " BIGINT;">>;
add_field(#{<<"type">> := Type}, Database, TableName, LowerIdentifier) ->
    <<"ALTER TABLE ", Database/binary, TableName/binary, " ADD COLUMN ", LowerIdentifier/binary, " ", Type/binary, ";">>.

%%  https://www.taosdata.com/cn/documentation/taos-sql#data-type
%%  #	类型       	Bytes    说明
%%  1	TIMESTAMP   8        时间戳。缺省精度毫秒，可支持微秒。从格林威治时间 1970-01-01 00:00:00.000 (UTC/GMT) 开始，计时不能早于该时间。（从 2.0.18.0 版本开始，已经去除了这一时间范围限制）
%%  2	INT       	4        整型，范围 [-2^31+1, 2^31-1], -2^31 用作 NULL
%%  3	BIGINT      8        长整型，范围 [-2^63+1, 2^63-1], -2^63 用于 NULL
%%  4	FLOAT       4        浮点型，有效位数 6-7，范围 [-3.4E38, 3.4E38]
%%  5	DOUBLE      8        双精度浮点型，有效位数 15-16，范围 [-1.7E308, 1.7E308]
%%  6	BINARY      自定义    记录单字节字符串，建议只用于处理 ASCII 可见字符，中文等多字节字符需使用 nchar。理论上，最长可以有 16374 字节，但由于每行数据最多 16K 字节，实际上限一般小于理论值。binary 仅支持字符串输入，字符串两端需使用单引号引用。使用时须指定大小，如 binary(20) 定义了最长为 20 个单字节字符的字符串，每个字符占 1 byte 的存储空间，总共固定占用 20 bytes 的空间，此时如果用户字符串超出 20 字节将会报错。对于字符串内的单引号，可以用转义字符反斜线加单引号来表示，即 \’。
%%  7	SMALLINT    2        短整型， 范围 [-32767, 32767], -32768 用于 NULL
%%  8	TINYINT     1        单字节整型，范围 [-127, 127], -128 用于 NULL
%%  9	BOOL       	1        布尔型，{true, false}
%%  10	NCHAR       自定义    记录包含多字节字符在内的字符串，如中文字符。每个 nchar 字符占用 4 bytes 的存储空间。字符串两端使用单引号引用，字符串内的单引号需用转义字符 \’。nchar 使用时须指定字符串大小，类型为 nchar(10) 的列表示此列的字符串最多存储 10 个 nchar 字符，会固定占用 40 bytes 的空间。如果用户字符串长度超出声明长度，将会报错。
get_field(#{<<"isstorage">> := false}) ->
    pass;
get_field(#{<<"isstorage">> := true} = Property) ->
    get_field_(Property);
get_field(#{<<"isshow">> := true} = Property) ->
    get_field_(Property);
get_field(_) ->
    pass.
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"int">>}}) ->
    {Field, #{<<"type">> => <<"INT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"image">>}}) ->
    {Field, #{<<"type">> => <<"BIGINT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"long">>}}) ->
    {Field, #{<<"type">> => <<"BIGINT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"float">>}}) ->
    {Field, #{<<"type">> => <<"FLOAT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"date">>}}) ->
    {Field, #{<<"type">> => <<"TIMESTAMP">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"bool">>}}) ->
    {Field, #{<<"type">> => <<"BOOL">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"double">>}}) ->
    {Field, #{<<"type">> => <<"DOUBLE">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"string">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 10), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"text">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"geopoint">>} = Spec}) ->
    Size = integer_to_binary(min(maps:get(<<"size">>, Spec, 50), 200)),
    {Field, #{<<"type">> => <<"NCHAR(", Size/binary, ")">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"enum">>, <<"specs">> := _Specs}}) ->
%%    Size = integer_to_binary(maps:size(Specs)),
    {Field, #{<<"type">> => <<"INT">>}};
get_field_(#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := <<"struct">>, <<"specs">> := SubFields}}) ->
    [get_field(SubField#{<<"identifier">> => ?Struct(Field, Field1)}) || #{<<"identifier">> := Field1} = SubField <- SubFields].


check_fields(Data, #{<<"properties">> := Props}) ->
    check_fields(Data, Props);
check_fields(Data, Props) -> check_fields(Data, Props, #{}).
check_fields(Data, Props, Acc) when Data == []; Props == [] -> Acc;
check_fields(Data, [#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType} = Prop | Other], Acc) ->
    LowerField = list_to_binary(string:to_lower(binary_to_list(Field))),
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
                                    LowerField1 = list_to_binary(string:to_lower(binary_to_list(Field1))),
                                    Acc1#{?Struct(LowerField, LowerField1) => Value1}
                            end
                        end, Acc, SubFields),
                    check_fields(Data, Other, Acc2);
                _ ->
                    check_fields(Data, Other, Acc#{LowerField => Value})
            end
    end.

check_field(Data, Props) when is_map(Data) ->
    NewData =
        maps:fold(fun(K, V, Acc) ->
            NewK = list_to_binary(string:to_lower(binary_to_list(K))),
            Acc#{NewK => V}
                  end, #{}, Data),
    check_field(maps:to_list(NewData), Props);

check_field(Data, #{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType}) ->
    NewField = list_to_binary(string:to_lower(binary_to_list(Field))),
    Specs = maps:get(<<"specs">>, DataType, #{}),
    case proplists:get_value(NewField, Data) of
        undefined ->
            undefined;
        Value ->
            Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
            NewValue =
                case Type1 of
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">>; Type1 == <<"SHORT">>; Type1 == <<"LONG">>;Type1 == <<"ENUM">>, is_list(Value) ->
                        round(dgiot_utils:to_int(Value));
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">>, is_float(Value) ->
                        round(Value);
                    _ when Type1 == <<"INT">>; Type1 == <<"DATE">> ->
                        Value;
                    _ when Type1 == <<"FLOAT">>; Type1 == <<"DOUBLE">> ->
                        Precision = maps:get(<<"precision">>, Specs, 3),
                        dgiot_utils:to_float(Value / 1, Precision);
                    <<"BOOL">> ->
                        Value;
                    <<"TEXT">> ->
                        {unicode:characters_to_binary(unicode:characters_to_list((dgiot_utils:to_binary(Value)))), text};
                    <<"GEOPOINT">> ->
                        {unicode:characters_to_binary(unicode:characters_to_list((Value))), text};
%%                    <<"ENUM">> ->
%%%%                        Specs = maps:get(<<"specs">>, DataType, #{}),
%%%%                        ?LOG(info, "Specs ~p", [Specs]),
%%%%                        EnumValue = maps:get(Value, Specs, <<"1">>),
%%%%                        dgiot_utils:to_int(EnumValue);
%%                        Value;
                    <<"STRUCT">> ->
                        Value;
                    <<"IMAGE">> ->
                        round(dgiot_utils:to_int(Value));
                    _ ->
                        Value
                end,
            case check_validate(NewValue, Specs) of
                true ->
                    NewValue;
                false ->
                    throw({error, <<Field/binary, " is not validate">>})
            end
    end;

check_field(Data, Props) ->
    ?LOG(error, "Data ~p", [Data]),
    ?LOG(error, "Props ~p", [Props]),
    undefined.

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
        _ ->
            dgiot_datetime:format(dgiot_datetime:to_localtime(NewV), <<"YY-MM-DD HH:NN:SS">>)
    end.
