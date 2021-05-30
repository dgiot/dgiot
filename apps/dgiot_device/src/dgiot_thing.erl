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

-module(dgiot_thing).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([decoder/2, check_value/2, check_value/3, format_string/2]).
-export([test/0]).


-spec decoder(Data :: map(), Props :: map()) -> Result :: map().
decoder(Data, #{<<"properties">> := Props}) -> decoder(Data, Props);
decoder(Data, Props) -> decoder(Data, Props, #{}).
decoder(Data, Props, Acc) when Data == []; Props == [] -> Acc;
decoder(Data, [#{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType} = Prop | Other], Acc) ->
    case format_field(Data, Prop) of
        undefined ->
            decoder(Data, Other, Acc);
        {Value, Rest} ->
            case list_to_binary(string:to_upper(binary_to_list(Type))) of
                <<"STRUCT">> ->
                    #{<<"specs">> := SubFields} = DataType,
                    Acc2 = lists:foldl(
                        fun(#{<<"identifier">> := Field1} = SubField, {_Rest1, Acc1}) ->
                            case format_field(Value, SubField) of
                                undefined ->
                                    Acc1;
                                Value1 ->
                                    Acc1#{Field1 => Value1}
                            end
                        end, {Rest, []}, SubFields),
                    decoder(Rest, Other, Acc#{Field => Acc2});
                _ ->
                    decoder(Rest, Other, Acc#{Field => Value})
            end
    end.

check_value(Data, Thing) when is_map(Data) ->
    maps:fold(
        fun(Id, Value, Acc) ->
            Acc#{Id => check_value(Id, Value, Thing)}
        end, #{}, Data).


check_value(Id, Value, #{<<"properties">> := Props}) ->
    check_value(Id, Value, Props);
check_value(_Id, Value, []) -> {undefied, Value};
check_value(Id, Value, [#{<<"identifier">> := Field, <<"dataForm">> := #{<<"address">> := Id} = _DataForm} = Prop | _]) ->
    {Value1, _Rest} = format_field(Value, Prop),
    {Field, Value1};
check_value(Id, Value, [_ | Props]) ->
    check_value(Id, Value, Props).


format_field(Buff, #{<<"identifier">> := Field, <<"dataType">> := #{<<"type">> := Type} = DataType, <<"dataForm">> := DataForm} = Prop) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    {Value, Rest} = format_value(Buff, Prop#{<<"dataType">> => DataType#{<<"type">> => Type1}}),
    case check_validate(Value, DataType) of
        true ->
            {change_value(Value, DataForm), Rest};
        false ->
            throw({field_error, <<Field/binary, " is not validate">>})
    end.

check_validate(Value, #{<<"max">> := Max, <<"min">> := Min}) when is_integer(Max), is_integer(Min) ->
    Value =< Max andalso Value >= Min;
check_validate(Value, #{<<"max">> := Max}) when is_integer(Max) ->
    Value =< Max;
check_validate(Value, #{<<"min">> := Min}) when is_integer(Min) ->
    Value >= Min;
check_validate(_, _) ->
    true.

change_value(Value, #{<<"rate">> := Rate} = Map) when is_integer(Rate); is_float(Rate) ->
    change_value(Value * Rate, maps:without([<<"rate">>], Map));
change_value(Value, #{<<"offset">> := Offset}) when is_integer(Offset); is_float(Offset) ->
    Value + Offset;
change_value(Value, _) ->
    Value.


format_value(Buff, #{
    <<"dataForm">> := #{<<"quantity">> := Len, <<"byteOrder">> := <<"big">>},
    <<"dataType">> := #{<<"type">> := <<"INT">>}
}) ->
    Size = Len * 8,
    <<Value:Size/unsigned-big-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{
    <<"dataForm">> := #{<<"quantity">> := Len, <<"byteOrder">> := <<"little">>},
    <<"dataType">> := #{<<"type">> := <<"INT">>}
}) ->
    Size = Len * 8,
    <<Value:Size/unsigned-little-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{
    <<"dataForm">> := #{<<"quantity">> := Len, <<"byteOrder">> := <<"big">>},
    <<"dataType">> := #{<<"type">> := Type}
}) when Type == <<"FLOAT">>; Type == <<"DOUBLE">> ->
    Size = max(4, Len) * 8,
    <<Value:Size/unsigned-big-float, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{
    <<"dataForm">> := #{<<"quantity">> := Len, <<"byteOrder">> := <<"little">>},
    <<"dataType">> := #{<<"type">> := Type}
}) when Type == <<"FLOAT">>; Type == <<"DOUBLE">> ->
    Size = max(4, Len) * 8,
    <<Value:Size/unsigned-little-float, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{
    <<"dataForm">> := #{<<"quantity">> := Len, <<"byteOrder">> := <<"little">>},
    <<"dataType">> := #{<<"type">> := <<"STRING">>}
}) ->
    Size = Len * 8,
    <<Value:Size/little-binary, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataType">> := #{<<"type">> := <<"BOOL">>}}) ->
    <<Value:8, Rest/binary>> = Buff,
    {Value, Rest};

%% @todo 其它类型处理
format_value(_, #{<<"identifier">> := Field}) ->
    throw({field_error, <<Field/binary, " is not validate">>}).


format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"INT">>}}) ->
    case Data of
        undefined -> 0;
        _ -> round(dgiot_utils:to_float(Data))
    end;

format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"int">>}}) ->
    case Data of
        undefined -> 0;
        _ -> round(dgiot_utils:to_float(Data))
    end;

format_string(Data, #{<<"dataType">> := #{<<"type">> := Type}})
    when Type == <<"FLOAT">>; Type == <<"DOUBLE">>; Type == <<"float">>; Type == <<"double">> ->
    case Data of
        undefined -> 0.0;
        _ -> dgiot_utils:to_float(Data)
    end;


format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"STRING">>}}) ->
    case Data of
        undefined -> <<"">>;
        _ -> dgiot_utils:to_list(Data)
    end;


format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"string">>}}) ->
    case Data of
        undefined -> <<"">>;
        _ -> dgiot_utils:to_list(Data)
    end;

format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"BOOL">>}}) ->
    case Data of
        undefined -> false;
        _ -> dgiot_utils:to_bool(Data)
    end;


format_string(Data, #{<<"dataType">> := #{<<"type">> := <<"bool">>}}) ->
    case Data of
        undefined -> false;
        _ -> dgiot_utils:to_bool(Data)
    end;

%% @todo 其它类型处理
format_string(_, #{<<"identifier">> := Field}) ->
    throw({field_error, <<Field/binary, " is not validate">>}).

test() ->
    Thing = #{
        <<"properties">> => [
            #{
                <<"name">> => <<"属性1">>,
                <<"dataForm">> =>  #{
                    <<"address">> => <<"TAG1">>,
                    <<"quantity">> => 2,
                    <<"byteOrder">> => <<"big">>,
                    <<"offset">> => 0,
                    <<"rate">> => 0.1
                },
                <<"dataType">> => #{
                    <<"type">> => <<"int">>,
                    <<"specs">> => #{
                        <<"max">> =>  10000,
                        <<"min">> =>  0,
                        <<"step">> =>  0.01,
                        <<"unit">> =>  <<>>
                    }
                },
                <<"required">> => true,
                <<"accessMode">> => <<"r">>,
                <<"identifier">> => <<"shuxing1">>
            },
            #{
                <<"name">> => <<"属性2">>,
                <<"dataForm">> => #{
                    <<"address">> => <<"TAG2">>,
                    <<"quantity">> => 2,
                    <<"byteOrder">> => <<"big">>,
                    <<"offset">> => 0,
                    <<"rate">> => 0.1
                },
                <<"dataType">> => #{
                    <<"type">> => <<"int">>,
                    <<"specs">> => #{
                        <<"max">> => 10000,
                        <<"min">> => 0,
                        <<"step">> => 0.01,
                        <<"unit">> => <<>>
                    }
                },
                <<"required">> => true,
                <<"accessMode">> => <<"r">>,
                <<"identifier">> => <<"shuxing2">>
            }
        ]
    },
    R1 = check_value(<<"shuxing1">>, 800, Thing),
    ?LOG(info,"shuxing1 800 -> ~p", [R1]),
    R2 = check_value(#{<<"shuxing1">> => 800, <<"shuxing2">> => 467}, Thing),
    ?LOG(info,"shuxing1 800, shuxing1 700 -> ~p", [R2]),
    Buff = <<800:16/big-integer, 467:16/big-integer>>,
    decoder(Buff, Thing).
