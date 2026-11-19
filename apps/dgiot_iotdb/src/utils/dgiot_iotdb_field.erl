-module(dgiot_iotdb_field).
-include("dgiot_iotdb.hrl").

-export([get_field/1, get_fields/1, get_type/1, infer_type/1, encode_segment/1,
    measurement/1, decode_measurement/1, encode_value/2, timestamp_ms/1, get_time/2, check_field/3]).

get_field(#{<<"isstorage">> := false}) ->
    pass;
get_field(#{<<"identifier">> := Identifier, <<"dataType">> := DataType} = Field) ->
    case maps:get(<<"isstorage">>, Field, true) of
        false -> pass;
        0 -> pass;
        _ -> {dgiot_utils:to_binary(Identifier), get_type(DataType)}
    end;
get_field(_) ->
    pass.

get_fields(Thing) when is_map(Thing) ->
    Properties = maps:get(<<"properties">>, Thing, []),
    Tags = maps:get(<<"tags">>, Thing, []),
    lists:foldl(fun(Field, Acc) ->
        case get_field(Field) of
            pass -> Acc;
            Value -> Acc ++ [Value]
        end
    end, [], Properties ++ Tags).

get_type(#{<<"type">> := Type}) ->
    get_type(Type);
get_type(<<"int">>) -> <<"INT32">>;
get_type(<<"long">>) -> <<"INT64">>;
get_type(<<"image">>) -> <<"INT64">>;
get_type(<<"float">>) -> <<"FLOAT">>;
get_type(<<"double">>) -> <<"DOUBLE">>;
get_type(<<"bool">>) -> <<"BOOLEAN">>;
get_type(<<"enum">>) -> <<"INT32">>;
get_type(<<"date">>) -> <<"TIMESTAMP">>;
get_type(<<"string">>) -> <<"STRING">>;
get_type(<<"text">>) -> <<"STRING">>;
get_type(<<"file">>) -> <<"STRING">>;
get_type(<<"url">>) -> <<"STRING">>;
get_type(<<"geopoint">>) -> <<"STRING">>;
get_type(<<"struct">>) -> <<"STRING">>;
get_type(<<"BOOLEAN">>) -> <<"BOOLEAN">>;
get_type(<<"INT32">>) -> <<"INT32">>;
get_type(<<"INT64">>) -> <<"INT64">>;
get_type(<<"FLOAT">>) -> <<"FLOAT">>;
get_type(<<"DOUBLE">>) -> <<"DOUBLE">>;
get_type(<<"STRING">>) -> <<"STRING">>;
get_type(<<"DATE">>) -> <<"DATE">>;
get_type(<<"TIMESTAMP">>) -> <<"TIMESTAMP">>;
get_type(<<"TEXT">>) -> <<"STRING">>;
get_type(<<"BLOB">>) -> <<"BLOB">>;
get_type(Other) ->
    _ = Other,
    <<"STRING">>.

infer_type(Value) when is_boolean(Value) -> <<"BOOLEAN">>;
infer_type(Value) when is_integer(Value) -> <<"INT64">>;
infer_type(Value) when is_float(Value) -> <<"DOUBLE">>;
infer_type(Value) when is_binary(Value) -> <<"STRING">>;
infer_type(Value) when is_list(Value); is_map(Value) -> <<"STRING">>;
infer_type(_) -> <<"STRING">>.

encode_segment(Value) ->
    <<"x", (hex(dgiot_utils:to_binary(Value)))/binary>>.

measurement(Field) ->
    <<"m", (hex(dgiot_utils:to_binary(Field)))/binary>>.

decode_measurement(Column) when is_binary(Column) ->
    Name0 = case re:split(Column, <<"\\s+AS\\s+">>, [caseless, {return, binary}]) of
        [_Expression, Alias] -> Alias;
        _ ->
            case binary:split(Column, <<".">>, [global]) of
                Parts when length(Parts) > 0 -> lists:last(Parts);
                _ -> Column
            end
    end,
    Name = case binary:split(Name0, <<"(">>, [global]) of
        [_Function, MeasurementWithClose] -> binary:replace(MeasurementWithClose, <<")">>, <<>>, [global]);
        _ -> binary:replace(Name0, <<")">>, <<>>, [global])
    end,
    case Name of
        <<"m", Encoded/binary>> -> unhex(Encoded);
        _ -> Name
    end;
decode_measurement(Column) ->
    dgiot_utils:to_binary(Column).

encode_value(_Type, null) -> null;
encode_value(<<"BOOLEAN">>, true) -> true;
encode_value(<<"BOOLEAN">>, false) -> false;
encode_value(<<"BOOLEAN">>, Value) when Value =:= 1; Value =:= <<"1">>; Value =:= <<"true">> -> true;
encode_value(<<"BOOLEAN">>, Value) when Value =:= 0; Value =:= <<"0">>; Value =:= <<"false">> -> false;
encode_value(<<"INT32">>, Value) when is_integer(Value) -> Value;
encode_value(<<"INT32">>, Value) -> dgiot_utils:to_int(Value);
encode_value(<<"INT64">>, Value) when is_integer(Value) -> Value;
encode_value(<<"INT64">>, Value) -> dgiot_utils:to_int(Value);
encode_value(<<"FLOAT">>, Value) when is_number(Value) -> Value;
encode_value(<<"FLOAT">>, Value) -> dgiot_utils:to_float(Value);
encode_value(<<"DOUBLE">>, Value) when is_number(Value) -> Value;
encode_value(<<"DOUBLE">>, Value) -> dgiot_utils:to_float(Value);
encode_value(<<"TIMESTAMP">>, Value) -> timestamp_ms(Value);
encode_value(_Type, Value) when is_map(Value); is_list(Value) -> dgiot_json:encode(Value);
encode_value(_Type, Value) -> dgiot_utils:to_binary(Value).

timestamp_ms(now) -> dgiot_datetime:now_ms();
timestamp_ms(undefined) -> dgiot_datetime:now_ms();
timestamp_ms(Value) when is_integer(Value), Value < 100000000000 -> Value * 1000;
timestamp_ms(Value) when is_integer(Value) -> Value;
timestamp_ms(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Integer -> timestamp_ms(Integer)
    catch
        _:_ ->
            case relative_timestamp_ms(Value) of
                {ok, Timestamp} -> Timestamp;
                error ->
                    try dgiot_datetime:localtime_to_unixtime(dgiot_datetime:to_localtime(Value)) * 1000
                    catch _:_ -> dgiot_datetime:now_ms() end
            end
    end;
timestamp_ms(Value) -> timestamp_ms(dgiot_utils:to_binary(Value)).

relative_timestamp_ms(Value) ->
    case re:run(Value, <<"^\\s*now\\s*([+-])\\s*([0-9]{1,10})\\s*(ms|s|m|h|d|w)\\s*$">>,
            [{capture, [1, 2, 3], binary}]) of
        {match, [Sign, Amount0, Unit]} ->
            Amount = binary_to_integer(Amount0) * time_unit_ms(Unit),
            Offset = case Sign of <<"-">> -> -Amount; _ -> Amount end,
            {ok, dgiot_datetime:now_ms() + Offset};
        nomatch -> error
    end.

time_unit_ms(<<"ms">>) -> 1;
time_unit_ms(<<"s">>) -> 1000;
time_unit_ms(<<"m">>) -> 60000;
time_unit_ms(<<"h">>) -> 3600000;
time_unit_ms(<<"d">>) -> 86400000;
time_unit_ms(<<"w">>) -> 604800000.

get_time(Value, Interval) ->
    Milliseconds = timestamp_ms(Value),
    LocalTime = dgiot_datetime:to_localtime(Milliseconds div 1000),
    Format = case Interval of
        <<>> -> <<"YY-MM-DD HH:NN:SS">>;
        _ ->
            case binary:last(Interval) of
                $a -> <<>>;
                $s -> <<"DD HH:NN:SS">>;
                $m -> <<"MM-DD HH:NN">>;
                $h -> <<"MM-DD HH">>;
                $d -> <<"YY-MM-DD">>;
                $y -> <<"YY">>;
                $H -> <<"HH">>;
                $D -> <<"DD">>;
                $M -> <<"MM">>;
                _ -> <<"YY-MM-DD HH:NN:SS">>
            end
    end,
    case Format of
        <<>> -> dgiot_utils:to_binary(Value);
        _ -> dgiot_datetime:format(LocalTime, Format)
    end.

check_field(_Type, Value, #{<<"specs">> := #{<<"type">> := <<"minutes">>}}) when is_number(Value) ->
    dgiot_utils:to_float(Value / 60, 2);
check_field(_Type, Value, #{<<"specs">> := #{<<"type">> := <<"hour">>}}) when is_number(Value) ->
    dgiot_utils:to_float(Value / 3600, 2);
check_field(_Type, Value, #{<<"specs">> := #{<<"type">> := <<"day">>}}) when is_number(Value) ->
    dgiot_utils:to_float(Value / 86400, 2);
check_field(<<"bool">>, Value, #{<<"specs">> := Specs}) -> maps:get(dgiot_utils:to_binary(Value), Specs, Value);
check_field(<<"enum">>, Value, #{<<"specs">> := Specs}) -> maps:get(dgiot_utils:to_binary(Value), Specs, Value);
check_field(<<"struct">>, Value, _Context) -> Value;
check_field(<<"geopoint">>, Value, #{<<"deviceid">> := DeviceId}) ->
    BinValue = dgiot_utils:to_binary(Value),
    case binary:split(BinValue, <<"_">>, [global, trim]) of
        [Longitude, Latitude] ->
            case dgiot_gps:get_baidu_addr(Longitude, Latitude) of
                #{<<"baiduaddr">> := #{<<"formatted_address">> := Address}} ->
                    update_device_location(DeviceId, Longitude, Latitude, Address),
                    Address;
                _ -> <<"[", BinValue/binary, "]经纬度解析错误"/utf8>>
            end;
        _ -> <<"无GPS信息"/utf8>>
    end;
check_field(<<"float">>, Value, #{<<"specs">> := Specs}) when is_number(Value) ->
    dgiot_utils:to_float(Value, maps:get(<<"precision">>, Specs, 3));
check_field(<<"double">>, Value, #{<<"specs">> := Specs}) when is_number(Value) ->
    dgiot_utils:to_float(Value, maps:get(<<"precision">>, Specs, 3));
check_field(<<"image">>, Value, #{<<"datatype">> := DataType, <<"deviceid">> := DeviceId}) ->
    AppName = dgiot_device:get_appname(DeviceId),
    Url = dgiot_device:get_url(AppName),
    ImageValue = maps:get(<<"imagevalue">>, DataType, <<>>),
    BinValue = dgiot_utils:to_binary(Value),
    <<Url/binary, "/dgiot_file/", DeviceId/binary, "/", BinValue/binary, ".", ImageValue/binary>>;
check_field(<<"date">>, <<"1970-01-01 08:00:00.000">>, _Context) -> <<"--">>;
check_field(<<"date">>, Value, _Context) -> dgiot_utils:to_binary(Value);
check_field(_Type, Value, _Context) -> dgiot_utils:to_binary(Value).

update_device_location(DeviceId, Longitude, Latitude, Address) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"location">> := #{<<"__type">> := <<"GeoPoint">>,
                <<"longitude">> := Longitude, <<"latitude">> := Latitude}}} ->
            ok;
        {ok, #{<<"detail">> := Detail}} ->
            dgiot_parsex:update_object(<<"Device">>, DeviceId, #{
                <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => Longitude,
                    <<"latitude">> => Latitude},
                <<"detail">> => Detail#{<<"address">> => Address}});
        _ ->
            ok
    end.

hex(Binary) ->
    << <<(hex_digit(Byte bsr 4)), (hex_digit(Byte band 16#0F))>> || <<Byte>> <= Binary >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

unhex(<<A, B, Rest/binary>>) ->
    case {unhex_digit(A), unhex_digit(B)} of
        {error, _} -> <<"m", (hex(<<A, B, Rest/binary>>))/binary>>;
        {_, error} -> <<"m", (hex(<<A, B, Rest/binary>>))/binary>>;
        {High, Low} -> <<((High bsl 4) bor Low), (unhex(Rest))/binary>>
    end;
unhex(<<>>) -> <<>>;
unhex(_) -> <<>>.

unhex_digit(C) when C >= $0, C =< $9 -> C - $0;
unhex_digit(C) when C >= $a, C =< $f -> C - $a + 10;
unhex_digit(C) when C >= $A, C =< $F -> C - $A + 10;
unhex_digit(_) -> error.
