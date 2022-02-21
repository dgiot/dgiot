%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dlt645_decoder).
-author("johnliu").
-include("dgiot_meter.hrl").
-include_lib("dgiot/include/logger.hrl").
-protocol([?DLT645]).

%% API
-export([
    parse_frame/2,
    to_frame/1,
    test/0,
    parse_value/2,
    binary_to_dtime_dlt645_bcd/1,
    process_message/2
]).

-define(TYPE, ?DLT645).

%% 注册协议类型
-protocol_type(#{
    cType => ?TYPE,
    type => <<"energy">>,
    colum => 10,
    title => #{
        zh => <<"DLT645协议"/utf8>>
    },
    description => #{
        zh => <<"DLT645协议"/utf8>>
    }
}).
%% 注册协议参数
-params(#{
    <<"di">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"0000"/utf8>>,
        title => #{
            zh => <<"信息标识"/utf8>>
        },
        description => #{
            zh => <<"信息标识"/utf8>>
        }
    },
    <<"type">> => #{
        order => 2,
        type => enum,
        required => true,
        default => #{<<"value">> => <<"byte">>, <<"label">> => <<"byte">>},
        enum => [
            #{<<"value">> => <<"byte">>, <<"label">> => <<"byte">>},
            #{<<"value">> => <<"little">>, <<"label">> => <<"little">>},
            #{<<"value">> => <<"bit">>, <<"label">> => <<"bit">>}
        ],
        title => #{
            zh => <<"数据类型"/utf8>>
        },
        description => #{
            zh => <<"数据类型"/utf8>>
        }
    },
    <<"length">> => #{
        order => 3,
        type => integer,
        required => true,
        default => 2,
        title => #{
            zh => <<"长度"/utf8>>
        },
        description => #{
            zh => <<"长度"/utf8>>
        }
    }
}).

parse_frame(Buff, Opts) ->
    parse_frame(Buff, [], Opts).

parse_frame(<<>>, Acc, _Opts) ->
    {<<>>, Acc};

parse_frame(<<16#68, Rest/binary>> = Bin, Acc, _Opts) when byte_size(Rest) =< 9 ->
    {Bin, Acc};

parse_frame(<<16#FE, 16#FE, 16#FE, 16#FE, Buff/binary>>, Acc, Opts) ->
    parse_frame(Buff, Acc, Opts);

%% DLT645协议
%% 68 41 07 00 00 16 04 68 91 09 33 33 33 33 33 33 33 33 36 9A 16
parse_frame(<<16#68, Addr:6/bytes, 16#68, C:8, Len:8, Rest/binary>> = Bin, Acc, Opts) ->
    case byte_size(Rest) - 2 >= Len of
        true ->
            case Rest of
                <<UserZone:Len/bytes, Crc:8, 16#16, Rest1/binary>> ->
                    CheckBuf = <<16#68, Addr:6/bytes, 16#68, C:8, Len:8, UserZone/binary>>,
                    CheckCrc = dgiot_utils:get_parity(CheckBuf),
                    Acc1 =
                        case CheckCrc =:= Crc of
                            true ->
                                Frame = #{
                                    <<"addr">> => dlt645_proctol:reverse(Addr),
                                    <<"command">> => C,
                                    <<"msgtype">> => ?DLT645
                                },
                                case catch (parse_userzone(UserZone, Frame, Opts)) of
                                    {'EXIT', Reason} ->
                                        ?LOG(warning, "UserZone error,UserZone:~p, Reason:~p~n", [dgiot_utils:binary_to_hex(UserZone), Reason]),
                                        Acc;
                                    NewFrame ->
                                        Acc ++ [NewFrame]
                                end;
                            false ->
                                Acc
                        end,
                    parse_frame(Rest1, Acc1, Opts);
                _ ->
                    parse_frame(Rest, Acc, Opts)
            end;
        false ->
            {Bin, Acc}
    end;

parse_frame(<<_:8, Rest/binary>>, Acc, Opts) when byte_size(Rest) > 50 ->
    parse_frame(Rest, Acc, Opts);

parse_frame(<<Rest/binary>>, Acc, _Opts) ->
    {Rest, Acc}.


parse_userzone(UserZone, #{<<"msgtype">> := ?DLT645} = Frame, _Opts) ->
    check_Command(Frame#{<<"data">> => UserZone}).


check_Command(State = #{<<"command">> := 16#11, <<"data">> := <<DataIndex:4/binary, Data/binary>>}) ->
    State#{
        <<"di">> => list_to_binary(dgiot_utils:sub_33h(DataIndex)),
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

%% #{<<"addr">>, <<"command">>, <<"msgtype">>, <<"di">>, <<"data">>, <<"value">>, <<"diff">>, <<"send_di">>}
check_Command(State = #{<<"command">> := 16#91, <<"data">> := <<DataIndex:4/binary, Data/binary>>}) ->
    Di = list_to_binary(dgiot_utils:sub_33h(DataIndex)),
    Bin = list_to_binary(dgiot_utils:sub_33h(Data)),
    {Value, Diff, TopicDI} = parse_value(dlt645_proctol:reverse(Di), Bin),
    State#{
        <<"di">> => dlt645_proctol:reverse(Di),
        <<"data">> => Bin,
        <<"value">> => Value,
        <<"diff">> => Diff,
        <<"send_di">> => TopicDI
    };
%16#B1
check_Command(State = #{<<"command">> := 16#B1, <<"data">> := <<DataIndex:4/binary, Data/binary>>}) ->
    State#{
        <<"di">> => list_to_binary(dgiot_utils:sub_33h(DataIndex)),
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

check_Command(State = #{<<"command">> := 16#D2, <<"data">> := <<Data/binary>>}) ->
    State#{
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

% -define(DLT645_97_MS_READ_DATA,          16#01).
% -define(DLT645_97_SM_READ_DATA_NONE,     16#81).
% -define(DLT645_97_SM_READ_DATA_MORE,     16#A1).
% -define(DLT645_97_SM_READ_DATA_ERRO,     16#C1).
%%1997
check_Command(State = #{<<"command">> := 16#81, <<"data">> := <<DataIndex:2/binary, Data/binary>>}) ->
    State#{
        <<"di">> => list_to_binary(dgiot_utils:sub_33h(DataIndex)),
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

check_Command(State = #{<<"command">> := 16#A1, <<"data">> := <<DataIndex:2/binary, Data/binary>>}) ->
    State#{
        <<"di">> => list_to_binary(dgiot_utils:sub_33h(DataIndex)),
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

check_Command(State = #{<<"command">> := 16#C1, <<"data">> := <<Data/binary>>}) ->
    State#{
        <<"data">> => list_to_binary(dgiot_utils:sub_33h(Data))
    };

% -define(DLT645_MS_FORCE_EVENT_NAME,        <<"1C">>).
% -define(DLT645_SM_FORCE_EVENT_NORM_NAME,   <<"9C">>).
% -define(DLT645_SM_FORCE_EVENT_ERRO_NAME,   <<"DC">>).
% 远程开闸、拉闸 返回正常
check_Command(State = #{<<"command">> := 16#9C}) ->
    State;

% 远程开闸、拉闸 返回正常
check_Command(State = #{<<"command">> := 16#DC}) ->
    State;

check_Command(State) ->
    State.

get_userzone(Msg) ->
    Di = maps:get(<<"di">>, Msg, <<>>),
    Data = maps:get(<<"data">>, Msg, <<>>),
    Di33 = list_to_binary(dgiot_utils:add_33h(Di)),
    Data33 = list_to_binary(dgiot_utils:add_33h(Data)),
    UserZone = <<Di33/binary, Data33/binary>>,
    {ok, UserZone}.

parse_value(Di, Data) ->
    {DI, Diff, SendDi} =
        case Di of
            <<16#05, 16#06, Di3:8, Di4:8>> when (Di3 >= 1 andalso Di3 =< 8) andalso (Di4 >= 1 andalso Di4 =< 63) ->
                {<<16#05, 16#06, Di3:8, 1>>, Di4 - 1, dgiot_utils:binary_to_hex(<<1, Di3:8, 6, 5>>)};
            <<16#00, D2:8, 16#FF, Di4:8>> when D2 >= 1 andalso D2 =< 10 andalso (Di4 >= 1 andalso Di4 =< 12) ->
                {<<16#00, D2:8, 16#FF, 1>>, Di4 - 1, dgiot_utils:binary_to_hex(<<1, 16#FF, D2:8, 0>>)};
            _ ->
                {Di, 0, dgiot_utils:binary_to_hex(dlt645_proctol:reverse(Di))}
        end,

    case dlt645_proctol:parse_data_to_json(DI, Data) of
        {Key, Value} ->
            ValueMap =
                case jsx:is_json(Value) of
                    false ->
                        #{Key => Value};
                    true ->
                        [{K1, _V1} | _] = Value0 = jsx:decode(Value),
                        case size(K1) == 8 of
                            true ->
                                maps:from_list(Value0);
                            false ->
                                maps:from_list(lists:map(fun({K2, V2}) ->
                                    <<Di1:6/binary, _:2/binary, Di2/binary>> = K2,
                                    {<<Di1:6/binary, Di2/binary>>, V2}
                                                         end, Value0))

                        end
                end,
            {ValueMap, Diff, SendDi};
        _ -> {#{}, Diff, SendDi}
    end.

%返回数据转化成时间（时间戳（单位：秒））如1554282344
binary_to_dtime_dlt645_bcd(BinValue) ->
    RValue =
        case BinValue of
            <<S1:4, S2:4, MT1:4, MT2:4, H1:4, H2:4, D1:4, D2:4, M1:4, M2:4, Y1:4, Y2:4, _/binary>> ->
                Year = 2 * 1000 + Y1 * 10 + Y2,
                Month = M1 * 10 + M2,
                Day = D1 * 10 + D2,
                Hour = H1 * 10 + H2,
                Minite = MT1 * 10 + MT2,
                Second = S1 * 10 + S2,
                case Year of
                    2000 ->
                        0;
                    _ ->
                        Value = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minite, Second}}) - 719528 * 24 * 3600 - 8 * 3600,
                        Value
                end;
            _ ->
                0
        end,
    RValue.

process_message(Frames, ChannelId) ->
    case Frames of
        % 查询上一次合闸时间返回
        [#{<<"command">> := 16#91, <<"di">> := <<16#1E, 16#00, 16#01, 16#01>>, <<"addr">> := Addr, <<"data">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/status">>,
                    Di = <<16#1E, 16#00, 16#01, 16#01>>,
                    DValue = #{dgiot_utils:to_hex(Di) => dlt645_decoder:binary_to_dtime_dlt645_bcd(Value)},
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(DValue));
                _ -> pass
            end;
        % 查询上一次拉闸时间返回
        [#{<<"command">> := 16#91, <<"di">> := <<16#1D, 16#00, 16#01, 16#01>>, <<"addr">> := Addr, <<"data">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/status">>,
                    Di = <<16#1D, 16#00, 16#01, 16#01>>,
                    DValue = #{dgiot_utils:to_hex(Di) => dlt645_decoder:binary_to_dtime_dlt645_bcd(Value)},
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(DValue));
                _ -> pass
            end;
        % 拉闸，合闸成功
        [#{<<"command">> := 16#9C, <<"addr">> := Addr} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/status">>,
                    Di = <<16#FE, 16#FE, 16#FE, 16#FE>>,
                    DValue = #{dgiot_utils:to_hex(Di) => 0},
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(DValue));
                _ -> pass
            end;
        % 拉闸，合闸失败
        [#{<<"command">> := 16#DC, <<"addr">> := Addr, <<"data">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/status">>,
                    Di = <<16#FE, 16#FE, 16#FE, 16#FD>>,
                    DValue = #{dgiot_utils:to_hex(Di) => dgiot_utils:to_hex(Value)},
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(DValue));
                _ -> pass
            end;
        % 抄表数据返回
        [#{<<"command">> := 16#91, <<"di">> := _Di, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        _ -> pass
    end.


%% 组装成封包
to_frame(#{
    % <<"msgtype">> := ?DLT645,
    <<"command">> := C,
    <<"addr">> := Addr
} = Msg) ->
    {ok, UserZone} = get_userzone(Msg),
    Len = byte_size(UserZone),
    Crc = dgiot_utils:get_parity(<<16#68, Addr:6/bytes, 16#68, C:8, Len:8, UserZone/binary>>),
    <<
        16#68,
        Addr:6/bytes,
        16#68,
        C:8,
        Len:8,
        UserZone/binary,
        Crc:8,
        16#16
    >>.


test() ->
    B1 = <<12, 16#68, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#68, 16#91, 16#08, 16#33, 16#33, 16#3D, 16#33, 16#33, 16#33, 16#33, 16#33, 16#0C, 16#16,
        1, 3, 36,
        16#68, 16#18, 16#00, 16#18, 16#00, 16#68, 16#88, 16#00, 16#31, 16#07, 16#02, 16#00, 16#00, 16#01, 16#0c, 16#64, 16#00, 16#00, 16#00, 16#00, 16#01, 16#01, 16#58, 16#23, 16#10, 16#03, 16#16, 16#93, 16#99, 16#02, 16#07, 16#16,
        16#68, 16#90, 16#F0, 16#55, 16#00, 16#87>>,
    {Rest, Frames} = parse_frame(B1, [], #{<<"vcaddr">> => <<"003107020000">>}),
    io:format("Rest:~p~n", [Frames]),
    B2 = <<16#00, 16#68, 16#12, 16#09, 16#00, 16#40, 16#01, 16#02, 16#00, 16#07, 16#00, 16#18, 16#11, 16#18, 16#D2, 16#16>>,
    {Rest2, Frames2} = parse_frame(<<Rest/binary, B2/binary>>, [], #{vcaddr => <<"00310702">>}),
    io:format("Rest:~p, Frames:~p~n", [Rest2, Frames2]).
