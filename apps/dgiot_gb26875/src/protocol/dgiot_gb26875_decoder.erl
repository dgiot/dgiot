%%%-------------------------------------------------------------------
%%% @author weixingzheng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 南网上行通信规约
%%% @end
%%% Created : 08. 十一月 2018 14:49
%%%-------------------------------------------------------------------
-module(dgiot_gb26875_decoder).
-include_lib("dgiot_gb26875.hrl").
-author("weixingzheng").
-include_lib("dgiot/include/logger.hrl").
-protocol([?GB26875]).

%% API
-export([parse_frame/2, to_frame/1, test/0]).


test() ->
    Buff = <<16#40, 16#40, 16#00, 16#00, 16#01, 16#01, 16#18, 16#0d, 16#11, 16#16, 16#0a, 16#14, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#06, 16#05, 16#04, 16#03, 16#02, 16#01, 16#30, 16#00, 16#02, 16#02, 16#01, 16#01, 16#03, 16#00, 16#d9, 16#00, 16#06, 16#00, 16#02, 16#00, 16#a3, 16#c1, 16#c7, 16#f8, 16#a3, 16#b1, 16#b2, 16#e3, 16#df, 16#c8, 16#b2, 16#b8, 16#d7, 16#df, 16#c0, 16#c8, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#30, 16#12, 16#13, 16#01, 16#08, 16#14, 16#68, 16#23, 16#23>>,
    {ok, Result} = parse_frame(Buff, #{}),
    ?LOG(info, "Result ~p~n", [Result]),
    io:format("Result ~p~n", [Result]),
    R = to_frame(Result),
    case R =:= Buff of
        true ->
            io:format("success Buff ~p~n", [Buff]);
        _ ->
            io:format("error R ~p~n", [R])
    end.

parse_frame(Buff, Opts) ->
    ?LOG(info, "Buff ~p~n", [Buff]),
    parse_frame(Buff, #{}, Opts).

parse_frame(<<"@@", Header:22/binary, Length:16/little-integer, Tail/binary>> = Buff, Acc, Opts) when size(Tail) >= Length + 4 ->
    <<"@@", Head:25/binary, _/binary>> = Buff,
    <<SerialId:16, Version:16, Time:6/binary, Source:6/binary, Destination:6/binary>> = Header,
    {Acc1, Rest1} =
        case Tail of
            <<Action:8, Appdata:Length/binary, Crc:1/binary, "##", Rest/binary>> ->
                CheckCrc = dgiot_utils:get_parity(<<Head/binary, Appdata/binary>>),
                case <<CheckCrc>> =:= Crc of
                    true ->
                        <<Type:1/binary, Len:1/binary, Bodys/binary>> = Appdata,
                        {maps:merge(Acc#{
                            <<"msgtype">> => ?GB26875,
                            <<"header">> => #{
                                <<"serialid">> => SerialId,
                                <<"version">> => Version,
                                <<"timestamp">> => get_timestamp(Time),
                                <<"source">> => dgiot_utils:binary_to_hex(reverse(Source)),
                                <<"target">> => dgiot_utils:binary_to_hex(reverse(Destination))
                            },
                            <<"action">> => Action,
                            <<"appdata">> => Appdata
                        }, decoder_appdata(dgiot_utils:to_int(dgiot_utils:binary_to_hex(Type)), dgiot_utils:to_int(dgiot_utils:binary_to_hex(Len)), Bodys)), Rest};
                    false ->
                        {Acc, <<>>}
                end;
            _Oth ->
                ?LOG(info, "_Oth ~p~n", [_Oth]),
                {Acc, <<>>}
        end,
    parse_frame(Rest1, Acc1, Opts);

%%parse_frame(<<_:8, _/binary>> = Rest, Acc, Opts) ->
%%    ?LOG(info, "Rest ~p~n", [Rest]),
%%    parse_frame(Rest, Acc, Opts);

parse_frame(<<>>, Acc, _Opts) ->
    {ok, Acc};

parse_frame(Buff, Acc, Opts) ->
    ?LOG(info, "Buff ~p", [Buff]),
    ?LOG(info, "Acc ~p", [Acc]),
    ?LOG(info, "Opts ~p", [Opts]),
    ok.

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

get_delen(Type) ->
    case Type of
        ?TYPE_UP_SYSTEM_STATE -> {<<"systemstate">>, 4};
        ?TYPE_UP_RUNNING_STATUS -> {<<"runningstatus">>, 40};
        _ ->
            {<<"unknow">>, 4}
    end.

decoder_appdata(Type, Len, Bodys) ->
    {Key, BodyLen} = get_delen(Type),
    ?LOG(info, "BodyLen ~p", [BodyLen]),
    ShortLen = Len * BodyLen,
    LongLen = Len * (BodyLen + 6),
    NewBodys =
        case size(Bodys) of
            ShortLen ->
                [decoder_appdata(Type, {Body, #{}}) || <<Body:BodyLen/binary>> <= Bodys];
            LongLen ->
                [decoder_appdata(Type, {Body, #{<<"timestamp">> => get_timestamp(Time)}}) || <<Body:BodyLen/binary, Time:6/binary>> <= Bodys];
            _ ->
                []
        end,
    ?LOG(info, "Key ~p~n", [Key]),
    ?LOG(info, "Bodys ~p~n", [NewBodys]),
    #{Key => NewBodys}.

decoder_appdata(?TYPE_UP_SYSTEM_STATE, {<<Type:8, Addr:8, Flag:16>>, Timestamp}) ->
    maps:merge(#{<<"equ">> => #{
        <<"ctrl">> => #{
            <<"type">> => Type,
            <<"addr">> => Addr
        }
    },
        <<"flag">> => dgiot_utils:binary_to_hex(reverse(Flag))
    }, Timestamp);

decoder_appdata(?TYPE_UP_RUNNING_STATUS, {<<Type:8, Addr:8, Type2:8, Addr2:4/binary, Flag:2/binary, Description:31/binary>>, Timestamp}) ->
    maps:merge(#{<<"equ">> => #{
        <<"ctrl">> => #{
            <<"type">> => Type,
            <<"addr">> => Addr
        },
        <<"type">> => Type2,
        <<"addr">> => dgiot_utils:binary_to_hex(reverse(Addr2))
    },
        <<"flag">> => dgiot_utils:binary_to_hex(reverse(Flag)),
        <<"description">> => Description
    }, Timestamp);

%%decoder_appdata(?TYPE_UP_ANALOG_QUANTITY, Len, Body) ->
%%
%%    #{};
%%decoder_appdata(?TYPE_UP_OPERATING_INFORMATION, Len, Body) ->
%%
%%    #{};
%%decoder_appdata(?TYPE_UP_SYSTEM_CONFIGURATION, Len, Body) ->
%%
%%    #{};
%%decoder_appdata(?TYPE_UP_PARTS_CONFIGURATION, Len, Body) ->
%%
%%    #{};
%%decoder_appdata(?TYPE_UP_SYSTEM_TIME, Len, Body) ->
%%
%%    #{};


decoder_appdata(_Type, {_Body, _Timestamp}) ->
    ?LOG(info, "_Type ~p", [_Type]),
    ?LOG(info, "_Body ~p", [_Body]),
    ?LOG(info, "_Timestamp ~p", [_Timestamp]),
    #{}.

%%
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_CONTROL, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_SEND_DATA, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_CONFIRM, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_REQUEST, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_RESPONSE, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := ?COMMAND_DENY, <<"data">> := Appdata}, Opts) ->
%%
%%    ok;
%%
%%parse_userzone(#{<<"msgtype">> := ?GB26875, <<"command">> := _, <<"data">> := _}, _Opts) ->
%%
%%    ok.

to_frame(Frame) ->
    to_frame_last(Frame).

to_frame_last(#{
    <<"msgtype">> := ?GB26875,
    <<"header">> := #{
        <<"version">> := Version,
        <<"timestamp">> := Timestamp,
        <<"source">> := Source,
        <<"target">> := Target
    },
    <<"action">> := Action
} = Frame) ->
    SerialId =
        case get(number) of
            undefined ->
                0;
            Num ->
                Num + 1
        end,
    put(number, SerialId),
    Time = get_time(Timestamp),
    ReverseSource = reverse(dgiot_utils:hex_to_binary(Source)),
    ReverseTarget = reverse(dgiot_utils:hex_to_binary(Target)),
    Header = <<SerialId:16, Version:16, Time:6/binary, ReverseSource:6/binary, ReverseTarget:6/binary>>,
    Appdata = encoder_appdata(Frame),
    Length = size(Appdata),
    Crcbin = <<Header/binary, Length:16/little-integer, Action:8, Appdata/binary>>,
    Crc = dgiot_utils:get_parity(Crcbin),
    Tail = <<Action:8, Appdata/binary, Crc:8, "##">>,
    <<"@@", Header:22/binary, Length:16/little-integer, Tail/binary>>;

to_frame_last(#{
    <<"msgtype">> := ?GB26875,
    <<"number">> := _Number,
    <<"protocol">> := _Protocol,
    <<"time">> := _Timestamp,
    <<"source">> := _Source,
    <<"destination">> := _Destination,
    <<"command">> := _Command,
    <<"data">> := _Data
}) ->
    <<>>.

set_time(Body) ->
    case maps:find(<<"timestamp">>, Body) of
        error ->
            <<>>;
        {ok, Timestamp} ->
            get_time(Timestamp)
    end.

get_enlen(Type) ->
    case Type of
        <<"systemstate">> -> {?TYPE_UP_SYSTEM_STATE, 4};
        <<"runningstatus">> -> {?TYPE_UP_RUNNING_STATUS, 40};
        _ ->
            {<<"unknow">>, 4}
    end.

encoder_appdata(Bodys) ->
    {Type, Infodata, Time} = encoder_infodata(Bodys),
    {AppType, InfoLen} = get_enlen(Type),
    LongLen = InfoLen + 6,
    InfoLen1 =
        case Time of
            <<>> ->
                dgiot_utils:to_int(size(Infodata) / InfoLen);
            _ ->
                dgiot_utils:to_int(size(Infodata) / LongLen)
        end,
    <<AppType:8, InfoLen1:8, Infodata/binary>>.

encoder_infodata(#{<<"systemstate">> := Bodys}) ->
    {Infodata, Time1} =
        lists:foldl(fun(Body, {Acc, _Acc1}) ->
            #{<<"equ">> := #{
                <<"ctrl">> := #{
                    <<"type">> := Type,
                    <<"addr">> := Addr
                }
            },
                <<"flag">> := Flag
            } = Body,
            Time = set_time(Body),
            ReverseFlag = reverse(dgiot_utils:hex_to_binary(Flag)),
            {<<Acc/binary, Type:8, Addr:8, ReverseFlag/binary, Time/binary>>, Time}
                    end, {<<>>, <<>>}, Bodys),
    {<<"systemstate">>, Infodata, Time1};

encoder_infodata(#{<<"runningstatus">> := Bodys}) ->
    {Infodata, Time1} =
        lists:foldl(fun(Body, {Acc, _Acc1}) ->
            #{<<"equ">> := #{
                <<"ctrl">> := #{
                    <<"type">> := Type,
                    <<"addr">> := Addr
                },
                <<"type">> := Type2,
                <<"addr">> := Addr2
            },
                <<"flag">> := Flag,
                <<"description">> := Description
            } = Body,
            Time = set_time(Body),
            ReverseAddr2 = reverse(dgiot_utils:hex_to_binary(Addr2)),
            ReverseFlag = reverse(dgiot_utils:hex_to_binary(Flag)),
            {<<Acc/binary, Type:8, Addr:8, Type2:8, ReverseAddr2/binary, ReverseFlag/binary, Description/binary, Time/binary>>, Time}
                    end, {<<>>, <<>>}, Bodys),
    {<<"runningstatus">>, Infodata, Time1};


encoder_infodata(_Data) ->
    <<>>.

get_timestamp(<<S:8, Mn:8, H:8, D:8, M:8, Y:8>>) ->
    dgiot_datetime:localtime_to_unixtime({{2000 + Y, M, D}, {H, Mn, S}}).

get_time(Timestamp) ->
    {{Y, M, D}, {H, Mn, S}} = dgiot_datetime:unixtime_to_localtime(Timestamp),
    Y1 = Y - 2000,
    <<S:8, Mn:8, H:8, D:8, M:8, Y1:8>>.
