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
-module(dlt376_decoder).
-author("gugm").
-include("dgiot_meter.hrl").
-include_lib("dgiot/include/logger.hrl").
-protocol([?DLT376]).

%% API
-export([
    parse_frame/2,
    to_frame/1,
    parse_value/2,
    process_message/2]).


parse_frame(Buff, Opts) ->
    parse_frame(Buff, [], Opts).

parse_frame(<<>>, Acc, _Opts) ->
    {<<>>, Acc};

% 对于小于9的消息，独立decode
parse_frame(<<Rest/binary>> = Bin, Acc, _Opts) when byte_size(Rest) == 15 ->
    NewFrame = #{
        <<"msgtype">> => ?DLT645
    },
    Acc1 = Acc ++ [NewFrame],
    {Bin, Acc1};


%% DLT376协议
%% 68 32 00 32 00 68 C9 14 03 32 63 00 02 73 00 00 01 00 EB 16
parse_frame(<<16#68, _:16, L2_low:6, _:2,L2_high:8, 16#68, C:8, A1:2/bytes,A2:2/bytes,A3:1/bytes, AFN:8, SEQ:8,Rest/binary>> = Bin, Acc, Opts) ->
    Len = L2_high * 255 + L2_low,
    DLen = Len -8,
    case byte_size(Rest) -2 >= DLen of
        true ->
            case Rest of
                <<UserZone:DLen/bytes, Crc:8, 16#16, Rest1/binary>> ->
                    CheckBuf = <<C:8, A1:2/bytes,A2:2/bytes,A3:1/bytes, AFN:8, SEQ:8, UserZone/binary>>,
                    CheckCrc = dgiot_utils:get_parity(CheckBuf),
                    % BinA = dgiot_utils:to_binary(A),
                    Acc1 =
                        case CheckCrc =:= Crc of
                            true ->
                                Frame = #{
                                    % <<"addr">> => <<"16#00,16#00",dlt645_proctol:reverse(A1)/binary,dlt645_proctol:reverse(A2)/binary>>,
                                    <<"addr">> => dgiot_utils:binary_to_hex(dlt376_proctol:encode_of_addr(A1,A2)), %dlt376_proctol:concrat_binary(dlt645_proctol:reverse(A1),dlt645_proctol:reverse(A2)),
                                    <<"command">> => C,
                                    <<"afn">> => AFN,
                                    <<"datalen">> => DLen,
                                    <<"msgtype">> => ?DLT376
                                },
                                case catch (parse_userzone(UserZone, Frame, Opts)) of
                                    {'EXIT', Reason} ->
                                        ?LOG(warning,"UserZone error,UserZone:~p, Reason:~p~n", [dgiot_utils:binary_to_hex(UserZone), Reason]),
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

parse_userzone(UserZone, #{<<"msgtype">> := ?DLT376} = Frame, _Opts) ->
    check_Command(Frame#{<<"data">> => UserZone}).

%% 组装成封包
to_frame(#{
    % <<"msgtype">> := ?DLT376,
    <<"command">> := C,
    <<"addr">> := Addr,
    <<"afn">> := AFN
} = Msg) ->
    {ok, UserZone} = get_userzone(Msg),
    Len = (byte_size(UserZone) + 8) * 4 + 2,
    Crc = dgiot_utils:get_parity(<<C:8, Addr:5/bytes, AFN:8, 16#71, UserZone/binary>>),
    <<
        16#68,
        Len:8,
        16#00,
        Len:8,
        16#00,
        16#68,
        C:8,
        Addr:5/bytes,
        AFN:8,
        16#71,
        UserZone/binary,
        Crc:8,
        16#16
    >>.

% DLT376 链路检测，心跳数据
check_Command(State = #{<<"command">> := 16#C9, <<"afn">> := 16#02}) ->
    State;

% DLT376 抄表返回的数据
check_Command(State = #{<<"command">> := 16#88, <<"afn">> := 16#0C}) ->
    Data = maps:get(<<"data">>, State, <<>>),
    case Data of
        <<Di:4/bytes,DTime:5/bytes,DNum:1/bytes,DValue:5/bytes,_/bytes>> ->
            % {Value, Diff, TopicDI} = parse_value(dlt645_proctol:reverse(Di), Bin),
            State1 = #{
                <<"di">> => Di,
                <<"time">> =>dgiot_utils:to_hex(DTime),
                <<"valuenum">> => DNum,
                <<"value">> => #{dgiot_utils:to_hex(Di)=>binary_to_value_dlt376_bcd(DValue) },
                <<"addr">> => maps:get(<<"addr">>, State, <<>>)
            },
            State1;
        _ ->
            State
    end;

% DLT376 穿透转发返回
check_Command(State = #{<<"command">> := 16#88, <<"afn">> := 16#10}) ->
    Data = maps:get(<<"data">>, State, <<>>),
    case Data of
        <<_:4/bytes,_:1/bytes,DLen2:8,DLen1:8,Rest/bytes>> ->
            DLen = DLen1 * 255 + DLen2,
            case Rest of
                <<DValue:DLen/bytes,_/bytes>> ->
                    {_, Frames} = dlt645_decoder:parse_frame(DValue, []),
                    ?LOG(warning,"GGM 160 check_Command:~p", [Frames]),
                    case Frames of
                        % 拉闸、合闸返回成功
                        [#{<<"command">>:=16#9C} | _] ->
                            Di = <<16#FE,16#FE,16#FE,16#FE>>,
                            State1 = #{
                                <<"di">> => Di,%不做处理
                                <<"value">> =>  #{dgiot_utils:to_hex(Di)=>0 },
                                <<"addr">> => maps:get(<<"addr">>, State, <<>>)
                            },
                            State1;
                        % 拉闸、合闸返回失败
                        [#{<<"command">>:=16#DC,<<"data">> := VData} | _] ->
                            Di = <<16#FE,16#FE,16#FE,16#FD>>,
                            State1 = #{
                                <<"di">> => Di,%不做处理
                                <<"value">> =>  #{dgiot_utils:to_hex(Di)=>dgiot_utils:to_hex(VData) },
                                <<"addr">> => maps:get(<<"addr">>, State, <<>>)
                            },
                            State1;
                        % 查询上一次合闸时间返回
                        [#{<<"command">>:=16#91,<<"di">> := <<16#1E,16#00,16#01,16#01>>,<<"data">> := VData} | _] ->
                            Di = <<16#1E,16#00,16#01,16#01>>,
                            State1 = #{
                                <<"di">> => Di,
                                <<"value">> =>  #{dgiot_utils:to_hex(Di)=>dlt645_decoder:binary_to_dtime_dlt645_bcd(VData) },
                                <<"addr">> => maps:get(<<"addr">>, State, <<>>)
                            },
                            State1;
                        % 查询上一次拉闸时间返回
                        [#{<<"command">>:=16#91,<<"di">> := <<16#1D,16#00,16#01,16#01>>,<<"data">> := VData} | _] ->
                            Di = <<16#1D,16#00,16#01,16#01>>,
                            State1 = #{
                                <<"di">> => Di,
                                <<"value">> =>  #{dgiot_utils:to_hex(Di)=>dlt645_decoder:binary_to_dtime_dlt645_bcd(VData) },
                                <<"addr">> => maps:get(<<"addr">>, State, <<>>)
                            },
                            State1;
                        _ ->
                            pass
                    end;
                _->
                    pass
            end;
        _ ->
            State
    end;

check_Command(State) ->
    State.


% DLT376协议中把二进制转化成float
binary_to_value_dlt376_bcd(BinValue) ->
    RValue =
        case BinValue of
            <<Vf3:4,Vf4:4,Vf1:4,Vf2:4,V2:4,V1:4,V4:4,V3:4,V6:4,V5:4,_/binary>> ->
                Value = V6 * 100000 + V5 * 10000 + V4 * 1000 + V3 * 100 + V2 * 10 + V1 + Vf1 * 0.1 + Vf2 * 0.01 + Vf3 * 0.001 + Vf4 * 0.0001,
                Value;
            _ ->
                0.0
        end,
    RValue.

get_userzone(Msg) ->
    Di = maps:get(<<"di">>, Msg, <<>>),
    Data = maps:get(<<"data">>, Msg, <<>>),
    Di2 = dgiot_utils:hex_to_binary(Di),
    Data2 = dgiot_utils:hex_to_binary(Data),
    UserZone = <<Di2/binary,Data2/binary>>,
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


process_message(Frames,ChannelId) ->
    case Frames of
        % 返回抄表数据
        [#{<<"di">> := <<16#01, 16#01, 16#01, 16#10>>, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/post">>,  % 发送给mqtt进行数据存储
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        % 返回读取上次合闸时间
        [#{<<"di">> := <<16#1E, 16#00, 16#01, 16#01>>, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/status">>,
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        % 返回读取上次拉闸时间
        [#{<<"di">> := <<16#1D, 16#00, 16#01, 16#01>>, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/status">>,
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        % 拉闸，合闸成功
        [#{<<"di">> := <<16#FE, 16#FE, 16#FE, 16#FE>>, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/status">>,
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        % 拉闸，合闸失败
        [#{<<"di">> := <<16#FE, 16#FE, 16#FE, 16#FD>>, <<"addr">> := Addr, <<"value">> := Value} | _] ->
            case dgiot_data:get({meter, ChannelId}) of
                {ProductId, _ACL, _Properties} -> DevAddr = dgiot_utils:binary_to_hex(Addr),
                    Topic = <<"thing/", ProductId/binary, "/", Addr/binary, "/status">>,
                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                    dgiot_mqtt:publish(DeviceId, Topic, jsx:encode(Value));
                _ -> pass
            end;
        _ -> pass
    end.

% test() ->
%     B1 = <<12, 16#68, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#68, 16#91, 16#08, 16#33, 16#33, 16#3D, 16#33, 16#33, 16#33, 16#33, 16#33, 16#0C, 16#16,
%         1, 3, 36,
%         16#68, 16#18, 16#00, 16#18, 16#00, 16#68, 16#88, 16#00, 16#31, 16#07, 16#02, 16#00, 16#00, 16#01, 16#0c, 16#64, 16#00, 16#00, 16#00, 16#00, 16#01, 16#01, 16#58, 16#23, 16#10, 16#03, 16#16, 16#93, 16#99, 16#02, 16#07, 16#16,
%         16#68, 16#90, 16#F0, 16#55, 16#00, 16#87>>,
%     {Rest, Frames} = parse_frame(B1, [], #{<<"vcaddr">> => <<"003107020000">>}),
%     io:format("Rest:~p~n", [Frames]),
%     B2 = <<16#00, 16#68, 16#12, 16#09, 16#00, 16#40, 16#01, 16#02, 16#00, 16#07, 16#00, 16#18, 16#11, 16#18, 16#D2, 16#16>>,
%     {Rest2, Frames2} = parse_frame(<<Rest/binary, B2/binary>>, [], #{vcaddr => <<"00310702">>}),
%     io:format("Rest:~p, Frames:~p~n", [Rest2, Frames2]).
