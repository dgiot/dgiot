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

%%%-------------------------------------------------------------------
%%% @author
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% DTU-水泵协议
%%% @end
%%% Created : 08. 十一月 2018 14:49
%%%-------------------------------------------------------------------
-module(pump_decoder).
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_decoder).
-export([handle_info/2, parse_frame/2, to_frame/2]).
-define(TIMEOUT, 1000).
-define(KEYS, [<<"vol">>, <<"pressure_in">>, <<"pressure_out">>, <<"temperature">>,
    <<"power_factor">>, <<"current">>, <<"power">>, <<"flow">>, <<"acc_flow">>]).

parse_frame(Buff, State) ->
    parse_frame(Buff, [], State).

to_frame(Frame, _State) ->
    {reply, to_frame_last(Frame)}.

handle_info({timeout, _TimerRef, send_cmd}, State) ->
    remove_timer(State),
    {ok, send_cmd(State)};

handle_info({mqtt, _Topic, #{<<"action">> := Action} = Cfg}, State) when Action == <<"start">>; Action == <<"stop">> ->
    erase(datas),
    remove_timer(State),
    send_frame(Action, State),
    NewState =
        case Action of
            <<"start">> ->
                put(status, start),
                TimerRef = erlang:start_timer(2000, self(), send_cmd),
                State#{<<"timer">> => TimerRef, <<"status">> => <<"running">>};
            <<"stop">> ->
                put(status, stop),
                maps:without([<<"timer">>, <<"timeout">>, <<"index">>], State#{<<"status">> => <<"ready">>})
        end,
    ?LOG(info,"PUMP RECV: ~p ~p~n", [Action, Cfg]),
    Timeout = maps:get(<<"freq">>, Cfg, ?TIMEOUT),
    {ok, NewState#{<<"timeout">> => dgiot_utils:to_int(Timeout)}};

handle_info({mqtt, Topic, Message}, State) ->
    ?LOG(info,"Topic:~p, Message:~p", [Topic, Message]),
    {ok, State};

handle_info({message, #{<<"cmd">> := <<"login">>, <<"addr">> := DevAddr}}, #{<<"productId">> := ProductId} = State) ->
    ?LOG(info,"PUMP Login ~s", [DevAddr]),
    dgiot_utils:subscribe(<<"thing/", ProductId/binary, "/", DevAddr/binary>>),
    {ok, State#{<<"addr">> => DevAddr}};

handle_info({message, Frame}, State) ->
    {ok, do_frame(Frame, State)};

handle_info(_Info, _State) ->
    ?LOG(info,"~p", [_Info]),
    ok.


%% ===============================================================

parse_frame(Bin, Acc, _State) when byte_size(Bin) < 6 ->
    {Bin, Acc};

parse_frame(<<"pump", L1:8, L2:8, Rest/binary>> = Buff, Acc, State) ->
    <<Len:8>> = dgiot_utils:hex_to_binary(list_to_binary([L1, L2])),
    Length = Len * 2,
    case byte_size(Rest) >= Length of
        true ->
            <<DevAddr:Length/bytes, Last/binary>> = Rest,
            NewAcc = [#{
                <<"addr">> => DevAddr,
                <<"cmd">> => <<"login">>
            } | Acc],
            parse_frame(Last, NewAcc, State);
        false ->
            {Buff, Acc}
    end;

parse_frame(<<16#11, Cmd:8, Len:8, Rest/binary>> = Buff, Acc, #{<<"addr">> := DevAddr} = State) ->
    case byte_size(Rest) - 2 >= Len of
        true ->
            <<UserZone:Len/bytes, Crc:2/binary, Rest1/binary>> = Rest,
            CheckBuf = <<16#11, Cmd:8, Len:8, UserZone/binary>>,
            CheckCrc = dgiot_utils:crc16(CheckBuf),
            case CheckCrc =:= Crc of
                true ->
                    Frame = #{
                        <<"cmd">> => Cmd,
                        <<"addr">> => DevAddr
                    },
                    case catch (parse_userzone(UserZone, Frame, State)) of
                        {'EXIT', Reason} ->
                            ?LOG(warning,"UserZone error,UserZone:~p, Reason:~p~n", [UserZone, Reason]),
                            parse_frame(Rest1, Acc, State);
                        NewFrame ->
                            parse_frame(Rest1, Acc ++ [NewFrame], State)
                    end;
                false ->
                    parse_frame(Rest, Acc, State)
            end;
        false ->
            {Buff, Acc}
    end;

%% modbus协议
parse_frame(<<Addr:8, Cmd:8, Len:8, Rest/binary>> = Buff, Acc, #{<<"addr">> := DevAddr} = State) ->
    case byte_size(Rest) - 2 >= Len of
        true ->
            <<UserZone:Len/bytes, Crc:2/binary, Rest1/binary>> = Rest,
            CheckBuf = <<Addr:8, Cmd:8, Len:8, UserZone/binary>>,
            CheckCrc = dgiot_utils:crc16(CheckBuf),
            case CheckCrc =:= Crc of
                true ->
                    Frame = #{
                        <<"addr">> => Addr,
                        <<"cmd">> => Cmd
                    },
                    case catch (parse_userzone(UserZone, Frame, State)) of
                        {'EXIT', Reason} ->
                            ?LOG(warning,"UserZone error,UserZone:~p, Reason:~p~n", [UserZone, Reason]),
                            parse_frame(Rest1, Acc, State);
                        NewFrame ->
                            parse_frame(Rest1, Acc ++ [NewFrame#{ <<"addr">> => DevAddr }], State)
                    end;
                false ->
                    parse_frame(Rest1, Acc, State)
            end;
        false ->
            {Buff, Acc}
    end;

parse_frame(<<_:8, Rest/binary>>, Acc, State) ->
    parse_frame(Rest, Acc, State).


%%修改ip回复
%%<- rx 11 10 10 75 00 01 16 43
parse_userzone(<<16#10, 16#75, 16#00, 16#01>>, #{<<"cmd">> := 16#10} = Frame, _State) ->
    Frame#{<<"cmd">> => <<"change_ip_success">>};

%%修改端口回复
%%<- rx 11 10 10 55 00 20 D7 91
parse_userzone(<<16#10, 16#55, 16#00, 16#20>>, #{<<"cmd">> := 16#10} = Frame, _State) ->
    Frame#{<<"cmd">> => <<"change_port_success">>};


%%进口压力
%% 地址01
%% 进口压力为负数，显示为反码。
%%01 03  02  10 ED  75 C9
parse_userzone(Data, #{<<"addr">> := 16#01, <<"cmd">> := 16#03} = Frame, _State) ->
    Press1 = binary_to_integer(dgiot_utils:binary_to_hex(<<16#FF, 16#FF>>), 16) - binary_to_integer(dgiot_utils:binary_to_hex(Data), 16),
    Press = Press1 / 1000,
    Frame#{<<"pressure_in">> => dgiot_utils:to_float(Press, 3)};

%%电能表
%% 地址04
%%04 03 18 43 5D 66 66 3F 99 58 10 3E 85 87 94 3C 8D B8 BB 3E 87 1D E7 3F 7C ED 91 75 95
parse_userzone(Data, #{<<"addr">> :=  16#04, <<"cmd">> := 16#03} = Frame, _State) ->
    <<V1:32/float, V2:32/float, V3:32/float, _V4:32/float, _V5:32/float, V6:32/float>> = Data,
    Result1 = list_to_float(io_lib:format("~.1f", [V1])),
    Result2 = list_to_float(io_lib:format("~.3f", [V2])),
    Result3 = list_to_float(io_lib:format("~.3f", [V3])),
    Result6 = list_to_float(io_lib:format("~.3f", [V6])),
    Frame#{
        <<"vol">> => Result1,
        <<"current">> => Result2,
        <<"power">> => Result3,
        <<"power_factor">> => Result6
    };

%读取流量计流量值
%%地址05
parse_userzone(Data, #{<<"addr">> :=  16#05, <<"cmd">> := 16#03} = Frame, _State) ->
    Result = binary_to_integer(dgiot_utils:binary_to_hex(Data), 16) / 1000,
    Frame#{<<"flow">> => Result};


%读取电磁流量计流量值
%%地址06
parse_userzone(Data, #{<<"addr">> :=  16#06, <<"cmd">> := 16#03} = Frame, _State) ->
    <<D1:4/binary, D2:4/binary, _R/binary>> = Data,
    Result1 = binary_to_integer(dgiot_utils:binary_to_hex(D1), 16) / 1000,
    Result2 = binary_to_integer(dgiot_utils:binary_to_hex(D2), 16) / 1000,
    Frame#{
        <<"acc_flow">> =>  Result1,
        <<"flow">> => Result2
    };


%%温度检测
%%寄存器地址范围：80（0x0050 温度采集数据）
%%支持功能码：03（读保持寄存器）、04（读输入寄存器）
%%温度计算公式：
%%实际温度 = （返回值 – 10000）/100
%%数据查询：11 04 00 50 00 01 33 4B
%%返回：11 04 02 06 92 FA FE
%%返回数据为：0x0692，即 1682，实际温度为（1682-10000）/100 = -83.18℃
parse_userzone(Data, #{<<"cmd">> := 16#04} = Frame, _State) ->
    Temperature = (binary_to_integer(dgiot_utils:binary_to_hex(Data), 16) - 10000) / 100,
    Frame#{<<"temperature">> => Temperature};

%%水泵出口压强
%%AI 输入
%%电压和电流的计算公式：
%%模拟量值 = 返回参数值 /1000。（模拟量值对应的单位为 mA 或者 V）
%%寄存器地址范围：88~89（0x0058~0x0059，电压采集数据）、96~97（0x0060~0x0061，电流采集数据）
%%支持功能码：03（读保持寄存器）、04（读输入寄存器）
%%以第一路电压检测为例：
%%数据查询：11 03 00 58 00 01 07 49
%%返回：11 03 02 10 00 74 47
%%返回数据为：0x1000，表示 4096 mV，即 4.096V。
%%压力值（mpa）=（采集量-4000）*62.5/1000000
parse_userzone(Data, #{<<"cmd">> := 16#03} = Frame, _State) ->
    Press = (binary_to_integer(dgiot_utils:binary_to_hex(Data), 16) - 4000) / 16000 + 0.04, %% 有稍许偏差
    Result2 = list_to_float(io_lib:format("~.3f", [Press])),
    Frame#{<<"pressure_out">> => dgiot_utils:to_float(Result2, 3)};


parse_userzone(Data, Frame, _State) ->
    Frame#{<<"data">> => dgiot_utils:binary_to_hex(Data)}.


%%3.2. DO 输出
%%接线方法：DO 输出为继电器无源输出，4 路共用一个 COM 端，继电器吸合将连接 DO 与 COM 端。
%%具体接线方式请参考《USR-IO424T 接线工艺说明书》。
%%寄存器地址范围：00~03（0x0000~0x0003）
%%支持功能码：01（读线圈）、05（写单个线圈）、0F（写多个线圈）
%%以第一路继电器控制为例：
%%查询：11 01 00 00 00 04 3F 59
%%控制闭合：11 05 00 00 FF 00 8E AA
%%控制断开：11 05 00 00 00 00 CF 5A
%%温度 11 04 00 50 00 01 33 4B
%%出口压强 11 03 00 60 00 01
to_frame_last(#{
    <<"cmd">> := Cmd,
    <<"regaddr">> := Reg,
    <<"data">> := Data
}) ->
    CrcBody = <<16#11, Cmd:8, Reg/binary, Data/binary>>,
    Crc = dgiot_utils:crc16(CrcBody),
    <<CrcBody/binary, Crc/binary>>;

%%modbus协议
%%addr 01 进口压力 04 电能表 05 流量计
%%"010300040001C5CB" 进口电压 "04032000000C4E5A"电能表  "050300000002C58F"流量计"0203000300017439" 出口电压 "030300000002C5E9" 温度
to_frame_last(#{
    <<"cmd">> := Cmd,
    <<"addr">> := Addr,
    <<"regaddr">> := Reg
}) ->
    case get_len(Addr, Cmd, Reg) of
        <<>> ->
            <<>>;
        Len ->
            CrcBody = <<Addr:8, Cmd:8, Reg/binary, Len/binary>>,
            Crc = dgiot_utils:crc16(CrcBody),
            <<CrcBody/binary, Crc/binary>>
    end;

to_frame_last(_) ->
    <<>>.

get_len(Addr, Cmd, Reg) when Cmd == 16#03
    andalso Reg >= <<0, 0>>
    andalso Reg =< <<0, 6>>
    andalso Addr == 16#01 ->
    <<0, 16#01>>;
get_len(Addr, Cmd, Reg) when Cmd == 16#03
    andalso Reg == <<0, 0>>
    andalso Addr == 16#05 ->
    <<0, 16#02>>;
get_len(Addr, Cmd, Reg) when Cmd == 16#03
    andalso Reg >= <<16#00, 16#09>>
    andalso Addr == 16#06 ->
    <<0, 16#04>>;
get_len(Addr, Cmd, Reg) when Cmd == 16#03
    andalso Reg == <<16#00, 16#09>>
    andalso Addr == 16#06 ->
    <<0, 16#04>>;
get_len(Addr, Cmd, Reg) when Cmd == 16#03
    andalso Reg == <<16#20, 16#00>>
    andalso Addr == 16#04 ->
    <<0, 16#0C>>;
get_len(_, _, _) -> <<>>.


remove_timer(State) ->
    case maps:get(<<"timer">>, State, no) of
        no -> no_timer;
        Ref -> erlang:cancel_timer(Ref)
    end.


send_frame(Addr, #{<<"send">> := Send} = State) ->
    Result =
        case maps:get(Addr, frames(), no) of
            no ->
                {error, not_find_frame};
            Frame ->
                {reply, Payload} = to_frame(Frame, State),
                Send(Payload)
        end,
    case Result of
        ok ->
            dgiot_logger:debug("send frame ~p~n", [Addr]),
            ok;
        {error, Reason} ->
            ?LOG(error,"send frame error, CMD:~p,~p~n", [Addr, Reason])
    end.


do_frame(Frame, #{<<"addr">> := Addr} = Env) ->
    Map = maps:with(?KEYS, Frame),
    case maps:size(Map) > 0 of
        false ->
            Env;
        true ->
            Timestamp = dgiot_datetime:nowstamp(),
            remove_timer(Env),
            maps:fold(
                fun(Key, Value, Acc) ->
                    Data = #{
                        <<"value">> => Value,
                        <<"timestamp">> => Timestamp
                    },
                    save_cache(Addr, Key, Data),
                    Acc
                end, Env, Map),
            send_cmd(Env)
    end.

frames() ->
    #{
        %%合闸
        <<"start">> => #{
            <<"cmd">> => 16#05,
            <<"data">> => <<16#FF, 16#00>>,
            <<"regaddr">> => <<16#00, 16#03>>
        },

        %%开闸
        <<"stop">> => #{
            <<"cmd">> => 16#05,
            <<"data">> => <<16#00, 16#00>>,
            <<"regaddr">> => <<16#00, 16#03>>
        },

        %%进口电压
        16#1 => #{
            <<"cmd">> => 16#03,
            <<"addr">> => 16#01,
            <<"regaddr">> => <<16#00, 16#04>>
        },

        %%出口压强检测
        16#2 => #{
            <<"cmd">> => 16#03,
            <<"data">> => <<16#00, 16#01>>,
            <<"regaddr">> => <<16#00, 16#61>>
        },

        %%温度检测
        16#3 => #{
            <<"cmd">> => 16#04,
            <<"data">> => <<16#00, 16#01>>,
            <<"regaddr">> => <<16#00, 16#50>>
        },

        %%电能表
        16#4 => #{
            <<"cmd">> => 16#03,
            <<"addr">> => 16#4,
            <<"regaddr">> => <<16#20, 16#00>>
        },

        %%流量计
        16#5 => #{
            <<"cmd">> => 16#03,
            <<"addr">> => 16#5,
            <<"regaddr">> => <<16#00, 16#00>>
        },

        16#6 => #{
            <<"cmd">> => 16#03,
            <<"addr">> => 16#06,
            <<"regaddr">> => <<16#00, 16#09>>
        }
    }.


send_cmd(State) ->
    case get(status) of
        start ->
            Index = maps:get(<<"index">>, State, 1),
            Cmds = [16#1, 16#2, 16#3, 16#4, 16#5, save],
            Idx =
                case Index > length(Cmds) orelse Index < 1 of
                    true -> 1;
                    false -> Index
                end,
            case lists:nth(Idx, Cmds) of
                save ->
                    %?LOG(info,"PUMP RECV ~p,~p", [Idx, get_cache()]),
                    TimerRef = erlang:start_timer(?TIMEOUT, self(), send_cmd),
                    maps:without([<<"index">>], State#{<<"timer">> => TimerRef});
                Addr ->
                    %?LOG(info,"PUMP RECV ~p,~p", [Idx, get_cache()]),
                    send_frame(Addr, State),
                    TimerRef = erlang:start_timer(?TIMEOUT, self(), send_cmd),
                    State#{<<"timer">> => TimerRef, <<"index">> => Idx + 1}
            end;
        _ ->
            State
    end.

save_cache(Addr, Name, Data) ->
    Datas = put_cache(Name, Data),
    Keys1 = maps:keys(Datas),
    New1 =
        case not lists:member(<<"head">>, Keys1) andalso lists:member(<<"pressure_out">>, Keys1) andalso lists:member(<<"pressure_in">>, Keys1) of
            true ->
                #{
                    <<"pressure_out">> := #{
                        <<"value">> := Pressure_out,
                        <<"timestamp">> := T1
                    },
                    <<"pressure_in">> := #{
                        <<"value">> := Pressure_in,
                        <<"timestamp">> := T2
                    }
                } = Datas,
                Head1 = dgiot_utils:to_float((Pressure_out + Pressure_in) * 1000000 / 9800, 2),
                save_cache(Addr, <<"head">>, #{
                    <<"value">> => Head1,
                    <<"timestamp">> => max(T1, T2)
                });
            false ->
                Datas
        end,
    Keys2 = maps:keys(New1),
    case not lists:member(<<"effect">>, Keys1) andalso lists:member(<<"flow">>, Keys2) andalso lists:member(<<"power">>, Keys2) andalso lists:member(<<"head">>, Keys2) of
        true ->
            #{
                <<"flow">> := #{
                    <<"value">> := Flow,
                    <<"timestamp">> := T3
                },
                <<"power">> := #{
                    <<"value">> := Power,
                    <<"timestamp">> := T4},
                <<"head">> := #{
                    <<"value">> := Head,
                    <<"timestamp">> := T5
                }
            } = New1,
            Effect =
                case Power == 0 of
                    true ->
                        0.0;
                    false ->
                        dgiot_utils:to_float(980 * Flow * Head / (Power * 3600), 2)
                end,
            save_cache(Addr, <<"effect">>, #{
                <<"value">> => Effect,
                <<"timestamp">> => max(T5, max(T3, T4))
            });
        false ->
            New1
    end.

get_cache() ->
    case get(datas) of
        undefined ->
            #{
                <<"speed">> =>#{
                    <<"value">> => 2860,
                    <<"timestamp">> => dgiot_datetime:nowstamp()
                }
            };
        Datas ->
            Datas
    end.

put_cache(Name, Data) ->
    Datas = get_cache(),
    New = Datas#{Name => Data},
    put(datas, New),
    New.
