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

-module(dgiot_gb26875_decoder).
-include_lib("dgiot_gb26875.hrl").
-author("stoneliu").
-include_lib("dgiot/include/logger.hrl").
-protocol([?GB26875]).

%% API
-export([parse_frame/2, to_frame/1, test/0]).


test() ->
    Buff = <<16#40, 16#40, 16#00, 16#00, 16#01, 16#01, 16#18, 16#0d, 16#11, 16#16, 16#0a, 16#14, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#06, 16#05, 16#04, 16#03, 16#02, 16#01, 16#30, 16#00, 16#02, 16#02, 16#01, 16#01, 16#03, 16#00, 16#d9, 16#00, 16#06, 16#00, 16#02, 16#00, 16#a3, 16#c1, 16#c7, 16#f8, 16#a3, 16#b1, 16#b2, 16#e3, 16#df, 16#c8, 16#b2, 16#b8, 16#d7, 16#df, 16#c0, 16#c8, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#30, 16#12, 16#13, 16#01, 16#08, 16#14, 16#68, 16#23, 16#23>>,
    {ok, Result} = parse_frame(Buff, #{}),
    io:format("Result ~p~n", [Result]),
    R = to_frame(Result),
    case R =:= Buff of
        true ->
            io:format("success Buff ~p~n", [Buff]);
        _ ->
            io:format("error R ~p~n", [R])
    end.

parse_frame(Buff, Opts) ->
    parse_frame(Buff, #{}, Opts).

parse_frame(<<>>, Acc, _Opts) ->
    {ok, Acc};

%%GB／T 26875．3—2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 定 义                         |          描 述                                                                        |
%------------------------------------------------------------------------------------------------------------------------
%%      启动符‘@@’               |                                                                                       |
%%      (2字节)                  |    数据包的第1、2字节，为固定值64，64                                                   |
%-------------------------------------------------------------------------------------------------------------------------
%%                               |  数据包的第3、4字节。发送／确认模式下，业务流水号由发送端在发送新的数据包                 |
%%                               |  时按顺序加一，确认方按发送包的业务流水号返回；请求／应答模式下，业务流水号               |
%%      业务流水号                |  由请求端在发送新的请求命令时按顺序加一，应答方按请求包的业务流水号返回。                 |
%%      (2字节)                  |  低字节传输在前。业务流水号是一个2字节的正整数，由通信双方第一次建立网络 (2字节)           |
%%                               |  连接时确定，初始值为o。业务流水号由业务发起方(业务发起方指发送／确认模式                 |
%%                               |  下的发送端或者请求／应答模式下的请求端)独立管理。业务发起方负责业务流水                  |
%%                               |  号的分配和回收，保证在业务存续期间业务流水号的唯一性                                    |
%%-----------------------------------------------------------------------------------------------------------------------
%%      协议版本号              |   协议版本号包含主版本号(第5字节)和用户版本号(第6字节)。主版本号为固定                    |
%%       (2字节)                |   值l，用户版本号由用户自行定义                                                         |
%%-----------------------------------------------------------------------------------------------------------------------
%%      时间标签                |   数据包的第7～12字节，为数据包发出的时间，具体定义见8．2．2                             |
%%     (6字节)                  |                                                                                       |
%%-----------------------------------------------------------------------------------------------------------------------
%%     源地址                   |  数据包的第13～18字节，为数据包的源地址(监控中心或用户信息传输装置地                      |
%%     (6字节) 址)。            |   低字节传输在前                                                                       |
%%-----------------------------------------------------------------------------------------------------------------------
%%     目的地址                  | 数据包的第19～24字节，为数据包的目的地址(监控中心或用户信息传输装置地                     |
%%     (6字节)                   | 址)。低字节传输在前                                                                    |
%%-----------------------------------------------------------------------------------------------------------------------
%%     应用数据单元长度           |  数据包的第25，26字节，为应用数据单元的长度，长度不应大于l 024；低字节传输在前            |
%%     (2字节)                   |                                                                                       |
%%-----------------------------------------------------------------------------------------------------------------------
%%     命令字节                  |                                                                                       |
%%     (1字节)                   |  数据包的第27字节，为控制单元的命令字节，具体定义见表2                                   |
%%-----------------------------------------------------------------------------------------------------------------------
%%   应用数据单元                 |  应用数据单元，基本格式见图5，对于确认／否认等命令包，此单元可为空                    |
%%   (最大1 024字节)              |                                                                                   |
%%------------------------------------------------------------------------------------------------------------------------
%%     校验和                     |  控制单元中各字节数据(第3～27字节)及应用数据单元的算术校验和，舍去8位                 |
%%     (1字节)                    |  以上的进位位后所形成的l字节二进制数                                                |
%%------------------------------------------------------------------------------------------------------------------------
%%   结束符‘##，                  |   为固定值35，35                                                                   |
%%   (2字节)                      |                                                                                   |
%%------------------------------------------------------------------------------------------------------------------------
%%启动符 业务流水号 协议版本号  时间标签       源地址       目的地址      应用数据单元长度  命令字节  应用数据单元 校验和  结束符
%%4040   F809       1000      081E09030316   000000000000 C0A801046D1D  0100            02        16           6C     2323

%%启动符 业务流水号 协议版本号  时间标签   源地址      目的地址   应用数据单元长度  命令字节  应用数据单元 校验和  结束符
%% 握手请求
%%4040   6900       1000   0a110e030615 000000000000  ac103cd56d1d  0100           02       16           30    2323
%%应答
%%4040   6900       1000  0a110e030615 ac103cd56d1d   000000000000  0100           03       17           30    2323
parse_frame(<<"@@", SerialId:16, Version:16, Time:6/binary, Source:6/binary, Destination:6/binary, Length:16/little, ?COMMAND_SEND_DATA:8, UserZone:1/binary, Crc:1/binary, "##", Rest/binary>> = Buff,
        Acc, Opts) when Length == 1 ->
    <<"@@", Head:25/binary, _/binary>> = Buff,
    CheckCrc = dgiot_utils:get_parity(<<Head/binary, UserZone/binary>>),
    {Acc1, Rest1} =
        case <<CheckCrc>> =:= Crc of
            true ->
                AckBuff = <<"@@", SerialId:16, Version:16, Time:6/binary, Destination:6/binary, Source:6/binary, Length:16/little, ?COMMAND_CONFIRM:8, UserZone:1/binary, Crc:1/binary, "##">>,
                NewAcc = Acc#{
                    <<"msgtype">> => ?GB26875,
                    <<"header">> => #{
                        <<"serialid">> => SerialId,
                        <<"version">> => Version,
                        <<"timestamp">> => get_timestamp(Time),
                        <<"source">> => get_address(Source),
                        <<"destination">> => get_address(Destination)
                    },
                    <<"command">> => ?COMMAND_SEND_DATA,
                    <<"appdata">> => dgiot_utils:binary_to_hex(UserZone),
                    <<"ack">> => AckBuff
                },
                {NewAcc, Rest};
            false ->
                {Acc, Rest}
        end,
    parse_frame(Rest1, Acc1, Opts);

parse_frame(<<"@@", SerialId:16, Version:16, Time:6/binary, Source:6/binary, Destination:6/binary, Length:16/little, Command:8, Tail/binary>> = Buff, Acc, Opts)
    when size(Tail) >= Length + 3 andalso Length > 2 ->
    <<"@@", Head:25/binary, _/binary>> = Buff,
    {Acc1, Rest1} =
        case Tail of
            <<UserZone:Length/binary, Crc:1/binary, "##", Rest/binary>> ->
                CheckCrc = dgiot_utils:get_parity(<<Head/binary, UserZone/binary>>),
                case <<CheckCrc>> =:= Crc of
                    true ->
                        Map = maps:merge(Acc#{
                            <<"msgtype">> => ?GB26875,
                            <<"header">> => #{
                                <<"serialid">> => SerialId,
                                <<"version">> => Version,
                                <<"timestamp">> => get_timestamp(Time),
                                <<"source">> => get_address(Source),
                                <<"destination">> => get_address(Destination)
                            },
                            <<"command">> => Command,
                            <<"appdata">> => dgiot_utils:binary_to_hex(UserZone),
                            <<"rawdata">> => #{
                                <<"timestamp">> => Time,
                                <<"source">> => Source,
                                <<"destination">> => Destination,
                                <<"appdata">> => UserZone
                            }
                        }, parse_userzone(UserZone)),
                        {Map, Rest};
                    _ ->
                        {Acc, <<>>}
                end;
            _Oth ->
                ?LOG(info, "_Oth ~p~n", [_Oth]),
                {Acc, <<>>}
        end,
    parse_frame(Rest1, Acc1, Opts);

parse_frame(<<_:8, _/binary>> = Rest, Acc, Opts) ->
    parse_frame(Rest, Acc, Opts);

parse_frame(Buff, Acc, Opts) ->
    ?LOG(info, "Buff ~p", [Buff]),
    ?LOG(info, "Acc ~p", [Acc]),
    ?LOG(info, "Opts ~p", [Opts]),
    ok.

get_address(<<A:8, B:8, C:8, D:8, Port:16>>) ->
    Address = lists:concat([A, ".", B, ".", C, ".", D, ":", Port]),
    dgiot_utils:to_binary(Address).

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

get_flag(Type, Flag) ->
    {_, Flags} =
        lists:foldl(fun(X, {Num, Acc}) ->
            FlagId = dgiot_utils:to_binary(dgiot_utils:to_list(Type) ++ "." ++ dgiot_utils:to_list(Num)),
            case X of
                1 ->
                    {Num + 1, Acc#{FlagId => X}};
                0 ->
                    {Num + 1, Acc}
            end
                    end, {0, #{}}, [X || <<X:1>> <= Flag]),
    Flags.
%%--------------------------------------------------------------------
%%  数据单元标识符    | 类型标志          | 1字节                     |
%%                   | 信息对象数目      |  1字节                     |
%%--------------------------------------------------------------------
%% 信息对象1         |  信息体           |根据类型不周长度不同        |
%%                   | 时间标签1(a)      | 6字节                      |
%%--------------------------------------------------------------------
%%                   |                  |                            |
%%--------------------------------------------------------------------
%%   信息对象n       | 信息体n          | 根据类型不同长度不同      |
%%                   | 时闻标签(b)      | 6字节                    |
%%--------------------------------------------------------------------
%% 对于某些特殊数据类型(a)(b)，此项可为空
%% 数据单元标识符

%%启动符 业务流水号 协议版本号  时间标签    源地址      目的地址 应用数据单元长度  命令字节  应用数据单元     校验和  结束符
%%火警
%%4040    3113     1000  160F11030316   000000000000 C0A801046D1D  0A00         02   18010200160F11030316   16    2323
parse_userzone(<<Type:8, Num:8, InfoBodys/binary>>) ->
    {Key, Len} = get_infobody_len(Type),
    ?LOG(info, "Type ~p Num ~p Len ~p", [Type, Num, Len]),
    ShortLen = Num * Len,
    LongLen = Num * (Len + 6),
    NewBodys =
        case size(InfoBodys) of
            ShortLen ->
                [parse_infobody(Type, {InfoBody, #{}}) || <<InfoBody:Len/binary>> <= InfoBodys];
            LongLen ->
                [parse_infobody(Type, {InfoBody, #{<<"timestamp">> => get_timestamp(Time)}}) || <<InfoBody:Len/binary, Time:6/binary>> <= InfoBodys];
            _ ->
                []
        end,
    #{Key => NewBodys}.

%%--------------------------------------------------------
%% 18 01                                                 |
%%--------------------------------------------------------
%%|   02        | 00       |  160F    | 11030316         |
%%--------------------------------------------------------
%%| 系统类型标记 | 系统地址 | 系统状态 |  状态发生时间     |
%%---------------------------------------------------------------------------------------------------------------------------------------
%%   |bit15|bit14|bit13|bit12    |bit11   |bit10  |bit9    |bit8    |bit7   |bit6   |bit5      |bit4  |bit3  |bit2 |bit1  |bit0         |
%% 1 |预留 |预留 |复位 |配置改变  |手动状态|总线故障|备电故障|主电故障|延时状态|反馈   |启动(开启)|监管  |屏蔽  |故障  |火警  |正常运行状态 |
%% 0 |预留 |预留 |正常 |无配置改变|自动状态|总线正常|备电正常|主电异常|未延时  |无反馈 |停止(关闭)|无监管|无屏蔽|无故障|无火警 |测试状态    |
%%---------------------------------------------------------------------------------------------------------------------------------------
parse_infobody(?CLASS_UP_SYSTEM_STATE, {<<Type:8, Addr:8, Flag:2/binary>>, Timestamp}) ->
    maps:merge(#{<<"equ">> => #{
        <<"ctrl">> => #{
            <<"type">> => Type,
            <<"addr">> => Addr
        }
    },
        <<"flag">> =>  get_flag(?CLASS_UP_SYSTEM_STATE, reverse(Flag))
    }, Timestamp);

%%------------------------
%%光电探测器火警(光电感烟)|
%%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%%40 40 14 00 10 00 05 09 09 1b 07 15 2c 31 2c 32 00 00 78 4d 07 91 8e 23 30 00 02 02 01 01 01 2a 01 00 01 00 02 00 31 ba c5 bd d3 bf da b0 e5 31 bb d8 c2 b7 31 ba c5 00 ba c5 d6 f7 bb fa f8 98 cf a0 00 00 00 25 11 03 01 06 0c 47 23 23|
%%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%%|启动符  |业务流水号| 协议版本号   |时间标签    |源地址       |目的地址|应用数据单元长度|命令字节|应用数据单元|校验和|结束符|
%%|40401400| 1000    | 0509091b0715|2c312c320000|784d07918e23|3000    |0202           |01      |XXXX..XX   | 47  | 2323 |
%%-----------------------------------------------------------------------------------------------------------------------
%%| 类型标识符(1字节)|信息对象数目(1字节) |
%%| 02              | 02                |
%%见8．2．1．1的定义
%%------------------------------------------------------------------------------------------------------------------------
%%|系统类型|系统地址|部件类型|部件地址|部件状态|部件说明                                                      | 状态发生时间|
%%|(1字节) |(1字节)|(1字节) |(4字节) |(2字节) |(31字节)                                                      |6字节       |
%%|01      | 01    |2a     |01000100|0200   |31bac5bdd3bfdab0e531bbd8c2b731bac500bac5d6f7bbfaf898cfa0000000|25110301060c|
%%------------------------------------------------------------------------------------------------------------------------
%%建筑消防设施系统类型标志、系统地址分别为1字节二进制数
%%建筑消防设施部件类型标志符为1字节二进制数，定义如表5所示。
%%建筑消防设施部件地址为4字节二进制数，建筑消防设施部件状态数据为2字节，低字节先传输。
%%|部件状态|
%%-------------------------------------------------------------------------------------------------------------------------
%%   |bit15|bit14|bit13|bit12|bit11|bit10|bit9|bit8    |bit7   |bit6   |bit5      |bit4  |bit3  |bit2 |bit1  |bit0        |
%% 1 |预留 |预留 |预留 |预留  |预留 |预留 |预留 |电源故障|延时状态|反馈   |启动(开启)|监管  |屏蔽  |故障  |火警   |正常运行状态|
%% 0 |预留 |预留 |预留 |预留  |预留 |预留 |预留 |电源正常|未延时  |无反馈 |停止(关闭)|无监管|无屏蔽|无故障|无火警 |测试状态    |
%%-------------------------------------------------------------------------------------------------------------------------
%%建筑消防设施部件说明为31字节的字符串，采用GB 18030--2005规定的编码。
%%状态发生时间
parse_infobody(?CLASS_UP_RUNNING_STATUS, {<<Type:8, Addr:8, Type2:8, Addr2:4/binary, Flag:2/binary, Description:31/binary>>, Timestamp}) ->
    maps:merge(#{<<"equ">> => #{
        <<"ctrl">> => #{
            <<"type">> => Type,
            <<"addr">> => Addr
        },
        <<"type">> => Type2,
        <<"addr">> => dgiot_utils:binary_to_hex(reverse(Addr2))
    },
        <<"flag">> => get_flag(?CLASS_UP_RUNNING_STATUS, reverse(Flag)),
        <<"description">> => dgiot_utils:binary_to_hex(Description)
    }, Timestamp);


%%---------------------------------------------------------------------------------------------
%%  |bit7|bit6           |bit5                  |bit4    |bit3     |bit2  |bit1  |bit0        |
%%---------------------------------------------------------------------------------------------
%% 1|预留|监测连接线路故障|与监控中心通信信道故障 |备电故障|主电故障 |故障   |火警  |正常监视状态|
%% 0|预留|监测连接线路正常|通信倍道正常           |备电异常|主电异常 |无故障 |无火警|测试状态    |
%%---------------------------------------------------------------------------------------------
%% 类型标志符 21
parse_infobody(?CLASS_UP_USERDEV_RUNINFO, {<<Flag:1/binary>>, Timestamp}) ->
    maps:merge(#{
        <<"equ">> => #{
            <<"ctrl">> => #{
                <<"type">> => ?CLASS_UP_USERDEV_RUNINFO}
        },
        <<"flag">> => get_flag(?CLASS_UP_USERDEV_RUNINFO, Flag)
    }, Timestamp);
%%
%%-------------------------------------------------------------
%%  |bit7|bit6  |bit5   |bit4  |bit3    |bit2   |bit1  |bit0  |
%%-------------------------------------------------------------
%% 1|预留|测试  |查岗应答|自检  |警情消除|手动报警|消音  |复位  |
%% 0|预留|无操作|无操作  |无操作|无操作  |无操作  |无操作|无操作|
%%-------------------------------------------------------------
%% 类型标志符 24
%%
parse_infobody(?CLASS_UP_USERDEV_OPINFO, {<<Flag:1/binary, Addr:8>>, Timestamp}) ->
    maps:merge(#{
        <<"equ">> => #{
            <<"ctrl">> => #{
                <<"type">> => ?CLASS_UP_USERDEV_OPINFO,
                <<"addr">> => Addr}
        },
        <<"flag">> => get_flag(?CLASS_UP_USERDEV_OPINFO, Flag)
    }, Timestamp);

parse_infobody(_Type, {_InfoBody, _Acc}) ->
    ?LOG(info, "_Type ~p", [_Type]),
    ?LOG(info, "_InfoBody ~p", [_InfoBody]),
    ?LOG(info, "_Acc ~p", [_Acc]),
    #{}.


get_infobody_len(Type) ->
    case Type of
        ?CLASS_UP_SYSTEM_STATE ->
            {<<"systemstate">>, 4};
        ?CLASS_UP_RUNNING_STATUS ->
            {<<"runningstatus">>, 40};
        ?CLASS_UP_USERDEV_RUNINFO ->
            {<<"userdev_runinfo">>, 1};
        ?CLASS_UP_USERDEV_OPINFO ->
            {<<"userdev_opinfo">>, 2};
        _ ->
            {<<"unknow">>, 4}
    end.

to_frame(Frame) ->
    to_frame_last(Frame).

to_frame_last(#{
    <<"msgtype">> := ?GB26875,
    <<"header">> := #{
        <<"version">> := Version,
        <<"timestamp">> := Timestamp,
        <<"source">> := Source,
        <<"destination">> := Destination
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
    ReverseTarget = reverse(dgiot_utils:hex_to_binary(Destination)),
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
        <<"systemstate">> ->
            {?CLASS_UP_SYSTEM_STATE, 4};
        <<"runningstatus">> ->
%%            40 40 14 00 10 00 05 09 09 1b 07 15 2c 31 2c 32 00 00 78 4d 07 91 8e 23 30 00 02 02 01 01 01 2a 01 00 01 00 02 00 31 ba c5 bd d3 bf da b0 e5 31 bb d8 c2 b7 31 ba c5 00 ba c5 d6 f7 bb fa f8 98 cf a0 00 00 00 25 11 03 01 06 0c 47 23 23
            {?CLASS_UP_RUNNING_STATUS, 40};
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
            HexDescription = dgiot_utils:hex_to_binary(Description),
            {<<Acc/binary, Type:8, Addr:8, Type2:8, ReverseAddr2/binary, ReverseFlag/binary, HexDescription/binary, Time/binary>>, Time}
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
