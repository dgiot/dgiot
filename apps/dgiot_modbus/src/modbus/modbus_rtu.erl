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

-module(modbus_rtu).
-author("jonhl").

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    init/1,
    dealwith/1,
    parse_frame/3,
    to_frame/1,
    format_value/3,
    build_req_message/1]
).

-export([modbus_encoder/4, modbus_decoder/5, is16/1, set_params/3, decode_data/5, get_datasource/1]).

-define(TYPE, ?MODBUS_RTU).

%% 注册协议参数
-params(#{
    <<"slaveid">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"0000"/utf8>>,
        title => #{
            zh => <<"从机地址"/utf8>>
        },
        description => #{
            zh => <<"从机地址(16进制加0X,例如:0X10,否在是10进制),范围1-247,一个字节"/utf8>>
        }
    },
    <<"operatetype">> => #{
        order => 2,
        type => string,
        required => true,
        default => #{<<"value">> => <<"readCoils">>, <<"label">> => <<"0X01:读线圈寄存器"/utf8>>},
        enum => [#{<<"value">> => <<"readCoils">>, <<"label">> => <<"0X01:读线圈寄存器"/utf8>>},
            #{<<"value">> => <<"readInputs">>, <<"label">> => <<"0X02:读离散输入寄存器"/utf8>>},
            #{<<"value">> => <<"readHregs">>, <<"label">> => <<"0X03:读保持寄存器"/utf8>>},
            #{<<"value">> => <<"readIregs">>, <<"label">> => <<"0X04:读输入寄存器"/utf8>>},
            #{<<"value">> => <<"writeCoil">>, <<"label">> => <<"0X05:写单个线圈寄存器"/utf8>>},
            #{<<"value">> => <<"writeHreg">>, <<"label">> => <<"0X06:写单个保持寄存器"/utf8>>},
            #{<<"value">> => <<"writeCoils">>, <<"label">> => <<"0X0f:写多个线圈寄存器"/utf8>>},
            #{<<"value">> => <<"writeHregs">>, <<"label">> => <<"0X10:写多个保持寄存器"/utf8>>}
        ],
        title => #{
            zh => <<"寄存器功能码"/utf8>>
        },
        description => #{
            zh => <<"寄存器功能码"/utf8>>
        }
    },
    <<"address">> => #{
        order => 3,
        type => string,
        required => true,
        default => <<"0X00"/utf8>>,
        title => #{
            zh => <<"寄存器起始地址"/utf8>>
        },
        description => #{
            zh => <<"寄存器起始地址:原数据地址(16进制加0X,例如:0X10,否在是10进制);8位寄存器,一个字节;16位寄存器,两个字节;32位寄存器,四个字节"/utf8>>
        }
    },
    <<"registersnumber">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"1">>,
        title => #{
            zh => <<"寄存器个数"/utf8>>
        },
        description => #{
            zh => <<"寄存器个数(多个寄存器个数)"/utf8>>
        }
    },
    <<"originaltype">> => #{
        order => 5,
        type => string,
        required => true,
        default => #{<<"value">> => <<"bit">>, <<"label">> => <<"位"/utf8>>},
        enum => [
            #{<<"value">> => <<"bit">>, <<"label">> => <<"位"/utf8>>},
            #{<<"value">> => <<"short16_AB">>, <<"label">> => <<"16位 有符号(AB)"/utf8>>},
            #{<<"value">> => <<"short16_BA">>, <<"label">> => <<"16位 有符号(BA)"/utf8>>},
            #{<<"value">> => <<"ushort16_AB">>, <<"label">> => <<"16位 无符号(AB)"/utf8>>},
            #{<<"value">> => <<"ushort16_BA">>, <<"label">> => <<"16位 无符号(BA)"/utf8>>},
            #{<<"value">> => <<"long32_ABCD">>, <<"label">> => <<"32位 有符号(ABCD)"/utf8>>},
            #{<<"value">> => <<"long32_CDAB">>, <<"label">> => <<"32位 有符号(CDAB)"/utf8>>},
            #{<<"value">> => <<"ulong32_ABCD">>, <<"label">> => <<"32位 无符号(ABCD)"/utf8>>},
            #{<<"value">> => <<"ulong32_CDAB">>, <<"label">> => <<"32位 无符号(CDAB)"/utf8>>},
            #{<<"value">> => <<"float32_ABCD">>, <<"label">> => <<"32位 浮点数(ABCD)"/utf8>>},
            #{<<"value">> => <<"float32_CDAB">>, <<"label">> => <<"32位 浮点数(CDAB)"/utf8>>}
        ],
        title => #{
            zh => <<"数据格式"/utf8>>
        },
        description => #{
            zh => <<"数据格式"/utf8>>
        }
    }
}).

%% 注册协议类型
-protocol_type(#{
    cType => ?TYPE,
    type => <<"energy">>,
    colum => 10,
    title => #{
        zh => <<"MODBUS RTU协议"/utf8>>
    },
    description => #{
        zh => <<"MODBUS RTU协议"/utf8>>
    }
}).

init(State) ->
    State#{<<"req">> => [], <<"ts">> => dgiot_datetime:now_ms(), <<"interval">> => 300}.
%%
%%login(#{<<"devaddr">> := DTUAddr, <<"product">> := ProductId, <<"ip">> := Ip,
%%    <<"channelId">> := ChannelId} = State) ->
%%    Topic = <<ProductId/binary, "/", ChannelId/binary, "/", DTUAddr/binary>>,
%%    dgiot_mqtt:subscribe(Topic),
%%    dgiot_device:register(ProductId, DTUAddr, ChannelId, #{<<"ip">> => Ip}),
%%    {ok, State};
%%
%%login(State) ->
%%    {ok, State}.

to_frame(#{
    <<"registersnumber">> := Quality,
    <<"slaveid">> := SlaveId,
    <<"operatetype">> := Operatetype,
    <<"originaltype">> := Originaltype,
    <<"address">> := Address
}) ->
    encode_data(Quality, Address, SlaveId, Operatetype, Originaltype);

%%<<"cmd">> => Cmd,
%%<<"gateway">> => DtuAddr,
%%<<"addr">> => SlaveId,
%%<<"di">> => Address
to_frame(#{
    <<"registersnumber">> := Quality,
    <<"gateway">> := DtuAddr,
    <<"slaveid">> := SlaveId,
    <<"originaltype">> := Originaltype,
    <<"address">> := Address
}) ->
    case dgiot_device:get_subdevice(DtuAddr, SlaveId) of
        not_find -> [];
        [ProductId, _DevAddr] ->
            encode_data(Quality, Address, SlaveId, ProductId, Originaltype)
    end.

%% Quality 读的时候代表寄存器个数，16位的寄存器，一个寄存器表示两个字节，写的时候代表实际下发值
encode_data(Quality, Address, SlaveId, OperateType, Originaltype) ->
    FunCode =
        case OperateType of
            <<"readCoils">> -> ?FC_READ_COILS;
            <<"readInputs">> -> ?FC_READ_INPUTS;
            <<"readHregs">> -> ?FC_READ_HREGS;
            <<"readIregs">> -> ?FC_READ_IREGS;
            <<"writeCoil">> -> ?FC_WRITE_COIL;
            <<"writeHreg">> -> ?FC_WRITE_HREG; %%需要校验，写多个线圈是什么状态
            <<"writeCoils">> -> ?FC_WRITE_COILS;
            <<"writeHregs">> -> ?FC_WRITE_HREGS; %%需要校验，写多个保持寄存器是什么状态
            _ -> ?FC_READ_HREGS
        end,
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(is16(Address)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(is16(SlaveId)),
    NewQuality = dgiot_utils:to_int(get_len(Quality, Originaltype) / 2),
    RtuReq = #rtu_req{
        slaveId = Sh * 256 + Sl,
        funcode = dgiot_utils:to_int(FunCode),
        address = H * 256 + L,
        quality = NewQuality
    },
    build_req_message(RtuReq).

is16(<<"0X", Data/binary>>) when size(Data) == 4 ->
    Data;

is16(<<"0X", Data/binary>>) when size(Data) > 4 ->
    Data;

is16(<<"0X", Data/binary>>) ->
    <<"00", Data/binary>>;

is16(<<"0x", Data/binary>>) when size(Data) == 4 ->
    Data;

is16(<<"0x", Data/binary>>) when size(Data) > 4 ->
    Data;

is16(<<"0x", Data/binary>>) ->
    <<"00", Data/binary>>;

is16(<<"00", Data/binary>>) ->
    is16(Data);

is16(Data) ->
    IntData = dgiot_utils:to_int(Data),
    dgiot_utils:binary_to_hex(<<IntData:16>>).

set_params(Payload, _ProductId, _DevAddr) ->
    Payloads =
        maps:fold(fun(_, #{
            <<"dataForm">> := #{
                <<"protocol">> := <<"MODBUSRTU">>,
                <<"control">> := Setting},
            <<"dataSource">> := #{
                <<"slaveid">> := SlaveId,
                <<"address">> := Address,
                <<"originaltype">> := Originaltype,
                <<"operatetype">> := OperateType} = DataSource
        } = Data, Acc) ->
            case maps:find(<<"value">>, Data) of
                error ->
                    Acc;
                {ok, Value} when erlang:byte_size(Value) == 0 ->
                    Acc;
                {ok, Value} ->
                    FunCode =
                        case OperateType of
                            <<"readCoils">> -> ?FC_READ_COILS;
                            <<"readInputs">> -> ?FC_READ_INPUTS;
                            <<"readHregs">> -> ?FC_READ_HREGS;
                            <<"readIregs">> -> ?FC_READ_IREGS;
                            <<"writeCoil">> -> ?FC_WRITE_COIL;
                            <<"writeHreg">> -> ?FC_WRITE_HREG;
                            <<"writeCoils">> -> ?FC_WRITE_COILS; %%需要校验，写多个线圈是什么状态
                            <<"writeHregs">> -> ?FC_WRITE_HREGS; %%需要校验，写多个保持寄存器是什么状态
                            _ -> ?FC_READ_HREGS
                        end,
                    <<H:8, L:8>> = dgiot_utils:hex_to_binary(is16(Address)),
                    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(is16(SlaveId)),
                    Str1 = re:replace(Setting, "%{d}", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1, <<"type">>)),
%%                                    NewBt = Bytes * 8,
                    Registersnumber = maps:get(<<"registersnumber">>, DataSource, <<"1">>),
                    Bytes = get_len(Registersnumber, Originaltype),
                    RtuReq = #rtu_req{
                        slaveId = Sh * 256 + Sl,
                        funcode = dgiot_utils:to_int(FunCode),
                        address = H * 256 + L,
                        registersnumber = dgiot_utils:to_int(Registersnumber),
                        dataByteSize = dgiot_utils:to_int(Bytes),
                        quality = Value1
                    },
                    Acc ++ [build_req_message(RtuReq)];
                _ ->
                    Acc
            end
                  end, [], Payload),
    Payloads.

%% 010300000002C40B 01030438A93E3B76C0
dealwith(<<SlaveId:8, FunCode:8, Address:16, _:4/binary, SlaveId:8, FunCode:8, Rest/binary>>) ->
    {ok, #{<<"buff">> => <<SlaveId:8, FunCode:8, Rest/binary>>, <<"slaveId">> => SlaveId, <<"address">> => Address}};

dealwith(Buff) ->
    Buff.

%rtu modbus
parse_frame(<<>>, Acc, _State) -> {<<>>, Acc};

parse_frame(Buff, Acc, _State) when size(Buff) < 6 ->
    {<<>>, Acc};

parse_frame(<<MbAddr:8, BadCode:8, ErrorCode:8, Crc:2/binary>> = Buff, Acc,
    #{<<"addr">> := DtuAddr} = State) ->
    CheckCrc = dgiot_utils:crc16(<<MbAddr:8, BadCode:8, ErrorCode:8>>),
    case CheckCrc =:= Crc of
        true ->
            Error = case ErrorCode of
                        ?ILLEGAL_FUNCTION -> {error, illegal_function};
                        ?ILLEGAL_DATA_ADDRESS -> {error, illegal_data_address};
                        ?ILLEGAL_DATA_VALUE -> {error, illegal_data_value};
                        ?SLAVE_DEVICE_FAILURE -> {error, slave_device_failure};
                        ?ACKNOWLEDGE -> {error, acknowledge};
                        ?SLAVE_DEVICE_BUSY -> {error, slave_device_busy};
                        ?NEGATIVE_ACKNOWLEDGE -> {error, negative_acknowledge};
                        ?MEMORY_PARITY_ERROR -> {error, memory_parity_error};
                        ?GATEWAY_PATH_UNAVAILABLE -> {error, gateway_path_unavailable};
                        ?GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND -> {error, gateway_target_device_failed_to_respond};
                        _ -> {error, unknown_response_code}
                    end,
            ?LOG(info, "DtuAddr ~p Modbus ~p, BadCode ~p, Error ~p", [DtuAddr, MbAddr, BadCode, Error]),
            {<<>>, #{}};
        false ->
            parse_frame(Buff, Acc, State)
    end;

%% 传感器直接做为dtu物模型的一个指标
parse_frame(<<SlaveId:8, _/binary>> = Buff, Acc, #{<<"dtuproduct">> := ProductId, <<"slaveId">> := SlaveId, <<"dtuaddr">> := DtuAddr, <<"address">> := Address} = State) ->
    case decode_data(Buff, ProductId, DtuAddr, Address, Acc) of
        {Rest1, Acc1} ->
            parse_frame(Rest1, Acc1, State);
        [Buff, Acc] ->
            [Buff, Acc]
    end;

%% 传感器独立建产品，做为子设备挂载到dtu上面
parse_frame(<<SlaveId:8, _/binary>> = Buff, Acc, #{<<"dtuaddr">> := DtuAddr, <<"slaveId">> := SlaveId, <<"address">> := Address} = State) ->
    case dgiot_device:get_subdevice(DtuAddr, dgiot_utils:to_binary(SlaveId)) of
        not_find ->
            [<<>>, Acc];
        [ProductId, _DevAddr] ->
            case decode_data(Buff, ProductId, DtuAddr, Address, Acc) of
                {Rest1, Acc1} ->
                    parse_frame(Rest1, Acc1, State);
                [Buff, Acc] ->
                    {Buff, Acc}
            end
    end;
%rtu modbus
parse_frame(_Other, Acc, _State) ->
%%    ?LOG(error, "_Other ~p", [_Other]),
    {error, Acc}.

decode_data(Buff, ProductId, DtuAddr, Address, Acc) ->
    <<SlaveId:8, FunCode:8, ResponseData/binary>> = Buff,
    {SizeOfData, DataBytes} =
        case FunCode of
            ?FC_READ_COILS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_INPUTS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_HREGS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_READ_IREGS ->
                <<Size:8, Data/binary>> = ResponseData,
                {Size, Data};
            ?FC_WRITE_COIL -> {0, []};
            ?FC_WRITE_HREG -> {0, []};
            ?FC_WRITE_COILS -> {0, []};
            ?FC_WRITE_HREGS -> {0, []};
            _ -> {0, []}
        end,
    case SizeOfData > 0 of
        true ->
            <<UserZone:SizeOfData/bytes, Crc:2/binary, Rest1/binary>> = DataBytes,
            CheckBuf = <<SlaveId:8, FunCode:8, SizeOfData:8, UserZone/binary>>,
            CheckCrc = dgiot_utils:crc16(CheckBuf),
            case CheckCrc =:= Crc of
                true ->
                    {Rest1, modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc)};
                false ->
                    {Rest1, Acc}
            end;
        false ->
            case FunCode of
                ?FC_WRITE_COIL ->
                    get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc);
                ?FC_WRITE_HREG ->
                    get_write(ResponseData, SlaveId, FunCode, DtuAddr, ProductId, Address, Acc);
                ?FC_WRITE_COILS ->
                    [Buff, Acc];
                ?FC_WRITE_HREGS ->
                    [Buff, Acc];
                _ -> [Buff, Acc]
            end
    end.

get_write(ResponseData, SlaveId, FunCode, _DtuAddr, ProductId, Address, Acc) ->
    <<_Addr:2/binary, Rest1/binary>> = ResponseData,
    Size1 = byte_size(Rest1) - 2,
    <<UserZone:Size1/bytes, Crc:2/binary>> = Rest1,
    CheckBuf = <<SlaveId:8, FunCode:8, _Addr:2/binary, UserZone/binary>>,
    CheckCrc = dgiot_utils:crc16(CheckBuf),
    case CheckCrc =:= Crc of
        true ->
            {<<>>, modbus_decoder(ProductId, SlaveId, Address, UserZone, Acc)};
        false ->
            {<<>>, Acc}
    end.

build_req_message(Req) when is_record(Req, rtu_req) ->
    % validate
    if
        (Req#rtu_req.slaveId < 0) or (Req#rtu_req.slaveId > 255) ->
            throw({argumentError, Req#rtu_req.slaveId});
        true -> ok
    end,
    if
        (Req#rtu_req.funcode < 0) or (Req#rtu_req.funcode > 255) ->
            throw({argumentError, Req#rtu_req.funcode});
        true -> ok
    end,
    if
        (Req#rtu_req.address < 0) or (Req#rtu_req.address > 65535) ->
            throw({argumentError, Req#rtu_req.address});
        true -> ok
    end,
    if
        (Req#rtu_req.quality < 0) or (Req#rtu_req.quality > 65535) ->
            throw({argumentError, Req#rtu_req.quality});
        true -> ok
    end,
    Message =
        case Req#rtu_req.funcode of
            ?FC_READ_COILS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_READ_INPUTS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_READ_HREGS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_READ_IREGS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_WRITE_COIL ->
                ValuesBin = case Req#rtu_req.quality of
                                1 ->
                                    <<16#ff, 16#00>>;
                                _ ->
                                    <<16#00, 16#00>>
                            end,
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, ValuesBin/binary>>;
            ?FC_WRITE_COILS ->
                Quantity = length(Req#rtu_req.quality),
                ValuesBin = list_bit_to_binary(Req#rtu_req.quality),
                ByteCount = length(binary_to_list(ValuesBin)),
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, Quantity:16, ByteCount:8, ValuesBin/binary>>;
            ?FC_WRITE_HREG ->
                ValueBin = list_word16_to_binary([Req#rtu_req.quality]),
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, ValueBin/binary>>;
            ?FC_WRITE_HREGS ->
%%                ValuesBin = list_word16_to_binary(Req#rtu_req.quality),
%%                寄存器个数
%%                Count = 1,
%%                写入字节数
%%                ByteCount = 2,
%%                               01                        10                  01 00           00 01 02
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.registersnumber):16, (Req#rtu_req.dataByteSize):8, (Req#rtu_req.quality):16>>;
            _ ->
                erlang:error(function_not_implemented)
        end,
    Checksum = dgiot_utils:crc16(Message),
    <<Message/binary, Checksum/binary>>.

%%%% ----------------
%%%% 解析数据值
%%parseData(FunCode, DataByteSize, Data) ->
%%    case FunCode of
%%        T when T =:= 1; T =:= 2 ->
%%            % 离散量
%%            parseCoilData(Data, DataByteSize);
%%        T when T =:= 3; T =:= 4 ->
%%            % 线圈
%%            parseRegisterData(Data)
%%    end.

%% ----------------
%% 解析离散量数据值, 两个字节（16bit）组成一个值
%%parseRegisterData(Data) ->
%%    parseRegisterData_2(Data, []).

%%parseRegisterData_2([], Rdata) ->
%%    lists:reverse(Rdata);
%%parseRegisterData_2([Hi, Lo | T], Rdata) ->
%%    <<D:16>> = <<Hi:8, Lo:8>>,
%%    parseRegisterData_2(T, [D | Rdata]).

%% ----------------
%% 解析线圈数据值, 一个字节（8bit）拆为8个值
%%parseCoilData(Data, DataByteSize) ->
%%    lists:sublist(parseCoilData_2(Data, []), DataByteSize * 8).

%%parseCoilData_2([], Rdata) ->
%%    lists:reverse(Rdata);
%%parseCoilData_2([H | T], Rdata) ->
%%    <<D8:1, D7:1, D6:1, D5:1, D4:1, D3:1, D2:1, D1:1>> = <<H:8>>,
%%    parseCoilData_2(T, [D8, D7, D6, D5, D4, D3, D2, D1 | Rdata]).


list_bit_to_binary(Values) when is_list(Values) ->
    L = length(Values),
    AlignedValues = case L rem 8 of
                        0 ->
                            Values;
                        Remainder ->
                            Values ++ [0 || _ <- lists:seq(1, 8 - Remainder)]
                    end,
    list_to_binary(
        bit_as_bytes(AlignedValues)
    ).

bit_as_bytes(L) when is_list(L) ->
    bit_as_bytes(L, []).

bit_as_bytes([], Res) ->
    lists:reverse(Res);
bit_as_bytes([B0, B1, B2, B3, B4, B5, B6, B7 | Rest], Res) ->
    bit_as_bytes(Rest, [<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> | Res]).

list_word16_to_binary(Values) when is_list(Values) ->
    list_to_binary(
        lists:map(
            fun(X) ->
                RoundedValue = round(X),
                <<RoundedValue:16>>
            end,
            Values
        )
    ).

modbus_decoder(ProductId, SlaveId, Address, Data, Acc1) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"strategy">> := Strategy,
                            <<"protocol">> := <<"MODBUSRTU">>},
                        <<"dataSource">> := #{
                            <<"slaveid">> := OldSlaveid,
                            <<"address">> := OldAddress}
                    } when Strategy =/= <<"计算值"/utf8>> ->
                        <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldSlaveid)),
                        <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldAddress)),
                        NewSlaveid = H * 256 + L,
                        NewAddress = Sh * 256 + Sl,
                        case {SlaveId, Address} of
                            {NewSlaveid, NewAddress} ->
                                case catch format_value(Data, X, Props) of
                                    {map, Value} ->
                                        maps:merge(Acc, Value);
                                    {Value, _Rest} ->
                                        Acc#{Identifier => Value};
                                    _A ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
                        end, Acc1, Props);
        _ -> #{}
    end.

modbus_encoder(ProductId, SlaveId, Address, Value) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"accessMode">> := <<"r">>,
                        <<"dataSource">> := #{<<"address">> := Address, <<"data">> := Data, <<"slaveid">> := SlaveId, <<"operatetype">> := Operatetype},
                        <<"dataForm">> := #{<<"protocol">> := <<"MODBUSRTU">>}
                    } ->
                        Acc ++ [{<<"r">>, Data, Operatetype}];
                    #{<<"accessMode">> := Cmd,
                        <<"dataSource">> := #{<<"address">> := Address, <<"data">> := _Quantity, <<"slaveid">> := SlaveId, <<"operatetype">> := Operatetype},
                        <<"dataForm">> := #{<<"protocol">> := <<"MODBUSRTU">>}
                    } ->
                        Acc ++ [{Cmd, Value, Operatetype}];
                    _Ot ->
                        Acc
                end
                        end, [], Props);
        Error ->
            ?LOG(info, "~p", [Error]),
            []
    end.

%% 1)大端模式：Big-Endian就是高位字节排放在内存的低地址端，低位字节排放在内存的高地址端。
%% （其实大端模式才是我们直观上认为的模式，和字符串存储的模式差类似）
%% 低地址 --------------------> 高地址
%% 0x12  |  0x34  |  0x56  |  0x78
%% 2)小端模式：Little-Endian就是低位字节排放在内存的低地址端，高位字节排放在内存的高地址端。
%% 低地址 --------------------> 高地址
%% 0x78  |  0x56  |  0x34  |  0x12

format_value(Buff, #{
    <<"dataType">> := #{<<"type">> := <<"geopoint">>, <<"gpstype">> := <<"NMEA0183">>}}, _Props) ->
    {Longitude, Latitude} = dgiot_gps:nmea0183_frame(Buff),
    {<<Longitude/binary, "_", Latitude/binary>>, <<"Rest">>};

format_value(Buff, #{
    <<"accessMode">> := <<"rw">>,
    <<"dataSource">> := DataSource} = X, _Props) ->
    format_value(Buff, X#{<<"accessMode">> => <<"r">>,
        <<"dataSource">> => DataSource#{<<"data">> => byte_size(Buff)}
    }, _Props);

format_value(Buff, #{<<"identifier">> := BitIdentifier,
    <<"dataSource">> := #{
        <<"originaltype">> := <<"bit">>
    }}, Props) ->
    <<BitValue:8, _/binary>> = Buff,
    Values =
        lists:foldl(fun(X, Acc) ->
            case X of
                #{<<"identifier">> := Identifier,
                    <<"dataForm">> := #{
                        <<"protocol">> := <<"MODBUSRTU">>,
                        <<"strategy">> := <<"计算值"/utf8>>},
                    <<"dataSource">> := #{
                        <<"slaveid">> := BitIdentifier,
                        <<"address">> := Offset,
                        <<"registersnumber">> := Num,
                        <<"originaltype">> := Originaltype}
                } ->
                    IntOffset = dgiot_utils:to_int(Offset),
                    IntNum = dgiot_utils:to_int(Num),
                    IntLen = get_len(IntNum, Originaltype),
                    IntOffsetLen = get_len(IntOffset, Originaltype),
                    Value =
                        case IntOffset of
                            0 ->
                                <<V:IntLen/binary, _/binary>> = Buff,
                                case catch format_value(V, X, Props) of
                                    {Value1, _Rest} ->
                                        Value1;
                                    _ ->
                                        V
                                end;
                            _ ->
                                <<_:IntOffsetLen/binary, V:IntLen/binary, _/binary>> = Buff,
                                case catch format_value(V, X, Props) of
                                    {Value1, _Rest} ->
                                        Value1;
                                    _ ->
                                        V
                                end
                        end,
                    Acc#{Identifier => Value};
                _ ->
                    Acc
            end
                    end, #{BitIdentifier => BitValue}, Props),
    {map, Values};

format_value(Buff, #{<<"dataSource">> := #{
    <<"registersnumber">> := Num,
    <<"originaltype">> := <<"short16_AB">>
}}, _Props) ->
    IntNum = dgiot_utils:to_int(Num),
    Size = IntNum * 2 * 8,
    <<Value:Size/signed-big-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"registersnumber">> := Num,
    <<"originaltype">> := <<"short16_BA">>
}}, _Props) ->
    IntNum = dgiot_utils:to_int(Num),
    Size = IntNum * 2 * 8,
    <<Value:Size/signed-little-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"registersnumber">> := Num,
    <<"originaltype">> := <<"ushort16_AB">>
}}, _Props) ->
    IntNum = dgiot_utils:to_int(Num),
    Size = IntNum * 2 * 8,
    <<Value:Size/unsigned-big-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"registersnumber">> := Num,
    <<"originaltype">> := <<"ushort16_BA">>
}}, _Props) ->
    IntNum = dgiot_utils:to_int(Num),
    Size = IntNum * 2 * 8,
    <<Value:Size/unsigned-little-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"registersnumber">> := Num,
    <<"originaltype">> := <<"long32_ABCD">>
}}, _Props) ->
    IntNum = dgiot_utils:to_int(Num),
    Size = IntNum * 4 * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/integer>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"originaltype">> := <<"long32_CDAB">>
}}, _Props) ->
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:32/integer>> = <<L/binary, H/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"originaltype">> := <<"ulong32_ABCD">>
}}, _Props) ->
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:32/integer>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"originaltype">> := <<"ulong32_CDAB">>
}}, _Props) ->
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:32/integer>> = <<L/binary, H/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"originaltype">> := <<"float32_ABCD">>
}}, _Props) ->
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:32/float>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataSource">> := #{
    <<"originaltype">> := <<"float32_CDAB">>
}}, _Props) ->
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:32/float>> = <<L/binary, H/binary>>,
    {Value, Rest};

%% @todo 其它类型处理
format_value(Buff, _, _) ->
%%    ?LOG(info, "Field ~p", [Field]),
%%    throw({field_error, <<Field/binary, " is not validate">>}),
    <<Value:8/signed-big-integer, Rest/binary>> = Buff,
    {Value, Rest}.

%% 获取寄存器字节长度
get_len(IntNum, <<"short16_AB">>) ->
    dgiot_utils:to_int(IntNum) * 2;

get_len(IntNum, <<"short16_BA">>) ->
    dgiot_utils:to_int(IntNum) * 2;

get_len(IntNum, <<"ushort16_AB">>) ->
    dgiot_utils:to_int(IntNum) * 2;

get_len(IntNum, <<"ushort16_BA">>) ->
    dgiot_utils:to_int(IntNum) * 2;

get_len(IntNum, <<"long32_ABCD">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, <<"long32_CDAB">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, <<"ulong32_ABCD">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, <<"ulong32_CDAB">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, <<"float32_ABCD">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, <<"float32_CDAB">>) ->
    dgiot_utils:to_int(IntNum) * 4;

get_len(IntNum, _Originaltype) ->
    dgiot_utils:to_int(IntNum) * 2.


get_datasource(#{<<"operatetype">> := <<"writeHreg">>, <<"data">> := Data} = DataSource) ->
    DataSource#{<<"data">> => Data};

get_datasource(DataSource) ->
    DataSource.
