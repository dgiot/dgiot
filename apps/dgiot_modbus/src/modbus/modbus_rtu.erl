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
    parse_frame/3,
    to_frame/1,
    build_req_message/1]
).

-export([modbus_encoder/4, modbus_decoder/4, is16/1]).

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
    <<"value">> := Data,
    <<"addr">> := SlaveId,
    <<"productid">> := ProductId,
    <<"di">> := Address
}) ->
    encode_data(Data, Address, SlaveId, ProductId);

%%<<"cmd">> => Cmd,
%%<<"gateway">> => DtuAddr,
%%<<"addr">> => SlaveId,
%%<<"di">> => Address
to_frame(#{
    <<"value">> := Value,
    <<"gateway">> := DtuAddr,
    <<"addr">> := SlaveId,
    <<"di">> := Address
}) ->
    case dgiot_device:lookup(DtuAddr, SlaveId) of
        {error, not_find} -> [];
        [ProductId, _DevAddr] ->
            encode_data(Value, Address, SlaveId, ProductId)
    end.

encode_data(Data, Address, SlaveId, ProductId) ->
    lists:foldl(fun({_Cmd, Quality, OperateType}, Acc) ->
        FunCode =
            case OperateType of
                <<"readCoils">> -> ?FC_READ_COILS;
                <<"readInputs">> -> ?FC_READ_INPUTS;
                <<"readHregs">> -> ?FC_READ_HREGS;
                <<"readIregs">> -> ?FC_READ_IREGS;
                <<"writeCoil">> -> ?FC_WRITE_COIL;
                <<"writeHreg">> -> ?FC_WRITE_COILS; %%需要校验，写多个线圈是什么状态
                <<"writeCoils">> -> ?FC_WRITE_HREG;
                <<"writeHregs">> -> ?FC_WRITE_HREGS; %%需要校验，写多个保持寄存器是什么状态
                _ -> ?FC_READ_HREGS
            end,
        <<H:8, L:8>> = dgiot_utils:hex_to_binary(is16(Address)),
        <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(is16(SlaveId)),
        RtuReq = #rtu_req{
            slaveId = Sh * 256 + Sl,
            funcode = dgiot_utils:to_int(FunCode),
            address = H * 256 + L,
            quality = dgiot_utils:to_int(Quality)
        },
        Acc ++ [build_req_message(RtuReq)]
                end, [], modbus_encoder(ProductId, SlaveId, Address, Data)).

is16(<<"0X", Data/binary>>) when size(Data) == 4 ->
    Data;

is16(<<"0X", Data/binary>>) when size(Data) > 4 ->
    Data;

is16(<<"0X", Data/binary>>) ->
    <<"00", Data/binary>>;

is16(Data) when size(Data) > 1 ->
    <<"00", Data/binary>>;

is16(Data) ->
    <<"000", Data/binary>>.

%rtu modbus
parse_frame(<<>>, Acc, _State) -> {<<>>, Acc};

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
            {<<>>, Acc};
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
    case dgiot_device:lookup(DtuAddr, dgiot_utils:to_binary(SlaveId)) of
        {error, _} ->
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
    ?LOG(error, "_Other ~p", [_Other]),
    {<<>>, Acc}.

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
                    Acc1 = Acc ++ modbus_decoder(ProductId, SlaveId, Address, UserZone),
                    {Rest1, Acc1};
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
            Acc1 = Acc ++ modbus_decoder(ProductId, SlaveId, Address, UserZone),
            {<<>>, Acc1};
        false ->
            {<<>>, Acc}
    end.

build_req_message(Req) when is_record(Req, rtu_req) ->
    % validate
    if
        (Req#rtu_req.slaveId < 0) or (Req#rtu_req.slaveId > 247) ->
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
        (Req#rtu_req.quality < 1) or (Req#rtu_req.quality > 2000) ->
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
                Quantity = length(Req#rtu_req.quality),
                ValuesBin = list_word16_to_binary(Req#rtu_req.quality),
                ByteCount = length(binary_to_list(ValuesBin)),
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, Quantity:16, ByteCount:8, ValuesBin/binary>>;
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

modbus_decoder(ProductId, SlaveId, Address, Data) ->
    case dgiot_device:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"slaveid">> := OldSlaveid,
                            <<"address">> := OldAddress,
                            <<"protocol">> := <<"modbus">>
                        }} ->
                        <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldSlaveid)),
                        <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldAddress)),
                        NewSlaveid = H * 256 + L,
                        NewAddress = Sh * 256 + Sl,
                        case {SlaveId, Address} of
                            {NewSlaveid, NewAddress} ->
                                case format_value(Data, X) of
                                    {Value, _Rest} ->
                                        Acc#{Identifier => Value};
                                    _ -> Acc
                                end;
                            _ ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
                        end, #{}, Props);
        _ -> []
    end.

modbus_encoder(ProductId, SlaveId, Address, Value) ->
    case dgiot_device:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"accessMode">> := <<"r">>, <<"dataForm">> := #{<<"address">> := Address, <<"protocol">> := <<"modbus">>,
                        <<"data">> := Data, <<"slaveid">> := SlaveId, <<"operatetype">> := Operatetype}} ->
                        Acc ++ [{<<"r">>, Data, Operatetype}];
                    #{<<"accessMode">> := Cmd, <<"dataForm">> := #{<<"address">> := Address, <<"protocol">> := <<"modbus">>,
                        <<"data">> := _Quantity, <<"slaveid">> := SlaveId, <<"operatetype">> := Operatetype}} ->
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
    <<"accessMode">> := <<"rw">>,
    <<"dataForm">> := DataForm} = X) ->
    format_value(Buff, X#{<<"accessMode">> => <<"r">>,
        <<"dataForm">> => DataForm#{<<"data">> => byte_size(Buff)}
    });

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"bit">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(2, IntLen) * 8,
    <<Value:Size, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"short16_AB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(2, IntLen) * 8,
    <<Value:Size/signed-big-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"short16_BA">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(2, IntLen) * 8,
    <<Value:Size/signed-little-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ushort16_AB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(2, IntLen) * 8,
    <<Value:Size/unsigned-big-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ushort16_BA">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(2, IntLen) * 8,
    <<Value:Size/unsigned-little-integer, Rest/binary>> = Buff,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"long32_ABCD">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/signed-big-integer>> = <<H:2/binary, L:2/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"long32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/signed-little-integer>> = <<L:2/binary, H:2/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ulong32_ABCD">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/unsigned-big-integer>> = <<H:2/binary, L:2/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ulong32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/unsigned-little-integer>> = <<L:2/binary, H:2/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"float32_ABCD">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/unsigned-big-float>> = <<H:2/binary, L:2/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"float32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/unsigned-little-float>> = <<L:2/binary, H:2/binary>>,
    {Value, Rest};

%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"uint16">>
%%}}) ->
%%    Size = max(2, Len) * 8,
%%    <<Value:Size/unsigned-big-integer, Rest/binary>> = Buff,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"uint16">>}}) ->
%%    Size = max(2, Len) * 8,
%%    <<Value:Size/unsigned-little-integer, Rest/binary>> = Buff,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"int16">>}}) ->
%%    Size = max(2, Len) * 8,
%%    <<Value:Size/signed-big-integer, Rest/binary>> = Buff,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"int16">>}}) ->
%%    Size = max(2, Len) * 8,
%%    <<Value:Size/signed-little-integer, Rest/binary>> = Buff,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"uint32">>}
%%}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-integer>> = <<H:2/binary, L:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"uint32">>}
%%}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-integer>> = <<L:2/binary, H:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"int32">>}
%%}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/signed-big-integer>> = <<H:2/binary, L:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"int32">>}}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/signed-big-integer>> = <<L:2/binary, H:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"float">>}}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-float>> = <<H:2/binary, L:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"float">>}}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-float>> = <<L:2/binary, H:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"double">>}}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-float>> = <<H:2/binary, L:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"double">>}}) ->
%%    Size = max(4, Len) * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/unsigned-big-float>> = <<L:2/binary, H:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>,
%%    <<"originaltype">> := <<"string">>}}) ->
%%    Size = Len * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/big-binary>> = <<H:2/binary, L:2/binary>>,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>,
%%    <<"originaltype">> := <<"string">>}}) ->
%%    Size = Len * 8,
%%    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
%%    <<Value:Size/big-binary>> = <<L:2/binary, H:2/binary>>,
%%    {Value, Rest};
%%
%%%% customized data（按大端顺序返回hex data）
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"big">>}}) ->
%%    <<Value:Len/big-binary, Rest/binary>> = Buff,
%%    {Value, Rest};
%%
%%format_value(Buff, #{<<"dataForm">> := #{
%%    <<"quantity">> := Len,
%%    <<"byteorder">> := <<"little">>}}) ->
%%    <<Value:Len/little-binary, Rest/binary>> = Buff,
%%    {Value, Rest};

%% @todo 其它类型处理
format_value(_, #{<<"identifier">> := Field}) ->
    ?LOG(info, "Field ~p", [Field]),
    throw({field_error, <<Field/binary, " is not validate">>}).
