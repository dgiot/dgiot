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

-module(modbus_tcp).
-author("jonhl").

-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    init/1,
    parse_frame/3,
    to_frame/1,
    build_req_message/1]
).

-export([modbus_encoder/4, modbus_decoder/5, is16/1, set_params/3, decode_data/4]).

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
    case dgiot_device:get_subdevice(DtuAddr, SlaveId) of
        not_find -> [];
        [ProductId, _DevAddr] ->
            encode_data(Value, Address, SlaveId, ProductId)
    end.

%% Quality 读的时候代表寄存器个数，16位的寄存器，一个寄存器表示两个字节，写的时候代表实际下发值
encode_data(Data, Address, SlaveId, ProductId) ->
    lists:foldl(fun({_Cmd, Quality, OperateType}, Acc) ->
        {FunCode, NewQuality} =
            case OperateType of
                <<"readCoils">> -> {?FC_READ_COILS, dgiot_utils:to_int(dgiot_utils:to_int(Quality) / 2)};
                <<"readInputs">> -> {?FC_READ_INPUTS, dgiot_utils:to_int(dgiot_utils:to_int(Quality) / 2)};
                <<"readHregs">> -> {?FC_READ_HREGS, dgiot_utils:to_int(dgiot_utils:to_int(Quality) / 2)};
                <<"readIregs">> -> {?FC_READ_IREGS, dgiot_utils:to_int(dgiot_utils:to_int(Quality) / 2)};
                <<"writeCoil">> -> {?FC_WRITE_COIL, dgiot_utils:to_int(Quality)};
                <<"writeHreg">> -> {?FC_WRITE_HREG, dgiot_utils:to_int(Quality)}; %%需要校验，写多个线圈是什么状态
                <<"writeCoils">> -> {?FC_WRITE_COILS, dgiot_utils:to_int(Quality)};
                <<"writeHregs">> -> {?FC_WRITE_HREGS, dgiot_utils:to_int(Quality)}; %%需要校验，写多个保持寄存器是什么状态
                _ -> {?FC_READ_HREGS, dgiot_utils:to_int(dgiot_utils:to_int(Quality) / 2)}
            end,
        <<H:8, L:8>> = dgiot_utils:hex_to_binary(is16(Address)),
        <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(is16(SlaveId)),
        RtuReq = #rtu_req{
            slaveId = Sh * 256 + Sl,
            funcode = dgiot_utils:to_int(FunCode),
            address = H * 256 + L,
            quality = NewQuality
        },
        Acc ++ [build_req_message(RtuReq)]
                end, [], modbus_encoder(ProductId, SlaveId, Address, Data)).

is16(<<"0X", Data/binary>>) when size(Data) == 4 ->
    Data;

is16(<<"0X", Data/binary>>) when size(Data) > 4 ->
    Data;

is16(<<"0X", Data/binary>>) ->
    <<"00", Data/binary>>;

is16(<<"00", Data/binary>>) when size(Data) == 2 ->
    Data;

is16(<<"256">>) ->
    <<"0100">>;

is16(Data) when size(Data) > 1 ->
    <<"00", Data/binary>>;

is16(Data) ->
    <<"000", Data/binary>>.

set_params(Basedata, ProductId, DevAddr) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"name">> := Productname, <<"config">> := #{<<"basedate">> := #{<<"params">> := Params}}}} ->
            Payloads =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"identifier">> := Identifier,
                            <<"protocol">> := <<"modbusTcp">>,
                            <<"slaveid">> := SlaveId,
                            <<"address">> := Address,
                            <<"bytes">> := Bytes,
                            <<"operatetype">> := OperateType,
                            <<"setting">> := Setting,
                            <<"name">> := Name} ->
                            case maps:find(Identifier, Basedata) of
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
                                    Str1 = re:replace(Setting, "%s", "(" ++ dgiot_utils:to_list(Value) ++ ")", [global, {return, list}]),
                                    Value1 = dgiot_utils:to_int(dgiot_task:string2value(Str1)),
%%                                    NewBt = Bytes * 8,
                                    Registersnumber = maps:get(<<"registersnumber">>, X, <<"1">>),
                                    RtuReq = #rtu_req{
                                        slaveId = Sh * 256 + Sl,
                                        funcode = dgiot_utils:to_int(FunCode),
                                        address = H * 256 + L,
                                        registersnumber = dgiot_utils:to_int(Registersnumber),
                                        dataByteSize = dgiot_utils:to_int(Bytes),
                                        quality = Value1
                                    },
                                    DeviceId = dgiot_parse:get_deviceid(ProductId, DevAddr),
                                    Sessiontoken = maps:get(<<"sessiontoken">>, Basedata, <<"">>),
                                    {Username, Acl} =
                                        case dgiot_auth:get_session(Sessiontoken) of
                                            #{<<"username">> := Name1, <<"ACL">> := Acl1} ->
                                                {Name1, Acl1};
                                            _ ->
                                                {<<"">>, #{}}
                                        end,
                                    ?MLOG(info, #{<<"clientid">> => DeviceId, <<"username">> => Username,
                                        <<"status">> => <<"ONLINE">>, <<"ACL">> => Acl,
                                        <<"devaddr">> => DevAddr, <<"productid">> => ProductId,
                                        <<"productname">> => Productname, <<"thingname">> => Name,
                                        <<"protocol">> => <<"modbusTcp">>, <<"identifier">> => Identifier, <<"value">> => Value1},
                                        ['device_operationlog']),
                                    Acc ++ [build_req_message(RtuReq)];
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
                            end, [], Params),
            Payloads;
        _ ->
            ?LOG(info, "NoProduct: ~p", [ProductId]),
            pass
    end.

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
            {<<>>, #{}};
        false ->
            parse_frame(Buff, Acc, State)
    end;

%% modbustcp
%% Buff = <<"000100000006011000000001">>,
parse_frame(<<_TransactionId:16, _ProtocolId:16, Size:16, _ResponseData:Size/bytes>> = Buff, Acc, #{<<"dtuproduct">> := ProductId, <<"address">> := Address} = State) ->
    io:format("~s ~p _TransactionId = ~p.~n", [?FILE, ?LINE, _TransactionId]),
    case decode_data(Buff, ProductId, Address, Acc) of
        {Rest1, Acc1} ->
            parse_frame(Rest1, Acc1, State);
        [Buff, Acc] ->
            [Buff, Acc]
    end;

%% 传感器直接做为dtu物模型的一个指标
parse_frame(<<SlaveId:8, _/binary>> = Buff, Acc, #{<<"dtuproduct">> := ProductId, <<"slaveId">> := SlaveId, <<"dtuaddr">> := _DtuAddr, <<"address">> := Address} = State) ->
    case decode_data(Buff, ProductId, Address, Acc) of
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
            case decode_data(Buff, ProductId, Address, Acc) of
                {Rest1, Acc1} ->
                    parse_frame(Rest1, Acc1, State);
                [Buff, Acc] ->
                    {Buff, Acc}
            end
    end;
%rtu modbus
parse_frame(_Other, Acc, _State) ->
    io:format("~s ~p _Other = ~p.~n", [?FILE, ?LINE, _Other]),
    {error, Acc}.

decode_data(<<_TransactionId:16, _ProtocolId:16, _Size1:16, Slaveid:8, _FunCode:8, DataLen:8, Data:DataLen/bytes>>, ProductId, Address, Acc) ->
    {<<>>, modbus_tcp_decoder(ProductId, Slaveid, Address, Data, Acc)};

decode_data(Buff, _ProductId, _Address, Acc) ->
    io:format("~s ~p errorBuff = ~p.~n", [?FILE, ?LINE, Buff]),
    {<<>>, Acc}.

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
        (Req#rtu_req.quality < 0) or (Req#rtu_req.quality > 2000) ->
            throw({argumentError, Req#rtu_req.quality});
        true -> ok
    end,
    Message =
        case Req#rtu_req.funcode of
            ?FC_READ_COILS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_READ_INPUTS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>;
            ?FC_READ_HREGS -> %% 读保持寄存器
%%              事务处理标识 随机值
                <<R:1/binary, _/binary>> = dgiot_utils:random(),
                Transaction = dgiot_utils:to_int(dgiot_utils:binary_to_hex(R)),
                %% 协议标识符 00 00表示ModbusTCP协议。
                Protocol = 0,
                Data = <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, (Req#rtu_req.quality):16>>,
                <<Transaction:16, Protocol:16, (size(Data)):16, Data/binary>>;
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
%%    Checksum = dgiot_utils:crc16(Message),
    Message.

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

modbus_tcp_decoder(ProductId, Slaveid, Address, Data, Acc1) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"slaveid">> := OldSlaveid,
                            <<"address">> := OldAddress,
                            <<"protocol">> := <<"modbusTcp">>
                        }} ->
                        <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldSlaveid)),
                        <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu:is16(OldAddress)),
                        NewSlaveid = H * 256 + L,
                        NewAddress = Sh * 256 + Sl,
                        case {Slaveid, Address} of
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
                        end, Acc1, Props);
        _ -> #{}
    end.

modbus_decoder(ProductId, SlaveId, Address, Data, Acc1) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataForm">> := #{
                            <<"slaveid">> := OldSlaveid,
                            <<"address">> := OldAddress,
                            <<"protocol">> := <<"modbusTcp">>
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
                        end, Acc1, Props);
        _ -> #{}
    end.

modbus_encoder(ProductId, SlaveId, Address, Value) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"accessMode">> := <<"r">>, <<"dataForm">> := #{<<"address">> := Address, <<"protocol">> := <<"modbusTcp">>,
                        <<"data">> := Data, <<"slaveid">> := SlaveId, <<"operatetype">> := Operatetype}} ->
                        Acc ++ [{<<"r">>, Data, Operatetype}];
                    #{<<"accessMode">> := Cmd, <<"dataForm">> := #{<<"address">> := Address, <<"protocol">> := <<"modbusTcp">>,
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
    <<"dataType">> := #{<<"type">> := <<"geopoint">>, <<"gpstype">> := <<"NMEA0183">>}}) ->
    {Longitude, Latitude} = dgiot_gps:nmea0183_frame(Buff),
    {<<Longitude/binary, "_", Latitude/binary>>, <<"Rest">>};

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
    <<Value:Size/integer>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"long32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/integer>> = <<L/binary, H/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ulong32_ABCD">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/integer>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"ulong32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/integer>> = <<L/binary, H/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"float32_ABCD">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/float>> = <<H/binary, L/binary>>,
    {Value, Rest};

format_value(Buff, #{<<"dataForm">> := #{
    <<"data">> := Len,
    <<"originaltype">> := <<"float32_CDAB">>
}}) ->
    IntLen = dgiot_utils:to_int(Len),
    Size = max(4, IntLen) * 8,
    <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
    <<Value:Size/float>> = <<L/binary, H/binary>>,
    {Value, Rest};

%% @todo 其它类型处理
format_value(_, #{<<"identifier">> := Field}) ->
    ?LOG(info, "Field ~p", [Field]),
    throw({field_error, <<Field/binary, " is not validate">>}).
