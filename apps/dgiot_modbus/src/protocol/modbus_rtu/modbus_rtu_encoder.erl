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

-module(modbus_rtu_encoder).
-author("jonhl").

-include("dgiot_modbus.hrl").

-export([
    encode_data/5,
    build_req_message/1,
    get_funcode/1,
    process_encoder_props/5,
    modbus_encoder/4
]).

%% @doc Modbus RTU编码器模块
%% 负责Modbus RTU协议数据的编码和请求帧构建
%% 支持多种功能码和数据类型，包括读写操作

%% @doc 编码Modbus RTU数据
%% 根据配置参数生成Modbus RTU协议请求帧
%% 参数: Quality - 寄存器数量/数据值, Address - 地址, SlaveId - 从机ID, OperateType - 操作类型, Originaltype - 数据类型
%% 返回: 编码后的Modbus RTU请求帧
encode_data(Quality, Address, SlaveId, OperateType, Originaltype) ->
    FunCode =
        case OperateType of
            <<"readCoils">> -> ?FC_READ_COILS;
            <<"readInputs">> -> ?FC_READ_INPUTS;
            <<"readHregs">> -> ?FC_READ_HREGS;
            <<"readIregs">> -> ?FC_READ_IREGS;
            <<"writeCoil">> -> ?FC_WRITE_COIL;
            <<"writeHreg">> -> ?FC_WRITE_HREG;
            <<"writeCoils">> -> ?FC_WRITE_COILS;
            <<"writeHregs">> -> ?FC_WRITE_HREGS;
            _ -> ?FC_READ_HREGS
        end,
    <<H:8, L:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(Address)),
    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(SlaveId)),
    NewQuality = dgiot_utils:to_int(modbus_rtu_utils:get_len(Quality, Originaltype) / 2),
    RtuReq = #rtu_req{
        slaveId = Sh * 256 + Sl,
        funcode = dgiot_utils:to_int(FunCode),
        address = H * 256 + L,
        quality = NewQuality
    },
    build_req_message(RtuReq).

%% @doc 构建Modbus RTU请求消息
%% 根据请求参数构建完整的Modbus RTU请求帧
%% 参数: Req - rtu_req记录
%% 返回: 包含CRC校验的完整请求帧
build_req_message(Req) when is_record(Req, rtu_req) ->
    % 参数验证
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
    
    % 根据功能码构建消息
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
                ValuesBin = modbus_rtu_utils:list_bit_to_binary(Req#rtu_req.quality),
                ByteCount = length(binary_to_list(ValuesBin)),
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, Quantity:16, ByteCount:8, ValuesBin/binary>>;
            ?FC_WRITE_HREG ->
                ValueBin = modbus_rtu_utils:list_word16_to_binary([Req#rtu_req.quality]),
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, ValueBin/binary>>;
            ?FC_WRITE_HREGS ->
                <<(Req#rtu_req.slaveId):8, (Req#rtu_req.funcode):8, (Req#rtu_req.address):16, 
                  (Req#rtu_req.registersnumber):16, (Req#rtu_req.dataByteSize):8, (Req#rtu_req.quality):16>>;
            _ ->
                erlang:error(function_not_implemented)
        end,
    
    % 计算并添加CRC校验
    Checksum = dgiot_utils:crc16(Message),
    <<Message/binary, Checksum/binary>>.

%% @doc 获取功能码
%% 根据操作类型字符串返回对应的Modbus功能码
%% 参数: OperateType - 操作类型字符串
%% 返回: 功能码二进制
get_funcode(<<"readCoils">>) -> ?FC_READ_COILS;
get_funcode(<<"readInputs">>) -> ?FC_READ_INPUTS;
get_funcode(<<"readHregs">>) -> ?FC_READ_HREGS;
get_funcode(<<"readIregs">>) -> ?FC_READ_IREGS;
get_funcode(<<"writeCoil">>) -> ?FC_WRITE_COIL;
get_funcode(<<"writeHreg">>) -> ?FC_WRITE_HREG;
get_funcode(<<"writeCoils">>) -> ?FC_WRITE_COILS;
get_funcode(<<"writeHregs">>) -> ?FC_WRITE_HREGS;
get_funcode(_) -> ?FC_READ_HREGS.

%% @doc Modbus编码器主函数
%% 根据产品配置编码Modbus RTU数据
%% 参数: ProductId - 产品ID, SlaveId - 从机ID, Address - 地址, Value - 值
%% 返回: 编码后的属性列表
modbus_encoder(ProductId, SlaveId, Address, Value) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            process_encoder_props(Props, SlaveId, Address, Value, []);
        Error ->
            io:format("~s ~p Error in modbus_encoder: ~p~n", [?FILE, ?LINE, Error]),
            []
    end.

%% @doc 递归处理编码器属性
%% 遍历属性列表，处理符合条件的属性配置
%% 参数: Props - 属性列表, SlaveId - 从机ID, Address - 地址, Value - 值, Acc - 累积结果
%% 返回: 处理后的属性列表
process_encoder_props([], _SlaveId, _Address, _Value, Acc) -> Acc;
process_encoder_props([X | Rest], SlaveId, Address, Value, Acc) ->
    NewAcc = case X of
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
        _ ->
            Acc
    end,
    process_encoder_props(Rest, SlaveId, Address, Value, NewAcc).
