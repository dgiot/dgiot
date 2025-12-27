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

-module(modbus_rtu_decoder).
-author("jonhl").

-include("dgiot_modbus.hrl").

-export([
    modbus_decoder/5,
    preprocess_data_fragments/4,
    extract_data_fragment/2,
    process_decoder_props/5,
    process_calculated_props/3,
    process_calculated_properties/5,
    parse_by_format/3,
    parse_single_format/2,
    parse_default/1,
    format_value/3,
    is_hex_format/1
]).

%% @doc Modbus RTU数据解码器
%% 负责Modbus RTU协议数据的解码和属性提取
%% 支持多种数据格式和计算值属性处理

%% @doc 主解码函数
%% 根据产品配置解码Modbus RTU数据
%% 参数: ProductId - 产品ID, SlaveId - 从机ID, Address - 地址, Data - 数据, Acc1 - 累积结果
%% 返回: 解码后的属性映射
modbus_decoder(ProductId, SlaveId, Address, Data, Acc1) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            % 预处理数据块，为每个属性提取对应的数据片段
            DataFragments = preprocess_data_fragments(Props, Data, SlaveId, Address),
            % 先处理非计算值属性
            NonCalculatedProps = lists:filter(fun(Prop) ->
                case Prop of
                    #{<<"dataForm">> := #{<<"strategy">> := Strategy}} ->
                        Strategy =/= <<"计算值"/utf8>>;
                    _ ->
                        true
                end
            end, Props),
            Acc2 = process_decoder_props(NonCalculatedProps, SlaveId, Address, DataFragments, Acc1),
            % 然后处理计算值属性，基于已解析的属性值
            process_calculated_props(Props, Acc2, DataFragments);
        _ -> #{}
    end.

%% @doc 预处理数据片段
%% 根据属性配置计算每个属性在数据块中的位置和长度
%% 只处理数字格式的slaveid，跳过标识符格式的slaveid（用于计算值属性）
preprocess_data_fragments(Props, Data, SlaveId, Address) ->
    lists:foldl(fun(Prop, Acc) ->
        case Prop of
            #{<<"identifier">> := Identifier,
              <<"dataSource">> := #{<<"slaveid">> := OldSlaveid, <<"address">> := OldAddress}}
            ->
                try
                    % 检查slaveid是否为数字格式（16进制）
                    case is_hex_format(OldSlaveid) of
                        true ->
                            SlaveIdHex = modbus_rtu_utils:is16(OldSlaveid),
                            AddressHex = modbus_rtu_utils:is16(OldAddress),
                            case byte_size(SlaveIdHex) =:= 4 andalso byte_size(AddressHex) =:= 4 of
                                true ->
                                    <<H:8, L:8>> = dgiot_utils:hex_to_binary(SlaveIdHex),
                                    <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(AddressHex),
                                    NewSlaveid = H * 256 + L,
                                    NewAddress = Sh * 256 + Sl,
                                    case {SlaveId, Address} of
                                        {NewSlaveid, NewAddress} ->
                                            case extract_data_fragment(Prop, Data) of
                                                {ok, Fragment} ->
                                                    Acc#{Identifier => Fragment};
                                                _ ->
                                                    Acc
                                            end;
                                        _ ->
                                            Acc
                                    end;
                                false ->
                                    io:format("~s ~p Invalid hex format for property ~p: SlaveId=~p (hex=~p), Address=~p (hex=~p)~n", 
                                             [?FILE, ?LINE, Identifier, OldSlaveid, SlaveIdHex, OldAddress, AddressHex]),
                                    Acc
                            end;
                        false ->
                            % 标识符格式的slaveid（如<<"meter">>）跳过预处理，留给计算值属性处理
                            Acc
                    end
                catch
                    _:Error ->
                        io:format("~s ~p Error processing property ~p: ~p (OldSlaveid=~p, OldAddress=~p)~n", 
                                 [?FILE, ?LINE, Identifier, Error, OldSlaveid, OldAddress]),
                        Acc
                end;
            _ ->
                Acc
        end
    end, #{}, Props).

%% @doc 提取数据片段
%% 根据属性配置从数据块中提取对应的数据片段
extract_data_fragment(#{<<"dataSource">> := DataSource}, Data) ->
    case DataSource of
        #{<<"registersnumber">> := Num, <<"originaltype">> := Originaltype} ->
            IntNum = dgiot_utils:to_int(Num),
            % get_len/2 现在对于bit类型返回字节数，对于其他类型也返回字节数
            RequiredBytes = modbus_rtu_utils:get_len(IntNum, Originaltype),
            case byte_size(Data) >= RequiredBytes of
                true ->
                    <<Fragment:RequiredBytes/binary, _/binary>> = Data,
                    {ok, Fragment};
                false ->
                    {error, insufficient_data}
            end;
        #{<<"originaltype">> := Originaltype} ->
            case Originaltype of
                <<"bit">> -> 
                    % 对于bit类型，需要根据registersnumber计算长度
                    case DataSource of
                        #{<<"registersnumber">> := Num} ->
                            IntNum = dgiot_utils:to_int(Num),
                            IntLen = modbus_rtu_utils:get_len(IntNum, Originaltype),
                            ByteLen = (IntLen + 7) div 8,  % 位数转换为字节数
                            case byte_size(Data) >= ByteLen of
                                true -> 
                                    <<Fragment:ByteLen/binary, _/binary>> = Data,
                                    {ok, Fragment};
                                false -> {error, insufficient_data}
                            end;
                        _ ->
                            % 没有registersnumber，默认提取1字节
                            case byte_size(Data) >= 1 of
                                true -> 
                                    <<Fragment:1/binary, _/binary>> = Data,
                                    {ok, Fragment};
                                false -> {error, insufficient_data}
                            end
                    end;
                <<"raw">> -> 
                    {ok, Data};
                _ -> 
                    case byte_size(Data) >= 2 of
                        true -> 
                            <<Fragment:2/binary, _/binary>> = Data,
                            {ok, Fragment};
                        false -> {error, insufficient_data}
                    end
            end;
        _ ->
            case byte_size(Data) >= 1 of
                true -> 
                    <<Fragment:1/binary, _/binary>> = Data,
                    {ok, Fragment};
                false -> {error, insufficient_data}
            end
    end.

%% @doc 递归处理解码器属性
process_decoder_props([], _SlaveId, _Address, _DataFragments, Acc) -> Acc;
process_decoder_props([X | Rest], SlaveId, Address, DataFragments, Acc) ->
    NewAcc = case X of
        #{<<"identifier">> := Identifier,
          <<"dataForm">> := #{
              <<"strategy">> := Strategy,
              <<"protocol">> := <<"MODBUSRTU">>},
          <<"dataSource">> := #{
              <<"slaveid">> := OldSlaveid,
              <<"address">> := OldAddress}
        } when Strategy =/= <<"计算值"/utf8>> ->
            try
                % 检查slaveid是否为数字格式（16进制）
                case is_hex_format(OldSlaveid) of
                    true ->
                        SlaveIdHex = modbus_rtu_utils:is16(OldSlaveid),
                        AddressHex = modbus_rtu_utils:is16(OldAddress),
                        case byte_size(SlaveIdHex) =:= 4 andalso byte_size(AddressHex) =:= 4 of
                            true ->
                                <<H:8, L:8>> = dgiot_utils:hex_to_binary(SlaveIdHex),
                                <<Sh:8, Sl:8>> = dgiot_utils:hex_to_binary(AddressHex),
                                NewSlaveid = H * 256 + L,
                                NewAddress = Sh * 256 + Sl,
                                case {SlaveId, Address} of
                                    {NewSlaveid, NewAddress} ->
                                        case maps:get(Identifier, DataFragments, undefined) of
                                            undefined ->
                                                Acc;
                                            DataFragment ->
                                                case catch format_value(DataFragment, X, Rest) of
                                                    {map, Value} ->
                                                        maps:merge(Acc, Value);
                                                    {Value, _Rest} ->
                                                        Acc#{Identifier => Value};
                                                    _A ->
                                                        Acc
                                                end
                                        end;
                                    _ ->
                                        Acc
                                end;
                            false ->
                                io:format("~s ~p Invalid hex format in process_decoder_props for property ~p: SlaveId=~p (hex=~p), Address=~p (hex=~p)~n", 
                                         [?FILE, ?LINE, Identifier, OldSlaveid, SlaveIdHex, OldAddress, AddressHex]),
                                Acc
                        end;
                    false ->
                        % 标识符格式的slaveid（如<<"meter">>）跳过处理，留给计算值属性处理
                        Acc
                end
            catch
                _:Error ->
                    io:format("~s ~p Error in process_decoder_props for property ~p: ~p (OldSlaveid=~p, OldAddress=~p)~n", 
                             [?FILE, ?LINE, Identifier, Error, OldSlaveid, OldAddress]),
                    Acc
            end;
        _ ->
            Acc
    end,
    process_decoder_props(Rest, SlaveId, Address, DataFragments, NewAcc).

%% @doc 处理计算值属性
%% 基于已解析的非计算值属性来处理计算值属性
process_calculated_props([], Acc, _DataFragments) -> Acc;
process_calculated_props([X | Rest], Acc, DataFragments) ->
    NewAcc = case X of
        #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>,
                             <<"protocol">> := <<"MODBUSRTU">>},
          <<"identifier">> := Identifier,
          <<"dataSource">> := #{<<"slaveid">> := BitIdentifier,
                               <<"address">> := Offset,
                               <<"registersnumber">> := Num,
                               <<"originaltype">> := Originaltype}
        } ->
            % 从已解析的属性中获取基础值
            case maps:get(BitIdentifier, Acc, undefined) of
                undefined ->
                    Acc;
                BaseValue ->
                    try
                        % 使用 is16/1 安全地转换偏移量和寄存器数量
                        <<OffsetH:8, OffsetL:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(Offset)),
                        IntOffset = OffsetH * 256 + OffsetL,
                        <<NumH:8, NumL:8>> = dgiot_utils:hex_to_binary(modbus_rtu_utils:is16(Num)),
                        IntNum = NumH * 256 + NumL,
                        IntLen = modbus_rtu_utils:get_len(IntNum, Originaltype),
                        
                        % 根据偏移量计算新值
                        case BaseValue of
                            Value when is_binary(Value) ->
                                case byte_size(Value) >= IntOffset + IntLen of
                                    true ->
                                        <<_:IntOffset/binary, Fragment:IntLen/binary, _/binary>> = Value,
                                        case catch format_value(Fragment, X, []) of
                                            {Value1, _Rest} ->
                                                Acc#{Identifier => Value1};
                                            _ ->
                                                Acc
                                        end;
                                    false ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end
                    catch
                        _:Error ->
                            io:format("~s ~p Error processing calculated property ~p: ~p (Offset=~p, Num=~p)~n", 
                                     [?FILE, ?LINE, Identifier, Error, Offset, Num]),
                            Acc
                    end
            end;
        _ ->
            Acc
    end,
    process_calculated_props(Rest, NewAcc, DataFragments).

%% @doc 递归处理计算属性
%% 处理策略为"计算值"的属性，从数据块中提取子数据点
process_calculated_properties([], _Buff, BitIdentifier, BitValue, _Props) ->
    #{BitIdentifier => BitValue};

process_calculated_properties([X | Rest], Buff, BitIdentifier, BitValue, Props) ->
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
            IntLen = modbus_rtu_utils:get_len(IntNum, Originaltype),
            IntOffsetLen = modbus_rtu_utils:get_len(IntOffset, Originaltype),
            Value =
                case IntOffset of
                    0 ->
                        <<V:IntLen/binary, _/binary>> = Buff,
                        case format_value(V, X, []) of
                            {Value1, _Rest} ->
                                Value1;
                            _ ->
                                V
                        end;
                    _ ->
                        <<_:IntOffsetLen/binary, V:IntLen/binary, _/binary>> = Buff,
                        case format_value(V, X, []) of
                            {Value1, _Rest} ->
                                Value1;
                            _ ->
                                V
                        end
                end,
            Acc = process_calculated_properties(Rest, Buff, BitIdentifier, BitValue, Props),
            Acc#{Identifier => Value};
        _ ->
            process_calculated_properties(Rest, Buff, BitIdentifier, BitValue, Props)
    end.

%% @doc 字节序说明
%% 1)大端模式：Big-Endian就是高位字节排放在内存的低地址端，低位字节排放在内存的高地址端。
%% （其实大端模式才是我们直观上认为的模式，和字符串存储的模式差类似）
%% 低地址 --------------------> 高地址
%% 0x12  |  0x34  |  0x56  |  0x78
%% 2)小端模式：Little-Endian就是低位字节排放在内存的低地址端，高位字节排放在内存的高地址端。
%% 低地址 --------------------> 高地址
%% 0x78  |  0x56  |  0x34  |  0x12

format_value(Buff, #{<<"dataType">> := #{<<"type">> := <<"geopoint">>, <<"gpstype">> := <<"NMEA0183">>}}, _Props) ->
    {Longitude, Latitude} = dgiot_gps:nmea0183_frame(Buff),
    {<<Longitude/binary, "_", Latitude/binary>>, <<"Rest">>};

format_value(Buff, #{<<"accessMode">> := <<"rw">>, <<"dataSource">> := DataSource} = X, _Props) ->
    format_value(Buff, X#{<<"accessMode">> => <<"r">>,
        <<"dataSource">> => DataSource#{<<"data">> => byte_size(Buff)}
    }, _Props);

format_value(Buff, #{<<"identifier">> := BitIdentifier,
    <<"dataSource">> := #{<<"originaltype">> := <<"bit">>}}, Props) ->
    % 对于bit类型，返回整个二进制数据，让process_calculated_properties处理偏移量
    Values = process_calculated_properties(Props, Buff, BitIdentifier, Buff, []),
    {map, Values};

format_value(Buff, #{<<"identifier">> := RawIdentifier,
    <<"dataSource">> := #{<<"originaltype">> := <<"raw">>}}, Props) ->
    Values = process_calculated_properties(Props, Buff, RawIdentifier, Buff, Props),
    {map, Values};

format_value(Buff, #{<<"dataSource">> := DataSource}, _Props) ->
    case DataSource of
        #{<<"registersnumber">> := Num, <<"originaltype">> := Originaltype} ->
            parse_by_format(Buff, Num, Originaltype);
        #{<<"originaltype">> := Originaltype} ->
            parse_single_format(Buff, Originaltype);
        _ ->
            parse_default(Buff)
    end.

%% @doc 根据数据格式和寄存器数量解析数据
parse_by_format(Buff, Num, Originaltype) ->
    IntNum = dgiot_utils:to_int(Num),
    case Originaltype of
        <<"short16_AB">> ->
            Size = IntNum * 2 * 8,
            <<Value:Size/signed-big-integer, Rest/binary>> = Buff,
            {Value, Rest};
        <<"short16_BA">> ->
            Size = IntNum * 2 * 8,
            <<Value:Size/signed-little-integer, Rest/binary>> = Buff,
            {Value, Rest};
        <<"ushort16_AB">> ->
            Size = IntNum * 2 * 8,
            <<Value:Size/unsigned-big-integer, Rest/binary>> = Buff,
            {Value, Rest};
        <<"ushort16_BA">> ->
            Size = IntNum * 2 * 8,
            <<Value:Size/unsigned-little-integer, Rest/binary>> = Buff,
            {Value, Rest};
        <<"long32_ABCD">> ->
            Size = IntNum * 4 * 8,
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:Size/integer>> = <<H/binary, L/binary>>,
            {Value, Rest};
        _ ->
            parse_single_format(Buff, Originaltype)
    end.

%% @doc 解析单个数据格式
parse_single_format(Buff, Originaltype) ->
    case Originaltype of
        <<"long32_CDAB">> ->
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:32/integer>> = <<L/binary, H/binary>>,
            {Value, Rest};
        <<"ulong32_ABCD">> ->
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:32/integer>> = <<H/binary, L/binary>>,
            {Value, Rest};
        <<"ulong32_CDAB">> ->
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:32/integer>> = <<L/binary, H/binary>>,
            {Value, Rest};
        <<"float32_ABCD">> ->
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:32/float>> = <<H/binary, L/binary>>,
            {Value, Rest};
        <<"float32_CDAB">> ->
            <<H:2/binary, L:2/binary, Rest/binary>> = Buff,
            <<Value:32/float>> = <<L/binary, H/binary>>,
            {Value, Rest};
        _ ->
            parse_default(Buff)
    end.

%% @doc 默认解析方式
parse_default(Buff) ->
    <<Value:8/signed-big-integer, Rest/binary>> = Buff,
    {Value, Rest}.

%% @doc 检查是否为16进制格式
%% 判断二进制字符串是否只包含16进制字符（0-9, A-F, a-f）
is_hex_format(Bin) when is_binary(Bin) ->
    case byte_size(Bin) of
        0 -> false;
        _ ->
            try
                % 尝试将二进制转换为16进制字符串
                HexStr = binary_to_list(Bin),
                lists:all(fun(Char) -> 
                    (Char >= $0 andalso Char =< $9) orelse
                    (Char >= $A andalso Char =< $F) orelse
                    (Char >= $a andalso Char =< $f)
                end, HexStr)
            catch
                _:_ -> false
            end
    end;
is_hex_format(_) -> false.
