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

-module(modbus_rtu_format).
-author("jonhl").

-include("dgiot_modbus.hrl").

-export([
    format_value/3,
    parse_by_format/3,
    parse_single_format/2,
    parse_default/1,
    process_calculated_properties/3
]).

%% @doc 格式化数值
%% 根据属性配置将原始数据转换为目标格式
format_value(DataFragment, Prop, Rest) ->
    case Prop of
        #{<<"dataForm">> := #{
            <<"protocol">> := <<"MODBUSRTU">>,
            <<"strategy">> := Strategy,
            <<"data">> := DataForm
        }} ->
            case Strategy of
                <<"计算值"/utf8>> ->
                    % 计算值处理逻辑
                    {DataFragment, Rest};
                _ ->
                    % 非计算值处理
                    parse_by_format(DataFragment, DataForm, Prop)
            end;
        _ ->
            parse_default(DataFragment)
    end.

%% @doc 根据格式解析数据
parse_by_format(DataFragment, DataForm, Prop) ->
    case DataForm of
        #{<<"order">> := Order, <<"data">> := Format} ->
            parse_with_order(DataFragment, Order, Format, Prop);
        #{<<"data">> := Format} ->
            parse_with_order(DataFragment, <<"big">>, Format, Prop);
        _ ->
            parse_default(DataFragment)
    end.

%% @doc 按字节序解析数据
parse_with_order(DataFragment, Order, Format, Prop) ->
    case Format of
        <<"int16">> ->
            parse_int16(DataFragment, Order, Prop);
        <<"uint16">> ->
            parse_uint16(DataFragment, Order, Prop);
        <<"int32">> ->
            parse_int32(DataFragment, Order, Prop);
        <<"uint32">> ->
            parse_uint32(DataFragment, Order, Prop);
        <<"float">> ->
            parse_float(DataFragment, Order, Prop);
        <<"double">> ->
            parse_double(DataFragment, Order, Prop);
        <<"bit">> ->
            parse_bit(DataFragment, Prop);
        <<"string">> ->
            parse_string(DataFragment, Prop);
        <<"raw">> ->
            parse_raw(DataFragment, Prop);
        _ ->
            parse_default(DataFragment)
    end.

%% @doc 解析16位有符号整数
parse_int16(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        2 ->
            Value = case Order of
                <<"big">> ->
                    <<V:16/signed-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:16/signed-little>> = DataFragment,
                    V;
                _ ->
                    <<V:16/signed-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析16位无符号整数
parse_uint16(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        2 ->
            Value = case Order of
                <<"big">> ->
                    <<V:16/unsigned-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:16/unsigned-little>> = DataFragment,
                    V;
                _ ->
                    <<V:16/unsigned-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析32位有符号整数
parse_int32(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        4 ->
            Value = case Order of
                <<"big">> ->
                    <<V:32/signed-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:32/signed-little>> = DataFragment,
                    V;
                _ ->
                    <<V:32/signed-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析32位无符号整数
parse_uint32(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        4 ->
            Value = case Order of
                <<"big">> ->
                    <<V:32/unsigned-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:32/unsigned-little>> = DataFragment,
                    V;
                _ ->
                    <<V:32/unsigned-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析32位浮点数
parse_float(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        4 ->
            Value = case Order of
                <<"big">> ->
                    <<V:32/float-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:32/float-little>> = DataFragment,
                    V;
                _ ->
                    <<V:32/float-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析64位浮点数
parse_double(DataFragment, Order, Prop) ->
    case byte_size(DataFragment) of
        8 ->
            Value = case Order of
                <<"big">> ->
                    <<V:64/float-big>> = DataFragment,
                    V;
                <<"little">> ->
                    <<V:64/float-little>> = DataFragment,
                    V;
                _ ->
                    <<V:64/float-big>> = DataFragment,
                    V
            end,
            apply_scaling(Value, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析位数据
parse_bit(DataFragment, Prop) ->
    case byte_size(DataFragment) of
        1 ->
            <<Bits:8>> = DataFragment,
            % 返回位数组
            BitArray = [ (Bits bsr N) band 1 || N <- lists:seq(0, 7) ],
            apply_bit_scaling(BitArray, Prop);
        _ ->
            {error, invalid_data_length}
    end.

%% @doc 解析字符串
parse_string(DataFragment, Prop) ->
    % 去除尾部的空字符
    CleanString = binary:replace(DataFragment, <<0>>, <<>>, [global]),
    apply_string_scaling(CleanString, Prop).

%% @doc 解析原始数据
parse_raw(DataFragment, _Prop) ->
    DataFragment.

%% @doc 解析单个格式
parse_single_format(DataFragment, Format) ->
    case Format of
        <<"int16">> ->
            case byte_size(DataFragment) of
                2 -> 
                    <<V:16/signed-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"uint16">> ->
            case byte_size(DataFragment) of
                2 -> 
                    <<V:16/unsigned-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"int32">> ->
            case byte_size(DataFragment) of
                4 -> 
                    <<V:32/signed-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"uint32">> ->
            case byte_size(DataFragment) of
                4 -> 
                    <<V:32/unsigned-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"float">> ->
            case byte_size(DataFragment) of
                4 -> 
                    <<V:32/float-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"double">> ->
            case byte_size(DataFragment) of
                8 -> 
                    <<V:64/float-big>> = DataFragment,
                    V;
                _ -> {error, invalid_data_length}
            end;
        <<"bit">> ->
            case byte_size(DataFragment) of
                1 -> 
                    <<Bits:8>> = DataFragment,
                    [ (Bits bsr N) band 1 || N <- lists:seq(0, 7) ];
                _ -> {error, invalid_data_length}
            end;
        <<"string">> ->
            binary:replace(DataFragment, <<0>>, <<>>, [global]);
        <<"raw">> ->
            DataFragment;
        _ ->
            DataFragment
    end.

%% @doc 默认解析
parse_default(DataFragment) ->
    case byte_size(DataFragment) of
        1 ->
            <<V:8>> = DataFragment,
            V;
        2 ->
            <<V:16/big>> = DataFragment,
            V;
        4 ->
            <<V:32/big>> = DataFragment,
            V;
        _ ->
            DataFragment
    end.

%% @doc 应用数值缩放
apply_scaling(Value, Prop) ->
    case Prop of
        #{<<"dataForm">> := #{<<"ratio">> := Ratio}} when is_number(Ratio), Ratio =/= 1 ->
            Value * Ratio;
        #{<<"dataForm">> := #{<<"offset">> := Offset}} when is_number(Offset), Offset =/= 0 ->
            Value + Offset;
        #{<<"dataForm">> := #{<<"ratio">> := Ratio, <<"offset">> := Offset}} 
          when is_number(Ratio), is_number(Offset) ->
            Value * Ratio + Offset;
        _ ->
            Value
    end.

%% @doc 应用位缩放
apply_bit_scaling(BitArray, Prop) ->
    case Prop of
        #{<<"dataForm">> := #{<<"bitmap">> := Bitmap}} when is_list(Bitmap) ->
            % 根据位图配置处理位数据
            lists:foldl(fun({Index, Mapping}, Acc) ->
                case Index < length(BitArray) of
                    true ->
                        BitValue = lists:nth(Index + 1, BitArray),
                        case Mapping of
                            #{<<"0">> := ZeroVal, <<"1">> := OneVal} ->
                                Value = case BitValue of
                                    0 -> ZeroVal;
                                    1 -> OneVal
                                end,
                                Acc#{Index => Value};
                            _ ->
                                Acc#{Index => BitValue}
                        end;
                    false ->
                        Acc
                end
            end, #{}, Bitmap);
        _ ->
            BitArray
    end.

%% @doc 应用字符串缩放
apply_string_scaling(String, Prop) ->
    case Prop of
        #{<<"dataForm">> := #{<<"encoding">> := Encoding}} ->
            case Encoding of
                <<"utf8">> -> 
                    String;
                <<"gbk">> -> 
                    % GBK编码处理（需要额外库支持）
                    String;
                <<"ascii">> -> 
                    binary:replace(String, <<0>>, <<>>, [global]);
                _ -> 
                    String
            end;
        _ ->
            String
    end.

%% @doc 处理计算值属性
%% 基于已解析的非计算值属性来处理计算值属性
process_calculated_properties(Props, Acc, _DataFragments) ->
    lists:foldl(fun(Prop, CurrentAcc) ->
        case Prop of
            #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>,
                                 <<"protocol">> := <<"MODBUSRTU">>},
              <<"identifier">> := Identifier,
              <<"dataSource">> := #{<<"slaveid">> := BitIdentifier,
                                   <<"address">> := Offset,
                                   <<"registersnumber">> := Num,
                                   <<"originaltype">> := Originaltype}
            } ->
                % 从已解析的属性中获取基础值
                case maps:get(BitIdentifier, CurrentAcc, undefined) of
                    undefined ->
                        CurrentAcc;
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
                                            case catch format_value(Fragment, Prop, []) of
                                                {Value1, _Rest} ->
                                                    CurrentAcc#{Identifier => Value1};
                                                _ ->
                                                    CurrentAcc
                                            end;
                                        false ->
                                            CurrentAcc
                                    end;
                                _ ->
                                    CurrentAcc
                            end
                        catch
                            _:Error ->
                                io:format("~s ~p Error processing calculated property ~p: ~p (Offset=~p, Num=~p)~n", 
                                         [?FILE, ?LINE, Identifier, Error, Offset, Num]),
                                CurrentAcc
                        end
                end;
            _ ->
                CurrentAcc
        end
    end, Acc, Props).
