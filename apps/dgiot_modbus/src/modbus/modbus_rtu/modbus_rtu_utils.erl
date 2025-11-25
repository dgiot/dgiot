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

-module(modbus_rtu_utils).
-author("jonhl").

-include("dgiot_modbus.hrl").

-export([
    is16/1,
    get_len/2,
    list_bit_to_binary/1,
    list_word16_to_binary/1,
    pad_hex_data/1,
    pad_hex_data_recursive/2
]).

%% @doc Modbus RTU工具模块
%% 提供通用的工具函数，包括数据转换、长度计算等

%% @doc 将地址转换为16进制格式
%% 处理多种输入格式：0X前缀、0x前缀、十进制数字
%% 返回: 4字符的16进制字符串
is16(<<"0X", Data/binary>>) ->
    pad_hex_data(Data);

is16(<<"0x", Data/binary>>) ->
    pad_hex_data(Data);

is16(<<"00", Data/binary>>) ->
    is16(Data);

is16(<<>>) ->
    <<"0000">>;

% 处理首字符非数字的情况（直接返回原数据，可能是其他字段的ID号）
is16(<<First, _/binary>> = Data) when First < $0 orelse First > $9 ->
    Data;

is16(Data) when is_binary(Data) ->
    try
        case dgiot_utils:to_int(Data) of
            IntData when is_integer(IntData), IntData >= 0, IntData =< 65535 ->
                dgiot_utils:binary_to_hex(<<IntData:16>>);
            _ ->
                io:format("~s ~p Invalid data range for is16: ~p~n", [?FILE, ?LINE, Data]),
                <<"0000">>
        end
    catch
        _:_ ->
            io:format("~s ~p Failed to convert data to integer for is16: ~p~n", [?FILE, ?LINE, Data]),
            <<"0000">>
    end;

is16(Data) ->
    io:format("~s ~p Unexpected data type for is16: ~p~n", [?FILE, ?LINE, Data]),
    <<"0000">>.

%% @doc 根据数据类型和寄存器数量计算数据长度
%% 参数: Num - 寄存器数量, Originaltype - 数据类型
%% 返回: 数据长度（字节数）
get_len(Num, Originaltype) when is_binary(Num) ->
    get_len(dgiot_utils:to_int(Num), Originaltype);

get_len(Num, Originaltype) when is_integer(Num) ->
    case Originaltype of
        <<"bit">> -> Num;
        <<"raw">> -> Num;
        <<"short16_AB">> -> Num * 2;
        <<"short16_BA">> -> Num * 2;
        <<"ushort16_AB">> -> Num * 2;
        <<"ushort16_BA">> -> Num * 2;
        <<"long32_ABCD">> -> Num * 4;
        <<"long32_CDAB">> -> Num * 4;
        <<"ulong32_ABCD">> -> Num * 4;
        <<"ulong32_CDAB">> -> Num * 4;
        <<"float32_ABCD">> -> Num * 4;
        <<"float32_CDAB">> -> Num * 4;
        _ -> Num * 2  % 默认按16位寄存器处理
    end.

%% @doc 将位列表转换为二进制
%% 参数: BitList - 位列表（0和1）
%% 返回: 二进制数据
list_bit_to_binary(BitList) ->
    list_bit_to_binary_recursive(BitList, <<>>).

%% @doc 递归将位列表转换为二进制
list_bit_to_binary_recursive([], Acc) -> Acc;
list_bit_to_binary_recursive(BitList, Acc) ->
    case length(BitList) of
        Len when Len >= 8 ->
            <<Bits:8>> = << <<Bit:1>> || Bit <- lists:sublist(BitList, 8) >>,
            list_bit_to_binary_recursive(lists:nthtail(8, BitList), <<Acc/binary, Bits>>);
        Len ->
            Padding = lists:duplicate(8 - Len, 0),
            <<Bits:8>> = << <<Bit:1>> || Bit <- BitList ++ Padding >>,
            <<Acc/binary, Bits>>
    end.

%% @doc 将16位字列表转换为二进制
%% 参数: WordList - 16位字列表
%% 返回: 二进制数据
list_word16_to_binary(WordList) ->
    << <<Word:16>> || Word <- WordList >>.

%% @doc 填充16进制数据到4字符
pad_hex_data(Data) ->
    case byte_size(Data) of
        Size when Size >= 4 -> Data;
        _ -> pad_hex_data_recursive(Data, 4 - byte_size(Data))
    end.

%% @doc 递归填充16进制数据
pad_hex_data_recursive(Data, 0) -> Data;
pad_hex_data_recursive(Data, Count) ->
    pad_hex_data_recursive(<<"0", Data/binary>>, Count - 1).
