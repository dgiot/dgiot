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

-module(keeloq).
-export([encrypt/2, numToRadix/2, change_data/2]).
-define(TARGET_STR, <<"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ">>).
-define(INTEGER0, 0).

encrypt(Key0, HxData) ->
    Len = byte_size(HxData),
    Size =
        case Len rem 8 of
            0 -> Len div 8;
            1 -> Len div 8 + 1
        end,
    Data = addZeroForString('after', Size * 8, HxData),
    Key = change_data(4, Key0),
    foreach(Data, Key, <<>>).

foreach(<<>>, _, Acc) -> Acc;
foreach(<<Data0:8/binary, Other/binary>>, Key, Acc) ->
    Source = change_data(4, Data0),
    EncryptData = encode(Source, Key),
    Result = change_data(4, numToRadix(EncryptData, 16)),
    Acc1 = <<Acc/binary, Result/binary>>,
    foreach(Other, Key, Acc1).


encode(Source, Key) -> encode(Source, Key, 0).
encode(Source, _Key, 528) -> Source;
encode(Source, Key, I) ->
    NLF =
        nlf(get_bit(Source, 31), get_bit(Source, 26),
            get_bit(Source, 20), get_bit(Source, 9),
            get_bit(Source, 1)),
    Y16 = get_bit(Source, 16),
    Y0 = get_bit(Source, 0),
    K = get_bit(Key, I rem 64),
    Result = NLF bxor Y16 bxor Y0 bxor K,
    NewSource =
        case  Result =/= 0 of
            true -> rrc(Source, 1);
            false -> rrc(Source, 0)
        end,
    encode(NewSource, Key, I + 1).

divide(0, _Radix, <<>>) -> <<"0">>;
divide(0, _Radix, Acc) -> Acc;
divide(Number, Radix, Acc) ->
    Pos = Number rem Radix,
    Char = binary:part(?TARGET_STR, Pos, 1),
    divide(Number div Radix, Radix, <<Char:1/binary, Acc/binary>>).


numToRadix(Number, Radix0) ->
    Radix =
        case Radix0 < 0 orelse Radix0 > byte_size(?TARGET_STR) of
            true -> byte_size(?TARGET_STR);
            false -> Radix0
        end,
    divide(Number, Radix, <<>>).



rrc(Hex, T) when is_binary(Hex) ->
    rrc(to_bigint(Hex), T);
rrc(Source, 0) ->
    Source bsr 1 band 16#7fffffff;
rrc(Source, _) ->
    Source bsr 1 bor 16#80000000.

to_bigint(Hex) ->
    binary_to_integer(Hex, 16).


change_data(Size, Data0) ->
    Content = addZeroForString('before', Size * 2, Data0),
    binary_reverse(Content, 2).

binary_reverse(Buff, Len) -> binary_reverse(Buff, Len, <<>>).
binary_reverse(<<>>, _, Acc) -> Acc;
binary_reverse(Buff, Len, Acc) ->
    <<Data:Len/binary, Other/binary>> = Buff,
    binary_reverse(Other, Len, <<Data/binary, Acc/binary>>).


get_bit(Hex, N) when is_binary(Hex) ->
    get_bit(to_bigint(Hex), N);
get_bit(Source, N) ->
    Bin = <<Source:64>>,
    Len = byte_size(Bin) * 8,
    PreLen = Len - N - 1,
    LastLen = N,
    <<_P:PreLen, V:1, _L:LastLen>> = Bin,
    V.


nlf(0, 0, 0, 0, 0) -> 0;
nlf(0, 0, 0, 0, 1) -> 1;
nlf(0, 0, 0, 1, 0) -> 1;
nlf(0, 0, 0, 1, 1) -> 1;
nlf(0, 0, 1, 0, 0) -> 0;
nlf(0, 0, 1, 0, 1) -> 1;
nlf(0, 0, 1, 1, 0) -> 0;
nlf(0, 0, 1, 1, 1) -> 0;

nlf(0, 1, 0, 0, 0) -> 0;
nlf(0, 1, 0, 0, 1) -> 0;
nlf(0, 1, 0, 1, 0) -> 1;
nlf(0, 1, 0, 1, 1) -> 0;
nlf(0, 1, 1, 0, 0) -> 1;
nlf(0, 1, 1, 0, 1) -> 1;
nlf(0, 1, 1, 1, 0) -> 1;
nlf(0, 1, 1, 1, 1) -> 0;

nlf(1, 0, 0, 0, 0) -> 0;
nlf(1, 0, 0, 0, 1) -> 0;
nlf(1, 0, 0, 1, 0) -> 1;
nlf(1, 0, 0, 1, 1) -> 1;
nlf(1, 0, 1, 0, 0) -> 1;
nlf(1, 0, 1, 0, 1) -> 0;
nlf(1, 0, 1, 1, 0) -> 1;
nlf(1, 0, 1, 1, 1) -> 0;

nlf(1, 1, 0, 0, 0) -> 0;
nlf(1, 1, 0, 0, 1) -> 1;
nlf(1, 1, 0, 1, 0) -> 0;
nlf(1, 1, 0, 1, 1) -> 1;
nlf(1, 1, 1, 0, 0) -> 1;
nlf(1, 1, 1, 0, 1) -> 1;
nlf(1, 1, 1, 1, 0) -> 0;
nlf(1, 1, 1, 1, 1) -> 0.


addZeroForString(Type, Size, Data) ->
    Len = byte_size(Data),
    case Size > Len of
        true ->
            Pre = list_to_binary(lists:concat(lists:duplicate(Size - Len, "0"))),
            case Type of
                'before' ->
                    <<Pre/binary, Data/binary>>;
                'after' ->
                    <<Data/binary, Pre/binary>>
            end;
        false ->
            Data
    end.