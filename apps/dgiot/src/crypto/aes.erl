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

-module(aes).
-author("johnliu").

%% API
-export([encode/3, encode/4, decode/4]).
-compile({nowarn_deprecated_function, [{crypto, block_encrypt, 4},{crypto, block_decrypt, 4}]}).
-define(AES_IV, <<"00000000000000000000000000000000">>).

encode(Type, AES_KEY, Bin) ->
    encode(Type, AES_KEY, ?AES_IV, Bin).

encode(aes_cbc128, AES_KEY, AES_IV0, Bin) ->
    AES_IV = dgiot_utils:hex_to_binary(AES_IV0),
    Len = erlang:size(Bin),
    Value = 16 - (Len rem 16),
    PadBin = binary:copy(<<0:8>>, Value),
    EncodeB = crypto:block_encrypt(aes_cbc128, AES_KEY, AES_IV, <<Bin/binary, PadBin/binary>>),
%%    EncodeB = crypto:crypto_one_time(aes_cbc128, AES_KEY, AES_IV, [<<Bin/binary, PadBin/binary>>], true),
%%    io:format("~s ~p EncodeB ~p ~n", [?FILE, ?LINE, EncodeB]),
    dgiot_utils:binary_to_hex(EncodeB);

encode(aes_cfb8, AES_KEY, AES_IV, Bin) ->
    EncodeB = crypto:block_encrypt(aes_cfb8, AES_KEY, AES_IV, Bin),
%%    EncodeB = crypto:crypto_one_time(aes_cfb8, AES_KEY, AES_IV, [Bin], true),
%%    io:format("~s ~p EncodeB ~p ~n", [?FILE, ?LINE, EncodeB]),
    dgiot_utils:binary_to_hex(EncodeB).

decode(aes_cbc128, AES_KEY, AES_IV, Bin) ->
    Bin1 = dgiot_utils:hex_to_binary(Bin),
    case erlang:size(Bin1) rem 16 of
        0 ->
            Bin2 = crypto:block_decrypt(aes_cbc128, AES_KEY, AES_IV, Bin1),
%%            Bin2 = crypto:crypto_one_time(aes_cbc128, AES_KEY, AES_IV, [Bin1], false),
%%            io:format("~s ~p EncodeB ~p ~n", [?FILE, ?LINE, Bin2]),
            binary:part(Bin2, {0, byte_size(Bin2) - binary:last(Bin2)});
        _ ->
            {error, 1102}
    end.





