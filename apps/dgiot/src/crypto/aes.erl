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
-export([encode/4, decode/4]).
%% https://www.erlang.org/doc/man/crypto.html
%% aes_cbc128
%% aes_cbc256
%% aes_ige256
%% aes_cfb8
%% aes_cfb128
%% aes_gcm

%%pbe_pad(Data, BlockSize) ->
%%    N = BlockSize - (erlang:byte_size(Data) rem BlockSize),
%%    Pad = binary:copy(<<N>>, N),
%%    <<Data/binary, Pad/binary>>.

%%ZeroPadding，数据长度不对齐时使用0填充，否则不填充
%%PKCS7Padding，假设数据长度需要填充n(n>0)个字节才对齐，那么填充n个字节，每个字节都是n;如果数据本身就已经对齐了，则填充一块长度为块大小的数据，每个字节都是块大小
%%PKCS5Padding，PKCS7Padding的子集，块大小固定为8字节。
%%
%% 在AES的CBC加密、解密模式中，包括三个参数：
%% 1、Key，这是AES加密的密钥，可为iolist类型，也可为binary类型，长度必须为128位，也就是16个字节；
%% 2、IVec，初始向量，类型为binary，长度必须为128位，16个字节；
%% 3、Text，待加密的数据，类型可为iolist也可为binary，但是，长度也必须是128位；
%% Types:
%% Key = Text = iolist() | binary()
%% IVec = Cipher = binary()
%% 根据 AES_KEY 的长度自动判定是
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
%% | KEY长度    | IVec长度  | Text       | 算法类型         |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
%% | 16字节     |  16字节   | 16字节*N   | aes_128_cbc      |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
%% | 24字节     |  24字节   | 24字节*N   | aes_192_cbc      |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
%% | 32字节     |  32字节   | 24字节*N   | aes_256_cbc      |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
encode(aes_cbc, AES_KEY, AES_IV, Bin) ->
    crypto:crypto_one_time(aes_cbc, AES_KEY, AES_IV, [pkcs:pad(Bin, 16)], true);

encode(aes_cfb8, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_cfb8, AES_KEY, AES_IV, [Text], true);

encode(aes_cfb128, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_cfb128, AES_KEY, AES_IV, [Text], true);

encode(aes_ctr, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ctr, AES_KEY, AES_IV, [Text], true);

encode(aes_ecb, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ecb, AES_KEY, AES_IV, [Text], true);

encode(aes_gcm, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_gcm, AES_KEY, AES_IV, [Text], true);

encode(aes_ccm, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ccm, AES_KEY, AES_IV, [Text], true);

encode(Type, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(Type, AES_KEY, AES_IV, [Text], true).

decode(aes_cbc, AES_KEY, AES_IV, Text) ->
    case erlang:size(Text) rem size(AES_KEY) of
        0 ->
            Bin2 = crypto:crypto_one_time(aes_cbc, AES_KEY, AES_IV, [Text], false),
            case pkcs:unpad(Bin2, size(Bin2)) of
                {ok, Data} ->
                    Data;
                Result ->
                    Result
            end;
        _ ->
            {error, 1102}
    end;

decode(aes_cfb8, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_cfb8, AES_KEY, AES_IV, [Text], false);

decode(aes_cfb128, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_cfb128, AES_KEY, AES_IV, [Text], false);

decode(aes_ctr, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ctr, AES_KEY, AES_IV, [Text], false);

decode(aes_ecb, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ecb, AES_KEY, AES_IV, [Text], false);

decode(aes_gcm, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_gcm, AES_KEY, AES_IV, [Text], false);

decode(aes_ccm, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(aes_ccm, AES_KEY, AES_IV, [Text], false);

decode(Type, AES_KEY, AES_IV, Text) ->
    crypto:crypto_one_time(Type, AES_KEY, AES_IV, [Text], false).





