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

-module(rsa).
-include_lib("public_key/include/public_key.hrl").
-author("johnliu").

%% API
-export([generate/0, en_rsa/2, de_rsa/2, rsa_to_string/1, test/0]).

generate() ->
    %%获取私钥
    PrivateKey = public_key:generate_key({rsa, 1024, 65537}),
    #'RSAPrivateKey'{
        modulus = Modulus,
        publicExponent = PublicExponent
    } = PrivateKey,
    %%获取公钥
    PublicKey = #'RSAPublicKey'{
        modulus = Modulus,
        publicExponent = PublicExponent
    },
    {PrivateKey, PublicKey}.


%%公钥加密
en_rsa(PublicKey, BinData) ->
    public_key:encrypt_public(BinData, PublicKey).

%%公钥转字符串
rsa_to_string(PublicKey) ->
    binary_to_list(public_key:pem_encode([public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey)])).


%%私钥解密
de_rsa(PrivateKey, BinData) ->
    case catch public_key:decrypt_private(BinData, PrivateKey) of
        {'EXIT', {decrypt_failed, _}} ->
            {error, decrypt_failed};
        Result ->
            Result
    end.

test() ->
    {PrivateKey, PublicKey} = generate(),
    Encode = en_rsa(PublicKey, <<"BinData">>),
    de_rsa(PrivateKey, Encode).
