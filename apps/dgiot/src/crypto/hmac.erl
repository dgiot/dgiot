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

-module(hmac).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([encode/3, sign/3]).

%% Type : sha sha256
encode(Type, Key, Data) ->
    B = crypto:mac(hmac, Type, Key, Data),
    Len = byte_size(B) * 8,
    <<Mac:Len/integer>> = B,
    integer_to_binary(Mac, 16).

sign(Count, ApId, KI) ->
    Count1 = list_to_binary(io_lib:format("~8.10.0B", [Count])),
    KI1 = dgiot_utils:binary_to_hex(KI),
    B = <<Count1/binary, ApId/binary, KI1/binary>>,
    case catch dgiot_utils:binary_to_hex(crypto:hash(sha256, dgiot_utils:hex_to_binary(B))) of
        {'EXIT', Reason} ->
            ?LOG(info, "Count:~p, ApId:~p, KI:~p ~p", [Count, ApId, KI, Reason]),
            throw({error, Reason});
        V ->
            V
    end.