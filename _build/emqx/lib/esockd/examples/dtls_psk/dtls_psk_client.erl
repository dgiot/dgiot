%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dtls_psk_client).

-export([ connect/0
        , connect/1
        , send/2
        , recv/2
        , recv/3
        , user_lookup/3
        ]).

connect() ->
    connect(5000).
connect(Port) ->
    {ok, Client} = ssl:connect({127, 0, 0, 1}, Port, opts()),
    Client.

send(Client, Msg) ->
    ssl:send(Client, Msg).

recv(Client, Len) ->
    ssl:recv(Client, Len).

recv(Client, Len, Timeout) ->
    ssl:recv(Client, Len, Timeout).

user_lookup(psk, ServerHint, _UserState = PSKs) ->
    ServerPskId = server_suggested_psk_id(ServerHint),
    ClientPsk = maps:get(ServerPskId, PSKs),
    io:format("ServerHint:~p, ServerSuggestedPSKID:~p, ClientPickedPSK: ~p~n",
              [ServerHint, ServerPskId, ClientPsk]),
    {ok, ClientPsk}.

server_suggested_psk_id(ServerHint) ->
    [_, Psk] = binary:split(ServerHint, <<"plz_use_">>),
    Psk.

opts() ->
    [{ssl_imp, new},
     {active, false},
     {verify, verify_none},
     {versions, [dtlsv1]},
     {protocol, dtls},
     {ciphers, [{psk, aes_128_cbc, sha}]},
     {psk_identity, "psk_b"},
     {user_lookup_fun, {fun user_lookup/3,
                          #{<<"psk_a">> => <<"shared_secret_a">>,
                            <<"psk_b">> => <<"shared_secret_b">>}}},
     {cb_info, {gen_udp, udp, udp_close, udp_error}}
    ].

