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

-module(dtls_psk_echo_server).

-export([start/1]).

-export([start_link/2, loop/2]).

start(Port) ->
    [{ok, _} = application:ensure_all_started(App) || App <- [sasl, crypto, ssl, esockd]],
    DtlsOpts = [{mode, binary}, {reuseaddr, true}] ++ psk_opts(),
    Opts = [{acceptors, 4}, {max_clients, 1000}, {dtls_options, DtlsOpts}],
    {ok, _} = esockd:open_dtls('echo/dtls', Port, Opts, {?MODULE, start_link, []}).

start_link(Transport, Socket) ->
    {ok, spawn_link(?MODULE, loop, [Transport, Socket])}.

loop(Transport, RawSocket) ->
    case Transport:wait(RawSocket) of
        {ok, Socket} ->
            {ok, Peername} = Transport:peername(Socket),
            run_loop(Peername, Transport, Socket);
        {error, Reason} ->
            ok = Transport:fast_close(RawSocket),
            io:format("Wait socket upgrade error ~p~n", [Reason])
    end.

run_loop(Peername, Transport, Socket) ->
    receive
        {ssl, _Sock, Packet} ->
            io:format("~s - ~p~n", [esockd:format(Peername), Packet]),
            Transport:async_send(Socket, Packet),
            run_loop(Peername, Transport, Socket)
    end.

psk_opts() ->
    [{verify, verify_none},
     {protocol, dtls},
     {versions, [dtlsv1, 'dtlsv1.2']},
     {ciphers, [{psk,aes_128_cbc,sha}, {rsa_psk,aes_128_cbc,sha256}]},
     {psk_identity, "plz_use_psk_a"},
     {user_lookup_fun,
       {fun user_lookup/3, #{<<"psk_a">> => <<"shared_secret_a">>,
                             <<"psk_b">> => <<"shared_secret_b">>}}}
    ].

user_lookup(psk, ClientPSKID, _UserState = PSKs) ->
    ServerPickedPsk = maps:get(<<"psk_a">>, PSKs),
    io:format("ClientPSKID: ~p, ServerPickedPSK: ~p~n", [ClientPSKID, ServerPickedPsk]),
    {ok, ServerPickedPsk}.

