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

-module(esockd_dtls_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(esockd),
    Config.

end_per_suite(_Config) ->
    application:stop(esockd).

%%--------------------------------------------------------------------
%% Test cases for DTLS Server
%%--------------------------------------------------------------------

t_dtls_server(Config) ->
    DtlsOpts = [{mode, binary},
                {reuseaddr, true},
                {certfile, esockd_ct:certfile(Config)},
                {keyfile, esockd_ct:keyfile(Config)}
               ],
    Options = [{acceptors, 4},
               {max_connections, 1000},
               {max_conn_rate, 10},
               {dtls_options, DtlsOpts}],
    {ok, _} = esockd:open_dtls('echo/dtls', 9876, Options, {?MODULE, dtls_echo_init, []}),
    {ok, Sock} = ssl:connect({127,0,0,1}, 9876, [binary, {protocol, dtls}, {active, false}], 5000),
    ok = ssl:send(Sock, <<"hello">>),
    {ok, <<"hello">>} = ssl:recv(Sock, 5, 3000),
    ok = ssl:send(Sock, <<"world">>),
    {ok, <<"world">>} = ssl:recv(Sock, 5, 3000),
    ok = esockd:close('echo/dtls', 9876).

%%--------------------------------------------------------------------
%% DTLS echo server
%%--------------------------------------------------------------------

dtls_echo_init(Transport, Peer) ->
    {ok, spawn_link(?MODULE, dtls_echo_loop, [Transport, Peer])}.

dtls_echo_loop(Transport, Peer) ->
    receive
        {datagram, From, Packet} ->
            %%io:format("~s - ~p~n", [esockd:format(Peer), Packet]),
            From ! {datagram, Peer, Packet},
            dtls_echo_loop(Transport, Peer)
    end.

