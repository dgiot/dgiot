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

-module(ssl_echo_server).

-export([start/1]).

-export([start_link/2, init/2, loop/2]).

start(Port) ->
    ok = esockd:start(),
    PrivDir = code:priv_dir(esockd),
    TcpOpts = [binary, {reuseaddr, true}],
    SslOpts = [{certfile, filename:join(PrivDir, "demo.crt")},
               {keyfile, filename:join(PrivDir, "demo.key")}
              ],
    Opts = [{acceptors, 4},
            {max_connections, 1000},
            {tcp_options, TcpOpts},
            {ssl_options, SslOpts}
           ],
    {ok, _} = esockd:open('echo/ssl', Port, Opts, ?MODULE).

start_link(Transport, Sock) ->
    {ok, spawn_link(?MODULE, init, [Transport, Sock])}.

init(Transport, Sock) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            loop(Transport, NewSock);
        Error -> Error
    end.

loop(Transport, Sock) ->
    case Transport:recv(Sock, 0) of
        {ok, Data} ->
            {ok, PeerName} = Transport:peername(Sock),
            io:format("~s - ~p~n", [esockd:format(PeerName), Data]),
            Transport:send(Sock, Data),
            loop(Transport, Sock);
        {error, Reason} ->
            io:format("tcp ~s~n", [Reason]),
            {stop, Reason}
    end.

