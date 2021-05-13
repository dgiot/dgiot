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

-module(dtls_echo_server).

-export([start/1]).

-export([start_link/2, loop/2]).

start(Port) ->
    ok = esockd:start(),
    PrivDir = code:priv_dir(esockd),
    DtlsOpts = [{mode, binary}, {reuseaddr, true}, {active, 100},
                {certfile, filename:join(PrivDir, "demo.crt")},
                {keyfile, filename:join(PrivDir, "demo.key")}
               ],
    Opts = [{acceptors, 4},
            {max_connections, 1000},
            {dtls_options, DtlsOpts}
           ],
    MFArgs = {?MODULE, start_link, []},
    {ok, _} = esockd:open_dtls('echo/dtls', Port, Opts, MFArgs).

start_link(Transport, Socket) ->
    Args = [Transport, Socket],
    CPid = proc_lib:spawn_link(?MODULE, loop, Args),
    {ok, CPid}.

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
        {ssl, _RawSocket, Packet} ->
            io:format("~s - ~p~n", [esockd:format(Peername), Packet]),
            Transport:async_send(Socket, Packet),
            run_loop(Peername, Transport, Socket);
        {ssl_passive, _RawSocket} ->
            io:format("~s - enter the passive mode.~n", [esockd:format(Peername)]),
            Transport:setopts(Socket, [{active, 100}]),
            run_loop(Peername, Transport, Socket);
        {inet_reply, _RawSocket, ok} ->
            run_loop(Peername, Transport, Socket);
        {ssl_closed, _RawSocket} ->
            ok;
        {ssl_error, _RawSocket, Reason} ->
            io:format("~s error: ~p~n", [esockd:format(Peername), Reason])
    end.

