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

%% @doc Async Recv Echo Server.
-module(async_recv_echo_server).

-include("esockd.hrl").

-behaviour(gen_server).

%% start
-export([start/1]).

-export([start_link/2]).

%% gen_server Function Exports
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {transport, socket}).

start(Port) ->
    ok = esockd:start(),
    TcpOpts = [binary,
               {reuseaddr, true},
               {backlog, 512},
               {nodelay, false}
              ],
    SslOpts = [{certfile, "./crt/demo.crt"},
               {keyfile,  "./crt/demo.key"}
              ],
    Options = [{acceptors, 8},
               {max_connections, 100000},
               {tcp_options, TcpOpts},
               {ssl_options, SslOpts}
              ],
    MFArgs = {?MODULE, start_link, []},
    esockd:open(echo, Port, Options, MFArgs).

%% esockd callbacks
start_link(Transport, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Sock]])}.

init([Transport, Sock]) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            Transport:async_recv(Sock, 0, infinity),
            State = #state{transport = Transport, socket = NewSock},
            gen_server:enter_loop(?MODULE, [], State);
        Error -> Error
    end.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, _Sock, _Ref, {ok, Data}},
            State = #state{transport = Transport, socket = Sock}) ->
    {ok, Peername} = Transport:peername(Sock),
    io:format("Data from ~s: ~s~n", [esockd:format(Peername), Data]),
    Transport:async_send(Sock, Data),
    Transport:async_recv(Sock, 0, infinity),
    {noreply, State};

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
    io:format("Shutdown for ~p~n", [Reason]),
    shutdown(Reason, State);

handle_info({inet_reply, _Sock ,ok}, State) ->
    {noreply, State};

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
    io:format("Shutdown for ~p~n", [Reason]),
    shutdown(Reason, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{transport = Transport, socket = Sock}) ->
    Transport:fast_close(Sock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

shutdown(Reason, State) ->
    {stop, {shutdown, Reason}, State}.

