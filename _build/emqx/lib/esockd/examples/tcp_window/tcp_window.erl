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

-module(tcp_window).

-export([start/2]).

-export([ start_server/3
        , server_init/3
        , server_loop/3
        ]).

-define(TCP_OPTIONS, [binary,
                      {packet, raw},
                      {active, false},
                      {recbuf, 1024},
                      {sndbuf, 1024},
                      {send_timeout, 5000},
                      {send_timeout_close, true}
                     ]).

-spec(start(inet:port_number(), sync|async|async_force|async_nosuspend) -> ok).
start(Port, How) when is_integer(Port) ->
    lists:foreach(fun application:ensure_all_started/1, [sasl, crypto, esockd]),
    Options = [{acceptors, 1},
               {shutdown, infinity},
               {max_connections, 100},
               {tcp_options, ?TCP_OPTIONS}],
    esockd:open(tcp_block, Port, Options, {?MODULE, start_server, [How]}),
    start_client(Port).

start_server(Transport, Sock, How) ->
    {ok, spawn_link(?MODULE, server_init, [Transport, Sock, How])}.

server_init(Transport, Sock, How) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            SockOpts = Transport:getopts(NewSock, [send_timeout, send_timeout_close]),
            io:format("Sockopts: ~p~n", [SockOpts]),
            SendFun = send_fun(How, Transport, Sock),
            server_loop(Transport, Sock, SendFun);
        {error, Reason} ->
            {stop, Reason}
    end.

server_loop(Transport, Sock, SendFun) ->
    case Transport:recv(Sock, 0) of
        {ok, Data} ->
            {ok, Peername} = Transport:peername(Sock),
            io:format("Server recv: ~p from (~s)~n", [Data, esockd:format(Peername)]),
            SendFun(Data),
            %% flood the tcp window of client
            send_loop(Transport, Sock, SendFun, Data, 0);
        {error, Reason} ->
            io:format("server tcp error ~s~n", [Reason]),
            Transport:fast_close(Sock),
            {stop, Reason}
    end.

send_loop(Transport, Sock, SendFun, Data, Count) ->
    case SendFun(Data) of
        true ->
            io:format("Send ~w~n", [Count]),
            io:format("Stats: ~p~n", [Transport:getstat(Sock, [send_cnt])]),
            send_loop(Transport, Sock, SendFun, Data, Count + iolist_size(Data));
        false ->
            io:format("False Send ~w~n", [Count]),
            io:format("Stats: ~p~n", [Transport:getstat(Sock, [send_cnt])]),
            send_loop(Transport, Sock, SendFun, Data, Count + iolist_size(Data));
        ok ->
            io:format("Send ~w~n", [Count]),
            send_loop(Transport, Sock, SendFun, Data, Count + iolist_size(Data));
        {error, Error} ->
            io:format("Server tcp error: ~p~n", [Error]),
            Transport:fast_close(Sock)
    end.

start_client(Port) ->
    case gen_tcp:connect("127.0.0.1", Port, ?TCP_OPTIONS, 60000) of
        {ok, Sock} ->
            inet:setopts(Sock, [{active, false}]),
            client_loop(Sock, 0);
        {error, Reason} ->
            io:format("client failed to connect: ~p~n", [Reason])
    end.

client_loop(Sock, I) ->
    gen_tcp:send(Sock, crypto:strong_rand_bytes(1024*1024)),
    timer:sleep(1000),
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            io:format("client recv: ~p~n", [Data]),
            client_loop(Sock, I+1);
        {error, Reason} ->
            io:format("client tcp error: ~p~n", [Reason]),
            gen_tcp:close(Sock)
    end.

send_fun(sync, Transport, Sock) ->
    fun(Data) -> Transport:send(Sock, Data) end;
send_fun(async, _Transport, Sock) ->
    fun(Data) -> port_command(Sock, Data) end;
send_fun(async_force, _Transport, Sock) ->
    fun(Data) -> port_command(Sock, Data, [force]) end;
send_fun(async_nosuspend, Transport, Sock) ->
    fun(Data) -> Transport:async_send(Sock, Data) end.

