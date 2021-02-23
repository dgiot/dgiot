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

-module(recv_eprof).

-export([start/3]).

-define(TCP_OPTS, [
    binary,
    {packet, raw},
    {nodelay,true},
    {active, false},
    {reuseaddr, true},
    {keepalive,true},
    {backlog,500}
]).

-spec(start(inet:port_number(), active_n|async_recv, pos_integer()) -> any()).
start(Port, How, N) ->
    eprof:start(),
    P = server(How, Port),
    eprof:start_profiling([P]),
    send(Port, N),
    eprof:stop_profiling(),
    eprof:analyze(procs, [{sort,time}]).

send(Port, N) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, raw}]),
    lists:foreach(fun(_) ->
                      ok = gen_tcp:send(Sock, <<"hello">>)
                  end, lists:seq(1, N)),
    ok = gen_tcp:close(Sock).

server(How, Port) ->
    spawn_link(fun() -> listen(How, Port) end).

listen(How, Port) ->
    {ok, L} = gen_tcp:listen(Port, ?TCP_OPTS),
    accept(How, L).

accept(How, L) ->
    {ok, Sock} = gen_tcp:accept(L),
    case How of
        active_n ->
            inet:setopts(Sock, [{active, 100}]);
        async_recv -> ok
    end,
    _ = recv_loop(How, Sock),
    accept(How, L).

recv_loop(How = active_n, Sock) ->
    receive
        {tcp, Sock, _Data} ->
            recv_loop(How, Sock);
        {tcp_passive, Sock} ->
            inet:setopts(Sock, [{active, 100}]);
        {tcp_closed, Sock}-> ok;
        {tcp_error, Sock, Reason} ->
            {error, Reason}
    end;

recv_loop(How = async_recv, Sock) ->
    {ok, Ref} = prim_inet:async_recv(Sock, 0, -1),
    receive
        {inet_async, Sock, Ref, {ok, _Data}} ->
            recv_loop(How, Sock);
        {inet_async, Sock, Ref, {error, Reason}} ->
            {error, Reason}
    end.

