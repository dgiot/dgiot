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

%% @doc Echo test client
-module(echo_client).

-export([start/3, send/2, run/4, connect/4, loop/2]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, true}]).

start(Host, Port, Num) ->
    spawn(?MODULE, run, [self(), Host, Port, Num]),
    mainloop(1).

mainloop(Count) ->
    receive
        {connected, _Sock} ->
            io:format("conneted: ~p~n", [Count]),
            mainloop(Count+1)
    end.

run(_Parent, _Host, _Port, 0) ->
    ok;
run(Parent, Host, Port, Num) ->
    spawn(?MODULE, connect, [Parent, Host, Port, Num]),
    timer:sleep(5),
    run(Parent, Host, Port, Num-1).

connect(Parent, Host, Port, Num) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, 6000) of
        {ok, Sock} ->
            Parent ! {connected, Sock},
            loop(Num, Sock);
        {error, Reason} ->
            io:format("Client ~p connect error: ~p~n", [Num, Reason])
    end.

loop(Num, Sock) ->
    Timeout = 5000 + rand:uniform(5000),
    receive
        {tcp, Sock, Data} ->
            io:format("Client ~w received: ~s~n", [Num, Data]),
            loop(Num, Sock);
        {tcp_closed, Sock} ->
            io:format("Client ~w socket closed~n", [Num]);
        {tcp_error, Sock, Reason} ->
            io:format("Client ~w socket error: ~p~n", [Num, Reason]);
        Other ->
            io:format("Client ~w unexpected: ~p", [Num, Other])
    after
        Timeout ->
            send(Num, Sock), loop(Num, Sock)
    end.

send(N, Sock) ->
    gen_tcp:send(Sock, [integer_to_list(N), ":", <<"Hello, eSockd!">>]).

