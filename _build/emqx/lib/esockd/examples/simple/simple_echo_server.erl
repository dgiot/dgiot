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

-module(simple_echo_server).

-export([start/1]).

-export([start_link/2, init/2, loop/2]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true}, {nodelay, false}]).

%% @doc Start echo server.
start(Port) ->
    ok = esockd:start(),
    Access = application:get_env(esockd, access, [{allow, all}]),
    SockOpts = [{access_rules, Access},
                {acceptors, 8},
                {shutdown, infinity},
                {max_connections, 1000000},
                {tcp_options, ?TCP_OPTIONS}
               ],
    MFArgs = {?MODULE, start_link, []},
    esockd:open(echo, Port, SockOpts, MFArgs).

%%--------------------------------------------------------------------
%% esockd callback
%%--------------------------------------------------------------------

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
			{ok, Peername} = Transport:peername(Sock),
			io:format("~s - ~s~n", [esockd:format(Peername), Data]),
			Transport:send(Sock, Data),
            loop(Transport, Sock);
		{error, Reason} ->
			io:format("TCP ~s~n", [Reason]),
			{stop, Reason}
	end.

