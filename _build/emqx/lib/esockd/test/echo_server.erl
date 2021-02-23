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

-module(echo_server).

-export([start_link/2]).

%% Callbacks
-export([init/2, loop/2]).

start_link(Transport, RawSock) ->
	{ok, spawn_link(?MODULE, init, [Transport, RawSock])}.

init(Transport, RawSock) ->
    case Transport:wait(RawSock) of
        {ok, Sock} ->
            loop(Transport, Sock);
        {error, Reason} ->
            {error, Reason}
    end.

loop(Transport, Sock) ->
	case Transport:recv(Sock, 0) of
        {ok, Data} ->
            %%{ok, Peername} = Transport:peername(Sock),
            %%io:format("RECV from ~s: ~s~n", [esockd:format(Peername), Data]),
            Transport:send(Sock, Data),
            loop(Transport, Sock);
        {error, Reason} ->
            exit({shutdown, Reason})
	end.

