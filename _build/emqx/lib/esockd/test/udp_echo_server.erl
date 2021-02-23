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

-module(udp_echo_server).

-export([start_link/2]).

-export([init/2, loop/2]).

start_link(Transport, Peer) ->
	{ok, spawn_link(?MODULE, init, [Transport, Peer])}.

init(Transport, Peer) ->
    loop(Transport, Peer).

loop(Transport, Peer) ->
    receive
        {datagram, _From, <<"stop">>} ->
            exit(normal);
        {datagram, From, Packet} ->
            From ! {datagram, Peer, Packet},
            loop(Transport, Peer)
    end.

