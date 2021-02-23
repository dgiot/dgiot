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

-export([start/1]).

-export([start_link/2, loop/2]).

-define(UDP_OPTS, [binary, {reuseaddr, true}]).

start(Port) ->
    ok = esockd:start(),
    Opts = [{udp_options, ?UDP_OPTS}],
    MFA = {?MODULE, start_link, []},
    esockd:open_udp('echo/udp', Port, Opts, MFA).

start_link(Transport, Peer) ->
    {ok, spawn_link(?MODULE, loop, [Transport, Peer])}.

loop(Transport = {udp, Server, Sock}, Peer = {IP, Port}) ->
    receive
        {datagram, Server, Packet} ->
            io:format("~s - ~p~n", [esockd:format(Peer), Packet]),
            %% Reply by pid
            Server ! {datagram, Peer, Packet},
            %% Reply by sock
            ok = gen_udp:send(Sock, IP, Port, Packet),
            loop(Transport, Peer)
    end.

