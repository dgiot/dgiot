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

-module(ssl_server).

-export([start_link/2, stop/1]).

%% Internal exports
-export([ssl_accept/1]).

start_link(Port, SslOpts) ->
    _ = ssl:start(),
    {ok, LSock} = ssl:listen(Port, [{active, false}|SslOpts]),
    spawn_link(?MODULE, ssl_accept, [LSock]).

ssl_accept(LSock) ->
    {ok, Sock} = ssl:transport_accept(LSock),
    {ok, SslSock} = ssl:handshake(Sock, 5000),
    ssl:setopts(SslSock, [{active, true}]),
    {stop, _Reason} = ssl_recvloop(SslSock),
    ssl:close(LSock).

ssl_recvloop(SslSock) ->
    receive
        {ssl, SslSock, Data} ->
            ssl:send(SslSock, Data),
            ssl_recvloop(SslSock);
        {ssl_error, SslSock, Reason} ->
            {stop, Reason};
        {ssl_closed, SslSock} ->
            {stop, closed};
        stop -> {stop, normal}
    end.

stop(Server) -> Server ! stop, ok.

