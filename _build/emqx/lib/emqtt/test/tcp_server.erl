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

-module(tcp_server).

-export([start_link/1, stop/1]).

%% Internal exports
-export([accept/1]).

start_link(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}]),
    spawn_link(?MODULE, accept, [LSock]).

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    inet:setopts(Sock, [{active, true}]),
    {stop, _Reason} = recvloop(Sock),
    gen_tcp:close(LSock).

recvloop(Sock) ->
    receive
        {tcp, Sock, Data} ->
            gen_tcp:send(Sock, Data),
            recvloop(Sock);
        {tcp_error, Sock, Reason} ->
            {stop, Reason};
        {tcp_closed, Sock} ->
            {stop, closed};
        stop -> {stop, normal}
    end.

stop(Server) -> Server ! stop, ok.

