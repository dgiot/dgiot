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

%% @doc Simple echo server.
-module(gen_echo_server).

-include("esockd.hrl").

-behaviour(gen_server).

%% start
-export([start/1]).

%% esockd callback
-export([start_link/2]).

%% gen_server function exports
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
    Opts = [{acceptors, 2},
            {max_connections, 100000},
            {max_conn_rate, 10}
           ],
    MFArgs = {?MODULE, start_link, []},
    esockd:open(echo, Port, Opts, MFArgs).

start_link(Transport, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Sock]])}.

init([Transport, Sock]) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            Transport:setopts(Sock, [{active, once}]),
            gen_server:enter_loop(?MODULE, [], #state{transport = Transport, socket = NewSock});
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State = #state{transport = Transport, socket = Sock}) ->
    {ok, Peername} = Transport:peername(Sock),
    io:format("Data from ~s: ~s~n", [esockd:format(Peername), Data]),
    Transport:send(Sock, Data),
    Transport:setopts(Sock, [{active, once}]),
    {noreply, State};

handle_info({tcp_error, Sock, Reason}, State = #state{socket = Sock}) ->
    io:format("Error from: ~p~n", [Sock]),
    io:format("tcp_error: ~s~n", [Reason]),
    {stop, {shutdown, Reason}, State};

handle_info({tcp_closed, Sock}, State = #state{socket = Sock}) ->
    io:format("tcp_closed~n"),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

