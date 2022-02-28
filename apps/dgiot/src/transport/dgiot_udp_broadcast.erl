%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_udp_broadcast).

-behaviour(gen_server).

-include_lib("dgiot/include/logger.hrl").

-export([ start_link/1
        , stop/0
        ]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {gwid, sock, port, addrs, duration, tref}).

-define(DEFAULT_DURATION, 1*1000).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(start_link(inet:port_number())
      -> {ok, pid()} | {error, term()}).
start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [ Port], []).

-spec(stop() -> ok).
stop() ->
    gen_server:stop(?MODULE, nomal, infinity).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Port]) ->
    Duration = application:get_env(dgiot, advertise_duration, ?DEFAULT_DURATION),
    {ok, Sock} = gen_udp:open(0, [binary, {broadcast, true}]),
    {ok, ensure_advertise(#state{addrs = boradcast_addrs(),
                                 sock = Sock, port = Port, duration = Duration})}.

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected request: ~p", [Req]),
	{reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected msg: ~p", [Msg]),
	{noreply, State}.

handle_info(broadcast_advertise, State) ->
    {noreply, ensure_advertise(State), hibernate};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
	{noreply, State}.

terminate(_Reason, #state{tref = Timer}) ->
    _ = erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

ensure_advertise(State = #state{duration = Duration}) ->
    send_advertise(State),
    State#state{tref = erlang:send_after(Duration, self(), broadcast_advertise)}.

send_advertise(#state{sock = Sock, port = Port, addrs = Addrs}) ->
    Data = dgiot_utils:hex_to_binary(<<"810b000c0120ffff00ff1008">>),
    lists:foreach(fun(Addr) ->
                      ?LOG(debug, "SEND SN_ADVERTISE to ~p~n", [Addr]),
                      gen_udp:send(Sock, Addr, Port, Data)
                  end, Addrs).

boradcast_addrs() ->
    lists:usort([Addr || {ok, IfList} <- [inet:getiflist()], If <- IfList,
                         {ok, [{broadaddr, Addr}]} <- [inet:ifget(If, [broadaddr])]]).

