%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_udpc_worker).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([init/1, handle_info/2, terminate/2]).
-export([start_connect/1]).

-define(MAX_BUFF_SIZE, 10 * 1024).
-define(DGIOT_UDPC_WORK, dgiot_udpc_work).

-record(state, {
    productid,
    devaddr,
    hb = 60,
    auto_reconnect = 30,
    reconnect_times = 3,
    login = <<"login">>,
    module = dgiot_sonbs
}).

start_connect(#{
    <<"auto_reconnect">> := Recon,
    <<"reconnect_times">> := ReTimes,
    <<"port">> := Port,
    <<"ip">> := Ip,
    <<"productid">> := ProductId,
    <<"hb">> := HB,
    <<"login">> := Login,
    <<"module">> := Module
}) ->
    State = #state{
        productid = ProductId,
        hb = HB,
        auto_reconnect = Recon,
        reconnect_times = ReTimes,
        login = Login,
        module = dgiot_utils:to_atom(Module)
    },
    io:format("~s ~p MODULE = ~p.~n", [?FILE, ?LINE, ?MODULE]),
    io:format("~s ~p Ip = ~p.~n", [?FILE, ?LINE, Ip]),
    io:format("~s ~p Port = ~p.~n", [?FILE, ?LINE, Port]),
    io:format("~s ~p State = ~p.~n", [?FILE, ?LINE, State]),

    dgiot_udp_client:start_link(?MODULE, Ip, Port, State).

init(UDPState) ->
    {ok, UDPState}.

handle_info(connection_ready, UDPState) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), login),
    {noreply, UDPState};

handle_info(udp_closed, UDPState) ->
    {noreply, UDPState};

handle_info(login, #udp{state = #state{productid = _ProductId, hb = Hb, login = Login} = _State} = UDPState) ->
    io:format("~s ~p UDPState ~p ~n", [?FILE, ?LINE, UDPState]),
%%    Topic = <<"mock/", ProductId/binary, "/", DevAddr/binary>>,
%%    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(Hb * 1000, self(), heartbeat),
    dgiot_udp_client:send(UDPState, Login),
    {noreply, UDPState};

handle_info(heartbeat, #udp{state = #state{hb = Hb, login = Login} = _State} = UDPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    io:format("~s ~p herart ~p ~n", [?FILE, ?LINE, Login]),
    dgiot_udp_client:send(UDPState, Login),
    {noreply, UDPState};

handle_info({udp, Buff}, #udp{state = #state{productid = ProductId, module = Module} = _State} = UDPState) ->
    Module:parse_frame(Buff, #{}, #{<<"productid">> => ProductId}),
    {noreply, UDPState};

handle_info({deliver, _Topic, Msg}, #udp{state = State} = UDPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Client recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
    {noreply, UDPState};

handle_info(_Info, UDPState) ->
    {noreply, UDPState}.

terminate(_Reason, _UDPState) ->
    ok.
