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

-module(dgiot_tcpc_worker).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
%% API
-export([init/1, handle_info/2, terminate/2]).
-export([start_connect/1]).

-define(MAX_BUFF_SIZE, 10 * 1024).
-record(state, {
    productid,
    devaddr,
    hb = 10
}).

start_connect(#{
    <<"auto_reconnect">> := Recon,
    <<"reconnect_times">> := ReTimes,
    <<"port">> := Port,
    <<"ip">> := Ip,
    <<"productid">> := ProductId,
    <<"hb">> := HB,
    <<"devaddr">> := DevAddr
}) ->
    State = #state{
        productid = ProductId,
        devaddr = DevAddr,
        hb = HB
    },
    dgiot_tcp_client:start_link(DevAddr, ?MODULE, Ip, Port, Recon, ReTimes, State).

init(TCPState) ->
    {ok, TCPState}.

handle_info(connection_ready, TCPState) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    ?LOG(info,"Time ~p ",[Time]),
    dgiot_tcp_client:send(TCPState, <<"login">>),
    erlang:send_after(Time, self(), login),
    {noreply, TCPState};

handle_info(tcp_closed, TCPState) ->
    {noreply, TCPState};

handle_info(login, #tcp{state = #state{productid = ProductId, devaddr = DevAddr, hb = Hb} = _State} = TCPState) ->
    Topic = <<"mock/", ProductId/binary, "/", DevAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(Hb * 1000, self(), heartbeat),
    ?LOG(info,"~p ",[<<"login">>]),
    dgiot_tcp_client:send(TCPState, <<"login">>),
    {noreply, TCPState};

handle_info(heartbeat, #tcp{state = #state{devaddr = _DevAddr, hb = Hb} = _State} = TCPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    ?LOG(info,"~p ",[<<"heartbeat">>]),
    dgiot_tcp_client:send(TCPState, <<"heartbeat">>),
    {noreply, TCPState};

handle_info({tcp, _Buff}, #tcp{state = #state{productid = _ProductId} = _State} = TCPState) ->
        {noreply, TCPState};

handle_info({deliver, _Topic, Msg}, #tcp{state = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Client recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
%%    Message =
%%        case jsx:is_json(Payload) of
%%            true ->
%%                jsx:decode(Payload, [{labels, binary}, return_maps]);
%%            false ->
%%                binary_to_term(Payload)
%%        end,
    {noreply, TCPState};

handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.