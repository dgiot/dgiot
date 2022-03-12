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
-record(state, {
    productid,
    devaddr,
    hb = 60
}).

start_connect(#{
    <<"auto_reconnect">> := _Recon,
    <<"reconnect_times">> := _ReTimes,
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
    io:format("~s ~p State ~p ~n",[?FILE,?LINE, State]),
    dgiot_udp_client:start_link(?MODULE, Ip, Port, State).
%%    dgiot_udp_client:start_link(?MODULE, Ip, Port, Recon, ReTimes, State).

init(UDPState) ->
    io:format("~s ~p UdpState ~p ~n",[?FILE,?LINE, UDPState]),
    {ok, UDPState}.

handle_info(connection_ready, UDPState) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), login),
    {noreply, UDPState};

handle_info(udp_closed, UDPState) ->
    {noreply, UDPState};

handle_info(login, #udp{state = #state{productid = ProductId, devaddr = DevAddr, hb = Hb} = State} = UDPState) ->
    Topic = <<"mock/", ProductId/binary, "/", DevAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(Hb * 1000, self(), heartbeat),
    do_message([{reply, #{<<"devaddr">> => DevAddr, <<"cmd">> => <<"login">>}, State}],
        State, UDPState),
    {noreply, UDPState};

handle_info(heartbeat, #udp{state = #state{devaddr = DevAddr, hb = Hb} = State} = UDPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    io:format("~s ~p herart ~p ~n",[?FILE,?LINE, <<"heartbeat">>]),
    dgiot_udp_client:send(UDPState, <<"heartbeat">>),
    do_message([{reply, #{<<"devaddr">> => DevAddr, <<"cmd">> => <<"heartbeat">>}, State}],
        State, UDPState),
    {noreply, UDPState};

handle_info({udp, Buff}, #udp{state = #state{productid = ProductId} = State} = UDPState) ->
    case decode(Buff, ProductId, UDPState) of
        {ok, Messages, #udp{state = NewState} = NewUDPState} ->
            case do_messages(Messages, NewState, UDPState) of
                {error, Reason, State0} ->
                    ?LOG(info, "stop Reason ~p State ~p~n", [Reason, State0]),
                    {stop, Reason, NewUDPState#udp{state = State0}};
                NewState0 ->
                    {noreply, NewUDPState#udp{state = NewState0}}
            end;
        {error, buff_size_limit} ->
            {stop, buff_size_limit, UDPState#udp{state = State}};
        {error, Reason} ->
            {stop, Reason, UDPState#udp{state = State}}
    end;

handle_info({deliver, _Topic, Msg}, #udp{state = State} = UDPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Client recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
    Message =
        case jsx:is_json(Payload) of
            true ->
                jsx:decode(Payload, [{labels, binary}, return_maps]);
            false ->
                binary_to_term(Payload)
        end,
    NewState = do_message(Message, State, UDPState),
    {noreply, UDPState#udp{state = NewState}};

handle_info(_Info, UDPState) ->
    {noreply, UDPState}.

terminate(_Reason, _UDPState) ->
    ok.

do_messages([], NState, _) -> NState;
do_messages([Message | Rest], State, UDPState) ->
    case do_message(Message, State, UDPState) of
        {error, Reason} ->
            {error, Reason, State};
        {ok, NState} ->
            do_messages(Rest, NState, UDPState);
        NState ->
            do_messages(Rest, NState, UDPState)
    end.

do_message(Message, State, #udp{state = #state{productid = ProductId} = State} = UDPState) ->
    case dgiot_hook:run_hook({ProductId, ack}, [Message, State]) of
        {error, not_find} ->
            dgiot_udp_client:send(UDPState, <<"login">>),
            ack(Message, State, UDPState);
        {ok, Rtns} ->
            dgiot_udp_client:send(UDPState, Rtns),
            UDPState
    end.

ack([], State, _) ->
    State;

ack([{error, Reason} | _], State, _) ->
    {error, Reason, State};

ack([{ok, NewState} | Rest], _State, UDPState) ->
    ack(Rest, NewState, UDPState);

ack([{reply, AckFrame, NewState} | Rest], #state{productid = ProductId}, UDPState) ->
    case dgiot_product:to_frame(ProductId, AckFrame) of
        {ok, Payload} ->
            dgiot_udp_client:send(UDPState, Payload);
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason])
    end,
    ack(Rest, NewState, UDPState);

ack([NewState | Rest], _State, UDPState) ->
    ack(Rest, NewState, UDPState).

decode(Payload, _, _UDPState) when byte_size(Payload) > ?MAX_BUFF_SIZE ->
    {error, buff_size_limit};
decode(Payload, ProductId, #udp{state = State} = UDPState) when is_binary(ProductId) ->
    {ok, Rest, Messages} = dgiot_product:parse_frame(ProductId, Payload, State),
    {ok, Messages, UDPState#udp{buff = Rest}};
decode(_Payload, _, _UDPState) ->
    {error, unknown_payload}.