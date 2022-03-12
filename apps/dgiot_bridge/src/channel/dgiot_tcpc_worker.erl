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
    hb = 60
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
    io:format("~s ~p State ~p ~n",[?FILE,?LINE, State]),
    dgiot_tcp_client:start_link(?MODULE, Ip, Port, Recon, ReTimes, State).

init(TCPState) ->
    io:format("~s ~p TCPState ~p ~n",[?FILE,?LINE, TCPState]),
    {ok, TCPState}.

handle_info(connection_ready, TCPState) ->
    io:format("~s ~p State ~p ~n",[?FILE,?LINE, TCPState]),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    erlang:send_after(Time, self(), login),
    {noreply, TCPState};

handle_info(tcp_closed, TCPState) ->
    {noreply, TCPState};

handle_info(login, #tcp{state = #state{productid = ProductId, devaddr = DevAddr, hb = Hb} = State} = TCPState) ->
    Topic = <<"mock/", ProductId/binary, "/", DevAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(Hb * 1000, self(), heartbeat),
    do_message([{reply, #{<<"devaddr">> => DevAddr, <<"cmd">> => <<"login">>}, State}],
        State, TCPState),
    {noreply, TCPState};

handle_info(heartbeat, #tcp{state = #state{devaddr = DevAddr, hb = Hb} = State} = TCPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
    io:format("~s ~p herart ~p ~n",[?FILE,?LINE, <<"heartbeat">>]),
    dgiot_tcp_client:send(TCPState, <<"heartbeat">>),
    do_message([{reply, #{<<"devaddr">> => DevAddr, <<"cmd">> => <<"heartbeat">>}, State}],
        State, TCPState),
    {noreply, TCPState};

handle_info({tcp, Buff}, #tcp{state = #state{productid = ProductId} = State} = TCPState) ->
    case decode(Buff, ProductId, TCPState) of
        {ok, Messages, #tcp{state = NewState} = NewTcpState} ->
            case do_messages(Messages, NewState, TCPState) of
                {error, Reason, State0} ->
                    ?LOG(info, "stop Reason ~p State ~p~n", [Reason, State0]),
                    {stop, Reason, NewTcpState#tcp{state = State0}};
                NewState0 ->
                    {noreply, NewTcpState#tcp{state = NewState0}}
            end;
        {error, buff_size_limit} ->
            {stop, buff_size_limit, TCPState#tcp{state = State}};
        {error, Reason} ->
            {stop, Reason, TCPState#tcp{state = State}}
    end;

handle_info({deliver, _Topic, Msg}, #tcp{state = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Client recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
    Message =
        case jsx:is_json(Payload) of
            true ->
                jsx:decode(Payload, [{labels, binary}, return_maps]);
            false ->
                binary_to_term(Payload)
        end,
    NewState = do_message(Message, State, TCPState),
    {noreply, TCPState#tcp{state = NewState}};

handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

do_messages([], NState, _) -> NState;
do_messages([Message | Rest], State, TCPState) ->
    case do_message(Message, State, TCPState) of
        {error, Reason} ->
            {error, Reason, State};
        {ok, NState} ->
            do_messages(Rest, NState, TCPState);
        NState ->
            do_messages(Rest, NState, TCPState)
    end.

do_message(Message, State, #tcp{state = #state{productid = ProductId} = State} = TCPState) ->
    case dgiot_hook:run_hook({ProductId, ack}, [Message, State]) of
        {error, not_find} ->
            dgiot_tcp_client:send(TCPState, <<"login">>),
            ack(Message, State, TCPState);
        {ok, Rtns} ->
            dgiot_tcp_client:send(TCPState, Rtns),
            TCPState
    end.

ack([], State, _) ->
    State;

ack([{error, Reason} | _], State, _) ->
    {error, Reason, State};

ack([{ok, NewState} | Rest], _State, TCPState) ->
    ack(Rest, NewState, TCPState);

ack([{reply, AckFrame, NewState} | Rest], #state{productid = ProductId}, TCPState) ->
    case dgiot_product:to_frame(ProductId, AckFrame) of
        {ok, Payload} ->
            dgiot_tcp_client:send(TCPState, Payload);
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason])
    end,
    ack(Rest, NewState, TCPState);

ack([NewState | Rest], _State, TCPState) ->
    ack(Rest, NewState, TCPState).

decode(Payload, _, _TCPState) when byte_size(Payload) > ?MAX_BUFF_SIZE ->
    {error, buff_size_limit};
decode(Payload, ProductId, #tcp{state = State} = TCPState) when is_binary(ProductId) ->
    {ok, Rest, Messages} = dgiot_product:parse_frame(ProductId, Payload, State),
    {ok, Messages, TCPState#tcp{buff = Rest}};
decode(_Payload, _, _TCPState) ->
    {error, unknown_payload}.