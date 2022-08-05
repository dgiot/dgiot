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

-module(dgiot_tcp2dlink_worker).
-author("kenneth").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").

-define(TYPE, <<"TCP2DLINK">>).
-define(MAX_BUFF_SIZE, 1024).


-record(state, {
    id,
    productId
    ,
    is_sub
}).


%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductId} ->
%%                do_cmd(ProductId, connection_ready, <<>>, TCPState),
            NewState = State#state{productId = ProductId},
            {ok, TCPState#tcp{state = NewState}};
        {error, not_find} ->
            {error, not_find_channel}
    end;
init(#tcp{state = State} = _TCPState) ->
    io:format("~s ~p State = ~p ~n", [?FILE, ?LINE, State]),
    {state, ID} = State,
    io:format("~s ~p ID = ~p ~n", [?FILE, ?LINE, ID]),
    ok.


%% task2device 下行
handle_info({deliver, _, Msg}, TCPState) ->
    io:format("~s ~p here ~n", [?FILE, ?LINE]),
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"thing">>, ProductId, DevAddr, <<"tcp">>, <<"hex">>] ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            dgiot_device:save_log(DeviceId, Payload, ['tcp_send']),
            dgiot_tcp_server:send(TCPState, dgiot_utils:hex_to_binary(dgiot_utils:trim_string(Payload))),
            {noreply, TCPState};
        _ ->
            case jsx:is_json(Payload) of
                true ->
                    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
                        #{<<"cmd">> := <<"send">>} = Cmd ->
                            handle_info(Cmd, TCPState);
                        Info ->
                            handle_info(Info, TCPState)
                    end;
                false ->
                    {noreply, TCPState}
            end
    end;

% device2task 上行
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, productId = ProductIds,is_sub = Is_sub} = State} = TCPState) ->
    dgiot_bridge:send_log(ChannelId,  "Payload ~s", [dgiot_utils:to_list(Buff)]),
    case jsx:is_json(Buff) of
        true ->
            Map = jsx:decode(Buff, [{labels, binary}, return_maps]),
            ProductId = lists:nth(1, ProductIds),
            dgiot_bridge:send_log(ChannelId, ProductId, " Recv ~s", [jsx:encode(Map)]),
            case maps:find(<<"devaddr">>, Map) of
                {ok, DevAddr} ->
                    NewState = case Is_sub of
                        0 ->
                            dgiot_mqtt:subscribe(<<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/#">>),
                             State#state{is_sub = 1} ;
                        _ ->

                            State
                    end,
                    Payload = maps:without([<<"devaddr">>], Map),
                    dgiot_dlink_proctol:properties_report(ProductId, DevAddr, Payload),
                    dgiot_bridge:send_log(ChannelId, ProductId, "Device:~p Save_td ~s", [DevAddr,jsx:encode(Payload)]),
                {noreply, TCPState#tcp{state = NewState}};
                _ ->
                    pass
            end;
        _ ->
            {noreply, TCPState}
    end;

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{state = #state{productId = _ProductId}} = _TCPState) ->
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

