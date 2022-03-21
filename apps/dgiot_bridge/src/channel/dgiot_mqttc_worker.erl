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
-module(dgiot_mqttc_worker).
-author("johnliu").
-record(state, {id, client = disconnect}).
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_bridge.hrl").

-export([childSpec/3, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

childSpec(ChannelId, State, ChannelArgs) ->
    Options = [
        {host, binary_to_list(maps:get(<<"address">>, ChannelArgs))},
        {port, maps:get(<<"port">>, ChannelArgs)},
        {clientid, ChannelId},
        {ssl, maps:get(<<"ssl">>, ChannelArgs, false)},
        {username, binary_to_list(maps:get(<<"username">>, ChannelArgs))},
        {password, binary_to_list(maps:get(<<"password">>, ChannelArgs))},
        {clean_start, maps:get(<<"clean_start">>, ChannelArgs, false)}
    ],
    [{dgiot_mqtt_client, {dgiot_mqtt_client, start_link, [?MODULE, [State], Options]}, permanent, 5000, worker, [dgiot_mqtt_client]}].

%% mqtt client hook
init([State]) ->
    {ok, State#state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #state{id = ChannelId} = State) ->
    emqtt:subscribe(Client, {<<"bridge/#">>, 1}),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _Type, ProductIds} ->
            case ProductIds of
                [] -> pass;
                _ ->
                    lists:map(fun(ProductId) ->
%%                      dgiot_product:load(ProductId),
                        emqtt:subscribe(Client, {<<"bridge/thing/", ProductId/binary, "/#">>, 1}),
                        dgiot_mqtt:subscribe(<<"forward/thing/", ProductId/binary, "/+/post">>),
                        dgiot_mqtt:publish(ChannelId, <<"thing/", ProductId/binary>>, jsx:encode(#{<<"network">> => <<"connect">>}))
                              end, ProductIds)
            end,
            ?LOG(info, "connect ~p sub ~n", [Client]);
        _ -> pass
    end,
    {noreply, State#state{client = Client}};

handle_info(disconnect, #state{id = ChannelId} = State) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _Type, ProductIds} ->
            case ProductIds of
                [] -> pass;
                _ ->
                    lists:map(fun(ProductId) ->
                        dgiot_mqtt:publish(ChannelId, <<"thing/", ProductId/binary>>, jsx:encode(#{<<"network">> => <<"disconnect">>}))
                              end, ProductIds)
            end;
        _ -> pass
    end,
    {noreply, State#state{client = disconnect}};

handle_info({publish, #{payload := Payload, topic := <<"bridge/", Topic/binary>>} = _Msg}, #state{id = ChannelId} = State) ->
    dgiot_mqtt:publish(ChannelId, Topic, Payload),
    {noreply, State};

handle_info({deliver, _, Msg}, #state{client = Client} = State) ->
    case dgiot_mqtt:get_topic(Msg) of
        <<"forward/", Topic/binary>> ->
            emqtt:publish(Client, Topic, dgiot_mqtt:get_payload(Msg));
        _ -> pass
    end,
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(info, "unkknow ~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
