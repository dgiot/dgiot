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
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").
-include("dgiot_bridge.hrl").

-export([start/2, childSpec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start(ChannelId, #{<<"product">> := Products} = ChannelArgs) ->
    lists:map(fun({ProductId, #{<<"productSecret">> := ProductSecret}}) ->
        dgiot_data:insert(?DGIOT_MQTT_WORK, ProductId, ChannelId),
        Success = fun(Page) ->
            lists:map(fun(#{<<"devaddr">> := DevAddr}) ->
                Clientid = <<ProductId/binary, "_", DevAddr/binary>>,
                io:format("~s ~p Clientid ~p ~n", [?FILE, ?LINE, Clientid]),
                Host = dgiot_utils:resolve(maps:get(<<"address">>, ChannelArgs)),
                Options = #{
                    host => Host,
                    port => maps:get(<<"port">>, ChannelArgs),
                    clientid => binary_to_list(Clientid),
                    ssl => maps:get(<<"ssl">>, ChannelArgs),
                    username => binary_to_list(ProductId),
                    password => binary_to_list(ProductSecret),
                    clean_start => maps:get(<<"clean_start">>, ChannelArgs)
                },
                dgiot_client:start(ChannelId, Clientid, #{<<"options">> => Options})
                      end, Page)
                  end,
        Query = #{
            <<"order">> => <<"updatedAt">>,
            <<"keys">> => [<<"devaddr">>],
            <<"where">> => #{<<"product">> => ProductId}
        },
        dgiot_parse_loader:start(<<"Device">>, Query, 0, 100, 1000000, Success)
              end, Products).

childSpec(ChannelId, _ChannelArgs) ->
    Args = #{<<"channel">> => ChannelId, <<"mod">> => ?MODULE},
    dgiot_client:register(ChannelId, mqtt_client_sup, Args).

%% mqtt client hook
init(#dclient{channel = ChannelId, client = ClientId} = State) ->
%%    io:format("~s ~p State ~p ~n",[?FILE, ?LINE, State]),
    dgiot_client:add(ChannelId, ClientId),
    {ok, State#dclient{channel = dgiot_utils:to_binary(ChannelId)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #dclient{channel = ChannelId, client = <<ProductId:10/binary, "_", DevAddr/binary>>} = State) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"connect">>})]),
    erlang:send_after(1000, self(), {sub, Client, ProductId, DevAddr}),
    {noreply, State#dclient{client = Client}};

handle_info(disconnect, #dclient{channel = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"disconnect">>})]),
    {noreply, State#dclient{client = disconnect}};

handle_info({sub, Client, ProductId, DevAddr},State) ->
    emqtt:subscribe(Client, {<<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/#">>, 1}), % cloud to edge
    dgiot_mqtt:subscribe(<<"edge2cloud/#">>),      %  edge  to cloud
    {noreply, State};

%% #{client_pid => <0.11482.0>, dup => false, packet_id => undefined, payload => <<"{ \"msg\": \"Hello, World!\" }">>, topic =><<"$dg/device/5392ccb3d7/00E0B45BFB4F_usb6-ai15/test">>, properties => undefined,qos => 0, retain => false}
handle_info({publish, Topic, Payload}, #dclient{channel = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p cloud to edge Topic ~p Payload ~p ~n", [?FILE, ?LINE, Topic, Payload]),
    {noreply, State};

handle_info({dclient_ack, Topic, Payload}, #dclient{client = Client, channel = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "edge to cloud: Topic ~p Payload ~p ~n", [Topic, Payload]),
    emqtt:publish(Client, Topic, Payload),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("~s ~p _Info ~p", [?FILE, ?LINE, _Info]),
    {noreply, State}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId} = _State) ->
    dgiot_client:stop(ChannelId, ClientId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
