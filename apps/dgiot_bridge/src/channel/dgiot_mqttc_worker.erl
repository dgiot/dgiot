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

-export([childSpec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

childSpec(ChannelId, ChannelArgs) ->
    ClientId = binary_to_list(maps:get(<<"password">>, ChannelArgs)),
    Options = #{
        host => binary_to_list(maps:get(<<"address">>, ChannelArgs)),
        port => maps:get(<<"port">>, ChannelArgs),
        clientid => binary_to_list(maps:get(<<"clientid">>, ChannelArgs)),
        ssl => maps:get(<<"ssl">>, ChannelArgs, false),
        username => binary_to_list(maps:get(<<"username">>, ChannelArgs)),
        password => binary_to_list(maps:get(<<"password">>, ChannelArgs)),
        clean_start => maps:get(<<"clean_start">>, ChannelArgs, false)
    },
    Args = #{<<"channel">> => ChannelId, <<"client">> => ClientId, <<"mod">> => ?MODULE, <<"options">> => Options},
    [?CHILD(dgiot_mqtt_client, worker, [Args])].

%% mqtt client hook
init(#dclient{channel = ChannelId, client = ClientId} = State) ->
%%    io:format("~s ~p State ~p ~n",[?FILE, ?LINE, State]),
    dgiot_client:add(ChannelId, ClientId),
    {ok, State#dclient{channel = dgiot_utils:to_binary(ChannelId)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #dclient{channel = ChannelId} = State) ->
    emqtt:subscribe(Client, {<<"bridge/#">>, 1}), % cloud to edge
    timer:sleep(1000),
    dgiot_bridge:send_log(ChannelId, "~s ~p ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"connect">>})]),
    dgiot_mqtt:subscribe(<<"forward/#">>),      %  edge  to cloud
    {noreply, State#dclient{client = Client}};

handle_info(disconnect, #dclient{channel = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"disconnect">>})]),
    {noreply, State#dclient{client = disconnect}};

handle_info({publish, #{payload := Payload, topic := <<"bridge/", Topic/binary>>} = _Msg}, #dclient{channel = ChannelId} = State) ->
    dgiot_bridge:send_log(ChannelId, "cloud to edge: Topic ~p Payload ~p ~n", [Topic, Payload]),
    dgiot_mqtt:publish(ChannelId, Topic, Payload),
    {noreply, State};

handle_info({deliver, _, Msg}, #dclient{client = Client, channel = ChannelId} = State) ->
    case dgiot_mqtt:get_topic(Msg) of
        <<"forward/", Topic/binary>> ->
            dgiot_bridge:send_log(ChannelId, "edge to cloud: Topic ~p Payload ~p ~n", [Topic, dgiot_mqtt:get_payload(Msg)]),
            emqtt:publish(Client, Topic, dgiot_mqtt:get_payload(Msg));
        _ -> pass
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId} = _State) ->
    dgiot_client:stop(ChannelId, ClientId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
