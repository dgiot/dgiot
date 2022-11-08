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
-module(dgiot_mock_udp).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-export([childspec/2]).

%% API
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).


childspec(ChannelId, ChannelArgs) ->
    Options = #{
        host => binary_to_list(maps:get(<<"address">>, ChannelArgs, <<"127.0.0.1">>)),
        port => maps:get(<<"port">>, ChannelArgs, 1883),
        ssl => maps:get(<<"ssl">>, ChannelArgs, false),
        username => binary_to_list(maps:get(<<"username">>, ChannelArgs, <<"anonymous">>)),
        password => binary_to_list(maps:get(<<"password">>, ChannelArgs, <<"password">>)),
        clean_start => maps:get(<<"clean_start">>, ChannelArgs, false)
    },
    Args = #{<<"channel">> => ChannelId, <<"mod">> => ?MODULE, <<"options">> => Options},
    dgiot_client:register(ChannelId, mqtt_client_sup, Args).

%%  callback
init(#dclient{channel = ChannelId} = State) ->
    {ok, State#dclient{channel = dgiot_utils:to_binary(ChannelId)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connect, Client}, #dclient{channel = ChannelId, client = ClientId} = Dclient) ->
    emqtt:subscribe(Client, {<<ClientId/binary, "/#">>, 1}), % cloud to edge
    dgiot_bridge:send_log(ChannelId, "~s ~p  ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"connect">>})]),
    update(ChannelId),
    {noreply, Dclient};

handle_info(disconnect, #dclient{channel = ChannelId} = Dclient) ->
    dgiot_bridge:send_log(ChannelId, "~s ~p  ~p ~n", [?FILE, ?LINE, jsx:encode(#{<<"network">> => <<"disconnect">>})]),
    {noreply, Dclient};

handle_info({publish, #{payload := Payload, topic := Topic} = _Msg}, #dclient{channel = ChannelId} = State) ->
    io:format("~s ~p ChannelId ~p Topic ~p  Payload ~p  ~n", [?FILE, ?LINE, ChannelId, Topic, Payload]),
    dgiot_bridge:send_log(ChannelId, "cloud to edge: Topic ~p Payload ~p ~n", [Topic, Payload]),
%%    dgiot_mqtt:publish(ChannelId, Topic, Payload),
    {noreply, State};

handle_info({deliver, _, Msg}, #dclient{client = Client, channel = ChannelId} = State) ->
    case dgiot_mqtt:get_topic(Msg) of
        <<"forward/", Topic/binary>> ->
            dgiot_bridge:send_log(ChannelId, "edge  to cloud: Topic ~p Payload ~p ~n", [Topic, dgiot_mqtt:get_payload(Msg)]),
            emqtt:publish(Client, Topic, dgiot_mqtt:get_payload(Msg));
        _ -> pass
    end,
    {noreply, State};

handle_info(_Info, Dclient) ->
%%    ?LOG(info,"ecapturer ~p~n", [_Info]),
    {noreply, Dclient}.

terminate(_Reason, #dclient{channel = ChannelId, client = ClientId}) ->
%%    ?LOG(info,"_Reason ~p~n", [_Reason]),
    dgiot_client:stop(ChannelId, ClientId),
    update(ChannelId),
    ok.

code_change(_OldVsn, Dclient, _Extra) ->
    {ok, Dclient}.

update(ChannelId) ->
    dgiot_data:insert({<<"mqtt_online">>, dlink_metrics}, dgiot_client:count(ChannelId)).
