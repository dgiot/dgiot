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

-module(dgiot_mqtt_message).
-dgiot_data("ets").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_mqtt.hrl").
-define(DGIOT_DLINK_REQUEST_ID, dgiot_dlink_request_id).
%% ACL Callbacks
-export([
    on_message_publish/2,
    init_ets/0
]).

-define(EMPTY_USERNAME, <<"">>).

init_ets() ->
    dgiot_data:init(?DGIOT_DLINK_REQUEST_ID).

%%
on_message_publish(Message = #message{topic = <<"$dg/user/dashboard/", SessionToken:34/binary, _Rest/binary>>, payload = Payload, from = ClientId}, _State) ->
    dgiot_mqtt:publish(dgiot_utils:to_md5(ClientId), <<"dashboard_ack/", SessionToken/binary>>, Payload),
    {ok, Message};

on_message_publish(Message = #message{topic = <<"$dg/thing/", Topic/binary>>, payload = Payload, from = _ClientId, headers = _Headers}, _State) ->
    case re:split(Topic, <<"/">>) of
        [ProductId, DevAddr, <<"init">>, <<"request">>] ->
%%      初始化请求      $dg/thing/{productId}/{deviceAddr}/init/request
            dgiot_dlink_proctol:firmware_report(ProductId, DevAddr, get_payload(Payload));
        [ProductId, DevAddr, <<"properties">>, <<"report">>] ->
%%       属性获取	$dg/thing/{productId}/{deviceAddr}/properties/report	设备 => 平台
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Ts = dgiot_datetime:now_secs(),
            case dgiot_data:get(?DGIOT_DLINK_REQUEST_ID, DeviceId) of
                {OldTs, Request_id} when (Ts - OldTs) < 5 ->
%%                    $dg/user/realtimecard/{DeviceId}/report
                    RequestTopic = <<"$dg/user/", Request_id/binary, "/", DeviceId/binary, "/report">>,
                    dgiot_mqtt:send(ProductId, DevAddr, DeviceId, RequestTopic, Payload);
                _ ->
                    pass
            end,
            dgiot_mqttc_channel:send(ProductId, DevAddr, <<"$dg/thing/", Topic/binary>>, Payload),
            dgiot_dlink_proctol:properties_report(ProductId, DevAddr, get_payload(Payload));
        [ProductId, DevAddr, <<"report">>] ->
%%       属性获取	$dg/thing/{productId}/{deviceAddr}/properties/report	设备 => 平台
            dgiot_dlink_proctol:properties_report(ProductId, DevAddr, get_payload(Payload));
        [ProductId, DevAddr, <<"firmware">>, <<"report">>] ->
            dgiot_dlink_proctol:firmware_report(ProductId, DevAddr, get_payload(Payload));
        [DeviceId, <<"properties">>, <<"get">>, <<"request_id=", Request_id/binary>>] ->
%%       属性获取	$dg/thing/{deviceId}/properties/get/request_id={request_id}	用户 =>	平台
            case dgiot_device:lookup(DeviceId) of
                {ok, #{<<"devaddr">> := DevAddr, <<"productid">> := ProductId}} ->
%%                  属性获取	$dg/device/{productId}/{deviceAddr}/properties 	平台 =>	设备
                    RequestTopic = <<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/properties">>,
                    dgiot_data:insert(?DGIOT_DLINK_REQUEST_ID, DeviceId, {dgiot_datetime:now_secs(), Request_id}),
                    dgiot_mqtt:send(ProductId, DevAddr, DeviceId, RequestTopic, Payload);
                _ ->
                    pass
            end;
        _ ->
            pass
    end,
    {ok, Message};

on_message_publish(Message = #message{topic = _Topic, payload = _Payload}, _State) ->
    {ok, Message}.

get_payload(Payload) ->
%%    io:format("~s ~p Payload: ~p~n", [?FILE, ?LINE, Payload]),
    case jsx:is_json(Payload) of
        true ->
            jiffy:decode(Payload, [return_maps]);
        false ->
            Payload
    end.
