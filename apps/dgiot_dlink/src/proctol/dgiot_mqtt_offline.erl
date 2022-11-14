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

-module(dgiot_mqtt_offline).

%% ACL Callbacks
-export([on_client_disconnected/4, on_session_terminated/4, description/0]).

%% 离线消息
on_client_disconnected(#{clientid := <<ProductID:10/binary, "_", DeviceAddr/binary>>, username := ProductID}, _ReasonCode, #{disconnected_at := _DisconnectedAt}, _State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductID, DeviceAddr),
    dgiot_device:offline(DeviceId),
    io:format("~s ~p ProductID = ~p DeviceAddr ~p DeviceId ~p ~n", [?FILE, ?LINE, ProductID, DeviceAddr, DeviceId]),
    ok;

on_client_disconnected(_Client, _ReasonCode, _, _State) ->
    ok.

on_session_terminated(#{clientid := <<ProductID:10/binary, "_", DeviceAddr/binary>>, username := ProductID}, _Reason, _SessInfo, _State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductID, DeviceAddr),
    dgiot_device:offline(DeviceId),
    io:format("~s ~p ProductID = ~p DeviceAddr ~p DeviceId ~p ~n", [?FILE, ?LINE, ProductID, DeviceAddr, DeviceId]),
    ok;

on_session_terminated(_ClientInfo, _Reason, _SessInfo, _State) ->
%%    io:format("~s ~p ClientInfo = ~p.~n", [?FILE, ?LINE, ClientInfo]),
    ok.

description() -> "Disconnected with Dlink".

%%--------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
