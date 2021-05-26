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
-author("johnliu").

%% See 'Application Message' in MQTT Version 5.0
-record(message, {
    %% Global unique message ID
    id :: binary(),
    %% Message QoS
    qos = 0,
    %% Message from
    from :: atom() | binary(),
    %% Message flags
    flags = #{} :: dgiot_types:flags(),
    %% Message headers. May contain any metadata. e.g. the
    %% protocol version number, username, peerhost or
    %% the PUBLISH properties (MQTT 5.0).
    headers = #{} :: dgiot_types:headers(),
    %% Topic that the message is published to
    topic :: dgiot_types:topic(),
    %% Message Payload
    payload :: dgiot_types:payload(),
    %% Timestamp (Unit: millisecond)
    timestamp :: integer()
}).

-record(delivery, {
    sender  :: pid(),      %% Sender of the delivery
    message :: #message{}  %% The message delivered
}).
