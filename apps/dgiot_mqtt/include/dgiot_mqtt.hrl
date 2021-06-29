%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 John liu <johnliu@iotn2n.com>.
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

%% @doc tcp_bridge COMMAND.

%%--------------------------------------------------------------------
%% Frame Size Limits
%%
%% To prevent malicious clients from exploiting memory allocation in a server,
%% servers MAY place maximum limits on:
%%
%% the number of frame headers allowed in a single frame
%% the maximum length of header lines
%% the maximum size of a frame body
%%
%% If these limits are exceeded the server SHOULD send the client an ERROR frame
%% and then close the connection.
%%--------------------------------------------------------------------
-define(SHUWA_MQTT_WORK, shuwa_mqtt_work).
-define(GROUP_TOPIC(Di, Mid), <<"group/", Di/binary,"/",Mid/binary>>).
-define(COMSUMER_KEY(Di), <<"comsumer/",Di/binary>>).

%% See 'Application Message' in MQTT Version 5.0
-record(message, {
  %% Global unique message ID
  id :: binary(),
  %% Message QoS
  qos = 0,
  %% Message from
  from :: atom() | binary(),
  %% Message flags
  flags :: #{atom() => boolean()},
  %% Message headers, or MQTT 5.0 Properties
  headers :: map(),
  %% Topic that the message is published to
  topic :: binary(),
  %% Message Payload
  payload :: binary(),
  %% Timestamp (Unit: millisecond)
  timestamp :: integer()
}).

-record(delivery, {
  sender  :: pid(),      %% Sender of the delivery
  message :: #message{}  %% The message delivered
}).


