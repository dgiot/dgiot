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


-module(dgiot_mqtt).
-author("jonhliu").
-include("dgiot_mqtt.hrl").
-export([has_routes/1
    , subscribe/1
    , unsubscribe/1
    , publish/3
    , publish/4
    , shared_sub/3
    , shared_unsub/3
    , get_payload/1
    , get_topic/1]).

has_routes(Topic) ->
    emqx_router:has_routes(Topic).

subscribe(Topic) ->
    Options = #{qos => 0},
    timer:sleep(1),
    emqx:subscribe(Topic, dgiot_utils:to_binary(self()), Options).

unsubscribe(Topic) ->
    emqx_broker:unsubscribe(iolist_to_binary(Topic)).

-spec(publish(Client :: binary(), Topic :: binary(), Payload :: binary())
        -> ok | {error, Reason :: any()}).
publish(Client, Topic, Payload) ->
    timer:sleep(10),
    Msg = emqx_message:make(dgiot_utils:to_binary(Client), 0, Topic, Payload),
    emqx:publish(Msg),
    ok.

publish(Client, Topic, Payload, check_route) ->
    case emqx_router:has_routes(Topic) of
        true ->
            publish(Client, Topic, Payload);
        false -> ok
    end;

publish(Client, Topic, Payload, _) ->
    publish(Client, Topic, Payload).

shared_sub(Group, Topic, SubPid) ->
    emqx_shared_sub:subscribe(Group, Topic, SubPid).

shared_unsub(Group, Topic, SubPid) ->
    emqx_shared_sub:unsubscribe(Group, Topic, SubPid).

get_payload(Msg) ->
    Msg#message.payload.

get_topic(Msg) ->
    Msg#message.topic.
