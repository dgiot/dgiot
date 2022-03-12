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
-include_lib("dgiot/include/logger.hrl").
-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-include_lib("emqx_rule_engine/include/rule_actions.hrl").

-export([
    has_routes/1
    , subscribe/1
    , unsubscribe/1
    , publish/3
    , publish/4
    , shared_sub/3
    , shared_unsub/3
    , get_payload/1
    , get_topic/1
    , get_channel/1
    , republish/2
    , get_message/2]).

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

get_channel(#{
    ?BINDING_KEYS := #{
        'Params' := Params
    }}) ->
    maps:get(<<"channel">>, Params, <<"">>);

get_channel(_) ->
    <<"">>.

get_message(_Selected, #{
    timestamp := _Timestamp,
    topic := _Topic,
    payload := _Payload} = Envs) ->
    maps:without([?BINDING_KEYS], Envs);

get_message(Selected, #{
    timestamp := Timestamp,
    ?BINDING_KEYS := #{
        'Params' := _Params,
        'TopicTks' := TopicTks,
        'PayloadTks' := PayloadTks
    }} = Env) ->
    NewEnv = maps:without([?BINDING_KEYS], Env),
    NewEnv#{
        topic => emqx_rule_utils:proc_tmpl(TopicTks, Selected),
        payload => emqx_rule_utils:proc_tmpl(PayloadTks, Selected),
        timestamp => Timestamp
    };

get_message(Selected, #{
    ?BINDING_KEYS := #{
        'Params' := _Params,
        'TopicTks' := TopicTks,
        'PayloadTks' := PayloadTks
    }} = Env) ->
    NewEnv = maps:without([?BINDING_KEYS], Env),
    NewEnv#{
        topic => emqx_rule_utils:proc_tmpl(TopicTks, Selected),
        payload => emqx_rule_utils:proc_tmpl(PayloadTks, Selected),
        timestamp => erlang:system_time(millisecond)
    };

get_message(_Selected, Envs = #{
    topic := _Topic,
    headers := #{republish_by := ActId},
    ?BINDING_KEYS := #{'_Id' := ActId}
}) ->
    maps:without([?BINDING_KEYS], Envs).

republish(Selected, #{
    qos := QoS, flags := Flags, timestamp := Timestamp,
    ?BINDING_KEYS := #{
        '_Id' := ActId,
        'Params' := _Params,
        'TopicTks' := TopicTks,
        'PayloadTks' := PayloadTks
    } = Bind}) ->
    ?LOG(debug, "Selected ~p ", [Selected]),
    TargetQoS = maps:get('TargetQoS', Bind, 0),
    Msg = #message{
        id = emqx_guid:gen(),
        qos = if TargetQoS =:= -1 -> QoS; true -> TargetQoS end,
        from = ActId,
        flags = Flags,
        headers = #{republish_by => ActId},
        topic = emqx_rule_utils:proc_tmpl(TopicTks, Selected),
        payload = emqx_rule_utils:proc_tmpl(PayloadTks, Selected),
        timestamp = Timestamp
    },
    ?LOG(debug, "Msg ~p ", [Msg]),
    _ = emqx_broker:safe_publish(Msg),
    emqx_rule_metrics:inc_actions_success(ActId),
    emqx_metrics:inc_msg(Msg);

republish(Selected, #{
    ?BINDING_KEYS := #{
        '_Id' := ActId,
        'Params' := _Params,
        'TopicTks' := TopicTks,
        'PayloadTks' := PayloadTks
    } = Bind}) ->
    ?LOG(debug, "Selected ~p ", [Selected]),
    TargetQoS = maps:get('TargetQoS', Bind, 0),
    Msg = #message{
        id = emqx_guid:gen(),
        qos = if TargetQoS =:= -1 -> 0; true -> TargetQoS end,
        from = ActId,
        flags = #{dup => false, retain => false},
        headers = #{republish_by => ActId},
        topic = emqx_rule_utils:proc_tmpl(TopicTks, Selected),
        payload = emqx_rule_utils:proc_tmpl(PayloadTks, Selected),
        timestamp = erlang:system_time(millisecond)
    },
    ?LOG(debug, "Msg ~p ", [Msg]),
    _ = emqx_broker:safe_publish(Msg),
    emqx_rule_metrics:inc_actions_success(ActId),
    emqx_metrics:inc_msg(Msg);

republish(_Selected, Envs = #{
    topic := Topic,
    headers := #{republish_by := ActId},
    ?BINDING_KEYS := #{'_Id' := ActId}
}) ->
    ?LOG(debug, " msg topic: ~p, target topic: ~p",
        [Topic, ?bound_v('TargetTopic', Envs)]),
    emqx_rule_metrics:inc_actions_error(?bound_v('_Id', Envs)).

