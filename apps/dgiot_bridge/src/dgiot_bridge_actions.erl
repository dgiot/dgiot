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

-module(dgiot_bridge_actions).
-author("johnliu").
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("emqx_rule_engine/include/rule_engine.hrl").
-include_lib("emqx_rule_engine/include/rule_actions.hrl").

-define(RESOURCE_TYPE_DGIOT, 'dgiot_resource').

-define(RESOURCE_CONFIG_SPEC, #{
    channel => #{
        order => 1,
        type => string,
        required => true,
        default => <<"">>,
        title => #{
            en => <<"DGIOT Channel ID">>,
            zh => <<"物联网通道ID"/utf8>>
        },
        description => #{
            en => <<"DGIOT Channel ID">>,
            zh => <<"物联网通道ID"/utf8>>}
    }
}).

-define(ACTION_DATA_SPEC, #{
    '$resource' => ?ACTION_PARAM_RESOURCE,
    target_topic => #{
        order => 1,
        type => string,
        required => true,
        default => <<"thing/${productid}/${clientid}/post">>,
        title => #{en => <<"Target Topic">>,
            zh => <<"目的主题"/utf8>>},
        description => #{en => <<"To which topic the message will be republished">>,
            zh => <<"重新发布消息到哪个主题"/utf8>>}
    },
    target_qos => #{
        order => 2,
        type => number,
        enum => [-1, 0, 1, 2],
        required => true,
        default => 0,
        title => #{en => <<"Target QoS">>,
            zh => <<"目的 QoS"/utf8>>},
        description => #{en => <<"The QoS Level to be uses when republishing the message. Set to -1 to use the original QoS">>,
            zh => <<"重新发布消息时用的 QoS 级别, 设置为 -1 以使用原消息中的 QoS"/utf8>>}
    },
    payload_tmpl => #{
        order => 3,
        type => string,
        input => textarea,
        required => false,
        default => <<"${payload}">>,
        title => #{en => <<"Payload Template">>,
            zh => <<"消息内容模板"/utf8>>},
        description => #{en => <<"The payload template, variable interpolation is supported">>,
            zh => <<"消息内容模板，支持变量"/utf8>>}
    }
}).

-define(ACTION_PARAM_RESOURCE, #{
    type => string,
    required => true,
    title => #{en => <<"Resource ID">>, zh => <<"资源 ID"/utf8>>},
    description => #{en => <<"Bind a resource to this action">>,
        zh => <<"给动作绑定一个资源"/utf8>>}
}).

-resource_type(#{
    name => ?RESOURCE_TYPE_DGIOT,
    create => on_resource_create,
    status => on_get_resource_status,
    destroy => on_resource_destroy,
    params => ?RESOURCE_CONFIG_SPEC,
    title => #{en => <<"DGIOT Bridge">>, zh => <<"DGIOT Bridge"/utf8>>},
    description => #{en => <<"MQTT Message Bridge">>, zh => <<"MQTT 消息桥接"/utf8>>}
}).

-rule_action(#{name => dgiot,
    category => data_forward,
    for => '$any',
    types => [?RESOURCE_TYPE_DGIOT],
    create => on_action_create_dgiot,
    params => ?ACTION_DATA_SPEC,
    title => #{en => <<"DGIOT CHANNEL">>,
        zh => <<"数蛙物联网通道"/utf8>>},
    description => #{en => <<"Republish a MQTT message to dgiot channel">>,
        zh => <<"重新发布消息到物联网通道"/utf8>>}
}).


-export([on_resource_create/2
    , on_get_resource_status/2
    , on_resource_destroy/2
]).

%% callbacks for rule engine
-export([on_action_create_dgiot/2
]).

-export([on_action_dgiot/2
]).

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(ResId, Conf) ->
    ?LOG(debug, "ResId ~p, Conf ~p", [ResId, Conf]),
    ChannelId = maps:get(<<"channel">>, Conf, <<"">>),
    #{<<"channel">> => ChannelId}.

-spec(on_get_resource_status(ResId :: binary(), Params :: map()) -> Status :: map()).
on_get_resource_status(_ResId, _Conf) ->
    #{is_alive => true}.

on_resource_destroy(ResId, Conf) ->
    ?LOG(debug, "on_resource_destroy ~p,~p", [ResId, Conf]),
    case catch emqx_rule_registry:remove_resource(ResId) of
        {'EXIT', {{throw, {dependency_exists, {rule, RuleId}}}, _}} ->
            ok = emqx_rule_registry:remove_rule(RuleId),
            ok = emqx_rule_registry:remove_resource(ResId);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% Action 'dgiot'
%%------------------------------------------------------------------------------
-spec on_action_create_dgiot(action_instance_id(), Params :: map()) -> {bindings(), NewParams :: map()}.
on_action_create_dgiot(_Id, Params = #{
    <<"target_topic">> := TargetTopic,
    <<"target_qos">> := _TargetQoS,
    <<"payload_tmpl">> := PayloadTmpl
}) ->
    TopicTks = emqx_rule_utils:preproc_tmpl(TargetTopic),
    PayloadTks = emqx_rule_utils:preproc_tmpl(PayloadTmpl),
    ?LOG(debug, " msg topic: ~p, payload: ~p", [TopicTks, PayloadTks]),
    Params.

%% mqtt事件
%%[ 'client.connected'
%%, 'client.disconnected'
%%, 'session.subscribed'
%%, 'session.unsubscribed'
%%, 'message.publish'
%%, 'message.delivered'
%%, 'message.acked'
%%, 'message.dropped'
%%]
-spec on_action_dgiot(selected_data(), env_vars()) -> any().
on_action_dgiot(Selected, #{event := Event} = Envs) ->
    ChannelId = dgiot_mqtt:get_channel(Envs),
    Msg = dgiot_mqtt:get_message(Selected, Envs),
%%    io:format("Event = ~p.~n", [Event]),
%%    io:format("Msg = ~p.~n", [Msg]),
    case Event of
        'message.publish' ->
            post_rule(Msg),
            case dgiot_channelx:do_message(ChannelId, {rule, Msg, Selected}) of
                not_find -> dgiot_mqtt:republish(Selected, Envs);
                _ -> pass
            end;
        EventId ->
            dgiot_channelx:do_event(ChannelId, EventId, {rule, Msg, Selected})
    end.

%% SELECT payload, payload.dump_energy as dump_energy, clientid, 'productid' as productid FROM "notification/c1e44b39f0/868615051803274/#"
%% WHERE (dump_energy < 90 or ad > 1) And ( da = 1 and d > 1) And ()
post_rule(#{metadata := #{rule_id := <<"rule:Notification_", Ruleid/binary>>}, clientid := DevAddr, payload := Payload, topic := _Topic}) ->
%%    ?LOG(info, "Msg ~p", [Msg]),
    NewPayload = jsx:decode(Payload, [{labels, binary}, return_maps]),
    dgiot_umeng:add_notification(Ruleid, DevAddr, NewPayload);

%% SELECT payload, payload.dump_energy as dump_energy, clientid, 'productid' as productid FROM "notification/c1e44b39f0/868615051803274/#" WHERE dump_energy < 90
post_rule(#{metadata := #{rule_id := <<"rule:profile_", Ruleid/binary>>}, clientid := DevAddr, payload := Payload, topic := _Topic}) ->
%%    ?LOG(info, "Msg ~p", [Msg]),
    NewPayload = jsx:decode(Payload, [{labels, binary}, return_maps]),
    dgiot_umeng:add_notification(Ruleid, DevAddr, NewPayload);

post_rule(Msg) ->
%%    io:format("~s ~p Msg = ~p.~n", [?FILE, ?LINE, Msg]),
    ?LOG(debug, "~s ~p Msg = ~p.~n", [?FILE, ?LINE, Msg]).
