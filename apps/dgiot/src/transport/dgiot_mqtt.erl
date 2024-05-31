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
%%-include_lib("emqx_rule_engine/include/rule_actions.hrl").
-define(DGIOT_MQTT_WORK, dgiot_mqtt_work).
-define(LOG_RULE_ACTION(Level, Metadata, Fmt, Args),
    emqx_rule_utils:log_action(Level, Metadata, Fmt, Args)).

-define(bound_v(Key, ENVS0),
    maps:get(Key,
        maps:get(?BINDING_KEYS, ENVS0, #{}))).

-define(BINDING_KEYS, '__bindings__').

%% ETS tables for PubSub
-define(SUBOPTION, emqx_suboption).
-define(SUBSCRIBER, emqx_subscriber).
-define(SUBSCRIPTION, emqx_subscription).
-dgiot_data("ets").
-export([init_ets/0]).
-define(DGIOT_ROUTE_KEY, dgiot_route_key).

-export([
    has_routes/1
    , subscribe/1
    , subscribe/2
    , unsubscribe/1
    , unsubscribe/2
    , publish/3
    , publish/4
    , message/3
    , shared_sub/3
    , shared_unsub/3
    , get_payload/1
    , get_topic/1
    , get_channel/1
    , republish/1
    , get_message/2
    , subopts/0
    , subscribe_route_key/3
    , unsubscribe_route_key/2
    , subscribe_mgmt/2
    , unsubscribe_mgmt/2
    , send/4
    , send/5
]).

init_ets() ->
    dgiot_data:init(?DGIOT_ROUTE_KEY).

%%
subscribe_route_key(Topics, Type, SessionToken) ->
    unsubscribe_route_key(SessionToken, Type),
    lists:foldl(fun(X, Acc) ->
        case dgiot_data:get({dlink_client, SessionToken}) of
            not_find ->
                dgiot_mqtt:subscribe_mgmt(SessionToken, X);
            Clients ->
                lists:foldl(fun(Client, _) ->
                    dgiot_mqtt:subscribe_mgmt(Client, X)
                            end, {}, Clients)
        end,
        Acc ++ [X]
                end, [], Topics),
    dgiot_data:insert(?DGIOT_ROUTE_KEY, {SessionToken, Type}, Topics).

unsubscribe_route_key(_, <<"all">>) ->
    Fun = fun({{SessionToken, Type}, _}) ->
        unsubscribe_route_key(SessionToken, Type)
          end,
    dgiot_mnesia:search(dgiot_route_key, Fun, #{});

unsubscribe_route_key(SessionToken, Type) ->
    case dgiot_data:get(?DGIOT_ROUTE_KEY, {SessionToken, Type}) of
        not_find ->
            pass;
        Topics ->
            lists:foldl(fun(X, _) ->
                dgiot_mqtt:unsubscribe_mgmt(SessionToken, X)
                        end, [], Topics),
            dgiot_data:delete(?DGIOT_ROUTE_KEY, {SessionToken, Type})
    end.

has_routes(Topic) ->
    emqx_router:has_routes(Topic).

%% 根据clientid动态订阅topic
subscribe_mgmt(ClientId, Topic) ->
    timer:sleep(1),
    emqx_mgmt:subscribe(ClientId, [{Topic, #{qos => 0}}]).

%% 根据clientid动态取消订阅topic
unsubscribe_mgmt(ClientId, Topic) ->
    timer:sleep(1),
    emqx_mgmt:do_unsubscribe(ClientId, Topic).

subscribe(Topic) ->
    Options = #{qos => 0},
    timer:sleep(1),
    emqx:subscribe(Topic, dgiot_utils:to_binary(self()), Options).

%% 根据clientid动态订阅topic
subscribe(ClientId, TopicFilter) ->
    timer:sleep(1),
    case emqx_broker_helper:lookup_subpid(ClientId) of
        Pid when is_pid(Pid) ->
            subscribe(TopicFilter, ClientId, Pid, subopts());
        _ ->
            emqx_broker:subscribe(TopicFilter, ClientId, subopts())
    end.

unsubscribe(Topic) ->
    emqx_broker:unsubscribe(iolist_to_binary(Topic)).

%% 根据clientid动态取消订阅topic
unsubscribe(ClientId, TopicFilter) ->
    timer:sleep(1),
    case emqx_broker_helper:lookup_subpid(ClientId) of
        Pid when is_pid(Pid) ->
            do_unsubscribe(TopicFilter, Pid);
        _ ->
            emqx_broker:unsubscribe(TopicFilter)
    end.

send(ProductId, DevAddr, Client, Topic, Payload) ->
    publish(Client, Topic, Payload),
    send(ProductId, DevAddr, Topic, Payload).

send(ProductId, DevAddr, Topic, Payload) ->
    case dgiot_data:get(?DGIOT_MQTT_WORK, ProductId) of
        not_find ->
            pass;
        ChannelId ->
            dgiot_client:send(ChannelId, <<ProductId:10/binary, "_", DevAddr/binary>>, Topic, dgiot_json:encode(Payload))
    end.

-spec(publish(Client :: binary(), Topic :: binary(), Payload :: binary())
        -> ok | {error, Reason :: any()}).
publish(Client, Topic, Payload) ->
    timer:sleep(1),
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

message(Client, Topic, Payload) ->
    emqx_message:make(dgiot_utils:to_binary(Client), 0, Topic, Payload).

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
        'Envs' := Params
    }}) ->
    maps:get(<<"channel">>, Params, <<"">>);

get_channel(#{
    ?BINDING_KEYS := #{
        'Params' := Params
    }}) ->
    maps:get(<<"channel">>, Params, <<"">>);

get_channel(_) ->
    <<"">>.

get_message(Selected, #{?BINDING_KEYS := #{
    '_Id' := ActId,
    'Envs' := #{
        <<"republish">> := Republish,
        <<"target_qos">> := Target_qos,
        <<"payload_tmpl">> := Payload_tmpl,
        <<"target_topic">> := Target_topic
    } = Params
}} = Envs) ->
    Republish = maps:get(<<"republish">>, Params, <<"channel">>),
    message(Selected, ActId, Payload_tmpl, Target_topic, Target_qos, Republish, Envs);

get_message(Selected, #{?BINDING_KEYS := #{
    '_Id' := ActId,
    'Params' := #{
        <<"republish">> := Republish,
        <<"target_qos">> := Target_qos,
        <<"payload_tmpl">> := Payload_tmpl,
        <<"target_topic">> := Target_topic
    } = Params
}} = Envs) ->
    Republish = maps:get(<<"republish">>, Params, <<"channel">>),
    message(Selected, ActId, Payload_tmpl, Target_topic, Target_qos, Republish, Envs);

get_message(_Selected, Envs) ->
    maps:without([?BINDING_KEYS], Envs).

message(Selected, ActId, Payload_tmpl, Target_topic, Target_qos, Republish, Envs) ->
    PayloadTks = emqx_rule_utils:preproc_tmpl(Payload_tmpl),
    TopicTks = emqx_rule_utils:preproc_tmpl(Target_topic),
    {Topic, Payload} =
        case emqx_rule_utils:proc_tmpl(PayloadTks, Selected) of
            <<"undefined">> ->
                {maps:get(topic, Envs, <<"">>), maps:get(payload, Envs, <<"{}">>)};
            Payload1 ->
                {emqx_rule_utils:proc_tmpl(TopicTks, Selected), Payload1}
        end,
    DeviceId =
        case Selected of
            #{<<"clientid">> := Clientid} ->
                Clientid;
            #{<<"devaddr">> := Devaddr, <<"productid">> := Productid} ->
                dgiot_parse_id:get_deviceid(Productid, Devaddr);
            _ ->
                <<"undefined">>
        end,
    NewEnvs = maps:without([?BINDING_KEYS], Envs),
    NewEnvs#{
        deviceid => DeviceId,
        republish_by => ActId,
        republish_mod => Republish,
        'TargetQoS' => Target_qos,
        topic => Topic,
        payload => Payload,
        timestamp => maps:get(timestamp, Envs, erlang:system_time(millisecond))
    }.

republish(#{headers := #{republish_by := _ActId}} = Envs) ->
    Envs;

republish(#{
    payload := Payload,
    topic := Topic,
    republish_by := ActId,
    'TargetQoS' := TargetQoS
}) ->
    Msg = #message{
        id = emqx_guid:gen(),
        qos = if TargetQoS =:= -1 -> 0; true -> TargetQoS end,
        from = ActId,
        flags = #{dup => false, retain => false},
        headers = #{republish_by => ActId},
        topic = Topic,
        payload = Payload,
        timestamp = erlang:system_time(millisecond)
    },
    _ = emqx_broker:safe_publish(Msg),
    emqx_rule_metrics:inc_actions_success(ActId),
    emqx_metrics:inc_msg(Msg);

republish(Envs) ->
    Envs.

%% @private
subopts() -> subopts(#{}).
subopts(Init) ->
    maps:merge(?DEFAULT_SUBOPTS, Init).

%% @private
-spec(subscribe(emqx_topic:topic(), emqx_types:subid(), pid(), emqx_types:subopts()) -> ok).
subscribe(Topic, SubId, SubPid, SubOpts0) when is_binary(Topic), is_pid(SubPid), is_map(SubOpts0) ->
    SubOpts = maps:merge(?DEFAULT_SUBOPTS, SubOpts0),
    case ets:member(?SUBOPTION, {SubPid, Topic}) of
        false -> %% New
            ok = emqx_broker_helper:register_sub(SubPid, SubId),
            do_subscribe(Topic, SubPid, with_subid(SubId, SubOpts));
        true -> %% Existed
            set_subopts(SubPid, Topic, with_subid(SubId, SubOpts)),
            ok %% ensure to return 'ok'
    end.

%% @private
set_subopts(SubPid, Topic, NewOpts) ->
    Sub = {SubPid, Topic},
    case ets:lookup(?SUBOPTION, Sub) of
        [{_, OldOpts}] ->
            ets:insert(?SUBOPTION, {Sub, maps:merge(OldOpts, NewOpts)});
        [] -> false
    end.

%%--------------------------------------------------------------------
%% Unsubscribe API
%%--------------------------------------------------------------------

-spec(do_unsubscribe(emqx_topic:topic(), pid()) -> ok).
do_unsubscribe(Topic, SubPid) when is_binary(Topic) ->
    case ets:lookup(?SUBOPTION, {SubPid, Topic}) of
        [{_, SubOpts}] ->
            _ = emqx_broker_helper:reclaim_seq(Topic),
            do_unsubscribe(Topic, SubPid, SubOpts);
        [] -> ok
    end.

do_unsubscribe(Topic, SubPid, SubOpts) ->
    true = ets:delete(?SUBOPTION, {SubPid, Topic}),
    true = ets:delete_object(?SUBSCRIPTION, {SubPid, Topic}),
    Group = maps:get(share, SubOpts, undefined),
    do_unsubscribe(Group, Topic, SubPid, SubOpts).

do_unsubscribe(undefined, Topic, SubPid, SubOpts) ->
    case maps:get(shard, SubOpts, 0) of
        0 -> true = ets:delete_object(?SUBSCRIBER, {Topic, SubPid}),
            cast(pick(Topic), {unsubscribed, Topic});
        I -> true = ets:delete_object(?SUBSCRIBER, {{shard, Topic, I}, SubPid}),
            cast(pick({Topic, I}), {unsubscribed, Topic, I})
    end;

do_unsubscribe(Group, Topic, SubPid, _SubOpts) ->
    emqx_shared_sub:unsubscribe(Group, Topic, SubPid).

%% @private
do_subscribe(Topic, SubPid, SubOpts) ->
    true = ets:insert(?SUBSCRIPTION, {SubPid, Topic}),
    Group = maps:get(share, SubOpts, undefined),
    do_subscribe(Group, Topic, SubPid, SubOpts).

do_subscribe(undefined, Topic, SubPid, SubOpts) ->
    case emqx_broker_helper:get_sub_shard(SubPid, Topic) of
        0 -> true = ets:insert(?SUBSCRIBER, {Topic, SubPid}),
            true = ets:insert(?SUBOPTION, {{SubPid, Topic}, SubOpts}),
            call(pick(Topic), {subscribe, Topic});
        I -> true = ets:insert(?SUBSCRIBER, {{shard, Topic, I}, SubPid}),
            true = ets:insert(?SUBOPTION, {{SubPid, Topic}, maps:put(shard, I, SubOpts)}),
            call(pick({Topic, I}), {subscribe, Topic, I})
    end;


%% Shared subscription
do_subscribe(Group, Topic, SubPid, SubOpts) ->
    true = ets:insert(?SUBOPTION, {{SubPid, Topic}, SubOpts}),
    emqx_shared_sub:subscribe(Group, Topic, SubPid).

-compile({inline, [with_subid/2]}).
with_subid(undefined, SubOpts) ->
    SubOpts;
with_subid(SubId, SubOpts) ->
    maps:put(subid, SubId, SubOpts).

%%--------------------------------------------------------------------
%% call, cast, pick
%%--------------------------------------------------------------------

-compile({inline, [call/2, pick/1]}).
call(Broker, Req) ->
    gen_server:call(Broker, Req, infinity).

cast(Broker, Msg) ->
    gen_server:cast(Broker, Msg).

%% Pick a broker
pick(Topic) ->
    gproc_pool:pick_worker(broker_pool, Topic).
