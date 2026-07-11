%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_broker).

-type topic() :: binary().
-type clientid() :: binary().
-type payload() :: binary().
-type qos() :: 0 | 1 | 2.

-callback has_routes(topic()) -> boolean().
-callback subscribe_mgmt(clientid(), topic()) -> ok.
-callback unsubscribe_mgmt(clientid(), topic()) -> ok.
-callback subscribe_self(topic(), clientid(), map()) -> ok.
-callback subscribe_client(clientid(), topic()) -> ok.
-callback unsubscribe_self(topic()) -> ok.
-callback unsubscribe_client(clientid(), topic()) -> ok.
-callback publish(clientid(), topic(), payload()) -> ok.
-callback safe_publish(map()) -> ok.
-callback message_make(clientid(), qos(), topic(), payload()) -> map().
-callback shared_sub(binary(), topic(), pid()) -> ok.
-callback shared_unsub(binary(), topic(), pid()) -> ok.
-callback lookup_subpid(clientid()) -> pid() | undefined.
-callback register_sub(pid(), clientid()) -> ok.
-callback reclaim_seq(topic()) -> ok.
-callback get_sub_shard(pid(), topic()) -> non_neg_integer().
-callback inc_actions_success(term()) -> ok.
-callback inc_msg(term()) -> ok.

-export([has_routes/1, subscribe_mgmt/2, unsubscribe_mgmt/2,
         subscribe_self/3, subscribe_client/2, unsubscribe_self/1,
         unsubscribe_client/2, publish/3, safe_publish/1, message_make/4,
         shared_sub/3, shared_unsub/3, lookup_subpid/1, register_sub/2,
         reclaim_seq/1, get_sub_shard/2, inc_actions_success/1, inc_msg/1,
         broker_module/0]).

broker_module() -> dgiot_broker_native.

has_routes(Topic) ->
    (broker_module()):has_routes(Topic).

subscribe_mgmt(ClientId, Topic) ->
    (broker_module()):subscribe_mgmt(ClientId, Topic).

unsubscribe_mgmt(ClientId, Topic) ->
    (broker_module()):unsubscribe_mgmt(ClientId, Topic).

subscribe_self(Topic, SubId, Options) ->
    (broker_module()):subscribe_self(Topic, SubId, Options).

subscribe_client(ClientId, Topic) ->
    (broker_module()):subscribe_client(ClientId, Topic).

unsubscribe_self(Topic) ->
    (broker_module()):unsubscribe_self(Topic).

unsubscribe_client(ClientId, Topic) ->
    (broker_module()):unsubscribe_client(ClientId, Topic).

publish(Client, Topic, Payload) ->
    (broker_module()):publish(Client, Topic, Payload).

safe_publish(Msg) ->
    (broker_module()):safe_publish(Msg).

message_make(Client, QoS, Topic, Payload) ->
    (broker_module()):message_make(Client, QoS, Topic, Payload).

shared_sub(Group, Topic, SubPid) ->
    (broker_module()):shared_sub(Group, Topic, SubPid).

shared_unsub(Group, Topic, SubPid) ->
    (broker_module()):shared_unsub(Group, Topic, SubPid).

lookup_subpid(ClientId) ->
    (broker_module()):lookup_subpid(ClientId).

register_sub(SubPid, SubId) ->
    (broker_module()):register_sub(SubPid, SubId).

reclaim_seq(Topic) ->
    (broker_module()):reclaim_seq(Topic).

get_sub_shard(SubPid, Topic) ->
    (broker_module()):get_sub_shard(SubPid, Topic).

inc_actions_success(ActId) ->
    (broker_module()):inc_actions_success(ActId).

inc_msg(Msg) ->
    (broker_module()):inc_msg(Msg).
