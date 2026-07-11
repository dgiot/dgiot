%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_broker_emqx).

-ifdef(DGIOT_WITH_EMQX).
-behaviour(dgiot_broker).

-export([has_routes/1, subscribe_mgmt/2, unsubscribe_mgmt/2,
         subscribe_self/3, subscribe_client/2, unsubscribe_self/1,
         unsubscribe_client/2, publish/3, safe_publish/1, message_make/4,
         shared_sub/3, shared_unsub/3, lookup_subpid/1, register_sub/2,
         reclaim_seq/1, get_sub_shard/2, inc_actions_success/1, inc_msg/1]).

has_routes(Topic) ->
    emqx_router:has_routes(Topic).

subscribe_mgmt(ClientId, Topic) ->
    emqx_mgmt:subscribe(ClientId, [{Topic, #{qos => 0}}]).

unsubscribe_mgmt(ClientId, Topic) ->
    emqx_mgmt:do_unsubscribe(ClientId, Topic).

subscribe_self(Topic, SubId, Options) ->
    emqx:subscribe(Topic, SubId, Options).

subscribe_client(TopicFilter, ClientId) ->
    emqx_broker:subscribe(TopicFilter, ClientId, #{}).

unsubscribe_self(Topic) ->
    emqx_broker:unsubscribe(iolist_to_binary(Topic)).

unsubscribe_client(_ClientId, TopicFilter) ->
    emqx_broker:unsubscribe(TopicFilter).

publish(Client, Topic, Payload) ->
    Msg = emqx_message:make(dgiot_utils:to_binary(Client), 0, Topic, Payload),
    emqx:publish(Msg),
    ok.

safe_publish(Msg) ->
    _ = emqx_broker:safe_publish(Msg),
    ok.

message_make(Client, QoS, Topic, Payload) ->
    emqx_message:make(dgiot_utils:to_binary(Client), QoS, Topic, Payload).

shared_sub(Group, Topic, SubPid) ->
    emqx_shared_sub:subscribe(Group, Topic, SubPid).

shared_unsub(Group, Topic, SubPid) ->
    emqx_shared_sub:unsubscribe(Group, Topic, SubPid).

lookup_subpid(ClientId) ->
    emqx_broker_helper:lookup_subpid(ClientId).

register_sub(SubPid, SubId) ->
    emqx_broker_helper:register_sub(SubPid, SubId).

reclaim_seq(Topic) ->
    _ = emqx_broker_helper:reclaim_seq(Topic),
    ok.

get_sub_shard(SubPid, Topic) ->
    emqx_broker_helper:get_sub_shard(SubPid, Topic).

inc_actions_success(ActId) ->
    emqx_rule_metrics:inc_actions_success(ActId).

inc_msg(Msg) ->
    emqx_metrics:inc_msg(Msg).

-else.
%% DGIOT_WITH_EMQX=false: stub module, not used at runtime
-export([has_routes/1, subscribe_mgmt/2, unsubscribe_mgmt/2,
         subscribe_self/3, subscribe_client/2, unsubscribe_self/1,
         unsubscribe_client/2, publish/3, safe_publish/1, message_make/4,
         shared_sub/3, shared_unsub/3, lookup_subpid/1, register_sub/2,
         reclaim_seq/1, get_sub_shard/2, inc_actions_success/1, inc_msg/1]).

has_routes(_) -> false.
subscribe_mgmt(_, _) -> ok.
unsubscribe_mgmt(_, _) -> ok.
subscribe_self(_, _, _) -> ok.
subscribe_client(_, _) -> ok.
unsubscribe_self(_) -> ok.
unsubscribe_client(_, _) -> ok.
publish(_, _, _) -> ok.
safe_publish(_) -> ok.
message_make(C, Q, T, P) -> #{id => dgiot_guid:gen(), qos => Q, from => C, topic => T, payload => P}.
shared_sub(_, _, _) -> ok.
shared_unsub(_, _, _) -> ok.
lookup_subpid(_) -> undefined.
register_sub(_, _) -> ok.
reclaim_seq(_) -> ok.
get_sub_shard(_, _) -> 0.
inc_actions_success(_) -> ok.
inc_msg(_) -> ok.
-endif.
