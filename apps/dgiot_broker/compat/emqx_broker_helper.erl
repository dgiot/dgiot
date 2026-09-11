%% @doc 同名承接：emqx_broker_helper（订阅分片助手）→ 我们的路由簿。
%% EMQX 集群里订阅按 shard 分片；我们单节点 = 单一分片，语义显式声明。
-module(emqx_broker_helper).

-export([register_sub/2, lookup_subpid/1, get_sub_shard/2, reclaim_seq/1,
         shard_count/0]).

%% 单一分片：总是 shard 0
get_sub_shard(_Topic, _ShardCount) -> 0.
shard_count() -> 1.

%% 进程级订阅登记（EMQX 的订阅表用 subpid 反查；我们路由簿以 pid 存）
register_sub(_SubPid, _Sub) ->
    {error, {not_implemented, cut7, shared_sub_registry}}.

lookup_subpid(_SubId) ->
    {error, {not_implemented, cut7, shared_sub_registry}}.

%% 消息序号回收（QoS 消息队列）——刀 4 inflight；显式声明
reclaim_seq(_SeqId) ->
    {error, {not_implemented, cut4, inflight_seq}}.
