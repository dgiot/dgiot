%% @doc 同名门面：emqx_broker_helper —— 仅在「无 EMQX 模式」下编译（刀 6 切换）。
%%
%% 为什么不在 src/：EMQX 在位时同名模块会与 emqx app 冲突（代码路径
%% 二义），故本目录不参与当前 build；切换刀把 compat/ 加入 src_dirs 并
%% 从 release 剔除 emqx 应用。
%%
%% 未实现的能力一律显式报错，绝不静默返回 ok。
-module(emqx_broker_helper).
-export([get_sub_shard/2, lookup_subpid/1, reclaim_seq/1, register_sub/2]).

get_sub_shard(_A0, _A1) ->
    {error, {not_implemented, cut4, emqx_broker_helper, get_sub_shard}}.

lookup_subpid(_A0) ->
    {error, {not_implemented, cut4, emqx_broker_helper, lookup_subpid}}.

reclaim_seq(_A0) ->
    {error, {not_implemented, cut4, emqx_broker_helper, reclaim_seq}}.

register_sub(_A0, _A1) ->
    {error, {not_implemented, cut4, emqx_broker_helper, register_sub}}.
