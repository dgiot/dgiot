%% @doc 同名门面：emqx_guid —— 仅在「无 EMQX 模式」下编译（刀 6 切换）。
%%
%% 为什么不在 src/：EMQX 在位时同名模块会与 emqx app 冲突（代码路径
%% 二义），故本目录不参与当前 build；切换刀把 compat/ 加入 src_dirs 并
%% 从 release 剔除 emqx 应用。
%%
%% 未实现的能力一律显式报错，绝不静默返回 ok。
-module(emqx_guid).
-export([gen/0, new/0, to_hexstr/1]).

gen() ->
    {error, {not_implemented, cut1, emqx_guid, gen}}.

new() ->
    {error, {not_implemented, cut1, emqx_guid, new}}.

to_hexstr(_A0) ->
    {error, {not_implemented, cut1, emqx_guid, to_hexstr}}.
