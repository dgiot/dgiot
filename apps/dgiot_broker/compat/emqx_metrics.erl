%% @doc 同名门面：emqx_metrics —— 仅在「无 EMQX 模式」下编译（刀 6 切换）。
%%
%% 为什么不在 src/：EMQX 在位时同名模块会与 emqx app 冲突（代码路径
%% 二义），故本目录不参与当前 build；切换刀把 compat/ 加入 src_dirs 并
%% 从 release 剔除 emqx 应用。
%%
%% 未实现的能力一律显式报错，绝不静默返回 ok。
-module(emqx_metrics).
-export([ensure/1, inc/2, inc_msg/1, set/2, val/1]).

ensure(_A0) ->
    {error, {not_implemented, cut4, emqx_metrics, ensure}}.

inc(_A0, _A1) ->
    {error, {not_implemented, cut4, emqx_metrics, inc}}.

inc_msg(_A0) ->
    {error, {not_implemented, cut4, emqx_metrics, inc_msg}}.

set(_A0, _A1) ->
    {error, {not_implemented, cut4, emqx_metrics, set}}.

val(_A0) ->
    {error, {not_implemented, cut4, emqx_metrics, val}}.
