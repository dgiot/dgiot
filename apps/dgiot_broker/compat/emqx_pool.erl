%% @doc 同名承接：emqx_pool（异步任务池）。
%% EMQX 用 gproc_pool 常驻 worker；本 broker 单节点，异步提交以 spawn 隔离
%% 进程等价实现（retainer dispatch 等 fire-and-forget 场景，结果被丢弃）。
-module(emqx_pool).

-export([async_submit/1, async_submit/2]).

%% task() 形态：{Fun, Args} 或 0 元 fun
async_submit({Fun, Args}) when is_function(Fun), is_list(Args) ->
    spawn(fun() -> apply(Fun, Args) end),
    ok;
async_submit(Fun) when is_function(Fun, 0) ->
    spawn(Fun),
    ok.

async_submit(Fun, Args) when is_function(Fun), is_list(Args) ->
    spawn(fun() -> apply(Fun, Args) end),
    ok.
