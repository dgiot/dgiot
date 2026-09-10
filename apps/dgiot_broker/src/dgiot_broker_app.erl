%% @doc dgiot_broker 应用入口。
%%
%% 刀 1 行为：只启动空监督树并记录当前 backend，**不触碰 EMQX**、
%% 不注册监听器、不改动任何现网行为（验收要求：241 项回归全绿）。
-module(dgiot_broker_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Backend = dgiot_broker:backend(),
    logger:notice("[dgiot_broker] backend=~p (~p)",
                  [Backend, dgiot_broker:backend_info()]),
    dgiot_broker_sup:start_link().

stop(_State) ->
    ok.
