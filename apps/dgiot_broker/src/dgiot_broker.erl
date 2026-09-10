%% @doc dgiot_broker 端口实现（派发层）。
%%
%% backend 选择：application env `backend` = emqx | dgiot
%%   emqx   —— 现网模式：委托 EMQX（本刀先做，行为与今天完全一致）
%%   dgiot  —— 自研模式：刀 3-5 落地后可用，刀 6 切换
%%
%% 能力矩阵 capabilities/0 供测试与运维断言"当前内核到底支持什么"。
-module(dgiot_broker).
-behaviour(dgiot_broker_port).

-export([start_listener/2, stop_listener/1,
         publish/3, subscribe/3, unsubscribe/2,
         routes/1, sessions/0,
         capabilities/0, backend_info/0,
         backend/0, backend_mod/0, set_backend/1]).

-define(BACKENDS, #{emqx => dgiot_broker_backend_emqx,
                    dgiot => dgiot_broker_native}).

-spec backend() -> emqx | dgiot.
backend() ->
    case application:get_env(dgiot_broker, backend) of
        {ok, B} when B =:= emqx; B =:= dgiot -> B;
        _ -> emqx
    end.

-spec set_backend(emqx | dgiot) -> ok.
set_backend(B) when B =:= emqx; B =:= dgiot ->
    application:set_env(dgiot_broker, backend, B).

-spec backend_mod() -> module().
backend_mod() ->
    maps:get(backend(), ?BACKENDS).

%% -- 生命周期 ------------------------------------------------------
start_listener(Name, Opts) -> call(start_listener, [Name, Opts]).
stop_listener(Name) -> call(stop_listener, [Name]).

%% -- 数据面 --------------------------------------------------------
publish(ClientId, Topic, Payload) -> call(publish, [ClientId, Topic, Payload]).
subscribe(ClientId, Filter, Opts) -> call(subscribe, [ClientId, Filter, Opts]).
unsubscribe(ClientId, Filter) -> call(unsubscribe, [ClientId, Filter]).

%% -- 观测面 --------------------------------------------------------
routes(Topic) -> call(routes, [Topic]).
sessions() -> call(sessions, []).
capabilities() -> call(capabilities, []).
backend_info() -> call(backend_info, []).

%% 统一派发 + 未实现显式化：后端模块未导出该函数即报 not_implemented，
%% 不允许出现"悄悄返回 ok"。
call(Fun, Args) ->
    Mod = backend_mod(),
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true ->
            erlang:apply(Mod, Fun, Args);
        false ->
            case code:ensure_loaded(Mod) =:= {module, Mod}
                andalso erlang:function_exported(Mod, Fun, length(Args)) of
                true -> erlang:apply(Mod, Fun, Args);
                _ ->
                    {error, {not_implemented, Mod, Fun}}
            end
    end.
