%% @doc 同名承接：emqx（影子内核，刀 4 手写实现）。
%%
%% 为什么存在：dgiot 自家应用与 12 个 emqx_* 插件都是**在 VM 内**通过
%% `emqx:publish/1`、`emqx_broker:subscribe/2` 这类同名 API 收发的。
%% 「主程序接管」= 让这些调用落到我们的内核上，而不是换端口。
%%
%% 只在**无 EMQX 模式**下编译（同名模块在 code path 中二义）；
%% 未实现的函数一律显式报错，绝不静默返回 ok。
-module(emqx).

-include("emqx.hrl").

-export([publish/1, subscribe/3, unsubscribe/2,
         hook/2, hook/3, unhook/2,
         get_env/1, get_config/2,
         reboot/0, shutdown/1, ping/0, version/0]).

%% 内部发布：委托 emqx_broker（单一实现路径，钩子与指标只做一次）
publish(#message{} = Message) ->
    emqx_broker:publish(Message);
publish(Other) ->
    {error, {bad_message, Other}}.

%% 内部订阅：进程 Pid 订阅 Filter（VM 内按记录形态投递）
subscribe(Pid, Filter, Qos) when is_pid(Pid) ->
    dgiot_broker_router:subscribe(Filter, client_id_of(Pid), Pid, Qos,
                                  #{shape => record});
subscribe(Other, _F, _Q) ->
    {error, {bad_subscriber, Other}}.

unsubscribe(Pid, Filter) when is_pid(Pid) ->
    dgiot_broker_router:unsubscribe(Filter, client_id_of(Pid));
unsubscribe(_Other, _Filter) ->
    ok.

%% 钩子面（dgiot 实测只用五个钩子点）
hook(HookPoint, Callback) -> emqx_hooks:add(HookPoint, Callback).
hook(HookPoint, Callback, Priority) ->
    emqx_hooks:add(HookPoint, Callback, Priority).
unhook(HookPoint, Callback) -> emqx_hooks:del(HookPoint, Callback).

%% 进程生命周期控制属主机管理面，刀 7 之前显式报错
reboot() ->
    {error, {not_implemented, cut7, emqx, reboot}}.
shutdown(Reason) ->
    {error, {not_implemented, cut7, {emqx, shutdown}, Reason}}.

ping() -> pong.

%% 配置读取：EMQX 的 env 都挂在 emqx 应用下；这里映射到 dgiot_broker 的 env，
%% 取不到再回落 kernel 环境，最终给默认值（不抛，避免插件启动崩）。
get_env(Key) ->
    case application:get_env(dgiot_broker, Key) of
        {ok, V} -> V;
        undefined ->
            case application:get_env(emqx, Key) of
                {ok, V2} -> V2;
                undefined -> undefined
            end
    end.

get_config(Key, Default) ->
    case get_env(Key) of
        undefined -> Default;
        V -> V
    end.

version() ->
    case application:get_key(dgiot_broker, vsn) of
        {ok, V} -> V;
        _ -> <<"dgiot_broker">>
    end.

client_id_of(Pid) ->
    iolist_to_binary(io_lib:format("inproc-~p", [Pid])).
