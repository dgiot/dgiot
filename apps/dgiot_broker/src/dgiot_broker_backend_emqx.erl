%% @doc EMQX 后端适配器：把端口调用转到现役 EMQX。
%%
%% 存在意义：证明端口形状可用、并让 dgiot 新代码从今天起就走端口，
%% 而不是继续加深对 emqx_* 的直接依赖。刀 6 之后本模块随 EMQX 一起退场。
-module(dgiot_broker_backend_emqx).
-behaviour(dgiot_broker_port).

-export([start_listener/2, stop_listener/1,
         publish/3, subscribe/3, unsubscribe/2,
         routes/1, sessions/0,
         capabilities/0, backend_info/0]).

start_listener(_Name, _Opts) ->
    %% EMQX 的监听器由 emqx 自身配置管理，端口层不重复实现
    {error, {managed_by, emqx, listener}}.

stop_listener(_Name) ->
    {error, {managed_by, emqx, listener}}.

%% 走 dgiot 自己的发布入口（等价于 emqx:publish），保持与现网一致
publish(ClientId, Topic, Payload) ->
    dgiot_mqtt:publish(ClientId, Topic, Payload).

subscribe(_ClientId, _Filter, _Opts) ->
    {error, {not_implemented, emqx_backend, subscribe}}.

unsubscribe(_ClientId, _Filter) ->
    {error, {not_implemented, emqx_backend, unsubscribe}}.

routes(Topic) ->
    emqx_router:match_routes(Topic).

sessions() ->
    %% EMQX 4.x 的连接列表来自 management 面；此处显式标注来源
    case erlang:function_exported(emqx_mgmt, lookup_connections, 1) of
        true -> emqx_mgmt:lookup_connections(all);
        false -> {error, {not_implemented, emqx_backend, sessions}}
    end.

capabilities() ->
    #{backend => emqx,
      mqtt_versions => [v3_1_1, v5],
      qos => [0, 1, 2],
      retain => true,
      will => true,
      shared_sub => true,
      persistent_session => true,
      listeners => tcp}.

backend_info() ->
    #{backend => emqx,
      app => emqx,
      vsn => case application:get_key(emqx, vsn) of
                 {ok, V} -> V;
                 _ -> unknown
             end,
      note => <<"EMQX 在位模式：监听器/集群/管理面由 EMQX 提供">>}.
