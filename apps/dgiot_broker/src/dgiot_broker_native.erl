%% @doc 自研 broker 后端（dgiot 模式）——刀 3-5 逐步落地。
%%
%% 本模块的每个函数在对应刀完成后才从 not_implemented 变成实现：
%%   start_listener/2  -> 刀 3（TCP 1883 监听 + 会话 + 认证钩子）
%%   publish/3         -> 刀 4（路由 trie + QoS0/1 投递）
%%   subscribe/3       -> 刀 4（含持久会话）           unsubscribe/3 -> 刀 4
%%   routes/1          -> 刀 4（可观测）               sessions/0    -> 刀 3
%%   capabilities/0    -> 刀 1（能力矩阵，先声明目标）
%%   backend_info/0    -> 刀 1（诚实标注"未就绪"）
%%
%% 铁律：未实现的调用一律 {error, {not_implemented, ...}}，
%% 直到对应刀有实机验收证据为止。
-module(dgiot_broker_native).
-behaviour(dgiot_broker_port).

-export([start_listener/2, stop_listener/1,
         publish/3, subscribe/3, unsubscribe/2,
         routes/1, sessions/0,
         capabilities/0, backend_info/0]).

-define(NI(Cut), {error, {not_implemented, dgiot_broker_native, Cut}}).

start_listener(_Name, _Opts) -> ?NI(cut3_tcp_listener).
stop_listener(_Name) -> ?NI(cut3_tcp_listener).
publish(_ClientId, _Topic, _Payload) -> ?NI(cut4_router_delivery).
subscribe(_ClientId, _Filter, _Opts) -> ?NI(cut4_router_delivery).
unsubscribe(_ClientId, _Filter) -> ?NI(cut4_router_delivery).
routes(_Topic) -> ?NI(cut4_router_delivery).
sessions() -> ?NI(cut3_session_store).

%% 目标能力矩阵（对 EMQX 后端逐项对齐；QoS2/will 依据实测使用量为 0 而延后）
capabilities() ->
    #{backend => dgiot,
      status => planned,
      mqtt_versions => [v3_1_1],
      qos => [0, 1],
      retain => pending,          %% 刀 5
      will => pending,            %% 刀 5
      shared_sub => pending,      %% 刀 7
      persistent_session => true, %% 刀 4
      listeners => tcp,
      planned_cuts => #{start_listener => cut3, publish => cut4,
                        subscribe => cut4, retain => cut5, will => cut5,
                        shared_sub => cut7}}.

backend_info() ->
    #{backend => dgiot,
      status => not_ready,
      implemented => [],
      next_cut => cut2_frame_codec,
      note => <<"自研内核未就绪：当前请使用 backend=emqx">>}.
