%% @doc 自研 broker 后端（dgiot 模式）——刀 3 已接通数据面主干。
%%
%% 已实现（有实机验收）：
%%   start_listener/2 stop_listener/1  刀 3：TCP + acceptor 池（开发期 1884）
%%   publish/3                          刀 3：服务端发布 → 本机路由投递
%%                                     （这正是 EMQX 侧静默丢弃的那条路径）
%%   routes/1 sessions/0                刀 3：可观测
%%   capabilities/0 backend_info/0      刀 3：能力矩阵如实标注
%% 仍待办（显式 not_implemented，绝不假装）：
%%   subscribe/3 unsubscribe/2          服务端代订阅（客户端订阅走连接进程）
%% 刀 4：持久会话离线队列 / QoS1 重投与 inflight；刀 5：retain/will；
%% 刀 7：shared_sub/$SYS。
-module(dgiot_broker_native).
-behaviour(dgiot_broker_port).

-export([start_listener/2, stop_listener/1,
         publish/3, subscribe/3, unsubscribe/2,
         routes/1, sessions/0,
         capabilities/0, backend_info/0]).

%% ---------------- 监听 ----------------
start_listener(tcp, Opts) when is_map(Opts) ->
    case whereis(dgiot_broker_listener) of
        undefined ->
            case dgiot_broker_listener:start_link(Opts) of
                {ok, Pid} -> {ok, Pid};
                {error, Reason} -> {error, Reason}
            end;
        Pid ->
            {error, {already_started, Pid}}
    end;
start_listener(Name, _Opts) ->
    {error, {unsupported_listener, Name}}.

stop_listener(tcp) ->
    dgiot_broker_listener:stop();
stop_listener(Name) ->
    {error, {unsupported_listener, Name}}.

%% ---------------- 数据面 ----------------
%% 服务端发布：路由簿命中 → 逐个投递给订阅者连接进程（QoS0 语义；QoS 降级由
%% 订阅端协商，这里不静默丢弃）。返回投递数量，便于上层断言。
-spec publish(binary(), binary(), term()) -> {ok, non_neg_integer()} | {error, term()}.
publish(ClientId, Topic, Payload) ->
    Bin = case Payload of
              B when is_binary(B) -> B;
              L when is_list(L) -> iolist_to_binary(L);
              M when is_map(M) -> dgiot_json:encode(M);
              Other -> iolist_to_binary(io_lib:format("~p", [Other]))
          end,
    Packet = #{type => publish, qos => 0, retain => false, dup => false,
               topic => Topic, payload => Bin, from => ClientId},
    Matches = dgiot_broker_router:match_full(Topic),
    Delivered =
        lists:foldl(
          fun({Cid, Pid, _Q, Meta}, Acc) ->
                  case is_process_alive(Pid) of
                      true ->
                          deliver_to(Pid, Packet, Meta),
                          Acc + 1;
                      false ->
                          logger:warning("[broker-native] stale subscriber ~p purged", [Cid]),
                          dgiot_broker_router:unsubscribe_all(Cid),
                          Acc
                  end
          end, 0, Matches),
    logger:notice("[broker-native] publish ~s from ~p -> ~p/~p delivered",
                  [Topic, ClientId, Delivered, length(Matches)]),
    {ok, Delivered}.

%% 投递形态：wire（外部客户端，线上 map）/ record（VM 内订阅者，#message{}）。
%% 记录桥（dgiot_broker_record_bridge）只在无 EMQX 模式编译，故用运行时探测：
%% 在位 EMQX 时它不存在，走 wire 路径，互不干扰。
deliver_to(Pid, Packet, #{shape := record}) ->
    Bridge = dgiot_broker_record_bridge,
    case erlang:function_exported(Bridge, deliver, 2) orelse
         code:ensure_loaded(Bridge) =:= {module, Bridge} of
        true -> Bridge:deliver(Pid, Packet);
        false -> dgiot_broker_conn:deliver(Pid, Packet)
    end;
deliver_to(Pid, Packet, _Meta) ->
    dgiot_broker_conn:deliver(Pid, Packet).

subscribe(_ClientId, _Filter, _Opts) ->
    {error, {not_implemented, cut4, server_side_subscribe}}.

unsubscribe(_ClientId, _Filter) ->
    {error, {not_implemented, cut4, server_side_unsubscribe}}.

%% ---------------- 观测面 ----------------
routes(Topic) when is_binary(Topic) ->
    dgiot_broker_router:match(Topic);
routes(_) ->
    {error, {bad_topic, need_binary}}.

sessions() ->
    dgiot_broker_session:list().

capabilities() ->
    #{backend => dgiot,
      status => partial,
      mqtt_versions => [v3_1_1],
      qos => [0, 1],                 %% QoS1 已收/PUBACK；重投待刀 4
      retain => pending,             %% 刀 5
      will => pending,               %% 刀 5
      shared_sub => pending,         %% 刀 7
      persistent_session => partial, %% 内存会话已通；离线队列待刀 4
      listeners => tcp,              %% 开发期 1884
      implemented => [connect_auth, suback, unsuback, pingreq,
                      publish_local_delivery, server_side_publish,
                      session_takeover, keepalive, sessions, routes],
      pending => #{cut4 => [qos1_retransmit, offline_queue, inflight_window],
                   cut5 => [retain, will],
                   cut7 => [shared_sub, sys_topics]}}.

backend_info() ->
    #{backend => dgiot,
      status => partial,
      listener => case whereis(dgiot_broker_listener) of
                      undefined -> not_running;
                      _ -> dgiot_broker_listener:info()
                  end,
      sessions => dgiot_broker_session:count(),
      subscriptions => dgiot_broker_router:count(),
      note => <<"自研内核：TCP/会话/认证/订阅/本机投递已通；retain/will/shared_sub 待后续刀">>}.
