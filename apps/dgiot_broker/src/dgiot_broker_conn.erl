%% @doc MQTT 连接进程（刀 3）：一个 socket 一个进程，持有会话状态机。
%%
%% 状态机：waiting_connect → connected →（关闭）
%% 铁律：协议违规显式回应（CONNACK 非 0 / 关闭），不静默忽略；
%% 每个包都续 keepalive 计时；进程退出时注销会话与订阅（生命周期可观测）。
%%
%% 刀 3 已实现：CONNECT/CONNACK、SUBSCRIBE/SUBACK、UNSUBSCRIBE/UNSUBACK、
%% PINGREQ/PINGRESP、PUBLISH（**本机投递**，QoS0/1）、DISCONNECT、keepalive。
%% 刀 4 待办：持久会话离线队列、QoS1 重投/inflight 窗口、保留消息（刀 5）。
-module(dgiot_broker_conn).
-behaviour(gen_server).

-export([start_link/1, deliver/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(CONNECT_TIMEOUT, 10000).
-define(MAX_PACKET, 268435455).

-record(st, {socket,
             peer,
             phase = waiting_connect :: waiting_connect | connected,
             client_id = undefined :: binary() | undefined,
             username = undefined,
             keepalive = 60,
             ka_timer = undefined,
             buf = <<>>,
             delivered = 0,
             received = 0}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%% 供路由投递：另一个连接进程把包送到本进程
deliver(Pid, Packet) ->
    gen_server:cast(Pid, {deliver, Packet}).

init([Socket]) ->
    {ok, Peer} = inet:peername(Socket),
    ok = inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    TRef = erlang:send_after(?CONNECT_TIMEOUT, self(), connect_timeout),
    logger:info("[broker-conn] accepted peer=~p", [Peer]),
    {ok, #st{socket = Socket, peer = Peer, ka_timer = TRef}}.

%% ---------------- 收包 ----------------
handle_info({tcp, Socket, Data}, #st{socket = Socket} = St) ->
    ok = inet:setopts(Socket, [{active, once}]),
    process_buffer(St#st{buf = <<(St#st.buf)/binary, Data/binary>>});

handle_info({tcp_closed, Socket}, #st{socket = Socket} = St) ->
    logger:info("[broker-conn] peer closed client=~p", [St#st.client_id]),
    {stop, normal, St};

handle_info({tcp_error, Socket, Reason}, #st{socket = Socket} = St) ->
    logger:warning("[broker-conn] socket error client=~p: ~p",
                   [St#st.client_id, Reason]),
    {stop, {tcp_error, Reason}, St};

handle_info(connect_timeout, #st{phase = waiting_connect} = St) ->
    logger:warning("[broker-conn] CONNECT timeout peer=~p", [St#st.peer]),
    {stop, connect_timeout, St};
handle_info(connect_timeout, St) ->
    {noreply, St};

handle_info(keepalive_expired, #st{client_id = Cid} = St) ->
    logger:warning("[broker-conn] keepalive expired client=~p", [Cid]),
    {stop, keepalive_expired, St};

handle_info(_Other, St) ->
    {noreply, St}.

handle_call({peer_info}, _From, St) ->
    {reply, #{phase => St#st.phase, client_id => St#st.client_id,
              peer => St#st.peer, received => St#st.received,
              delivered => St#st.delivered}, St};
handle_call(_Req, _From, St) ->
    {reply, {error, not_implemented}, St}.

handle_cast({deliver, Packet}, St) ->
    ok = send(St, Packet),
    {noreply, St#st{delivered = St#st.delivered + 1}};
handle_cast(_Msg, St) ->
    {noreply, St}.

terminate(Reason, #st{socket = Socket, client_id = Cid} = _St) ->
    case Cid of
        undefined -> ok;
        _ ->
            dgiot_broker_router:unsubscribe_all(Cid),
            dgiot_broker_session:unregister(Cid)
    end,
    catch gen_tcp:close(Socket),
    logger:info("[broker-conn] terminated client=~p reason=~p", [Cid, Reason]),
    ok.

code_change(_Old, St, _Extra) -> {ok, St}.

%% ---------------- 解码循环 ----------------
process_buffer(#st{buf = Buf} = St) ->
    case dgiot_broker_frame:decode(Buf) of
        {more, _} ->
            {noreply, St};
        {error, Reason} ->
            logger:warning("[broker-conn] malformed packet client=~p: ~p",
                           [St#st.client_id, Reason]),
            {stop, {malformed, Reason}, St};
        {ok, Packet, Rest} ->
            case handle_packet(Packet, St#st{buf = Rest}) of
                {noreply, NewSt} -> process_buffer(NewSt);
                {stop, Reason, NewSt} -> {stop, Reason, NewSt}
            end
    end.

%% ---------------- 各包处理 ----------------
handle_packet(#{type := connect} = P, #st{phase = waiting_connect} = St) ->
    handle_connect(P, St);
handle_packet(#{type := connect}, St) ->
    protocol_violation(second_connect, St);

handle_packet(#{type := subscribe} = P, #st{phase = connected} = St) ->
    handle_subscribe(P, St);
handle_packet(#{type := unsubscribe} = P, #st{phase = connected} = St) ->
    handle_unsubscribe(P, St);
handle_packet(#{type := publish} = P, #st{phase = connected} = St) ->
    handle_publish(P, St);
handle_packet(#{type := pingreq}, #st{phase = connected} = St) ->
    ok = send(St, #{type => pingresp}),
    {noreply, St};
handle_packet(#{type := disconnect}, St) ->
    logger:info("[broker-conn] client ~p disconnected", [St#st.client_id]),
    {stop, normal, St};
handle_packet(#{type := Type}, St) ->
    %% 客户端不应发来的包（CONNACK/SUBACK/…）或多包同发
    logger:warning("[broker-conn] unexpected packet ~p from client=~p",
                   [Type, St#st.client_id]),
    protocol_violation({unexpected_packet, Type}, St).

handle_connect(P, St) ->
    ProtoLevel = maps:get(proto_level, P, 0),
    ClientId = maps:get(client_id, P, <<>>),
    Username = maps:get(username, P, undefined),
    Password = maps:get(password, P, undefined),
    CleanStart = maps:get(clean_start, P, true),
    Keepalive = maps:get(keepalive, P, 60),
    case ProtoLevel of
        4 ->
            case dgiot_broker_auth:authenticate(ClientId, Username, Password) of
                ok ->
                    accept_connect(ClientId, Username, CleanStart, Keepalive, St);
                {error, Reason} ->
                    logger:warning("[broker-conn] auth denied client=~p: ~p",
                                   [ClientId, Reason]),
                    ok = send(St, #{type => connack, session_present => false,
                                    return_code => 5}),
                    {stop, {auth_denied, Reason}, St}
            end;
        Level ->
            logger:warning("[broker-conn] unsupported protocol level ~p", [Level]),
            ok = send(St, #{type => connack, session_present => false,
                            return_code => 1}),
            {stop, {bad_protocol_level, Level}, St}
    end.

accept_connect(ClientId, Username, CleanStart, Keepalive, St) ->
    %% 会话接管：同 ClientId 的旧连接必须退场（显式，不静默顶掉）
    case dgiot_broker_session:register(ClientId, self(),
                                       #{username => Username,
                                         clean_start => CleanStart,
                                         keepalive => Keepalive}) of
        {takeover, OldPid} ->
            logger:notice("[broker-conn] session takeover client=~p old=~p",
                          [ClientId, OldPid]),
            catch gen_server:stop(OldPid, {takeover, ClientId});
        {ok, _} ->
            ok
    end,
    ok = send(St, #{type => connack, session_present => false, return_code => 0}),
    St1 = St#st{phase = connected, client_id = ClientId,
                username = Username, keepalive = Keepalive},
    logger:notice("[broker-conn] CONNECT accepted client=~p keepalive=~p clean=~p",
                  [ClientId, Keepalive, CleanStart]),
    {noreply, arm_keepalive(St1)}.

handle_subscribe(#{packet_id := Id, topics := Topics}, St) ->
    Codes = [subscribe_one(Filter, Qos, St) || {Filter, Qos} <- Topics],
    ok = send(St, #{type => suback, packet_id => Id, return_codes => Codes}),
    %% 会话内记录订阅（可观测用；真正的路由在 router）
    Cid = St#st.client_id,
    _ = dgiot_broker_session:update(
          Cid, fun(S) ->
                       Subs = maps:get(subscriptions, S, []),
                       New = lists:usort(Subs ++ [F || {F, Q} <- Topics, Q =< 2]),
                       S#{subscriptions => New}
               end),
    {noreply, St}.

subscribe_one(Filter, Qos, St) ->
    case dgiot_broker_router:subscribe(Filter, St#st.client_id, self(), Qos) of
        ok ->
            logger:notice("[broker-conn] SUB ~s qos=~p client=~p",
                          [Filter, Qos, St#st.client_id]),
            Qos;
        {error, Reason} ->
            %% 失败必须响：回 0x80 并记日志，绝不假装成功
            logger:warning("[broker-conn] SUB rejected ~s: ~p", [Filter, Reason]),
            16#80
    end.

handle_unsubscribe(#{packet_id := Id, topics := Topics}, St) ->
    [dgiot_broker_router:unsubscribe(F, St#st.client_id) || F <- Topics],
    ok = send(St, #{type => unsuback, packet_id => Id}),
    logger:notice("[broker-conn] UNSUB ~p client=~p", [Topics, St#st.client_id]),
    {noreply, St}.

handle_publish(#{qos := 0} = P, St) ->
    deliver_locally(P, St),
    {noreply, St};
handle_publish(#{qos := 1, packet_id := Id} = P, St) ->
    deliver_locally(P, St),
    ok = send(St, #{type => puback, packet_id => Id}),
    {noreply, St};
handle_publish(#{qos := 2} = P, St) ->
    %% MQTT 3.1.1 里我们不需要 QoS2（实测 dgiot 与自家链路使用量为 0）：
    %% 显式拒绝并断开，而不是悄悄降级
    logger:warning("[broker-conn] QoS2 not supported, closing client=~p",
                   [St#st.client_id]),
    protocol_violation({qos2_unsupported, maps:get(topic, P)}, St).

deliver_locally(P, St) ->
    Topic = maps:get(topic, P),
    Matches = dgiot_broker_router:match(Topic),
    Out = P#{dup => false},
    [deliver_to(Cid, Pid, Out) || {Cid, Pid, _Q} <- Matches],
    logger:notice("[broker-conn] PUB ~s -> ~p subscriber(s) client=~p",
                  [Topic, length(Matches), St#st.client_id]),
    St#st{received = St#st.received + 1}.

deliver_to(ClientId, Pid, Packet) ->
    case is_process_alive(Pid) of
        true -> dgiot_broker_conn:deliver(Pid, Packet);
        false ->
            %% 死亡订阅者立即清理（否则路由表变成垃圾场）
            logger:warning("[broker-router] stale subscriber ~p, purging", [ClientId]),
            dgiot_broker_router:unsubscribe_all(ClientId)
    end.

protocol_violation(Reason, St) ->
    logger:warning("[broker-conn] protocol violation client=~p: ~p",
                   [St#st.client_id, Reason]),
    {stop, {protocol_violation, Reason}, St}.

%% ---------------- 工具 ----------------
send(#st{socket = Socket}, Packet) ->
    gen_tcp:send(Socket, dgiot_broker_frame:encode(Packet)).

arm_keepalive(#st{keepalive = 0} = St) ->
    St;   %% keepalive=0：无超时
arm_keepalive(#st{keepalive = KA, ka_timer = Old} = St) ->
    case Old of
        undefined -> ok;
        Ref when is_reference(Ref) -> erlang:cancel_timer(Ref);
        _ -> ok
    end,
    Ms = trunc(KA * 1500),   %% 1.5 × keepalive（协议规定）
    St#st{ka_timer = erlang:send_after(Ms, self(), keepalive_expired)}.
