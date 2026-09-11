%% @doc TCP 监听（刀 3）：acceptor 池 + 连接监督。
%%
%% 开发期刻意用独立端口（默认 1884），与在位的 EMQX（1883）并行——
%% 现网零扰动；切换刀再把 1883 交过来。配置：
%%   {listeners, #{tcp => #{enabled => true, port => 1884, acceptors => 4,
%%                          max_conns => 1024}}}
%%
%% 铁律：bind 失败必须让监督树看到（不吞）；acceptor 崩溃自动补位并记日志。
-module(dgiot_broker_listener).
-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0, info/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(st, {listen, port, acceptors = [], conns = 0, max_conns = 1024}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Opts) when is_map(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _ -> gen_server:stop(?SERVER)
    end.

info() ->
    case whereis(?SERVER) of
        undefined -> {error, not_running};
        _ -> gen_server:call(?SERVER, info)
    end.

init(Args) ->
    Opts = case Args of
               [] -> tcp_opts();
               [O] -> O
           end,
    Port = maps:get(port, Opts, 1884),
    Acceptors = maps:get(acceptors, Opts, 4),
    MaxConns = maps:get(max_conns, Opts, 1024),
    case gen_tcp:listen(Port, [binary, {packet, raw}, {active, false},
                               {reuseaddr, true}, {backlog, 256},
                               {nodelay, true}]) of
        {ok, Listen} ->
            logger:notice("[broker-listener] listening on :~p (~p acceptors)",
                          [Port, Acceptors]),
            Pids = [spawn_acceptor(Listen, N) || N <- lists:seq(1, Acceptors)],
            {ok, #st{listen = Listen, port = Port, acceptors = Pids,
                     max_conns = MaxConns}};
        {error, Reason} ->
            %% bind 失败必须让监督树看到
            logger:error("[broker-listener] bind ~p failed: ~p", [Port, Reason]),
            {stop, {listen_failed, Port, Reason}}
    end.

tcp_opts() ->
    case application:get_env(dgiot_broker, listeners) of
        {ok, #{tcp := Tcp}} -> Tcp;
        _ -> #{enabled => false, port => 1884}
    end.

handle_call(info, _From, #st{} = St) ->
    {reply, #{port => St#st.port,
              acceptors => length([P || P <- St#st.acceptors,
                                        is_process_alive(P)]),
              conns => St#st.conns,
              max_conns => St#st.max_conns,
              sessions => dgiot_broker_session:count(),
              subscriptions => dgiot_broker_router:count()}, St};
handle_call(_R, _F, St) ->
    {reply, {error, not_implemented}, St}.

handle_cast(_M, St) -> {noreply, St}.

%% acceptor 崩溃/退出 → 补位（可观测：记日志而不是假装无事）
handle_info({'EXIT', Pid, Reason}, #st{listen = Listen, acceptors = As} = St) ->
    case lists:member(Pid, As) of
        true ->
            logger:warning("[broker-listener] acceptor ~p exited: ~p — respawning",
                           [Pid, Reason]),
            New = spawn_acceptor(Listen, 0),
            {noreply, St#st{acceptors = [New | lists:delete(Pid, As)]}};
        false ->
            {noreply, St}
    end;
handle_info({conn_accepted, _Pid}, St) ->
    {noreply, St#st{conns = St#st.conns + 1}};
handle_info(_Other, St) ->
    {noreply, St}.

terminate(_Reason, #st{listen = Listen}) ->
    catch gen_tcp:close(Listen),
    ok.

code_change(_Old, St, _Extra) -> {ok, St}.

%% ---------------- acceptor ----------------
spawn_acceptor(Listen, N) ->
    Parent = self(),
    spawn_link(fun() -> accept_loop(Listen, Parent, N) end).

accept_loop(Listen, Parent, N) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Parent ! {conn_accepted, self()},
            case start_conn(Socket) of
                {ok, Pid} ->
                    %% 控制权交给连接进程（socket 归它所有）
                    ok = gen_tcp:controlling_process(Socket, Pid),
                    Pid ! socket_ready,
                    N1 = N + 1;
                {error, Reason} ->
                    logger:error("[broker-listener] conn start failed: ~p", [Reason]),
                    catch gen_tcp:close(Socket),
                    N1 = N
            end,
            accept_loop(Listen, Parent, N1);
        {error, closed} ->
            logger:notice("[broker-listener] acceptor stopping (listen closed)"),
            ok;
        {error, Reason} ->
            logger:error("[broker-listener] accept error: ~p", [Reason]),
            ok
    end.

start_conn(Socket) ->
    %% 连接进程挂在 broker 监督树下（dgiot_broker_conn_sup）
    case whereis(dgiot_broker_conn_sup) of
        undefined -> dgiot_broker_conn:start_link(Socket);
        _ -> supervisor:start_child(dgiot_broker_conn_sup, [Socket])
    end.
