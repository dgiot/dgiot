%% @doc dgiot_broker 监督树（刀 3）。
%%
%% 启动顺序与归属：
%%   * ETS 表（路由簿/会话登记）由**本监督进程**创建并持有 —— 表随树存活；
%%   * dgiot_broker_conn_sup：每 socket 一连接进程；
%%   * dgiot_broker_listener：acceptor 池（仅当 listeners.tcp.enabled=true
%%     且 backend=dgiot 时启动；开发期跑 1884，与在位 EMQX 并行）。
-module(dgiot_broker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% 表由监督进程持有：树在表在（生命周期可观测）
    ok = dgiot_broker_router:init(),
    ok = dgiot_broker_session:init(),
    Backend = dgiot_broker:backend(),
    Children =
        [#{id => conn_sup,
           start => {dgiot_broker_conn_sup, start_link, []},
           restart => permanent, shutdown => infinity, type => supervisor,
           modules => [dgiot_broker_conn_sup]}]
        ++ listener_child(Backend),
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

listener_child(dgiot) ->
    [#{id => listener,
       start => {dgiot_broker_listener, start_link, []},
       restart => permanent, shutdown => 5000, type => worker,
       modules => [dgiot_broker_listener]}];
listener_child(emqx) ->
    %% 现网模式：监听在 EMQX 手里，本 app 不抢端口
    [].
