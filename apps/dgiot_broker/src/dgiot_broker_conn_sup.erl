%% @doc 连接进程监督（刀 3）：simple_one_for_one，每个 socket 一个子进程。
-module(dgiot_broker_conn_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = #{id => dgiot_broker_conn,
              start => {dgiot_broker_conn, start_link, []},
              restart => temporary,      %% 连接进程按需创建，退出不重启
              shutdown => 5000,
              type => worker,
              modules => [dgiot_broker_conn]},
    {ok, {#{strategy => simple_one_for_one, intensity => 100, period => 10},
          [Child]}}.
