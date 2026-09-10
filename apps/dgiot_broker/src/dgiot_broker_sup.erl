%% @doc dgiot_broker 监督树。
%%
%% 刀 1 为空壳；后续按刀挂载：
%%   刀 2 编解码（无进程，纯函数模块）
%%   刀 3 dgiot_broker_listener_sup + 会话监督
%%   刀 4 路由表 ETS owner
%%   刀 5 retainer
-module(dgiot_broker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, []}}.
