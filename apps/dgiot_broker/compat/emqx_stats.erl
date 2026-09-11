%% @doc 同名承接：emqx_stats（统计聚合）→ 汇总我们的指标/VM/会话。
-module(emqx_stats).

-export([getstats/0, statsfun/2, get_stat/1]).

getstats() ->
    #{version => emqx_sys:version(),
      uptime => emqx_sys:uptime(),
      sysdescr => emqx_sys:sysdescr(),
      sessions => dgiot_broker_session:count(),
      subscriptions => dgiot_broker_router:count(),
      processes => erlang:system_info(process_count),
      metrics => emqx_metrics:all(),
      vm => emqx_vm:mem_info()}.

%% EMQX 的 statsfun/2 返回给定统计函数的结果
statsfun(all, _Args) -> getstats();
statsfun(metrics, _Args) -> emqx_metrics:all();
statsfun(sessions, _Args) -> dgiot_broker_session:count();
statsfun(Other, _Args) -> {error, {unknown_stat, Other}}.

get_stat(Key) -> maps:get(Key, getstats(), undefined).
