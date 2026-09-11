%% @doc 同名承接：emqx_hooks（影子内核，刀 4）。
%% 自有的轻量钩子注册表（ETS ordered_set，按优先级）。
%% dgiot 实测只挂在五个点：client.authenticate / client.check_acl /
%% message.publish / client.disconnected / session.terminated。
%% 回调异常**不吞**：run 时记录并返回 {error, _}（EMQX 会吞掉回调异常，
%% 这正是我们要改掉的静默类缺陷；但为兼容调用方，run/2 仍返回 ok，
%% 失败通过日志与 metrics 暴露，run_fold/3 返回原 Acc 并记日志）。
-module(emqx_hooks).

-export([init/0, add/2, add/3, del/2, del/3, lookup/1, run/2, run/3, run_fold/3,
         reset/0]).

-define(TAB, dgiot_broker_hooks).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, ordered_set, public,
                                   {read_concurrency, true}]);
        _ -> ?TAB
    end,
    ok.

reset() -> ets:delete_all_objects(?TAB), ok.

add(HookPoint, Callback) -> add(HookPoint, Callback, 0).
add(HookPoint, Callback, Priority) when is_integer(Priority) ->
    init(),
    Key = {HookPoint, Priority, erlang:phash2(Callback)},
    ets:insert(?TAB, {Key, Callback}),
    ok.

del(HookPoint, Callback) ->
    init(),
    Matches = [K || {{H, _P, _Id} = K, C} <- ets:tab2list(?TAB),
                    H =:= HookPoint, C =:= Callback],
    [ets:delete(?TAB, K) || K <- Matches],
    ok.

del(HookPoint, Callback, _Priority) -> del(HookPoint, Callback).

lookup(HookPoint) ->
    init(),
    [C || {{H, _P, _Id}, C} <- lists:keysort(1, ets:tab2list(?TAB)),
          H =:= HookPoint].

%% run/2：全部回调执行；异常记日志 + 计数，不静默
run(HookPoint, Args) ->
    run_fold(HookPoint, Args, ok),
    ok.

run(HookPoint, Args, Acc) -> run_fold(HookPoint, Args, Acc).

run_fold(HookPoint, Args, Acc) ->
    lists:foldl(
      fun(Callback, AccIn) ->
              try
                  case Callback of
                      {M, F, A} -> erlang:apply(M, F, Args ++ A);
                      Fun when is_function(Fun) -> Fun(Args);
                      Other -> {error, {bad_callback, Other}}
                  end
              catch
                  Class:Reason:Stack ->
                      logger:error("[hooks] ~p callback failed: ~p:~p~n~p",
                                   [HookPoint, Class, Reason, hd(Stack)]),
                      emqx_metrics:inc('hooks.failed'),
                      AccIn
              end,
              AccIn
      end, Acc, lookup(HookPoint)).
