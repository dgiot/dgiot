%% @doc 同名承接：emqx_metrics（影子内核，刀 4）。
%% 计数器放 ETS；`ensure/1` 先建后加（EMQX 的语义是"没有就建"）。
%% 未实现的函数（如 create/1 的高级变体）显式报错。
-module(emqx_metrics).

-export([init/0, ensure/1, create/1, val/1, inc/1, inc/2, dec/1, dec/2,
         reset/0, all/0]).

-define(TAB, dgiot_broker_metrics).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public,
                                   {write_concurrency, true}]);
        _ -> ?TAB
    end,
    ok.

reset() -> init(), ets:delete_all_objects(?TAB), ok.

ensure(Name) ->
    init(),
    case ets:lookup(?TAB, Name) of
        [] -> ets:insert(?TAB, {Name, 0});
        _ -> ok
    end,
    ok.

create(Name) -> ensure(Name).

inc(Name) -> inc(Name, 1).

inc(Name, N) when is_integer(N) ->
    ensure(Name),
    ets:update_counter(?TAB, Name, N),
    ok.

dec(Name) -> dec(Name, 1).

dec(Name, N) when is_integer(N) -> inc(Name, -N).

val(Name) ->
    init(),
    case ets:lookup(?TAB, Name) of
        [{Name, V}] -> V;
        [] -> 0
    end.

all() ->
    init(),
    maps:from_list(ets:tab2list(?TAB)).
