%% @doc 同名承接：emqx_metrics（刀 6 批次 2 补齐）。在刀 4 的计数基础上补
%% inc_msg/1、inc_recv/1、set/2 等 EMQX 常用入口。
-module(emqx_metrics).

-export([init/0, ensure/1, create/1, val/1, inc/1, inc/2, dec/1, dec/2,
         set/2, inc_msg/1, inc_recv/1, inc_sent/1,
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

set(Name, Value) when is_integer(Value) ->
    init(),
    ets:insert(?TAB, {Name, Value}),
    ok.

%% EMQX 的消息计数族：按方向归类到我们自己的指标名
inc_msg(Dir) ->
    inc(metric_of_dir(Dir)),
    inc('messages.total').

inc_recv(Dir) -> inc({recv, metric_of_dir(Dir)}).
inc_sent(Dir) -> inc({sent, metric_of_dir(Dir)}).

metric_of_dir(publish) -> 'messages.publish';
metric_of_dir(P) when is_atom(P) -> {messages, P};
metric_of_dir(Other) -> {messages, Other}.

val(Name) ->
    init(),
    case ets:lookup(?TAB, Name) of
        [{Name, V}] -> V;
        [] -> 0
    end.

all() ->
    init(),
    maps:from_list(ets:tab2list(?TAB)).
