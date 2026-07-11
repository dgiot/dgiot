%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_metrics_adapter).
-author("dgiot").

-export([val/1, inc/2, ensure/1, set/2]).

-define(TAB, dgiot_metrics_tab).

init() ->
    _ = ets:new(?TAB, [named_table, public, set, {write_concurrency, true}]),
    ok.

val(Key) ->
    case ets:lookup(?TAB, Key) of
        [{Key, V}] -> V;
        [] -> 0
    end.

inc(Key, Val) ->
    try ets:update_counter(?TAB, Key, Val, {Key, 0})
    catch _:_ -> ok
    end.

ensure(Key) ->
    case ets:member(?TAB, Key) of
        false -> ets:insert(?TAB, {Key, 0});
        true -> ok
    end.

set(Key, Val) ->
    ets:insert(?TAB, {Key, Val}),
    ok.
