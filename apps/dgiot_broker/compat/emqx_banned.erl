%% @doc 同名承接：emqx_banned（封禁列表）→ ETS 简单实现。
-module(emqx_banned).

-export([add/1, delete/1, check/1, banned/0, create/1, info/1]).

-define(TAB, dgiot_broker_banned).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public]);
        _ -> ?TAB
    end,
    ok.

create(Who) -> #{who => Who, at => erlang:system_time(second)}.

add(Ban) when is_map(Ban) ->
    init(),
    ets:insert(?TAB, {maps:get(who, Ban), Ban}),
    ok.

delete(Who) ->
    init(),
    ets:delete(?TAB, Who),
    ok.

check(Who) ->
    init(),
    case ets:lookup(?TAB, Who) of
        [{Who, Ban}] -> {true, Ban};
        [] -> false
    end.

banned() ->
    init(),
    [B || {_W, B} <- ets:tab2list(?TAB)].

info(Who) -> check(Who).
