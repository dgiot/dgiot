%% @doc 同名承接：emqx_pd（进程字典封装）→ erlang:get/put 薄封装。
%% 关闭 get/put/erase 的 auto-import，避免与本地同名函数冲突。
-module(emqx_pd).

-compile({no_auto_import, [get/1, get/2, put/2, erase/1]}).

-export([get/1, get/2, put/2, erase/1, get_counter/1, inc_counter/1,
         inc_counter/2, reset_counter/1, get_counters/1]).

get(Key) -> erlang:get(Key).

get(Key, Default) ->
    case erlang:get(Key) of
        undefined -> Default;
        V -> V
    end.

put(Key, Val) -> erlang:put(Key, Val), ok.

erase(Key) -> erlang:erase(Key), ok.

get_counter(Key) -> get(Key, 0).

inc_counter(Key) -> inc_counter(Key, 1).

inc_counter(Key, N) ->
    V = get(Key, 0) + N,
    put(Key, V),
    V.

reset_counter(Key) -> put(Key, 0), 0.

get_counters(Keys) when is_list(Keys) ->
    [{Key, get_counter(Key)} || Key <- Keys].
