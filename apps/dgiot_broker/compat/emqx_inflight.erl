%% @doc 同名承接：emqx_inflight（QoS1/2 在途窗口）→ ETS 简单实现。
%% 用于避免消息重放；完整重投语义属刀 4，这里提供可用的窗口 API。
-module(emqx_inflight).

-export([new/0, is_full/2, insert/2, update/2, lookup/2, delete/2,
         size/1, is_empty/1, foreach/2, reset/1, to_list/1]).

-define(TAB, dgiot_broker_inflight).

new() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public]);
        _ -> ?TAB
    end,
    ?TAB.

is_full(_Tab, Max) -> ets:info(?TAB, size) >= Max.

insert(_Tab, {Key, Val}) -> ets:insert(?TAB, {Key, Val}), ok.
update(_Tab, {Key, Val}) -> insert(?TAB, {Key, Val}).

lookup(_Tab, Key) ->
    case ets:lookup(?TAB, Key) of
        [{Key, Val}] -> {ok, Val};
        [] -> undefined
    end.

delete(_Tab, Key) -> ets:delete(?TAB, Key), ok.

size(_Tab) -> ets:info(?TAB, size).
is_empty(_Tab) -> ets:info(?TAB, size) =:= 0.

foreach(_Tab, Fun) -> lists:foreach(Fun, ets:tab2list(?TAB)), ok.
reset(_Tab) -> ets:delete_all_objects(?TAB), ok.

%% @doc 在途窗口 → {Key, Val} 列表（QoS 重放观测用）。
%% 注：当前 inflight 是单共享 ETS（连接级隔离属后续刀），to_list 返回全部。
to_list(_Inflight) -> ets:tab2list(?TAB).
