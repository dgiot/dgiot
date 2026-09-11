%% @doc 同名承接：emqx_trace_handler（trace 安装/卸载）→ 轻量 trace 登记。
-module(emqx_trace_handler).

-export([install/3, install/4, uninstall/1, uninstall/2, running/0,
         running/1, list/0]).

-define(TAB, dgiot_broker_traces).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public]);
        _ -> ?TAB
    end,
    ok.

install(Id, Type, Spec) -> install(Id, Type, Spec, #{}).

install(Id, Type, Spec, _Opts) ->
    init(),
    ets:insert(?TAB, {Id, #{type => Type, spec => Spec,
                            started_at => erlang:system_time(second)}}),
    ok.

uninstall(Id) ->
    init(),
    ets:delete(?TAB, Id),
    ok.

uninstall(Id, _Type) -> uninstall(Id).

running() ->
    init(),
    ets:info(?TAB, size) > 0.

running(Id) ->
    init(),
    ets:member(?TAB, Id).

list() ->
    init(),
    [I || {I, _} <- ets:tab2list(?TAB)].
