%% @doc 同名承接：emqx_alarm（刀 6 批次 2）。轻量告警表（ETS）。
%% EMQX 的告警有激活/停用两态，这里保持一致语义。
-module(emqx_alarm).

-include("emqx.hrl").

-export([init/0, activate/2, activate/3, deactivate/1, get_alarms/1,
         delete_all_deactivated_alarms/0, delete_alarm/1, clear/0]).

-define(TAB, dgiot_broker_alarms).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public]);
        _ -> ?TAB
    end,
    ok.

clear() -> init(), ets:delete_all_objects(?TAB), ok.

activate(Name, Details) -> activate(Name, Details, undefined).

activate(Name, Details, Message) ->
    init(),
    ets:insert(?TAB, {Name, #{name => Name,
                              details => Details,
                              message => Message,
                              activate_at => erlang:system_time(millisecond),
                              deactivated => false}}),
    logger:warning("[alarm] ~p ~p", [Name, Details]),
    ok.

%% deactivate 幂等（EMQX 语义：重复停用不报错）
deactivate(Name) ->
    init(),
    case ets:lookup(?TAB, Name) of
        [{Name, Alarm}] ->
            ets:insert(?TAB, {Name, Alarm#{deactivated => true,
                                           deactivate_at =>
                                               erlang:system_time(millisecond)}}),
            ok;
        [] ->
            ok
    end.

%% get_alarms(activated | deactivated | all)
get_alarms(activated) ->
    [A || {_N, #{deactivated := false} = A} <- alarms()];
get_alarms(deactivated) ->
    [A || {_N, #{deactivated := true} = A} <- alarms()];
get_alarms(all) ->
    [A || {_N, A} <- alarms()];
get_alarms(Other) ->
    {error, {bad_alarm_filter, Other}}.

delete_all_deactivated_alarms() ->
    init(),
    [ets:delete(?TAB, N) || {N, #{deactivated := true}} <- alarms()],
    ok.

delete_alarm(Name) ->
    init(),
    ets:delete(?TAB, Name),
    ok.

alarms() -> init(), ets:tab2list(?TAB).
