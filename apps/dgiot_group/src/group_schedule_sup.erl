%% 所有抄表任务计划
-module(group_schedule_sup).

-include("dgiot_group.hrl").

-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SCH_NAME(Tid), list_to_atom(binary_to_list(<<Tid/binary, "_sch">>))).

start_link(#{<<"tid">> := Tid, <<"di">> := Di}) ->
    BinTid = dgiot_utils:to_binary(Tid),
    BinDi = dgiot_utils:to_binary(Di),
    start_link(?SCH_NAME(<<BinTid/binary,"/",BinDi/binary>>));

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Child = [{group_schedule_worker, {group_schedule_worker, start_link, []}, transient, 5000, worker, [group_schedule_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.




















