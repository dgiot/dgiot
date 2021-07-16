
-module(group_task_sup).

-include("dgiot_group.hrl").
-behaviour(supervisor).
-include_lib("dgiot/include/logger.hrl").

-export([start_link/1, init/1]).

-define(TASK_NAME(Tid), list_to_atom(binary_to_list(<<Tid/binary, "_task">>))).

start_link(#{<<"tid">> := Tid, <<"di">> := Di}) ->
    BinTid = dgiot_utils:to_binary(Tid),
    BinDi = dgiot_utils:to_binary(Di),
    start_link(?TASK_NAME(<<BinTid/binary,"/",BinDi/binary>>));

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Child = [{group_task_worker, {group_task_worker, start_link, []}, transient, 5000, worker, [group_task_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.











