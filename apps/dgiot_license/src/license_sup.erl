
-module(license_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(TASK_NAME(Tid), list_to_atom(binary_to_list(<<Tid/binary, "_lic">>))).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Child = [{license_worker, {license_worker, start_link, []}, transient, 5000, worker, [license_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.











