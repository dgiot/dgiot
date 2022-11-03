
-module(dashboard_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).


init([]) ->
    Child = [
        {dashboard_worker, {dashboard_worker, start_link, []}, transient, 5000, worker, [dashboard_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.











