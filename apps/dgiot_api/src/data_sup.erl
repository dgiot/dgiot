
-module(data_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).


init([]) ->
    Child = [
        {data_worker, {data_worker, start_link, []}, transient, 5000, worker, [data_worker]}],
    {ok, {{simple_one_for_one, 5, 10}, Child}}.











