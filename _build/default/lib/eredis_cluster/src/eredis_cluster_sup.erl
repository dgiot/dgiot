-module(eredis_cluster_sup).
-behaviour(supervisor).

%% Supervisor.
-export([start_link/0]).
-export([start_child/2, stop_child/1]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([])
    -> {ok, {{supervisor:strategy(), 1, 5}, [supervisor:child_spec()]}}.
init([]) ->
    ets:new(eredis_cluster_monitor, [public, set, named_table, {read_concurrency, true}]),
    Procs = [{eredis_cluster_pool,
                {eredis_cluster_pool, start_link, []},
                permanent, 5000, supervisor, [dynamic]}
            ],
    {ok, {{one_for_one, 1, 5}, Procs}}.

start_child(Name, Params) ->
    ChildSpec = {name(Name),
                 {eredis_cluster_monitor, start_link, Params},
                 permanent, 5000, worker, [eredis_cluster_monitor]},
    supervisor:start_child(?MODULE, ChildSpec).

stop_child(Name) ->
    case supervisor:terminate_child(?MODULE, name(Name)) of
        ok -> supervisor:delete_child(?MODULE, name(Name));
        Error -> Error
    end.

name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_eredis_cluster_monitor").