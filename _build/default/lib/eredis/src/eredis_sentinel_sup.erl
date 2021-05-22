-module(eredis_sentinel_sup).

-behaviour(supervisor).

-export([ start_link/1
        , start_child/1
        ]).

-export([init/1]).

start_link(Env) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Env]).

start_child(Env) ->
  ChildSpec = child_spec(Env),
  case supervisor:start_child(?MODULE, ChildSpec) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    {error, {{already_started, Pid}, _}} -> {ok, Pid};
    {error, Error} -> {error, Error}
  end.

init([Env]) ->
    {ok, {{one_for_one, 10, 100}, [{eredis_sentinel,
                                   {eredis_sentinel, start_link, [Env]},
                                   permanent, 5000, worker, [eredis_sentinel]}]}}.

child_spec(Env) ->
  #{id => eredis_sentinel,
    start => {eredis_sentinel, start_link, [Env]},
    restart => transient,
    type => worker,
    modules => [eredis_sentinel]
   }.