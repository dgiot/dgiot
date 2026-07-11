-module(dgiot_ontology_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Registry = #{id => dgiot_ontology_registry,
                 start => {dgiot_ontology_registry, start_link, []},
                 restart => permanent, type => worker},
    {ok, {SupFlags, [Registry]}}.
