-module(dgaiot_plugin_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Children) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Children).

init(Children) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, Children}}.
