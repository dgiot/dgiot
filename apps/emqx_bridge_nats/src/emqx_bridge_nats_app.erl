-module(emqx_bridge_nats_app).

-behaviour(application).
-include("emqx_bridge_nats.hrl").

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_bridge_nats_sup:start_link(),
    ok = emqx_bridge_nats:load([]),
    {ok, Sup}.

stop(_State) ->
    emqx_bridge_nats:unload().
