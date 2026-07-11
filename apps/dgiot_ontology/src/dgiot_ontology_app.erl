-module(dgiot_ontology_app).

-ifdef(DGIOT_WITH_EMQX).
-dgiot_plugin(?MODULE).
-endif.

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dgiot_ontology:init(),
    dgiot_statem_model:init(),
    {ok, OntSup} = dgiot_ontology_sup:start_link(),
    _StatemSup = dgiot_statem_sup:start_link(),
    dgiot_statem_hook:register(),
    io:format("[DGIOT-ONTOLOGY] Engine started (OWL + gen_statem)~n"),
    {ok, OntSup}.

stop(_State) ->
    io:format("[DGIOT-ONTOLOGY] Engine stopped~n"),
    ok.
