-module(dgiot_ontology).
-export([init/0, start/0, load_model/1, spawn_instance/2, list_models/0,
         add_rule/2, delete_model/1, get_rules/1]).

-define(TBL, dgiot_ontology_models).

init() -> 
    case ets:info(?TBL) of
        undefined -> ets:new(?TBL, [named_table, public, set, {keypos, 1}]), ok;
        _ -> ok
    end.

start() ->
    init(),
    case whereis(dgiot_ontology_registry) of
        undefined -> dgiot_ontology_registry:start_link();
        _ -> ok
    end,
    io:format("[ONTOLOGY] Started (ETS + Registry)~n"),
    ok.

load_model(#{<<"modelId">> := Id} = M) ->
    {ok, Model} = dgiot_ontology_model:from_map(M),
    ets:insert(?TBL, {Id, Model}),
    io:format("[ONTOLOGY] Model ~s loaded~n", [Id]),
    {ok, Id}.

spawn_instance(ModelId, #{<<"id">> := Id} = Data) ->
    [{ModelId, Model}] = ets:lookup(?TBL, ModelId),
    dgiot_ontology_registry:register(maps:get(class, Model), Id, Data),
    {ok, Id}.

list_models() -> ets:tab2list(?TBL).
delete_model(ModelId) -> ets:delete(?TBL, ModelId).
add_rule(ModelId, Rule) ->
    [{ModelId, Model}] = ets:lookup(?TBL, ModelId),
    Rules = maps:get(rules, Model, []),
    ets:insert(?TBL, {ModelId, Model#{rules => [Rule | Rules]}}),
    {ok, rule_added}.
get_rules(ModelId) ->
    [{ModelId, Model}] = ets:lookup(?TBL, ModelId),
    maps:get(rules, Model, []).
