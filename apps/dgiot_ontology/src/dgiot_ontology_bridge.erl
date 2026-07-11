-module(dgiot_ontology_bridge).
-export([on_device_data/2, on_device_register/2, evaluate/2]).

on_device_data(Id, #{<<"properties">> := Props}) ->
    case evaluate(Id, Props) of
        {triggered, Actions} -> {ok, Actions};
        {ok, none} -> {ok, []}
    end.

on_device_register(Id, _Product) ->
    dgiot_ontology_registry:register(equipment, Id, #{}).

evaluate(_Id, Props) ->
    All = dgiot_ontology:list_models(),
    Compiled = [dgiot_ontology_rule:compile(maps:get(rules, M, [])) || {_, M} <- All],
    AllRules = lists:flatten(Compiled),
    case AllRules of
        [] -> {ok, none};
        _ ->
            Triggered = dgiot_ontology_rule:evaluate(AllRules, Props, #{}),
            case Triggered of
                [] -> {ok, none};
                _ -> {triggered, [maps:get(action, R) || R <- Triggered]}
            end
    end.
