-module(dgiot_ontology_model).
-export([from_map/1, validate/1]).

from_map(#{<<"modelId">> := ModelId} = M) ->
    Model = #{
        model_id => ModelId,
        class => binary_to_atom(maps:get(<<"class">>, M, <<"entity">>)),
        properties => maps:get(<<"properties">>, M, []),
        relations => maps:get(<<"relations">>, M, []),
        rules => maps:get(<<"rules">>, M, [])
    },
    case validate(Model) of ok -> {ok, Model}; {error, R} -> {error, R} end.

validate(#{model_id := Id}) when byte_size(Id) > 0 -> ok;
validate(_) -> {error, missing_model_id}.
