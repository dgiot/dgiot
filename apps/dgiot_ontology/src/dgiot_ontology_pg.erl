-module(dgiot_ontology_pg).
-export([create_schema/0, save_model/1, load_model/1, list_models/0, delete_model/1,
         save_instance/2, list_instances/0, save_relation/3, audit_log/4]).

-define(MODEL, <<"OntologyModel">>).
-define(INSTANCE, <<"OntologyInstance">>).
-define(RELATION, <<"OntologyRelation">>).
-define(AUDIT, <<"OntologyAudit">>).

create_schema() ->
    dgiot_parse:create_schemas(#{<<className>> => ?MODEL}),
    dgiot_parse:create_schemas(#{<<className>> => ?INSTANCE}),
    dgiot_parse:create_schemas(#{<<className>> => ?RELATION}),
    dgiot_parse:create_schemas(#{<<className>> => ?AUDIT}),
    ok.

save_model(#{model_id := Id} = M) ->
    dgiot_parse:create_object(?MODEL, #{
        <<"model_id">> => Id,
        <<"class_name">> => maps:get(class, M, <<"entity">>),
        <<"properties">> => maps:get(properties, M, []),
        <<"relations">> => maps:get(relations, M, []),
        <<"rules">> => maps:get(rules, M, [])
    }).

load_model(Id) -> dgiot_parse:get_object(?MODEL, Id).
list_models() -> dgiot_parse:query_object(?MODEL, #{<<limit>> => 100}).
delete_model(Id) -> dgiot_parse:del_object(?MODEL, Id).

save_instance(Id, #{class := Class}) ->
    dgiot_parse:create_object(?INSTANCE, #{
        <<"instance_id">> => Id,
        <<"class_name">> => atom_to_binary(Class),
        <<"state">> => <<"active">>
    }).

list_instances() -> dgiot_parse:query_object(?INSTANCE, #{<<limit>> => 100}).

save_relation(Source, Relation, Target) ->
    dgiot_parse:create_object(?RELATION, #{
        <<"source_id">> => Source,
        <<"relation_type">> => atom_to_binary(Relation),
        <<"target_id">> => Target
    }).

audit_log(EntityId, Action, Old, New) ->
    dgiot_parse:create_object(?AUDIT, #{
        <<"entity_id">> => EntityId,
        <<"action">> => atom_to_binary(Action),
        <<"old_state">> => Old,
        <<"new_state">> => New
    }).
