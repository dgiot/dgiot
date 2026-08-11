-module(dgiot_ontology_owl).
-export([export/2, export_all/1]).

export(ModelId, OutputPath) ->
    {ok, Models} = dgiot_ontology:list_models(),
    case lists:keyfind(ModelId, 1, Models) of
        false -> {error, model_not_found};
        {ModelId, Model} ->
            Xml = model_to_owl(Model),
            file:write_file(OutputPath, Xml),
            {ok, OutputPath}
    end.

export_all(OutputPath) ->
    {ok, Entities} = dgiot_ontology_registry:all(),
    Xml = entities_to_owl(Entities),
    file:write_file(OutputPath, Xml),
    {ok, OutputPath}.

model_to_owl(Model) ->
    ClassName = element(2, lists:keyfind(class, 1, Model)),
    SubClass  = element(2, lists:keyfind(sub_class, 1, Model)),
    Props = element(2, lists:keyfind(properties, 1, Model)),
    Rels  = element(2, lists:keyfind(relations, 1, Model)),
    iolist_to_binary([
     "<rdf:RDF",
     " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
     " xmlns:owl=\"http://www.w3.org/2002/07/owl#\"",
     " xml:base=\"http://dgiot.org/ontology/\">",
     "<owl:Class rdf:about=\"#", atom_to_list(ClassName), "\">",
     "<rdfs:subClassOf rdf:resource=\"#", atom_to_list(SubClass), "\"/>",
     "</owl:Class>",
     "</rdf:RDF>"
    ]).

entities_to_owl(Entities) ->
    iolist_to_binary([
     "<rdf:RDF",
     " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
     " xmlns:owl=\"http://www.w3.org/2002/07/owl#\"",
     " xml:base=\"http://dgiot.org/ontology/\">",
     [["<owl:NamedIndividual rdf:about=\"#", Id, "\">",
       "<rdf:type rdf:resource=\"#", atom_to_list(maps:get(class, E)), "\"/>",
       "</owl:NamedIndividual>"]
      || {Id, E} <- maps:to_list(Entities)],
     "</rdf:RDF>"
    ]).
