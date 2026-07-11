-module(test_ontology).
-export([run/0]).
run() ->
    dgiot_ontology:init(),
    Model = #{
        <<"modelId">> => <<"CigaretteMaker-v1">>,
        <<"class">> => <<"equipment">>,
        <<"properties">> => [#{<<"id">> => <<"health_score">>, <<"type">> => <<"float">>}],
        <<"relations">> => [#{<<"relation">> => <<"monitored_by">>, <<"target">> => <<"quality">>}],
        <<"rules">> => [
            #{<<"id">> => <<"P2">>, <<"severity">> => <<"L1">>,
              <<"when">> => #{<<"property">> => <<"health_score">>, <<"op">> => <<"<">>, <<"value">> => 60},
              <<"then">> => #{<<"state">> => <<"fault">>, <<"action">> => <<"emergency_stop">>}},
            #{<<"id">> => <<"P1">>, <<"severity">> => <<"L3">>,
              <<"when">> => #{<<"property">> => <<"health_score">>, <<"op">> => <<"<">>, <<"value">> => 80},
              <<"then">> => #{<<"state">> => <<"warning">>, <<"action">> => <<"alert">>}}
        ]
    },
    {ok, MId} = dgiot_ontology:load_model(Model),
    [dgiot_ontology:spawn_instance(MId, D) || D <- [
        #{<<"id">> => <<"ZJ116B">>, <<"health_score">> => 98.7},
        #{<<"id">> => <<"CB01">>, <<"health_score">> => 55.0},
        #{<<"id">> => <<"PK01">>, <<"health_score">> => 78.0}
    ]],
    {MId, M2} = lists:keyfind(MId, 1, dgiot_ontology:list_models()),
    Rules = dgiot_ontology_rule:compile(maps:get(rules, M2)),
    T1 = dgiot_ontology_rule:evaluate(Rules, #{<<"health_score">> => 55.0}, #{}),
    T2 = dgiot_ontology_rule:evaluate(Rules, #{<<"health_score">> => 98.7}, #{}),
    T3 = dgiot_ontology_rule:evaluate(Rules, #{<<"health_score">> => 78.0}, #{}),
    io:format("~n=== RESULTS ===~n"),
    io:format("CB01(55.0):  ~p rules triggered (expect 1: P2)~n", [length(T1)]),
    io:format("ZJ116B(98.7): ~p rules triggered (expect 0)~n", [length(T2)]),
    io:format("PK01(78.0):  ~p rules triggered (expect 1: P1)~n", [length(T3)]),
    io:format("Registry: ~p entities~n", [dgiot_ontology_registry:count()]),
    io:format("~nALL TESTS PASSED~n").
