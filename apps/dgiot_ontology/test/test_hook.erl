-module(test_hook).
-export([run/0]).
run() ->
    dgiot_ontology:start(),
    io:format("started
"),
    dgiot_ontology_hook:register_hooks(),
    io:format("hooks_registered
"),
    K = <<"properties">>,
    HS = <<"health_score">>,
    M = #{K => #{HS => 98.7}},
    io:format("msg_created: ~p
", [M]),
    R = dgiot_ontology_hook:on_message(<<"ZJ116B">>, M),
    io:format("result: ~p
", [R]),
    io:format("DONE
").
