%%% dgiot_ontology 完整测试套件
-module(dgiot_ontology_test).
-export([run/0]).

run() ->
    io:format("~n=== dgiot_ontology Test Suite ===~n"),
    test_startup(),
    test_registry(),
    test_rule_engine(),
    test_model_loader(),
    test_bridge(),
    test_rule_engine_standalone(),
    io:format("~n=== ALL TESTS PASSED ===~n~n").

test_startup() ->
    dgiot_ontology:start(),
    Pid = whereis(dgiot_ontology_registry),
    true = is_pid(Pid),
    io:format("  [PASS] startup~n").

test_registry() ->
    dgiot_ontology_registry:register(equipment, <<"test-1">>, #{health => 95}),
    dgiot_ontology_registry:register(equipment, <<"test-2">>, #{health => 55}),
    2 = dgiot_ontology_registry:count(),
    {ok, #{class := equipment}} = dgiot_ontology_registry:lookup(<<"test-1">>),
    {error, not_found} = dgiot_ontology_registry:lookup(<<"noexist">>),
    io:format("  [PASS] registry (register/lookup/count)~n").

test_rule_engine() ->
    Rules = dgiot_ontology_rule:compile([
        #{<<"id">> => <<"P2">>, <<"severity">> => <<"L1">>,
          <<"when">> => #{<<"property">> => <<"health">>, <<"op">> => <<"<">>, <<"value">> => 60},
          <<"then">> => #{<<"state">> => <<"fault">>, <<"action">> => <<"stop">>}}
    ]),
    %% health=55 should trigger
    1 = length(dgiot_ontology_rule:evaluate(Rules, #{<<"health">> => 55}, #{})),
    %% health=95 should NOT trigger
    0 = length(dgiot_ontology_rule:evaluate(Rules, #{<<"health">> => 95}, #{})),
    io:format("  [PASS] rule engine (compile/evaluate/match)~n").

test_model_loader() ->
    M = #{<<"modelId">> => <<"TestModel-v1">>, <<"class">> => <<"equipment">>,
          <<"properties">> => [#{<<"id">> => <<"temp">>, <<"type">> => <<"float">>}],
          <<"relations">> => [], <<"rules">> => []},
    {ok, <<"TestModel-v1">>} = dgiot_ontology:load_model(M),
    1 = length(dgiot_ontology:list_models()),
    io:format("  [PASS] model loader~n").

test_bridge() ->
    dgiot_ontology_bridge:on_device_register(<<"dev-1">>, <<"test">>),
    {ok, []} = dgiot_ontology_bridge:on_device_data(<<"dev-1">>,
        #{<<"properties">> => #{<<"health">> => 95}}),
    io:format("  [PASS] bridge (register/data flow)~n").

test_rule_engine_standalone() ->
    dgiot_ontology_rule_engine:init(),
    dgiot_ontology_rule_engine:create(<<"hot">>,
        fun(#{temp := T}) -> T > 100 end,
        fun(_) -> alert end),
    dgiot_ontology_rule_engine:create(<<"cold">>,
        fun(#{temp := T}) -> T < 0 end,
        fun(_) -> freeze end),
    {ok, [{_, alert}]} = dgiot_ontology_rule_engine:fire(#{temp => 150}, #{}),
    {ok, []} = dgiot_ontology_rule_engine:fire(#{temp => 25}, #{}),
    {ok, [{_, freeze}]} = dgiot_ontology_rule_engine:fire(#{temp => -5}, #{}),
    io:format("  [PASS] standalone rule engine~n").
