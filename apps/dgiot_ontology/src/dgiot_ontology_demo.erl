-module(dgiot_ontology_demo).
-export([run/0, run_demo/1]).

run_demo(m) -> run_mfg();
run_demo(e) -> run_energy();
run_demo(b) -> run_bldg();
run_demo(_) -> run_mfg().
run() -> run_mfg().

run_mfg() ->
    header("Manufacturing - Machine Health"),
    demo(<<"Machine-v1">>,<<"equipment">>,
        [prop(<<"health_score">>,<<"float">>),prop(<<"speed">>,<<"int">>),prop(<<"vibration">>,<<"float">>)],
        [rule(<<"P2">>,<<"health_score">>,<<"<">>,60,<<"fault">>,<<"emergency_stop">>,<<"L1">>),
         rule(<<"P1">>,<<"health_score">>,<<"<">>,80,<<"warning">>,<<"alert">>,<<"L3">>),
         rule(<<"V1">>,<<"vibration">>,<<">">>,25,<<"fault">>,<<"maintenance">>,<<"L2">>)],
        [<<"M1">>,<<"M2">>,<<"M3">>],
        [{<<"M1">>,<<"OK">>,#{<<"health_score">>=>92,<<"vibration">>=>18}},
         {<<"M2">>,<<"FAIL">>,#{<<"health_score">>=>52,<<"vibration">>=>28}},
         {<<"M3">>,<<"WARN">>,#{<<"health_score">>=>76,<<"vibration">>=>22}}]).

run_energy() ->
    header("Energy - Solar O&M"),
    demo(<<"Solar-v1">>,<<"equipment">>,
        [prop(<<"power_output">>,<<"float">>),prop(<<"efficiency">>,<<"float">>)],
        [rule(<<"E1">>,<<"efficiency">>,<<"<">>,75,<<"warning">>,<<"clean_panel">>,<<"L2">>)],
        [<<"PV1">>,<<"PV2">>],
        [{<<"PV1">>,<<"OK">>,#{<<"power_output">>=>85,<<"efficiency">>=>88}},
         {<<"PV2">>,<<"LOW">>,#{<<"power_output">>=>45,<<"efficiency">>=>68}}]).

run_bldg() ->
    header("Building - Smart Energy"),
    demo(<<"HVAC-v1">>,<<"equipment">>,
        [prop(<<"power">>,<<"float">>),prop(<<"air_quality">>,<<"float">>)],
        [rule(<<"A1">>,<<"air_quality">>,<<"<">>,60,<<"warning">>,<<"ventilate">>,<<"L2">>)],
        [<<"F1">>,<<"F2">>],
        [{<<"F1">>,<<"OK">>,#{<<"power">>=>65,<<"air_quality">>=>78}},
         {<<"F2">>,<<"LOW">>,#{<<"power">>=>72,<<"air_quality">>=>52}}]).

demo(ModelId, Class, Props, Rules, Devices, Tests) ->
    dgiot_ontology:start(),
    M = #{<<"modelId">>=>ModelId,<<"class">>=>Class,<<"properties">>=>Props,<<"relations">>=>[],<<"rules">>=>Rules},
    dgiot_ontology:load_model(M),
    io:format("  Model: ~s (~p props, ~p rules)~n",[ModelId,length(Props),length(Rules)]),
    [dgiot_ontology_bridge:on_device_register(I,<<"default">>) || I <- Devices],
    io:format("  Devices: ~p~n",[length(Devices)]),
    [begin
        io:format("  ~s(~s): ",[Id,Tag]),
        case dgiot_ontology_bridge:on_device_data(Id,#{<<"properties">>=>PD}) of
            {ok,[]} -> io:format("OK~n");
            {ok,As} -> io:format("TRIGGERED ~p: ~p~n",[length(As),As])
        end
    end || {Id,Tag,PD} <- Tests],
    io:format("  Registry:~p Models:~p~n",[dgiot_ontology_registry:count(),length(dgiot_ontology:list_models())]).

prop(Id,Type) -> #{<<"id">>=>Id,<<"type">>=>Type}.
rule(Id,P,Op,Val,S,Act,Sev) ->
    #{<<"id">>=>Id,<<"severity">>=>Sev,
      <<"when">>=>#{<<"property">>=>P,<<"op">>=>Op,<<"value">>=>Val},
      <<"then">>=>#{<<"state">>=>S,<<"action">>=>Act}}.
header(T) -> io:format("~n=== ~s ===~n",[T]).
