-module(dgiot_ontology_api).
-export([handle/3, init/0]).

init() -> ok.

%% handle(Method, Path, Body) -> {StatusCode, Headers, ResponseBody}
%% ═══ 本体三层定义 API ═══
handle(<<"GET">>, <<"/api/ontology/definition">>, _Body) ->
    Def = #{
        <<"ontology">> => <<"本体 = 知识图谱(骨架) + SWRL推理(大脑) + 实时执行(神经)">>,
        <<"quote">> => <<"本体不是地图，是神经。不是存储，是过程。不是名词，是动词。">>,
        <<"vs_knowledge_graph">> => <<"本体 ⊃ 知识图谱。知识图谱是本体的第一层(骨架)。少了实时同步和闭环执行，本体只是死的地图。">>,
        <<"layers">> => [
            #{<<"name">> => <<"第一层: 知识图谱">>,
              <<"aka">> => <<"Gruber 1993 | 概念化的形式规范">>,
              <<"role">> => <<"骨架 — 245+类·58属性·43约束 — OWL RDF/XML — Neo4j">>,
              <<"status">> => <<"inherited">>},
            #{<<"name">> => <<"第二层: SWRL推理">>,
              <<"aka">> => <<"Guarino 1995 | 可能世界的逻辑理论">>,
              <<"role">> => <<"大脑 — 20条规则·L0-L3分层推理·50条安全判据">>,
              <<"status">> => <<"inherited">>},
            #{<<"name">> => <<"第三层: 实时执行">>,
              <<"aka">> => <<"本方案 2026 | 物理世界的实时数字镜像">>,
              <<"role">> => <<"神经 — 传感器驱动·gen_statem <1ms·<2s闭环·Actor进程隔离·99.999%可用">>,
              <<"status">> => <<"innovated">>}
        ]},
    {200, json_header(), jsx:encode(Def)};

handle(<<"GET">>, <<"/api/ontology/models">>, _Body) ->
    Models = dgiot_ontology:list_models(),
    List = [format_model(Id, M) || {Id, M} <- Models],
    {200, json_header(), list_to_json(List)};

handle(<<"GET">>, <<"/api/ontology/models/", Id/binary>>, _Body) ->
    case lists:keyfind(Id, 1, dgiot_ontology:list_models()) of
        {Id, M} -> {200, json_header(), map_to_json(format_model(Id, M))};
        false -> {404, json_header(), <<"{\"error\":\"not found\"}">>}
    end;

handle(<<"POST">>, <<"/api/ontology/evaluate">>, Body) ->
    Props = jsx:decode(Body, [return_maps]),
    Rules = dgiot_ontology_rule:compile([]),
    Triggered = dgiot_ontology_rule:evaluate(Rules, Props, #{}),
    {200, json_header(), list_to_json(Triggered)};

handle(<<"POST">>, <<"/api/ontology/spawn">>, Body) ->
    Data = jsx:decode(Body, [return_maps]),
    Id = maps:get(<<"id">>, Data, <<"anon">>),
    dgiot_ontology_registry:register(equipment, Id, Data),
    {201, json_header(), <<"{\"status\":\"created\"}">>};

handle(_, _, _) ->
    {404, json_header(), <<"{\"error\":\"not found\"}">>}.

format_model(Id, M) ->
    #{id => Id, class => maps:get(class, M),
      properties => maps:get(properties, M, []),
      rules => maps:get(rules, M, [])}.

json_header() -> #{<<"Content-Type">> => <<"application/json">>}.

list_to_json(L) -> jsx:encode(L).
map_to_json(M) -> jsx:encode(M).
