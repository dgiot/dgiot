%% dgiot_ontology_reasoner — 混合架构L2/L3桥接模块
%% 连接 Java Pellet/Jena/Drools 和 Python ONNX Runtime
-module(dgiot_ontology_reasoner).
-export([check_consistency/1, sparql_query/2, swrl_evaluate/2,
         anomaly_detect/2, trend_predict/2, health/0]).

-define(JAVA_URL, "http://localhost:8081").
-define(PYTHON_URL, "http://localhost:8082").

%% ========== L2: Java推理层 ==========

check_consistency(OwlRdf) ->
    case http_post(?JAVA_URL ++ "/api/reasoner/consistency", "application/rdf+xml", OwlRdf) of
        {ok, Body} ->
            #{<<"consistent">> := Consistent, <<"time_ms">> := Ms} = jsx:decode(Body, [return_maps]),
            {ok, #{consistent => Consistent, time_ms => Ms}};
        {error, Reason} ->
            {error, Reason}
    end.

sparql_query(Query, ModelId) ->
    Body = jsx:encode(#{<<"query">> => Query, <<"modelId">> => ModelId}),
    case http_post(?JAVA_URL ++ "/api/reasoner/sparql", "application/json", Body) of
        {ok, Resp} ->
            #{<<"results">> := Results, <<"time_ms">> := Ms} = jsx:decode(Resp, [return_maps]),
            {ok, #{results => Results, time_ms => Ms}};
        {error, Reason} ->
            {error, Reason}
    end.

swrl_evaluate(Props, _Context) ->
    Body = jsx:encode(#{<<"properties">> => Props}),
    case http_post(?JAVA_URL ++ "/api/reasoner/swrl", "application/json", Body) of
        {ok, Resp} ->
            #{<<"triggered_rules">> := Rules} = jsx:decode(Resp, [return_maps]),
            {ok, Rules};
        {error, Reason} ->
            {error, Reason}
    end.

%% ========== L3: Python深度学习层 ==========

anomaly_detect(DeviceId, Features) ->
    Body = jsx:encode(#{
        <<"device_id">> => DeviceId,
        <<"features">> => Features,
        <<"threshold">> => 0.85
    }),
    case http_post(?PYTHON_URL ++ "/api/inference/anomaly", "application/json", Body) of
        {ok, Resp} ->
            #{<<"anomaly_score">> := Score, <<"is_anomaly">> := IsAnomaly,
              <<"inference_ms">> := Ms} = jsx:decode(Resp, [return_maps]),
            {ok, #{score => Score, is_anomaly => IsAnomaly, time_ms => Ms}};
        {error, Reason} ->
            {error, Reason}
    end.

trend_predict(DeviceId, History) ->
    Body = jsx:encode(#{
        <<"device_id">> => DeviceId,
        <<"history">> => History,
        <<"horizon">> => 30
    }),
    case http_post(?PYTHON_URL ++ "/api/inference/trend", "application/json", Body) of
        {ok, Resp} ->
            #{<<"predictions">> := Preds, <<"trend">> := Trend,
              <<"inference_ms">> := Ms} = jsx:decode(Resp, [return_maps]),
            {ok, #{predictions => Preds, trend => Trend, time_ms => Ms}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ========== 健康检查 ==========

health() ->
    L2 = case http_get(?JAVA_URL ++ "/api/reasoner") of {ok, _} -> up; _ -> down end,
    L3 = case http_get(?PYTHON_URL ++ "/api/inference/health") of {ok, _} -> up; _ -> down end,
    #{l2_reasoner => L2, l3_inference => L3}.

%% ========== HTTP Helpers ==========

http_post(Url, ContentType, Body) ->
    case httpc:request(post, {Url, [], ContentType, Body},
                       [{timeout, 5000}, {connect_timeout, 2000}], []) of
        {ok, {{_, 200, _}, _, RespBody}} -> {ok, RespBody};
        {ok, {{_, Code, _}, _, _}} -> {error, {http_error, Code}};
        {error, Reason} -> {error, Reason}
    end.

http_get(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 2000}], []) of
        {ok, {{_, 200, _}, _, Body}} -> {ok, Body};
        _ -> {error, unreachable}
    end.
