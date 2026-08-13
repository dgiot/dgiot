%% dgiot_ontology v3 — ETS path cache optimization
%% Fix #1: get_path() caches result in ETS, no 3x REST per push

%% ——— 新增 ETS 表 ———
-define(PATH_CACHE, dgiot_ontology_path_cache).  %% PointId -> Path (binary)

init() ->
    ets:new(?MODEL_TABLE,  [named_table, public, {keypos, 1}]),
    ets:new(?INST_TABLE,   [named_table, public, {keypos, 1}]),
    ets:new(?RULES_TABLE,  [named_table, public, {keypos, 1}]),
    ets:new(?PATH_CACHE,   [named_table, public, {keypos, 1}]),  %% NEW
    logger:info("[ontology] ETS tables created (v3: +path_cache)"),
    {ok, #{tables => [model, instance, rules, path_cache]}}.

%% ——— get_path v3: ETS cache first, REST fallback ———
get_path(PointId) ->
    case ets:lookup(?PATH_CACHE, PointId) of
        [{PointId, Path}] ->
            Path;  %% <1us 缓存命中
        [] ->
            %% 3次REST (冷路径, 仅首次)
            {ok, #{<<"device">> := DevId, <<"id">> := Pid}} =
                dgiot_parse:get_object(<<"Point">>, PointId),
            {ok, #{<<"gateway">> := GwId, <<"id">> := Did}} =
                dgiot_parse:get_object(<<"Device">>, DevId),
            {ok, #{<<"site">> := SiteId}} =
                dgiot_parse:get_object(<<"Gateway">>, GwId),
            Path = <<"dgiot/", SiteId/binary, "/", GwId/binary, "/", Did/binary, "/", Pid/binary>>,
            ets:insert(?PATH_CACHE, {PointId, Path}),  %% 存入缓存
            Path
    end.

%% ——— register 时预热缓存 ———
register(point, #{id := Id} = Map) ->
    Result = dgiot_parse:create_object(<<"Point">>, Map#{"objectId" => Id}),
    %% 注册后立即计算并缓存 path
    spawn(fun() -> get_path(Id) end),  %% 异步预热,不阻塞
    Result;

%% ——— push_point v3: 直接走缓存 ———
push_point(PointId, Value) ->
    Topic = get_path(PointId),     %% v3: 首次REST,后续<1us
    TopicData = iolist_to_binary([Topic, <<"/data">>]),
    dgiot_mqtt:publish(TopicData, jsx:encode(#{
        ts => erlang:system_time(millisecond), v => Value, q => 192
    })),
    {ok, TopicData}.
