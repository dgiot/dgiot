%%--------------------------------------------------------------------
%% dgiot_ontology — DLAS 本体论引擎 v2.0
%%
%% Data:    Parse/PG · TDengine · ETS tables
%% Logic:   load_model · spawn_instance · registry · rules · reasoner
%% Action:  Shadow gen_statem · Bridge · MQTT
%% Security: auth · role · ACL/CLP (via dgiot_parse)
%%
%% FDE Pipeline: Model → Ontology → Device → TimeSeries → Rules → Dashboard
%%--------------------------------------------------------------------
-module(dgiot_ontology).
-author("edge-hub").
-include_lib("dgiot/include/logger.hrl").

%% ── API ──
-export([
    init/0,                  %% 初始化 ETS 表 + 加载所有模型
    load_model/1,            %% 加载单个物模型
    spawn_instance/2,        %% 根据模型创建 Shadow 进程
    get_model/1,             %% 查询模型定义
    list_instances/1,        %% 列出 Class 下所有实例
    register/2,              %% 注册本体节点 (向下兼容)
    get_path/1,              %% MQTT topic 路径
    push_point/2,            %% 推送测点
    health/0                 %% 健康检查
]).

%% ── ETS 表定义 ──
-define(MODEL_TABLE,  dgiot_ontology_model).      %% Class -> #{properties, rules, relations}
-define(INST_TABLE,   dgiot_ontology_instance).   %% InstanceId -> #{class, model, pid}
-define(RULES_TABLE,  dgiot_ontology_rules).      %% RuleId -> #{when, then, severity}

%% ── 4层 Record ──
-record(site,    {id, name, type, location}).
-record(gateway, {id, ip, site, protocols=[], devices=[]}).
-record(device,  {id, gateway, name, type, protocol, slaveid, points=[]}).
-record(point,   {id, device, name, unit, range, alarm}).

%%====================================================================
%% Data Layer: ETS Init
%%====================================================================

init() ->
    ets:new(?MODEL_TABLE,  [named_table, public, {keypos, 1}]),
    ets:new(?INST_TABLE,   [named_table, public, {keypos, 1}]),
    ets:new(?RULES_TABLE,  [named_table, public, {keypos, 1}]),
    logger:info("[ontology] ETS tables created"),
    {ok, #{tables => [model, instance, rules]}}.

%%====================================================================
%% Logic Layer: Model Loading
%%====================================================================

load_model(#{<<"class">> := Class} = Model) ->
    %% 物模型格式: {class, sub_class, properties[], relations[], rules[]}
    Properties = maps:get(<<"properties">>, Model, []),
    Relations  = maps:get(<<"relations">>,  Model, []),
    RuleDefs   = maps:get(<<"rules">>,       Model, []),

    %% 存模型定义
    ets:insert(?MODEL_TABLE, {Class, #{
        class => Class,
        sub_class => maps:get(<<"sub_class">>, Model, <<>>),
        properties => Properties,
        relations => Relations,
        rules => RuleDefs
    }}),

    %% 编译规则
    lists:foreach(fun(Rule) ->
        RuleId = maps:get(<<"id">>, Rule),
        ets:insert(?RULES_TABLE, {RuleId, Rule})
    end, RuleDefs),

    logger:info("[ontology] Model loaded: ~s (~p props, ~p rules)",
        [Class, length(Properties), length(RuleDefs)]),
    {ok, Class}.

%%====================================================================
%% Logic Layer: Instance Spawning
%%====================================================================

spawn_instance(Class, InstanceId) ->
    case ets:lookup(?MODEL_TABLE, Class) of
        [] -> {error, model_not_found};
        [{Class, Model}] ->
            %% 启动 gen_statem Shadow 进程
            {ok, Pid} = dgiot_shadow:start_link(InstanceId, #{
                class => Class,
                model => Model,
                properties => init_properties(maps:get(properties, Model, []))
            }),
            ets:insert(?INST_TABLE, {InstanceId, #{
                class => Class,
                model => Model,
                pid => Pid,
                status => init,
                created => erlang:system_time(second)
            }}),
            logger:info("[ontology] Instance spawned: ~s (~s)", [InstanceId, Class]),
            {ok, Pid}
    end.

%%====================================================================
%% Logic Layer: Registry
%%====================================================================

get_model(Class) ->
    case ets:lookup(?MODEL_TABLE, Class) of
        [] -> {error, not_found};
        [{Class, Model}] -> {ok, Model}
    end.

list_instances(Class) ->
    MatchSpec = [{{'_', #{class => Class, pid => '$1'}}, [], ['$1']}],
    ets:select(?INST_TABLE, MatchSpec).

%%====================================================================
%% 4-Layer Register (向下兼容)
%%====================================================================

register(site, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Site">>, Map#{"objectId" => Id}),
    {ok, Id};

register(gateway, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Gateway">>, Map#{"objectId" => Id}),
    {ok, Id};

register(device, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Device">>, Map#{"objectId" => Id}),
    {ok, Id};

register(point, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Point">>, Map#{"objectId" => Id}),
    {ok, Id}.

%%====================================================================
%% MQTT Topic Path
%%====================================================================

get_path(PointId) ->
    {ok, #{<<"device">> := DevId, <<"id">> := Pid}} =
        dgiot_parse:get_object(<<"Point">>, PointId),
    {ok, #{<<"gateway">> := GwId, <<"id">> := Did}} =
        dgiot_parse:get_object(<<"Device">>, DevId),
    {ok, #{<<"site">> := SiteId}} =
        dgiot_parse:get_object(<<"Gateway">>, GwId),
    <<"dgiot/", SiteId/binary, "/", GwId/binary, "/", Did/binary, "/", Pid/binary>>.

%%====================================================================
%% Action Layer: Push
%%====================================================================

push_point(PointId, Value) ->
    Topic = get_path(PointId),
    TopicData = iolist_to_binary([Topic, <<"/data">>]),
    Payload = #{
        ts => erlang:system_time(millisecond),
        v => Value,
        q => 192
    },
    dgiot_mqtt:publish(TopicData, jsx:encode(Payload)),
    {ok, TopicData}.

%%====================================================================
%% Internal
%%====================================================================

init_properties(Props) ->
    maps:from_list([{maps:get(<<"id">>, P), maps:get(<<"type">>, P)} || P <- Props]).

%%====================================================================
%% Health
%%====================================================================

health() ->
    #{
        version => <<"2.0">>,
        ontology => <<"DLAS: Data·Logic·Action·Security">>,
        pipeline => <<"FDE: Model→Ontology→Device→TS→Rules→Dashboard">>,
        tables => #{
            models => ets:info(?MODEL_TABLE, size),
            instances => ets:info(?INST_TABLE, size),
            rules => ets:info(?RULES_TABLE, size)
        }
    }.
