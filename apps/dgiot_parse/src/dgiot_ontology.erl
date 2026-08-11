%% dgiot_ontology v3.0 — DLAS 本体引擎 (完整版+优化)
-module(dgiot_ontology).
-author("edge-hub").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    init/0, register/2,
    get_topic/1, push_point/2,
    load_model/1, spawn_instance/2,
    get_model/1, list_instances/1, health/0
]).

%% ETS
-define(MODEL_TABLE,  dgiot_ontology_model).
-define(INST_TABLE,   dgiot_ontology_instance).
-define(RULES_TABLE,  dgiot_ontology_rules).
-define(PATH_CACHE,   dgiot_ontology_path).      %% 优化#1

%% Records
-record(site,    {id, name, type, location}).
-record(gateway, {id, ip, site, protocols=[], devices=[]}).
-record(device,  {id, gateway, name, type, protocol, slaveid, points=[]}).
-record(point,   {id, device, name, unit, range, alarm}).

%%====================================================================
%% Data: ETS Init
%%====================================================================
init() ->
    ets:new(?MODEL_TABLE, [named_table, public, {keypos, 1}]),
    ets:new(?INST_TABLE,  [named_table, public, {keypos, 1}]),
    ets:new(?RULES_TABLE, [named_table, public, {keypos, 1}]),
    ets:new(?PATH_CACHE,  [named_table, public, {keypos, 1}]),
    {ok, #{tables => [model, instance, rules, path]}}.

%%====================================================================
%% Logic: Model + Instance
%%====================================================================
load_model(#{<<"class">> := Class} = Model) ->
    Properties = maps:get(<<"properties">>, Model, []),
    Relations  = maps:get(<<"relations">>,  Model, []),
    RuleDefs   = maps:get(<<"rules">>,       Model, []),
    ets:insert(?MODEL_TABLE, {Class, #{class=>Class, properties=>Properties, relations=>Relations, rules=>RuleDefs}}),
    lists:foreach(fun(Rule) -> ets:insert(?RULES_TABLE, {maps:get(<<"id">>, Rule), Rule}) end, RuleDefs),
    {ok, Class}.

spawn_instance(Class, InstanceId) ->
    case ets:lookup(?MODEL_TABLE, Class) of
        [] -> {error, model_not_found};
        [{Class, Model}] ->
            {ok, Pid} = dgiot_shadow:start_link(InstanceId, #{class=>Class, model=>Model, properties=>init_props(Model)}),
            ets:insert(?INST_TABLE, {InstanceId, #{class=>Class, model=>Model, pid=>Pid, status=>init}}),
            {ok, Pid}
    end.

get_model(Class) ->
    case ets:lookup(?MODEL_TABLE, Class) of [] -> {error, not_found}; [{Class, M}] -> {ok, M} end.

list_instances(Class) ->
    [Pid || {_, #{class:=C, pid:=Pid}} <- ets:tab2list(?INST_TABLE), C =:= Class].

%%====================================================================
%% 4-Layer Register
%% Site=Channel=Device have MD5 objectId; Point uses thing_model identifier
%%====================================================================
register(channel, #{id := Id} = Map) -> dgiot_parse:create_object(<<"Channel">>, Map#{"objectId"=>Id});
register(device,  #{id := Id} = Map) -> dgiot_parse:create_object(<<"Device">>,  Map#{"objectId"=>Id}),
    spawn(fun() -> get_topic(Id) end).  %% 预热topic缓存

%%====================================================================
%% MQTT Topic — dlink标准: $dg/thing/{ProductID}/{ProductID}_{DevAddr}/properties/report
%% 与 dgiot_mock_mqtt.erl + dgiot_mqtt_acl.erl 一致
%%====================================================================
get_topic(DeviceId) ->
    {ok, #{<<"productid">>:=Pid, <<"devaddr">>:=DevAddr}} = dgiot_parse:get_object(<<"Device">>, DeviceId),
    <<"$dg/thing/", Pid/binary, "/", Pid/binary, "_", DevAddr/binary, "/properties/report">>.

get_points(DeviceId) ->
    {ok, #{<<"results">>:=Points}} = dgiot_parse:query_object(<<"Point">>, #{<<"device">>=>DeviceId}), Points.

get_devices(GatewayId) ->
    {ok, #{<<"results">>:=Devices}} = dgiot_parse:query_object(<<"Device">>, #{<<"gateway">>=>GatewayId}), Devices.

%%====================================================================
%% Action: Push
%%====================================================================
push_point(DeviceId, Props) ->
    Topic = get_topic(DeviceId),
    Payload = Props#{ts => erlang:system_time(millisecond)},
    dgiot_mqtt:publish(Topic, jsx:encode(Payload)),
    {ok, Topic}.

%%====================================================================
%% Internal
%%====================================================================
init_props(#{properties := Props}) ->
    maps:from_list([{maps:get(<<"id">>, P), maps:get(<<"type">>, P)} || P <- Props]).

%%====================================================================
%% Health
%%====================================================================
health() ->
    #{version => <<"3.0">>, ontology => <<"DLAS 4-layer">>, pipeline => <<"FDE">>,
      tables => #{models=>ets:info(?MODEL_TABLE,size), instances=>ets:info(?INST_TABLE,size), rules=>ets:info(?RULES_TABLE,size)}}.
