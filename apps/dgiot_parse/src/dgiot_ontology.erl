%% dgiot_ontology v3.0 — DLAS 本体引擎 (完整版+优化)
-module(dgiot_ontology).
-author("edge-hub").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    init/0, register/2, get_path/1,
    get_points/1, get_devices/1, push_point/2,
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
register(site,    #{name := Name})  -> {ok, Name};  %% human-readable, no Parse
register(channel, #{id := Id} = Map) -> dgiot_parse:create_object(<<"Channel">>, Map#{"objectId"=>Id});
register(device,  #{id := Id} = Map) -> dgiot_parse:create_object(<<"Device">>,  Map#{"objectId"=>Id});
register(point,   #{id := Id} = Map) ->
    spawn(fun() -> get_path(Id) end),  %% 优化#1: 预热path缓存
    ok.

%%====================================================================
%% MQTT Path (优化#1: ETS缓存)
%% Topic: dgiot/{site}/{channel_id}/{device_id}/{point_id}/data
%%====================================================================
get_path(PointId) ->
    case ets:lookup(?PATH_CACHE, PointId) of
        [{PointId, Path}] -> Path;
        [] ->
            %% Point -> Device -> Channel -> Site
            {ok, #{<<"device">>:=DevId, <<"identifier">>:=Pid}} = dgiot_parse:get_object(<<"Point">>, PointId),
            {ok, #{<<"channel">>:=ChId}} = dgiot_parse:get_object(<<"Device">>, DevId),
            {ok, #{<<"site">>:=SiteName}} = dgiot_parse:get_object(<<"Channel">>, ChId),
            Path = <<"dgiot/", SiteName/binary, "/", ChId/binary, "/", DevId/binary, "/", Pid/binary>>,
            ets:insert(?PATH_CACHE, {PointId, Path}),
            Path
    end.

get_points(DeviceId) ->
    {ok, #{<<"results">>:=Points}} = dgiot_parse:query_object(<<"Point">>, #{<<"device">>=>DeviceId}), Points.

get_devices(GatewayId) ->
    {ok, #{<<"results">>:=Devices}} = dgiot_parse:query_object(<<"Device">>, #{<<"gateway">>=>GatewayId}), Devices.

%%====================================================================
%% Action: Push
%%====================================================================
push_point(PointId, Value) ->
    Topic = get_path(PointId),
    TopicData = iolist_to_binary([Topic, <<"/data">>]),
    dgiot_mqtt:publish(TopicData, jsx:encode(#{ts=>erlang:system_time(millisecond), v=>Value, q=>192})),
    {ok, TopicData}.

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
