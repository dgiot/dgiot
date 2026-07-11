%%--------------------------------------------------------------------
%% dgiot_ontology — 4层本体论引擎 (简洁版)
%%
%% 层1 Site    采油厂/井场          Class: Site
%% 层2 Gateway IO服务器/协议网关     Class: Gateway
%% 层3 Device  RTU/传感器/执行器    Class: Device
%% 层4 Point   测点                 Class: Point
%%
%% MQTT Topic: dgiot/{site}/{gateway}/{device}/{point}/data
%% Payload:    {ts, v, q}
%%--------------------------------------------------------------------
-module(dgiot_ontology).
-author("edge-hub").
-include_lib("dgiot/include/logger.hrl").

%% API — 核心 6 个函数
-export([
    register/2,         %% 注册本体节点
    get_path/1,         %% 获取 MQTT topic 路径
    get_points/1,       %% 获取设备下所有测点
    get_devices/1,      %% 获取网关下所有设备
    push_point/2,       %% 推送测点值到 MQTT
    health/0            %% 健康检查
]).

%% ——— 4 层 record ———
-record(site,    {id, name, type, location}).
-record(gateway, {id, ip, site, protocols=[], devices=[]}).
-record(device,  {id, gateway, name, type, protocol, slaveid, points=[]}).
-record(point,   {id, device, name, register, unit, range, alarm}).

%% ——— register: 注册任意层节点 ———
register(site, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Site">>, Map#{"objectId" => Id});

register(gateway, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Gateway">>, Map#{"objectId" => Id});

register(device, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Device">>, Map#{"objectId" => Id});

register(point, #{id := Id} = Map) ->
    dgiot_parse:create_object(<<"Point">>, Map#{"objectId" => Id}).

%% ——— get_path: 构建 MQTT topic ———
%% 入: PointId  出: <<"dgiot/site_01/gw_131/rtu_001/oil_pressure">>
get_path(PointId) ->
    {ok, #{<<"device">> := DevId, <<"id">> := Pid}} =
        dgiot_parse:get_object(<<"Point">>, PointId),
    {ok, #{<<"gateway">> := GwId, <<"id">> := Did}} =
        dgiot_parse:get_object(<<"Device">>, DevId),
    {ok, #{<<"site">> := SiteId}} =
        dgiot_parse:get_object(<<"Gateway">>, GwId),
    <<"dgiot/", SiteId/binary, "/", GwId/binary, "/", Did/binary, "/", Pid/binary>>.

%% ——— get_points: 设备 → 测点列表 ———
get_points(DeviceId) ->
    {ok, #{<<"results">> := Points}} =
        dgiot_parse:query_object(<<"Point">>, #{<<"device">> => DeviceId}),
    Points.

%% ——— get_devices: 网关 → 设备列表 ———
get_devices(GatewayId) ->
    {ok, #{<<"results">> := Devices}} =
        dgiot_parse:query_object(<<"Device">>, #{<<"gateway">> => GatewayId}),
    Devices.

%% ——— push_point: 推送测点值 ———
push_point(PointId, Value) ->
    Topic = get_path(PointId),
    TopicData = iolist_to_binary([Topic, <<"/data">>]),
    Payload = #{
        ts => erlang:system_time(millisecond),
        v => Value,
        q => 192  %% 质量码: 192=good
    },
    dgiot_mqtt:publish(TopicData, jsx:encode(Payload)).

%% ——— health ———
health() ->
    #{ontology => <<"4-layer: Site > Gateway > Device > Point">>,
      mqtt_topic => <<"dgiot/{site}/{gateway}/{device}/{point}/data">>}.
