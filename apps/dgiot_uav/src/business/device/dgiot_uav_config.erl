%%--------------------------------------------------------------------
%% @doc 无人机测试系统配置模块
%% 提供端口到设备信息的映射，以及工位基地址等配置。
%%--------------------------------------------------------------------
-module(dgiot_uav_config).

-include_lib("dgiot/include/logger.hrl").

-export([
    get_port_device_mapping/1,
    get_station_base_addr/1
]).

%% 根据超近距无人机测试产线设计模式：
%% 固定端口作为设备类型标识符（源端口=设备类型，所有设备连接到目标端口20000）
%% 格式: {源端口, {设备ID, 产品ID, 设备名称, 是否创建设备, 是否是舵面设备}}
-define(PORT_DEVICE_MAPPINGS, [
    {10001, {<<"wrj_dm_zqy">>, <<"de7130b0a1">>, <<"左前翼舵面"/utf8>>, true, true}},
    {10002, {<<"wrj_dm_yqy">>, <<"de7130b0a1">>, <<"右前翼舵面"/utf8>>, true, true}},
    {10003, {<<"wrj_dm_zcw">>, <<"de7130b0a1">>, <<"左侧翼舵面"/utf8>>, true, true}},
    {10004, {<<"wrj_dm_ycw">>, <<"de7130b0a1">>, <<"右侧翼舵面"/utf8>>, true, true}},
    {10005, {<<"wrj_dm_zhj">>, <<"de7130b0a1">>, <<"治具基准舵面"/utf8>>, true, true}},
    {10006, {<<"wrj_danpianji">>, <<"bd49cc8272">>, <<"单片机"/utf8>>, true, false}},
    {10007, {<<"wrj_dicekou">>, <<"6235befb62">>, <<"地测口"/utf8>>, false, false}},  % 不自动创建设备，从EB90帧提取飞机ID后创建
    {1234,  {<<"scanner">>, <<"1893e1feb3">>, <<"扫描枪"/utf8>>, true, false}},
    {21000, {<<"noise_sensor">>, <<"51f2902af3">>, <<"噪音传感器"/utf8>>, true, false}}
]).

%% 工位地址到基地址映射
-define(STATION_BASE_ADDR_MAP, #{
    0 => <<"D1700">>,   % 磁航向
    1 => <<"D1600">>,   % 总测工位2
    2 => <<"D1600">>,   % 总测工位2-动力检测
    3 => <<"D1500">>,   % 总测工位1
    4 => <<"D1500">>,   % 总测工位1-动力检测
    5 => <<"D1300">>,   % 拷机工位2
    6 => <<"D1200">>,   % 拷机工位1
    7 => <<"D1100">>,   % 桁行架
    255 => <<"未知">>   % 上料台
}).

%% @doc 根据端口获取设备信息
get_port_device_mapping(Port) ->
    case lists:keyfind(Port, 1, ?PORT_DEVICE_MAPPINGS) of
        {Port, {DeviceId, ProductId, Name, ShouldCreate, IsSurface}} ->
            {DeviceId, ProductId, Name, ShouldCreate, IsSurface};
        false ->
            undefined
    end.

%% @doc 根据工位地址获取基地址
get_station_base_addr(StationAddr) ->
    maps:get(StationAddr, ?STATION_BASE_ADDR_MAP, <<"未知">>).