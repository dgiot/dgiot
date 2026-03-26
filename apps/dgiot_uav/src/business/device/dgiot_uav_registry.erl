%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_registry - 进程注册表
%%% 维护 IP端口 ↔ PID、无人机ID ↔ PID 的映射，以及工位相关进程的映射。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_registry).

-include_lib("dgiot/include/logger.hrl").

%% API
-export([register_ip_port/5, unregister_ip_port/2, lookup_ip_port/2,
         register_drone/2, unregister_drone/1, lookup_drone/1,
         register_station_plc/2, unregister_station_plc/1, get_station_plc/1,
         register_station_fixture/2, unregister_station_fixture/1, get_station_fixture/1,
         init_tables/0]).

%% 初始化所有 ETS 表
init_tables() ->
    lists:foreach(fun(Table) ->
        case ets:info(Table) of
            undefined -> ets:new(Table, [set, public, named_table, {keypos, 1}, {heir, none}]);
            _ -> ok
        end
    end, [uav_ip_port_info, uav_drone_worker, uav_station_plc, uav_station_fixture]).

%% IP端口映射
register_ip_port(Pid, IpBin, Port, LoginId, ProductId) ->
    %% 确保ETS表存在
    case ets:info(uav_ip_port_info) of
        undefined -> 
            init_tables();
        _ -> 
            ok
    end,
    %% 插入或更新记录
    ets:insert(uav_ip_port_info, {{IpBin, Port}, #{pid => Pid, device_id => LoginId, product_id => ProductId}}),
    ?LOG(error, "【设备IP端口注册】IP=~s:~p, DeviceId=~s, Pid=~p", [IpBin, Port, LoginId, Pid]).

unregister_ip_port(IpBin, Port) ->
    ets:delete(uav_ip_port_info, {IpBin, Port}).

lookup_ip_port(IpBin, Port) ->
    case ets:lookup(uav_ip_port_info, {IpBin, Port}) of
        [{_, Info}] -> {ok, Info};
        [] -> {error, not_find}
    end.

%% 无人机ID映射
register_drone(DroneId, Pid) ->
    ets:insert(uav_drone_worker, {DroneId, Pid}),
    ?LOG(error, "【无人机注册成功】DroneId=~s, Pid=~p", [DroneId, Pid]).

unregister_drone(DroneId) ->
    ets:delete(uav_drone_worker, DroneId),
    ?LOG(error, "【无人机注销】DroneId=~s", [DroneId]).

lookup_drone(DroneId) ->
    case ets:lookup(uav_drone_worker, DroneId) of
        [{DroneId, Pid}] -> {ok, Pid};
        [] -> {error, not_find}
    end.

%% 工位PLC进程映射
register_station_plc(StationId, Pid) ->
    ets:insert(uav_station_plc, {StationId, Pid}).

unregister_station_plc(StationId) ->
    ets:delete(uav_station_plc, StationId).

get_station_plc(StationId) ->
    case ets:lookup(uav_station_plc, StationId) of
        [{StationId, Pid}] -> {ok, Pid};
        [] -> {error, not_find}
    end.

%% 工位治具进程映射
register_station_fixture(StationId, Pid) ->
    ets:insert(uav_station_fixture, {StationId, Pid}).

unregister_station_fixture(StationId) ->
    ets:delete(uav_station_fixture, StationId).

get_station_fixture(StationId) ->
    case ets:lookup(uav_station_fixture, StationId) of
        [{StationId, Pid}] -> {ok, Pid};
        [] -> {error, not_find}
    end.