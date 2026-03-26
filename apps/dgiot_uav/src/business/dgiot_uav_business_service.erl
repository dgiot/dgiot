%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_business_service 模块 - 无人机业务逻辑服务（总入口）
%%% 聚合各子模块功能，提供统一的对外接口。
%%% 实际实现转发至：
%%%   - dgiot_uav_registry      : 进程注册表
%%%   - dgiot_uav_device_manager : 设备生命周期管理
%%%   - dgiot_uav_station_manager: 工位信息管理
%%%   - dgiot_uav_aggregator     : 数据汇聚存储
%%%
%%% 此模块作为业务层的统一外观，封装底层细节，供外部调用。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_business_service).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-dgiot_data("ets").

-on_load(on_load/0).

%% API 导出（按功能分组）
-export([
    %% 设备注册与状态
    register_device_to_ip/4,
    register_device_to_ip/5,
    complete_station_registration/5,
    get_device_info_by_id/1,
    get_device_info_by_ip/1,
    get_fixture_addr_by_ip/1,
    create_device/5,
    update_device_name/2,

    %% IP分类
    is_shared_ip/1,
    is_dedicated_ip/1,

    %% 聚合注册表（委托至 dgiot_uav_registry）
    register_drone_worker/2,
    unregister_drone_worker/1,
    lookup_drone_worker/1,
    register_station_worker/3,
    unregister_station_worker/1,
    lookup_station_worker/1,

    %% IP端口映射（委托至 dgiot_uav_registry）
    register_ip_port/5,
    update_device_id/3,
    unregister_ip_port/2,
    get_pid_by_ip_port/2,
    get_device_id_by_ip_port/2,
    get_product_id_by_ip_port/2,
    get_full_info_by_ip_port/2,

    %% 工位IP映射（委托至 dgiot_uav_station_manager）
    set_ip_station/2,
    get_station_by_ip/1,
    bind_station_drone/2,
    unbind_station_drone/1,
    get_drone_by_station/1,
    get_drone_id_by_ip/1,
    get_ip_by_station/1,
    set_station_ip/2,

    %% 工位进程映射（委托至 dgiot_uav_registry）
    register_station_plc/2,
    unregister_station_plc/1,
    get_station_plc/1,
    register_station_fixture/2,
    unregister_station_fixture/1,
    get_station_fixture/1,
    set_station_drone/2,
    get_station_drone/1,

    %% 无人机与工位绑定（委托至 dgiot_uav_station_manager）
    bind_uav_to_station/2,
    get_station_by_uav/1,

    %% 单片机初始化检查（委托至 dgiot_uav_station_manager）
    is_fixture_completed/1,

    %% 数据汇聚（委托至 dgiot_uav_aggregator）
    send_aggregate_to_drone/2,
    get_latest_state/2,

    %% 测试与初始化
    test/0,
    init_ets/0
]).

%% 内部函数
-export([ensure_tdengine_subtable/2]).

-define(TD_TYPE, <<"TD">>).

%%%===================================================================
%%% on_load 钩子：模块加载时初始化 ETS 表
%%%===================================================================

on_load() ->
    init_ets(),
    ok.

%%%===================================================================
%%% ETS 表初始化（兼容旧表 + 新模块所需表）
%%%===================================================================

init_ets() ->
    %% 原有 ETS 表（保持兼容）
    dgiot_data:init(uav_device_registration),
    dgiot_data:init(uav_ip_station_mapping),
    dgiot_data:init(uav_station_ip_mapping),
    dgiot_data:init(uav_station_binding),
    create_table_if_not_exists(uav_aggregate_cache, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_drone_worker, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_worker, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_ip_port_info, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_drone, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_latest_state, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_plc, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_fixture, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_drone_id, [set, public, named_table, {keypos, 1}, {heir, none}]),
    create_table_if_not_exists(uav_station_info, [set, public, named_table, {keypos, 1}, {heir, none}]),

    %% 新模块所需表（如果尚未创建）
    catch dgiot_uav_registry:init_tables(),   % 确保注册表存在，忽略可能未加载的错误
    ?LOG(info, "业务层ETS表初始化完成").

create_table_if_not_exists(Name, Opts) ->
    case ets:info(Name) of
        undefined -> 
            ets:new(Name, Opts);
        Info when is_list(Info) ->
            %% 检查现有表的类型是否匹配
            ExistingType = proplists:get_value(type, Info, set),
            case lists:member(type, Opts) of
                true ->
                    WantedType = case lists:keyfind(type, 1, Opts) of
                        {type, T} -> T;
                        _ -> set
                    end,
                    case ExistingType of
                        WantedType -> ok;
                        _ ->
                            %% 类型不匹配，删除并重建
                            ets:delete(Name),
                            ets:new(Name, Opts)
                    end;
                false ->
                    ok
            end
    end.

%%%===================================================================
%%% 设备注册与状态（兼容旧接口）
%%%===================================================================

register_device_to_ip(DeviceId, IpStr, SensorAddr, Status) when is_list(IpStr) ->
    register_device_to_ip(DeviceId, IpStr, 0, SensorAddr, Status, undefined).

register_device_to_ip(DeviceId, IpStr, Port, SensorAddr, Status) when is_list(IpStr), is_integer(Port) ->
    register_device_to_ip(DeviceId, IpStr, Port, SensorAddr, Status, undefined).

register_device_to_ip(DeviceId, IpStr, Port, SensorAddr, Status, StationId) when is_list(IpStr), is_integer(Port) ->
    BinIp = list_to_binary(IpStr),
    Registration = #{
        device_id => DeviceId,
        ip => BinIp,
        port => Port,
        sensor_addr => SensorAddr,
        registered_at => erlang:system_time(millisecond),
        status => Status,
        station_id => StationId,
        updated_at => erlang:system_time(millisecond)
    },
    dgiot_data:insert(uav_device_registration, DeviceId, Registration),
    ?LOG(info, "设备注册到IP: ~ts:~p, 传感器地址=~p, 状态=~p, 工位=~p", [DeviceId, IpStr, Port, SensorAddr, Status, StationId]).

complete_station_registration(IpStr, FixtureAddress, StationId, StationName, BaseAddress) when is_list(IpStr) ->
    BinIp = list_to_binary(IpStr),
    Mapping = #{
        fixture_address => FixtureAddress,
        station_id => StationId,
        station_name => StationName,
        base_address => BaseAddress,
        bound_at => erlang:system_time(millisecond),
        ip => BinIp
    },
    dgiot_data:insert(uav_ip_station_mapping, BinIp, Mapping),
    update_devices_status_by_ip(BinIp, completed),
    ?LOG(info, "第二阶段注册完成: ~ts → 治具地址~p → 工位~p (~ts)",
         [IpStr, FixtureAddress, StationId, StationName]).

update_devices_status_by_ip(BinIp, NewStatus) when is_binary(BinIp) ->
    case dgiot_data:get(uav_device_registration) of
        not_find -> ok;
        _ ->
            AllDevices = ets:tab2list(uav_device_registration),
            lists:foreach(fun({DeviceId, Registration}) ->
                case maps:get(ip, Registration, undefined) of
                    BinIp ->
                        Updated = Registration#{status => NewStatus, updated_at => erlang:system_time(millisecond)},
                        dgiot_data:insert(uav_device_registration, DeviceId, Updated);
                    _ -> ok
                end
            end, AllDevices)
    end.

get_device_info_by_id(DeviceId) ->
    case dgiot_data:lookup(uav_device_registration, DeviceId) of
        {ok, Info} -> {ok, Info};
        {error, not_find} -> {error, not_find}
    end.

get_device_info_by_ip(Ip) ->
    BinIp = if is_list(Ip) -> list_to_binary(Ip); is_binary(Ip) -> Ip; true -> error(badarg) end,
    case dgiot_data:get(uav_device_registration) of
        not_find -> {ok, []};
        _ ->
            All = ets:tab2list(uav_device_registration),
            Matching = lists:filtermap(fun({Id, Reg}) ->
                case maps:get(ip, Reg, undefined) of
                    BinIp -> {true, #{device_id => Id, info => Reg}};
                    _ -> false
                end
            end, All),
            {ok, Matching}
    end.

get_fixture_addr_by_ip(IpBin) ->
    case dgiot_data:lookup(uav_ip_station_mapping, IpBin) of
        {ok, #{fixture_address := Addr}} -> {ok, Addr};
        {error, not_find} -> {error, not_find}
    end.

create_device(LoginId, ProductId, DevAddr, Ip, ChineseName) ->
    dgiot_uav_device_manager:create_device(LoginId, ProductId, DevAddr, Ip, ChineseName).

update_device_name(DevAddr, NewName) ->
    dgiot_uav_device_manager:update_device_name(DevAddr, NewName).

%%%===================================================================
%%% IP分类
%%%===================================================================

is_shared_ip(IpStr) when is_list(IpStr) ->
    SharedIps = [
        "192.168.100.40", "192.168.100.50", "192.168.100.51",
        "192.168.100.52", "192.168.100.53", "192.168.100.54",
        "192.168.100.55", "192.168.100.56"
    ],
    lists:member(IpStr, SharedIps).

is_dedicated_ip(IpStr) ->
    not is_shared_ip(IpStr).

%%%===================================================================
%%% 聚合注册表（转发至 dgiot_uav_registry）
%%%===================================================================

register_drone_worker(DroneId, Pid) ->
    dgiot_uav_registry:register_drone(DroneId, Pid).

unregister_drone_worker(DroneId) ->
    dgiot_uav_registry:unregister_drone(DroneId).

lookup_drone_worker(DroneId) ->
    dgiot_uav_registry:lookup_drone(DroneId).

register_station_worker(StationKey, DroneId, Pid) ->
    %% 注：station_worker 表未在新模块中实现，暂时保留原逻辑
    ensure_table_exists(uav_station_worker),
    ets:insert(uav_station_worker, {StationKey, DroneId, Pid}).

unregister_station_worker(StationKey) ->
    ensure_table_exists(uav_station_worker),
    ets:delete(uav_station_worker, StationKey).

lookup_station_worker(StationKey) ->
    ensure_table_exists(uav_station_worker),
    case ets:lookup(uav_station_worker, StationKey) of
        [{StationKey, DroneId, Pid}] -> {ok, {DroneId, Pid}};
        [] -> {error, not_find}
    end.

ensure_table_exists(Table) ->
    case ets:info(Table) of
        undefined -> create_table_if_not_exists(Table, [set, public, named_table, {keypos, 1}]);
        _ -> ok
    end.

%%%===================================================================
%%% IP端口映射（转发至 dgiot_uav_registry）
%%%===================================================================

register_ip_port(Pid, Ip, Port, LoginId, ProductId) ->
    dgiot_uav_registry:register_ip_port(Pid, Ip, Port, LoginId, ProductId).

update_device_id(IpBin, Port, NewDeviceId) ->
    case dgiot_uav_registry:lookup_ip_port(IpBin, Port) of
        {ok, Info} ->
            NewInfo = Info#{device_id => NewDeviceId},
            ets:insert(uav_ip_port_info, {{IpBin, Port}, NewInfo}),
            ok;
        _ -> ok
    end.

unregister_ip_port(IpBin, Port) ->
    dgiot_uav_registry:unregister_ip_port(IpBin, Port).

get_pid_by_ip_port(IpBin, Port) ->
    case dgiot_uav_registry:lookup_ip_port(IpBin, Port) of
        {ok, #{pid := Pid}} -> {ok, Pid};
        {error, _} -> {error, not_find}
    end.

get_device_id_by_ip_port(IpBin, Port) ->
    case dgiot_uav_registry:lookup_ip_port(IpBin, Port) of
        {ok, #{device_id := DevId}} -> {ok, DevId};
        {error, _} -> {error, not_find}
    end.

get_product_id_by_ip_port(IpBin, Port) ->
    case dgiot_uav_registry:lookup_ip_port(IpBin, Port) of
        {ok, #{product_id := ProdId}} -> {ok, ProdId};
        {error, _} -> {error, not_find}
    end.

get_full_info_by_ip_port(IpBin, Port) ->
    dgiot_uav_registry:lookup_ip_port(IpBin, Port).

%%%===================================================================
%%% 工位IP映射（转发至 dgiot_uav_station_manager）
%%%===================================================================

set_ip_station(IpBin, FixtureAddr) ->
    dgiot_uav_station_manager:set_station_ip(IpBin, FixtureAddr).

get_station_by_ip(IpBin) ->
    dgiot_uav_station_manager:get_station_by_ip(IpBin).

bind_station_drone(StationAddr, DroneId) ->
    dgiot_uav_station_manager:bind_drone_to_station(StationAddr, DroneId).

unbind_station_drone(StationAddr) ->
    dgiot_uav_station_manager:unbind_drone_from_station(StationAddr).

get_drone_by_station(StationAddr) ->
    dgiot_uav_station_manager:get_drone_by_station(StationAddr).

get_drone_id_by_ip(IpBin) ->
    case get_station_by_ip(IpBin) of
        {ok, StationAddr} -> get_drone_by_station(StationAddr);
        {error, _} -> {error, not_find}
    end.

get_ip_by_station(StationAddr) ->
    dgiot_uav_station_manager:get_ip_by_station(StationAddr).

set_station_ip(StationAddr, Ip) ->
    dgiot_uav_station_manager:set_station_ip(StationAddr, Ip).

%%%===================================================================
%%% 工位进程映射（转发至 dgiot_uav_registry）
%%%===================================================================

register_station_plc(StationId, Pid) ->
    dgiot_uav_registry:register_station_plc(StationId, Pid).

unregister_station_plc(StationId) ->
    dgiot_uav_registry:unregister_station_plc(StationId).

get_station_plc(StationId) ->
    dgiot_uav_registry:get_station_plc(StationId).

register_station_fixture(StationId, Pid) ->
    dgiot_uav_registry:register_station_fixture(StationId, Pid).

unregister_station_fixture(StationId) ->
    dgiot_uav_registry:unregister_station_fixture(StationId).

get_station_fixture(StationId) ->
    dgiot_uav_registry:get_station_fixture(StationId).

set_station_drone(StationId, DroneId) ->
    ets:insert(uav_station_drone_id, {StationId, DroneId}).

get_station_drone(StationId) ->
    case ets:lookup(uav_station_drone_id, StationId) of
        [{StationId, DroneId}] -> {ok, DroneId};
        [] -> {error, not_find}
    end.

%%%===================================================================
%%% 无人机与工位绑定（转发至 dgiot_uav_station_manager）
%%%===================================================================

bind_uav_to_station(DroneId, StationInfo) ->
    dgiot_uav_station_manager:bind_drone(DroneId, StationInfo).

get_station_by_uav(DroneId) ->
    case dgiot_data:lookup(uav_station_binding, DroneId) of
        {ok, Info} -> {ok, Info};
        _ -> {error, not_find}
    end.

%%%===================================================================
%%% 单片机初始化检查（转发至 dgiot_uav_station_manager）
%%%===================================================================

is_fixture_completed(IpBin) ->
    dgiot_uav_station_manager:is_fixture_completed(IpBin).

%%%===================================================================
%%% 数据汇聚（转发至 dgiot_uav_aggregator）
%%%===================================================================

send_aggregate_to_drone(DroneId, Data) when is_binary(DroneId) ->
    ?LOG(info, "========================================", []),
    ?LOG(info, "[BUSINESS] ========== 收到汇聚请求 ==========", []),
    ?LOG(info, "[BUSINESS] DroneId: ~s", [DroneId]),
    ?LOG(info, "[BUSINESS] 数据字段数: ~p", [maps:size(Data)]),
    ?LOG(info, "[BUSINESS] 数据字段: ~p", [maps:keys(Data)]),
    ?LOG(info, "========================================", []),

    % 通过DroneId查找对应的IP，然后发送aggregate消息
    case dgiot_uav_station_manager:get_ip_by_drone(DroneId) of
        {ok, IpBin} ->
            ?LOG(info, "[BUSINESS] ✅ 找到IP: ~s", [IpBin]),
            case get_pid_by_ip_port(IpBin, 10007) of
                {ok, DronePid} ->
                    ?LOG(info, "[BUSINESS] ✅ 找到无人机进程: ~p", [DronePid]),
                    DronePid ! {aggregate, DroneId, Data},
                    ?LOG(info, "[BUSINESS] ✅ 发送 aggregate 消息成功: DroneId=~s, IP=~s, 字段数=~p", [DroneId, IpBin, maps:size(Data)]),
                    ?LOG(info, "[BUSINESS] ========== 发送完成 ==========", []),
                    ?LOG(info, "========================================", []),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[BUSINESS] ❌ 未找到无人机进程，DroneId: ~s, IP: ~s, 原因: ~p", [DroneId, IpBin, Reason]),
                    ?LOG(info, "[BUSINESS] ========== 发送失败 ==========", []),
                    ?LOG(info, "========================================", []),
                    ok
            end;
        {error, Reason} ->
            ?LOG(error, "[BUSINESS] ❌ 未找到无人机IP，DroneId: ~s, 原因: ~p", [DroneId, Reason]),
            ?LOG(info, "[BUSINESS] ========== 发送失败 ==========", []),
            ?LOG(info, "========================================", []),
            ok
    end;

send_aggregate_to_drone(IpBin, Data) when is_binary(IpBin) ->
    % 兼容旧版本：直接通过IP发送（不含DroneId）
    case get_pid_by_ip_port(IpBin, 10007) of
        {ok, DronePid} ->
            DronePid ! {aggregate, Data},
            ok;
        {error, _} ->
            ?LOG(error, "[BUSINESS DEBUG] 未找到无人机进程，IP: ~s，端口: 10007", [IpBin]),
            ok
    end.

get_latest_state(ProductId, DevAddr) ->
    ensure_table_exists(uav_latest_state),
    Key = {ProductId, DevAddr},
    case ets:lookup(uav_latest_state, Key) of
        [{Key, _Timestamp, Data}] -> {ok, Data};
        [] -> {error, not_find}
    end.

%%%===================================================================
%%% 测试函数
%%%===================================================================

test() ->
    ?LOG(info, "开始测试业务层功能"),
    init_ets(),
    test_ip_classification(),
    ok.

test_ip_classification() ->
    Shared = ["192.168.100.40","192.168.100.45","192.168.100.56"],
    lists:foreach(fun(Ip) -> true = is_shared_ip(Ip), false = is_dedicated_ip(Ip) end, Shared),
    Dedicated = ["192.168.100.20","192.168.100.21"],
    lists:foreach(fun(Ip) -> false = is_shared_ip(Ip), true = is_dedicated_ip(Ip) end, Dedicated),
    ?LOG(info, "IP分类测试通过").

%%%===================================================================
%%% 内部辅助函数（ensure_tdengine_subtable 仍保留）
%%%===================================================================

ensure_tdengine_subtable(ProductId, DevAddr) ->
    ?LOG(info, "TODO: 实现TDengine子表创建功能 ProductId=~p, DevAddr=~p", [ProductId, DevAddr]),
    %% TODO: 暂时返回ok，待dgiot_uav_aggregator模块实现ensure_tdengine_subtable/2函数
    ok.