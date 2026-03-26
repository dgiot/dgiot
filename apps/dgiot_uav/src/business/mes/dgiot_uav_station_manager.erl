%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_station_manager - 工位信息管理
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_station_manager).

%% ETS自动初始化
-dgiot_data("ets").

-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

-export([bind_drone_to_station/2, unbind_drone_from_station/1, get_drone_by_station/1,
         get_station_by_drone/1, get_ip_by_drone/1, get_drone_by_ip/1,
         set_station_ip/2, get_station_by_ip/1, get_ip_by_station/1,
         register_fixture_completion/2, is_fixture_completed/1,
         drone_powered/3, bind_drone/2,
         trigger_mes_report_for_connected_drone/1, trigger_mes_report_for_connected_drone/2, cleanup_expired_ip_mappings/0,
         test_drone_station_binding/0, test_address_mapping/0]).

%% 工位定义和查询
-export([init_ets/0, get_station_by_fixture/1, get_station_by_ip_and_fixture/2,
         cache_station_ip/3, get_station_table_name/1, get_station_info_by_name/1]).
%% 工位创建和管理
-export([create_station/1, create_station/6, update_station/2, delete_station/1, 
         list_all_stations/0, get_station_by_name/1, search_stations/1]).
%% 真实工位同步和设备创建
-export([sync_real_stations/0, create_real_station_device/6, 
         get_station_by_devaddr/1, devaddr_to_fixture_addr/1,
         sync_station_from_device/2, initialize_real_stations/0]).
%% 虚拟工位管理
-export([create_virtual_stations/0, create_virtual_alarm_station/0, 
         create_virtual_heartbeat_station/0, list_virtual_stations/0,
         is_virtual_station/1, delete_virtual_stations/0]).
%% 工位检查和诊断
-export([check_station_status/1, check_all_stations/0, diagnose_station_issues/1]).
%% 地址段映射功能
-export([get_station_by_address/1, get_address_ranges/0, 
         validate_address_for_station/2, map_alarm_to_station/2,
         init_address_mappings/0, reload_address_mappings/0]).

%% 工位测试数据缓存（每个工位一个ETS表，每个指标一个key）
-export([cache_station_test_data/2, cache_station_metric/3, 
         get_station_test_data/1, get_station_metric/2, get_station_metric_with_ts/2,
         cache_qrcode_to_station/2, cache_drone_online_to_station/2,
         get_qrcode_serial_from_station/1, get_drone_online_from_station/1]).

%% 测试函数
-export([test/0]).

%% 定义产品ID宏（如果头文件中未定义，此处提供默认值）
-ifndef(UAV_PRODUCT_ID).
-define(UAV_PRODUCT_ID, <<"6235befb62">>).
-endif.

%% 无人机与工位绑定（工位地址 -> 无人机ID）
-spec bind_drone_to_station(integer(), binary()) -> ok.
bind_drone_to_station(StationAddr, DroneId) ->
    ?LOG(info, <<"绑定无人机 ~s 到工位 ~p"/utf8>>, [DroneId, StationAddr]),
    %% 存储绑定关系
    true = ets:insert(uav_station_drone, {StationAddr, DroneId}),
    %% 反向索引（使用 uav_station_binding 保持系统一致性）
    true = dgiot_data:insert(uav_station_binding, DroneId, StationAddr),

    %% 获取无人机设备信息（将整数ID转换为二进制设备地址）
    DroneDevAddr = case is_integer(DroneId) of
        true -> integer_to_binary(DroneId);
        false -> DroneId
    end,
    ProductId = ?UAV_PRODUCT_ID,

    %% 调用扫描枪模块，检查并绑定该工位的待处理二维码
    _ = dgiot_scanner_protocol:bind_pending_qrcode(StationAddr, ProductId, DroneDevAddr),

    %% 检查工位是否有治具进程，如果有则触发关联事件（无人机后上线场景）
    case get_ip_by_station(StationAddr) of
        {ok, IpBin} ->
            ?LOG(info, "工位 ~p 对应IP ~s，触发后上线者关联事件", [StationAddr, IpBin]),
            %% 更新IP到无人机的直接映射缓存
            true = ets:insert(uav_ip_drone, {IpBin, DroneId}),
            ?LOG(debug, "更新IP到无人机映射缓存: IP=~s -> 无人机ID=~s", [IpBin, DroneId]),
            trigger_mes_report_for_connected_drone(IpBin, undefined);
        {error, not_find} ->
            ?LOG(debug, "工位 ~p 尚未设置IP映射，跳过关联事件触发", [StationAddr])
    end,
    ok.

-spec unbind_drone_from_station(integer()) -> ok.
unbind_drone_from_station(StationAddr) ->
    case ets:lookup(uav_station_drone, StationAddr) of
        [{StationAddr, DroneId}] ->
            ets:delete(uav_station_drone, StationAddr),
            dgiot_data:delete(uav_station_binding, DroneId),
            %% 删除IP到无人机的直接映射缓存
            case get_ip_by_station(StationAddr) of
                {ok, IpBin} ->
                    ets:delete(uav_ip_drone, IpBin),
                    ?LOG(debug, "删除IP到无人机映射缓存: IP=~s", [IpBin]);
                {error, not_find} ->
                    ok
            end;
        [] -> ok
    end.

-spec get_drone_by_station(integer()) -> {ok, binary()} | {error, not_find}.
get_drone_by_station(StationAddr) ->
    case ets:lookup(uav_station_drone, StationAddr) of
        [{StationAddr, DroneId}] -> {ok, DroneId};
        [] -> {error, not_find}
    end.

-spec get_station_by_drone(binary()) -> {ok, integer()} | {error, not_find}.
get_station_by_drone(DroneId) ->
    case dgiot_data:lookup(uav_station_binding, DroneId) of
        {ok, StationAddr} -> {ok, StationAddr};
        _ -> {error, not_find}
    end.

%% IP to station mapping (with timestamp)
-define(IP_CACHE_EXPIRE, 20000).  %% 20 seconds cache expiration



-spec set_station_ip(binary(), integer()) -> ok.
set_station_ip(IpBin, StationAddr) ->
    Now = erlang:system_time(millisecond),
    ets:insert(uav_ip_station_mapping, {IpBin, {Now, StationAddr}}),
    ?LOG(debug, <<"设置工位IP映射: IP=~s -> 工位=~p (时间戳:~p)"/utf8>>, [IpBin, StationAddr, Now]).

-spec get_station_by_ip(binary()) -> {ok, integer()} | {error, not_find}.
get_station_by_ip(IpBin) ->
    try
        case ets:lookup(uav_ip_station_mapping, IpBin) of
            [{IpBin, {Timestamp, StationAddr}}] -> 
                Now = erlang:system_time(millisecond),
                if Now - Timestamp =< ?IP_CACHE_EXPIRE ->
                       {ok, StationAddr};
                   true ->
                       ?LOG(warning, "工位IP映射已过期: IP=~s, 工位=~p, 过期时间:~pms", 
                            [IpBin, StationAddr, Now - Timestamp - ?IP_CACHE_EXPIRE]),
                       ets:delete(uav_ip_station_mapping, IpBin),
                       {error, expired}
                end;
            [{IpBin, Mapping}] when is_map(Mapping) ->
                %% 处理map格式的数据（支持二进制和原子两种key格式）
                StationId = maps:get(<<"station_id">>, Mapping, maps:get(station_id, Mapping, undefined)),
                case StationId of
                    undefined ->
                        ?LOG(error, "工位IP映射中缺少station_id字段: ~p", [Mapping]),
                        {error, not_find};
                    StationId ->
                        BoundAt = maps:get(<<"bound_at">>, Mapping, maps:get(bound_at, Mapping, erlang:system_time(millisecond))),
                        Now = erlang:system_time(millisecond),
                        if Now - BoundAt =< ?IP_CACHE_EXPIRE ->
                               {ok, StationId};
                           true ->
                               ?LOG(warning, "工位IP映射已过期(map格式): IP=~s, 工位=~p, 过期时间:~pms", 
                                    [IpBin, StationId, Now - BoundAt - ?IP_CACHE_EXPIRE]),
                               ets:delete(uav_ip_station_mapping, IpBin),
                               {error, expired}
                        end
                end;
            [] -> {error, not_find};
            Other ->
                ?LOG(error, "工位IP映射数据格式异常: IP=~s, 数据: ~p", [IpBin, Other]),
                {error, invalid_format}
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "获取工位IP映射异常: IP=~s, 错误: ~p, 堆栈: ~p", [IpBin, Error, Stack]),
            {error, exception}
    end.

-spec get_ip_by_station(integer()) -> {ok, binary()} | {error, not_find}.
get_ip_by_station(StationAddr) ->
    ets:foldl(fun({Ip, Addr}, _Acc) when Addr == StationAddr -> {ok, Ip};
                 ({Ip, Mapping}, _Acc) when is_map(Mapping) ->
                      case maps:get(station_id, Mapping, undefined) of
                          StationAddr -> {ok, Ip};
                          _ -> {error, not_find}
                      end;
                 (_, _Acc) -> {error, not_find}
              end, {error, not_find}, uav_ip_station_mapping).

%% 通过无人机ID获取对应的IP地址
-spec get_ip_by_drone(binary()) -> {ok, binary()} | {error, not_find}.
get_ip_by_drone(DroneId) ->
    case get_station_by_drone(DroneId) of
        {ok, StationAddr} ->
            case get_ip_by_station(StationAddr) of
                {ok, IpBin} -> {ok, IpBin};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 通过IP地址获取对应的无人机ID
-spec get_drone_by_ip(binary()) -> {ok, binary()} | {error, not_find}.
get_drone_by_ip(IpBin) ->
    %% 1. 通过IP获取工位地址（检查过期）
    case get_station_by_ip(IpBin) of
        {ok, StationAddr} ->
            %% 2. 通过工位地址获取无人机ID
            case get_drone_by_station(StationAddr) of
                {ok, DroneId} -> {ok, DroneId};
                {error, not_find} -> 
                    ?LOG(debug, "IP ~s 对应工位 ~p 尚未绑定无人机", [IpBin, StationAddr]),
                    {error, not_find};
                {error, Reason} -> {error, Reason}
            end;
        {error, expired} ->
            ?LOG(warning, "IP ~s 的工位映射已过期，无法获取无人机", [IpBin]),
            {error, expired};
        {error, not_find} ->
            ?LOG(debug, "IP ~s 未找到对应的工位映射", [IpBin]),
            {error, not_find};
        {error, Reason} -> {error, Reason}
    end.

%% 治具完成标志
-spec register_fixture_completion(binary(), integer()) -> ok.
register_fixture_completion(IpBin, StationAddr) ->
    ets:insert(uav_fixture_completed, {IpBin, StationAddr}).

-spec is_fixture_completed(binary()) -> boolean().
is_fixture_completed(IpBin) ->
    ets:member(uav_fixture_completed, IpBin).

%% 处理 drone_powered 事件
-spec drone_powered(binary(), binary(), integer()) -> ok.
drone_powered(LoginId, IpStr, StationAddr) ->
    ?LOG(info, "无人机上电: LoginId=~s, IP=~s, StationAddr=~p", [LoginId, IpStr, StationAddr]),
    ok.

%% 处理 bind_station 事件
-spec bind_drone(binary(), map()) -> ok.
bind_drone(DroneId, StationInfo) ->
    StationAddr = maps:get(fixture_address, StationInfo),
    bind_drone_to_station(StationAddr, DroneId).

%%%===================================================================
%%% 工位定义和ETS缓存（9个工位，每个工位独特业务）
%%%===================================================================

%% 工位定义 - 按治具工位地址排序
-define(STATIONS, [
    %% {治具工位地址, 英文名, 工位名称, IP, 基地址, 业务类型}
    {1,  station_test_2,            <<"总测工位2"/utf8>>,                <<"192.168.100.40">>, <<"D1600">>, <<"性能测试"/utf8>>},
    {2,  station_test_2_power,      <<"总测工位2-动力检测"/utf8>>,       <<"192.168.100.40">>, <<"D1600">>, <<"动力检测"/utf8>>},
    {3,  station_test_1,            <<"总测工位1"/utf8>>,                <<"192.168.100.40">>, <<"D1500">>, <<"性能测试"/utf8>>},
    {4,  station_test_1_power,      <<"总测工位1-动力检测"/utf8>>,       <<"192.168.100.40">>, <<"D1500">>, <<"动力检测"/utf8>>},
    {5,  station_burn_in_2,         <<"拷机工位2"/utf8>>,                <<"192.168.100.40">>, <<"D1300">>, <<"拷机测试"/utf8>>},
    {6,  station_burn_in_1,         <<"拷机工位1"/utf8>>,                <<"192.168.100.40">>, <<"D1200">>, <<"拷机测试"/utf8>>},
    {7,  station_gantry,            <<"桁行架"/utf8>>,                    <<"192.168.100.40">>, <<"D1100">>, <<"结构测试"/utf8>>},
    {255, station_feeding_table,     <<"上料台"/utf8>>,                   undefined,            undefined,   <<"物料管理"/utf8>>},
    {0,  station_magnetic_heading,  <<"磁航向工位"/utf8>>,               <<"192.168.100.20">>, <<"D1700">>, <<"扫码绑定"/utf8>>}
]).

%% 磁航向工位常量（扫码枪专用）
-define(MAGNETIC_STATION, station_magnetic_heading).
-define(MAGNETIC_STATION_IP_20, <<"192.168.100.20">>).   %% 磁航向工位IP
-define(MAGNETIC_STATION_IP_21, <<"192.168.100.21">>).   %% 磁航向工位DTU IP

%% @doc ETS初始化函数（由-dgiot_data("ets")自动调用，也由应用启动时显式调用）
-spec init_ets() -> ok.
init_ets() ->
    ?LOG(info, "开始初始化工位管理器ETS表"),
    
    %% 初始化工位注册表（用于动态工位管理）
    dgiot_data:init(station_registry, [public, named_table, set,
                                      {write_concurrency, true},
                                      {read_concurrency, true},
                                      {keypos, 1}]),
    
    %% 初始化工位列表（将静态列表存储到ETS），使用insert_new避免重复
    case ets:insert_new(station_registry, {station_list, ?STATIONS}) of
        true ->
            ?LOG(info, "工位静态列表已初始化到ETS");
        false ->
            ?LOG(info, "工位静态列表已在ETS中存在，跳过重复初始化")
    end,
    
    %% 初始化所有工位的ETS表
    lists:foreach(fun({FixtureAddr, StationNameEn, _StationNameCn, _Ip, _BaseAddr, _BusinessType}) ->
        TableName = get_station_table_name(StationNameEn),
        dgiot_data:init(TableName, [public, named_table, set,
                                   {write_concurrency, true},
                                   {read_concurrency, true},
                                   {keypos, 1}]),
        ?LOG(debug, "工位ETS表初始化: ~p -> ~p (治具地址: ~p)", 
             [StationNameEn, TableName, FixtureAddr])
    end, ?STATIONS),
    
    %% 初始化工位IP映射缓存
    dgiot_data:init(uav_station_ip_cache, [public, named_table, set,
                                          {write_concurrency, true},
                                          {read_concurrency, true},
                                          {keypos, 1}]),
    
    %% 初始化IP到无人机映射表
    dgiot_data:init(uav_ip_drone, [public, named_table, set,
                                  {write_concurrency, true},
                                  {read_concurrency, true},
                                  {keypos, 1}]),
    
    %% 初始化治具完成标志表
    dgiot_data:init(uav_fixture_completed, [public, named_table, set,
                                            {write_concurrency, true},
                                            {read_concurrency, true},
                                            {keypos, 1}]),
    
    %% 新增：初始化地址映射ETS表
    init_address_mappings(),
    
    ?LOG(info, "所有工位ETS表初始化完成"),
    ok.

%% @doc 获取工位ETS表名
-spec get_station_table_name(atom()) -> atom().
get_station_table_name(StationNameEn) ->
    StationNameEn.

%% @doc 通过治具地址获取工位英文名
-spec get_station_by_fixture(integer()) -> {ok, atom()} | {error, not_find}.
get_station_by_fixture(FixtureAddr) ->
    case lists:keyfind(FixtureAddr, 1, ?STATIONS) of
        {FixtureAddr, StationNameEn, _, _, _, _} -> {ok, StationNameEn};
        false -> {error, not_find}
    end.

%% @doc 通过IP和治具地址获取工位英文名
-spec get_station_by_ip_and_fixture(binary(), integer()) -> {ok, atom()} | {error, not_find}.
get_station_by_ip_and_fixture(IpBin, FixtureAddr) ->
    case IpBin of
        <<"192.168.100.20">> -> {ok, station_magnetic_heading};
        <<"192.168.100.21">> -> {ok, station_magnetic_heading};
        <<"192.168.100.40">> -> 
            %% 共享IP，通过治具地址区分
            get_station_by_fixture(FixtureAddr);
        _ -> {error, not_find}
    end.

%% @doc 缓存工位IP映射
-spec cache_station_ip(binary(), integer(), atom()) -> ok.
cache_station_ip(IpBin, FixtureAddr, StationNameEn) ->
    ets:insert(uav_station_ip_cache, {IpBin, FixtureAddr, StationNameEn}),
    ?LOG(debug, "缓存工位IP映射: IP=~s, FixtureAddr=~p -> ~p", 
         [IpBin, FixtureAddr, StationNameEn]).

%%%===================================================================
%%% 工位测试数据缓存（每个工位一个ETS表，每个指标一个key）
%%%===================================================================

%% 指标命名前缀
-define(METRIC_QRCODE, <<"qrcode">>).                 %% 二维码相关
-define(METRIC_DRONE, <<"drone">>).                  %% 无人机相关  
-define(METRIC_VOLTAGE, <<"voltage">>).              %% 电压相关
-define(METRIC_CURRENT, <<"current">>).              %% 电流相关
-define(METRIC_TEMPERATURE, <<"temperature">>).      %% 温度相关
-define(METRIC_PRESSURE, <<"pressure">>).            %% 压力相关
-define(METRIC_TEST, <<"test">>).                    %% 测试状态

%% @doc 缓存工位测试数据（每个指标一个key）
%% 数据格式: {MetricKey, {Timestamp, MetricValue}}
-spec cache_station_test_data(atom(), map()) -> ok.
cache_station_test_data(StationNameEn, TestData) ->
    Now = erlang:system_time(millisecond),
    
    %% 将map中的每个指标作为单独的key存储
    maps:fold(fun(Key, Value, _Acc) ->
        ets:insert(StationNameEn, {Key, {Now, Value}}),
        ?LOG(debug, "工位 ~p 指标 ~s 已缓存", [StationNameEn, Key])
    end, ok, TestData),
    
    ?LOG(info, "工位 ~p 测试数据已缓存，指标数量: ~p", [StationNameEn, maps:size(TestData)]),
    ok.

%% @doc 缓存工位单个指标
-spec cache_station_metric(atom(), binary(), term()) -> ok.
cache_station_metric(StationNameEn, MetricKey, MetricValue) ->
    Now = erlang:system_time(millisecond),
    ets:insert(StationNameEn, {MetricKey, {Now, MetricValue}}),
    ?LOG(debug, "工位 ~p 指标 ~s 已缓存: ~p", [StationNameEn, MetricKey, MetricValue]).

%% @doc 获取工位测试数据（获取所有指标）
-spec get_station_test_data(atom()) -> {ok, map()} | {error, not_find}.
get_station_test_data(StationNameEn) ->
    case ets:info(StationNameEn) of
        undefined -> {error, not_find};
        _ ->
            %% 遍历表，构建map（包含时间戳）
            Data = ets:foldl(fun({Key, {Ts, Value}}, Acc) ->
                maps:put(Key, #{value => Value, timestamp => Ts}, Acc)
            end, #{}, StationNameEn),
            
            case maps:size(Data) of
                0 -> {error, not_find};
                _ -> {ok, Data}
            end
    end.

%% @doc 获取工位特定指标
-spec get_station_metric(atom(), binary()) -> {ok, term()} | {error, not_find}.
get_station_metric(StationNameEn, MetricKey) ->
    case ets:lookup(StationNameEn, MetricKey) of
        [] -> {error, not_find};
        [{MetricKey, {_Timestamp, Value}}] -> {ok, Value}
    end.

%% @doc 获取工位特定指标（带时间戳）
-spec get_station_metric_with_ts(atom(), binary()) -> {ok, #{value => term(), timestamp => integer()}} | {error, not_find}.
get_station_metric_with_ts(StationNameEn, MetricKey) ->
    case ets:lookup(StationNameEn, MetricKey) of
        [] -> {error, not_find};
        [{MetricKey, {Timestamp, Value}}] -> {ok, #{value => Value, timestamp => Timestamp}}
    end.

%% @doc 缓存二维码数据到指定工位
-spec cache_qrcode_to_station(atom(), map()) -> ok.
cache_qrcode_to_station(StationNameEn, QrcodeData) ->
    %% 序列号作为主要指标
    case maps:get(<<"serial_no">>, QrcodeData, undefined) of
        undefined -> 
            ?LOG(warning, "二维码数据无序列号，工位: ~p", [StationNameEn]);
        SerialNo ->
            cache_station_metric(StationNameEn, <<?METRIC_QRCODE/binary, "_serial">>, SerialNo)
    end,
    
    %% 缓存完整的二维码数据
    cache_station_metric(StationNameEn, <<?METRIC_QRCODE/binary, "_data">>, QrcodeData),
    ?LOG(info, "二维码数据已缓存到工位: ~p", [StationNameEn]).

%% @doc 缓存无人机上线事件到指定工位
-spec cache_drone_online_to_station(atom(), binary()) -> ok.
cache_drone_online_to_station(StationNameEn, DroneId) ->
    cache_station_metric(StationNameEn, <<?METRIC_DRONE/binary, "_online">>, DroneId),
    cache_station_metric(StationNameEn, <<?METRIC_DRONE/binary, "_online_time">>, erlang:system_time(millisecond)),
    ?LOG(info, "无人机上线事件已缓存到工位: ~p, 无人机ID: ~s", [StationNameEn, DroneId]).

%% @doc 从工位获取缓存的二维码序列号
-spec get_qrcode_serial_from_station(atom()) -> {ok, binary()} | {error, not_find}.
get_qrcode_serial_from_station(StationNameEn) ->
    get_station_metric(StationNameEn, <<?METRIC_QRCODE/binary, "_serial">>).

%% @doc 从工位获取缓存的无人机上线事件
-spec get_drone_online_from_station(atom()) -> {ok, binary()} | {error, not_find}.
get_drone_online_from_station(StationNameEn) ->
    get_station_metric(StationNameEn, <<?METRIC_DRONE/binary, "_online">>).

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试无人机与工位绑定功能
-spec test_drone_station_binding() -> ok.
test_drone_station_binding() ->
    ?LOG(info, "=== 开始测试无人机与工位绑定功能 ==="),
    
    %% 1. 测试绑定功能
    StationAddr1 = 1,  %% 总测工位2
    DroneId1 = <<"test_drone_001">>,
    
    ?LOG(info, "测试1: 绑定无人机 ~s 到工位 ~p", [DroneId1, StationAddr1]),
    ok = bind_drone_to_station(StationAddr1, DroneId1),
    
    %% 2. 测试通过工位获取无人机
    case get_drone_by_station(StationAddr1) of
        {ok, RetrievedDroneId1} ->
            ?LOG(info, "测试1通过: 通过工位 ~p 获取到无人机 ~s", [StationAddr1, RetrievedDroneId1]);
        {error, Reason1} ->
            ?LOG(error, "测试1失败: 无法通过工位 ~p 获取无人机, 原因: ~p", [StationAddr1, Reason1])
    end,
    
    %% 3. 测试通过无人机获取工位
    case get_station_by_drone(DroneId1) of
        {ok, RetrievedStationAddr1} ->
            ?LOG(info, "测试1通过: 通过无人机 ~s 获取到工位 ~p", [DroneId1, RetrievedStationAddr1]);
        {error, Reason2} ->
            ?LOG(error, "测试1失败: 无法通过无人机 ~s 获取工位, 原因: ~p", [DroneId1, Reason2])
    end,
    
    %% 4. 测试第二个绑定
    StationAddr2 = 255,  %% 上料台
    DroneId2 = <<"test_drone_002">>,
    
    ?LOG(info, "测试2: 绑定无人机 ~s 到工位 ~p", [DroneId2, StationAddr2]),
    ok = bind_drone_to_station(StationAddr2, DroneId2),
    
    %% 5. 测试解绑
    ?LOG(info, "测试3: 解绑工位 ~p", [StationAddr1]),
    ok = unbind_drone_from_station(StationAddr1),
    
    %% 6. 验证解绑结果
    case get_drone_by_station(StationAddr1) of
        {error, not_find} ->
            ?LOG(info, "测试3通过: 工位 ~p 已成功解绑", [StationAddr1]);
        {ok, StillBoundDrone} ->
            ?LOG(error, "测试3失败: 工位 ~p 仍然绑定着无人机 ~s", [StationAddr1, StillBoundDrone])
    end,
    
    %% 7. 测试IP与工位映射
    TestIp = <<"192.168.100.50">>,
    TestStationAddr = 3,  %% 总测工位1
    
    ?LOG(info, "测试4: 设置IP ~s 映射到工位 ~p", [TestIp, TestStationAddr]),
    ok = set_station_ip(TestIp, TestStationAddr),
    
    %% 8. 测试通过IP获取工位
    case get_station_by_ip(TestIp) of
        {ok, RetrievedStationAddr} ->
            ?LOG(info, "测试4通过: 通过IP ~s 获取到工位 ~p", [TestIp, RetrievedStationAddr]);
        {error, Reason3} ->
            ?LOG(error, "测试4失败: 无法通过IP ~s 获取工位, 原因: ~p", [TestIp, Reason3])
    end,
    
    %% 9. 测试通过IP获取无人机（需要先绑定）
    ok = bind_drone_to_station(TestStationAddr, DroneId2),
    
    case get_drone_by_ip(TestIp) of
        {ok, RetrievedDroneId} ->
            ?LOG(info, "测试5通过: 通过IP ~s 获取到无人机 ~s", [TestIp, RetrievedDroneId]);
        {error, Reason4} ->
            ?LOG(error, "测试5失败: 无法通过IP ~s 获取无人机, 原因: ~p", [TestIp, Reason4])
    end,
    
    %% 10. 清理测试数据
    ?LOG(info, "清理测试数据..."),
    ok = unbind_drone_from_station(TestStationAddr),
    ok = unbind_drone_from_station(StationAddr2),
    
    ?LOG(info, "=== 无人机与工位绑定功能测试完成 ==="),
    ok.

%%%===================================================================
%%% 真实工位同步和设备创建函数
%%%===================================================================

%% @doc 将devaddr地址转换为治具地址
%% devaddr格式: "D1200", "D1100", 转换为整数地址
-spec devaddr_to_fixture_addr(binary()) -> integer().
devaddr_to_fixture_addr(DevAddr) when is_binary(DevAddr) ->
    case binary:split(DevAddr, <<"D">>) of
        [<<>>, NumStr] ->
            try binary_to_integer(NumStr) of
                Num -> Num
            catch
                _:_ -> 0
            end;
        _ -> 0
    end;
devaddr_to_fixture_addr(_) -> 0.

%% @doc 根据devaddr获取工位信息
-spec get_station_by_devaddr(binary()) -> {ok, atom()} | {error, not_find}.
get_station_by_devaddr(DevAddr) ->
    FixtureAddr = devaddr_to_fixture_addr(DevAddr),
    get_station_by_fixture(FixtureAddr).

%% @doc 从设备数据同步工位到工位管理器
-spec sync_station_from_device(binary(), map()) -> {ok, atom()} | {error, term()}.
sync_station_from_device(DevAddr, DeviceInfo) ->
    ?LOG(info, "从设备同步工位: devaddr=~s, 设备信息=~p", [DevAddr, DeviceInfo]),
    
    %% 从devaddr获取治具地址
    FixtureAddr = devaddr_to_fixture_addr(DevAddr),
    
    %% 从设备信息中提取工位名称
    StationName = maps:get(<<"name">>, DeviceInfo, <<"">>),
    
    %% 根据devaddr映射到工位英文名
    {StationNameEn, StationNameCn} = case DevAddr of
        <<"D1100">> -> {station_gantry, <<"桁行架工位"/utf8>>};
        <<"D1200">> -> {station_burn_in_1, <<"拷机工位1"/utf8>>};
        <<"D1300">> -> {station_burn_in_2, <<"拷机工位2"/utf8>>};
        <<"D1500">> -> {station_test_1, <<"总测工位1"/utf8>>};
        <<"D1600">> -> {station_test_2, <<"总测工位2"/utf8>>};
        <<"D1700">> -> {station_magnetic_heading, <<"磁航向工位"/utf8>>};
        _ -> 
            %% 生成默认工位名
            DefaultEn = list_to_atom("station_" ++ integer_to_list(FixtureAddr)),
            DefaultCn = case byte_size(StationName) > 0 of
                true -> StationName;
                false -> <<"自定义工位"/utf8>>
            end,
            {DefaultEn, DefaultCn}
    end,
    
    %% IP地址映射
    Ip = case FixtureAddr of
        0 -> <<"192.168.100.20">>;    %% 磁航向工位
        _ -> <<"192.168.100.40">>     %% 其他工位共享IP
    end,
    
    %% 业务类型映射
    BusinessType = case FixtureAddr of
        0 -> <<"扫码绑定"/utf8>>;
        N when N >= 1100 andalso N =< 1199 -> <<"结构测试"/utf8>>;
        N when N >= 1200 andalso N =< 1299 -> <<"拷机测试"/utf8>>;
        N when N >= 1500 andalso N =< 1699 -> <<"性能测试"/utf8>>;
        _ -> <<"其他测试"/utf8>>
    end,
    
    %% 创建设置工位到工位管理器
    create_station(FixtureAddr, StationNameEn, StationNameCn, Ip, DevAddr, BusinessType).

%% @doc 创建真实的工位设备（在Parse数据库中）
-spec create_real_station_device(integer(), binary(), binary(), binary(), binary(), map()) -> 
    {ok, binary()} | {error, term()}.
create_real_station_device(FixtureAddr, StationName, DevAddr, ProductId, Ip, ExtraInfo) ->
    ?LOG(info, "创建真实工位设备: 治具地址=~p, 工位名=~s, devaddr=~s, 产品ID=~s, IP=~s", 
         [FixtureAddr, StationName, DevAddr, ProductId, Ip]),
    
    %% 构建设备信息
    DeviceName = StationName,
    DeviceContent = #{
        <<"fixture_addr">> => FixtureAddr,
        <<"station_name">> => StationName,
        <<"ip">> => Ip,
        <<"devaddr">> => DevAddr,
        <<"business_type">> => maps:get(<<"business_type">>, ExtraInfo, <<"测试工位"/utf8>>)
    },
    
    %% 合并额外信息
    FullContent = maps:merge(DeviceContent, ExtraInfo),
    
    %% 创建设备（使用dgiot_parse接口）
    try
        %% 首先检查设备是否已存在（通过devaddr查询）
        case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DevAddr}}) of
            {ok, #{<<"results">> := []}} ->
                %% 设备不存在，创建新设备
                case dgiot_parse:create_object(<<"Device">>, #{
                    <<"name">> => DeviceName,
                    <<"devaddr">> => DevAddr,
                    <<"product">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Product">>,
                        <<"objectId">> => ProductId
                    },
                    <<"status">> => <<"OFFLINE">>,
                    <<"content">> => FullContent,
                    <<"address">> => maps:get(<<"address">>, ExtraInfo, <<"北京延庆航天九院项目部"/utf8>>),
                    <<"isEnable">> => true,
                    <<"location">> => #{
                        <<"__type">> => <<"GeoPoint">>,
                        <<"latitude">> => 120.167375,
                        <<"longitude">> => 30.268806
                    },
                    <<"detail">> => #{
                        <<"category">> => <<"5ca6049839">>,
                        <<"devType">> => <<"dgiot">>,
                        <<"executorname">> => <<"开发者"/utf8>>
                    },
                    <<"ACL">> => #{
                        <<"role:开发者">> => #{<<"read">> => true, <<"write">> => true}
                    }
                }) of
                    {ok, #{<<"objectId">> := DeviceId}} ->
                        ?LOG(info, "真实工位设备创建成功: 设备ID=~s, 工位名=~s", [DeviceId, StationName]),
                        
                        %% 同时同步到工位管理器
                        _ = sync_station_from_device(DevAddr, #{<<"name">> => StationName}),
                        
                        {ok, DeviceId};
                    {error, Reason} ->
                        ?LOG(error, "创建真实工位设备失败: ~p", [Reason]),
                        {error, Reason}
                end;
            {ok, #{<<"results">> := [ExistingDevice | _]}} ->
                %% 设备已存在，返回现有设备ID
                ExistingDeviceId = maps:get(<<"objectId">>, ExistingDevice),
                ?LOG(info, "真实工位设备已存在: 设备ID=~s, 工位名=~s", [ExistingDeviceId, StationName]),
                {ok, ExistingDeviceId};
            {error, QueryReason} ->
                ?LOG(error, "查询设备失败: ~p", [QueryReason]),
                {error, QueryReason}
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "创建真实工位设备异常: ~p, 堆栈: ~p", [Error, Stack]),
            {error, {exception, Error}}
    end.

%% @doc 同步所有真实工位到工位管理器
-spec sync_real_stations() -> {ok, map()}.
sync_real_stations() ->
    ?LOG(info, "开始同步真实工位到工位管理器"),
    
    %% 产品ID常量
    ProductId = <<"2de1b3e1b8">>,
    
    %% 定义需要同步的工位列表（根据您提供的真实数据）
    RealStations = [
        #{fixture_addr => 0,  devaddr => <<"D1700">>, name => <<"磁航向工位"/utf8>>, 
          ip => <<"192.168.100.20">>, business_type => <<"扫码绑定"/utf8>>},
        #{fixture_addr => 1100, devaddr => <<"D1100">>, name => <<"桁行架工位"/utf8>>,
          ip => <<"192.168.100.40">>, business_type => <<"结构测试"/utf8>>},
        #{fixture_addr => 1200, devaddr => <<"D1200">>, name => <<"拷机工位1"/utf8>>,
          ip => <<"192.168.100.40">>, business_type => <<"拷机测试"/utf8>>},
        #{fixture_addr => 1300, devaddr => <<"D1300">>, name => <<"拷机工位2"/utf8>>,
          ip => <<"192.168.100.40">>, business_type => <<"拷机测试"/utf8>>},
        #{fixture_addr => 1500, devaddr => <<"D1500">>, name => <<"总测工位1"/utf8>>,
          ip => <<"192.168.100.40">>, business_type => <<"性能测试"/utf8>>},
        #{fixture_addr => 1600, devaddr => <<"D1600">>, name => <<"总测工位2"/utf8>>,
          ip => <<"192.168.100.40">>, business_type => <<"性能测试"/utf8>>}
    ],
    
    %% 同步每个工位
    Results = lists:map(fun(Station) ->
        FixtureAddr = maps:get(fixture_addr, Station),
        DevAddr = maps:get(devaddr, Station),
        Name = maps:get(name, Station),
        Ip = maps:get(ip, Station),
        BusinessType = maps:get(business_type, Station),
        
        %% 检查是否已存在工位
        case get_station_by_fixture(FixtureAddr) of
            {ok, ExistingStation} ->
                ?LOG(info, "工位已存在: 治具地址=~p, 工位=~p", [FixtureAddr, ExistingStation]),
                #{status => already_exists, station => ExistingStation};
            {error, not_find} ->
                %% 创建设置工位
                ExtraInfo = #{
                    <<"business_type">> => BusinessType,
                    <<"address">> => <<"北京延庆航天九院项目部"/utf8>>
                },
                case create_real_station_device(FixtureAddr, Name, DevAddr, ProductId, Ip, ExtraInfo) of
                    {ok, DeviceId} ->
                        #{status => created, device_id => DeviceId, station_name => Name};
                    {error, Reason} ->
                        ?LOG(error, "创建设置工位失败: 治具地址=~p, 原因: ~p", [FixtureAddr, Reason]),
                        #{status => error, reason => Reason}
                end
        end
    end, RealStations),
    
    %% 统计结果
    CreatedCount = length([R || R <- Results, maps:get(status, R) =:= created]),
    UpdatedCount = length([R || R <- Results, maps:get(status, R) =:= already_exists]),
    ErrorCount = length([R || R <- Results, maps:get(status, R) =:= error]),
    
    Result = #{created => CreatedCount, updated => UpdatedCount, errors => ErrorCount},
    ?LOG(info, "真实工位同步完成: 创建=~p, 已存在=~p, 错误=~p", [CreatedCount, UpdatedCount, ErrorCount]),
    {ok, Result}.

%% @doc 初始化所有真实工位（一键创建）
-spec initialize_real_stations() -> {ok, map()} | {error, term()}.
initialize_real_stations() ->
    ?LOG(info, "开始初始化所有真实工位"),
    
    %% 1. 同步真实工位到工位管理器
    case sync_real_stations() of
        {ok, SyncResult} ->
            ?LOG(info, "真实工位同步结果: ~p", [SyncResult]),
            
            %% 2. 列出所有工位验证
            case list_all_stations() of
                {ok, AllStations} ->
                    StationCount = length(AllStations),
                    ?LOG(info, "工位管理器中共有 ~p 个工位", [StationCount]),
                    
                    %% 3. 验证设备创建
                    ProductId = <<"2de1b3e1b8">>,
                    Query = #{<<"where">> => #{<<"product">> => #{
                        <<"__type">> => <<"Pointer">>,
                        <<"className">> => <<"Product">>,
                        <<"objectId">> => ProductId
                    }}},
                    
                    case dgiot_parse:query_object(<<"Device">>, Query) of
                        {ok, #{<<"results">> := Devices}} ->
                            DeviceCount = length(Devices),
                            ?LOG(info, "Parse数据库中相关设备数量: ~p", [DeviceCount]),
                            
                            %% 返回完整结果
                            {ok, #{
                                sync_result => SyncResult,
                                station_count => StationCount,
                                device_count => DeviceCount,
                                stations => AllStations
                            }};
                        {error, Reason} ->
                            ?LOG(error, "查询设备失败: ~p", [Reason]),
                            {error, {query_devices_failed, Reason}}
                    end;
                {error, Reason} ->
                    ?LOG(error, "列出工位失败: ~p", [Reason]),
                    {error, {list_stations_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {sync_failed, Reason}}
    end.

%%%===================================================================
%%% 虚拟工位管理函数
%%%===================================================================

%% 虚拟工位常量定义
-define(VIRTUAL_ALARM_STATION_ID, 10).      %% 虚拟告警检测工位
-define(VIRTUAL_HEARTBEAT_STATION_ID, 11).  %% 虚拟心跳检测工位
-define(VIRTUAL_ALARM_IP, <<"192.168.100.20">>).   %% 与磁航向工位共用IP
-define(VIRTUAL_HEARTBEAT_IP, <<"192.168.100.40">>). %% 与共享PLC共用IP

%% @doc 创建虚拟告警检测工位
-spec create_virtual_alarm_station() -> {ok, atom()} | {error, term()}.
create_virtual_alarm_station() ->
    ?LOG(info, "创建虚拟告警检测工位"),
    
    StationParams = #{
        fixture_addr => ?VIRTUAL_ALARM_STATION_ID,
        station_name_en => station_virtual_alarm,
        station_name_cn => <<"虚拟告警检测工位"/utf8>>,
        ip => ?VIRTUAL_ALARM_IP,
        base_addr => <<"D1800">>,  %% 使用专门的虚拟地址
        business_type => <<"虚拟告警检测"/utf8>>
    },
    
    %% 设置虚拟工位标志
    ExtraInfo = #{
        <<"is_virtual">> => true,
        <<"virtual_type">> => <<"alarm_detection">>,
        <<"description">> => <<"虚拟告警检测工位，用于检测系统告警状态"/utf8>>,
        <<"address">> => <<"虚拟环境"/utf8>>
    },
    
    %% 创建虚拟工位（不创建设备，只在工位管理器中）
    create_station_with_virtual_flag(StationParams, ExtraInfo).

%% @doc 创建虚拟心跳检测工位
-spec create_virtual_heartbeat_station() -> {ok, atom()} | {error, term()}.
create_virtual_heartbeat_station() ->
    ?LOG(info, "创建虚拟心跳检测工位"),
    
    StationParams = #{
        fixture_addr => ?VIRTUAL_HEARTBEAT_STATION_ID,
        station_name_en => station_virtual_heartbeat,
        station_name_cn => <<"虚拟心跳检测工位"/utf8>>,
        ip => ?VIRTUAL_HEARTBEAT_IP,
        base_addr => <<"D1900">>,  %% 使用专门的虚拟地址
        business_type => <<"虚拟心跳检测"/utf8>>
    },
    
    %% 设置虚拟工位标志
    ExtraInfo = #{
        <<"is_virtual">> => true,
        <<"virtual_type">> => <<"heartbeat_detection">>,
        <<"description">> => <<"虚拟心跳检测工位，用于检测设备心跳状态"/utf8>>,
        <<"address">> => <<"虚拟环境"/utf8>>
    },
    
    %% 创建虚拟工位
    create_station_with_virtual_flag(StationParams, ExtraInfo).

%% @doc 带虚拟标志的工位创建
-spec create_station_with_virtual_flag(map(), map()) -> {ok, atom()} | {error, term()}.
create_station_with_virtual_flag(StationParams, ExtraInfo) ->
    FixtureAddr = maps:get(fixture_addr, StationParams),
    StationNameEn = maps:get(station_name_en, StationParams),
    StationNameCn = maps:get(station_name_cn, StationParams),
    Ip = maps:get(ip, StationParams),
    BaseAddr = maps:get(base_addr, StationParams),
    BusinessType = maps:get(business_type, StationParams),
    
    ?LOG(info, "创建设置虚拟工位: ~p (~s)", [StationNameEn, StationNameCn]),
    
    %% 检查是否已存在
    case get_station_by_fixture(FixtureAddr) of
        {ok, Existing} ->
            ?LOG(warning, "虚拟工位已存在: 治具地址=~p, 工位=~p", [FixtureAddr, Existing]),
            {ok, Existing};
        {error, not_find} ->
            %% 创建设置工位
            case create_station(FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType) of
                {ok, CreatedStation} ->
                    %% 设置虚拟工位标志到工位表
                    TableName = get_station_table_name(CreatedStation),
                    Now = erlang:system_time(millisecond),
                    
                    %% 存储虚拟工位信息
                    lists:foreach(fun({Key, Value}) ->
                        MetricKey = <<"virtual_", Key/binary>>,
                        ets:insert(TableName, {MetricKey, {Now, Value}})
                    end, maps:to_list(ExtraInfo)),
                    
                    %% 存储虚拟标志
                    ets:insert(TableName, {<<"is_virtual">>, {Now, true}}),
                    
                    ?LOG(info, "虚拟工位创建设置成功: ~p", [CreatedStation]),
                    {ok, CreatedStation};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 创建所有虚拟工位
-spec create_virtual_stations() -> {ok, #{alarm => atom() | error, heartbeat => atom() | error}}.
create_virtual_stations() ->
    ?LOG(info, "开始创建所有虚拟工位"),
    
    AlarmResult = case create_virtual_alarm_station() of
        {ok, AlarmStation} -> AlarmStation;
        {error, Reason1} -> {error, Reason1}
    end,
    
    HeartbeatResult = case create_virtual_heartbeat_station() of
        {ok, HeartbeatStation} -> HeartbeatStation;
        {error, Reason2} -> {error, Reason2}
    end,
    
    Result = #{alarm => AlarmResult, heartbeat => HeartbeatResult},
    ?LOG(info, "虚拟工位创建完成: ~p", [Result]),
    {ok, Result}.

%% @doc 检查是否为虚拟工位
-spec is_virtual_station(integer()) -> boolean().
is_virtual_station(FixtureAddr) ->
    case get_station_by_fixture(FixtureAddr) of
        {ok, StationNameEn} ->
            TableName = get_station_table_name(StationNameEn),
            case ets:lookup(TableName, <<"is_virtual">>) of
                [{<<"is_virtual">>, {_Timestamp, true}}] -> true;
                _ -> false
            end;
        {error, _} -> false
    end.

%% @doc 列出所有虚拟工位
-spec list_virtual_stations() -> {ok, [map()]}.
list_virtual_stations() ->
    case list_all_stations() of
        {ok, AllStations} ->
            VirtualStations = lists:filter(fun(Station) ->
                FixtureAddr = maps:get(fixture_addr, Station),
                is_virtual_station(FixtureAddr)
            end, AllStations),
            {ok, VirtualStations};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除所有虚拟工位
-spec delete_virtual_stations() -> {ok, map()}.
delete_virtual_stations() ->
    ?LOG(info, "开始删除所有虚拟工位"),
    
    case list_virtual_stations() of
        {ok, VirtualStations} ->
            Results = lists:map(fun(Station) ->
                FixtureAddr = maps:get(fixture_addr, Station),
                StationNameEn = maps:get(station_name_en, Station),
                
                case delete_station(FixtureAddr) of
                    {ok, _DeletedStation} ->
                        ?LOG(info, "虚拟工位删除成功: ~p", [StationNameEn]),
                        #{status => deleted, station => StationNameEn};
                    {error, Reason} ->
                        ?LOG(error, "虚拟工位删除失败: ~p, 原因: ~p", [StationNameEn, Reason]),
                        #{status => error, station => StationNameEn, reason => Reason}
                end
            end, VirtualStations),
            
            DeletedCount = length([R || R <- Results, maps:get(status, R) =:= deleted]),
            ErrorCount = length([R || R <- Results, maps:get(status, R) =:= error]),
            
            Result = #{deleted => DeletedCount, errors => ErrorCount},
            ?LOG(info, "虚拟工位删除完成: 删除=~p, 错误=~p", [DeletedCount, ErrorCount]),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 工位创建和管理函数
%%%===================================================================

%% @doc 创建设置工位（从map参数）
%% 参数格式: #{fixture_addr => integer(), station_name_en => atom(), 
%%            station_name_cn => binary(), ip => binary(), 
%%            base_addr => binary(), business_type => binary()}
-spec create_station(map()) -> {ok, atom()} | {error, term()}.
create_station(#{fixture_addr := FixtureAddr, station_name_en := StationNameEn,
                 station_name_cn := StationNameCn, ip := Ip,
                 base_addr := BaseAddr, business_type := BusinessType}) ->
    create_station(FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType);
create_station(Params) ->
    {error, {missing_params, Params}}.

%% @doc 创建设置工位（完整参数）
-spec create_station(integer(), atom(), binary(), binary(), binary(), binary()) -> 
    {ok, atom()} | {error, term()}.
create_station(FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType) ->
    ?LOG(info, "创建设置工位: 治具地址=~p, 英文名=~p, 名称=~s, IP=~s, 基地址=~s, 业务类型=~s", 
         [FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType]),
    
    %% 检查参数有效性
    case validate_station_params(FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType) of
        ok ->
            %% 检查治具地址是否已存在
            case get_station_by_fixture(FixtureAddr) of
                {ok, ExistingStation} ->
                    ?LOG(warning, "治具地址 ~p 已存在，对应工位: ~p", [FixtureAddr, ExistingStation]),
                    {error, {fixture_addr_exists, FixtureAddr, ExistingStation}};
                {error, not_find} ->
                    %% 检查工位英文名是否已存在
                    case get_station_by_name(StationNameEn) of
                        {ok, _ExistingStation} ->
                            {error, {station_name_exists, StationNameEn}};
                        {error, not_find} ->
                            %% 创建工位ETS表
                            TableName = get_station_table_name(StationNameEn),
                            dgiot_data:init(TableName, [public, named_table, set,
                                                       {write_concurrency, true},
                                                       {read_concurrency, true},
                                                       {keypos, 1}]),
                            
                            %% 添加到工位列表
                            StationRecord = {FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType},
                            case add_station_to_list(StationRecord) of
                                ok ->
                                    ?LOG(info, "工位创建设置成功: ~p (治具地址: ~p)", [StationNameEn, FixtureAddr]),
                                    {ok, StationNameEn};
                                {error, Reason} ->
                                    ?LOG(error, "工位创建设置失败: ~p", [Reason]),
                                    {error, Reason}
                            end
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 验证工位参数
-spec validate_station_params(integer(), atom(), binary(), binary(), binary(), binary()) -> 
    ok | {error, term()}.
validate_station_params(FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType) ->
    %% 检查治具地址
    case not is_integer(FixtureAddr) of
        true -> {error, fixture_addr_not_integer};
        false ->
            case FixtureAddr < 0 of
                true -> {error, fixture_addr_negative};
                false ->
                    case FixtureAddr > 65535 of
                        true -> {error, fixture_addr_too_large};
                        false ->
                            %% 检查工位英文名
                            case not is_atom(StationNameEn) of
                                true -> {error, station_name_en_not_atom};
                                false ->
                                    %% 检查工位中文名
                                    case not is_binary(StationNameCn) of
                                        true -> {error, station_name_cn_not_binary};
                                        false ->
                                            case byte_size(StationNameCn) =:= 0 of
                                                true -> {error, station_name_cn_empty};
                                                false ->
                                                    %% 检查IP地址（允许undefined）
                                                    case Ip =:= undefined of
                                                        true -> ok;
                                                        false ->
                                                            case not is_binary(Ip) of
                                                                true -> {error, ip_not_binary};
                                                                false ->
                                                                    case byte_size(Ip) =:= 0 of
                                                                        true -> {error, ip_empty};
                                                                        false ->
                                                                            %% 检查基地址（允许undefined）
                                                                            case BaseAddr =:= undefined of
                                                                                true -> ok;
                                                                                false ->
                                                                                    case not is_binary(BaseAddr) of
                                                                                        true -> {error, base_addr_not_binary};
                                                                                        false ->
                                                                                            case byte_size(BaseAddr) =:= 0 of
                                                                                                true -> {error, base_addr_empty};
                                                                                                false ->
                                                                                                    %% 检查业务类型
                                                                                                    case not is_binary(BusinessType) of
                                                                                                        true -> {error, business_type_not_binary};
                                                                                                        false ->
                                                                                                            case byte_size(BusinessType) =:= 0 of
                                                                                                                true -> {error, business_type_empty};
                                                                                                                false -> ok
                                                                                                            end
                                                                                                    end
                                                                                            end
                                                                                    end
                                                                            end
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

%% @doc 添加工位到工位列表
-spec add_station_to_list(tuple()) -> ok | {error, term()}.
add_station_to_list(StationRecord) ->
    try
        %% 从ETS获取当前工位列表
        case ets:lookup(station_registry, station_list) of
            [{station_list, Stations}] ->
                %% 检查是否已存在相同治具地址
                FixtureAddr = element(1, StationRecord),
                case lists:keyfind(FixtureAddr, 1, Stations) of
                    false ->
                        %% 添加到列表
                        NewStations = Stations ++ [StationRecord],
                        
                        %% 更新ETS中的工位列表
                        ets:insert(station_registry, {station_list, NewStations}),
                        ?LOG(debug, "工位列表已更新，新增工位: ~p", [StationRecord]),
                        ok;
                    Existing ->
                        ?LOG(warning, "治具地址 ~p 已存在: ~p", [FixtureAddr, Existing]),
                        {error, fixture_addr_exists}
                end;
            [] ->
                %% 如果ETS中没有工位列表，使用静态列表
                Stations = ?STATIONS,
                FixtureAddr = element(1, StationRecord),
                case lists:keyfind(FixtureAddr, 1, Stations) of
                    false ->
                        NewStations = Stations ++ [StationRecord],
                        ets:insert(station_registry, {station_list, NewStations}),
                        ?LOG(debug, "工位列表已更新（从静态列表），新增工位: ~p", [StationRecord]),
                        ok;
                    Existing ->
                        ?LOG(warning, "治具地址 ~p 已存在（静态列表）: ~p", [FixtureAddr, Existing]),
                        {error, fixture_addr_exists}
                end
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "添加工位到列表失败: ~p, 堆栈: ~p", [Error, Stack]),
            {error, {internal_error, Error}}
    end.

%% @doc 根据工位英文名获取工位信息
-spec get_station_by_name(atom()) -> {ok, map()} | {error, not_find}.
get_station_by_name(StationNameEn) ->
    case lists:keyfind(StationNameEn, 2, ?STATIONS) of
        {FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType} ->
            {ok, #{fixture_addr => FixtureAddr,
                   station_name_en => StationNameEn,
                   station_name_cn => StationNameCn,
                   ip => Ip,
                   base_addr => BaseAddr,
                   business_type => BusinessType}};
        false ->
            {error, not_find}
    end.

%% @doc 更新工位信息
-spec update_station(integer(), map()) -> {ok, atom()} | {error, term()}.
update_station(FixtureAddr, Updates) ->
    ?LOG(info, "更新工位信息: 治具地址=~p, 更新内容=~p", [FixtureAddr, Updates]),
    
    %% 获取现有工位信息
    case get_station_by_fixture(FixtureAddr) of
        {ok, StationNameEn} ->
            case get_station_info_by_name(StationNameEn) of
                #{fixture_addr := _OldFixtureAddr, station_name_en := OldStationNameEn,
                  station_name_cn := OldStationNameCn, ip := OldIp,
                  base_addr := OldBaseAddr, business_type := OldBusinessType} = _OldStation ->
                    
                    %% 构建更新后的工位记录
                    NewStationNameEn = maps:get(station_name_en, Updates, OldStationNameEn),
                    NewStationNameCn = maps:get(station_name_cn, Updates, OldStationNameCn),
                    NewIp = maps:get(ip, Updates, OldIp),
                    NewBaseAddr = maps:get(base_addr, Updates, OldBaseAddr),
                    NewBusinessType = maps:get(business_type, Updates, OldBusinessType),
                    
                    %% 验证新参数
                    case validate_station_params(FixtureAddr, NewStationNameEn, NewStationNameCn, 
                                                 NewIp, NewBaseAddr, NewBusinessType) of
                        ok ->
                            %% 删除旧工位
                            delete_station(FixtureAddr),
                            
                            %% 创建新工位
                            create_station(FixtureAddr, NewStationNameEn, NewStationNameCn, 
                                          NewIp, NewBaseAddr, NewBusinessType);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, station_info_not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 删除工位
-spec delete_station(integer()) -> {ok, atom()} | {error, term()}.
delete_station(FixtureAddr) ->
    ?LOG(info, "删除工位: 治具地址=~p", [FixtureAddr]),
    
    case get_station_by_fixture(FixtureAddr) of
        {ok, StationNameEn} ->
            %% 从ETS中的工位列表移除
            case ets:lookup(station_registry, station_list) of
                [{station_list, Stations}] ->
                    NewStations = lists:keydelete(FixtureAddr, 1, Stations),
                    ets:insert(station_registry, {station_list, NewStations});
                [] ->
                    %% 如果没有动态列表，使用静态列表
                    Stations = ?STATIONS,
                    NewStations = lists:keydelete(FixtureAddr, 1, Stations),
                    ets:insert(station_registry, {station_list, NewStations})
            end,
            
            %% 删除工位ETS表
            TableName = get_station_table_name(StationNameEn),
            case ets:info(TableName) of
                undefined -> ok;
                _ -> ets:delete(TableName)
            end,
            
            ?LOG(info, "工位删除成功: ~p (治具地址: ~p)", [StationNameEn, FixtureAddr]),
            {ok, StationNameEn};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 列出所有工位
-spec list_all_stations() -> {ok, [map()]}.
list_all_stations() ->
    %% 从ETS获取工位列表，如果为空则使用静态列表
    Stations = case ets:lookup(station_registry, station_list) of
        [{station_list, DynamicStations}] -> DynamicStations;
        [] -> ?STATIONS
    end,
    
    StationMaps = lists:map(fun({FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType}) ->
        #{fixture_addr => FixtureAddr,
          station_name_en => StationNameEn,
          station_name_cn => StationNameCn,
          ip => Ip,
          base_addr => BaseAddr,
          business_type => BusinessType}
    end, Stations),
    {ok, StationMaps}.

%% @doc 搜索工位
-spec search_stations(map()) -> {ok, [map()]} | {error, term()}.
search_stations(Criteria) ->
    try
        %% 获取所有工位
        {ok, AllStations} = list_all_stations(),
        
        %% 根据条件过滤
        FilteredStations = lists:filter(fun(Station) ->
            maps:fold(fun
                (fixture_addr, Value, Acc) when is_integer(Value) ->
                    Acc and (maps:get(fixture_addr, Station) =:= Value);
                (station_name_en, Value, Acc) when is_atom(Value) ->
                    Acc and (maps:get(station_name_en, Station) =:= Value);
                (station_name_cn, Value, Acc) when is_binary(Value) ->
                    StationNameCN = binary_to_list(maps:get(station_name_cn, Station)),
                    SearchValueCN = binary_to_list(Value),
                    Acc and (string:str(StationNameCN, SearchValueCN) > 0);
                (ip, Value, Acc) when is_binary(Value) ->
                    Acc and (maps:get(ip, Station) =:= Value);
                (business_type, Value, Acc) when is_binary(Value) ->
                    BusinessType = binary_to_list(maps:get(business_type, Station)),
                    SearchValueBT = binary_to_list(Value),
                    Acc and (string:str(BusinessType, SearchValueBT) > 0);
                (_, _, Acc) -> Acc
            end, true, Criteria)
        end, AllStations),
        
        {ok, FilteredStations}
    catch
        _:Error:Stack ->
            ?LOG(error, "搜索工位失败: ~p, 堆栈: ~p", [Error, Stack]),
            {error, {search_error, Error}}
    end.

%%%===================================================================
%%% 工位检查和诊断函数
%%%===================================================================

%% @doc 检查工位状态
%% 返回工位是否正常、存在的问题和建议
-spec check_station_status(integer()) -> #{status => atom(), issues => [binary()], suggestions => [binary()]}.
check_station_status(FixtureAddr) ->
    ?LOG(info, "检查工位状态: 治具地址=~p", [FixtureAddr]),
    
    case get_station_by_fixture(FixtureAddr) of
        {ok, StationNameEn} ->
            %% 检查工位ETS表
            TableName = get_station_table_name(StationNameEn),
            TableInfo = case ets:info(TableName) of
                undefined -> #{exists => false};
                Info -> Info#{exists => true}
            end,
            
            %% 获取工位信息
            StationInfo = case get_station_info_by_name(StationNameEn) of
                #{fixture_addr := Addr, station_name_cn := NameCn, 
                  ip := Ip, base_addr := BaseAddr, business_type := BusinessType} ->
                    #{fixture_addr => Addr, station_name_cn => NameCn,
                      ip => Ip, base_addr => BaseAddr, business_type => BusinessType};
                _ -> #{}
            end,
            
            %% 检查IP地址是否绑定
            IpStatus = case maps:get(ip, StationInfo, undefined) of
                undefined -> <<"未配置IP地址"/utf8>>;
                <<"">> -> <<"IP地址为空"/utf8>>;
                IpBin -> 
                    case get_station_by_ip(IpBin) of
                        {ok, MappedAddr} when MappedAddr =:= FixtureAddr -> 
                            <<"IP地址已正确映射"/utf8>>;
                        {ok, _MappedAddr} -> 
                            <<"IP地址映射到其他工位"/utf8>>;
                        {error, _} -> 
                            <<"IP地址未映射"/utf8>>
                    end
            end,
            
            %% 构建状态报告
            Issues = collect_station_issues(TableInfo, StationInfo, IpStatus),
            Suggestions = generate_suggestions(Issues),
            
            Status = case Issues of
                [] -> ok;
                _ -> warning
            end,
            
            #{status => Status,
              fixture_addr => FixtureAddr,
              station_name_en => StationNameEn,
              station_info => StationInfo,
              table_info => TableInfo,
              ip_status => IpStatus,
              issues => Issues,
              suggestions => Suggestions};
        {error, not_find} ->
            #{status => error,
              fixture_addr => FixtureAddr,
              issues => [<<"工位不存在"/utf8>>],
              suggestions => [<<"请先创建设置此工位"/utf8>>]}
    end.

%% @doc 收集工位问题
collect_station_issues(TableInfo, StationInfo, IpStatus) ->
    Issues0 = [],
    
    %% 检查ETS表
    Issues1 = case maps:get(exists, TableInfo, false) of
        false -> 
            Issues0 ++ [<<"工位ETS表不存在"/utf8>>];
        true ->
            case maps:get(size, TableInfo, 0) of
                0 -> Issues0 ++ [<<"工位ETS表为空"/utf8>>];
                _ -> Issues0
            end
    end,
    
    %% 检查IP地址
    Issues2 = case IpStatus of
        <<"未配置IP地址"/utf8>> -> Issues1 ++ [<<"未配置IP地址"/utf8>>];
        <<"IP地址为空"/utf8>> -> Issues1 ++ [<<"IP地址为空"/utf8>>];
        <<"IP地址未映射"/utf8>> -> Issues1 ++ [<<"IP地址未映射"/utf8>>];
        <<"IP地址映射到其他工位"/utf8>> -> Issues1 ++ [<<"IP地址映射到其他工位"/utf8>>];
        _ -> Issues1
    end,
    
    %% 检查基本信息
    Issues3 = case maps:get(ip, StationInfo, undefined) of
        undefined -> Issues2 ++ [<<"缺少IP配置"/utf8>>];
        _ -> Issues2
    end,
    
    case maps:get(base_addr, StationInfo, undefined) of
        undefined -> Issues3 ++ [<<"缺少基地址配置"/utf8>>];
        _ -> Issues3
    end.

%% @doc 生成建议
generate_suggestions(Issues) ->
    Suggestions = [],
    
    lists:foldl(fun
        (<<"工位不存在"/utf8>>, Acc) -> 
            Acc ++ [<<"使用 create_station/6 函数创建设置工位"/utf8>>];
        (<<"工位ETS表不存在"/utf8>>, Acc) -> 
            Acc ++ [<<"重新初始化工位ETS表"/utf8>>];
        (<<"工位ETS表为空"/utf8>>, Acc) -> 
            Acc ++ [<<"等待设备数据上报或手动添加测试数据"/utf8>>];
        (<<"未配置IP地址"/utf8>>, Acc) -> 
            Acc ++ [<<"在工位配置中添加IP地址"/utf8>>];
        (<<"IP地址为空"/utf8>>, Acc) -> 
            Acc ++ [<<"配置有效的IP地址"/utf8>>];
        (<<"IP地址未映射"/utf8>>, Acc) -> 
            Acc ++ [<<"使用 set_station_ip/2 函数设置IP映射"/utf8>>];
        (<<"IP地址映射到其他工位"/utf8>>, Acc) -> 
            Acc ++ [<<"检查IP地址配置，确保每个IP只映射到一个工位"/utf8>>];
        (<<"缺少IP配置"/utf8>>, Acc) -> 
            Acc ++ [<<"添加工位IP配置"/utf8>>];
        (<<"缺少基地址配置"/utf8>>, Acc) -> 
            Acc ++ [<<"添加工位基地址配置"/utf8>>];
        (_, Acc) -> Acc
    end, Suggestions, Issues).

%% @doc 检查所有工位状态
-spec check_all_stations() -> #{total => integer(), ok => integer(), warning => integer(), error => integer(), stations => list()}.
check_all_stations() ->
    ?LOG(info, "检查所有工位状态"),
    
    case list_all_stations() of
        {ok, Stations} ->
            Results = lists:map(fun(#{fixture_addr := Addr}) ->
                Status = check_station_status(Addr),
                Status
            end, Stations),
            
            %% 统计状态
            {OkCount, WarningCount, ErrorCount} = lists:foldl(fun
                (#{status := ok}, {Ok, Warn, Err}) -> {Ok + 1, Warn, Err};
                (#{status := warning}, {Ok, Warn, Err}) -> {Ok, Warn + 1, Err};
                (#{status := error}, {Ok, Warn, Err}) -> {Ok, Warn, Err + 1};
                (_, Acc) -> Acc
            end, {0, 0, 0}, Results),
            
            #{total => length(Stations),
              ok => OkCount,
              warning => WarningCount,
              error => ErrorCount,
              stations => Results};
        {error, Reason} ->
            #{total => 0,
              ok => 0,
              warning => 0,
              error => 1,
              stations => [],
              error_reason => Reason}
    end.

%% @doc 诊断工位问题
-spec diagnose_station_issues(integer()) -> #{diagnosis => [map()], recommendations => [binary()]}.
diagnose_station_issues(FixtureAddr) ->
    ?LOG(info, "诊断工位问题: 治具地址=~p", [FixtureAddr]),
    
    Status = check_station_status(FixtureAddr),
    
    case maps:get(status, Status, error) of
        ok ->
            #{diagnosis => [#{level => <<"正常"/utf8>>, 
                            message => <<"工位状态正常"/utf8>>}],
              recommendations => [<<"无需修复"/utf8>>]};
        warning ->
            Issues = maps:get(issues, Status, []),
            Suggestions = maps:get(suggestions, Status, []),
            
            Diagnosis = lists:map(fun(Issue) ->
                #{level => <<"警告"/utf8>>,
                  issue => Issue}
            end, Issues),
            
            #{diagnosis => Diagnosis,
              recommendations => Suggestions};
        error ->
            Issues = maps:get(issues, Status, []),
            Suggestions = maps:get(suggestions, Status, []),
            
            Diagnosis = lists:map(fun(Issue) ->
                #{level => <<"错误"/utf8>>,
                  issue => Issue}
            end, Issues),
            
            #{diagnosis => Diagnosis,
              recommendations => Suggestions}
    end.

%%%===================================================================
%%% 后来者上报机制 - 治具单片机触发已连接无人机的MES上报
%%%===================================================================

%% @doc 清理过期的IP映射
-spec cleanup_expired_ip_mappings() -> {ok, integer()}.
cleanup_expired_ip_mappings() ->
    Now = erlang:system_time(millisecond),
    ExpiredKeys = ets:foldl(fun({IpBin, {Timestamp, _StationAddr}}, Acc) ->
                                if Now - Timestamp > ?IP_CACHE_EXPIRE ->
                                       [IpBin | Acc];
                                   true -> Acc
                                end;
                               ({IpBin, Mapping}, Acc) when is_map(Mapping) ->
                                case maps:get(bound_at, Mapping, undefined) of
                                    undefined -> Acc;
                                    BoundAt when Now - BoundAt > ?IP_CACHE_EXPIRE ->
                                        [IpBin | Acc];
                                    _ -> Acc
                                end;
                               (_, Acc) -> Acc
                            end, [], uav_ip_station_mapping),
    
    %% 删除过期的映射
    lists:foreach(fun(IpBin) ->
                     ets:delete(uav_ip_station_mapping, IpBin),
                     ?LOG(info, "清理过期IP映射: ~s", [IpBin])
                  end, ExpiredKeys),
    
    Count = length(ExpiredKeys),
    ?LOG(info, "清理过期IP映射完成，共清理 ~p 个", [Count]),
    {ok, Count}.

%% @doc 治具单片机初始化时触发已连接无人机的MES上报
%% 当治具单片机上线时，如果已经有关联的无人机连接，则触发MES上报
-spec trigger_mes_report_for_connected_drone(binary()) -> ok.
trigger_mes_report_for_connected_drone(FixtureIp) ->
    trigger_mes_report_for_connected_drone(FixtureIp, undefined).

%% @doc 治具单片机初始化时触发已连接无人机的MES上报，并注册治具进程
%% 当治具单片机上线时，如果已经有关联的无人机连接，则触发MES上报
%% 同时注册治具进程到工位，为后续测试指令下发做准备
-spec trigger_mes_report_for_connected_drone(binary(), pid() | undefined) -> ok.
trigger_mes_report_for_connected_drone(FixtureIp, FixturePid) ->
    ?LOG(info, "治具IP ~s 上线，检查是否有已连接的无人机需要上报MES, 治具PID: ~p", [FixtureIp, FixturePid]),
    
    %% 1. 通过IP获取工位信息
    case get_station_by_ip(FixtureIp) of
        {ok, StationAddr} ->
            %% 2. 通过工位获取无人机ID
            case get_drone_by_station(StationAddr) of
                {ok, DroneId} ->
                    %% 3. 触发MES上报
                    ?LOG(info, "治具IP ~s (工位~p) 关联无人机 ~s，触发MES上报", 
                         [FixtureIp, StationAddr, DroneId]),
                    
                    %% 导入MES模块（避免编译错误）
                    case code:is_loaded(dgiot_uav_mes_api) of
                        false ->
                            ?LOG(warning, "MES模块未加载，无法上报");
                        true ->
                            %% 异步上报无人机上线状态
                            spawn(fun() ->
                                try
                                    %% 获取工位名称
                                    case get_station_by_fixture(StationAddr) of
                                        {ok, StationNameEn} ->
                                            StationInfo = get_station_info_by_name(StationNameEn),
                                            StationName = maps:get(station_name_cn, StationInfo, <<"未知工位">>),
                                            
                                            %% 获取二维码信息
                                            QrcodeSerial = case get_qrcode_serial_from_station(StationNameEn) of
                                                {ok, Serial} -> Serial;
                                                {error, _} -> <<"">>
                                            end,
                                            
                                            %% 上报无人机上线
                                            dgiot_uav_mes_api:report_drone_online_to_mes(DroneId, StationAddr, 
                                                                                         StationName, QrcodeSerial),
                                            %% 触发自动化测试
                                            case code:is_loaded(dgiot_uav_auto_tester) of
                                                false ->
                                                    ?LOG(warning, "自动化测试模块未加载，无法启动测试");
                                                true ->
                                                    dgiot_uav_auto_tester:start_test_for_device(DroneId)
                                            end,
                                            %% 完善治具进程注册，为后续测试指令下发做准备
                                            case FixturePid of
                                                undefined ->
                                                    %% 未提供治具PID，尝试通过注册表查找
                                                    case code:is_loaded(dgiot_uav_business_service) of
                                                        false ->
                                                            ?LOG(warning, "业务服务模块未加载，无法检查治具进程注册");
                                                        true ->
                                                            case dgiot_uav_business_service:get_station_fixture(StationAddr) of
                                                                {ok, ExistingPid} ->
                                                                    ?LOG(info, "治具进程已注册: 工位~p -> PID ~p", [StationAddr, ExistingPid]);
                                                                {error, not_find} ->
                                                                    ?LOG(warning, "治具进程未注册，工位~p，需要后续注册", [StationAddr])
                                                            end
                                                    end;
                                                Pid when is_pid(Pid) ->
                                                    %% 注册治具进程到工位
                                                    case code:is_loaded(dgiot_uav_business_service) of
                                                        false ->
                                                            ?LOG(warning, "业务服务模块未加载，无法注册治具进程");
                                                        true ->
                                                            dgiot_uav_business_service:register_station_fixture(StationAddr, Pid),
                                                            ?LOG(info, "治具进程注册完成: 工位~p -> PID ~p", [StationAddr, Pid])
                                                    end
                                            end;
                                        {error, Reason} ->
                                            ?LOG(warning, "无法获取工位名称，工位地址: ~p, 原因: ~p", [StationAddr, Reason])
                                    end
                                catch
                                    _:Error ->
                                        ?LOG(error, "触发MES上报失败: ~p", [Error])
                                end
                            end)
                    end;
                {error, not_find} ->
                    ?LOG(debug, "治具IP ~s (工位~p) 尚未绑定无人机", [FixtureIp, StationAddr]);
                {error, Reason} ->
                    ?LOG(error, "获取治具IP ~s 的无人机失败: ~p", [FixtureIp, Reason])
            end;
        {error, expired} ->
            ?LOG(warning, "治具IP ~s 的工位映射已过期", [FixtureIp]);
        {error, not_find} ->
            ?LOG(debug, "治具IP ~s 未找到对应的工位映射", [FixtureIp]);
        {error, Reason} ->
            ?LOG(error, "获取治具IP ~s 的工位信息失败: ~p", [FixtureIp, Reason])
    end,
    ok.

%% @doc 根据工位英文名获取工位信息
get_station_info_by_name(StationNameEn) ->
    case lists:keyfind(StationNameEn, 2, ?STATIONS) of
        {FixtureAddr, StationNameEn, StationNameCn, Ip, BaseAddr, BusinessType} ->
            #{fixture_addr => FixtureAddr,
              station_name_en => StationNameEn,
              station_name_cn => StationNameCn,
              ip => Ip,
              base_addr => BaseAddr,
              business_type => BusinessType};
        false ->
            #{fixture_addr => 0,
              station_name_en => StationNameEn,
              station_name_cn => <<"未知工位">>,
              ip => <<"">>,
              base_addr => <<"">>,
              business_type => <<"未知">>}
    end.

%%%===================================================================
%%% 地址段映射功能 - 实现告警地址到真实工位的自动归结
%%%===================================================================

%% @doc 根据地址获取对应的工位信息
%% 参数: Address - PLC寄存器地址，如1130
%% 返回: {ok, StationId, StationName, DeviceName} | {error, not_found}
-spec get_station_by_address(integer()) -> {ok, integer(), binary(), binary()} | {error, not_found}.
get_station_by_address(Address) ->
    ?LOG(debug, "根据地址查询工位: ~p", [Address]),
    
    %% 在地址段范围内查找
    case lists:foldl(fun
        ({StationId, Start, End, DeviceName, StationName}, Acc) ->
            case Address >= Start andalso Address =< End of
                true ->
                    %% 找到匹配的地址段
                    {found, StationId, StationName, DeviceName};
                false ->
                    Acc
            end;
        (_, Acc) ->
            Acc
    end, not_found, ?ALARM_ADDRESS_RANGES) of
        
        {found, StationId, StationName, DeviceName} ->
            ?LOG(info, "地址 ~p 映射到工位: ID=~p, 名称=~s, 设备=~s", 
                 [Address, StationId, StationName, DeviceName]),
            {ok, StationId, StationName, DeviceName};
            
        not_found ->
            ?LOG(warning, "地址 ~p 未找到对应的工位映射", [Address]),
            {error, not_found}
    end.

%% @doc 获取所有地址段映射定义
-spec get_address_ranges() -> {ok, list(#address_range_mapping{})}.
get_address_ranges() ->
    %% 将宏定义转换为记录列表
    Mappings = lists:map(fun({StationId, Start, End, DeviceName, StationName}) ->
        #address_range_mapping{
            station_id = StationId,
            range_start = Start,
            range_end = End,
            device_name = DeviceName,
            station_name = StationName,
            description = <<"告警地址段: D", (integer_to_binary(Start))/binary, 
                          "-D", (integer_to_binary(End))/binary>>
        }
    end, ?ALARM_ADDRESS_RANGES),
    
    {ok, Mappings}.

%% @doc 验证地址是否属于指定工位
%% 参数: StationId - 工位ID, Address - PLC寄存器地址
%% 返回: true | false
-spec validate_address_for_station(integer(), integer()) -> boolean().
validate_address_for_station(StationId, Address) ->
    case get_station_by_address(Address) of
        {ok, FoundStationId, _, _} ->
            FoundStationId =:= StationId;
        {error, not_found} ->
            false
    end.

%% @doc 将告警地址映射到工位信息
%% 参数: WordAddress - 字地址, Bit - 位偏移
%% 返回: {ok, StationId, StationName, DeviceName, FullAddress} | {error, Reason}
-spec map_alarm_to_station(integer(), 0..15) -> 
    {ok, integer(), binary(), binary(), binary()} | {error, term()}.
map_alarm_to_station(WordAddress, Bit) ->
    %% 首先根据字地址获取工位信息
    case get_station_by_address(WordAddress) of
        {ok, StationId, StationName, DeviceName} ->
            %% 构建完整的地址表示
            FullAddress = <<"D", (integer_to_binary(WordAddress))/binary, 
                           ".", (integer_to_binary(Bit))/binary>>,
            
            ?LOG(info, "告警地址 ~p.~p 映射到工位 ~p: ~s (设备: ~s)", 
                 [WordAddress, Bit, StationId, StationName, DeviceName]),
            
            {ok, StationId, StationName, DeviceName, FullAddress};
            
        {error, not_found} ->
            %% 如果是虚拟地址段（如9990-9999），则映射到虚拟工位
            case WordAddress >= 9990 andalso WordAddress =< 9999 of
                true ->
                    %% 根据地址范围确定虚拟工位类型
                    VirtualStationId = case WordAddress of
                        Addr when Addr >= 9990 andalso Addr =< 9995 -> 10;  % 虚拟告警工位
                        _ -> 11  % 虚拟心跳工位
                    end,
                    
                    VirtualStationName = case VirtualStationId of
                        10 -> <<"虚拟告警检测工位">>;
                        11 -> <<"虚拟心跳检测工位">>;
                        _ -> <<"虚拟工位">>
                    end,
                    
                    FullAddress = <<"D", (integer_to_binary(WordAddress))/binary,
                                   ".", (integer_to_binary(Bit))/binary>>,
                    
                    {ok, VirtualStationId, VirtualStationName, <<"虚拟设备">>, FullAddress};
                    
                false ->
                    ?LOG(error, "无法映射告警地址: D~p.~p", [WordAddress, Bit]),
                    {error, {address_not_mapped, WordAddress, Bit}}
            end
    end.

%% @doc 初始化地址映射ETS表
-spec init_address_mappings() -> ok.
init_address_mappings() ->
    ?LOG(info, "初始化地址映射ETS表"),
    
    %% 检查是否已存在
    case ets:info(uav_address_mappings) of
        undefined ->
            %% 创建地址映射表
            ets:new(uav_address_mappings, [
                named_table, public, set,
                {keypos, #address_range_mapping.station_id},
                {write_concurrency, true}
            ]),
            
            %% 插入地址段映射
            {ok, Mappings} = get_address_ranges(),
            lists:foreach(fun(Mapping) ->
                ets:insert(uav_address_mappings, Mapping)
            end, Mappings),
            
            ?LOG(info, "地址映射ETS表初始化完成，插入 ~p 条记录", [length(Mappings)]),
            ok;
            
        _ ->
            ?LOG(info, "地址映射ETS表已存在，跳过初始化"),
            ok
    end.

%% @doc 重新加载地址段映射
-spec reload_address_mappings() -> {ok, integer()} | {error, term()}.
reload_address_mappings() ->
    ?LOG(info, "重新加载地址段映射"),
    
    try
        %% 清空现有表
        ets:delete_all_objects(uav_address_mappings),
        
        %% 重新插入地址段映射
        {ok, Mappings} = get_address_ranges(),
        lists:foreach(fun(Mapping) ->
            ets:insert(uav_address_mappings, Mapping)
        end, Mappings),
        
        Count = length(Mappings),
        ?LOG(info, "地址段映射重新加载完成，共 ~p 条记录", [Count]),
        {ok, Count}
    catch
        _:Error:Stack ->
            ?LOG(error, "重新加载地址段映射失败: ~p, 堆栈: ~p", [Error, Stack]),
            {error, {reload_failed, Error}}
    end.

%% @doc 测试地址映射功能
-spec test_address_mapping() -> ok.
test_address_mapping() ->
    ?LOG(info, "=== 开始测试地址映射功能 ==="),
    
    %% 1. 测试地址段到工位的映射
    TestCases = [
        {1130, 1, <<"测试线PLC工位">>},      %% 地址段开头
        {1150, 1, <<"测试线PLC工位">>},      %% 地址段中间
        {1189, 1, <<"测试线PLC工位">>},      %% 地址段结尾
        {1730, 2, <<"磁航向工位">>},         %% 磁航向工位
        {1755, 2, <<"磁航向工位">>},         %% 磁航向工位中间
        {1789, 2, <<"磁航向工位">>},         %% 磁航向工位结尾
        {1530, 3, <<"机器人1工位">>},        %% 机器人1工位
        {1560, 3, <<"机器人1工位">>},        %% 机器人1工位中间
        {1589, 3, <<"机器人1工位">>},        %% 机器人1工位结尾
        {1630, 4, <<"机器人2工位">>},        %% 机器人2工位
        {1650, 4, <<"机器人2工位">>},        %% 机器人2工位中间
        {1689, 4, <<"机器人2工位">>}         %% 机器人2工位结尾
    ],
    
    %% 测试每个用例
    lists:foreach(fun({Address, ExpectedStationId, ExpectedStationName}) ->
        case get_station_by_address(Address) of
            {ok, StationId, StationName, _DeviceName} ->
                if StationId =:= ExpectedStationId andalso StationName =:= ExpectedStationName ->
                        ?LOG(info, "测试通过: 地址 ~p -> 工位 ~p (~s)", 
                             [Address, StationId, StationName]);
                   true ->
                        ?LOG(error, "测试失败: 地址 ~p, 期望: 工位 ~p (~s), 实际: 工位 ~p (~s)", 
                             [Address, ExpectedStationId, ExpectedStationName, StationId, StationName])
                end;
            {error, not_found} ->
                ?LOG(error, "测试失败: 地址 ~p 未找到对应的工位映射", [Address])
        end
    end, TestCases),
    
    %% 2. 测试地址验证功能
    ?LOG(info, "测试地址验证功能"),
    ValidationTests = [
        {1130, 1, true},
        {1130, 2, false},   %% 错误工位ID
        {1200, 1, false},   %% 不在地址段范围内
        {1789, 2, true}
    ],
    
    lists:foreach(fun({Address, StationId, Expected}) ->
        Result = validate_address_for_station(StationId, Address),
        if Result =:= Expected ->
                ?LOG(info, "验证通过: 地址 ~p 属于工位 ~p? ~p", [Address, StationId, Expected]);
           true ->
                ?LOG(error, "验证失败: 地址 ~p 属于工位 ~p? 期望: ~p, 实际: ~p", 
                     [Address, StationId, Expected, Result])
        end
    end, ValidationTests),
    
    %% 3. 测试告警地址映射功能
    ?LOG(info, "测试告警地址映射功能"),
    AlarmTests = [
        {1135, 2, 1, <<"测试线PLC工位">>},  %% 字地址1135，位偏移2
        {1740, 5, 2, <<"磁航向工位">>},     %% 字地址1740，位偏移5
        {1550, 0, 3, <<"机器人1工位">>},    %% 字地址1550，位偏移0
        {9992, 3, 10, <<"虚拟告警检测工位">>}, %% 虚拟告警工位
        {9998, 7, 11, <<"虚拟心跳检测工位">>}  %% 虚拟心跳工位
    ],
    
    lists:foreach(fun({WordAddress, Bit, ExpectedStationId, ExpectedStationName}) ->
        case map_alarm_to_station(WordAddress, Bit) of
            {ok, StationId, StationName, _DeviceName, FullAddress} ->
                if StationId =:= ExpectedStationId andalso StationName =:= ExpectedStationName ->
                        ?LOG(info, "告警映射通过: D~p.~p -> 工位 ~p (~s), 完整地址: ~s", 
                             [WordAddress, Bit, StationId, StationName, FullAddress]);
                   true ->
                        ?LOG(error, "告警映射失败: D~p.~p, 期望: 工位 ~p (~s), 实际: 工位 ~p (~s)", 
                             [WordAddress, Bit, ExpectedStationId, ExpectedStationName, StationId, StationName])
                end;
            {error, Reason} ->
                ?LOG(error, "告警映射失败: D~p.~p, 原因: ~p", [WordAddress, Bit, Reason])
        end
    end, AlarmTests),
    
    %% 4. 测试地址段列表获取
    ?LOG(info, "测试地址段列表获取功能"),
    case get_address_ranges() of
        {ok, Mappings} ->
            Count = length(Mappings),
            ?LOG(info, "获取到 ~p 条地址段映射记录", [Count]),
            lists:foreach(fun(#address_range_mapping{
                station_id = StationId,
                range_start = Start,
                range_end = End,
                station_name = StationName,
                device_name = DeviceName,
                description = Description
            }) ->
                ?LOG(info, "地址段 ~p: D~p-D~p -> 工位 ~p (~s, 设备: ~s, 描述: ~s)", 
                     [StationId, Start, End, StationId, StationName, DeviceName, Description])
            end, Mappings);
        {error, Reason} ->
            ?LOG(error, "获取地址段映射失败: ~p", [Reason])
    end,
    
    ?LOG(info, "=== 地址映射功能测试完成 ==="),
    ok.

%% @doc 运行完整测试
-spec test() -> ok.
test() ->
    ?LOG(info, "=== 工位管理器完整测试开始 ==="),
    
    %% 测试ETS初始化
    init_ets(),
    
    %% 测试工位列表
    case list_all_stations() of
        {ok, Stations} ->
            ?LOG(info, "当前工位数量: ~p", [length(Stations)]),
            lists:foreach(fun(Station) ->
                try
                    %% 从station_name_en提取ID信息
                    StationNameEn = maps:get(station_name_en, Station, <<"unknown">>),
                    StationNameCn = maps:get(station_name_cn, Station, <<"未知">>),
                    ?LOG(info, "工位 ~p (CN: ~p)", [StationNameEn, StationNameCn])
                catch
                    _:_ -> 
                        ?LOG(info, "工位: ~p", [Station])
                end
            end, Stations);
        Error ->
            ?LOG(error, "获取工位列表失败: ~p", [Error])
    end,
    
    %% 测试地址映射
    test_address_mapping(),
    
    %% 测试所有工位状态检查 (暂时跳过，避免错误)
    try
        case check_all_stations() of
            {ok, Results} ->
                ?LOG(info, "工位状态检查结果数量: ~p", [length(Results)]);
            Error2 ->
                ?LOG(error, "工位状态检查失败: ~p", [Error2])
        end
    catch
        _:Err ->
            ?LOG(error, "工位状态检查异常: ~p", [Err])
    end,
    
    ?LOG(info, "=== 工位管理器完整测试完成 ==="),
    ok.

