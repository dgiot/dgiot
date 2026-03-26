%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_plc_business 模块 - PLC业务层
%%% 负责虚拟工位处理、告警监控、心跳监控等业务逻辑
%%% 与通信层解耦，提高代码可维护性
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_plc_business).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

%% API
-export([
    is_virtual_station/1,
    start_virtual_monitoring/3,
    handle_virtual_station_ready/4,
    handle_virtual_station_disconnected/2
]).

%% 告警监控相关
-export([
    monitor_alarms/2,
    get_alarm_info_by_address/1
]).

%% 心跳监控相关
-export([
    monitor_heartbeat/2,
    check_operation_mode/2
]).

%% 工位管理相关
-export([
    notify_worker_connected/1,
    notify_worker_disconnected/1,
    get_worker_name/1
]).

%%%===================================================================
%%% 虚拟工位处理函数
%%%===================================================================

%% @doc 检查是否为虚拟工位
-spec is_virtual_station(StationId :: integer()) -> boolean().
is_virtual_station(StationId) ->
    StationId =:= ?BASE_VIRTUAL_ALARM orelse StationId =:= ?BASE_VIRTUAL_HEARTBEAT.

%% @doc 启动虚拟工位监控
-spec start_virtual_monitoring(StationId :: integer(), Ip :: binary(), ChildState :: map()) -> ok.
start_virtual_monitoring(StationId, Ip, _ChildState) ->
    case is_virtual_station(StationId) of
        true ->
            ?LOG(info, "启动虚拟工位监控: 工位 ~p, IP ~ts", [StationId, Ip]),
            %% 虚拟告警检测工位启动告警监控
            case StationId =:= ?BASE_VIRTUAL_ALARM of
                true ->
                    spawn_link(fun() -> monitor_alarms(StationId, Ip) end),
                    ?LOG(info, "虚拟告警监控进程已启动: 工位 ~p", [StationId]);
                false -> ok
            end,
            %% 虚拟心跳检测工位启动心跳监控
            case StationId =:= ?BASE_VIRTUAL_HEARTBEAT of
                true ->
                    spawn_link(fun() -> monitor_heartbeat(StationId, Ip) end),
                    ?LOG(info, "虚拟心跳监控进程已启动: 工位 ~p", [StationId]);
                false -> ok
            end,
            ok;
        false ->
            ?LOG(info, "工位 ~p 不是虚拟工位，跳过监控启动", [StationId]),
            ok
    end.

%% @doc 处理虚拟工位连接就绪
-spec handle_virtual_station_ready(StationId :: integer(), StationName :: binary(), 
                                   Ip :: binary(), ChildState :: map()) -> ok.
handle_virtual_station_ready(StationId, StationName, Ip, ChildState) ->
    case is_virtual_station(StationId) of
        true ->
            ?LOG(info, "虚拟工位连接就绪: ID=~p, 名称=~ts, IP=~ts", 
                 [StationId, StationName, Ip]),
            %% 启动监控
            start_virtual_monitoring(StationId, Ip, ChildState);
        false ->
            ok  % 不是虚拟工位，不做特殊处理
    end.

%% @doc 处理虚拟工位连接断开
-spec handle_virtual_station_disconnected(StationId :: integer(), StationName :: binary()) -> ok.
handle_virtual_station_disconnected(StationId, StationName) ->
    case is_virtual_station(StationId) of
        true ->
            ?LOG(warning, "虚拟工位连接断开: ID=~p, 名称=~ts", [StationId, StationName]);
        false ->
            ok  % 不是虚拟工位，不做特殊处理
    end.

%%%===================================================================
%%% 告警监控函数
%%%===================================================================

%% @doc 虚拟告警监控函数
-spec monitor_alarms(StationId :: integer(), Ip :: binary()) -> no_return().
monitor_alarms(StationId, Ip) ->
    ?LOG(info, "虚拟告警监控进程启动: 工位 ~p, IP ~ts", [StationId, Ip]),
    monitor_alarms_loop(StationId, Ip, 0).

%% @doc 虚拟告警监控循环
monitor_alarms_loop(StationId, Ip, CycleCount) ->
    %% 每10秒读取一次告警状态
    timer:sleep(10000),
    
    %% 根据IP地址确定要监控的PLC寄存器范围
    %% 注意：这里传递的是相对地址，read/3函数会加上基地址
    {RelativeAddr, WordCount} = case Ip of
        <<"192.168.100.20">> ->
            %% 磁航向PLC告警区域: D1730.0-D1789.15
            %% 基地址: D1700, 相对地址: 30 (1730-1700=30)
            {30, 60};
        <<"192.168.100.40">> ->
            %% 测试线PLC告警区域: D1130.0-D1189.15
            %% 基地址: D1100, 相对地址: 30 (1130-1100=30)
            {30, 60};
        _ ->
            %% 默认：相对地址30，60个寄存器
            {30, 60}
    end,
    
    %% 读取告警状态（异步读取，不等待响应）
    %% 注意：read/3函数是异步的，不会立即返回数据
    %% read/3会计算：基地址 + 相对地址 = 绝对地址
    ?LOG(debug, "异步读取告警状态: 工位 ~p, 相对地址 ~p, 数量 ~p", [StationId, RelativeAddr, WordCount]),
    dgiot_uav_plc_tcp_client:read(StationId, RelativeAddr, WordCount),
    
    %% 继续监控循环
    monitor_alarms_loop(StationId, Ip, CycleCount + 1).






%%%===================================================================
%%% 心跳监控函数
%%%===================================================================

%% @doc 虚拟心跳监控函数
-spec monitor_heartbeat(StationId :: integer(), Ip :: binary()) -> no_return().
monitor_heartbeat(StationId, Ip) ->
    ?LOG(info, "虚拟心跳监控进程启动: 工位 ~p, IP ~ts", [StationId, Ip]),
    monitor_heartbeat_loop(StationId, Ip, 0).

%% @doc 虚拟心跳监控循环
monitor_heartbeat_loop(StationId, Ip, CycleCount) ->
    %% 每5秒检查一次心跳
    timer:sleep(5000),
    
    %% 异步读取心跳寄存器（D49为心跳寄存器）
    ?LOG(debug, "异步读取心跳: 工位 ~p, 地址 49, 数量 1", [StationId]),
    dgiot_uav_plc_tcp_client:read(StationId, 49, 1),
    
    %% 每10次循环（约50秒）检查一次运行模式
    case CycleCount rem 10 =:= 0 of
        true ->
            check_operation_mode(StationId, Ip);
        false -> ok
    end,
    
    %% 继续监控循环
    monitor_heartbeat_loop(StationId, Ip, CycleCount + 1).

%% @doc 检查运行模式（手动/自动）
-spec check_operation_mode(StationId :: integer(), Ip :: binary()) -> ok.
check_operation_mode(StationId, _Ip) ->
    %% 读取运行模式寄存器（假设D50为运行模式寄存器）
    %% 异步读取，不等待响应
    ?LOG(debug, "异步读取运行模式: 工位 ~p, 地址 50, 数量 1", [StationId]),
    dgiot_uav_plc_tcp_client:read(StationId, 50, 1).

%% @doc 占位函数：根据地址获取告警信息
-spec get_alarm_info_by_address(Addr :: integer()) -> binary().
get_alarm_info_by_address(_Addr) ->
    <<"未知告警"/utf8>>.

%%%===================================================================
%%% 工位管理函数
%%%===================================================================

%% @doc 通知工位Worker连接就绪
-spec notify_worker_connected(StationId :: integer()) -> ok.
notify_worker_connected(StationId) ->
    WorkerName = get_worker_name(StationId),
    try
        gen_server:cast(WorkerName, {connection_ready, StationId})
    catch
        _:Error ->
            ?LOG(error, "PLC系统: 通知工位Worker连接就绪失败: StationId=~p, Error=~p", [StationId, Error])
    end.

%% @doc 通知工位Worker连接断开
-spec notify_worker_disconnected(StationId :: integer()) -> ok.
notify_worker_disconnected(StationId) ->
    WorkerName = get_worker_name(StationId),
    try
        gen_server:cast(WorkerName, {connection_disconnected, StationId})
    catch
        _:Error ->
            ?LOG(error, "PLC系统: 通知工位Worker连接断开失败: StationId=~p, Error=~p", [StationId, Error])
    end.

%% @doc 获取工位Worker名称
-spec get_worker_name(StationId :: integer()) -> atom().
get_worker_name(StationId) ->
    list_to_atom("dgiot_uav_plc_worker_" ++ integer_to_list(StationId)).