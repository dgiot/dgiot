%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_alarm_manager - 无人机告警管理器
%%% 核心功能：告警地址到真实工位的自动映射
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_alarm_manager).

%% 头文件包含
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").

%% 核心功能导出
-export([start_link/0, stop/0]).
-export([init_ets/0]).
-export([trigger_alarm_by_address/3, trigger_alarm_by_address/4]).
-export([clear_alarm/1]).
-export([get_active_alarms/0]).
-export([test/0, debug/0]).

%%%===================================================================
%%% 进程管理（简化版本，避免supervisor重启循环）
%%%===================================================================

%% @doc 启动告警管理器进程（简化版本，只初始化ETS表）
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG(info, "启动无人机告警管理器"),
    %% 初始化ETS表
    try
        init_ets(),
        %% 返回一个简单的dummy进程ID
        {ok, spawn(fun() -> 
            ?LOG(info, "告警管理器ETS表初始化完成，进程运行中"),
            timer:sleep(infinity)
        end)}
    catch
        _:Error:Stack ->
            ?LOG(error, "告警管理器启动失败: ~p, stack: ~p", [Error, Stack]),
            {error, Error}
    end.

%% @doc 停止告警管理器进程
-spec stop() -> ok.
stop() ->
    ?LOG(info, "停止无人机告警管理器"),
    ok.

%%%===================================================================
%%% 核心功能实现
%%%===================================================================

%% @doc 根据地址自动触发告警（自动映射到真实工位）
-spec trigger_alarm_by_address(integer(), 0..15, binary()) -> 
    {ok, integer(), binary(), binary(), binary()} | {error, term()}.
trigger_alarm_by_address(WordAddress, Bit, Description) ->
    trigger_alarm_by_address(WordAddress, Bit, Description, 2).  %% 默认级别2（中）

%% @doc 根据地址自动触发告警（带告警级别）
-spec trigger_alarm_by_address(integer(), 0..15, binary(), integer()) -> 
    {ok, integer(), binary(), binary(), binary()} | {error, term()}.
trigger_alarm_by_address(WordAddress, Bit, Description, Level) ->
    try
        %% 使用地址映射功能自动获取工位信息
        case dgiot_uav_station_manager:map_alarm_to_station(WordAddress, Bit) of
            {ok, StationId, StationName, DeviceName, FullAddress} ->
                ?LOG(warning, "Address trigger alarm: address=~s, mapped to station=~p(~ts), device=~ts", 
                     [FullAddress, StationId, StationName, DeviceName]),
                
                %% 生成告警ID
                AlarmId = generate_alarm_id(WordAddress, Bit),
                
                %% 触发告警
                case trigger_alarm(AlarmId, StationId, Description, DeviceName, Level) of
                    {ok, _} ->
                        {ok, StationId, StationName, DeviceName, FullAddress};
                    {error, TriggerError} ->
                        ?LOG(error, "Trigger alarm failed: ~p", [TriggerError]),
                        {error, {trigger_failed, TriggerError}}
                end;
                
            {error, Reason} ->
                ?LOG(error, "Address mapping failed: address=~p.~p, reason=~p", [WordAddress, Bit, Reason]),
                {error, Reason}
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "Address trigger alarm exception: ~p, stack: ~p", [Error, Stack]),
            {error, {exception, Error}}
    end.

%% @doc 生成告警ID
-spec generate_alarm_id(integer(), 0..15) -> binary().
generate_alarm_id(WordAddress, Bit) ->
    Timestamp = erlang:system_time(millisecond),
    <<"ALARM_", (integer_to_binary(WordAddress))/binary, "_", 
      (integer_to_binary(Bit))/binary, "_", (integer_to_binary(Timestamp))/binary>>.

%% @doc 触发告警
-spec trigger_alarm(binary(), integer(), binary(), binary(), integer()) -> 
    {ok, binary()} | {error, term()}.
trigger_alarm(AlarmId, StationId, Description, Device, Level) ->
    try
        Now = erlang:system_time(millisecond),
        
        %% 创建告警记录 - 使用元组格式，确保第一个元素是alarm_id作为key
        %% 对于ETS表的set类型，keypos默认是1，所以元组的第一个元素必须是键
        AlarmRecord = {AlarmId, #{
            station_id => StationId,
            description => Description,
            device => Device,
            level => Level,
            triggered_at => Now,
            active => true,
            cleared => false
        }},
        
        %% 检查ETS表是否存在
        case ets:info(uav_alarms) of
            undefined ->
                ?LOG(error, "ETS table uav_alarms not found, initializing..."),
                init_ets();
            _ ->
                ok
        end,
        
        %% 查看ETS表当前状态
        TableInfo = ets:info(uav_alarms),
        ?LOG(info, "ETS table info: ~p", [TableInfo]),
        
        %% 打印要插入的数据结构
        ?LOG(info, "Inserting alarm record: ~p", [AlarmRecord]),
        
        %% 保存到ETS - 使用元组格式
        Result = ets:insert(uav_alarms, AlarmRecord),
        ?LOG(info, "ETS insert result: ~p", [Result]),
        
        %% 验证插入是否成功 - 使用正确的键查找
        case ets:lookup(uav_alarms, AlarmId) of
            [{AlarmId, LookedRecord}] ->
                ?LOG(info, "Alarm inserted successfully: ~p", [LookedRecord]);
            [] ->
                ?LOG(error, "Alarm not found after insert!")
        end,
        
        %% 检查表中所有记录
        AllRecords = ets:tab2list(uav_alarms),
        ?LOG(info, "All alarms in ETS table: ~p", [AllRecords]),
        
        ?LOG(warning, "Alarm triggered: ID=~s, station=~p, level=~p, description=~ts", 
             [AlarmId, StationId, Level, Description]),
        
        {ok, AlarmId}
    catch
        _:Error:Stack ->
            ?LOG(error, "Trigger alarm failed: ~p, stack: ~p", [Error, Stack]),
            {error, {trigger_failed, Error}}
    end.

%% @doc 清除告警
-spec clear_alarm(binary()) -> ok | {error, term()}.
clear_alarm(AlarmId) ->
    try
        case ets:lookup(uav_alarms, AlarmId) of
            [{AlarmId, AlarmRecord = #{active := true}}] ->
                Now = erlang:system_time(millisecond),
                NewRecord = AlarmRecord#{
                    active => false,
                    cleared => true,
                    cleared_at => Now,
                    cleared_by => <<"system">>
                },
                
                %% 更新记录 - 保持元组格式
                ets:insert(uav_alarms, {AlarmId, NewRecord}),
                ?LOG(info, "Alarm cleared: ID=~s", [AlarmId]),
                ok;
                
            [{AlarmId, #{active := false}}] ->
                ?LOG(info, "Alarm already cleared: ~s", [AlarmId]),
                ok;
                
            [] ->
                ?LOG(error, "Alarm not found: ~s", [AlarmId]),
                {error, alarm_not_found}
        end
    catch
        _:Error:Stack ->
            ?LOG(error, "Clear alarm failed: ~p, stack: ~p", [Error, Stack]),
            {error, {clear_failed, Error}}
    end.

%% @doc 获取活跃告警列表
-spec get_active_alarms() -> {ok, list(map())}.
get_active_alarms() ->
    try
        %% 遍历所有告警，筛选活跃的
        ActiveAlarms = ets:foldl(fun
            ({_Key, Alarm = #{active := true}}, Acc) -> [Alarm | Acc];
            (_, Acc) -> Acc
        end, [], uav_alarms),
        
        {ok, ActiveAlarms}
    catch
        _:Error:Stack ->
            ?LOG(error, "Get active alarms failed: ~p, stack: ~p", [Error, Stack]),
            {error, {get_active_failed, Error}}
    end.

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 简单调试函数
-spec debug() -> {ok, term()}.
debug() ->
    io:format("=== Alarm Manager Debug ===~n"),
    
    %% 初始化ETS
    init_ets(),
    
    %% 触发一个简单的告警
    io:format("Triggering alarm...~n"),
    Result = trigger_alarm_by_address(1135, 2, <<"调试告警"/utf8>>),
    io:format("Trigger result: ~p~n", [Result]),
    
    %% 检查ETS表
    try
        Count = ets:info(uav_alarms, size),
        io:format("uav_alarms table size: ~p~n", [Count]),
        
        AllAlarms = ets:tab2list(uav_alarms),
        io:format("All alarms: ~p~n", [AllAlarms])
    catch
        _:Error ->
            io:format("Error checking ETS table: ~p~n", [Error])
    end,
    
    io:format("=== Debug completed ===~n"),
    {ok, Result}.

%% @doc 运行完整测试
-spec test() -> ok.
test() ->
    ?LOG(info, "=== Alarm manager full test started ==="),
    
    %% 初始化ETS
    init_ets(),
    
    %% 测试地址段1：测试线PLC工位 (1130-1189)
    TestAddresses1 = [1130, 1140, 1150, 1160, 1170, 1180, 1189],
    lists:foreach(fun(Addr) ->
        case dgiot_uav_station_manager:get_station_by_address(Addr) of
            {ok, StationId, StationName, DeviceName} ->
                ?LOG(info, "Address ~p -> station ~p(~ts), device: ~ts", 
                     [Addr, StationId, StationName, DeviceName]);
            {error, not_found} ->
                ?LOG(warning, "Address ~p not mapped to any station", [Addr])
        end
    end, TestAddresses1),
    
    %% 测试地址段2：磁航向工位 (1730-1789)
    TestAddresses2 = [1730, 1740, 1750, 1760, 1770, 1780, 1789],
    lists:foreach(fun(Addr) ->
        case dgiot_uav_station_manager:get_station_by_address(Addr) of
            {ok, StationId, StationName, DeviceName} ->
                ?LOG(info, "Address ~p -> station ~p(~ts), device: ~ts", 
                     [Addr, StationId, StationName, DeviceName]);
            {error, not_found} ->
                ?LOG(warning, "Address ~p not mapped to any station", [Addr])
        end
    end, TestAddresses2),
    
    %% 测试告警触发
    TestCases = [
        {1135, 2, <<"测试线温度过高"/utf8>>},
        {1740, 5, <<"磁航向传感器故障"/utf8>>},
        {1550, 0, <<"机器人1电机过载"/utf8>>},
        {1675, 7, <<"机器人2位置偏差"/utf8>>}
    ],
    
    lists:foreach(fun({WordAddress, Bit, Description}) ->
        ?LOG(info, "Test alarm trigger: D~p.~p - ~ts", [WordAddress, Bit, Description]),
        
        case trigger_alarm_by_address(WordAddress, Bit, Description) of
            {ok, StationId, StationName, DeviceName, FullAddress} ->
                ?LOG(info, "Alarm triggered successfully: D~p.~p -> station ~p(~ts), device: ~ts, full address: ~s", 
                     [WordAddress, Bit, StationId, StationName, DeviceName, FullAddress]);
            {error, Reason} ->
                ?LOG(error, "Alarm trigger failed: D~p.~p, reason: ~p", [WordAddress, Bit, Reason])
        end
    end, TestCases),
    
    %% 测试获取活跃告警
    case get_active_alarms() of
        {ok, ActiveAlarms} ->
            Count = length(ActiveAlarms),
            ?LOG(info, "Current active alarms count: ~p", [Count]),
            lists:foreach(fun(#{station_id := Sid, description := Desc}) ->
                ?LOG(info, "Active alarm: station=~p, description=~ts", [Sid, Desc])
            end, ActiveAlarms);
        {error, Reason} ->
            ?LOG(error, "Get active alarms failed: ~p", [Reason])
    end,
    
    ?LOG(info, "=== Alarm manager full test completed ==="),
    ok.

%%%===================================================================
%%% ETS初始化
%%%===================================================================

%% @doc ETS初始化函数（由-dgiot_data("ets")自动调用）
-spec init_ets() -> ok.
init_ets() ->
    ?LOG(info, "初始化无人机告警管理器ETS表"),
    
    %% 首先检查表是否已经存在
    case ets:info(uav_alarms) of
        undefined ->
            ?LOG(info, "Creating uav_alarms ETS table..."),
            %% 创建表，使用默认keypos（对于map是1，但map的第一个字段可能不是alarm_id）
            ets:new(uav_alarms, [public, named_table, set,
                                {write_concurrency, true},
                                {read_concurrency, true},
                                {heir, none}]),
            ?LOG(info, "uav_alarms table created");
        _ ->
            ?LOG(info, "uav_alarms table already exists")
    end,
    
    case ets:info(uav_alarm_stats) of
        undefined ->
            ?LOG(info, "Creating uav_alarm_stats ETS table..."),
            ets:new(uav_alarm_stats, [public, named_table, set,
                                     {write_concurrency, true},
                                     {read_concurrency, true},
                                     {heir, none}]),
            ?LOG(info, "uav_alarm_stats table created");
        _ ->
            ?LOG(info, "uav_alarm_stats table already exists")
    end,
    
    ?LOG(info, "无人机告警管理器ETS表初始化完成"),
    ok.