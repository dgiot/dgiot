%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(dgiot_uav_plc_tcp_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").
-define(TYPE, <<"UAVPLCC">>).

%% API
-export([start/2]).
-dgiot_data("ets").
-export([init_ets/0, get_station_by_fixture_addr/1, init_fixture_station_mappings/0]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% 工位配置（根据用户提供的所有工位配置，增加 commands 和 command_interval）
-define(STATION_CONFIGS, [
    #{
        station_id => ?BASE_GANTRY ,
        station_name => <<"桁行架"/utf8>>,
        ip => ?IP_SHARED,
        port => ?PORT_MODBUS,
        base_address => ?BASE_GANTRY,  % D1100
        fixture_address => ?FIXTURE_GANTRY,  % 7
        fixture_address1 => ?FIXTURE_GANTRY,
        instruction_set => <<"桁行架"/utf8>>,
        commands => [   % 示例指令集，实际可根据需要修改
            {<<"58e0d17e22_1">>, 1},
            {<<"58e0d17e22_2">>, 2}
        ],
        command_interval => 1000   % 指令间延时（毫秒）
    },
    #{
        station_id => ?BASE_BURN_IN_1,
        station_name => <<"拷机"/utf8>>,
        ip => ?IP_SHARED,
        port => ?PORT_MODBUS,
        base_address => ?BASE_BURN_IN_1,  % D1200
        fixture_address => ?FIXTURE_BURN_IN_1,  % 6
        fixture_address1 => ?FIXTURE_BURN_IN_1,
        instruction_set => <<"拷机测试"/utf8>>,
        commands => [
            {<<"b377b6e364">>, 1},   % 拷机准备
            {<<"ff197f0670">>, 2}    % 空速标定
        ],
        command_interval => 1500
    },
    #{
        station_id => ?BASE_BURN_IN_2,
        station_name => <<"拷机"/utf8>>,
        ip => ?IP_SHARED,
        port => ?PORT_MODBUS,
        base_address => ?BASE_BURN_IN_2,  % D1300
        fixture_address => ?FIXTURE_BURN_IN_2,  % 5
        fixture_address1 => ?FIXTURE_BURN_IN_2,
        instruction_set => <<"拷机测试"/utf8>>,
        commands => [],
        command_interval => 1000
    },
    #{
        station_id => ?BASE_TOTAL_TEST_1,
        station_name => <<"总测"/utf8>>,
        ip => ?IP_SHARED,
        port => ?PORT_MODBUS,
        base_address => ?BASE_TOTAL_TEST_1,  % D1500
        fixture_address => ?FIXTURE_TOTAL_TEST_1,  % 3
        fixture_address1 => ?FIXTURE_TOTAL_TEST_1_POWER,  % 4
        instruction_set => <<"机器人手臂"/utf8>>,
        commands => [
            {<<"bb896ba543_1">>, 1},   % 飞控版本号检查
            {<<"ce7d8a050c_2">>, 2},   % 弹翼开关与引信通信调试
            {<<"7e6c8a5125_3">>, 3},   % 空速调试
            {<<"7e6155207c_4">>, 4},   % 铁电故障调试
            {<<"4950ffcc3a_5">>, 5},   % 引信24V供电调试
            {<<"082099bb72_6">>, 6}    % 帧频检查
        ],
        command_interval => 1200
    },
    #{
        station_id => ?BASE_MAGNETIC,
        station_name => <<"磁航向测试"/utf8>>,
        ip => ?IP_MAGNETIC_PLC,
        port => ?PORT_MODBUS,
        base_address => ?BASE_MAGNETIC,  % D1700
        fixture_address => ?FIXTURE_MAGNETIC,  % 0
        fixture_address1 => ?FIXTURE_MAGNETIC,
        instruction_set => <<"磁航向"/utf8>>,
        commands => [
            {<<"58e0d17e22">>, 1},   % 磁航向校准
            {<<"eef47bcea7">>, 2}    % 磁航向测试
        ],
        command_interval => 1000
    },
    #{
        station_id => ?BASE_TOTAL_TEST_2,
        station_name => <<"总测"/utf8>>,
        ip => ?IP_SHARED,
        port => ?PORT_MODBUS,
        base_address => ?BASE_TOTAL_TEST_2,  % D1600
        fixture_address => ?FIXTURE_TOTAL_TEST_2,  % 1
        fixture_address1 => ?FIXTURE_TOTAL_TEST_2_POWER,  % 2
        instruction_set => <<"机器人手臂"/utf8>>,
        commands => [],
        command_interval => 1000
    },
    #{
        station_id => ?BASE_VIRTUAL_ALARM,
        station_name => <<"磁航向PLC监控"/utf8>>,
        ip => ?IP_VIRTUAL_ALARM,
        port => ?PORT_MODBUS,
        base_address => ?BASE_VIRTUAL_ALARM,  % D1700（与PLC模拟器地址范围1700-1799对齐）
        fixture_address => ?FIXTURE_VIRTUAL_ALARM,  % 8
        fixture_address1 => ?FIXTURE_VIRTUAL_ALARM,
        instruction_set => <<"PLC监控"/utf8>>,
        commands => [],  % 无指令，只做PLC状态监控
        command_interval => 1000
    },
    #{
        station_id => ?BASE_VIRTUAL_HEARTBEAT,
        station_name => <<"共享PLC监控"/utf8>>,
        ip => ?IP_VIRTUAL_HEARTBEAT,
        port => ?PORT_MODBUS,
        base_address => ?BASE_VIRTUAL_HEARTBEAT,  % D1100（与PLC模拟器地址范围1100-1199对齐）
        fixture_address => ?FIXTURE_VIRTUAL_HEARTBEAT,  % 9
        fixture_address1 => ?FIXTURE_VIRTUAL_HEARTBEAT,
        instruction_set => <<"PLC监控"/utf8>>,
        commands => [],  % 无指令，只做PLC状态监控
        command_interval => 1000
    }
]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"无人机PLC通道"/utf8>>
    },
    description => #{
        zh => <<"无人机PLC通道，处理无人机PLC的TCP会话，集成工位管理和五步校验功能"/utf8>>
    }
}).

%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/uav_plc_channel.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).

%% 状态记录
-record(state, {
    id :: binary(),
    env :: map(),
    client_monitors :: map(),      % 客户端进程监控映射 {ClientId => MonitorRef}
    heartbeat_timer :: reference() % 心跳检查定时器
}).

init_ets() ->
    init_two_stage_registration_ets().

%% @doc 启动通道
start(ChannelId, ChannelArgs) ->
    ?LOG(info, "~ts", [<<"启动UAV PLC统一通道"/utf8>>]),
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% @doc 通道初始化
init(?TYPE, ChannelId, ChannelArgs) ->
    ?LOG(info, "~ts", [<<"初始化UAV PLC统一通道"/utf8>>]),
    
    %% 初始化业务ETS表
    catch dgiot_uav_business_service:init_ets(),
    
    %% 启动自动化测试器和设备监控器
    ?LOG(info, "启动无人机自动化测试器和设备监控器..."),

    %% 启动自动化测试器
    %% TODO: dgiot_uav_auto_tester:start_link() - 模块不存在，暂时注释
    %% case dgiot_uav_auto_tester:start_link() of
    %%     {ok, _TesterPid} ->
    %%         ?LOG(info, "自动化测试器启动成功");
    %%     {error, {already_started, _Pid}} ->
    %%         ?LOG(info, "自动化测试器已启动");
    %%     {error, Reason} ->
    %%         ?LOG(error, "自动化测试器启动失败: ~p", [Reason])
    %% end,

    %% 启动设备监控器
    case dgiot_uav_device_monitor:start_link() of
        {ok, MonitorPid} ->
            ?LOG(info, "设备监控器启动成功, Pid=~p", [MonitorPid]);
        {error, {already_started, Pid}} ->
            ?LOG(info, "设备监控器已启动, Pid=~p", [Pid]);
        {error, Reason2} ->
            ?LOG(error, "设备监控器启动失败: ~p", [Reason2])
    end,
    
    State = #state{
        id = ChannelId,
        env = ChannelArgs,
        client_monitors = #{},
        heartbeat_timer = undefined
    },
    dgiot_client:add_clock(ChannelId, dgiot_datetime:now_secs() - 10, dgiot_datetime:now_secs() + 120),
    SupArgs = #{
        <<"channel">> => ChannelId,
        <<"mod">> => dgiot_uav_plc_tcp_client
    },
    SupChildSpec = dgiot_client:register(ChannelId, tcp_client_sup, SupArgs),
    {ok, State, SupChildSpec}.

handle_init(#state{id = ChannelId, client_monitors = Monitors} = State) ->
    %% 启动心跳检查定时器（每30秒检查一次客户端状态）
    HeartbeatTimer = erlang:send_after(30000, self(), {heartbeat_check, ChannelId}),
    
    %% 如果已有客户端启动过，重新建立监控
    UpdatedMonitors = case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            Monitors;
        _ ->
            %% 重建所有客户端的进程监控
            rebuild_client_monitors(ChannelId, Monitors)
    end,
    
    {ok, State#state{heartbeat_timer = HeartbeatTimer, client_monitors = UpdatedMonitors}}.

handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(start_client, #state{id = ChannelId, client_monitors = Monitors} = State) ->
    %% 错误级别才打印：首次启动或重复启动都打印ERROR
    ?LOG(error, "【UAV PLC通道】收到start_client消息: ChannelId=~p, ChannelType=~p, 描述=无人机测试产线PLC通道", [ChannelId, ?TYPE]),
    case dgiot_data:get({start_client, ChannelId}) of
        not_find ->
            ?LOG(error, "UAV PLC通道首次启动工位客户端: 配置工位数量=~p, 无需重启", [length(?STATION_CONFIGS)]),
            NewMonitors = lists:foldl(fun(#{station_id := StationId, ip := Ip, port := Port,
                                           commands := Commands, command_interval := Interval} = StationConfig, AccMonitors) ->
                BinId  = dgiot_utils:to_binary(StationId),
                ClientId = <<"plc", "_", BinId/binary>>,
                %% 注意：静态指令集注册已废弃，改用测试项管理。此处仅传递配置给客户端，不再调用 register_station_commands。
                %% 如需自动执行指令集，应在工位绑定时根据关联的测试项启动测试流程。
                % dgiot_uav_business_service:register_station_commands(StationId, Commands, Interval),
                StartResult = dgiot_client:start(ChannelId, ClientId, #{
                    <<"ip">> => Ip,
                    <<"port">> => Port,
                    <<"child">> => StationConfig#{commands => Commands, command_interval => Interval}
                }),
                
                %% 监控客户端进程
                case StartResult of
                    {ok, Pid} when is_pid(Pid) ->
                        %% 建立进程监控
                        MonitorRef = erlang:monitor(process, Pid),
                        ?LOG(error, "UAV PLC监控客户端进程: ClientId=~p, Pid=~p", [ClientId, Pid]),
                        AccMonitors#{ClientId => {Pid, MonitorRef}};
                    _ ->
                        ?LOG(error, "UAV PLC客户端启动失败: ClientId=~p, Result=~p", [ClientId, StartResult]),
                        AccMonitors
                end
            end, Monitors, ?STATION_CONFIGS),
            
            dgiot_data:insert({start_client, ChannelId}, ChannelId),
            {ok, State#state{client_monitors = NewMonitors}};
        _ ->
            ?LOG(error, "UAV PLC通道客户端已启动: 跳过重复启动 (ChannelId=~p), 无需重启", [ChannelId]),
            {ok, State}
    end;

%% 客户端进程DOWN消息处理
handle_message({'DOWN', MonitorRef, process, Pid, Reason}, #state{id = ChannelId, client_monitors = Monitors} = State) ->
    %% 区分正常关闭和异常崩溃
    case Reason of
        shutdown ->
            %% 正常关闭,静默处理或debug级别
            ?LOG(debug, "PLC客户端正常关闭: Pid=~p, MonitorRef=~p", [Pid, MonitorRef]);
        normal ->
            ?LOG(debug, "PLC客户端正常退出: Pid=~p, MonitorRef=~p", [Pid, MonitorRef]);
        _ ->
            %% 异常崩溃,打印错误日志
            ?LOG(error, "PLC客户端进程崩溃: Pid=~p, Reason=~p, MonitorRef=~p", [Pid, Reason, MonitorRef])
    end,

    %% 查找对应的客户端ID
    case find_client_by_pid_and_ref(Pid, MonitorRef, Monitors) of
        {ok, ClientId} ->
            ?LOG(info, "客户端进程崩溃: ClientId=~p, 尝试自动重启...", [ClientId]),
            %% 尝试重启客户端
            case restart_client(ChannelId, ClientId, Monitors) of
                {ok, NewMonitors} ->
                    {ok, State#state{client_monitors = NewMonitors}};
                {error, Error} ->
                    ?LOG(error, "重启客户端失败: ClientId=~p, Error=~p", [ClientId, Error]),
                    %% 移除监控但保留记录，等待心跳检查时处理
                    NewMonitors = remove_monitor(ClientId, Monitors),
                    {ok, State#state{client_monitors = NewMonitors}}
            end;
        not_found ->
            ?LOG(warning, "收到未知进程的DOWN消息: Pid=~p, MonitorRef=~p", [Pid, MonitorRef]),
            {ok, State}
    end;

%% 心跳检查消息
handle_message({heartbeat_check, ChannelId}, #state{id = ChannelId, client_monitors = Monitors, heartbeat_timer = OldTimer} = State) ->
    %% 取消旧的定时器（如果存在）
    if 
        OldTimer =/= undefined -> 
            erlang:cancel_timer(OldTimer);
        true -> 
            ok
    end,
    
    ?LOG(debug, "执行心跳检查，当前监控客户端数量: ~p", [map_size(Monitors)]),
    
    %% 检查所有客户端进程是否存活
    NewMonitors = maps:fold(fun(ClientId, {Pid, MonitorRef}, Acc) ->
        case is_process_alive(Pid) of
            true ->
                %% 进程存活，保留监控
                Acc#{ClientId => {Pid, MonitorRef}};
            false ->
                ?LOG(warning, "心跳检查发现死亡进程: ClientId=~p, Pid=~p, 尝试重启...", [ClientId, Pid]),
                %% 进程死亡，尝试重启
                case restart_client(ChannelId, ClientId, Acc) of
                    {ok, UpdatedMonitors} ->
                        UpdatedMonitors;
                    {error, Error} ->
                        ?LOG(error, "心跳检查时重启客户端失败: ClientId=~p, Error=~p", [ClientId, Error]),
                        %% 移除死亡进程的监控
                        remove_monitor(ClientId, Acc)
                end
        end
    end, #{}, Monitors),
    
    %% 设置下一次心跳检查（30秒后）
    NewHeartbeatTimer = erlang:send_after(30000, self(), {heartbeat_check, ChannelId}),
    
    {ok, State#state{client_monitors = NewMonitors, heartbeat_timer = NewHeartbeatTimer}};

%% 设备上线消息处理
handle_message({device_online, DeviceId, DeviceInfo}, State) ->
    ?LOG(info, "~n========================================", []),
    ?LOG(info, "📥 【PLC通道】收到设备上线消息", []),
    ?LOG(info, "----------------------------------------", []),
    ?LOG(info, "DeviceId: ~p", [DeviceId]),
    ?LOG(info, "DeviceInfo: ~p", [DeviceInfo]),
    ?LOG(info, "DeviceInfo Keys: ~p", [maps:keys(DeviceInfo)]),
    case maps:find(<<"devaddr">>, DeviceInfo) of
        {ok, DevAddr} ->
            ?LOG(info, "DevAddr: ~p", [DevAddr]);
        _ ->
            ?LOG(info, "DevAddr: 未找到")
    end,
    case maps:find(<<"productName">>, DeviceInfo) of
        {ok, ProductName} ->
            ?LOG(info, "ProductName: ~p", [ProductName]);
        _ ->
            ?LOG(info, "ProductName: 未找到")
    end,
    case maps:find(<<"station_id">>, DeviceInfo) of
        {ok, StationId} ->
            ?LOG(info, "StationId: ~p", [StationId]);
        _ ->
            ?LOG(info, "StationId: 未找到")
    end,
    case maps:find(<<"status">>, DeviceInfo) of
        {ok, Status} ->
            ?LOG(info, "Status: ~p", [Status]);
        _ ->
            ?LOG(info, "Status: 未找到")
    end,
    ?LOG(info, "========================================~n", []),
    
    %% 通知自动化测试器，使用try...catch防止noproc异常导致进程崩溃
    try dgiot_uav_auto_tester:handle_device_online(DeviceId) of
        ok ->
            ?LOG(info, "✅ 自动化测试器处理设备上线成功: DeviceId=~p", [DeviceId]);
        {error, Reason} ->
            ?LOG(warning, "⚠️  自动化测试器处理设备上线失败: DeviceId=~p, Reason=~p", [DeviceId, Reason])
    catch
        exit:{noproc, {gen_server, call, _}} ->
            ?LOG(warning, "⚠️  自动化测试器进程不存在，忽略设备上线事件: DeviceId=~p", [DeviceId]);
        _Exit:_Reason ->
            %% 临时屏蔽自动化测试器异常日志，避免日志刷屏
            %% ?LOG(error, "❌ 自动化测试器调用异常: DeviceId=~p, Exit=~p, Reason=~p", [DeviceId, Exit, Reason])
            ok
    end,
    
    {ok, State};

%% 设备离线消息处理
handle_message({device_offline, DeviceId}, State) ->
    ?LOG(info, "收到设备离线消息: DeviceId=~p", [DeviceId]),
    
    %% 通知自动化测试器，使用try...catch防止noproc异常导致进程崩溃
    try dgiot_uav_auto_tester:handle_device_offline(DeviceId) of
        ok ->
            ?LOG(info, "自动化测试器处理设备离线成功: DeviceId=~p", [DeviceId]);
        {error, Reason} ->
            ?LOG(warning, "自动化测试器处理设备离线失败: DeviceId=~p, Reason=~p", [DeviceId, Reason])
    catch
        exit:{noproc, {gen_server, call, _}} ->
            ?LOG(warning, "自动化测试器进程不存在，忽略设备离线事件: DeviceId=~p", [DeviceId]);
        _Exit:_Reason ->
            %% 临时屏蔽自动化测试器异常日志，避免日志刷屏
            %% ?LOG(error, "自动化测试器调用异常: DeviceId=~p, Exit=~p, Reason=~p", [DeviceId, Exit, Reason])
            ok
    end,
    
    {ok, State};

%% 启动测试消息
handle_message({start_test_for_device, DeviceId}, State) ->
    ?LOG(info, "收到启动测试消息: DeviceId=~p", [DeviceId]),
    
    %% 通知自动化测试器，使用try...catch防止noproc异常导致进程崩溃
    try dgiot_uav_auto_tester:start_test_for_device(DeviceId) of
        {ok, TestId} ->
            ?LOG(info, "自动化测试启动成功: DeviceId=~p, TestId=~p", [DeviceId, TestId]);
        {error, Reason} ->
            ?LOG(error, "自动化测试启动失败: DeviceId=~p, Reason=~p", [DeviceId, Reason])
    catch
        exit:{noproc, {gen_server, call, _}} ->
            ?LOG(warning, "自动化测试器进程不存在，忽略启动测试请求: DeviceId=~p", [DeviceId]);
        _Exit:_Reason ->
            %% 临时屏蔽自动化测试器异常日志，避免日志刷屏
            %% ?LOG(error, "自动化测试器调用异常: DeviceId=~p, Exit=~p, Reason=~p", [DeviceId, Exit, Reason])
            ok
    end,
    
    {ok, State};

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, ChannelId, #state{heartbeat_timer = HeartbeatTimer, client_monitors = Monitors}) ->
    ?LOG(error, "~ts", [<<"停止UAV PLC通道"/utf8>>]),
    
    %% 取消心跳定时器
    if 
        HeartbeatTimer =/= undefined -> 
            erlang:cancel_timer(HeartbeatTimer);
        true -> 
            ok
    end,
    
    %% 清理所有监控
    maps:foreach(fun(_ClientId, {_Pid, MonitorRef}) ->
        erlang:demonitor(MonitorRef, [flush])
    end, Monitors),
    
    %% 停止所有客户端
    lists:foreach(fun(#{station_id := StationId} ) ->
                BinId  = dgiot_utils:to_binary(StationId),
                dgiot_client:stop(ChannelId, <<"plc", "_", BinId/binary>>)
            end, ?STATION_CONFIGS),
    dgiot_data:delete({start_client, ChannelId}),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

init_two_stage_registration_ets() ->
    dgiot_data:init(uav_device_registration),
    dgiot_data:init(uav_ip_station_mapping),
    dgiot_data:init(uav_fixture_station_mapping),
    init_fixture_station_mappings(),
    ?LOG(info, "二次注册ETS表初始化完成").

init_fixture_station_mappings() ->
    %% 使用 ?STATION_CONFIGS 中的配置数据初始化治具工位映射
    lists:foreach(fun(StationConfig) ->
        StationId = maps:get(station_id, StationConfig),
        FixtureAddr = maps:get(fixture_address, StationConfig),
        StationName = maps:get(station_name, StationConfig),
        BaseAddr = maps:get(base_address, StationConfig),
        Ip = maps:get(ip, StationConfig),
        
        %% 构建完整的映射信息
        Mapping = #{
            fixture_address => FixtureAddr,
            station_id => StationId,
            station_name => StationName,
            base_address => BaseAddr,
            ip => Ip,
            port => maps:get(port, StationConfig, 502),
            instruction_set => maps:get(instruction_set, StationConfig, <<"">>),
            commands => maps:get(commands, StationConfig, []),
            command_interval => maps:get(command_interval, StationConfig, 1000)
        },
        dgiot_data:insert(uav_fixture_station_mapping, FixtureAddr, Mapping),
        
        %% 同时插入第二个治具地址（如果有且不等于第一个地址）
        case maps:get(fixture_address1, StationConfig, undefined) of
            undefined -> 
                ok;
            FixtureAddr1 when FixtureAddr1 =/= undefined andalso FixtureAddr1 =/= FixtureAddr ->
                Mapping1 = Mapping#{fixture_address => FixtureAddr1},
                dgiot_data:insert(uav_fixture_station_mapping, FixtureAddr1, Mapping1);
            _ ->
                %% 如果fixture_address1等于fixture_address或为其他无效值，跳过
                ok
        end
    end, ?STATION_CONFIGS),
    
    Count = length(?STATION_CONFIGS) * 2,  % 每个工位可能有2个治具地址
    ?LOG(info, "初始化治具工位地址映射完成，共~p个映射", [Count]).

get_station_by_fixture_addr(FixtureAddr) ->
    case dgiot_data:lookup(uav_fixture_station_mapping, FixtureAddr) of
        {ok, Map} -> {ok, Map};
        {error, not_find} -> {error, not_find}
    end.

%%%===================================================================
%%% 守护机制辅助函数
%%%===================================================================

%% @doc 根据PID和MonitorRef查找客户端ID
find_client_by_pid_and_ref(Pid, MonitorRef, Monitors) ->
    maps:fold(fun
        (ClientId, {ClientPid, ClientMonitorRef}, not_found) when ClientPid =:= Pid, ClientMonitorRef =:= MonitorRef ->
            {ok, ClientId};
        (_, _, Acc) ->
            Acc
    end, not_found, Monitors).

%% @doc 移除客户端的监控
remove_monitor(ClientId, Monitors) ->
    case maps:get(ClientId, Monitors, undefined) of
        {_Pid, MonitorRef} ->
            %% 取消监控
            erlang:demonitor(MonitorRef, [flush]),
            maps:remove(ClientId, Monitors);
        undefined ->
            Monitors
    end.

%% @doc 重启客户端
restart_client(ChannelId, ClientId, Monitors) ->
    %% 首先移除旧的监控
    Monitors1 = remove_monitor(ClientId, Monitors),
    
    %% 从ClientId中提取工位ID（格式："plc_1100"）
    case binary:split(ClientId, <<"_">>) of
        [<<"plc">>, StationIdBin] ->
            StationId = binary_to_integer(StationIdBin),
            %% 查找工位配置
            case find_station_config(StationId) of
                {ok, StationConfig} ->
                    #{ip := Ip, port := Port, commands := Commands, command_interval := Interval} = StationConfig,
                    
                    %% 重启客户端
                    StartResult = dgiot_client:start(ChannelId, ClientId, #{
                        <<"ip">> => Ip,
                        <<"port">> => Port,
                        <<"child">> => StationConfig#{commands => Commands, command_interval => Interval}
                    }),
                    
                    case StartResult of
                        {ok, Pid} when is_pid(Pid) ->
                            %% 建立新的监控
                            MonitorRef = erlang:monitor(process, Pid),
                            ?LOG(info, "客户端重启成功: ClientId=~p, Pid=~p", [ClientId, Pid]),
                            {ok, Monitors1#{ClientId => {Pid, MonitorRef}}};
                        Error ->
                            ?LOG(error, "客户端重启失败: ClientId=~p, Error=~p", [ClientId, Error]),
                            {error, Error}
                    end;
                {error, not_found} ->
                    ?LOG(error, "找不到工位配置，无法重启客户端: StationId=~p", [StationId]),
                    {error, station_config_not_found}
            end;
        _ ->
            ?LOG(error, "无效的客户端ID格式: ~p", [ClientId]),
            {error, invalid_client_id}
    end.

%% @doc 根据工位ID查找配置
find_station_config(StationId) ->
    lists:foldl(fun
        (StationConfig, not_found) ->
            case maps:get(station_id, StationConfig, undefined) of
                StationId -> {ok, StationConfig};
                _ -> not_found
            end;
        (_, Acc) -> Acc
    end, not_found, ?STATION_CONFIGS).

%% @doc 重建所有客户端的进程监控
rebuild_client_monitors(ChannelId, Monitors) ->
    maps:fold(fun(ClientId, {_OldPid, _OldMonitorRef}, Acc) ->
        %% 从ClientId中提取工位ID
        case binary:split(ClientId, <<"_">>) of
            [<<"plc">>, _StationIdBin] ->
                %% 尝试获取客户端PID
                case dgiot_client:get(ChannelId, ClientId) of
                    {ok, Pid} when is_pid(Pid) ->
                        %% 建立新的监控
                        case is_process_alive(Pid) of
                            true ->
                                MonitorRef = erlang:monitor(process, Pid),
                                ?LOG(info, "重建客户端监控: ClientId=~p, Pid=~p", [ClientId, Pid]),
                                Acc#{ClientId => {Pid, MonitorRef}};
                            false ->
                                ?LOG(warning, "客户端进程已死亡，移除监控: ClientId=~p", [ClientId]),
                                Acc
                        end;
                    _ ->
                        ?LOG(warning, "找不到客户端进程，移除监控: ClientId=~p", [ClientId]),
                        Acc
                end;
            _ ->
                ?LOG(warning, "跳过无效客户端ID: ~p", [ClientId]),
                Acc
        end
    end, #{}, Monitors).