%%--------------------------------------------------------------------
%% @doc 治具控制器主模块
%%--------------------------------------------------------------------
-module(dgiot_fixture_controller).
-author("johnliu").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

%% API
-export([
    %% 委托给命令模块
    control_power_relay_on/1, control_power_relay_off/1,
    start_drone/1, stop_drone/1,
    block_wind_tube/1, open_wind_tube/1,
    test_fuse_9_10_resistance/1, test_fuse_7_8_resistance/1,
    test_fuse_7_wing_nail_resistance/1, test_fuse_8_wing_nail_resistance/1,
    test_battery_port_resistance/1,
    test_fuse_5_ground_voltage/1, test_fuse_1_ground_voltage/1,
    read_station_info/1, build_read_station_info/1,
    check_communication/1, start_test/1, end_test/1,
    
    %% 委托给指标模块
    registers_to_resistance/1, register_to_voltage/1,
    resistance_to_registers/1, voltage_to_register/1,
    
    %% 核心处理函数
    handle_tcp_data/2, handle_port_data/4,
    handle_communication_check/2,
    start_communication_check/1, start_test_flow/1, end_test_flow/1,
    
    %% 测试相关
    get_test_items/0, next_test_command/2, handle_test_response/2,
    read_all_fixture_metrics/1, test/0
]).

%% 导出供测试使用
-export([init_state/1, handle_periodic_read/3, handle_init_timeout/3]).

%%====================================================================
%% 委托给命令模块
%%====================================================================
control_power_relay_on(S) -> dgiot_fixture_commands:control_power_relay_on(S).
control_power_relay_off(S) -> dgiot_fixture_commands:control_power_relay_off(S).
start_drone(S) -> dgiot_fixture_commands:start_drone(S).
stop_drone(S) -> dgiot_fixture_commands:stop_drone(S).
block_wind_tube(S) -> dgiot_fixture_commands:block_wind_tube(S).
open_wind_tube(S) -> dgiot_fixture_commands:open_wind_tube(S).
test_fuse_9_10_resistance(S) -> dgiot_fixture_commands:test_fuse_9_10_resistance(S).
test_fuse_7_8_resistance(S) -> dgiot_fixture_commands:test_fuse_7_8_resistance(S).
test_fuse_7_wing_nail_resistance(S) -> dgiot_fixture_commands:test_fuse_7_wing_nail_resistance(S).
test_fuse_8_wing_nail_resistance(S) -> dgiot_fixture_commands:test_fuse_8_wing_nail_resistance(S).
test_battery_port_resistance(S) -> dgiot_fixture_commands:test_battery_port_resistance(S).
test_fuse_5_ground_voltage(S) -> dgiot_fixture_commands:test_fuse_5_ground_voltage(S).
test_fuse_1_ground_voltage(S) -> dgiot_fixture_commands:test_fuse_1_ground_voltage(S).
read_station_info(S) -> dgiot_fixture_commands:read_station_info(S).
build_read_station_info(S) -> dgiot_fixture_commands:build_read_station_info(S).
check_communication(S) -> dgiot_fixture_commands:check_communication(S).
start_test(S) -> dgiot_fixture_commands:start_test(S).
end_test(S) -> dgiot_fixture_commands:end_test(S).

registers_to_resistance(R) -> dgiot_fixture_metrics:registers_to_resistance(R).
register_to_voltage(R) -> dgiot_fixture_metrics:register_to_voltage(R).
resistance_to_registers(R) -> dgiot_fixture_metrics:resistance_to_registers(R).
voltage_to_register(V) -> dgiot_fixture_metrics:voltage_to_register(V).

%%====================================================================
%% 初始化
%%====================================================================
init_state(SlaveId) ->
    Cmd = read_station_info(SlaveId),
    State = #uav_state{
        id = <<"fixture">>,
        port = 10006,
        product_id = <<"6235befb62">>,
        device_id = <<"fixture">>,
        station_addr = undefined,
        drone_powered = false
    },
    {Cmd, State}.

handle_periodic_read(_SlaveId, Tcp, State) -> {Tcp, State, noreply}.
handle_init_timeout(_Step, Tcp, State) -> {Tcp, State, noreply}.

%%====================================================================
%% TCP数据入口
%%====================================================================
-spec handle_tcp_data(binary(), {#tcp{}, #uav_state{}}) -> 
    {#tcp{}, #uav_state{}, noreply}.
handle_tcp_data(Data, {TCPState, UavState}) ->
    Combined = <<(TCPState#tcp.buff)/binary, Data/binary>>,
    IpBin = UavState#uav_state.ip_bin,
    
    case classify_data(Combined) of
        {heartbeat, StationNum} ->
            handle_heartbeat(StationNum, TCPState, UavState, IpBin);
        {registration, Type, Rest} ->
            handle_registration(Type, Rest, TCPState, UavState, IpBin);
        {modbus, BinData} ->
            handle_modbus_data(BinData, TCPState, UavState, IpBin);
        {sticky_frames, BinData} ->
            handle_sticky_frames(BinData, TCPState, UavState, IpBin)
    end.

%%====================================================================
%% 数据分类
%%====================================================================
classify_data(Data) ->
    %% 调试日志：检测EB90同步头
    case Data of
        <<16#EB, 16#90, _/binary>> ->
            ?LOG(warning, "[FIXTURE] EB90数据进入夹具控制器，可能路由错误: ~s", [dgiot_utils:binary_to_hex(Data)]);
        _ ->
            ok
    end,
    case Data of
        %% 心跳: danpianjiX
        <<"danpianji", StationBin/binary>> when byte_size(StationBin) > 0 ->
            StationNum = parse_station_number(StationBin),
            {heartbeat, StationNum};
        %% 注册: wrj_danpianji
        <<"wrj_danpianji\n", Rest/binary>> ->
            {registration, danpianji, Rest};
        <<"wrj_danpianji", Rest/binary>> ->
            {registration, danpianji, Rest};
        <<"wrj_dicekou\n", Rest/binary>> ->
            {registration, dicekou, Rest};
        <<"wrj_dicekou", Rest/binary>> ->
            {registration, dicekou, Rest};
        %% 8字节倍数可能是粘包帧
        Data when byte_size(Data) >= 8, byte_size(Data) rem 8 =:= 0 ->
            {sticky_frames, Data};
        %% 其他数据
        _ ->
            {modbus, Data}
    end.

parse_station_number(StationBin) ->
    %% 去掉可能的换行符
    Len = byte_size(StationBin),
    case binary:last(StationBin) of
        $\n -> binary_to_integer(binary:part(StationBin, 0, Len - 1));
        _ -> binary_to_integer(StationBin)
    end.

%%====================================================================
%% 心跳处理
%%====================================================================
handle_heartbeat(StationNum, TCPState, UavState, IpBin) ->
    update_heartbeat(IpBin),
    
    case StationNum of
        0 ->
            ?LOG(debug, "[HB] 工位0(未绑定) - IP:~s", [IpBin]),
            {TCPState, UavState, noreply};
        _ when StationNum > 0 ->
            handle_heartbeat_with_station(StationNum, TCPState, UavState, IpBin)
    end.

handle_heartbeat_with_station(StationNum, TCPState, UavState, IpBin) ->
    LastStation = get({current_station, IpBin}),
    
    case LastStation =:= StationNum of
        true ->
            ?LOG(debug, "[HB] 工位~p心跳 - IP:~s", [StationNum, IpBin]),
            {TCPState, UavState, noreply};
        false ->
            ?LOG(info, "[HB] 工位变化: ~p -> ~p - IP:~s", [LastStation, StationNum, IpBin]),
            register_and_power_on(StationNum, TCPState, UavState, IpBin)
    end.

update_heartbeat(IpBin) ->
    put({fixture_heartbeat, IpBin}, erlang:system_time(millisecond)),
    put({fixture_connected, IpBin}, true).

%%====================================================================
%% 注册处理
%%====================================================================
handle_registration(danpianji, Rest, TCPState, UavState, IpBin) ->
    ?LOG(info, "[REG] 治具单片机上线 - IP:~s", [IpBin]),
    
    %% 设置连接状态
    put({fixture_connected, IpBin}, true),
    update_heartbeat(IpBin),
    start_heartbeat_monitor(IpBin, self()),
    
    %% 触发上线流程
    trigger_online_flow(IpBin, TCPState),
    
    %% 更新状态
    NewTCPState = TCPState#tcp{
        clientid = UavState#uav_state.device_id,
        buff = ensure_binary(Rest)
    },
    {NewTCPState, UavState, noreply};
    
handle_registration(dicekou, Rest, TCPState, UavState, _IpBin) ->
    ?LOG(info, "[REG] 地测口注册"),
    {TCPState#tcp{buff = ensure_binary(Rest)}, UavState, noreply}.

trigger_online_flow(IpBin, TCPState) ->
    %% 步骤1: 触发后来者上报
    try
        dgiot_uav_station_manager:trigger_mes_report_for_connected_drone(IpBin, self())
    catch _:E -> ?LOG(error, "[REG] 上报触发失败: ~p", [E]) end,
    
    %% 步骤2: 读取工位地址
    send_read_station_command(TCPState),
    
    %% 步骤3: 预注册IP映射
    pre_register_by_ip(IpBin).

send_read_station_command(TCPState) ->
    SlaveId = ?DEFAULT_SLAVE_ID,
    Cmd = dgiot_fixture_commands:read_station_info(SlaveId),
    gen_tcp:send(TCPState#tcp.socket, Cmd),
    ?LOG(info, "[REG] 发送读工位命令 - Hex:~s", [dgiot_utils:binary_to_hex(Cmd)]).

pre_register_by_ip(IpBin) ->
    case dgiot_uav_business_service:get_station_by_ip(IpBin) of
        {ok, StationAddr} ->
            dgiot_uav_business_service:register_station_fixture(StationAddr, self()),
            ?LOG(info, "[REG] IP预注册成功 - IP:~s -> 工位~p", [IpBin, StationAddr]);
        {error, not_find} ->
            ?LOG(debug, "[REG] IP未映射 - IP:~s", [IpBin])
    end.

%%====================================================================
%% Modbus数据处理
%%====================================================================
handle_modbus_data(Data, TCPState, UavState, IpBin) ->
    case dgiot_modbus_client:parse_response(Data, 16#03) of
        {ok, [StationAddr | _]} when is_integer(StationAddr), StationAddr > 0 ->
            handle_station_response(StationAddr, TCPState, UavState, IpBin);
        {ok, _} ->
            {TCPState, UavState, noreply};
        {error, Reason} ->
            log_modbus_error(Data, Reason, IpBin),
            {TCPState, UavState, noreply}
    end.

handle_station_response(StationAddr, TCPState, UavState, IpBin) ->
    case get({fixture_connected, IpBin}) of
        true -> process_station_address(StationAddr, TCPState, UavState, IpBin);
        false -> 
            ?LOG(debug, "[MODBUS] 治具未连接，忽略工位~p", [StationAddr]),
            {TCPState, UavState, noreply};
        undefined -> 
            ?LOG(warning, "[MODBUS] 治具连接状态未定义，忽略工位~p", [StationAddr]),
            {TCPState, UavState, noreply}
    end.

process_station_address(StationAddr, TCPState, UavState, IpBin) ->
    case is_valid_station(StationAddr) of
        false ->
            ?LOG(warning, "[STATION] 无效工位地址:~p", [StationAddr]),
            clear_station_cache(IpBin),
            {TCPState, UavState, noreply};
        true ->
            process_valid_station(StationAddr, TCPState, UavState, IpBin)
    end.

is_valid_station(Addr) -> Addr > 0 andalso Addr =/= 255.

process_valid_station(StationAddr, TCPState, UavState, IpBin) ->
    CurrentTime = erlang:system_time(millisecond),
    LastPrinted = get({station_addr_printed, IpBin}),
    LastTs = get({station_addr_timestamp, IpBin}),
    
    case should_process_station(StationAddr, LastPrinted, LastTs, CurrentTime) of
        false ->
            safe_register(StationAddr),
            {TCPState, UavState, noreply};
        true ->
            activate_station(StationAddr, TCPState, UavState, IpBin, LastPrinted, CurrentTime)
    end.

should_process_station(StationAddr, LastPrinted, LastTs, Now) ->
    IsExpired = case LastTs of undefined -> true; _ -> (Now - LastTs) > 5000 end,
    (LastPrinted =/= StationAddr) orelse IsExpired.

activate_station(StationAddr, TCPState, UavState, IpBin, LastPrinted, Now) ->
    put({station_addr_printed, IpBin}, StationAddr),
    put({station_addr_timestamp, IpBin}, Now),
    
    SameAddr = case LastPrinted of
        undefined -> undefined;
        _ when LastPrinted =:= StationAddr -> true;
        _ -> false
    end,
    log_station_activation(StationAddr, IpBin, SameAddr),
    
    try
        dgiot_uav_station_manager:set_station_ip(IpBin, StationAddr),
        dgiot_uav_business_service:register_station_fixture(StationAddr, self()),
        execute_power_sequence(StationAddr, TCPState, SameAddr)
    catch _:Err ->
        ?LOG(error, "[STATION] 激活失败 - 工位~p: ~p", [StationAddr, Err])
    end,
    {TCPState, UavState, noreply}.

%%====================================================================
%% 粘包处理
%%====================================================================
handle_sticky_frames(Data, TCPState, UavState, IpBin) ->
    handle_sticky_frames_loop(Data, TCPState, UavState, IpBin, <<>>).

handle_sticky_frames_loop(<<>>, TCPState, UavState, _IpBin, Buff) ->
    {ok, TCPState#tcp{buff = Buff}, UavState, noreply};
    
handle_sticky_frames_loop(Data, TCPState, UavState, _IpBin, Buff) when byte_size(Data) < 8 ->
    {ok, TCPState#tcp{buff = <<Buff/binary, Data/binary>>}, UavState, noreply};
    
handle_sticky_frames_loop(Data, TCPState, UavState, IpBin, Buff) ->
    <<Frame:8/binary, Rest/binary>> = Data,
    
    case parse_sticky_frame(Frame, IpBin) of
        {ok, StationAddr} ->
            ?LOG(debug, "[STICKY] 解析工位~p", [StationAddr]),
            %% 递归处理剩余数据
            handle_sticky_frames_loop(Rest, TCPState, UavState, IpBin, Buff);
        {error, _} ->
            handle_sticky_frames_loop(Rest, TCPState, UavState, IpBin, Buff)
    end.

parse_sticky_frame(<<ModbusPart:7/binary, StationAddr:8>>, IpBin) ->
    case dgiot_modbus_client:parse_response(ModbusPart, 16#03) of
        {ok, _Registers} when StationAddr > 0, StationAddr =/= 255 ->
            case get({fixture_connected, IpBin}) of
                true -> {ok, StationAddr};
                false -> {error, not_connected}
            end;
        _ ->
            {error, parse_failed}
    end.

%%====================================================================
%% 上电流程
%%====================================================================
execute_power_sequence(StationAddr, TCPState, LastPrinted) ->
    case StationAddr of
        0 -> execute_drone_power(StationAddr, TCPState, 1000, LastPrinted);
        _ ->
            execute_fixture_power(StationAddr, TCPState, LastPrinted),
            execute_drone_power(StationAddr, TCPState, 2000, LastPrinted)
    end.

execute_fixture_power(StationAddr, TCPState, LastPrinted) ->
    log_power_action("治具上电", StationAddr, LastPrinted),
    spawn_power_cmd(StationAddr, 
        fun() -> dgiot_fixture_commands:control_power_relay_on(StationAddr) end,
        TCPState, 1000).

execute_drone_power(StationAddr, TCPState, DelayMs, LastPrinted) ->
    log_power_action("飞机上电", StationAddr, LastPrinted),
    spawn_power_cmd(StationAddr,
        fun() -> dgiot_fixture_commands:start_drone(StationAddr) end,
        TCPState, DelayMs).

spawn_power_cmd(_StationAddr, CmdFun, TCPState, Delay) ->
    spawn(fun() ->
        timer:sleep(Delay),
        case CmdFun() of
            Cmd when is_binary(Cmd) ->
                gen_tcp:send(TCPState#tcp.socket, Cmd);
            _ -> ok
        end
    end).

log_power_action(Action, StationAddr, SameAddr) ->
    Type = case SameAddr of
        true -> "复用";
        false -> "新工位";
        undefined -> "新工位"
    end,
    ?LOG(info, "[POWER] ~s - ~s工位~p", [Action, Type, StationAddr]).

log_station_activation(Addr, IpBin, SameAddr) ->
    case SameAddr of
        true -> ?LOG(info, "[STATION] 缓存过期重新激活 - 工位~p", [Addr]);
        false -> ?LOG(info, "[STATION] 新工位~p上线 - IP:~s", [Addr, IpBin]);
        undefined -> ?LOG(info, "[STATION] 首次激活工位~p - IP:~s", [Addr, IpBin]);
        _ -> ?LOG(info, "[STATION] 工位~p激活 (SameAddr=~p) - IP:~s", [Addr, SameAddr, IpBin])
    end.

%%====================================================================
%% 心跳监控
%%====================================================================
start_heartbeat_monitor(IpBin, Pid) ->
    spawn(fun() -> check_heartbeat(IpBin, Pid) end).

check_heartbeat(IpBin, _Pid) ->
    timer:sleep(2500),
    case get({fixture_heartbeat, IpBin}) of
        undefined -> ok;
        Last ->
            case erlang:system_time(millisecond) - Last of
                Elapsed when Elapsed > 2500 ->
                    ?LOG(warning, "[HB] 心跳超时离线 - IP:~s, 超时:~pms", [IpBin, Elapsed]),
                    clear_connection_state(IpBin);
                _ ->
                    check_heartbeat(IpBin, _Pid)
            end
    end.

clear_connection_state(IpBin) ->
    put({fixture_connected, IpBin}, false),
    put({current_station, IpBin}, undefined),
    put({fixture_heartbeat, IpBin}, undefined).

clear_station_cache(IpBin) ->
    put({station_addr_printed, IpBin}, undefined),
    put({station_addr_timestamp, IpBin}, undefined).

safe_register(StationAddr) ->
    try dgiot_uav_business_service:register_station_fixture(StationAddr, self())
    catch _:_ -> ok end.

register_and_power_on(StationAddr, TCPState, UavState, IpBin) ->
    put({current_station, IpBin}, StationAddr),
    dgiot_uav_business_service:register_station_fixture(StationAddr, self()),
    spawn(fun() -> execute_power_sequence(StationAddr, TCPState, false) end),
    {TCPState, UavState, noreply}.

%%====================================================================
%% 通讯检测
%%====================================================================
start_communication_check(StationAddr) ->
    dgiot_fixture_state_manager:reset_fixture_state(StationAddr),
    dgiot_fixture_state_manager:start_communication_check(StationAddr, self()).

handle_communication_check(StationAddr, Socket) ->
    {ok, Cmd} = dgiot_fixture_commands:check_communication(?DEFAULT_SLAVE_ID),
    gen_tcp:send(Socket, Cmd),
    dgiot_fixture_state_manager:start_communication_check(StationAddr, self()),
    dgiot_fixture_state_manager:set_communication_state(StationAddr, online).

start_test_flow(StationAddr) ->
    dgiot_fixture_state_manager:stop_communication_check(StationAddr),
    dgiot_fixture_state_manager:set_test_state(StationAddr, testing),
    send_test_command(StationAddr, start_test, "启动测试").

end_test_flow(StationAddr) ->
    dgiot_fixture_state_manager:set_test_state(StationAddr, completed),
    send_test_command(StationAddr, end_test, "结束测试").

send_test_command(StationAddr, CmdFun, Desc) ->
    Cmd = dgiot_fixture_commands:CmdFun(?DEFAULT_SLAVE_ID),
    case get(fixture_socket) of
        Socket when is_port(Socket) ->
            gen_tcp:send(Socket, Cmd),
            ?LOG(info, "[TEST] ~s - 工位~p, Hex:~s", [Desc, StationAddr, dgiot_utils:binary_to_hex(Cmd)]);
        _ ->
            ?LOG(error, "[TEST] ~s失败 - 工位~p, Socket未找到", [Desc, StationAddr])
    end.

%%====================================================================
%% 测试项定义
%%====================================================================
get_test_items() ->
    [
        {1, "保险丝9-10电阻", fun test_fuse_9_10_resistance/1},
        {2, "保险丝7-8电阻", fun test_fuse_7_8_resistance/1},
        {3, "保险丝7翼钉电阻", fun test_fuse_7_wing_nail_resistance/1},
        {4, "保险丝8翼钉电阻", fun test_fuse_8_wing_nail_resistance/1},
        {5, "电池端口电阻", fun test_battery_port_resistance/1},
        {6, "保险丝5对地电压", fun test_fuse_5_ground_voltage/1},
        {7, "保险丝1对地电压", fun test_fuse_1_ground_voltage/1}
    ].

next_test_command(CurrentStep, SlaveId) ->
    case lists:keyfind(CurrentStep + 1, 1, get_test_items()) of
        {_, _, CmdFun} -> CmdFun(SlaveId);
        false -> {error, no_more_tests}
    end.

handle_test_response(Step, Result) ->
    ?LOG(info, "[TEST] 步骤~p完成: ~p", [Step, Result]),
    {ok, completed}.

read_all_fixture_metrics(SlaveId) ->
    [Cmd || {_, _, F} <- get_test_items(), (Cmd = F(SlaveId)) /= undefined].

%%====================================================================
%% 工具函数
%%====================================================================
handle_port_data(NewBuf, TCPState, UavState, Socket) ->
    case handle_tcp_data(NewBuf, {TCPState, UavState}) of
        {NewTCP, NewUav, noreply} ->
            inet:setopts(Socket, [{active, once}]),
            {NewTCP, NewUav, noreply};
        {error, Reason} ->
            ?LOG(error, "端口数据处理失败: ~p", [Reason]),
            inet:setopts(Socket, [{active, once}]),
            {TCPState, UavState, noreply};
        Other ->
            ?LOG(error, "handle_tcp_data返回意外值: ~p", [Other]),
            inet:setopts(Socket, [{active, once}]),
            {TCPState, UavState, noreply}
    end.

log_modbus_error(Data, Reason, IpBin) ->
    case is_ascii_data(Data) of
        true -> ?LOG(warning, "[MODBUS] ASCII数据被过滤 - IP:~s", [IpBin]);
        false -> ?LOG(error, "[MODBUS] 解析失败 - IP:~s, Hex:~s, Reason:~p",
            [IpBin, dgiot_utils:binary_to_hex(Data), Reason])
    end.

is_ascii_data(Data) when byte_size(Data) > 0 ->
    CheckSize = min(10, byte_size(Data)),
    <<CheckPart:CheckSize/binary, _/binary>> = Data,
    is_printable_ascii(CheckPart);
is_ascii_data(_) -> false.

is_printable_ascii(<<>>) -> true;
is_printable_ascii(<<C, Rest/binary>>) when C >= 32, C =< 126 -> is_printable_ascii(Rest);
is_printable_ascii(<<$\n, Rest/binary>>) -> is_printable_ascii(Rest);
is_printable_ascii(<<$\r, Rest/binary>>) -> is_printable_ascii(Rest);
is_printable_ascii(_) -> false.

ensure_binary(undefined) -> <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(_) -> <<>>.

test() ->
    ?LOG(info, "[TEST] 治具控制器测试函数执行"),
    ok.
