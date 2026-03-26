%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_tcp_worker - 无人机TCP工作进程（精简版）
%%% 负责TCP连接生命周期管理，将端口数据分发给对应的handler模块。
%%% 日志精简：将“station_id未设置”日志降为debug，避免干扰。
%%%-------------------------------------------------------------------
-module(dgiot_uav_tcp_worker).
-author("johnliu").

-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include("dgiot_uav.hrl").
-include("dgiot_uav_config.hrl").

-define(TYPE, <<"UAV_TCP">>).

%% TCP callback
-export([child_spec/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

%% 测试函数
-export([test_fixture_result/0]).

ensure_binary(undefined) -> <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(_) -> <<>>.

%% ==================== API ====================
child_spec(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, Port, State).

%% ==================== init ====================
init({tcp, Transport, Socket, _IsSsl, Buffer, {state, ChannelId, _Port, _ProductId, _Env}, _Registered, _Buff}) ->
    {ok, {ClientIP, ClientPort}} = inet:peername(Socket),
    IpStr = inet:ntoa(ClientIP),
    ?LOG(error, "tcp client 连接: ~s:~p, ChannelId=~p", [IpStr, ClientPort, ChannelId]),
    inet:setopts(Socket, [{active, once}]),

    {LoginId, ProductId, ChineseName, ShouldCreate, IsSurface} = determine_device_by_port(ClientPort, IpStr),
    ?LOG(error, "端口映射结果: LoginId=~s, ProductId=~s, 名称=~ts, ShouldCreate=~p, IsSurface=~p",
         [LoginId, ProductId, ChineseName, ShouldCreate, IsSurface]),

    DevAddr = list_to_binary(IpStr ++ "_" ++ integer_to_list(ClientPort)),
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),

    %% 为单片机启动 periodic_read 定时器（3秒）
    case LoginId of
        <<"wrj_danpianji">> ->
            PeriodicTimer = erlang:send_after(3000, self(), {periodic_read, ?DEFAULT_SLAVE_ID}),
            put(periodic_timer, PeriodicTimer);
        _ ->
            ok
    end,

    %% 初始化进程字典
    init_process_dict(IpStr, ClientPort, DevAddr, ProductId, LoginId, IsSurface),

    BaseUavState = #uav_state{
        id = ChannelId,
        port = ClientPort,
        product_id = ProductId,
        devaddr = DevAddr,
        ip_bin = list_to_binary(IpStr),
        drone_powered = false,
        station_addr = undefined,
        timer_ref = undefined,
        retry_count = 0
    },

    TCPState = build_tcp_state(ShouldCreate, LoginId, ProductId, ChineseName,
                               DeviceId, BaseUavState, Socket, Transport, Buffer, IpStr, ClientPort),
    {ok, TCPState};
init(Other) ->
    ?LOG(warning, "未知init参数: ~p", [Other]),
    {stop, badarg}.

init_process_dict(IpStr, ClientPort, DevAddr, ProductId, LoginId, IsSurface) ->
    put(aggregate_cache, #{}),
    put(station_bound, false),
    put(drone_id, undefined),
    put(fixture_addr, undefined),
    put(station_key, list_to_binary(IpStr ++ ":" ++ integer_to_list(ClientPort))),
    put(port, ClientPort),
    put(ip, IpStr),
    put(ip_bin, list_to_binary(IpStr)),
    put(product_id, ProductId),
    put(devaddr, DevAddr),
    put(login_id, LoginId),
    case IsSurface of true -> put(device_type_id, LoginId); false -> ok end.

%% 检查是否应该创建设备
should_create_device(IpStr, Port, ChineseName) ->
    %% 地测口设备不创建设备，从EB90帧提取飞机ID后创建
    case {Port, ChineseName} of
        {10007, <<"地测口"/utf8>>} ->
            %% 端口10007不创建设备，从EB90帧提取飞机ID后创建无人机设备
            ?LOG(info, "地测口不创建设备，从EB90帧提取飞机ID: IP=~s, Port=~p", [IpStr, Port]),
            false;
        {10007, <<"超近距无人机"/utf8>>} ->
            %% 端口10007上的超近距无人机是错误设备，不应该创建
            ?LOG(warning, "检测到错误的无人机设备，跳过创建: IP=~s, Port=~p, Name=~ts",
                 [IpStr, Port, ChineseName]),
            false;
        {10007, _} ->
            %% 端口10007上的其他设备，检查IP是否在错误的无人机IP范围内
            WrongIPs = ["192.168.100.45", "192.168.100.46", "192.168.100.47",
                       "192.168.100.48", "192.168.100.49"],
            case lists:member(IpStr, WrongIPs) of
                true ->
                    ?LOG(warning, "检测到错误IP的无人机设备，跳过创建: IP=~s, Port=~p, Name=~ts",
                         [IpStr, Port, ChineseName]),
                    false;
                false ->
                    true
            end;
        _ ->
            %% 其他端口正常创建
            true
    end.

build_tcp_state(true, LoginId, ProductId, ChineseName, DeviceId, BaseUavState,
                Socket, Transport, Buffer, IpStr, ClientPort) ->
    %% 检查是否为错误的无人机设备
    case should_create_device(IpStr, ClientPort, ChineseName) of
        false ->
            ?LOG(warning, "跳过设备创建: IP=~s, Port=~p, Name=~ts (错误的无人机设备)", 
                 [IpStr, ClientPort, ChineseName]),
            %% 仍然注册IP端口但不创建设备
            dgiot_uav_business_service:register_ip_port(self(), list_to_binary(IpStr), ClientPort, LoginId, ProductId),
            ?LOG(info, "注册IP端口但不创建设备: ~s:~p, LoginId: ~s", [IpStr, ClientPort, LoginId]),
            UpdatedUavState = case ClientPort of
                10007 ->
                    case dgiot_uav_business_service:is_fixture_completed(list_to_binary(IpStr)) of
                        true -> BaseUavState#uav_state{drone_powered = true};
                        false -> BaseUavState
                    end;
                _ ->
                    BaseUavState
            end,
            FinalUavState = UpdatedUavState#uav_state{
                device_id = DeviceId,
                protocol_state = init_protocol_state(LoginId, undefined)
            },
            #tcp{
                socket = Socket,
                buff = ensure_binary(Buffer),
                transport = Transport,
                clientid = DeviceId,
                register = true,
                state = FinalUavState
            };
        true ->
            ?LOG(error, "[TCP_WORKER] 开始创建设备: LoginId=~s, ProductId=~s, DevAddr=~s, IP=~s:~p",
                 [LoginId, ProductId, BaseUavState#uav_state.devaddr, IpStr, ClientPort]),
            case dgiot_uav_business_service:create_device(LoginId, ProductId, BaseUavState#uav_state.devaddr,
                                                           list_to_binary(IpStr), ChineseName) of
                {ok, DeviceObjectId} ->
                    ?LOG(error, "[TCP_WORKER] ✅ 设备创建成功: ~ts (~s) IP:~s:~p, ObjectId=~s", 
                         [ChineseName, BaseUavState#uav_state.devaddr, IpStr, ClientPort, DeviceObjectId]);
                {error, already_exists} ->
                    ?LOG(warning, "[TCP_WORKER] 设备已存在: ~ts (~s) IP:~s:~p", 
                         [ChineseName, BaseUavState#uav_state.devaddr, IpStr, ClientPort]);
                {error, Reason} ->
                    ?LOG(error, "[TCP_WORKER] ❌ 设备创建失败: ~ts (~s), Reason=~p", 
                         [ChineseName, BaseUavState#uav_state.devaddr, Reason])
            end,
            dgiot_uav_business_service:register_ip_port(self(), list_to_binary(IpStr), ClientPort, LoginId, ProductId),
            ?LOG(info, "注册IP端口: ~s:~p, LoginId: ~s", [IpStr, ClientPort, LoginId]),
            UpdatedUavState = case ClientPort of
                10007 ->
                    case dgiot_uav_business_service:is_fixture_completed(list_to_binary(IpStr)) of
                        true -> BaseUavState#uav_state{drone_powered = true};
                        false -> BaseUavState
                    end;
                _ ->
                    BaseUavState
            end,
            FinalUavState = UpdatedUavState#uav_state{
                device_id = DeviceId,
                protocol_state = init_protocol_state(LoginId, undefined)
            },
            #tcp{
                socket = Socket,
                buff = ensure_binary(Buffer),
                transport = Transport,
                clientid = DeviceId,
                register = true,
                state = FinalUavState
            }
    end;

build_tcp_state(false, LoginId, ProductId, _ChineseName, _DeviceId, BaseUavState,
                Socket, Transport, Buffer, IpStr, ClientPort) ->
    ?LOG(error, "设备 ~s 不创建设备，仅作为数据通道", [LoginId]),
    dgiot_uav_business_service:register_ip_port(self(), list_to_binary(IpStr), ClientPort, LoginId, ProductId),
    ?LOG(error, "注册IP端口: ~s:~p, LoginId: ~s", [IpStr, ClientPort, LoginId]),
    FinalUavState = BaseUavState#uav_state{
        device_id = undefined,
        protocol_state = init_protocol_state(LoginId, undefined)
    },
    #tcp{
        socket = Socket,
        buff = ensure_binary(Buffer),
        transport = Transport,
        clientid = LoginId,
        register = true,
        state = FinalUavState
    }.

init_protocol_state(LoginId, _UavState) ->
    case LoginId of
        <<"wrj_danpianji">> -> undefined;
        <<"wrj_dicekou">> -> {<<>>, dgiot_eb90_protocol:init_state()};
        _ -> undefined
    end.

determine_device_by_port(ClientPort, IpStr) ->
    case dgiot_uav_config:get_port_device_mapping(ClientPort) of
        {DeviceId, ProductId, DeviceName, ShouldCreate, IsSurface} ->
            case IsSurface of
                true -> start_surface_timer();
                false -> ok
            end,
            {DeviceId, ProductId, DeviceName, ShouldCreate, IsSurface};
        undefined ->
            ?LOG(warning, "未知端口 ~p, IP=~s", [ClientPort, IpStr]),
            {<<"unknown">>, <<"6235befb62">>, <<"未知设备"/utf8>>, false, false}
    end.

%% ==================== handle_info ====================
handle_info({tcp, Data}, TCPState = #tcp{socket = Socket, buff = OldBuf, state = UavState}) ->
    SafeOldBuf = ensure_binary(OldBuf),
    NewBuf = <<SafeOldBuf/binary, Data/binary>>,
    ?LOG(info, "[TCP RAW] 收到原始数据: ~p", [dgiot_utils:binary_to_hex(Data)]),
    case UavState#uav_state.port of
        10006 -> 
            {NewTCPState, NewUavState, Reply} = dgiot_fixture_controller:handle_port_data(NewBuf, TCPState, UavState, Socket),
            case Reply of
                noreply -> {noreply, NewTCPState#tcp{state = NewUavState}};
                {stop, Reason} -> {stop, Reason, NewTCPState#tcp{state = NewUavState}}
            end;
        Port when Port >= 10001, Port =< 10005 ->
            {NewTCPState, NewUavState, Reply} = dgiot_uav_surface_controller:handle_port_data(NewBuf, TCPState, UavState, Socket),
            case Reply of
                noreply -> {noreply, NewTCPState#tcp{state = NewUavState}};
                {stop, Reason} -> {stop, Reason, NewTCPState#tcp{state = NewUavState}}
            end;
        10007 ->
            ?LOG(info, "[TCP RAW] 端口10007收到原始数据: ~p", [dgiot_utils:binary_to_hex(Data)]),
            {NewTCPState, NewUavState, Reply} = dgiot_eb90_protocol:handle_port_data(NewBuf, TCPState, UavState, Socket),
            case Reply of
                noreply -> {noreply, NewTCPState#tcp{state = NewUavState}};
                {stop, Reason} -> {stop, Reason, NewTCPState#tcp{state = NewUavState}}
            end;
        1234 ->
            ?LOG(error, "[TCP RAW 1234] 扫描枪原始报文: ~p", [NewBuf]),
            {NewTCPState, NewUavState, Reply} = dgiot_scanner_protocol:handle_port_data(NewBuf, TCPState, UavState, Socket),
            case Reply of
                noreply -> {noreply, NewTCPState#tcp{state = NewUavState}};
                {stop, Reason} -> {stop, Reason, NewTCPState#tcp{state = NewUavState}}
            end;
        21000 ->
            {NewTCPState, NewUavState, Reply} = dgiot_noise_protocol:handle_port_data(NewBuf, TCPState, UavState, Socket),
            case Reply of
                noreply -> {noreply, NewTCPState#tcp{state = NewUavState}};
                {stop, Reason} -> {stop, Reason, NewTCPState#tcp{state = NewUavState}}
            end;
        _ ->
            ?LOG(warning, "未知端口 ~p，数据丢弃", [UavState#uav_state.port]),
            inet:setopts(Socket, [{active, once}]),
            {noreply, TCPState#tcp{buff = <<>>}}
    end;

handle_info({aggregate, Data}, TCPState = #tcp{state = UavState}) ->
    DroneId = UavState#uav_state.devaddr,
    ProductId = UavState#uav_state.product_id,
    ?LOG(info, "[AGGREGATE] 收到聚合数据，无人机=~s, ProductId=~s, 数据字段=~p",
         [DroneId, ProductId, maps:keys(Data)]),
    handle_aggregate(Data, DroneId, ProductId, TCPState),
    {noreply, TCPState};

handle_info({aggregate, DroneId, Data}, TCPState = #tcp{state = UavState}) ->
    ProductId = UavState#uav_state.product_id,
    ?LOG(info, "[AGGREGATE] 收到聚合数据，无人机=~s, ProductId=~s, 数据字段=~p",
         [DroneId, ProductId, maps:keys(Data)]),
    handle_aggregate(Data, DroneId, ProductId, TCPState),
    NewUavState = UavState#uav_state{devaddr = DroneId},
    {noreply, TCPState#tcp{state = NewUavState}};

handle_info({drone_powered, StationAddr}, TCPState = #tcp{state = UavState}) ->
    ?LOG(info, "地测口收到二次注册: 工位地址=~p", [StationAddr]),
    NewUavState = UavState#uav_state{drone_powered = true, station_addr = StationAddr},
    DroneId = UavState#uav_state.devaddr,
    put(station_id, StationAddr),
    put(drone_id, DroneId),

    %% 设置设备为在线状态
    case DroneId of
        <<>> ->
            ?LOG(warning, "无人机设备ID为空，无法设置在线状态");
        _ ->
            case dgiot_device:lookup(DroneId) of
                {ok, _Device} ->
                    DeviceStatus = #{<<"status">> => <<"online">>},
                    case dgiot_parse:update_object(<<"Device">>, DroneId, DeviceStatus) of
                        {ok, _} ->
                            ?LOG(info, "无人机 ~s 上电成功，状态设为online", [DroneId]);
                        {error, Reason} ->
                            ?LOG(error, "设置无人机 ~s 在线状态失败: ~p", [DroneId, Reason])
                    end;
                {error, not_find} ->
                    ?LOG(warning, "未找到无人机设备: ~p", [DroneId])
            end
    end,

    {noreply, TCPState#tcp{state = NewUavState}};

handle_info({drone_powered, false}, #tcp{state = _UavState} = TCPState) ->
    ?LOG(info, "地测口收到下电通知"),
    DroneId = _UavState#uav_state.devaddr,
    NewUavState = _UavState#uav_state{drone_powered = false, station_addr = undefined},

    %% 设置设备为离线状态
    case DroneId of
        <<>> ->
            ?LOG(warning, "无人机设备ID为空，无法设置离线状态");
        _ ->
            case dgiot_device:lookup(DroneId) of
                {ok, _Device} ->
                    DeviceStatus = #{<<"status">> => <<"offline">>},
                    case dgiot_parse:update_object(<<"Device">>, DroneId, DeviceStatus) of
                        {ok, _} ->
                            ?LOG(info, "无人机 ~s 下电成功，状态设为offline", [DroneId]);
                        {error, Reason} ->
                            ?LOG(error, "设置无人机 ~s 离线状态失败: ~p", [DroneId, Reason])
                    end;
                {error, not_find} ->
                    ?LOG(warning, "未找到无人机设备: ~p", [DroneId])
            end
    end,

    {noreply, TCPState#tcp{state = NewUavState}};


handle_info({bind_station, DroneId, StationInfo}, TCPState = #tcp{state = _UavState}) ->
    ?LOG(info, "无人机 ~s 绑定工位", [DroneId]),
    bind_station(DroneId, StationInfo),
    {noreply, TCPState};

handle_info({send_fixture_command, ModbusSlaveId, FunctionCode, RegisterAddr, ValueToWrite}, TCPState = #tcp{socket = Socket, state = UavState}) ->
    case UavState#uav_state.port of
        10006 ->
            ?LOG(info, "发送治具Modbus命令: SlaveId=~p, FunctionCode=~p, RegisterAddr=~p, ValueToWrite=~p", 
                 [ModbusSlaveId, FunctionCode, RegisterAddr, ValueToWrite]),
            Command = dgiot_modbus_client:encode_command(ModbusSlaveId, FunctionCode, RegisterAddr, ValueToWrite),
            case gen_tcp:send(Socket, Command) of
                ok -> 
                    ?LOG(info, "治具Modbus命令发送成功");
                {error, Reason} ->
                    ?LOG(error, "治具Modbus命令发送失败: ~p", [Reason])
            end;
        _ ->
            ?LOG(warning, "收到治具命令但端口不是10006: ~p", [UavState#uav_state.port])
    end,
    {noreply, TCPState};

handle_info({periodic_read, SlaveId}, TCPState = #tcp{state = UavState}) ->
    dgiot_fixture_controller:handle_periodic_read(SlaveId, TCPState, UavState),
    {noreply, TCPState};

handle_info({init_timeout, Step}, TCPState = #tcp{state = UavState}) ->
    {NewTCPState, NewUavState} = dgiot_fixture_controller:handle_init_timeout(Step, TCPState, UavState),
    {noreply, NewTCPState#tcp{state = NewUavState}};

handle_info(read_surface, TCPState = #tcp{state = _UavState}) ->
    dgiot_uav_surface_controller:handle_read_surface(TCPState),
    {noreply, TCPState};

handle_info({tcp_closed, Socket}, #tcp{clientid = ClientId, socket = Socket} = TCPState) ->
    case inet:peername(Socket) of
        {ok, {ClientIP, ClientPort}} ->
            IpStr = inet:ntoa(ClientIP),
            ?LOG(warning, "TCP连接断开: IP=~s:~p, ChannelId=~ts, Reason=客户端主动关闭",
                  [IpStr, ClientPort, ClientId]);
        {error, _} ->
            ?LOG(warning, "TCP连接断开: ChannelId=~ts, Socket=~p, Reason=客户端主动关闭", [ClientId, Socket])
    end,
    {stop, normal, TCPState#tcp{buff = <<>>}};

handle_info({tcp_error, Socket, Reason}, #tcp{clientid = ClientId, socket = Socket} = TCPState) ->
    case inet:peername(Socket) of
        {ok, {ClientIP, ClientPort}} ->
            IpStr = inet:ntoa(ClientIP),
            ?LOG(error, "TCP连接错误: IP=~s:~p, ChannelId=~ts, Reason=~p", [IpStr, ClientPort, ClientId, Reason]);
        {error, _} ->
            ?LOG(error, "TCP连接错误: ChannelId=~ts, Socket=~p, Reason=~p", [ClientId, Socket, Reason])
    end,
    {stop, Reason, TCPState};

handle_info(Other, TCPState) ->
    ?LOG(info, "未处理消息: ~p", [Other]),
    {noreply, TCPState}.

%% ==================== 聚合函数 ====================
handle_aggregate(Data, DroneId, ProductId, _TCPState) ->
    DataType = case Data of
        #{<<"airspeed">> := _} -> <<"D1">>;
        #{<<"warhead_frame_freq">> := _} -> <<"D2">>;
        #{<<"ground_speed_direction">> := _} -> <<"D3">>;
        #{<<"zqy_acceleration_x">> := _} -> <<"SURFACE">>;
        #{<<"zhj_acceleration_x">> := _} -> <<"SURFACE">>;  %% 组合加速度计
        #{<<"ycw_acceleration_x">> := _} -> <<"SURFACE">>;  %% 右垂尾
        #{<<"test_item_device_id">> := _} -> <<"TEST_ITEM">>;
        #{<<"noise">> := _} -> <<"NOISE">>;
        _ -> <<"UNKNOWN">>
    end,
    DataSize = maps:size(Data),
    ?LOG(info, "[handle_aggregate] 无人机=~s, 类型=~s, 字段数=~p, ProductId=~s",
         [DroneId, DataType, DataSize, ProductId]),

    ensure_drone_registered(DroneId),
    ensure_uav_device_created(DroneId, ProductId, get(ip)),
    aggregate_local(DroneId, Data),

    %% 处理治具测试结果
    FixtureKeys = [
        <<"fuse1_ground_voltage">>, <<"fuse5_ground_voltage">>,
        <<"battery_port_resistance">>, <<"fuse8_wing_nail_resistance">>,
        <<"fuse7_wing_nail_resistance">>, <<"fuse7_8_resistance">>,
        <<"fuse9_10_resistance">>
    ],
    PresentFixtureKeys = [K || K <- FixtureKeys, maps:is_key(K, Data)],
    if PresentFixtureKeys /= [] ->
        ?LOG(error, "[DEBUG] 聚合数据中包含治具字段: ~p", [PresentFixtureKeys]);
       true -> ok
    end,

    case get(station_id) of
        undefined -> 
            ?LOG(debug, "station_id 未设置，跳过治具结果处理");  % 改为 debug
        StationId ->
            handle_fixture_test_results(Data, StationId, DroneId, ProductId)
    end,

    try_bind_station(DroneId).

aggregate_local(DroneId, NewData) ->
    Now = erlang:system_time(millisecond),
    Cache = case get(aggregate_cache) of
        undefined -> #{};
        C -> C
    end,
    ?LOG(info, "[aggregate_local] DroneId=~s, Cache大小=~p, 新数据字段数=~p",
         [DroneId, maps:size(Cache), maps:size(NewData)]),

    case maps:find(DroneId, Cache) of
        {ok, {LastTime, OldData}} when Now - LastTime >= 1000 ->
            ?LOG(info, "[aggregate_local] 触发存储: DroneId=~s, 时间间隔=~pms", [DroneId, Now - LastTime]),
            store_data(DroneId, OldData#{<<"createdat">> => LastTime}),
            Merged = maps:merge(OldData#{<<"createdat">> => LastTime}, NewData),
            put(aggregate_cache, maps:put(DroneId, {Now, Merged}, Cache));
        {ok, {LastTime, OldData}} ->
            ?LOG(info, "[aggregate_local] 合并数据: DroneId=~s, 时间间隔=~pms", [DroneId, Now - LastTime]),
            Merged = maps:merge(OldData, NewData),
            put(aggregate_cache, maps:put(DroneId, {LastTime, Merged}, Cache));
        error ->
            ?LOG(info, "[aggregate_local] 首次汇聚: DroneId=~s", [DroneId]),
            put(aggregate_cache, maps:put(DroneId, {Now, NewData}, Cache))
    end.

store_data(DroneId, Data) ->
    ProductId = get(product_id),
    ?LOG(info, "[store_data] ProductId=~p, DroneId=~p, 数据字段=~p", [ProductId, DroneId, maps:keys(Data)]),
    case ProductId of
        undefined ->
            ?LOG(error, "[store_data] ProductId未设置,无法存储数据: DroneId=~p", [DroneId]);
        _ ->
            Timestamp = erlang:system_time(millisecond),
            %% 先调用聚合器进行汇聚
            ?LOG(info, "[store_data] 调用聚合器汇聚数据"),
            case whereis(dgiot_uav_aggregator) of
                undefined ->
                    ?LOG(warning, "[store_data] 聚合器未启动，尝试启动聚合器"),
                    dgiot_uav_aggregator:start_link(),
                    %% 稍等片刻再调用
                    timer:sleep(100);
                _ -> ok
            end,
            dgiot_uav_aggregator:aggregate(DroneId, ProductId, Data, Timestamp),
            ?LOG(info, "[store_data] 数据已提交到聚合器")
    end.

flush_all_cache() ->
    case get(aggregate_cache) of
        undefined -> ok;
        Cache ->
            maps:fold(fun(DroneId, {LastTime, Data}, ok) ->
                store_data(DroneId, Data#{<<"createdat">> => LastTime})
            end, ok, Cache)
    end.

handle_fixture_test_results(Data, StationId, DroneId, ProductId) ->
    ?LOG(info, "[DEBUG] 进入 handle_fixture_test_results: StationId=~p, DroneId=~s, ProductId=~s, Data keys=~p",
         [StationId, DroneId, ProductId, maps:keys(Data)]),

    FixtureKeys = [
        <<"fuse1_ground_voltage">>,
        <<"fuse5_ground_voltage">>,
        <<"battery_port_resistance">>,
        <<"fuse8_wing_nail_resistance">>,
        <<"fuse7_wing_nail_resistance">>,
        <<"fuse7_8_resistance">>,
        <<"fuse9_10_resistance">>
    ],
    StationShortName = %% dgiot_uav_test_executor:station_id_to_short_name(StationId),
    Now = erlang:system_time(millisecond),
    lists:foreach(fun(Key) ->
        case maps:find(Key, Data) of
            {ok, Value} ->
                TestItemDeviceId = <<StationShortName/binary, "_", Key/binary>>,
                Result = case Value of
                    V when is_integer(V), V > 0 -> <<"PASS">>;
                    V when is_float(V), V > 0.0 -> <<"PASS">>;
                    _ -> <<"FAIL">>
                end,
                TestResult = #{
                    <<"test_item_device_id">> => TestItemDeviceId,
                    <<"test_step">> => 1,
                    <<"test_result">> => Result,
                    <<"createdat">> => Now
                },
                uav_thing_model:save_thing_model_data(ProductId, DroneId, TestResult),
                ?LOG(info, "[DEBUG] 治具结果已记录: ~s=~p -> ~s", [Key, Value, Result]);
            error ->
                ok
        end
    end, FixtureKeys),
    ok.

%% ==================== 设备辅助函数 ====================
ensure_drone_registered(DroneId) ->
    case get(drone_id) of
        undefined ->
            ?LOG(info, "注册无人机 ~s", [DroneId]),
            dgiot_uav_business_service:register_drone_worker(DroneId, self()),
            put(drone_id, DroneId),
            case get(port) of 10007 -> dgiot_uav_business_service:update_device_id(get(ip_bin), 10007, DroneId); _ -> ok end;
        _ -> ok
    end.

ensure_uav_device_created(DroneId, ProductId, Ip) ->
    %% 验证DroneId格式：应该是IP地址加端口的形式，如 "192.168.100.100_10007"
    %% 用户明确指出："超近距无人机 这种无人机是错的，不能用ip+port"
    %% 因此我们禁止创建IP地址_端口格式的设备
    case validate_drone_id(DroneId) of
        true ->
            %% IP地址_端口格式的设备是错误的，不创建
            ?LOG(warning, "禁止创建IP地址_端口格式的错误设备: ~s (用户明确指示这种设备是错误的)", [DroneId]),
            ok;
        false ->
            %% 对于非IP地址_端口格式的设备，检查是否已存在，不存在则创建
            case dgiot_device:lookup(DroneId) of
                {ok, _} -> ok;
                _ ->
                    %% 获取正确的设备名称（根据端口映射）
                    ChineseName = get_device_name_by_drone_id(DroneId, ProductId),
                    DevAddr = DroneId,
                    %% 确保Ip是二进制格式（根据错误日志，Ip可能已经是二进制）
                    IpBin = case is_binary(Ip) of
                               true -> Ip;
                               false -> list_to_binary(Ip)
                            end,
                    case dgiot_uav_business_service:create_device(DroneId, ProductId, DevAddr, IpBin, ChineseName) of
                        {ok, _ObjectId} -> ok;           %% 处理 {ok, ObjectId} 返回值
                        {DroneId, _} -> ok;
                        {<<>>, <<>>} -> ok;
                        Other -> 
                            ?LOG(error, "create_device返回了未处理的模式: ~p", [Other]),
                            ok
                    end
            end
    end.

%% 验证无人机ID格式
validate_drone_id(DroneId) when is_binary(DroneId) ->
    %% 检查格式：IP地址_端口，如 "192.168.100.100_10007"
    case binary:split(DroneId, <<"_">>) of
        [IpPart, PortPart] ->
            %% 验证IP地址格式
            case validate_ip(binary_to_list(IpPart)) of
                true ->
                    %% 验证端口号
                    try binary_to_integer(PortPart) of
                        Port when Port >= 10000, Port =< 11000 -> true;
                        _ -> false
                    catch
                        _:_ -> false
                    end;
                false -> false
            end;
        _ -> false
    end;
validate_drone_id(_) -> false.

%% 验证IP地址格式
validate_ip(IpStr) ->
    case inet:parse_address(IpStr) of
        {ok, _} -> true;
        _ -> false
    end.

%% 根据无人机ID和产品ID获取设备名称
get_device_name_by_drone_id(DroneId, ProductId) ->
    %% 首先尝试根据产品ID确定设备类型
    case ProductId of
        <<"6235befb62">> -> 
            %% 默认无人机产品，根据DroneId格式确定具体类型
            case validate_drone_id(DroneId) of
                true ->
                    %% IP地址_端口格式的设备是错误的"超近距无人机"
                    <<"超近距无人机(错误格式)"/utf8>>;
                false ->
                    %% 其他格式的无人机设备
                    <<"无人机设备"/utf8>>
            end;
        <<"wrj_danpianji_product">> -> <<"单片机设备"/utf8>>;
        <<"wrj_dicekou_product">> -> <<"地测口设备"/utf8>>;
        <<"wrj_surface_product">> -> <<"舵面设备"/utf8>>;
        <<"wrj_fixture_product">> -> <<"治具设备"/utf8>>;
        <<"wrj_scanner_product">> -> <<"扫描仪设备"/utf8>>;
        <<"wrj_noise_product">> -> <<"噪声设备"/utf8>>;
        _ ->
            %% 尝试从端口映射获取设备名称
            case get(port) of
                Port when is_integer(Port) ->
                    case dgiot_uav_config:get_port_device_mapping(Port) of
                        {_DeviceId, _ProductId, DeviceName, _ShouldCreate, _IsSurface} ->
                            DeviceName;
                        undefined ->
                            <<"未知设备"/utf8>>
                    end;
                _ ->
                    <<"未知设备"/utf8>>
            end
    end.

%% ==================== 工位绑定函数 ====================
bind_station(DroneId, StationInfo) ->
    put(station_bound, true),
    dgiot_uav_business_service:bind_uav_to_station(DroneId, StationInfo),
    FixtureAddr = maps:get(fixture_address, StationInfo),
    StationId = maps:get(station_id, StationInfo),
    put(station_id, StationId),
    put(fixture_addr, FixtureAddr),
    ?LOG(info, "[DEBUG] 工位绑定: station_id=~p, fixture_addr=~p", [StationId, FixtureAddr]),
    dgiot_uav_business_service:bind_station_drone(FixtureAddr, DroneId),
    load_and_execute_test_items(StationId, DroneId),
    dgiot_uav_command_scheduler:station_bind(StationId, DroneId).

try_bind_station(DroneId) ->
    case get(station_bound) of
        true -> ok;
        false ->
            case get(fixture_addr) of
                undefined -> try_bind_by_ip(DroneId);
                Addr ->
                    case dgiot_uav_stub_functions:get_station_by_fixture_addr(Addr) of
                        {ok, StationInfo} -> self() ! {bind_station, DroneId, StationInfo};
                        {error, not_find} -> ok
                    end
            end
    end.

try_bind_by_ip(DroneId) ->
    case get(ip) of
        undefined -> ok;
        Ip ->
            case dgiot_uav_business_service:get_device_info_by_ip(Ip) of
                {ok, Devices} ->
                    case lists:filter(fun(#{info := Info}) -> maps:get(device_id, Info) == <<"wrj_danpianji">> end, Devices) of
                        [#{info := #{sensor_addr := Addr}}] ->
                            case dgiot_uav_stub_functions:get_station_by_fixture_addr(Addr) of
                                {ok, StationInfo} -> self() ! {bind_station, DroneId, StationInfo};
                                {error, not_find} -> ?LOG(debug, "工位地址 ~p 未找到", [Addr])
                            end;
                        [] -> ?LOG(debug, "IP ~s 下未找到单片机", [Ip])
                    end;
                _ -> ok
            end
    end.

load_and_execute_test_items(StationId, DroneId) ->
    ?LOG(info, "开始加载工位 ~p 的测试项，无人机: ~s", [StationId, DroneId]),
    case dgiot_uav_stub_functions:load_test_items_by_station(StationId) of
        {ok, TestItems} ->
            ?LOG(info, "工位 ~p 加载了 ~p 个测试项", [StationId, length(TestItems)]),
            lists:foreach(fun(TestItem) ->
                StationAddr = StationId,
                dgiot_uav_stub_functions:execute_test_item(DroneId, StationAddr, TestItem)
            end, TestItems);
        {error, Reason} ->
            ?LOG(error, "加载测试项失败: ~p", [Reason])
    end,
    ok.

%% ==================== 工具函数 ====================
cleanup_timer(Key) ->
    case get(Key) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer), put(Key, undefined)
    end.

start_surface_timer() ->
    ?LOG(info, "启动舵面定时器, 间隔 ~p ms", [?SURFACE_INTERVAL]),
    Timer = erlang:send_after(?SURFACE_INTERVAL, self(), read_surface),
    put(surface_timer, Timer).

cleanup_all_timers() ->
    lists:foreach(fun cleanup_timer/1, [periodic_timer, surface_timer, init_timeout_timer]).

cleanup_registrations() ->
    cleanup_ip_port_registration(),
    cleanup_drone_registration(),
    cleanup_station_registration(),
    cleanup_station_binding().

cleanup_ip_port_registration() ->
    IpBin = get(ip_bin), Port = get(port),
    if IpBin =/= undefined andalso Port =/= undefined ->
        try dgiot_uav_business_service:unregister_ip_port(IpBin, Port) of
            _ -> ok
        catch _:_ -> ok end;
       true -> ok
    end.

cleanup_drone_registration() ->
    case get(drone_id) of undefined -> ok; DroneId ->
        catch dgiot_uav_business_service:unregister_drone_worker(DroneId) end.

cleanup_station_registration() ->
    case get(station_key) of undefined -> ok; Key ->
        catch dgiot_uav_business_service:unregister_station_worker(Key) end.

cleanup_station_binding() ->
    case get(fixture_addr) of undefined -> ok; Addr ->
        catch dgiot_uav_business_service:unbind_station_drone(Addr) end.

%% ==================== terminate ====================
terminate(_Reason, #tcp{state = _UavState}) ->
    ?LOG(info, "进程终止"),
    cleanup_all_timers(),
    cleanup_registrations(),
    flush_all_cache(),
    erase(),
    ok.

handle_call(_Msg, _From, TCPState) -> {reply, ok, TCPState}.
handle_cast(_Msg, TCPState) -> {noreply, TCPState}.
code_change(_OldVsn, TCPState, _Extra) -> {ok, TCPState}.

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试治具指令结果保存功能
%% @spec test_fixture_result() -> ok | {error, Reason}
test_fixture_result() ->
    ?LOG(info, "开始测试治具指令结果保存功能..."),
    
    %% 模拟治具测试数据
    TestData = #{
        <<"fuse1_ground_voltage">> => 220,
        <<"fuse5_ground_voltage">> => 110,
        <<"battery_port_resistance">> => 50,
        <<"fuse8_wing_nail_resistance">> => 100,
        <<"fuse7_wing_nail_resistance">> => 150,
        <<"fuse7_8_resistance">> => 200,
        <<"fuse9_10_resistance">> => 250
    },
    
    %% 模拟工位ID和无人机ID
    StationId = <<"D1">>,
    DroneId = <<"test_drone_001">>,
    ProductId = <<"6235befb62">>,
    
    ?LOG(info, "测试数据: StationId=~s, DroneId=~s, ProductId=~s", 
         [StationId, DroneId, ProductId]),
    ?LOG(info, "治具字段: ~p", [maps:keys(TestData)]),
    
    %% 调用 handle_fixture_test_results 函数
    try
        handle_fixture_test_results(TestData, StationId, DroneId, ProductId),
        ?LOG(info, "✅ 治具指令结果保存测试完成"),
        ?LOG(info, "请检查日志确认测试结果已保存到无人机物模型"),
        ok
    catch
        Class:Reason:Stacktrace ->
            ?LOG(error, "❌ 测试失败: Class=~p, Reason=~p, Stacktrace=~p", 
                 [Class, Reason, Stacktrace]),
            {error, {test_failed, Class, Reason, Stacktrace}}
    end.