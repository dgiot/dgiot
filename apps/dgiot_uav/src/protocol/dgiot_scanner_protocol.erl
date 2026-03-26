%%%-------------------------------------------------------------------
%%% @doc 扫描枪协议处理模块
%%% @version 1.0.0
%%% @author dgiot_uav_team
%%% @doc 扫描枪协议处理模块，负责处理二维码数据、缓存管理和MES上报
%%%-------------------------------------------------------------------
-module(dgiot_scanner_protocol).
-author("dgiot_uav_team").

%% 依赖
-dgiot_data("ets").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").
-include_lib("dgiot_uav/include/types.hrl").

%% 缓存配置
-define(QRCODE_EXPIRE, 20000).      % 20秒
-define(DRONE_EXPIRE, 20000).       % 20秒
-define(CLEANUP_INTERVAL, 10000).   % 10秒
-define(TEST_ITEM_EXPIRE, 30 * 60 * 1000).  % 30分钟

%% API导出
-export([
    handle_port_data/4,
    get_cached_qrcode/1,
    cache_drone_online/2,
    bind_pending_qrcode/3,
    report_to_mes/2
]).

%% 内部导出
-export([init_ets/0, cleanup_cache/0]).

%% 磁航向工位配置
-define(MAGNETIC_STATION_IP, <<"192.168.100.21">>).
-define(MAGNETIC_STATION_IP2, <<"192.168.100.20">>).
-define(MAGNETIC_STATION_PORT, 10007).  %% 磁航向工位端口

%%====================================================================
%% 端口数据处理入口
%%====================================================================
-spec handle_port_data(binary(), #tcp{}, #uav_state{}, inet:socket()) -> 
    {#tcp{}, #uav_state{}, noreply}.
handle_port_data(NewBuf, TCPState, UavState, Socket) ->
    ?LOG(error, "扫描枪端口~p收到原始数据: ~p", [UavState#uav_state.port, NewBuf]),
    ensure_cache(),
    
    ProductId = UavState#uav_state.product_id,
    IpBin = UavState#uav_state.ip_bin,
    DevAddr = get_or_create_devaddr(UavState),
    
    case parse_qrcode_data(NewBuf) of
        {ok, ParsedData} ->
            process_qrcode(ProductId, IpBin, DevAddr, ParsedData, TCPState, UavState, Socket);
        {error, not_qrcode} ->
            handle_non_qrcode(NewBuf, TCPState, UavState, Socket)
    end.

%% 获取或创建设备地址
get_or_create_devaddr(UavState) ->
    case UavState#uav_state.devaddr of
        <<>> ->
            Port = UavState#uav_state.port,
            <<"scanner_", (UavState#uav_state.ip_bin)/binary, "_", (integer_to_binary(Port))/binary>>;
        DevAddr -> DevAddr
    end.

%%====================================================================
%% 二维码处理
%%====================================================================
process_qrcode(ProductId, IpBin, DevAddr, ParsedData, TCPState, UavState, Socket) ->
    Port = UavState#uav_state.port,
    SerialNo = maps:get(<<"serial_no">>, ParsedData, <<>>),
    TestId = maps:get(<<"test_id">>, ParsedData, <<>>),
    StationId = maps:get(<<"station_id">>, ParsedData, <<>>),
    
    ?LOG(info, "~n========================================~n"
                "【Step 1/7】扫描枪发送二维码~n"
                "========================================~n"
                "  IP:Port: ~s:~p~n"
                "  TestID: ~s~n"
                "  StationID: ~s~n"
                "  SerialNo: ~s~n"
                "  MaterialCode: ~s~n"
                "========================================", 
         [IpBin, Port, TestId, StationId, SerialNo, 
          maps:get(<<"material_code">>, ParsedData, <<>>)]),
    
    %% 缓存二维码（磁航向工位需要同时判断IP和端口）
    cache_qrcode(IpBin, Port, ParsedData),
    
    ?LOG(info, "~n========================================~n"
                "【Step 2/7】缓存二维码数据~n"
                "========================================~n"
                "  缓存Key: magnetic_station~n"
                "  SerialNo: ~s~n"
                "  状态: ✅ 已缓存，等待EB90帧提取PlaneID~n"
                "========================================", [SerialNo]),
    
    %% 保存到物模型
    save_to_thing_model(ProductId, DevAddr, ParsedData),
    
    %% 上报MES
    report_to_mes(DevAddr, ParsedData),
    
    inet:setopts(Socket, [{active, once}]),
    {TCPState#tcp{buff = <<>>}, UavState, noreply}.

handle_non_qrcode(NewBuf, TCPState, UavState, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    {TCPState#tcp{buff = ensure_binary(NewBuf)}, UavState, noreply}.

%% 缓存二维码（扫码枪固定缓存到magnetic_station）
cache_qrcode(IpBin, _Port, ParsedData) ->
    %% 扫码枪设备（端口1234）固定缓存到magnetic_station
    %% 因为扫码枪的二维码数据需要被地测口（EB90帧）使用
    cache_for_magnetic_station(ParsedData),
    %% 同时也缓存到IP key（备用）
    cache_by_ip(IpBin, ParsedData).

cache_for_magnetic_station(ParsedData) ->
    Now = erlang:system_time(millisecond),
    ets:insert(scanner_qrcode_cache, {magnetic_station, {Now, ParsedData}}),
    ?LOG(error, "[SCANNER] 缓存到磁航向工位").

cache_by_ip(IpBin, ParsedData) ->
    Now = erlang:system_time(millisecond),
    ets:insert(scanner_qrcode_cache, {IpBin, {Now, ParsedData}}).

%%====================================================================
%% 二维码解析
%%====================================================================
parse_qrcode_data(Data) when is_binary(Data) ->
    CleanData = re:replace(Data, <<"[\r\n]+">>, <<>>, [global, {return, binary}]),
    Parts = binary:split(CleanData, <<"|">>, [global]),
    
    case length(Parts) of
        8 -> parse_and_process_8_fields(Parts);
        6 -> {ok, parse_6_fields(Parts)};
        _ -> {error, not_qrcode}
    end.

%% 新格式：Test01|1|5000000020004|10|2026032502|||
%% 字段含义：测试ID|工位ID|物料编码|数量|设备序列号|||
parse_and_process_8_fields([TestId, StationIdBin, MaterialCode, Qty, SerialNo, _Empty1, _Empty2, _Empty3]) ->
    StationId = try binary_to_integer(StationIdBin) catch _:_ -> 0 end,
    Quantity = try binary_to_integer(Qty) catch _:_ -> 0 end,
    
    ParsedData = #{
        <<"test_id">> => TestId,
        <<"station_id">> => StationId,
        <<"serial_no">> => SerialNo,           %% 第5字段：设备序列号（用于设备名称）
        <<"quantity">> => Quantity,
        <<"material_code">> => MaterialCode,   %% 第3字段：物料编码
        <<"qrcode_format">> => <<"v3.0">>,
        <<"purchase_order_no">> => TestId,
        <<"project_no">> => <<>>,
        <<"batch_no">> => <<>>,
        <<"supplier">> => <<>>,
        <<"expiry_date">> => <<>>
    },
    
    %% 【修复】立即创建无人机设备，不等待EB90帧
    %% 使用SerialNo作为设备地址创建设备，EB90帧可以后续更新PlaneID
    spawn(fun() -> 
        case create_uav_device(SerialNo, ParsedData, StationId) of
            {ok, DeviceId} ->
                ?LOG(info, "[SCANNER] 无人机设备创建成功: DeviceId=~s, SerialNo=~s", [DeviceId, SerialNo]);
            {error, Reason} ->
                ?LOG(error, "[SCANNER] 无人机设备创建失败: SerialNo=~s, Reason=~p", [SerialNo, Reason])
        end
    end),
    ?LOG(info, "[SCANNER] 二维码解析成功，设备创建已触发"),
    
    {ok, ParsedData}.

parse_6_fields([PO, PN, MC, Qty, SN, BN]) ->
    #{
        <<"purchase_order_no">> => PO,
        <<"project_no">> => PN,
        <<"material_code">> => MC,
        <<"quantity">> => to_int(Qty),
        <<"serial_no">> => SN,
        <<"batch_no">> => BN,
        <<"qrcode_format">> => <<"v1.0">>
    }.

to_int(Bin) -> try binary_to_integer(Bin) catch _:_ -> 0 end.

%%====================================================================
%% 设备创建辅助函数
%%====================================================================
%% 创建无人机设备
-spec create_uav_device(binary(), map(), integer()) -> {ok, binary()} | {error, term()}.
create_uav_device(SerialNo, _ParsedData, _StationId) ->
    ProductId = ?UAV_PRODUCT_ID,
    DeviceName = <<"无人机_", SerialNo/binary>>,
    DevAddr = SerialNo,
    
    try
        %% 检查设备是否已存在
        case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DevAddr}}) of
            {ok, #{<<"results">> := [_ | _]}} ->
                ?LOG(info, "[SCANNER] 设备已存在，跳过创建: ~s", [SerialNo]),
                {ok, DevAddr};
            _ ->
                %% 使用dgiot_uav_device_manager创建设备
                case dgiot_uav_device_manager:create_device(
                    <<"admin">>,  % LoginId
                    ProductId,
                    DevAddr,
                    ?MAGNETIC_STATION_IP,  % IP
                    DeviceName  % ChineseName
                ) of
                    {ok, DeviceId} ->
                        ?LOG(info, "[SCANNER] 设备创建成功: DeviceId=~s", [DeviceId]),
                        {ok, DeviceId};
                    {error, Reason} ->
                        ?LOG(error, "[SCANNER] 设备创建失败: Reason=~p", [Reason]),
                        {error, Reason}
                end
        end
    catch
        Type:ExReason:Stacktrace ->
            ?LOG(error, "[SCANNER] 创建设备异常: Type=~p, Reason=~p, Stacktrace=~p", 
                 [Type, ExReason, Stacktrace]),
            {error, ExReason}
    end.

%%====================================================================
%% 数据存储
%%====================================================================
save_to_thing_model(ProductId, DevAddr, ParsedData) ->
    ScannerCode = maps:get(<<"serial_no">>, ParsedData, <<"unknown">>),
    ThingData = maps:merge(#{<<"scanner_code">> => ScannerCode}, ParsedData),
    uav_thing_model:save_thing_model_data(ProductId, DevAddr, ThingData).

%%====================================================================
%% 二维码缓存查询
%%====================================================================
-spec get_cached_qrcode(integer() | atom()) -> {ok, map()} | {error, term()}.
get_cached_qrcode(Key) ->
    ensure_cache(),
    case ets:lookup(scanner_qrcode_cache, Key) of
        [] -> {error, not_found};
        [{Key, {Timestamp, Data}}] ->
            case is_expired(Timestamp, ?QRCODE_EXPIRE) of
                false -> {ok, Data};
                true -> {error, expired}
            end
    end.

%%====================================================================
%% 无人机上线事件缓存
%%====================================================================
-spec cache_drone_online(integer() | atom(), binary()) -> ok.
cache_drone_online(StationId, DroneId) ->
    ensure_cache(),
    Now = erlang:system_time(millisecond),
    
    case StationId of
        0 ->  % 磁航向工位
            ets:insert(scanner_qrcode_cache, {{drone_online, magnetic_station}, {Now, DroneId}}),
            %% TODO: dgiot_uav_magnetic_heading模块不存在，暂时注释
            %% dgiot_uav_magnetic_heading:cache_drone_online(DroneId);
            ok;
        _ ->
            ets:insert(scanner_qrcode_cache, {{drone_online, StationId}, {Now, DroneId}})
    end,
    ?LOG(debug, "[SCANNER] 缓存无人机上线 - 工位:~p, 无人机:~s", [StationId, DroneId]).

%%====================================================================
%% 二维码绑定
%%====================================================================
-spec bind_pending_qrcode(integer(), binary(), binary()) -> ok.
bind_pending_qrcode(StationId, ProductId, DroneDevAddr) ->
    %% TODO: dgiot_uav_station_base模块不存在，使用简化实现
    ?LOG(debug, "[SCANNER] 绑定待处理二维码: StationId=~p, ProductId=~s, DroneDevAddr=~s", 
         [StationId, ProductId, DroneDevAddr]),
    case dgiot_uav_station_manager:get_station_by_fixture(StationId) of
        {ok, _StationNameEn} ->
            %% 暂时使用ETS缓存中的二维码数据
            case get_cached_qrcode(StationId) of
                {ok, ParsedData} ->
                    do_bind_qrcode(ProductId, DroneDevAddr, ParsedData);
                _ -> ok
            end;
        _ -> ok
    end.

do_bind_qrcode(ProductId, DroneDevAddr, ParsedData) ->
    %% 保存到无人机物模型
    DroneData = #{
        <<"serial_no">> => maps:get(<<"serial_no">>, ParsedData, <<>>),
        <<"qrcode_data">> => ParsedData,
        <<"scanner_time">> => erlang:system_time(millisecond)
    },
    uav_thing_model:save_thing_model_data(ProductId, DroneDevAddr, DroneData),
    
    %% 更新设备名称
    case maps:get(<<"serial_no">>, ParsedData, <<>>) of
        <<>> -> ok;
        SerialNo ->
            dgiot_uav_business_service:update_device_name(DroneDevAddr, SerialNo),
            dgiot_uav_device_manager:update_device_content(DroneDevAddr, ParsedData)
    end,
    ?LOG(info, "[SCANNER] 二维码绑定成功 - 无人机:~s", [DroneDevAddr]).

%%====================================================================
%% MES上报
%%====================================================================
-spec report_to_mes(binary(), map()) -> ok | {error, term()}.
report_to_mes(DevAddr, ParsedData) ->
    try
        SerialNo = maps:get(<<"serial_no">>, ParsedData, <<>>),
        {ProductionLine, _StationName} = get_mes_config(),
        
        ExtraData = build_mes_extra(ParsedData),
        LineNo = build_line_no(ProductionLine, ParsedData),
        DroneNo = if SerialNo =:= <<>> -> DevAddr; true -> SerialNo end,
        
        ?LOG(info, "[MES] 开始上报MES - 设备地址:~s, 序列号:~s, 产线:~s, 额外数据:~p", 
             [DevAddr, SerialNo, ProductionLine, ExtraData]),
        ?LOG(info, "[MES] 二维码完整报文数据: ~p", [ParsedData]),
        ?LOG(debug, "[MES] 完整报文数据(debug): ~p", [ParsedData]),
        
        case dgiot_uav_mes_service:report_device_status(<<"SCANNER_MES">>, LineNo, DroneNo, ExtraData) of
            {ok, Response} -> 
                ?LOG(info, "[MES] 上报成功 - SN:~s, 响应:~p", [SerialNo, Response]);
            {error, R} -> 
                ?LOG(error, "[MES] 上报失败 - Reason:~p", [R])
        end
    catch _:E -> ?LOG(error, "[MES] 上报异常 - ~p", [E]) end.

build_mes_extra(ParsedData) ->
    #{
        <<"material_code">> => maps:get(<<"material_code">>, ParsedData, <<>>),
        <<"supplier">> => maps:get(<<"supplier">>, ParsedData, <<>>),
        <<"project_no">> => maps:get(<<"project_no">>, ParsedData, <<>>),
        <<"purchase_order_no">> => maps:get(<<"purchase_order_no">>, ParsedData, <<>>),
        <<"scanner_time">> => dgiot_datetime:now_secs()
    }.

build_line_no(ProductionLine, ParsedData) ->
    case maps:get(<<"project_no">>, ParsedData, <<>>) of
        <<>> -> <<ProductionLine/binary, ":", (get_station_name())/binary>>;
        PN -> <<ProductionLine/binary, ":", PN/binary>>
    end.

%%====================================================================
%% 配置
%%====================================================================
get_mes_config() -> {get_production_line(), get_station_name()}.

get_production_line() ->
    case application:get_env(dgiot_uav, production_line, <<"A">>) of
        {ok, V} when is_binary(V) -> V;
        {ok, V} when is_list(V) -> list_to_binary(V);
        _ -> <<"A">>
    end.

get_station_name() ->
    case application:get_env(dgiot_uav, station_name, <<"总测01"/utf8>>) of
        {ok, V} when is_binary(V) -> V;
        {ok, V} when is_list(V) -> list_to_binary(V);
        _ -> <<"总测01"/utf8>>
    end.

%%====================================================================
%% ETS管理
%%====================================================================
ensure_cache() ->
    case ets:info(scanner_qrcode_cache) of
        undefined -> init_ets();
        _ -> start_cleanup()
    end.

init_ets() ->
    dgiot_data:init(scanner_qrcode_cache, [public, named_table, set, 
                                           {write_concurrency, true}, 
                                           {read_concurrency, true}]),
    start_cleanup().

start_cleanup() ->
    case whereis(scanner_qrcode_cleanup) of
        undefined ->
            Pid = spawn_link(fun cleanup_loop/0),
            register(scanner_qrcode_cleanup, Pid);
        _ -> ok
    end.

cleanup_loop() ->
    timer:sleep(?CLEANUP_INTERVAL),
    cleanup_cache(),
    cleanup_loop().

cleanup_cache() ->
    case ets:info(scanner_qrcode_cache) of
        undefined -> ok;
        _ ->
            Now = erlang:system_time(millisecond),
            ets:foldl(fun({Key, {Ts, _}}, _) ->
                if Now - Ts > ?QRCODE_EXPIRE -> 
                        ets:delete(scanner_qrcode_cache, Key);
                   true -> ok
                end
            end, ok, scanner_qrcode_cache)
    end.

%%====================================================================
%% 辅助函数
%%====================================================================
is_expired(Timestamp, Expire) ->
    erlang:system_time(millisecond) - Timestamp > Expire.

ensure_binary(undefined) -> <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(List) when is_list(List) -> list_to_binary(List);
ensure_binary(_) -> <<>>.
