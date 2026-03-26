%%%-------------------------------------------------------------------
%%% @doc
%%% station_magnetic_heading - 磁航向工位业务处理器
%%% 专门处理扫码枪二维码绑定业务
%%% @end
%%%-------------------------------------------------------------------
-module(station_magnetic_heading).

%% 实现工位基类接口
-behaviour(station_base).

%% 工位基类回调
-export([
    init/1,
    handle_data/3,
    cache_metric/3,
    get_metric/2,
    cleanup_expired/1
]).

%% 扫码枪专用接口
-export([
    handle_scanner_qrcode/3,
    cache_qrcode_data/2,
    get_cached_qrcode/1,
    cache_drone_online/1,
    get_cached_drone/1,
    bind_qrcode_to_drone/2,
    clear_qrcode_cache/0,
    send_bind_success_message/2,
    send_bind_failure_message/3
]).

%% 工位定义
-define(STATION_NAME_EN, station_magnetic_heading).
-define(STATION_NAME_CN, <<"磁航向工位"/utf8>>).
-define(STATION_IP_20, <<"192.168.100.20">>).   %% 工位IP
-define(STATION_IP_21, <<"192.168.100.21">>).   %% DTU IP
-define(STATION_BASE_ADDR, <<"D1700">>).
-define(STATION_BUSINESS, <<"扫码绑定"/utf8>>).

%% 指标定义
-define(METRIC_QRCODE_SERIAL, <<"qrcode_serial">>).
-define(METRIC_QRCODE_DATA, <<"qrcode_data">>).
-define(METRIC_DRONE_ONLINE, <<"drone_online">>).
-define(METRIC_DRONE_ONLINE_TIME, <<"drone_online_time">>).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/types.hrl").

%% 使用types.hrl中定义的无人机产品ID宏
-ifndef(UAV_PRODUCT_ID).
-define(UAV_PRODUCT_ID, <<"6235befb62">>).
-endif.

%%%===================================================================
%%% 工位基类回调实现
%%%===================================================================

%% @doc 初始化工位
-spec init(atom()) -> {ok, map()}.
init(station_magnetic_heading) ->
    %% 初始化工位ETS表
    station_base:init_station_table(station_magnetic_heading),
    
    %% 启动过期数据清理定时器
    start_cleanup_timer(),
    
    State = #{
        name_en => station_magnetic_heading,
        name_cn => ?STATION_NAME_CN,
        ip_20 => ?STATION_IP_20,
        ip_21 => ?STATION_IP_21,
        base_addr => ?STATION_BASE_ADDR,
        business => ?STATION_BUSINESS,
        cleanup_timer => undefined
    },
    
    ?LOG(info, "磁航向工位初始化完成: ~p", [State]),
    {ok, State}.

%% @doc 处理工位数据
-spec handle_data(atom(), atom(), term()) -> ok.
handle_data(station_magnetic_heading, scanner_qrcode, QrcodeData) ->
    %% 处理扫码枪二维码数据
    handle_scanner_qrcode(self(), station_magnetic_heading, QrcodeData);
handle_data(station_magnetic_heading, drone_online, DroneId) ->
    %% 处理无人机上线事件
    cache_drone_online(DroneId);
handle_data(station_magnetic_heading, DataType, Data) ->
    ?LOG(warning, "磁航向工位不支持的数据类型: ~p, 数据: ~p", [DataType, Data]),
    ok.

%% @doc 缓存工位指标
-spec cache_metric(atom(), binary(), term()) -> ok.
cache_metric(station_magnetic_heading, MetricKey, MetricValue) ->
    station_base:cache_station_metric(station_magnetic_heading, MetricKey, MetricValue).

%% @doc 获取工位指标
-spec get_metric(atom(), binary()) -> {ok, term()} | {error, term()}.
get_metric(station_magnetic_heading, MetricKey) ->
    station_base:get_station_metric(station_magnetic_heading, MetricKey).

%% @doc 清理过期数据
-spec cleanup_expired(atom()) -> ok.
cleanup_expired(station_magnetic_heading) ->
    ExpireTime = 20 * 1000,  %% 20秒
    station_base:cleanup_expired_metrics(station_magnetic_heading, ExpireTime).

%%%===================================================================
%%% 扫码枪专用业务处理
%%%===================================================================

%% @doc 处理扫码枪二维码数据
-spec handle_scanner_qrcode(pid(), atom(), map()) -> ok.
handle_scanner_qrcode(FromPid, StationNameEn, QrcodeData) ->
    SerialNo = maps:get(<<"serial_no">>, QrcodeData, <<"unknown">>),
    ?LOG(info, "磁航向工位-扫码枪数据: SerialNo=~s", [SerialNo]),
    
    %% 缓存二维码数据
    cache_qrcode_data(StationNameEn, QrcodeData),
    
    %% 检查是否有无人机上线事件
    case get_cached_drone(StationNameEn) of
        {ok, DroneId} ->
            %% 找到无人机，绑定二维码
            bind_qrcode_to_drone(DroneId, QrcodeData),
            ?LOG(info, "二维码已绑定到无人机: ~s", [DroneId]),
            FromPid ! {station_response, StationNameEn, {bound, DroneId}};
        {error, not_find} ->
            %% 无无人机，等待后续绑定
            ?LOG(info, "二维码已缓存，等待无人机上线"),
            FromPid ! {station_response, StationNameEn, cached};
        {error, expired} ->
            %% 无人机上线事件已过期
            ?LOG(warning, "无人机上线事件已过期，二维码仅缓存"),
            FromPid ! {station_response, StationNameEn, drone_expired}
    end,
    ok.

%% @doc 缓存二维码数据
-spec cache_qrcode_data(atom(), map()) -> ok.
cache_qrcode_data(StationNameEn, QrcodeData) ->
    %% 缓存序列号
    case maps:get(<<"serial_no">>, QrcodeData, undefined) of
        undefined -> 
            ?LOG(warning, "二维码数据无序列号");
        SerialNo ->
            cache_metric(StationNameEn, ?METRIC_QRCODE_SERIAL, SerialNo)
    end,
    
    %% 缓存完整数据
    cache_metric(StationNameEn, ?METRIC_QRCODE_DATA, QrcodeData),
    ?LOG(info, "二维码数据已缓存到磁航向工位").

%% @doc 获取缓存的二维码数据
-spec get_cached_qrcode(atom()) -> {ok, map()} | {error, term()}.
get_cached_qrcode(StationNameEn) ->
    get_metric(StationNameEn, ?METRIC_QRCODE_DATA).

%% @doc 缓存无人机上线事件
-spec cache_drone_online(binary()) -> ok.
cache_drone_online(DroneId) ->
    %% 缓存无人机上线事件和时间
    cache_metric(station_magnetic_heading, ?METRIC_DRONE_ONLINE, DroneId),
    cache_metric(station_magnetic_heading, ?METRIC_DRONE_ONLINE_TIME, erlang:system_time(millisecond)),
    ?LOG(info, "磁航向工位-UDP无人机上线: DroneId=~s", [DroneId]),
    
    %% 检查是否有等待的二维码
    case get_cached_qrcode(station_magnetic_heading) of
        {ok, QrcodeData} ->
            %% 绑定二维码到无人机
            bind_qrcode_to_drone(DroneId, QrcodeData),
            ?LOG(info, "磁航向工位-自动绑定: DroneId=~s", [DroneId]);
        {error, _} ->
            ok
    end.

%% @doc 获取缓存的无人机上线事件
-spec get_cached_drone(atom()) -> {ok, binary()} | {error, term()}.
get_cached_drone(StationNameEn) ->
    get_metric(StationNameEn, ?METRIC_DRONE_ONLINE).

%% @doc 将二维码绑定到无人机
-spec bind_qrcode_to_drone(binary(), map()) -> ok.
bind_qrcode_to_drone(DroneId, QrcodeData) ->
    SerialNo = maps:get(<<"serial_no">>, QrcodeData, <<"unknown">>),
    ?LOG(info, "开始绑定二维码到无人机: 无人机ID=~s, 序列号=~s", [DroneId, SerialNo]),
    
    try
        %% 直接保存到物模型
        DroneData = #{
            <<"serial_no">> => SerialNo,
            <<"qrcode_data">> => QrcodeData,
            <<"scanner_time">> => erlang:system_time(millisecond)
        },
        uav_thing_model:save_thing_model_data(?UAV_PRODUCT_ID, DroneId, DroneData),
        
        %% 更新设备名称
        case SerialNo of
            <<>> -> ok;
            _ ->
                dgiot_uav_business_service:update_device_name(DroneId, SerialNo),
                dgiot_uav_device_manager:update_device_content(DroneId, QrcodeData)
        end,
        
        ?LOG(info, "二维码绑定到无人机成功: 无人机ID=~s", [DroneId]),
        
        %% 3. 清除工位的二维码缓存（绑定成功后清理）
        clear_qrcode_cache(),
        
        %% 4. 发送绑定成功消息
        send_bind_success_message(DroneId, SerialNo),
        
        ok
    catch
        Class:Exception:Stack ->
            ?LOG(error, "二维码绑定到无人机失败: Class=~p, Exception=~p, Stack=~p", 
                 [Class, Exception, Stack]),
            %% 发送绑定失败消息
            send_bind_failure_message(DroneId, SerialNo, Exception),
            ok
    end.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

%% @doc 启动过期数据清理定时器
-spec start_cleanup_timer() -> ok.
start_cleanup_timer() ->
    %% 每10秒清理一次过期数据
    TimerRef = timer:apply_interval(10000, ?MODULE, cleanup_expired, [station_magnetic_heading]),
    ?LOG(debug, "磁航向工位清理定时器已启动: ~p", [TimerRef]),
    ok.

%% @doc 清除二维码缓存
-spec clear_qrcode_cache() -> ok.
clear_qrcode_cache() ->
    %% 删除二维码相关的缓存指标
    station_base:cache_station_metric(station_magnetic_heading, ?METRIC_QRCODE_SERIAL, undefined),
    station_base:cache_station_metric(station_magnetic_heading, ?METRIC_QRCODE_DATA, undefined),
    ?LOG(info, "磁航向工位二维码缓存已清除"),
    ok.

%% @doc 发送绑定成功消息
-spec send_bind_success_message(binary(), binary()) -> ok.
send_bind_success_message(DroneId, SerialNo) ->
    Message = #{
        <<"event">> => <<"qrcode_bind_success">>,
        <<"drone_id">> => DroneId,
        <<"serial_no">> => SerialNo,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"station">> => station_magnetic_heading
    },
    %% 可以发送到消息总线或其他监听器
    ?LOG(info, "二维码绑定成功消息: ~p", [Message]),
    ok.

%% @doc 发送绑定失败消息
-spec send_bind_failure_message(binary(), binary(), term()) -> ok.
send_bind_failure_message(DroneId, SerialNo, Reason) ->
    Message = #{
        <<"event">> => <<"qrcode_bind_failure">>,
        <<"drone_id">> => DroneId,
        <<"serial_no">> => SerialNo,
        <<"reason">> => dgiot_utils:to_binary(Reason),
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"station">> => station_magnetic_heading
    },
    ?LOG(warning, "二维码绑定失败消息: ~p", [Message]),
    ok.