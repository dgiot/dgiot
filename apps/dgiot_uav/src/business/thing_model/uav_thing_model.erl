%%%-------------------------------------------------------------------
%%% @doc
%%% uav_thing_model - 无人机物模型处理外观模块
%%% 聚合各子模块的转换函数，并提供存储、缓存等公共功能。
%%% 修改点：增加 convert_link_full_to_thing_model/1 转换完整链路状态。
%%% 增加：在存储前强制移除 funcid 字段，避免插入错误。
%%% @end
%%%-------------------------------------------------------------------
-module(uav_thing_model).

-export([
    convert_d1_to_thing_model/1,
    convert_d2_to_thing_model/1,
    convert_d3_to_thing_model/1,
    convert_version_to_thing_model/1,
    convert_waypoint_to_thing_model/1,
    convert_surface_calibration_to_thing_model/1,
    convert_battery_to_thing_model/1,
    convert_link_full_to_thing_model/1,   % 新增
    convert_link_to_thing_model/1,       % 新增，用于协议解析
    save_d1_thing_model/3,
    save_d2_thing_model/3,
    save_d3_thing_model/3,
    save_thing_model_data/3,
    safe_binary_to_hex/1,
    escape_sql_string/1
]).

-include("d1_data.hrl").
-include("d2_data.hrl").
-include("d3_data.hrl").
-include_lib("dgiot_uav/include/extra_commands.hrl").
-include("link_data.hrl").                 % 新增，用于链路状态记录
-include_lib("dgiot/include/logger.hrl").

-define(TD_TYPE, <<"TD">>).
-define(COLUMNS_CACHE, td_stable_columns).

%%%===================================================================
%%% 转换函数（转发到子模块）
%%%===================================================================

-spec convert_d1_to_thing_model(#drone_status_d1{}) -> map().
convert_d1_to_thing_model(Status) ->
    uav_thing_model_d1:convert(Status).

-spec convert_d2_to_thing_model(#drone_status_d2{}) -> map().
convert_d2_to_thing_model(Status) ->
    uav_thing_model_d2:convert(Status).

-spec convert_d3_to_thing_model(#drone_status_d3{}) -> map().
convert_d3_to_thing_model(Status) ->
    uav_thing_model_d3:convert(Status).

%% 以下为其他命令的转换函数（可保留在主模块，或也拆分为独立子模块）
-spec convert_version_to_thing_model(#version_info{}) -> map().
convert_version_to_thing_model(#version_info{
    frame_length = _FrameLen,
    drone_model = _DroneModel,
    drone_id = _DroneId,
    version_string = VersionStr,
    crc = _Crc
}) ->
    #{
        <<"version_string">> => VersionStr,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

-spec convert_waypoint_to_thing_model(#waypoint_info{}) -> map().
convert_waypoint_to_thing_model(#waypoint_info{
    latitude = Lat,
    longitude = Lon,
    altitude = Alt,
    total_count = Total,
    waypoint_index = Idx,
    crc = _Crc
}) ->
    #{
        <<"waypoint_latitude">> => Lat,
        <<"waypoint_longitude">> => Lon,
        <<"waypoint_altitude">> => Alt,
        <<"waypoint_total_count">> => Total,
        <<"waypoint_index">> => Idx,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

-spec convert_surface_calibration_to_thing_model(#surface_calibration{}) -> map().
convert_surface_calibration_to_thing_model(#surface_calibration{
    channel = Ch,
    pwm_center = Pwm,
    up_ratio = Up,
    down_ratio = Down,
    crc = _Crc
}) ->
    #{
        <<"surface_channel">> => Ch,
        <<"surface_pwm_center">> => Pwm,
        <<"surface_up_ratio">> => Up,
        <<"surface_down_ratio">> => Down,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

-spec convert_battery_to_thing_model(#battery_status{}) -> map().
convert_battery_to_thing_model(#battery_status{
    status_byte = StatusByte,
    voltage = Voltage,
    activate_state = ActivateState,
    temperature1 = Temp1,
    temp2_or_count = Temp2OrCount,
    sequence = Seq,
    cmd_result = Result
}) ->
    #{
        <<"battery_status_byte">> => StatusByte,
        <<"battery_voltage">> => Voltage,
        <<"battery_activate_state">> => ActivateState,
        <<"battery_temp1">> => Temp1,
        <<"battery_temp2">> => Temp2OrCount,
        <<"battery_sequence">> => Seq,
        <<"battery_cmd_result">> => Result,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 新增：转换链路状态为物模型（简化版本，用于协议解析）
%%%===================================================================

-spec convert_link_to_thing_model(LinkData :: map()) -> {ok, map()} | {error, term()}.
convert_link_to_thing_model(LinkData) when is_map(LinkData) ->
    %% 构建简化的链路物模型
    ThingModel = #{
        <<"properties">> => #{},
        <<"services">> => #{},
        <<"events">> => #{}
    },
    %% 从LinkData中提取关键字段
    FilteredData = maps:fold(fun(K, V, Acc) ->
        case is_simple_type(V) of
            true -> Acc#{K => V};
            false -> Acc
        end
    end, #{}, LinkData),
    Result = maps:merge(ThingModel, FilteredData),
    {ok, Result};
convert_link_to_thing_model(_) ->
    {error, invalid_data}.

is_simple_type(null) -> true;
is_simple_type(Value) when is_binary(Value) -> true;
is_simple_type(Value) when is_integer(Value) -> true;
is_simple_type(Value) when is_float(Value) -> true;
is_simple_type(Value) when is_boolean(Value) -> true;
is_simple_type(_) -> false.

%%%===================================================================
%%% 新增：转换完整链路状态为物模型
%%%===================================================================

-spec convert_link_full_to_thing_model(#link_status_full{}) -> map().
convert_link_full_to_thing_model(LinkStatus) ->
    #link_status_full{
        up_link_ber = UpLinkBER,
        air_status = AirStatus,
        air_agc = AirAGC,
        work_channel = WorkChannel,
        air_set_channel = AirSetChannel,
        air_set_addr = AirSetAddr,
        down_link_ber = DownLinkBER,
        ground_status = GroundStatus,
        ground_agc1 = GroundAGC1,
        ground_agc2 = GroundAGC2,
        ground_agc3 = GroundAGC3,
        ground_work_channel = GroundWorkChannel,
        ground_set_channel = GroundSetChannel,
        ground_set_addr = GroundSetAddr,
        ground_power = GroundPower,
        ground_work_addr = GroundWorkAddr,
        range = Range,
        air_temp = AirTemp,
        air_link_ext = _AirLinkExt,
        network_access_flag = AccessFlag,
        node_address = NodeAddr,
        granted_count = Granted,
        denied_count = Denied,
        online_nodes = Online,
        latest_denied = LatestDenied,
        crc1 = Crc1,
        crc2 = Crc2
    } = LinkStatus,
    #{
        <<"link_up_ber">> => UpLinkBER,
        <<"link_air_status">> => AirStatus,
        <<"link_air_agc">> => AirAGC,
        <<"link_work_channel">> => WorkChannel,
        <<"link_air_set_channel">> => AirSetChannel,
        <<"link_air_set_addr">> => AirSetAddr,
        <<"link_down_ber">> => DownLinkBER,
        <<"link_ground_status">> => GroundStatus,
        <<"link_ground_agc1">> => GroundAGC1,
        <<"link_ground_agc2">> => GroundAGC2,
        <<"link_ground_agc3">> => GroundAGC3,
        <<"link_ground_work_channel">> => GroundWorkChannel,
        <<"link_ground_set_channel">> => GroundSetChannel,
        <<"link_ground_set_addr">> => GroundSetAddr,
        <<"link_ground_power">> => GroundPower,
        <<"link_ground_work_addr">> => GroundWorkAddr,
        <<"link_range">> => Range,
        <<"link_air_temp">> => AirTemp,
        <<"link_network_access_flag">> => AccessFlag,
        <<"link_node_address">> => NodeAddr,
        <<"link_granted_nodes">> => Granted,
        <<"link_denied_nodes">> => Denied,
        <<"link_online_nodes">> => list_to_binary(lists:join(<<",">>, [integer_to_binary(Addr) || Addr <- Online])),
        <<"link_latest_denied">> => LatestDenied,
        <<"link_crc1">> => Crc1,
        <<"link_crc2">> => Crc2,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% 存储相关函数（修改点：增加详细错误日志，并移除 funcid 字段）
%%%===================================================================

-spec save_thing_model_data(binary(), binary(), map()) -> ok | {error, term()}.
save_thing_model_data(ProductId, DevAddr, ThingModelData) when is_binary(ProductId), is_binary(DevAddr) ->

    % 强制移除 timestamp 字段，并确保有 createdat
    Data0 = maps:remove(<<"timestamp">>, ThingModelData),
    Data1 = maps:remove(<<"funcid">>, Data0),   % 强制移除可能存在的 funcid 字段
    Data = case Data1 of
        #{<<"createdat">> := _} -> Data1;
        _ -> Data1#{<<"createdat">> => erlang:system_time(millisecond)}
    end,

    %% 更新最新状态 ETS 表
    Key = {ProductId, DevAddr},
    true = ets:insert(uav_latest_state, {Key, erlang:system_time(millisecond), Data}),

    % 获取 TDengine 通道
    case dgiot_data:lookup({ProductId, ?TD_TYPE}) of
        {ok, ChannelId} ->
            case get_stable_columns(ChannelId, ProductId) of
                {error, Reason} ->
                    ?LOG(error, "[存储] 获取超级表列名失败: ~p", [Reason]),
                    {error, Reason};
                StableColumns ->
                    DataFiltered = maps:with(StableColumns, Data),
                    _OriginalCount = maps:size(Data),   % 加下划线避免未使用警告
                    FilteredCount = maps:size(DataFiltered),

                    if FilteredCount == 0 ->
                        ?LOG(error, "[存储] No columns to insert, stable may not contain any columns. StableColumns=~p", [StableColumns]),
                        {error, no_columns};
                       true ->
                        DB0 = dgiot_tdengine:get_database(ChannelId, ProductId),
                        DB = case binary:last(DB0) of
                            $. -> binary:part(DB0, 0, byte_size(DB0)-1);
                            _ -> DB0
                        end,
                        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
                        SubTable = <<"_", DeviceId/binary>>,
                        Stable = <<"_", ProductId/binary>>,
                        
                        % 确保子表存在
                        SafeDevAddr = escape_sql_string(DevAddr),
                        CreateSql = <<"CREATE TABLE IF NOT EXISTS ", DB/binary, ".", SubTable/binary, " USING ", DB/binary, ".", Stable/binary, " TAGS ('", SafeDevAddr/binary, "');">>,
                        case dgiot_tdengine:batch_sql(ChannelId, DB, CreateSql) of
                            {ok, _} -> ok;
                            {error, CreateReason} ->
                                ?LOG(error, "Failed to ensure subtable (ignoring): ~p, SQL=~s", [CreateReason, CreateSql])
                        end,
                        
                        Columns = StableColumns,
                        Values = lists:map(fun(Col) ->
                            case maps:get(Col, DataFiltered, null) of
                                null -> <<"NULL">>;
                                V when is_binary(V) -> <<"'", (escape_sql_string(V))/binary, "'">>;
                                V when is_integer(V) -> integer_to_binary(V);
                                V when is_float(V) -> float_to_binary(V, [{decimals, 6}, compact]);
                                Other -> <<"'", (dgiot_utils:to_binary(Other))/binary, "'">>
                            end
                        end, Columns),
                        ColsStr = list_to_binary(lists:join(",", Columns)),
                        ValsStr = list_to_binary(lists:join(",", Values)),
                        Sql = <<"INSERT INTO ", DB/binary, ".", SubTable/binary, " (", ColsStr/binary, ") VALUES (", ValsStr/binary, ");">>,
                        case dgiot_tdengine:batch_sql(ChannelId, DB, Sql) of
                            {ok, _Result} ->
                                ok;
                            {error, Reason} ->
                                ?LOG(error, "[存储] TDengine insert failed: ~p, SQL=~s", [Reason, Sql]),
                                {error, Reason}
                        end
                    end
            end;
        _ ->
            ?LOG(error, "[存储] 未找到TDengine通道: ProductId=~p. 请检查产品配置.", [ProductId]),
            {error, no_tdengine_channel}
    end;
save_thing_model_data(ProductId, DevAddr, _ThingModelData) ->
    ?LOG(error, "[存储] Invalid arguments: ProductId=~p, DevAddr=~p", [ProductId, DevAddr]),
    {error, {invalid_args, ProductId, DevAddr}}.

-spec save_d1_thing_model(binary(), binary(), #drone_status_d1{}) -> ok | {error, term()}.
save_d1_thing_model(ProductId, DevAddr, D1Status) ->
    ThingModel = convert_d1_to_thing_model(D1Status),
    save_thing_model_data(ProductId, DevAddr, ThingModel).

-spec save_d2_thing_model(binary(), binary(), #drone_status_d2{}) -> ok | {error, term()}.
save_d2_thing_model(ProductId, DevAddr, D2Status) ->
    ThingModel = convert_d2_to_thing_model(D2Status),
    save_thing_model_data(ProductId, DevAddr, ThingModel).

-spec save_d3_thing_model(binary(), binary(), #drone_status_d3{}) -> ok | {error, term()}.
save_d3_thing_model(ProductId, DevAddr, D3Status) ->
    ThingModel = convert_d3_to_thing_model(D3Status),
    save_thing_model_data(ProductId, DevAddr, ThingModel).

%%%===================================================================
%%% 缓存列名函数（保持不变）
%%%===================================================================

ensure_cache_table() ->
    case ets:info(?COLUMNS_CACHE) of
        undefined -> ets:new(?COLUMNS_CACHE, [set, public, named_table, {keypos, 1}]);
        _ -> ok
    end.

get_stable_columns(ChannelId, ProductId) ->
    ensure_cache_table(),
    Key = {ChannelId, ProductId},
    case ets:lookup(?COLUMNS_CACHE, Key) of
        [{Key, Columns}] -> Columns;
        [] ->
            DB0 = dgiot_tdengine:get_database(ChannelId, ProductId),
            DB = case binary:last(DB0) of
                $. -> binary:part(DB0, 0, byte_size(DB0)-1);
                _ -> DB0
            end,
            Stable = <<"_", ProductId/binary>>,
            Sql = <<"DESCRIBE ", DB/binary, ".", Stable/binary>>,
            ?LOG(debug, "Fetching stable columns: ~s", [Sql]),
            case dgiot_tdengine:batch_sql(ChannelId, DB, Sql) of
                {ok, #{<<"column_meta">> := _ColMeta, <<"data">> := Data}} ->
                    Columns = lists:filtermap(fun([ColName, _Type, _Len, Note, _, _, _]) ->
                        case Note of
                            <<"TAG">> -> false;
                            _ -> {true, ColName}
                        end
                    end, Data),
                    ets:insert(?COLUMNS_CACHE, {Key, Columns}),
                    Columns;
                {error, Reason} ->
                    ?LOG(error, "Failed to describe stable ~s: ~p", [Stable, Reason]),
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% 辅助函数（保持不变）
%%%===================================================================

-spec safe_binary_to_hex(binary() | integer() | undefined | list()) -> binary().
safe_binary_to_hex(Bin) when is_binary(Bin) ->
    binary_to_hex(Bin);
safe_binary_to_hex(0) ->
    <<>>;
safe_binary_to_hex(Val) when is_integer(Val) ->
    integer_to_binary(Val);
safe_binary_to_hex(undefined) ->
    <<>>;
safe_binary_to_hex(List) when is_list(List) ->
    try list_to_binary(List) of
        Bin -> binary_to_hex(Bin)
    catch
        _:_ -> <<>>
    end;
safe_binary_to_hex(_) ->
    <<>>.

binary_to_hex(<<>>) -> <<>>;
binary_to_hex(Bin) ->
    << <<(hex_char(H)), (hex_char(L))>> || <<H:4, L:4>> <= Bin >>.

hex_char(0) -> $0; hex_char(1) -> $1; hex_char(2) -> $2; hex_char(3) -> $3;
hex_char(4) -> $4; hex_char(5) -> $5; hex_char(6) -> $6; hex_char(7) -> $7;
hex_char(8) -> $8; hex_char(9) -> $9; hex_char(10) -> $A; hex_char(11) -> $B;
hex_char(12) -> $C; hex_char(13) -> $D; hex_char(14) -> $E; hex_char(15) -> $F.

-spec escape_sql_string(binary()) -> binary().
escape_sql_string(Bin) when is_binary(Bin) ->
    re:replace(Bin, "'", "''", [global, {return, binary}]);
escape_sql_string(_) ->
    <<>>.