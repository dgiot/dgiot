%%%-------------------------------------------------------------------
%%% @doc
%%% station_base - 工位基类/接口定义
%%% 所有工位业务处理器都应该实现这个接口
%%% @end
%%%-------------------------------------------------------------------
-module(station_base).

%% 工位行为定义
-callback init(StationNameEn :: atom()) -> {ok, State :: map()}.
-callback handle_data(StationNameEn :: atom(), DataType :: atom(), Data :: term()) -> ok.
-callback cache_metric(StationNameEn :: atom(), MetricKey :: binary(), MetricValue :: term()) -> ok.
-callback get_metric(StationNameEn :: atom(), MetricKey :: binary()) -> {ok, term()} | {error, term()}.
-callback cleanup_expired(StationNameEn :: atom()) -> ok.

%% 通用函数
-export([
    get_station_table_name/1,
    init_station_table/1,
    cache_station_metric/3,
    get_station_metric/2,
    cleanup_expired_metrics/2
]).

%% 注意：station_base是接口/基类，不进行ETS初始化
%% ETS表由 dgiot_uav_station_manager 统一管理

-include_lib("dgiot/include/logger.hrl").

%% 工位ETS表命名模式
-define(STATION_TABLE_PREFIX, "station_").

%% 缓存过期时间（秒）
-define(DEFAULT_CACHE_EXPIRE, 20).  %% 20秒
-define(TEST_DATA_EXPIRE, 1800).    %% 30分钟

%%%===================================================================
%%% 通用工位表管理
%%%===================================================================

%% @doc 获取工位ETS表名
-spec get_station_table_name(atom()) -> atom().
get_station_table_name(StationNameEn) ->
    %% 直接使用工位英文名作为表名
    StationNameEn.

%% @doc 初始化工位ETS表
-spec init_station_table(atom()) -> ok.
init_station_table(StationNameEn) ->
    TableName = get_station_table_name(StationNameEn),
    case ets:info(TableName) of
        undefined ->
            dgiot_data:init(TableName, [public, named_table, set,
                                       {write_concurrency, true},
                                       {read_concurrency, true},
                                       {keypos, 1}]),
            ?LOG(info, "工位ETS表初始化: ~p", [TableName]),
            ok;
        _ ->
            ?LOG(debug, "工位ETS表已存在: ~p", [TableName]),
            ok
    end.

%% @doc 缓存工位指标（通用实现）
-spec cache_station_metric(atom(), binary(), term()) -> ok.
cache_station_metric(StationNameEn, MetricKey, MetricValue) ->
    init_station_table(StationNameEn),
    Now = erlang:system_time(millisecond),
    TableName = get_station_table_name(StationNameEn),
    ets:insert(TableName, {MetricKey, {Now, MetricValue}}),
    ?LOG(debug, "工位 ~p 指标 ~s 已缓存", [StationNameEn, MetricKey]).

%% @doc 获取工位指标（通用实现）
-spec get_station_metric(atom(), binary()) -> {ok, term()} | {error, term()}.
get_station_metric(StationNameEn, MetricKey) ->
    TableName = get_station_table_name(StationNameEn),
    case ets:info(TableName) of
        undefined -> 
            {error, table_not_exist};
        _ ->
            case ets:lookup(TableName, MetricKey) of
                [] -> {error, not_find};
                [{MetricKey, {Timestamp, Value}}] ->
                    Now = erlang:system_time(millisecond),
                    %% 检查是否过期
                    ExpireTime = get_expire_time(MetricKey),
                    if Now - Timestamp =< ExpireTime ->
                            {ok, Value};
                       true ->
                            {error, expired}
                    end
            end
    end.

%% @doc 清理过期指标（通用实现）
-spec cleanup_expired_metrics(atom(), integer()) -> ok.
cleanup_expired_metrics(StationNameEn, ExpireTime) ->
    TableName = get_station_table_name(StationNameEn),
    case ets:info(TableName) of
        undefined -> ok;
        _ ->
            Now = erlang:system_time(millisecond),
            %% 遍历表，删除过期数据
            ets:safe_fixtable(TableName, true),
            ets:foldl(fun({Key, {Timestamp, _Value}}, ok) ->
                if Now - Timestamp > ExpireTime ->
                        ets:delete(TableName, Key),
                        ?LOG(debug, "工位 ~p 删除过期指标: ~s", [StationNameEn, Key]);
                   true -> ok
                end
            end, ok, TableName),
            ets:safe_fixtable(TableName, false)
    end.

%% 根据指标类型获取过期时间
get_expire_time(<<"qrcode_", _/binary>>) -> ?DEFAULT_CACHE_EXPIRE * 1000;
get_expire_time(<<"drone_online", _/binary>>) -> ?DEFAULT_CACHE_EXPIRE * 1000;
get_expire_time(<<"test_", _/binary>>) -> ?TEST_DATA_EXPIRE * 1000;
get_expire_time(_) -> ?DEFAULT_CACHE_EXPIRE * 1000.