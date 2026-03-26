%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_aggregator - 无人机数据汇聚模块
%%% 负责将舵面数据、地测口数据等汇聚到无人机物模型
%%% 支持缓存聚合，定时批量写入，提高 TDengine 写入性能
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_aggregator).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    aggregate/4,
    get_latest_state/1,
    flush/0,
    get_cached_count/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("dgiot/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(FLUSH_INTERVAL, 1000).  % 1秒刷新一次
-define(MAX_CACHE_SIZE, 100).  % 最大缓存数量

%% ETS 表定义
-define(AGGREGATE_CACHE, uav_aggregate_cache).
-define(AGGREGATE_STATE, uav_aggregate_state).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 启动汇聚模块
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 汇聚数据（由 dgiot_uav_tcp_worker 调用）
-spec aggregate(binary(), binary(), map(), integer()) -> ok.
aggregate(DroneId, ProductId, Data, Timestamp) ->
    ensure_cache_table(),
    ensure_state_table(),
    gen_server:cast(?SERVER, {aggregate, DroneId, ProductId, Data, Timestamp}).

%% @doc 获取最新状态
-spec get_latest_state(binary()) -> {ok, map()} | {error, not_find}.
get_latest_state(DroneId) ->
    ensure_cache_table(),
    case ets:lookup(?AGGREGATE_STATE, DroneId) of
        [{DroneId, _Timestamp, Data}] -> {ok, Data};
        [] -> {error, not_find}
    end.

%% @doc 手动刷新缓存
-spec flush() -> ok.
flush() ->
    gen_server:cast(?SERVER, flush).

%% @doc 获取缓存数量
-spec get_cached_count() -> non_neg_integer().
get_cached_count() ->
    ensure_cache_table(),
    ets:info(?AGGREGATE_CACHE, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ensure_cache_table(),
    ensure_state_table(),
    %% 启动定时刷新定时器
    TimerRef = erlang:send_after(?FLUSH_INTERVAL, self(), flush),
    ?LOG(info, "[AGGREGATOR] 数据汇聚模块启动成功，刷新间隔=~p ms", [?FLUSH_INTERVAL]),
    {ok, #{timer_ref => TimerRef}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({aggregate, DroneId, ProductId, Data, Timestamp}, State) ->
    %% 存入缓存
    do_aggregate(DroneId, ProductId, Data, Timestamp),
    {noreply, State};

handle_cast(flush, State) ->
    do_flush(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State = #{timer_ref := _TimerRef}) ->
    %% 定时刷新
    do_flush(),
    %% 重新设置定时器
    NewTimerRef = erlang:send_after(?FLUSH_INTERVAL, self(), flush),
    {noreply, State#{timer_ref := NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% 进程退出时刷新缓存
    do_flush(),
    ?LOG(info, "[AGGREGATOR] 数据汇聚模块停止"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 执行数据汇聚
do_aggregate(DroneId, ProductId, Data, Timestamp) ->
    ensure_cache_table(),
    ensure_state_table(),

    %% 检查ProductId是否有效，跳过无效的test数据
    case ProductId of
        <<"test">> ->
            ?LOG(warning, "[AGGREGATOR] 跳过无效ProductId: ~s, DroneId=~s", [ProductId, DroneId]),
            ok;
        _ ->
            %% 1. 更新状态表（存储最新数据）
            ets:insert(?AGGREGATE_STATE, {DroneId, Timestamp, Data}),

            %% 2. 更新缓存表（带时间戳）
            CacheKey = {DroneId, ProductId, Timestamp},
            ets:insert(?AGGREGATE_CACHE, {CacheKey, Data}),

            %% 3. 检查缓存大小，超过限制则立即刷新
            CacheSize = ets:info(?AGGREGATE_CACHE, size),
            if CacheSize >= ?MAX_CACHE_SIZE ->
                ?LOG(warning, "[AGGREGATOR] 缓存达到上限(~p), 立即刷新", [CacheSize]),
                do_flush();
               true -> ok
            end
    end.

%% @doc 执行缓存刷新（批量写入 TDengine）
%% 修复：避免使用 ets:select_delete（在 bag 表上可能引起 badarg），改用安全方式清理和写入
do_flush() ->
    try
        ensure_cache_table(),

        %% 安全清理无效的 test 数据（使用 select + 逐个删除）
        MatchSpec = [{{{'$1', <<"test">>, '$2'}, '_'}, [], ['$_']}],
        case ets:select(?AGGREGATE_CACHE, MatchSpec) of
            [] ->
                ok;
            MatchedKeys ->
                DeletedCount = lists:foldl(
                    fun({Key, _Value}, Acc) ->
                        ets:delete(?AGGREGATE_CACHE, Key),
                        Acc + 1
                    end, 0, MatchedKeys),
                ?LOG(warning, "[AGGREGATOR] 清理了 ~p 条无效的 test 数据", [DeletedCount])
        end,

        CacheSize = ets:info(?AGGREGATE_CACHE, size),
        if CacheSize == 0 ->
            ok;
           true ->
            ?LOG(info, "[AGGREGATOR] 开始刷新缓存，数量: ~p", [CacheSize]),

            FailedList = ets:foldl(
                fun({Key, Data}, Acc) ->
                    case Key of
                        {DroneId, ProductId, Timestamp} when ProductId =/= <<"test">> ->
                            case save_to_thing_model(ProductId, DroneId, Data, Timestamp) of
                                ok ->
                                    Acc;
                                {error, Reason} ->
                                    ?LOG(error, "[AGGREGATOR] 保存失败: DroneId=~s, Reason=~p", [DroneId, Reason]),
                                    [{DroneId, ProductId, Data, Timestamp} | Acc]
                            end;
                        _ ->
                            %% 理论上 test 数据已被清理，此处为防御性编程
                            Acc
                    end
                end, [], ?AGGREGATE_CACHE),

            %% 清空缓存
            ets:delete_all_objects(?AGGREGATE_CACHE),

            %% 重新插入失败的数据
            lists:foreach(fun({DroneId, ProductId, Data, Timestamp}) ->
                CacheKey = {DroneId, ProductId, Timestamp},
                ets:insert(?AGGREGATE_CACHE, {CacheKey, Data})
            end, FailedList),

            case FailedList of
                [] -> ?LOG(info, "[AGGREGATOR] 刷新完成，所有数据保存成功");
                _ -> ?LOG(warning, "[AGGREGATOR] 部分数据保存失败，保留 ~p 条", [length(FailedList)])
            end
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG(error, "[AGGREGATOR] 刷新缓存时发生异常: ~p:~p~n~p", [Class, Reason, Stacktrace])
    end.

%% @doc 保存到物模型（调用 uav_thing_model 模块）
save_to_thing_model(ProductId, DroneId, Data, Timestamp) ->
    %% 添加 createdat 时间戳
    DataWithTime = Data#{<<"createdat">> => Timestamp},
    uav_thing_model:save_thing_model_data(ProductId, DroneId, DataWithTime).

%% @doc 确保缓存表存在
ensure_cache_table() ->
    case ets:info(?AGGREGATE_CACHE) of
        undefined ->
            ets:new(?AGGREGATE_CACHE, [bag, public, named_table, {keypos, 1}]);
        _ -> ok
    end.

%% @doc 确保状态表存在
ensure_state_table() ->
    case ets:info(?AGGREGATE_STATE) of
        undefined ->
            ets:new(?AGGREGATE_STATE, [set, public, named_table, {keypos, 1}]);
        _ -> ok
    end.