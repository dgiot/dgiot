%%%-------------------------------------------------------------------
%%% @doc 无人机测试管理器（统一版本）
%%% 负责测试项的状态管理和生命周期控制
%%%-------------------------------------------------------------------
-module(dgiot_uav_test_manager).
-author("johnliu").

-behaviour(gen_server).

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    start_link/0,
    stop/0,
    
    %% 测试项管理
    create_test_item/3,
    update_test_item/2,
    delete_test_item/1,
    get_test_item/1,
    list_test_items/0,
    list_test_items_by_station/1,
    
    %% 测试执行管理
    start_test/2,
    stop_test/1,
    pause_test/1,
    resume_test/1,
    get_test_status/1,
    get_test_history/2,
    
    %% 事件监听
    subscribe/0,
    unsubscribe/0,
    
    %% 测试
    test/0
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

%% 记录定义
-record(test_item_state, {
    id :: binary(),
    name :: binary(),
    station_id :: integer(),
    station_name :: binary(),
    steps = [] :: list(),
    status = pending :: pending | active | archived,
    created_at :: integer(),
    updated_at :: integer(),
    last_test_id :: binary() | undefined,
    last_test_status :: atom() | undefined
}).

-record(test_state, {
    test_id :: binary(),
    test_item_id :: binary(),
    device_id :: binary(),
    station_id :: integer(),
    status :: pending | running | paused | completed | failed,
    start_time :: integer(),
    end_time :: integer() | undefined,
    current_step :: integer(),
    total_steps :: integer(),
    progress :: float(),
    executor_pid :: pid() | undefined
}).

%% ETS表名
-define(TEST_ITEMS_TABLE, uav_test_items).
-define(TEST_STATES_TABLE, uav_test_states).

%% 订阅者列表
-define(SUBSCRIBERS, uav_test_subscribers).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动管理器
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 停止管理器
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc 创建测试项
-spec create_test_item(binary(), binary(), integer()) -> {ok, binary()} | {error, term()}.
create_test_item(Name, StationName, StationId) ->
    gen_server:call(?MODULE, {create_test_item, Name, StationName, StationId}).

%% @doc 更新测试项
-spec update_test_item(binary(), map()) -> ok | {error, term()}.
update_test_item(TestItemId, Updates) ->
    gen_server:call(?MODULE, {update_test_item, TestItemId, Updates}).

%% @doc 删除测试项
-spec delete_test_item(binary()) -> ok | {error, term()}.
delete_test_item(TestItemId) ->
    gen_server:call(?MODULE, {delete_test_item, TestItemId}).

%% @doc 获取测试项
-spec get_test_item(binary()) -> {ok, map()} | {error, term()}.
get_test_item(TestItemId) ->
    gen_server:call(?MODULE, {get_test_item, TestItemId}).

%% @doc 列出所有测试项
-spec list_test_items() -> {ok, list()} | {error, term()}.
list_test_items() ->
    gen_server:call(?MODULE, list_test_items).

%% @doc 按工位列出测试项
-spec list_test_items_by_station(integer() | binary()) -> {ok, list()} | {error, term()}.
list_test_items_by_station(StationId) when is_integer(StationId) ->
    gen_server:call(?MODULE, {list_test_items_by_station, StationId});
list_test_items_by_station(StationName) when is_binary(StationName) ->
    gen_server:call(?MODULE, {list_test_items_by_station_name, StationName}).

%% @doc 启动测试
-spec start_test(binary(), binary()) -> {ok, binary()} | {error, term()}.
start_test(TestItemId, DeviceId) ->
    gen_server:call(?MODULE, {start_test, TestItemId, DeviceId}).

%% @doc 停止测试
-spec stop_test(binary()) -> ok | {error, term()}.
stop_test(TestId) ->
    gen_server:call(?MODULE, {stop_test, TestId}).

%% @doc 暂停测试
-spec pause_test(binary()) -> ok | {error, term()}.
pause_test(TestId) ->
    gen_server:call(?MODULE, {pause_test, TestId}).

%% @doc 恢复测试
-spec resume_test(binary()) -> ok | {error, term()}.
resume_test(TestId) ->
    gen_server:call(?MODULE, {resume_test, TestId}).

%% @doc 获取测试状态
-spec get_test_status(binary()) -> {ok, map()} | {error, term()}.
get_test_status(TestId) ->
    gen_server:call(?MODULE, {get_test_status, TestId}).

%% @doc 获取测试历史
-spec get_test_history(binary(), integer()) -> {ok, list()} | {error, term()}.
get_test_history(DeviceId, Limit) ->
    gen_server:call(?MODULE, {get_test_history, DeviceId, Limit}).

%% @doc 订阅测试事件
-spec subscribe() -> ok.
subscribe() ->
    Pid = self(),
    gen_server:cast(?MODULE, {subscribe, Pid}).

%% @doc 取消订阅
-spec unsubscribe() -> ok.
unsubscribe() ->
    Pid = self(),
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% 初始化ETS表
    init_ets_tables(),
    
    %% 启动监控
    start_monitor(),
    
    ?LOG(info, "[MANAGER] 测试管理器启动"),
    {ok, #{}}.

handle_call({create_test_item, Name, StationName, StationId}, _From, State) ->
    TestItemId = generate_test_item_id(),
    Now = erlang:system_time(millisecond),
    
    TestItem = #test_item_state{
        id = TestItemId,
        name = Name,
        station_id = StationId,
        station_name = StationName,
        status = pending,
        created_at = Now,
        updated_at = Now
    },
    
    ets:insert(?TEST_ITEMS_TABLE, TestItem),
    ?LOG(info, "[MANAGER] 测试项已创建 - Id:~s, Name:~s", [TestItemId, Name]),
    {reply, {ok, TestItemId}, State};

handle_call({update_test_item, TestItemId, Updates}, _From, State) ->
    case ets:lookup(?TEST_ITEMS_TABLE, TestItemId) of
        [#test_item_state{} = Item] ->
            Updated = update_test_item_record(Item, Updates),
            ets:insert(?TEST_ITEMS_TABLE, Updated),
            ?LOG(info, "[MANAGER] 测试项已更新 - Id:~s", [TestItemId]),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_test_item, TestItemId}, _From, State) ->
    ets:delete(?TEST_ITEMS_TABLE, TestItemId),
    ?LOG(info, "[MANAGER] 测试项已删除 - Id:~s", [TestItemId]),
    {reply, ok, State};

handle_call({get_test_item, TestItemId}, _From, State) ->
    case ets:lookup(?TEST_ITEMS_TABLE, TestItemId) of
        [#test_item_state{} = Item] ->
            {reply, {ok, test_item_to_map(Item)}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_test_items, _From, State) ->
    Items = ets:foldl(fun(Item, Acc) -> [test_item_to_map(Item) | Acc] end, [], ?TEST_ITEMS_TABLE),
    {reply, {ok, Items}, State};

handle_call({list_test_items_by_station, StationId}, _From, State) ->
    Items = ets:foldl(fun
        (#test_item_state{station_id = Sid} = Item, Acc) when Sid =:= StationId ->
            [test_item_to_map(Item) | Acc];
        (_, Acc) -> Acc
    end, [], ?TEST_ITEMS_TABLE),
    {reply, {ok, Items}, State};

handle_call({list_test_items_by_station_name, StationName}, _From, State) ->
    Items = ets:foldl(fun
        (#test_item_state{station_name = SName} = Item, Acc) when SName =:= StationName ->
            [test_item_to_map(Item) | Acc];
        (_, Acc) -> Acc
    end, [], ?TEST_ITEMS_TABLE),
    {reply, {ok, Items}, State};

handle_call({start_test, TestItemId, DeviceId}, _From, State) ->
    case ets:lookup(?TEST_ITEMS_TABLE, TestItemId) of
        [#test_item_state{station_id = StationId}] ->
            %% 创建测试状态
            TestId = generate_test_id(),
            Now = erlang:system_time(millisecond),
            
            TestState = #test_state{
                test_id = TestId,
                test_item_id = TestItemId,
                device_id = DeviceId,
                station_id = StationId,
                status = running,
                start_time = Now,
                current_step = 0,
                total_steps = 0,
                progress = 0.0,
                executor_pid = undefined
            },
            
            ets:insert(?TEST_STATES_TABLE, TestState),
            
            %% 更新测试项的最后测试信息
            update_test_item_last_test(TestItemId, TestId),
            
            %% 启动执行器
            {ok, ExecutorPid} = start_executor(TestId, TestItemId, DeviceId, StationId),
            
            %% 更新执行器PID
            UpdatedState = TestState#test_state{executor_pid = ExecutorPid},
            ets:insert(?TEST_STATES_TABLE, UpdatedState),
            
            %% 通知订阅者
            notify_subscribers({test_started, TestId, DeviceId, StationId}),
            
            ?LOG(info, "[MANAGER] 测试已启动 - TestId:~s, DeviceId:~s", [TestId, DeviceId]),
            {reply, {ok, TestId}, State};
        [] ->
            {reply, {error, test_item_not_found}, State}
    end;

handle_call({stop_test, TestId}, _From, State) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{status = running, executor_pid = Pid}] ->
            %% 停止执行器
            stop_executor(Pid),
            
            %% 更新状态
            Updated = #test_state{
                test_id = TestId,
                status = completed,
                end_time = erlang:system_time(millisecond)
            },
            ets:insert(?TEST_STATES_TABLE, Updated),
            
            notify_subscribers({test_stopped, TestId}),
            ?LOG(info, "[MANAGER] 测试已停止 - TestId:~s", [TestId]),
            {reply, ok, State};
        [#test_state{}] ->
            {reply, {error, test_not_running}, State};
        [] ->
            {reply, {error, test_not_found}, State}
    end;

handle_call({pause_test, TestId}, _From, State) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{status = running, executor_pid = Pid}] ->
            pause_executor(Pid),
            update_test_status(TestId, paused),
            notify_subscribers({test_paused, TestId}),
            {reply, ok, State};
        [#test_state{}] ->
            {reply, {error, test_not_running}, State};
        [] ->
            {reply, {error, test_not_found}, State}
    end;

handle_call({resume_test, TestId}, _From, State) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{status = paused, executor_pid = Pid}] ->
            resume_executor(Pid),
            update_test_status(TestId, running),
            notify_subscribers({test_resumed, TestId}),
            {reply, ok, State};
        [#test_state{}] ->
            {reply, {error, test_not_paused}, State};
        [] ->
            {reply, {error, test_not_found}, State}
    end;

handle_call({get_test_status, TestId}, _From, State) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{} = TestState] ->
            {reply, {ok, test_state_to_map(TestState)}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_test_history, DeviceId, Limit}, _From, State) ->
    History = dgiot_uav_test_storage:get_by_device(DeviceId, #{limit => Limit}),
    {reply, History, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Pid}, State) ->
    add_subscriber(Pid),
    {noreply, State};

handle_cast({unsubscribe, Pid}, State) ->
    remove_subscriber(Pid),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({test_completed, TestId, Results}, State) ->
    update_test_status(TestId, completed),
    notify_subscribers({test_completed, TestId, Results}),
    {noreply, State};

handle_info({test_failed, TestId, Reason}, State) ->
    update_test_status(TestId, failed),
    notify_subscribers({test_failed, TestId, Reason}),
    {noreply, State};

handle_info({test_progress, TestId, Progress}, State) ->
    update_test_progress(TestId, Progress),
    notify_subscribers({test_progress, TestId, Progress}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG(info, "[MANAGER] 测试管理器停止"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

init_ets_tables() ->
    %% 测试项表
    case ets:info(?TEST_ITEMS_TABLE) of
        undefined ->
            ets:new(?TEST_ITEMS_TABLE, [named_table, public, set, {keypos, #test_item_state.id}]);
        _ -> ok
    end,
    
    %% 测试状态表
    case ets:info(?TEST_STATES_TABLE) of
        undefined ->
            ets:new(?TEST_STATES_TABLE, [named_table, public, set, {keypos, #test_state.test_id}]);
        _ -> ok
    end,
    
    %% 订阅者表
    case ets:info(?SUBSCRIBERS) of
        undefined ->
            ets:new(?SUBSCRIBERS, [named_table, public, set]);
        _ -> ok
    end.

start_monitor() ->
    %% 启动进度监控定时器
    erlang:send_after(5000, self(), check_progress).

start_executor(TestId, TestItemId, DeviceId, StationId) ->
    case dgiot_uav_test_executor:start(TestItemId, StationId, #{device_id => DeviceId}) of
        {ok, _} ->
            %% 返回一个虚拟PID用于状态管理
            {ok, spawn(fun() -> wait_for_test(TestId) end)};
        Error -> Error
    end.

wait_for_test(TestId) ->
    receive
        {test_done, TestId} -> ok
    after 3600000 -> ok  % 1小时超时
    end.

stop_executor(Pid) when is_pid(Pid) ->
    Pid ! stop,
    ok;
stop_executor(_) -> ok.

pause_executor(Pid) ->
    Pid ! pause,
    ok.

resume_executor(Pid) ->
    Pid ! resume,
    ok.

update_test_status(TestId, Status) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{} = State] ->
            Updated = State#test_state{
                status = Status,
                end_time = if Status =:= completed; Status =:= failed -> erlang:system_time(millisecond); true -> State#test_state.end_time end
            },
            ets:insert(?TEST_STATES_TABLE, Updated);
        _ -> ok
    end.

update_test_progress(TestId, Progress) ->
    case ets:lookup(?TEST_STATES_TABLE, TestId) of
        [#test_state{} = State] ->
            Updated = State#test_state{
                progress = Progress,
                current_step = maps:get(current_step, Progress, 0),
                total_steps = maps:get(total_steps, Progress, 0)
            },
            ets:insert(?TEST_STATES_TABLE, Updated);
        _ -> ok
    end.

update_test_item_last_test(TestItemId, TestId) ->
    case ets:lookup(?TEST_ITEMS_TABLE, TestItemId) of
        [#test_item_state{} = Item] ->
            Updated = Item#test_item_state{
                last_test_id = TestId,
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TEST_ITEMS_TABLE, Updated);
        _ -> ok
    end.

add_subscriber(Pid) ->
    ets:insert(?SUBSCRIBERS, {Pid, erlang:system_time(millisecond)}).

remove_subscriber(Pid) ->
    ets:delete(?SUBSCRIBERS, Pid).

notify_subscribers(Message) ->
    ets:foldl(fun({Pid, _}, _) ->
        try Pid ! Message catch _:_ -> ok end
    end, ok, ?SUBSCRIBERS).

generate_test_item_id() ->
    Timestamp = erlang:system_time(millisecond),
    Rand = rand:uniform(9999),
    list_to_binary(io_lib:format("ti_~p_~4..0B", [Timestamp, Rand])).

generate_test_id() ->
    Timestamp = erlang:system_time(millisecond),
    Rand = rand:uniform(9999),
    list_to_binary(io_lib:format("test_~p_~4..0B", [Timestamp, Rand])).

test_item_to_map(#test_item_state{
    id = Id,
    name = Name,
    station_id = StationId,
    station_name = StationName,
    status = Status,
    created_at = Created,
    updated_at = Updated,
    last_test_id = LastTestId,
    last_test_status = LastStatus
}) ->
    #{
        id => Id,
        name => Name,
        station_id => StationId,
        station_name => StationName,
        status => Status,
        created_at => Created,
        updated_at => Updated,
        last_test_id => LastTestId,
        last_test_status => LastStatus
    }.

test_state_to_map(#test_state{
    test_id = TestId,
    test_item_id = TestItemId,
    device_id = DeviceId,
    station_id = StationId,
    status = Status,
    start_time = StartTime,
    end_time = EndTime,
    current_step = CurrentStep,
    total_steps = TotalSteps,
    progress = Progress
}) ->
    #{
        test_id => TestId,
        test_item_id => TestItemId,
        device_id => DeviceId,
        station_id => StationId,
        status => Status,
        start_time => StartTime,
        end_time => EndTime,
        current_step => CurrentStep,
        total_steps => TotalSteps,
        progress => Progress
    }.

update_test_item_record(Item, Updates) ->
    maps:fold(fun
        (<<"name">>, V, Acc) -> Acc#test_item_state{name = V};
        (<<"station_id">>, V, Acc) -> Acc#test_item_state{station_id = V};
        (<<"station_name">>, V, Acc) -> Acc#test_item_state{station_name = V};
        (<<"status">>, V, Acc) when is_atom(V) -> Acc#test_item_state{status = V};
        (<<"status">>, V, Acc) -> Acc#test_item_state{status = binary_to_atom(V, utf8)};
        (_, _, Acc) -> Acc
    end, Item#test_item_state{updated_at = erlang:system_time(millisecond)}, Updates).

%%====================================================================
%% 测试函数
%%====================================================================
-spec test() -> ok.
test() ->
    io:format("~n========== 测试管理器测试 ==========~n", []),
    
    %% 启动管理器
    case start_link() of
        {ok, Pid} ->
            io:format("✓ 管理器启动成功: ~p~n", [Pid]);
        {error, {already_started, Pid}} ->
            io:format("✓ 管理器已运行: ~p~n", [Pid]);
        {error, Reason} ->
            io:format("✗ 管理器启动失败: ~p~n", [Reason]),
            return
    end,
    
    %% 创建测试项
    io:format("~n创建测试项...~n"),
    case create_test_item(<<"磁航向测试项"/utf8>>, <<"磁航向"/utf8>>, 1700) of
        {ok, TestItemId} ->
            io:format("✓ 测试项创建成功: ~s~n", [TestItemId]),
            
            %% 获取测试项
            case get_test_item(TestItemId) of
                {ok, Item} ->
                    io:format("✓ 测试项信息: ~p~n", [Item]);
                {error, Reason2} ->
                    io:format("✗ 获取失败: ~p~n", [Reason2])
            end,
            
            %% 列出测试项
            case list_test_items() of
                {ok, Items} ->
                    io:format("✓ 测试项列表数量: ~p~n", [length(Items)]);
                {error, Reason3} ->
                    io:format("✗ 列表获取失败: ~p~n", [Reason3])
            end;
        {error, CreateReason} ->
            io:format("✗ 创建失败: ~p~n", [CreateReason])
    end,
    
    io:format("~n========== 测试完成 ==========~n", []),
    ok.
