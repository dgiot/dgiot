%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_ground_station_mapper - 地测口命令映射服务
%%% 
%%% 负责维护CommandId与测试项ID、步骤索引的映射关系，实现指令闭环跟踪
%%% 由于EB90协议帧没有预留字段用于携带跟踪信息，需要通过地测口服务
%%% 建立外部映射来实现指令与测试项的关联
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_ground_station_mapper).

-include_lib("dgiot/include/logger.hrl").

%% 命令映射ETS表
-define(TABLE_COMMAND_MAPPING, uav_command_mapping).

%% 命令映射记录
-record(command_mapping, {
    command_id :: binary(),           % 命令ID
    test_item_id :: binary(),         % 测试项ID
    step_index :: integer(),          % 步骤索引
    station_id :: integer(),          % 工位ID
    command_type :: atom(),           % 命令类型: uav | plc | fixture
    command_code :: integer(),        % 命令码
    command_value :: integer(),       % 命令值
    sent_time :: integer(),           % 发送时间戳(毫秒)
    timeout :: integer(),             % 超时时间(毫秒，默认5000)
    status :: atom()                  % 状态: pending | waiting_response | completed | timeout
}).

%% API
-export([
    init_mapping_table/0,
    register_command/7,
    get_mapping/1,
    update_mapping_status/2,
    remove_mapping/1,
    cleanup_timeout_commands/0,
    get_pending_commands/0,
    notify_command_response/2,
    notify_command_timeout/1,
    start_cleanup_timer/1,
    stop_cleanup_timer/1
]).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 初始化命令映射表
-spec init_mapping_table() -> ok.
init_mapping_table() ->
    case ets:info(?TABLE_COMMAND_MAPPING) of
        undefined ->
            ets:new(?TABLE_COMMAND_MAPPING, [
                named_table,
                public,
                {keypos, #command_mapping.command_id},
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ?LOG(info, <<"【地测口映射】命令映射表初始化完成"/utf8>>),
            ok;
        _ ->
            ?LOG(info, <<"【地测口映射】命令映射表已存在"/utf8>>),
            ok
    end.

%% @doc 注册命令映射
-spec register_command(
    CommandId :: binary(),
    TestItemId :: binary(),
    StepIndex :: integer(),
    StationId :: integer(),
    CommandType :: atom(),
    CommandCode :: integer(),
    CommandValue :: integer()
) -> ok | {error, term()}.
register_command(CommandId, TestItemId, StepIndex, StationId, CommandType, CommandCode, CommandValue) ->
    init_mapping_table(),
    Now = erlang:system_time(millisecond),
    Timeout = 5000,  % 默认5秒超时
    
    Mapping = #command_mapping{
        command_id = CommandId,
        test_item_id = TestItemId,
        step_index = StepIndex,
        station_id = StationId,
        command_type = CommandType,
        command_code = CommandCode,
        command_value = CommandValue,
        sent_time = Now,
        timeout = Timeout,
        status = pending
    },
    
    case ets:insert_new(?TABLE_COMMAND_MAPPING, Mapping) of
        true ->
            ?LOG(info, <<"【地测口映射】注册命令映射: CommandId=~p, TestItemId=~p, StepIndex=~p, StationId=~p, Type=~p, Code=~p, Value=~p"/utf8>>, 
                 [CommandId, TestItemId, StepIndex, StationId, CommandType, CommandCode, CommandValue]),
            ok;
        false ->
            ?LOG(warning, <<"【地测口映射】命令ID已存在: ~p"/utf8>>, [CommandId]),
            {error, command_id_exists}
    end.

%% @doc 获取命令映射
-spec get_mapping(CommandId :: binary()) -> {ok, map()} | {error, not_found}.
get_mapping(CommandId) ->
    case ets:lookup(?TABLE_COMMAND_MAPPING, CommandId) of
        [#command_mapping{} = Mapping] ->
            Map = #{
                command_id => Mapping#command_mapping.command_id,
                test_item_id => Mapping#command_mapping.test_item_id,
                step_index => Mapping#command_mapping.step_index,
                station_id => Mapping#command_mapping.station_id,
                command_type => Mapping#command_mapping.command_type,
                command_code => Mapping#command_mapping.command_code,
                command_value => Mapping#command_mapping.command_value,
                sent_time => Mapping#command_mapping.sent_time,
                timeout => Mapping#command_mapping.timeout,
                status => Mapping#command_mapping.status
            },
            {ok, Map};
        [] ->
            {error, not_found}
    end.

%% @doc 更新映射状态
-spec update_mapping_status(CommandId :: binary(), Status :: atom()) -> ok | {error, not_found}.
update_mapping_status(CommandId, Status) ->
    case ets:lookup(?TABLE_COMMAND_MAPPING, CommandId) of
        [#command_mapping{} = Mapping] ->
            Updated = Mapping#command_mapping{status = Status},
            ets:insert(?TABLE_COMMAND_MAPPING, Updated),
            ?LOG(info, <<"【地测口映射】更新命令状态: CommandId=~p, Status=~p"/utf8>>, [CommandId, Status]),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc 移除命令映射
-spec remove_mapping(CommandId :: binary()) -> ok | {error, not_found}.
remove_mapping(CommandId) ->
    case ets:take(?TABLE_COMMAND_MAPPING, CommandId) of
        [#command_mapping{}] ->
            ?LOG(info, <<"【地测口映射】移除命令映射: CommandId=~p"/utf8>>, [CommandId]),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc 清理超时命令
-spec cleanup_timeout_commands() -> {ok, integer()}.
cleanup_timeout_commands() ->
    init_mapping_table(),
    Now = erlang:system_time(millisecond),
    
    % 查找所有超时的命令
    TimeoutCommands = ets:foldl(
        fun(#command_mapping{command_id = Id, sent_time = SentTime, timeout = Timeout, status = Status}, Acc) ->
            case Status =:= pending andalso (Now - SentTime) > Timeout of
                true -> [Id | Acc];
                false -> Acc
            end
        end,
        [],
        ?TABLE_COMMAND_MAPPING
    ),
    
    % 移除超时命令并通知超时
    lists:foreach(
        fun(CommandId) ->
            ets:delete(?TABLE_COMMAND_MAPPING, CommandId),
            notify_command_timeout(CommandId),
            ?LOG(warning, <<"【地测口映射】命令超时: CommandId=~p"/utf8>>, [CommandId])
        end,
        TimeoutCommands
    ),
    
    {ok, length(TimeoutCommands)}.

%% @doc 获取所有待处理命令
-spec get_pending_commands() -> {ok, [map()]}.
get_pending_commands() ->
    init_mapping_table(),
    
    PendingCommands = ets:foldl(
        fun(#command_mapping{status = Status} = Mapping, Acc) ->
            case Status =:= pending of
                true ->
                    Map = #{
                        command_id => Mapping#command_mapping.command_id,
                        test_item_id => Mapping#command_mapping.test_item_id,
                        step_index => Mapping#command_mapping.step_index,
                        station_id => Mapping#command_mapping.station_id,
                        command_type => Mapping#command_mapping.command_type,
                        command_code => Mapping#command_mapping.command_code,
                        command_value => Mapping#command_mapping.command_value,
                        sent_time => Mapping#command_mapping.sent_time,
                        timeout => Mapping#command_mapping.timeout,
                        elapsed => erlang:system_time(millisecond) - Mapping#command_mapping.sent_time
                    },
                    [Map | Acc];
                false ->
                    Acc
            end
        end,
        [],
        ?TABLE_COMMAND_MAPPING
    ),
    
    {ok, PendingCommands}.

%% @doc 通知命令响应
-spec notify_command_response(CommandId :: binary(), ResponseData :: map()) -> ok | {error, term()}.
notify_command_response(CommandId, _ResponseData) ->
    case get_mapping(CommandId) of
        {ok, Mapping} ->
            #{
                test_item_id := TestItemId,
                step_index := StepIndex,
                command_type := _CommandType,
                station_id := _StationId
            } = Mapping,
            
            % 更新命令状态为完成
            update_mapping_status(CommandId, completed),
            
            % 通知测试项管理器
            case TestItemId of
                undefined ->
                    ?LOG(info, <<"【地测口映射】命令响应无测试项关联: CommandId=~p"/utf8>>, [CommandId]),
                    ok;
                _ ->
                    %% TODO: dgiot_uav_test_manager:update_test_step/4 函数不存在
                    %% 暂时注释掉，等待实现
                    ?LOG(info, <<"【地测口映射】命令响应处理（待实现）: CommandId=~p, TestItemId=~p, StepIndex=~p"/utf8>>,
                         [CommandId, TestItemId, StepIndex]),
                    ok
            end;
            
        {error, not_found} ->
            ?LOG(warning, <<"【地测口映射】未找到命令映射: CommandId=~p"/utf8>>, [CommandId]),
            {error, mapping_not_found}
    end.

%% @doc 通知命令超时
-spec notify_command_timeout(CommandId :: binary()) -> ok.
notify_command_timeout(CommandId) ->
    case get_mapping(CommandId) of
        {ok, Mapping} ->
            #{
                test_item_id := TestItemId,
                step_index := StepIndex,
                command_value := _CommandValue
            } = Mapping,
            
            % 通知测试项管理器
            case TestItemId of
                undefined ->
                    ?LOG(info, <<"【地测口映射】命令超时无测试项关联: CommandId=~p"/utf8>>, [CommandId]),
                    ok;
                _ ->
                    %% TODO: dgiot_uav_test_manager:update_test_step/4 函数不存在
                    %% 暂时注释掉，等待实现
                    ?LOG(warning, <<"【地测口映射】命令超时（待实现）: CommandId=~p, TestItemId=~p, StepIndex=~p"/utf8>>,
                         [CommandId, TestItemId, StepIndex]),
                    ok
            end;
        _ ->
            ok
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 启动定时清理任务
-spec start_cleanup_timer(Interval :: integer()) -> {ok, reference()}.
start_cleanup_timer(Interval) ->
    timer:apply_interval(Interval, ?MODULE, cleanup_timeout_commands, []).

%% @doc 停止定时清理任务
-spec stop_cleanup_timer(reference()) -> ok.
stop_cleanup_timer(TimerRef) ->
    timer:cancel(TimerRef),
    ok.