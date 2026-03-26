%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_test_item.hrl - 无人机测试项数据模型和记录定义
%%%
%%% 包含测试项管理相关的所有记录定义和类型声明。
%%% 主要记录：test_item, test_step, test_execution_context, test_communication,
%%% test_send_config, test_receive_config 等。
%%%
%%% 还包含大量宏定义，用于测试项状态、动作类型、目标类型等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-ifndef(DGIOT_UAV_TEST_ITEM_HRL).
-define(DGIOT_UAV_TEST_ITEM_HRL, true).

%% 包含系统头文件
%% -include_lib("dgiot/include/logger.hrl").
%% -include_lib("dgiot/include/dgiot.hrl").
-include("dgiot_uav.hrl").

%%=============================================================================
%% 宏定义
%%=============================================================================

%% 测试项产品ID
-define(TEST_ITEM_PRODUCT_ID, <<"343cf21f82">>).

%% 工位名称到ID的映射（与前端一致）
-define(STATION_NAME_MAP, #{
    <<"桁架"/utf8>>   => 1100,
    <<"拷机1"/utf8>>  => 1200,
    <<"拷机2"/utf8>>  => 1300,
    <<"总测1"/utf8>>  => 1500,
    <<"总测2"/utf8>>  => 1600,
    <<"磁航向"/utf8>> => 1700
}).

%% 目标类型映射
-define(TARGET_TYPE_MAP, #{
    <<"工位PLC"/utf8>> => <<"1">>,
    <<"治具"/utf8>>    => <<"2">>,
    <<"无人机"/utf8>>  => <<"3">>,
    <<"plc">>          => <<"1">>,
    <<"fixture">>      => <<"2">>,
    <<"uav">>          => <<"3">>
}).

%% 动作类型定义
-define(ACTION_TYPE_SEND, <<"send">>).
-define(ACTION_TYPE_RECEIVE, <<"receive">>).
-define(ACTION_TYPE_JUDGE, <<"judge">>).
-define(ACTION_TYPE_REQUEST_RESPONSE, <<"request_response">>).
-define(ACTION_TYPE_OPERATE, <<"operate">>).

%% 测试项状态
-define(TEST_ITEM_STATUS_ACTIVE, <<"active">>).
-define(TEST_ITEM_STATUS_INACTIVE, <<"inactive">>).
-define(TEST_ITEM_STATUS_DRAFT, <<"draft">>).

%% 测试执行状态
-define(TEST_EXECUTION_PENDING, <<"pending">>).
-define(TEST_EXECUTION_RUNNING, <<"running">>).
-define(TEST_EXECUTION_COMPLETED, <<"completed">>).
-define(TEST_EXECUTION_FAILED, <<"failed">>).
-define(TEST_EXECUTION_CANCELLED, <<"cancelled">>).

%% 测试结果
-define(TEST_RESULT_PASS, <<"pass">>).
-define(TEST_RESULT_FAIL, <<"fail">>).
-define(TEST_RESULT_SKIP, <<"skip">>).
-define(TEST_RESULT_IN_PROGRESS, <<"in_progress">>).

%% 默认等待时间（秒）
-define(DEFAULT_WAIT_TIME, 0.0).

%%=============================================================================
%% 类型定义
%%=============================================================================

%% 目标类型
-type target_type() :: binary().

%% 动作类型
-type action_type() :: binary().

%% 测试项状态
-type test_item_status() :: binary().

%% 测试执行状态
-type test_execution_status() :: binary().

%% 测试结果
-type test_result() :: binary().

%% 通信协议
-type communication_protocol() :: binary().

%% 发送内容格式
-type send_format() :: binary().

%%=============================================================================
%% 记录定义
%%=============================================================================

%% 通信配置记录
-record(test_communication, {
    ip :: binary(),                    % IP地址
    port :: integer() | undefined,     % 端口
    dtu_port :: integer() | undefined, % DTU端口
    protocol :: communication_protocol() % 协议类型
}).

%% 发送配置记录
-record(test_send_config, {
    content :: binary(),               % 发送内容
    address :: binary() | undefined,   % 地址（如寄存器地址）
    format :: send_format() | undefined % 内容格式
}).

%% 接收配置记录
-record(test_receive_config, {
    recv_content :: binary() | undefined,   % 期望接收内容
    timeout :: integer() | undefined,       % 超时时间（毫秒）
    retry_count :: integer() | undefined    % 重试次数
}).

%% 测试步骤记录
-record(test_step, {
    step_number :: integer(),          % 步骤编号（从1开始）
    action_type :: action_type(),      % 动作类型
    description :: binary(),           % 步骤描述
    target :: target_type(),           % 目标设备类型
    communication :: #test_communication{} | undefined, % 通信配置
    send :: #test_send_config{} | undefined, % 发送配置
    recv_config :: #test_receive_config{} | undefined, % 接收配置
    wait :: float(),                   % 等待时间（秒）
    notes :: binary() | undefined,     % 备注
    expected_result :: binary() | undefined, % 预期结果
    actual_result :: binary() | undefined,   % 实际结果
    result :: test_result() | undefined,     % 步骤结果
    start_time :: integer() | undefined,     % 开始时间
    end_time :: integer() | undefined        % 结束时间
}).

%% 测试项公共参数记录
-record(test_item_common_params, {
    port :: integer(),                 % 端口
    station_name :: binary(),          % 工位名称
    station_number :: integer(),       % 工位编号
    test_station_name :: binary()      % 测试工位名称
}).

%% 测试项记录
-record(test_item, {
    object_id :: binary(),             % Parse Server对象ID
    device_id :: binary(),             % 设备ID
    devaddr :: binary(),               % 设备地址（格式：工位_测试项）
    name :: binary(),                  % 测试项名称
    product_id :: binary(),            % 产品ID
    content :: map(),                  % 内容（包含steps等）
    common_params :: #test_item_common_params{}, % 公共参数
    is_test_item_device :: boolean(),  % 是否为测试项设备
    last_updated :: integer(),         % 最后更新时间戳
    test_item_count :: integer(),      % 测试步骤数量
    status :: test_item_status(),      % 状态
    created_at :: integer() | undefined, % 创建时间
    updated_at :: integer() | undefined  % 更新时间
}).

%% 测试执行上下文记录
-record(test_execution_context, {
    test_item_id :: binary(),          % 测试项ID
    device_id :: binary(),             % 设备ID
    station_id :: integer(),           % 工位ID
    station_addr :: binary(),          % 工位地址
    current_step :: integer(),         % 当前步骤
    total_steps :: integer(),          % 总步骤数
    status :: test_execution_status(), % 执行状态
    result :: test_result() | undefined, % 执行结果
    start_time :: integer() | undefined, % 开始时间
    end_time :: integer() | undefined,   % 结束时间
    error_message :: binary() | undefined, % 错误信息
    step_results = [] :: list(#test_step{}), % 步骤结果列表
    metadata = #{} :: map()            % 元数据
}).

%% 测试项查询选项记录
-record(test_item_query_options, {
    station_id :: integer() | undefined, % 工位ID
    station_name :: binary() | undefined, % 工位名称
    status :: test_item_status() | undefined, % 状态
    limit :: integer() | undefined,     % 限制数量
    offset :: integer() | undefined,    % 偏移量
    sort_by :: binary() | undefined,    % 排序字段
    sort_order :: asc | desc | undefined % 排序顺序
}).

%% 测试项缓存记录
-record(test_item_cache_entry, {
    key :: binary(),                    % 缓存键
    value :: #test_item{} | list(#test_item{}), % 缓存值
    timestamp :: integer(),             % 缓存时间戳
    ttl :: integer()                    % 生存时间（秒）
}).

%%=============================================================================
%% 函数类型定义
%%=============================================================================

%% 测试步骤执行函数类型
-type step_execution_fun() :: fun((#test_step{}, #test_execution_context{}) -> 
    {ok, #test_step{}, #test_execution_context{}} | {error, binary()}).

%% 测试项验证函数类型
-type test_item_validation_fun() :: fun((#test_item{}) -> 
    {ok, #test_item{}} | {error, binary()}).

%% 测试项加载函数类型
-type test_item_loader_fun() :: fun((binary() | integer()) -> 
    {ok, list(#test_item{})} | {error, binary()}).

%% 测试结果处理函数类型
-type test_result_handler_fun() :: fun((#test_execution_context{}) -> 
    {ok, map()} | {error, binary()}).

%%=============================================================================
%% 导出宏
%%=============================================================================

%% 测试项字段列表（用于查询和更新）
-define(TEST_ITEM_FIELDS, [
    <<"objectId">>, <<"deviceId">>, <<"devaddr">>, <<"name">>, 
    <<"product">>, <<"content">>, <<"common_params">>, 
    <<"is_test_item_device">>, <<"last_updated">>, 
    <<"test_item_count">>, <<"status">>, <<"createdAt">>, <<"updatedAt">>
]).

%% 测试步骤字段列表
-define(TEST_STEP_FIELDS, [
    <<"step_number">>, <<"action_type">>, <<"description">>, <<"target">>,
    <<"communication">>, <<"send">>, <<"receive">>, <<"wait">>, 
    <<"notes">>, <<"expected_result">>, <<"actual_result">>, <<"result">>
]).

%% 默认测试项状态
-define(DEFAULT_TEST_ITEM_STATUS, <<"active">>).

%% 默认缓存TTL（5分钟）
-define(DEFAULT_CACHE_TTL, 300).

%% 最大测试步骤数
-define(MAX_TEST_STEPS, 100).

%% 最小测试步骤数
-define(MIN_TEST_STEPS, 1).

%% 默认等待时间（秒）
-define(DEFAULT_STEP_WAIT_TIME, 0.0).

%% 默认重试次数
-define(DEFAULT_RETRY_COUNT, 3).

%% 默认超时时间（毫秒）
-define(DEFAULT_TIMEOUT, 5000).

%%=============================================================================
%% 辅助函数宏
%%=============================================================================

%% 创建默认测试步骤
-define(DEFAULT_TEST_STEP(StepNum, Desc, TargetType),
    #test_step{
        step_number = StepNum,
        action_type = <<"send">>,
        description = Desc,
        target = TargetType,
        wait = ?DEFAULT_STEP_WAIT_TIME
    }).

%% 创建发送步骤
-define(SEND_STEP(StepNum, Desc, TargetType, Content),
    #test_step{
        step_number = StepNum,
        action_type = <<"send">>,
        description = Desc,
        target = TargetType,
        send = #test_send_config{content = Content},
        wait = ?DEFAULT_STEP_WAIT_TIME
    }).

%% 创建接收步骤
-define(RECEIVE_STEP(StepNum, Desc, TargetType, ExpectedContent),
    #test_step{
        step_number = StepNum,
        action_type = <<"receive">>,
        description = Desc,
        target = TargetType,
        recv_config = #test_receive_config{recv_content = ExpectedContent},
        wait = ?DEFAULT_STEP_WAIT_TIME
    }).

%% 创建判定步骤
-define(JUDGE_STEP(StepNum, Desc, TargetType, ExpectedResult),
    #test_step{
        step_number = StepNum,
        action_type = <<"judge">>,
        description = Desc,
        target = TargetType,
        expected_result = ExpectedResult,
        wait = ?DEFAULT_STEP_WAIT_TIME
    }).

%% 检查测试项是否有效
-define(IS_VALID_TEST_ITEM(TestItem),
    is_record(TestItem, test_item) andalso
    TestItem#test_item.object_id =/= undefined andalso
    TestItem#test_item.devaddr =/= undefined andalso
    TestItem#test_item.name =/= undefined andalso
    TestItem#test_item.product_id =/= undefined).

%% 检查测试步骤是否有效
-define(IS_VALID_TEST_STEP(TestStep),
    is_record(TestStep, test_step) andalso
    TestStep#test_step.step_number > 0 andalso
    TestStep#test_step.action_type =/= undefined andalso
    TestStep#test_step.description =/= undefined andalso
    TestStep#test_step.target =/= undefined).

%% 获取工位ID
-define(GET_STATION_ID(StationName),
    maps:get(StationName, ?STATION_NAME_MAP, 0)).

%% 获取目标类型
-define(GET_TARGET_TYPE(Target),
    maps:get(Target, ?TARGET_TYPE_MAP, Target)).

%% 检查是否为有效的动作类型
-define(IS_VALID_ACTION_TYPE(ActionType),
    ActionType =:= <<"send">> orelse
    ActionType =:= <<"receive">> orelse
    ActionType =:= <<"judge">> orelse
    ActionType =:= <<"request_response">> orelse
    ActionType =:= <<"operate">>).

%% 检查是否为有效的目标类型
-define(IS_VALID_TARGET_TYPE(Target),
    Target =:= <<"1">> orelse
    Target =:= <<"2">> orelse
    Target =:= <<"3">> orelse
    Target =:= <<"工位PLC"/utf8>> orelse
    Target =:= <<"治具"/utf8>> orelse
    Target =:= <<"无人机"/utf8>>).

%% 检查是否为有效的测试项状态
-define(IS_VALID_TEST_ITEM_STATUS(Status),
    Status =:= <<"active">> orelse
    Status =:= <<"inactive">> orelse
    Status =:= <<"draft">>).

%% 检查是否为有效的执行状态
-define(IS_VALID_EXECUTION_STATUS(Status),
    Status =:= <<"pending">> orelse
    Status =:= <<"running">> orelse
    Status =:= <<"completed">> orelse
    Status =:= <<"failed">> orelse
    Status =:= <<"cancelled">>).

%% 检查是否为有效的结果
-define(IS_VALID_TEST_RESULT(Result),
    Result =:= <<"pass">> orelse
    Result =:= <<"fail">> orelse
    Result =:= <<"skip">> orelse
    Result =:= <<"in_progress">>).

%% 创建测试项键（用于缓存）
-define(TEST_ITEM_KEY(StationId, TestItemId),
    list_to_binary(io_lib:format("test_item:~p:~s", [StationId, TestItemId]))).

%% 创建工位测试项列表键
-define(STATION_TEST_ITEMS_KEY(StationId),
    list_to_binary(io_lib:format("station_test_items:~p", [StationId]))).

%% 创建测试执行上下文键
-define(TEST_EXECUTION_KEY(DeviceId, TestItemId),
    list_to_binary(io_lib:format("test_execution:~s:~s", [DeviceId, TestItemId]))).

-endif. % DGIOT_UAV_TEST_ITEM_HRL