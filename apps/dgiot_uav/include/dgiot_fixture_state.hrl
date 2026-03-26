%%--------------------------------------------------------------------
%% @doc 治具状态记录定义
%%
%% 此头文件定义了治具单片机状态管理相关的记录结构
%%
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% 治具状态记录
%%%===================================================================

%% @doc 治具状态记录
-record(fixture_state, {
    station_addr :: integer(),           %% 工位地址
    power_relay :: on | off,             %% 大继电器状态
    drone_power :: on | off,             %% 无人机电源状态
    test_state :: not_started | testing | completed | failed,  %% 测试状态
    test_step :: integer(),               %% 当前测试步骤（1-10）
    test_results :: list(),              %% 测试结果列表
    start_time :: integer() | undefined,  %% 测试开始时间
    end_time :: integer() | undefined,    %% 测试结束时间
    comm_state :: online | offline,      %% 通讯状态
    comm_check_timer :: reference() | undefined  %% 通讯检测定时器
}).

%% @doc 测试结果记录
-record(test_result, {
    step :: integer(),                   %% 测试步骤
    test_name :: binary(),               %% 测试名称
    status :: passed | failed | untested | running, %% 测试状态
    value :: number() | undefined,       %% 测试值
    unit :: binary(),                    %% 单位
    error_reason :: binary() | undefined, %% 错误原因
    start_time :: integer(),             %% 开始时间
    end_time :: integer() | undefined    %% 结束时间
}).
