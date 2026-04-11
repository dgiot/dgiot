%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_log_control - 通用日志控制器
%%%
%%% 基于进程字典的日志控制，支持多维度控制：
%%% 1. 系统日志级别：优先级最高，遵循系统配置
%%% 2. 全局开关：控制所有日志
%%% 3. 模块开关：控制特定模块的日志
%%% 4. 行号开关：控制特定模块特定行的日志
%%% 5. 级别开关：控制特定级别的日志（debug, info, warning, error等）
%%% 6. 频率控制：限制日志打印频率（秒）
%%% 7. 次数控制：限制日志打印次数
%%%
%%% 使用方式（三种方法，都在调用进程内操作）：
%%%
%%% 方法1：进程初始化时设置（推荐）
%%%   init([]) ->
%%%       dgiot_log_control:disable_level(debug),
%%%       dgiot_log_control:set_interval(?MODULE, 60),
%%%       {ok, #state{}}.
%%%
%%% 方法2：直接操作进程字典（轻量级）
%%%   put(dgiot_log_disabled, true),
%%%   put({dgiot_log_interval, ?MODULE}, 30),
%%%
%%% 方法3：通过进程消息控制（动态调整）
%%%   handle_info({log_control, disable_module, Module}, State) ->
%%%       dgiot_log_control:disable_module(Module),
%%%       {noreply, State}.
%%%
%%% API函数：
%%%   dgiot_log_control:disable_all()              %% 禁用所有日志
%%%   dgiot_log_control:disable_module(?MODULE)    %% 禁用模块日志
%%%   dgiot_log_control:disable_line(?MODULE, 123) %% 禁用行号日志
%%%   dgiot_log_control:disable_level(debug)       %% 禁用debug级别日志
%%%   dgiot_log_control:set_interval(?MODULE, 30)  %% 设置模块频率(秒)
%%%   dgiot_log_control:set_limit(?MODULE, 10)     %% 设置模块次数限制
%%%
%%% 控制优先级：
%%%   系统日志级别 -> 全局开关 -> 模块开关 -> 行号开关 -> 级别开关 -> 频率限制 -> 次数限制
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_log_control).
-author("johnliu").

%% 避免get_keys/0与erlang:get_keys/0冲突
-compile({no_auto_import,[get_keys/0]}).

%% API
-export([
    %% 日志检查
    should_log/3,
    
    %% ========== 新策略控制API（基于进程字典）==========
    %% 策略设置
    set_strategy/2,          %% set_strategy(Type, DefaultCount)
    add_balance/1,           %% add_balance(Count)
    clear_strategy/0,        %% clear_strategy()
    
    %% 策略查询
    get_balance/0,           %% get_balance()
    get_strategy_config/0,   %% get_strategy_config()
    
    %% 开关控制
    enable_all/0,
    disable_all/0,
    enable_module/1,
    disable_module/1,
    enable_line/2,
    disable_line/2,
    enable_level/1,
    disable_level/1,
    
    %% 频率控制
    set_interval/2,
    set_interval/3,
    
    %% 次数控制
    set_limit/2,
    set_limit/3,
    set_limit/4,
    
    %% 存储和统计控制
    set_storage/2,
    set_storage/3,
    set_statistics/2,
    set_statistics/3,
    
    %% 查询接口
    is_enabled/0,
    is_module_enabled/1,
    is_line_enabled/2,
    is_level_enabled/1,
    
    %% 便捷函数：限制特定位置的日志打印次数
    limit_line_log/3,
    limit_module_log/2,
    
    %% 默认配置
    init_default_limits/0,
    set_default_log_limit/1,
    get_default_log_limit/0,
    
    %% 日志标签管理（通用，5个自定义tag槽位）
    set_log_tag/2,
    get_log_tag/1,
    get_log_tags/0,
    format_log_tags/0,
    clear_log_tag/1,
    clear_log_tags/0,
    
    %% 兼容旧接口
    set_ip_port/2,
    get_ip_port/0,
    clear_ip_port/0
]).

%% 进程字典Key定义
-define(LOG_DISABLED, dgiot_log_disabled).
-define(MODULE_DISABLED(Module), {dgiot_log_module_disabled, Module}).
-define(LINE_DISABLED(Module, Line), {dgiot_log_line_disabled, Module, Line}).
-define(LEVEL_DISABLED(Level), {dgiot_log_level_disabled, Level}).
-define(LOG_INTERVAL(Key), {dgiot_log_interval, Key}).
-define(LOG_LAST_TIME(Key), {dgiot_log_last_time, Key}).
-define(LOG_LIMIT(Key), {dgiot_log_limit, Key}).
-define(LOG_COUNTER(Key), {dgiot_log_counter, Key}).
-define(LOG_STORAGE(Key), {dgiot_log_storage, Key}).
-define(LOG_STATISTICS(Key), {dgiot_log_statistics, Key}).

%% 日志标签Key定义（通用，5个自定义tag槽位）
%% 用户可以随意定义tag的含义，例如：
%%   tag1 = IP地址
%%   tag2 = 端口号
%%   tag3 = 设备ID
%%   tag4 = 用户ID
%%   tag5 = 会话ID
-define(LOG_TAG(N), {dgiot_log_tag, N}).

%%%===================================================================
%% 新策略：基于进程字典的日志控制（推荐）
%%%===================================================================
%%
%% 使用方式：
%% 1. 进程初始化时设置策略：
%%    init([]) ->
%%        dgiot_log_control:set_strategy(pid_mod_line, 20),  %% 每行默认20次
%%        {ok, #state{}}.
%%
%% 2. 业务代码正常使用LOG宏：
%%    handle_info({tcp, _Socket, Data}, State) ->
%%        ?LOG(error, "接收数据: ~p", [Data]),  %% 自动应用策略
%%        {noreply, State}.
%%
%% 策略类型：
%% - pid_mod_line : 每个 {Pid, Module, Line} 独立计数（最细粒度）
%% - pid_mod      : 每个 {Pid, Module} 独立计数
%% - mod_line     : 每个 {Module, Line} 独立计数
%% - pid          : 每个进程独立计数
%% - mod          : 每个模块独立计数
%%
%% 进程字典Key定义：
%% - {log_strategy_type, Type} => {Type, DefaultCount}  %% 策略配置
%% - {log_counter, Type, Key}  => CurrentCount          %% 计数器

-define(LOG_STRATEGY_TYPE(Type), {log_strategy_type, Type}).
-define(LOG_COUNTER(Type, Key), {log_counter, Type, Key}).

%%%===================================================================
%% 日志检查（核心函数）
%%%===================================================================

%% @doc 检查是否应该打印日志（尾递归优化）
%% 返回值：boolean
%%
%% 检查优先级（从高到低）：
%% 1. 系统日志级别
%% 2. 新策略控制（基于进程字典）
%% 3. 原有控制逻辑（开关/频率/次数）
-spec should_log(atom(), integer(), atom()) -> boolean().
should_log(Module, Line, Level) ->
    %% 0. 检查系统日志级别（优先级最高）
    case check_system_level(Level) of
        false -> false;
        true ->
            %% 1. 检查新策略控制
            case check_strategy_control(Module, Line) of
                false -> false;
                true ->
                    %% 2. 原有控制逻辑
                    Checks = [
                        fun() -> check_global_switch() end,
                        fun() -> check_module_switch(Module) end,
                        fun() -> check_line_switch(Module, Line) end,
                        fun() -> check_level_switch(Level) end,
                        fun() -> check_module_count_limit(Module) end,
                        fun() -> check_line_count_limit(Module, Line) end,
                        fun() -> check_level_count_limit(Level) end,
                        fun() -> check_module_interval(Module) end,
                        fun() -> check_line_interval(Module, Line) end,
                        fun() -> check_level_interval(Level) end
                    ],
                    run_checks(Checks)
            end
    end.

%% @doc 检查新策略控制
%% 如果进程设置了策略，则按策略检查；否则允许打印
-spec check_strategy_control(atom(), integer()) -> boolean().
check_strategy_control(Module, Line) ->
    %% 检查是否有策略配置（按优先级顺序）
    case get_strategy_config() of
        undefined -> 
            %% 没有设置策略，允许打印
            true;
        {StrategyType, DefaultCount} ->
            %% 有策略，执行策略检查
            do_check_strategy(StrategyType, DefaultCount, Module, Line)
    end.

%% @doc 获取当前进程的策略配置
%% 返回：{StrategyType, DefaultCount} | undefined
-spec get_strategy_config() -> {atom(), non_neg_integer()} | undefined.
get_strategy_config() ->
    %% 按优先级查找策略配置
    case get(?LOG_STRATEGY_TYPE(pid_mod_line)) of
        {_, _} = Config -> Config;
        undefined ->
            case get(?LOG_STRATEGY_TYPE(pid_mod)) of
                {_, _} = Config -> Config;
                undefined ->
                    case get(?LOG_STRATEGY_TYPE(mod_line)) of
                        {_, _} = Config -> Config;
                        undefined ->
                            case get(?LOG_STRATEGY_TYPE(pid)) of
                                {_, _} = Config -> Config;
                                undefined ->
                                    get(?LOG_STRATEGY_TYPE(mod))
                            end
                    end
            end
    end.

%% @doc 执行策略检查
%% 根据策略类型构造Key，检查并更新计数器
-spec do_check_strategy(atom(), non_neg_integer(), atom(), integer()) -> boolean().
do_check_strategy(StrategyType, DefaultCount, Module, Line) ->
    %% 构造策略Key
    Key = make_strategy_key(StrategyType, Module, Line),
    CounterKey = ?LOG_COUNTER(StrategyType, Key),
    
    %% 获取当前计数
    case get(CounterKey) of
        undefined ->
            %% 首次访问，初始化计数器
            put(CounterKey, DefaultCount - 1),
            true;
        0 ->
            %% 余额为0，禁止打印
            false;
        N when N > 0 ->
            %% 有余额，减1后允许打印
            put(CounterKey, N - 1),
            true
    end.

%% @doc 根据策略类型构造Key
-spec make_strategy_key(atom(), atom(), integer()) -> term().
make_strategy_key(pid_mod_line, Module, Line) ->
    {self(), Module, Line};
make_strategy_key(pid_mod, Module, _Line) ->
    {self(), Module};
make_strategy_key(mod_line, Module, Line) ->
    {Module, Line};
make_strategy_key(pid, _Module, _Line) ->
    self();
make_strategy_key(mod, Module, _Line) ->
    Module.

%% @doc 设置日志策略（进程初始化时调用）
%% Type: 策略类型 (pid_mod_line | pid_mod | mod_line | pid | mod)
%% DefaultCount: 默认打印次数
%%
%% 使用示例：
%%   init([]) ->
%%       dgiot_log_control:set_strategy(pid_mod_line, 20),
%%       {ok, #state{}}.
-spec set_strategy(atom(), non_neg_integer()) -> ok.
set_strategy(Type, DefaultCount) when is_atom(Type), is_integer(DefaultCount), DefaultCount >= 0 ->
    put(?LOG_STRATEGY_TYPE(Type), {Type, DefaultCount}),
    ok.

%% @doc 增加打印余额（动态调整）
%% 在当前位置增加指定次数的打印余额
%%
%% 使用示例：
%%   handle_info(reset_log, State) ->
%%       dgiot_log_control:add_balance(10),  %% 再增加10次
%%       {noreply, State}.
-spec add_balance(non_neg_integer()) -> ok.
add_balance(Count) when is_integer(Count), Count >= 0 ->
    case get_strategy_config() of
        {StrategyType, _DefaultCount} ->
            Key = make_strategy_key(StrategyType, ?MODULE, ?LINE),
            CounterKey = ?LOG_COUNTER(StrategyType, Key),
            CurrentCount = case get(CounterKey) of
                undefined -> 0;
                C -> C
            end,
            put(CounterKey, CurrentCount + Count),
            ok;
        undefined ->
            ok
    end.

%% @doc 查询当前余额
%% 返回当前位置的剩余打印次数
%%
%% 使用示例：
%%   Balance = dgiot_log_control:get_balance(),
%%   ?LOG(info, "剩余打印次数: ~p", [Balance]).
-spec get_balance() -> non_neg_integer() | unlimited.
get_balance() ->
    case get_strategy_config() of
        {StrategyType, _DefaultCount} ->
            Key = make_strategy_key(StrategyType, ?MODULE, ?LINE),
            CounterKey = ?LOG_COUNTER(StrategyType, Key),
            case get(CounterKey) of
                undefined -> unlimited;
                Count -> Count
            end;
        undefined ->
            unlimited
    end.

%% @doc 清除当前进程的所有策略和计数器
-spec clear_strategy() -> ok.
clear_strategy() ->
    %% 清除所有策略类型配置
    [erase(?LOG_STRATEGY_TYPE(Type)) || Type <- [pid_mod_line, pid_mod, mod_line, pid, mod]],
    %% 清除所有计数器（匹配log_counter前缀）
    [erase(Key) || Key <- get_keys(), is_tuple(Key), element(1, Key) =:= log_counter],
    ok.

%% 辅助函数：获取进程字典所有Key
get_keys() ->
    [Key || {Key, _} <- get()].



%% 检查系统日志级别（优先级最高）
check_system_level(Level) ->
    %% 获取全局日志级别（从logger配置）
    SystemLevel = case logger:get_primary_config() of
        #{level := GlobalLevel} -> GlobalLevel;
        _ -> debug  %% 默认允许所有级别
    end,
    
    %% 比较日志级别：Level优先级 >= SystemLevel优先级 才打印
    level_priority(Level) >= level_priority(SystemLevel).

%% 日志级别优先级
level_priority(debug) -> 1;
level_priority(info) -> 2;
level_priority(notice) -> 3;
level_priority(warning) -> 4;
level_priority(error) -> 5;
level_priority(critical) -> 6;
level_priority(alert) -> 7;
level_priority(emergency) -> 8;
level_priority(_) -> 0.

%% 尾递归执行检查列表
run_checks([]) -> true;
run_checks([Check | Rest]) ->
    case Check() of
        true -> run_checks(Rest);
        false -> false
    end.

%% 各项检查函数（简洁版）
check_global_switch() -> get(?LOG_DISABLED) =/= true.
check_module_switch(Module) -> get(?MODULE_DISABLED(Module)) =/= true.
check_line_switch(Module, Line) -> get(?LINE_DISABLED(Module, Line)) =/= true.
check_level_switch(Level) -> get(?LEVEL_DISABLED(Level)) =/= true.
check_module_count_limit(Module) -> check_count_limit(Module).
check_line_count_limit(Module, Line) -> check_count_limit({Module, Line}).
check_level_count_limit(Level) -> check_count_limit({level, Level}).
check_module_interval(Module) -> check_interval(Module).
check_line_interval(Module, Line) -> check_interval({Module, Line}).
check_level_interval(Level) -> check_interval({level, Level}).

%% 检查次数限制（支持全局默认限制）
check_count_limit(Key) ->
    %% 优先级：精细化限制 > 全局默认限制 > 无限制
    case get(?LOG_LIMIT(Key)) of
        MaxCount when is_integer(MaxCount) ->
            %% 1. 有精细化限制，使用精细化限制
            check_and_increment_counter(Key, MaxCount);
        undefined ->
            %% 2. 没有精细化限制，检查全局默认限制
            case get(dgiot_default_log_limit) of
                DefaultMax when is_integer(DefaultMax) ->
                    %% 使用全局默认限制
                    check_and_increment_counter(Key, DefaultMax);
                undefined ->
                    %% 3. 无限制
                    true
            end
    end.

%% 检查并递增计数器（抽取公共逻辑）
check_and_increment_counter(Key, MaxCount) ->
    CurrentCount = case get(?LOG_COUNTER(Key)) of
        undefined -> 0;
        C -> C
    end,
    if
        CurrentCount >= MaxCount -> false;  % 达到限制
        true ->
            put(?LOG_COUNTER(Key), CurrentCount + 1),
            true
    end.

%% 检查频率限制
check_interval(Key) ->
    case get(?LOG_INTERVAL(Key)) of
        undefined -> true;  % 无频率限制
        IntervalSec ->
            Now = erlang:system_time(second),
            case get(?LOG_LAST_TIME(Key)) of
                undefined ->
                    put(?LOG_LAST_TIME(Key), Now),
                    true;
                LastTime when is_integer(LastTime) ->
                    if
                        Now - LastTime >= IntervalSec ->
                            put(?LOG_LAST_TIME(Key), Now),
                            true;
                        true -> false
                    end
            end
    end.

%%%===================================================================
%% 开关控制
%%%===================================================================

%% @doc 启用所有日志
-spec enable_all() -> ok.
enable_all() ->
    put(?LOG_DISABLED, false),
    ok.

%% @doc 禁用所有日志
-spec disable_all() -> ok.
disable_all() ->
    put(?LOG_DISABLED, true),
    ok.

%% @doc 启用特定模块日志
-spec enable_module(atom()) -> ok.
enable_module(Module) ->
    put(?MODULE_DISABLED(Module), false),
    ok.

%% @doc 禁用特定模块日志
-spec disable_module(atom()) -> ok.
disable_module(Module) ->
    put(?MODULE_DISABLED(Module), true),
    ok.

%% @doc 启用特定行日志
-spec enable_line(atom(), integer()) -> ok.
enable_line(Module, Line) ->
    put(?LINE_DISABLED(Module, Line), false),
    ok.

%% @doc 禁用特定行日志
-spec disable_line(atom(), integer()) -> ok.
disable_line(Module, Line) ->
    put(?LINE_DISABLED(Module, Line), true),
    ok.

%% @doc 启用特定级别日志
-spec enable_level(atom()) -> ok.
enable_level(Level) ->
    put(?LEVEL_DISABLED(Level), false),
    ok.

%% @doc 禁用特定级别日志
-spec disable_level(atom()) -> ok.
disable_level(Level) ->
    put(?LEVEL_DISABLED(Level), true),
    ok.

%%%===================================================================
%% 频率控制
%%%===================================================================

%% @doc 设置模块日志频率（秒）
-spec set_interval(atom(), non_neg_integer()) -> ok.
set_interval(Module, IntervalSec) ->
    put(?LOG_INTERVAL(Module), IntervalSec),
    ok.

%% @doc 设置特定行日志频率（秒）
-spec set_interval(atom(), integer(), non_neg_integer()) -> ok.
set_interval(Module, Line, IntervalSec) ->
    put(?LOG_INTERVAL({Module, Line}), IntervalSec),
    ok.

%%%===================================================================
%% 次数控制
%%%===================================================================

%% @doc 设置模块日志次数限制
-spec set_limit(atom(), non_neg_integer()) -> ok.
set_limit(Module, MaxCount) ->
    put(?LOG_LIMIT(Module), MaxCount),
    put(?LOG_COUNTER(Module), 0),
    ok.

%% @doc 设置特定行日志次数限制
-spec set_limit(atom(), integer(), non_neg_integer()) -> ok.
set_limit(Module, Line, MaxCount) ->
    put(?LOG_LIMIT({Module, Line}), MaxCount),
    put(?LOG_COUNTER({Module, Line}), 0),
    ok.

%% @doc 设置特定进程+模块+行的日志次数限制
%% 用于多进程环境下针对特定进程设置独立的日志配额
%%
%% 使用示例：
%%   dgiot_log_control:set_limit(self(), dgiot_uav_tcp_worker, 1007, 5).
%%   %% 当前进程的 dgiot_uav_tcp_worker 模块第1007行日志只打印5次
%%
-spec set_limit(pid(), atom(), integer(), non_neg_integer()) -> ok.
set_limit(Pid, Module, Line, MaxCount) when is_pid(Pid) ->
    Key = {Pid, Module, Line},
    put(?LOG_LIMIT(Key), MaxCount),
    put(?LOG_COUNTER(Key), 0),
    ok.

%%%===================================================================
%% 存储和统计控制
%%%===================================================================

%% @doc 设置模块日志存储控制
-spec set_storage(atom(), boolean()) -> ok.
set_storage(Module, Enable) ->
    put(?LOG_STORAGE(Module), Enable),
    ok.

%% @doc 设置特定行日志存储控制
-spec set_storage(atom(), integer(), boolean()) -> ok.
set_storage(Module, Line, Enable) ->
    put(?LOG_STORAGE({Module, Line}), Enable),
    ok.

%% @doc 设置模块日志统计控制
-spec set_statistics(atom(), boolean()) -> ok.
set_statistics(Module, Enable) ->
    put(?LOG_STATISTICS(Module), Enable),
    ok.

%% @doc 设置特定行日志统计控制
-spec set_statistics(atom(), integer(), boolean()) -> ok.
set_statistics(Module, Line, Enable) ->
    put(?LOG_STATISTICS({Module, Line}), Enable),
    ok.

%%%===================================================================
%% 查询接口
%%%===================================================================

%% @doc 检查全局日志是否启用
-spec is_enabled() -> boolean().
is_enabled() ->
    case get(?LOG_DISABLED) of
        true -> false;
        _ -> true
    end.

%% @doc 检查模块日志是否启用
-spec is_module_enabled(atom()) -> boolean().
is_module_enabled(Module) ->
    case get(?MODULE_DISABLED(Module)) of
        true -> false;
        _ -> true
    end.

%% @doc 检查特定行日志是否启用
-spec is_line_enabled(atom(), integer()) -> boolean().
is_line_enabled(Module, Line) ->
    case get(?LINE_DISABLED(Module, Line)) of
        true -> false;
        _ -> true
    end.

%% @doc 检查特定级别日志是否启用
-spec is_level_enabled(atom()) -> boolean().
is_level_enabled(Level) ->
    case get(?LEVEL_DISABLED(Level)) of
        true -> false;
        _ -> true
    end.

%%%===================================================================
%% 便捷函数：限制日志打印次数
%%%===================================================================

%% @doc 限制特定行的日志打印次数（常用快捷函数）
%% 使用示例：
%%   dgiot_log_control:limit_line_log(dgiot_eb90_handler, 180, 5).
%%   %% 限制 dgiot_eb90_handler 模块第180行的日志只打印5次
%%
-spec limit_line_log(atom(), integer(), non_neg_integer()) -> ok.
limit_line_log(Module, Line, MaxCount) ->
    set_limit(Module, Line, MaxCount).

%% @doc 限制模块的日志打印次数（常用快捷函数）
%% 使用示例：
%%   dgiot_log_control:limit_module_log(dgiot_eb90_handler, 10).
%%   %% 限制 dgiot_eb90_handler 模块的所有日志总共只打印10次
%%
-spec limit_module_log(atom(), non_neg_integer()) -> ok.
limit_module_log(Module, MaxCount) ->
    set_limit(Module, MaxCount).

%% @doc 设置全局默认日志打印次数限制
%% 所有日志（除非单独设置）都受此限制
%% 
%% 使用示例：
%%   dgiot_log_control:set_default_log_limit(5).
%%   %% 所有日志默认只打印前5次
%%
-spec set_default_log_limit(non_neg_integer()) -> ok.
set_default_log_limit(MaxCount) ->
    put(dgiot_default_log_limit, MaxCount),
    ok.

%% @doc 获取全局默认日志打印次数限制
-spec get_default_log_limit() -> non_neg_integer() | undefined.
get_default_log_limit() ->
    get(dgiot_default_log_limit).

%% @doc 初始化默认日志限制配置
%% 在进程初始化时调用，设置全局默认限制
%%
%% 默认规则：
%% - 所有周期性报文日志默认只打印前5次
%% - 避免日志刷屏
%%
%% 【重要】此函数在进程初始化时调用，只影响当前进程
%% 已存在的TCP连接需要重启才能生效
%%
-spec init_default_limits() -> ok.
init_default_limits() ->
    %% 设置全局默认限制：所有日志默认打印前5次
    set_default_log_limit(5),
    
    %% 如果需要针对特定位置精细化控制，可以在这里添加：
    %% limit_line_log(dgiot_eb90_handler, 180, 10),  %% EB90帧提取打印10次
    %% limit_line_log(dgiot_eb90_handler, 180, 3),   %% 或更少次数
    
    ok.

%%%===================================================================
%% 日志标签管理（通用）
%%%===================================================================

%% @doc 设置日志标签（通用，5个槽位：tag1-tag5）
%% 用户可以随意定义tag的含义，例如：
%%   tag1 = IP地址
%%   tag2 = 端口号
%%   tag3 = 设备ID
%%   tag4 = 用户ID
%%   tag5 = 会话ID
%%
%% 使用示例：
%%   dgiot_log_control:set_log_tag(tag1, <<"192.168.100.52">>),
%%   dgiot_log_control:set_log_tag(tag2, 10006),
%%   dgiot_log_control:set_log_tag(tag3, <<"Device-001">>),
%%   ?LOG(error, "测试日志")  %% 输出：[192.168.100.52:10006][Device-001] 测试日志
%%
-spec set_log_tag(atom(), term()) -> ok.
set_log_tag(TagName, Value) when TagName =:= tag0; TagName =:= tag1; TagName =:= tag2;
                                  TagName =:= tag3; TagName =:= tag4;
                                  TagName =:= tag5 ->
    put(?LOG_TAG(TagName), Value),
    ok.

%% @doc 获取日志标签
-spec get_log_tag(atom()) -> term() | undefined.
get_log_tag(TagName) when TagName =:= tag0; TagName =:= tag1; TagName =:= tag2;
                          TagName =:= tag3; TagName =:= tag4;
                          TagName =:= tag5 ->
    get(?LOG_TAG(TagName)).

%% @doc 获取所有日志标签（按顺序）
%% 返回：[{tag0, Value0}, {tag1, Value1}, ...]
-spec get_log_tags() -> [{atom(), term()}].
get_log_tags() ->
    Tags = [tag0, tag1, tag2, tag3, tag4, tag5],
    [{Tag, Value} || Tag <- Tags, (Value = get(?LOG_TAG(Tag))) =/= undefined].

%% @doc 格式化日志标签为字符串（用于LOG宏）
%% 统一格式：[通道类型:IP:端口] 或 [通道类型] 或 [tag4:value] [tag5:value]
%% tag0 = 工位编号（不再显示在日志前缀中）
%% tag1 = 通道类型（TCP/PLC/UDP等）
%% tag2 = IP地址
%% tag3 = 端口号
%% @doc 格式化日志标签
%% 标准格式: [工位][IP:Port] msg
%%   Station = tag0 (工位编号，如 1700, Unknown)
%%   IP      = tag2 (IP地址)
%%   Port    = tag3 (端口号)
%%
%% 连接进程三大标配：工位、IP、Port
-spec format_log_tags() -> string().
format_log_tags() ->
    TagList = get_log_tags(),
    case TagList of
        [] ->
            "";
        _ ->
            %% 提取标签（连接进程三大标配）
            StationValue = get(?LOG_TAG(tag0)), %% 工位编号
            IpValue = get(?LOG_TAG(tag2)),      %% IP地址
            PortValue = get(?LOG_TAG(tag3)),    %% 端口号

            %% 辅助函数：将binary转换为字符串（正确处理UTF-8）
            ToStr = fun(V) when is_binary(V) -> 
                           case unicode:characters_to_list(V) of
                               {error, _, _} -> binary_to_list(V);
                               {incomplete, _, _} -> binary_to_list(V);
                               List -> List
                           end;
                       (V) when is_list(V) -> V;
                       (V) -> io_lib:format("~p", [V])
                    end,

            %% 1. 工位标签（最前面）
            StationTag = case StationValue of
                undefined -> "[Unknown]";
                S -> io_lib:format("[~ts]", [ToStr(S)])
            end,

            %% 2. IP:Port 标签（连接进程标配）
            IpPortTag = case {IpValue, PortValue} of
                {undefined, undefined} -> "";
                {undefined, Port} -> io_lib:format("[:~p]", [Port]);
                {Ip, undefined} -> io_lib:format("[~ts]", [ToStr(Ip)]);
                {Ip, Port} -> io_lib:format("[~ts:~p]", [ToStr(Ip), Port])
            end,

            %% 3. 组合标签字符串
            Result = case IpPortTag of
                "" -> StationTag ++ " ";
                _ -> StationTag ++ IpPortTag ++ " "
            end,
            lists:flatten(Result)
    end.

%% @doc 清除特定日志标签
-spec clear_log_tag(atom()) -> ok.
clear_log_tag(TagName) when TagName =:= tag0; TagName =:= tag1; TagName =:= tag2;
                            TagName =:= tag3; TagName =:= tag4;
                            TagName =:= tag5 ->
    erase(?LOG_TAG(TagName)),
    ok.

%% @doc 清除所有日志标签
-spec clear_log_tags() -> ok.
clear_log_tags() ->
    [erase(?LOG_TAG(Tag)) || Tag <- [tag0, tag1, tag2, tag3, tag4, tag5]],
    ok.

%%%===================================================================
%% 兼容旧接口（IP端口管理）
%%%===================================================================

%% @doc 设置IP和端口（兼容旧接口，使用tag1和tag2）
-spec set_ip_port(binary() | string(), integer()) -> ok.
set_ip_port(Ip, Port) when is_binary(Ip); is_list(Ip) ->
    IpBin = if is_list(Ip) -> list_to_binary(Ip); true -> Ip end,
    set_log_tag(tag1, IpBin),
    set_log_tag(tag2, Port),
    ok.

%% @doc 获取IP和端口（兼容旧接口）
-spec get_ip_port() -> {binary(), integer()} | undefined.
get_ip_port() ->
    case {get_log_tag(tag1), get_log_tag(tag2)} of
        {Ip, Port} when is_binary(Ip), is_integer(Port) ->
            {Ip, Port};
        _ ->
            undefined
    end.

%% @doc 清除IP和端口（兼容旧接口）
-spec clear_ip_port() -> ok.
clear_ip_port() ->
    clear_log_tag(tag1),
    clear_log_tag(tag2),
    ok.


