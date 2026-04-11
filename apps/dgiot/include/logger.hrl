%%%-------------------------------------------------------------------
%%% @doc 日志宏定义 - 原始版本（保持原样）
%%%-------------------------------------------------------------------

-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, true).

%% ====================================================================
%% 基础日志宏
%% ====================================================================

%% DEBUG宏
-define(DEBUG(Format), ?LOG(debug, Format, [])).
-define(DEBUG(Format, Args), ?LOG(debug, Format, Args)).

%% INFO宏  
-define(INFO(Format), ?LOG(info, Format, [])).
-define(INFO(Format, Args), ?LOG(info, Format, Args)).

%% NOTICE宏
-define(NOTICE(Format), ?LOG(notice, Format, [])).
-define(NOTICE(Format, Args), ?LOG(notice, Format, Args)).

%% WARN宏
-define(WARN(Format), ?LOG(warning, Format, [])).
-define(WARN(Format, Args), ?LOG(warning, Format, Args)).

%% ERROR宏
-define(ERROR(Format), ?LOG(error, Format, [])).
-define(ERROR(Format, Args), ?LOG(error, Format, Args)).

%% CRITICAL宏
-define(CRITICAL(Format), ?LOG(critical, Format, [])).
-define(CRITICAL(Format, Args), ?LOG(critical, Format, Args)).

%% ALERT宏
-define(ALERT(Format), ?LOG(alert, Format, [])).
-define(ALERT(Format, Args), ?LOG(alert, Format, Args)).

%% ====================================================================
%% 基础LOG宏定义 - 保持原样，不加文件位置
%% ====================================================================

%% LOG宏单参数版本
-define(LOG(Level, Format), ?LOG(Level, Format, [])).

%% LOG宏双参数版本 - 增强版（支持进程字典控制）
%% 
%% 基于进程字典的日志控制，支持多维度控制：
%% 1. 系统日志级别：优先级最高，遵循系统配置
%% 2. 全局开关：控制所有日志
%% 3. 模块开关：控制特定模块的日志
%% 4. 行号开关：控制特定模块特定行的日志
%% 5. 级别开关：控制特定级别的日志
%% 6. 频率控制：限制日志打印频率
%% 7. 次数控制：限制日志打印次数
%%
%% 重要说明：进程字典作用域
%% - 所有控制函数都在**调用进程**的进程字典中操作
%% - 每个进程独立控制，互不影响
%%
%% 使用方式（三种方法）：
%%
%% 方法1：进程初始化时设置（推荐）
%%   init([]) ->
%%       dgiot_log_control:disable_level(debug),
%%       dgiot_log_control:set_interval(?MODULE, 60),
%%       {ok, #state{}}.
%%
%% 方法2：直接操作进程字典（轻量级）
%%   put(dgiot_log_disabled, true),
%%   put({dgiot_log_interval, ?MODULE}, 30),
%%
%% 方法3：通过进程消息控制（动态调整）
%%   handle_info({log_control, disable_module, Module}, State) ->
%%       dgiot_log_control:disable_module(Module),
%%       {noreply, State}.
%%
%% API函数：
%%   dgiot_log_control:disable_all()              %% 禁用所有日志
%%   dgiot_log_control:disable_module(?MODULE)    %% 禁用模块日志
%%   dgiot_log_control:disable_line(?MODULE, 123) %% 禁用行号日志
%%   dgiot_log_control:disable_level(debug)       %% 禁用debug级别日志
%%   dgiot_log_control:set_interval(?MODULE, 30)  %% 设置模块频率(秒)
%%   dgiot_log_control:set_limit(?MODULE, 10)     %% 设置模块次数限制
%%
%% 控制优先级：
%%   系统日志级别 -> 全局开关 -> 模块开关 -> 行号开关 -> 级别开关 -> 频率限制 -> 次数限制
%%
-define(LOG(Level, Format, Args),
    begin
        %% 检查日志开关（多级控制：全局 -> 模块 -> 行号 -> 级别）
        case dgiot_log_control:should_log(?MODULE, ?LINE, Level) of
            true ->
                %% 打印日志（所有变量计算都在report_cb内部，避免作用域问题）
                logger:log(Level, #{}, #{
                    report_cb => fun(_) ->
                        %% 转换Format为字符串（避免二进制格式问题）
                        FormatStr = case is_binary(Format) of
                            true -> unicode:characters_to_list(Format);
                            false -> Format
                        end,
                        %% 获取标签前缀
                        TagPrefix = dgiot_log_control:format_log_tags(),
                        %% 返回格式化后的日志
                        {TagPrefix ++ FormatStr, (Args)}
                    end,
                    domain => [dgiot_public],
                    mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                    line => ?LINE});
            false ->
                ok
        end
    end).

%% LOG宏三参数版本（带ACL）
-define(LOG(Level, Format, Args, ACL),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> {(Format), (Args)} end,
            domain => ACL,
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

%% ====================================================================
%% 首次打印日志宏（LOG_ONCE）- 便捷函数
%% ====================================================================

%% LOG_ONCE宏 - 只打印首次（默认只打印1次）
%% 使用示例：
%%   ?LOG_ONCE(info, "无人机首次上线: ~p", [DeviceId]).
%%   ?LOG_ONCE(error, "EB90帧首次识别: ~p", [FrameInfo]).
%%
-define(LOG_ONCE(Level, Format, Args),
    begin
        %% 动态设置行号限制为1次
        dgiot_log_control:set_limit(?MODULE, ?LINE, 1),
        %% 调用标准LOG宏
        ?LOG(Level, Format, Args)
    end).

%% LOG_N次宏 - 只打印前N次
%% 使用示例：
%%   ?LOG_N(info, 5, "EB90遥测帧: ~p bytes", [Size]).
%%   ?LOG_N(error, 3, "PLC指令下发: ~p", [Command]).
%%
-define(LOG_N(Level, MaxCount, Format, Args),
    begin
        %% 动态设置行号限制为N次
        dgiot_log_control:set_limit(?MODULE, ?LINE, MaxCount),
        %% 调用标准LOG宏
        ?LOG(Level, Format, Args)
    end).

%% ====================================================================
%% 结构化日志宏
%% ====================================================================

%% MLOG宏
-define(MLOG(Level, Map),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> Map end,
            domain => [dgiot_public],
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

%% MLOG宏带ACL
-define(MLOG(Level, Map, ACL),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> Map end,
            domain => ACL,
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

%% ====================================================================
%% 解析日志宏
%% ====================================================================

-define(PLOG(Level, Map),
    begin
        (dgiot_parse_log:log(#{
            <<"pid">> => erlang:pid_to_list(self()),
            <<"time">> => dgiot_datetime:now_microsecs(),
            <<"node">> => node(),
            <<"type">> => <<"json">>,
            <<"level">> => Level,
            <<"msg">> => Map,
            <<"module">> => ?MODULE,
            <<"function">> => ?FUNCTION_NAME,
            <<"funtion_arity">> => ?FUNCTION_ARITY,
            <<"file">> => ?FILE,
            <<"line">> => ?LINE
        }))
    end).

%% ====================================================================
%% 调试宏
%% ====================================================================

-ifdef(DEBUG_ENABLED).
-define(IS_DEBUG_ENABLED, true).
-else.
-define(IS_DEBUG_ENABLED, false).
-endif.

-define(DPRINT(Format),
    case ?IS_DEBUG_ENABLED of
        true -> 
            FileName = filename:basename(?FILE),
            io:format("DEBUG [~s:~w] " ++ Format, [FileName, ?LINE]);
        false -> ok
    end).

-define(DPRINT(Format, Args),
    case ?IS_DEBUG_ENABLED of
        true -> 
            FileName = filename:basename(?FILE),
            io:format("DEBUG [~s:~w] " ++ Format, [FileName, ?LINE | Args]);
        false -> ok
    end).

-endif. % LOGGER_HRL