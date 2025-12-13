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

%% LOG宏双参数版本
-define(LOG(Level, Format, Args),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> { (Format), (Args)} end,
            domain => [dgiot_public],
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
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