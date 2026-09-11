%% @doc 同名承接头文件：logger.hrl（刀 5）。
%%
%% dgiot 侧只用到 `?LOG(Level, Format[, Args])`（实测 849 处），插件侧还会用
%% 等级快捷宏。这里把两者都映射到 OTP logger —— 没有 EMQX 的 logger 依赖。
%% EMQX 的等级名（debug/info/notice/warning/error/critical/alert）与 OTP
%% logger 完全一致，因此直接透传。
-ifndef(DGIOT_BROKER_LOGGER_HRL).
-define(DGIOT_BROKER_LOGGER_HRL, true).

-define(LOG(Level, Format), ?LOG(Level, Format, [])).
-define(LOG(Level, Format, Args), logger:log(Level, Format, Args)).

%% 等级快捷宏（插件常用）
-define(DEBUG(Format), ?LOG(debug, Format, [])).
-define(DEBUG(Format, Args), ?LOG(debug, Format, Args)).
-define(INFO(Format), ?LOG(info, Format, [])).
-define(INFO(Format, Args), ?LOG(info, Format, Args)).
-define(NOTICE(Format), ?LOG(notice, Format, [])).
-define(NOTICE(Format, Args), ?LOG(notice, Format, Args)).
-define(WARN(Format), ?LOG(warning, Format, [])).
-define(WARN(Format, Args), ?LOG(warning, Format, Args)).
-define(ERROR(Format), ?LOG(error, Format, [])).
-define(ERROR(Format, Args), ?LOG(error, Format, Args)).
-define(CRITICAL(Format), ?LOG(critical, Format, [])).
-define(CRITICAL(Format, Args), ?LOG(critical, Format, Args)).
-define(ALERT(Format), ?LOG(alert, Format, [])).
-define(ALERT(Format, Args), ?LOG(alert, Format, Args)).

%% EMQX 的敏感日志宏：本实现不做脱敏分流，显式等价到同一日志（并标注）
-define(LOG_SENSITIVE(Level, Format, Args), logger:log(Level, Format, Args)).

-endif.
