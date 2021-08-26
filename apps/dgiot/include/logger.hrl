-compile({parse_transform, emqx_logger}).

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

-define(LOG(Level, Format), ?LOG(Level, Format, [])).

-define(LOG(Level, Format, Args),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> {'$logger_header'() ++ (Format), (Args)} end,
            domain => [public_dgiot],
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

-define(LOG(Level, Format, Args, ACL),
    begin
        (logger:log(Level, #{}, #{
            report_cb => fun(_) -> {'$logger_header'() ++ (Format), (Args)} end,
            domain => ACL,
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

-define(MLOG(Level, Map),
    begin
        (logger:log(Level, Map, #{
            report_cb => fun(_) -> Map end,
            domain => [publiclog],
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).

-define(MLOG(Level, Map, ACL),
    begin
        (logger:log(Level, Map, #{
            report_cb => fun(_) -> Map end,
            domain => ACL,
            mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
            line => ?LINE}))
    end).
