%% @doc 同名承接：emqx_logger（刀 6 第一批）。实测 22 次 / 11 组合。
%% 映射到 OTP logger：等级读写、处理器枚举、进程元数据（peername/clientid）。
%% 未支持的能力显式报错（如自定义日志处理器），不静默。
-module(emqx_logger).

-export([get_primary_log_level/0, set_primary_log_level/1,
         set_log_level/1, info/3,
         get_log_handlers/0, get_log_handler/1,
         start_log_handler/1, stop_log_handler/1, set_log_handler_level/2,
         set_metadata_peername/1, set_metadata_clientid/1,
         set_metadata/2]).

get_primary_log_level() ->
    case logger:get_primary_config() of
        #{level := Level} -> Level;
        _ -> info
    end.

set_primary_log_level(Level) when is_atom(Level) ->
    case logger:set_primary_config(level, Level) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% EMQX 的 set_log_level/1 语义是"设置默认处理器等级"
set_log_level(Level) when is_atom(Level) ->
    logger:set_handler_config(default, level, Level).

info(Format, Args, _Meta) -> logger:info(Format, Args).

get_log_handlers() ->
    [Id || {Id, _} <- logger:get_handler_config()].

get_log_handler(Id) ->
    case logger:get_handler_config(Id) of
        {ok, Cfg} -> Cfg;
        {error, Reason} -> {error, Reason}
    end.

%% 自定义处理器：仅支持 OTP logger 的处理器模块（EMQX 自有处理器不提供）
start_log_handler({Id, Mod, Cfg}) ->
    case logger:add_handler(Id, Mod, Cfg) of
        ok -> ok;
        {error, Reason} -> {error, {handler_start_failed, Reason}}
    end;
start_log_handler(Other) ->
    {error, {unsupported_log_handler, Other}}.

stop_log_handler(Id) ->
    case logger:remove_handler(Id) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

set_log_handler_level(Id, Level) ->
    logger:set_handler_config(Id, level, Level).

%% 进程元数据（EMQX 用它给日志带上连接信息）
set_metadata_peername(Peername) -> set_metadata(peername, Peername).
set_metadata_clientid(Clientid) -> set_metadata(clientid, Clientid).

set_metadata(_Key, undefined) -> ok;
set_metadata(Key, Value) ->
    logger:update_process_metadata(#{Key => Value}).
