%% @doc 同名承接：emqx_listeners（监听器）→ 我们的监听器。
%% 开发期单 TCP 监听（1884）；format/identifier/find 映射到监听器信息，
%% stop/restart 转监听器进程控制。不支持的监听器类型显式报错。
-module(emqx_listeners).

-export([format_listen_on/1, identifier/1, find_by_listen_on/1,
         find_id_by_listen_on/1, find_by_id/1, stop_listener/1,
         restart_listener/1, current/0]).

current() ->
    case whereis(dgiot_broker_listener) of
        undefined -> #{};
        _ -> dgiot_broker_listener:info()
    end.

format_listen_on(#{port := Port}) -> io_lib:format(":~p", [Port]);
format_listen_on(#{<<"port">> := Port}) -> io_lib:format(":~p", [Port]);
format_listen_on(Port) when is_integer(Port) -> io_lib:format(":~p", [Port]);
format_listen_on(Other) -> io_lib:format("~p", [Other]).

identifier(tcp) -> "tcp:1884";
identifier(#{port := Port}) -> io_lib:format("tcp:~p", [Port]);
identifier(Other) -> io_lib:format("~p", [Other]).

find_by_listen_on(_ListenOn) ->
    case whereis(dgiot_broker_listener) of
        undefined -> [];
        Pid -> [Pid]
    end.

find_id_by_listen_on(ListenOn) ->
    case whereis(dgiot_broker_listener) of
        undefined -> undefined;
        _ -> identifier(ListenOn)
    end.

find_by_id(_Id) ->
    case whereis(dgiot_broker_listener) of
        undefined -> undefined;
        Pid -> Pid
    end.

stop_listener(tcp) -> dgiot_broker_listener:stop();
stop_listener(_Id) -> {error, {unsupported_listener, only_tcp}}.

restart_listener(tcp) ->
    case dgiot_broker_listener:stop() of
        ok -> dgiot_broker_listener:start_link();
        {error, _} -> {error, not_running}
    end;
restart_listener(_Id) -> {error, {unsupported_listener, only_tcp}}.
