%% @doc 同名承接：emqx_plugins（插件宿主）→ 我们的 dgiot_broker_plugins。
%% 这是「插件即插即用」的宿主面：list/load/unload/reload/find 全部转自有实现。
%% 宿主行为（emqx_plugin behaviour 的同名接管）在切换刀随 apps/emqx 一起启用，
%% 这里先提供函数面，未就绪部分显式标注。
-module(emqx_plugins).

-export([list/0, load/1, unload/1, reload/1, find_plugin/1, ensure_loaded/1]).

list() ->
    [{Name, Enabled, running(Name)}
     || {Name, Enabled} <- dgiot_broker_plugins:discover()].

running(Name) ->
    lists:keymember(Name, 1, application:which_applications()).

load(Name) ->
    case dgiot_broker_plugins:start(to_atom(Name)) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

unload(Name) ->
    case dgiot_broker_plugins:stop(to_atom(Name)) of
        ok -> ok;
        {error, not_running} -> ok;
        {error, Reason} -> {error, Reason}
    end.

reload(Name) ->
    case unload(Name) of
        ok -> load(Name);
        {error, Reason} -> {error, Reason}
    end.

find_plugin(Name) ->
    case lists:keyfind(to_atom(Name), 1,
                       dgiot_broker_plugins:discover()) of
        {N, Enabled} -> #{name => N, enabled => Enabled};
        false -> false
    end.

ensure_loaded(Name) ->
    case load(Name) of
        ok -> {ok, true};
        {error, _} -> {ok, false}
    end.

to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
to_atom(L) when is_list(L) -> list_to_atom(L).
