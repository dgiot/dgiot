%% @doc 插件宿主骨架（刀 3）：让生态插件"即插即用"的那一半。
%%
%% 现状（诚实标注）：本模块提供**发现/启停/状态**三件事，宿主行为
%% （`emqx_plugins`/`emqx_plugin` behaviour 的同名接管）在切换刀完成——
%% 那时 12 个 emqx_* 插件应用不改一行直接由本宿主加载。
%%
%% 发现口径：读取发布清单（`data/loaded_plugins` 同款 {App, Enabled} 列表）
%% 或通过 application env {plugins, #{...}} 显式声明。
%% 铁律：加载失败必须响亮（记 error + 进 failed 列表），绝不静默跳过。
-module(dgiot_broker_plugins).

-export([discover/0, list/0, start/1, stop/1, host_info/0, loaded_plugins_file/0]).

-define(DEFAULT_FILE, "/data/dgiot/data/loaded_plugins").

%% @doc 发现的插件清单 [{Name, Enabled}]（来自发布清单或 env）
discover() ->
    case application:get_env(dgiot_broker, plugins) of
        {ok, #{list := L}} when is_list(L) ->
            [{to_atom(N), maps:get(enabled, E, true)} || {N, E} <- L];
        {ok, L} when is_list(L) ->
            [{to_atom(N), En} || {N, En} <- L];
        _ ->
            read_plugins_file()
    end.

read_plugins_file() ->
    File = loaded_plugins_file(),
    case file:consult(File) of
        {ok, Terms} ->
            [{to_atom(N), En} || {N, En} <- Terms];
        {error, enoent} ->
            logger:warning("[broker-plugins] plugin list not found: ~s", [File]),
            [];
        {error, Reason} ->
            logger:error("[broker-plugins] cannot parse ~s: ~p", [File, Reason]),
            []
    end.

loaded_plugins_file() ->
    application:get_env(dgiot_broker, plugins_file, ?DEFAULT_FILE).

%% @doc 插件清单 + 运行状态（可观测）
list() ->
    [{Name, Enabled, running(Name)} || {Name, Enabled} <- discover()].

host_info() ->
    All = list(),
    #{plugins => length(All),
      running => [N || {N, _, true} <- All],
      stopped => [N || {N, _, false} <- All],
      host_behaviour => #{emqx_plugin => not_yet_taken_over,
                          note => <<"切换刀用同名 emqx_plugins 承接插件宿主">>}}.

running(Name) ->
    lists:keymember(Name, 1, application:which_applications()).

%% @doc 启动插件（用 ensure_all_started：依赖也一并起，失败响亮）
start(Name) when is_atom(Name) ->
    case application:ensure_all_started(Name) of
        {ok, Started} ->
            logger:notice("[broker-plugins] started ~p (with ~p)", [Name, Started]),
            {ok, Started};
        {error, Reason} ->
            logger:error("[broker-plugins] start ~p failed: ~p", [Name, Reason]),
            {error, Reason}
    end.

stop(Name) when is_atom(Name) ->
    case application:stop(Name) of
        ok ->
            logger:notice("[broker-plugins] stopped ~p", [Name]),
            ok;
        {error, {not_started, Name}} ->
            {error, not_running};
        {error, Reason} ->
            logger:error("[broker-plugins] stop ~p failed: ~p", [Name, Reason]),
            {error, Reason}
    end.

to_atom(N) when is_atom(N) -> N;
to_atom(N) when is_binary(N) -> binary_to_atom(N, utf8);
to_atom(N) when is_list(N) -> list_to_atom(N).
