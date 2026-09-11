%% @doc 会话登记（刀 3）：ClientId → 连接进程与状态。
%%
%% 刀 3 为内存态（ETS）；持久会话的离线队列/重投属刀 4，能力矩阵里如实标注。
%% 同 ClientId 重连 = 会话接管（我们探针在 EMQX 上实测过的行为）：旧进程被告知
%% 并关闭，事件显式返回给调用方，不静默顶掉。
-module(dgiot_broker_session).

-export([init/0, reset/0,
         register/3, unregister/1, lookup/1,
         list/0, count/0, touch/1, update/2]).

-define(TAB, dgiot_broker_sessions).

init() ->
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, set, public,
                                   {read_concurrency, true}]);
        _ -> ?TAB
    end,
    ok.

reset() ->
    ets:delete_all_objects(?TAB),
    ok.

%% @doc 登记会话。返回 {ok, New} 或 {takeover, OldPid}（由连接进程决定如何收尾）。
-spec register(binary(), pid(), map()) -> {ok, map()} | {takeover, pid()}.
register(ClientId, Pid, Opts) ->
    Now = erlang:system_time(second),
    New = #{pid => Pid,
            username => maps:get(username, Opts, undefined),
            clean_start => maps:get(clean_start, Opts, true),
            keepalive => maps:get(keepalive, Opts, 60),
            connected_at => Now,
            last_seen => Now,
            subscriptions => []},
    case ets:lookup(?TAB, ClientId) of
        [{ClientId, #{pid := OldPid}}] when OldPid =/= Pid ->
            ets:insert(?TAB, {ClientId, New}),
            {takeover, OldPid};
        _ ->
            ets:insert(?TAB, {ClientId, New}),
            {ok, New}
    end.

unregister(ClientId) ->
    ets:delete(?TAB, ClientId),
    ok.

lookup(ClientId) ->
    case ets:lookup(?TAB, ClientId) of
        [{ClientId, S}] -> {ok, S};
        [] -> {error, not_found}
    end.

touch(ClientId) ->
    case ets:lookup(?TAB, ClientId) of
        [{ClientId, S}] ->
            ets:insert(?TAB, {ClientId, S#{last_seen => erlang:system_time(second)}}),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc 局部更新会话（保留 connected_at/username 等字段，避免用 register 覆盖）
-spec update(binary(), fun((map()) -> map())) -> ok | {error, not_found}.
update(ClientId, Fun) when is_function(Fun, 1) ->
    case ets:lookup(?TAB, ClientId) of
        [{ClientId, S}] ->
            ets:insert(?TAB, {ClientId, Fun(S)}),
            ok;
        [] ->
            {error, not_found}
    end.

list() ->
    [{C, S} || {C, S} <- ets:tab2list(?TAB)].

count() ->
    ets:info(?TAB, size).
