%% @doc 同名承接：emqx_channel（连接通道）→ 我们的连接进程 + 会话登记。
%%
%% 数据面访问器（info/set_session/get_session/do_deliver/stats/keepalive）
%% 直接转我们的 conn/session；gen_statem 内部回调（init/handle_*）是 EMQX
%% 通道状态机的内部件，跨模块不调用，显式 not_implemented 占位。
-module(emqx_channel).

-export([info/1, info/2, set_session/2, get_session/1, do_deliver/2,
         stats/1, terminate/2, ensure_keepalive/2, clear_keepalive/1,
         init/2, handle_in/2, handle_info/2, handle_call/2,
         handle_deliver/2, handle_timeout/3, channel/0]).

%% 通道信息：若 ChanPid 是我们的连接进程，读其 peer_info；否则回会话表
info(ChanPid) -> info(ChanPid, all).

info(ChanPid, all) when is_pid(ChanPid) ->
    case is_process_alive(ChanPid) of
        true ->
            try gen_server:call(ChanPid, {peer_info}) catch _:_ -> #{}
            end;
        false -> #{}
    end;
info(ChanPid, Keys) when is_pid(ChanPid) ->
    maps:with(normalize_keys(Keys), info(ChanPid, all)).

normalize_keys(all) -> [phase, client_id, peer, received, delivered];
normalize_keys(Keys) when is_list(Keys) -> Keys;
normalize_keys(Key) -> [Key].

set_session(ChanPid, Session) ->
    case client_id_of(ChanPid) of
        undefined -> ok;
        ClientId -> dgiot_broker_session:update(ClientId,
                                                fun(_) -> Session end)
    end.

get_session(ChanPid) ->
    case client_id_of(ChanPid) of
        undefined -> undefined;
        ClientId ->
            case dgiot_broker_session:lookup(ClientId) of
                {ok, S} -> S;
                _ -> undefined
            end
    end.

do_deliver(ChanPid, {deliver, _Topic, Msg}) ->
    dgiot_broker_conn:deliver(ChanPid, Msg);
do_deliver(_ChanPid, _Deliver) ->
    ok.

stats(ChanPid) ->
    case info(ChanPid, all) of
        M when is_map(M) -> M;
        _ -> #{}
    end.

terminate(_ChanPid, _Reason) -> ok.

ensure_keepalive(_ChanPid, _Interval) -> ok.
clear_keepalive(_ChanPid) -> ok.

%% gen_statem 内部回调：跨模块不调用，显式声明未实现
init(_Args, _Opts) ->
    {error, {not_implemented, emqx_channel_internal, init}}.
handle_in(_Event, _State) ->
    {error, {not_implemented, emqx_channel_internal, handle_in}}.
handle_info(_Info, _State) ->
    {error, {not_implemented, emqx_channel_internal, handle_info}}.
handle_call(_Req, _State) ->
    {error, {not_implemented, emqx_channel_internal, handle_call}}.
handle_deliver(_Deliver, _State) ->
    {error, {not_implemented, emqx_channel_internal, handle_deliver}}.
handle_timeout(_Event, _State, _Data) ->
    {error, {not_implemented, emqx_channel_internal, handle_timeout}}.

channel() -> ?MODULE.

%% 从连接进程反查 client_id（进程字典里放一个标记，无则 undefined）
client_id_of(Pid) when is_pid(Pid) ->
    case get({client_id, Pid}) of
        undefined -> undefined;
        C -> C
    end.
