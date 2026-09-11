%% @doc 同名承接：emqx_session（会话）→ 我们的会话登记表。
%% info/2 直接读会话字段；离线队列三件套（enqueue/dequeue/replay）属
%% 持久会话离线队列（刀 4/7），显式 not_implemented，不静默。
-module(emqx_session).

-export([info/2, replay/2, enqueue/3, dequeue/2, is_persistent/1, terminate/2]).

info(ClientId, Keys) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, S} ->
            case Keys of
                all -> S;
                _ when is_list(Keys) -> maps:with(Keys, S);
                Key -> maps:get(Key, S, undefined)
            end;
        {error, not_found} ->
            undefined
    end.

is_persistent(ClientId) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, S} -> maps:get(clean_start, S, true) =:= false;
        _ -> false
    end.

%% 离线队列（持久会话断线重连补投）——刀 4 落地；此处显式声明未实现
replay(_ClientId, _ChanPid) ->
    {error, {not_implemented, cut4, offline_replay}}.

enqueue(_ClientId, _Deliver, _MaxLen) ->
    {error, {not_implemented, cut4, offline_enqueue}}.

dequeue(_ClientId, _Deliver) ->
    {error, {not_implemented, cut4, offline_dequeue}}.

terminate(_ClientId, _Reason) -> ok.
