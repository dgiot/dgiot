%% @doc 同名承接：emqx_cm（连接管理器）→ 我们的会话登记表。
%% 连接信息 = 会话字段；踢会话 = 注销 + 停连接进程。
-module(emqx_cm).

-export([insert_channel_info/3, set_chan_info/2, get_chan_info/2,
         lookup_channels/1, kick_session/1, connection_closed/1,
         set_chan_stats/2, get_chan_stats/2, all_channels/0]).

insert_channel_info(ClientId, _ChanPid, Info) when is_binary(ClientId) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, S} -> dgiot_broker_session:update(ClientId,
                                                fun(_) -> maps:merge(S, Info) end);
        {error, not_found} ->
            dgiot_broker_session:register(ClientId, self(),
                                          maps:with([username, clean_start,
                                                     keepalive], Info))
    end.

set_chan_info(ClientId, Info) when is_map(Info) ->
    dgiot_broker_session:update(ClientId, fun(S) -> maps:merge(S, Info) end).

get_chan_info(ClientId, Key) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, S} -> maps:get(Key, S, undefined);
        {error, not_found} -> undefined
    end.

lookup_channels(ClientId) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, #{pid := Pid}} -> [Pid];
        {error, not_found} -> []
    end.

kick_session(ClientId) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, #{pid := Pid}} ->
            dgiot_broker_session:unregister(ClientId),
            catch exit(Pid, {kick, ClientId}),
            ok;
        {error, not_found} ->
            ok
    end.

connection_closed(ClientId) ->
    dgiot_broker_session:unregister(ClientId),
    ok.

set_chan_stats(ClientId, Stats) ->
    dgiot_broker_session:update(ClientId,
                                fun(S) -> S#{stats => Stats} end).

get_chan_stats(ClientId, Key) ->
    case dgiot_broker_session:lookup(ClientId) of
        {ok, #{stats := Stats}} -> maps:get(Key, Stats, undefined);
        _ -> undefined
    end.

all_channels() ->
    [C || {C, _S} <- dgiot_broker_session:list()].
