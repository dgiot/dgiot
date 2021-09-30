%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% Channel Manager
-module(dgiot_cm).

-behaviour(gen_server).

-include_lib("dgiot/include/dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/types.hrl").

-logger_header("[CM]").

-export([start_link/0]).

-export([ register_channel/3
        , unregister_channel/1
        , insert_channel_info/3
        ]).

-export([connection_closed/1]).

-export([ get_chan_info/1
        , get_chan_info/2
        , set_chan_info/2
        ]).

-export([ get_chan_stats/1
        , get_chan_stats/2
        , set_chan_stats/2
        ]).

-export([get_chann_conn_mod/2]).

%%-export([ open_session/3
%%        , discard_session/1
%%        , discard_session/2
%%        , takeover_session/1
%%        , takeover_session/2
%%        , kick_session/1
%%        , kick_session/2
%%        ]).

-export([ lookup_channels/1
        , lookup_channels/2
        ]).

-export([all_channels/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Internal export
-export([stats_fun/0]).

-type(chan_pid() :: pid()).

%% Tables for channel management.
-define(CHAN_TAB, dgiot_channel).
-define(CHAN_CONN_TAB, dgiot_channel_conn).
-define(CHAN_INFO_TAB, dgiot_channel_info).

-define(CHAN_STATS,
        [{?CHAN_TAB, 'channels.count', 'channels.max'},
         {?CHAN_TAB, 'sessions.count', 'sessions.max'},
         {?CHAN_CONN_TAB, 'connections.count', 'connections.max'}
        ]).

%% Batch drain
-define(BATCH_SIZE, 100000).

%% Server name
-define(CM, ?MODULE).

-define(T_TAKEOVER, 15000).

%% @doc Start the channel manager.
-spec(start_link() -> startlink_ret()).
start_link() ->
    gen_server:start_link({local, ?CM}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Insert/Update the channel info and stats to dgiot_channel table
-spec(insert_channel_info(binary(),
                          dgiot_types:infos(),
                          dgiot_types:stats()) -> ok).
insert_channel_info(ChannelId, Info, Stats) ->
    Chan = {ChannelId, self()},
    true = ets:insert(?CHAN_INFO_TAB, {Chan, Info, Stats}),
    ok.

%% @private
%% @doc Register a channel with pid and conn_mod.
%%
%% There is a Race-Condition on one node or cluster when many connections
%% login to Broker with the same clientid. We should register it and save
%% the conn_mod first for taking up the clientid access right.
%%
%% Note that: It should be called on a lock transaction
register_channel(ChannelId, ChanPid, #{conn_mod := ConnMod}) when is_pid(ChanPid) ->
    Chan = {ChannelId, ChanPid},
    true = ets:insert(?CHAN_TAB, Chan),
    true = ets:insert(?CHAN_CONN_TAB, {Chan, ConnMod}),
    ok = dgiot_cm_registry:register_channel(Chan),
    cast({registered, Chan}).

%% @doc Unregister a channel.
-spec(unregister_channel(binary()) -> ok).
unregister_channel(ChannelId) when is_binary(ChannelId) ->
    true = do_unregister_channel({ChannelId, self()}),
    ok.

%% @private
do_unregister_channel(Chan) ->
    ok = dgiot_cm_registry:unregister_channel(Chan),
    true = ets:delete(?CHAN_CONN_TAB, Chan),
    true = ets:delete(?CHAN_INFO_TAB, Chan),
    ets:delete_object(?CHAN_TAB, Chan).

-spec(connection_closed(binary()) -> true).
connection_closed(ChannelId) ->
    connection_closed(ChannelId, self()).

-spec(connection_closed(binary(), chan_pid()) -> true).
connection_closed(ChannelId, ChanPid) ->
    ets:delete_object(?CHAN_CONN_TAB, {ChannelId, ChanPid}).

%% @doc Get info of a channel.
-spec(get_chan_info(binary()) -> maybe(dgiot_types:infos())).
get_chan_info(ChannelId) ->
    with_channel(ChannelId, fun(ChanPid) -> get_chan_info(ChannelId, ChanPid) end).

-spec(get_chan_info(binary(), chan_pid())
      -> maybe(dgiot_types:infos())).
get_chan_info(ChannelId, ChanPid) when node(ChanPid) == node() ->
    Chan = {ChannelId, ChanPid},
    try ets:lookup_element(?CHAN_INFO_TAB, Chan, 2)
    catch
        error:badarg -> undefined
    end;
get_chan_info(ChannelId, ChanPid) ->
    rpc_call(node(ChanPid), get_chan_info, [ChannelId, ChanPid]).

%% @doc Update infos of the channel.
-spec(set_chan_info(binary(), dgiot_types:attrs()) -> boolean()).
set_chan_info(ChannelId, Info) when is_binary(ChannelId) ->
    Chan = {ChannelId, self()},
    try ets:update_element(?CHAN_INFO_TAB, Chan, {2, Info})
    catch
        error:badarg -> false
    end.

%% @doc Get channel's stats.
-spec(get_chan_stats(dgiot_types:clientid()) -> maybe(dgiot_types:stats())).
get_chan_stats(ClientId) ->
    with_channel(ClientId, fun(ChanPid) -> get_chan_stats(ClientId, ChanPid) end).

-spec(get_chan_stats(dgiot_types:clientid(), chan_pid())
        -> maybe(dgiot_types:stats())).
get_chan_stats(ClientId, ChanPid) when node(ChanPid) == node() ->
    Chan = {ClientId, ChanPid},
    try ets:lookup_element(?CHAN_INFO_TAB, Chan, 3)
    catch
        error:badarg -> undefined
    end;
get_chan_stats(ClientId, ChanPid) ->
    rpc_call(node(ChanPid), get_chan_stats, [ClientId, ChanPid]).

%% @doc Set channel's stats.
-spec(set_chan_stats(dgiot_types:clientid(), dgiot_types:stats()) -> boolean()).
set_chan_stats(ClientId, Stats) when is_binary(ClientId) ->
    set_chan_stats(ClientId, self(), Stats).

-spec(set_chan_stats(dgiot_types:clientid(), chan_pid(), dgiot_types:stats())
        -> boolean()).
set_chan_stats(ClientId, ChanPid, Stats) ->
    Chan = {ClientId, ChanPid},
    try ets:update_element(?CHAN_INFO_TAB, Chan, {3, Stats})
    catch
        error:badarg -> false
    end.

with_channel(ClientId, Fun) ->
    case lookup_channels(ClientId) of
        []    -> undefined;
        [Pid] -> Fun(Pid);
        Pids  -> Fun(lists:last(Pids))
    end.

%% @doc Get all channels registed.
all_channels() ->
    Pat = [{{'_', '$1'}, [], ['$1']}],
    ets:select(?CHAN_TAB, Pat).

%% @doc Lookup channels.
-spec(lookup_channels(dgiot_types:clientid()) -> list(chan_pid())).
lookup_channels(ClientId) ->
    lookup_channels(global, ClientId).

%% @doc Lookup local or global channels.
-spec(lookup_channels(local | global, dgiot_types:clientid()) -> list(chan_pid())).
lookup_channels(global, ClientId) ->
    case dgiot_cm_registry:is_enabled() of
        true ->
            dgiot_cm_registry:lookup_channels(ClientId);
        false ->
            lookup_channels(local, ClientId)
    end;

lookup_channels(local, ClientId) ->
    [ChanPid || {_, ChanPid} <- ets:lookup(?CHAN_TAB, ClientId)].

%% @private
rpc_call(Node, Fun, Args) ->
    case rpc:call(Node, ?MODULE, Fun, Args) of
        {badrpc, Reason} -> error(Reason);
        Res -> Res
    end.

%% @private
cast(Msg) -> gen_server:cast(?CM, Msg).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    TabOpts = [public, {write_concurrency, true}],
    ok = dgiot_tables:new(?CHAN_TAB, [bag, {read_concurrency, true}|TabOpts]),
    ok = dgiot_tables:new(?CHAN_CONN_TAB, [bag | TabOpts]),
    ok = dgiot_tables:new(?CHAN_INFO_TAB, [set, compressed | TabOpts]),
    ok = dgiot_stats:update_interval(chan_stats, fun ?MODULE:stats_fun/0),
    {ok, #{chan_pmon => dgiot_pmon:new()}}.

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignored, State}.

handle_cast({registered, {ClientId, ChanPid}}, State = #{chan_pmon := PMon}) ->
    PMon1 = dgiot_pmon:monitor(ChanPid, ClientId, PMon),
    {noreply, State#{chan_pmon := PMon1}};

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, State = #{chan_pmon := PMon}) ->
    ChanPids = [Pid | dgiot_misc:drain_down(?BATCH_SIZE)],
    {Items, PMon1} = dgiot_pmon:erase_all(ChanPids, PMon),
    ok = dgiot_pool:async_submit(fun lists:foreach/2, [fun clean_down/1, Items]),
    {noreply, State#{chan_pmon := PMon1}};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    dgiot_stats:cancel_update(chan_stats).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

clean_down({ChanPid, ClientId}) ->
    do_unregister_channel({ClientId, ChanPid}).

stats_fun() ->
    lists:foreach(fun update_stats/1, ?CHAN_STATS).

update_stats({Tab, Stat, MaxStat}) ->
    case ets:info(Tab, size) of
        undefined -> ok;
        Size -> dgiot_stats:setstat(Stat, MaxStat, Size)
    end.

get_chann_conn_mod(ClientId, ChanPid) when node(ChanPid) == node() ->
    Chan = {ClientId, ChanPid},
    try [ConnMod] = ets:lookup_element(?CHAN_CONN_TAB, Chan, 2), ConnMod
    catch
        error:badarg -> undefined
    end;
get_chann_conn_mod(ClientId, ChanPid) ->
    rpc_call(node(ChanPid), get_chann_conn_mod, [ClientId, ChanPid]).

