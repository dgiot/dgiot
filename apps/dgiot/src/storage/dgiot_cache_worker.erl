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

-module(dgiot_cache_worker).
-author("johnliu").
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([start_link/0, start_link/1,
    stop/1, get/1, set/3, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).

-record(cachestate, {threshold, maxsize, cacheets, checkpid, skip, dskip}).


%%%===================================================================
%%% API
%%%===================================================================

get(Key) ->
    get(?MODULE, Key).
get(ValueEts, Key) ->
    case dgiot_cache_check_worker:lookup(Key) of
        ok ->
            case ets:lookup(ValueEts, Key) of
                [] ->
                    <<>>;
                [H | _] ->
                    {_, Value} = H,
                    Value
            end;
        expired ->
            ets:delete(ValueEts, Key),
            <<>>;
        _ ->
            <<>>
    end.

set(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {set, Key, Value, TTL}).

delete(Key) ->
    ets:delete(?MODULE, Key),
    dgiot_cache_check_worker:delete(Key),
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

stop(NameOrPid) ->
    gen_server:cast(NameOrPid, stop).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxSize = proplists:get_value(ets_maxsize, Opts, 8 * 1024 * 1024),
    Threshold = proplists:get_value(ets_threshold, Opts, 0.85),
    CheckPid = proplists:get_value(checkpid, Opts),
    ValueEts = ets:new(?MODULE, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    Interval = dgiot:get_env(load_cache_classes_interval, 10),
    erlang:send_after(1000 * Interval, self(), load_cache_Product),
    erlang:send_after(1000 * Interval, self(), load_cache_Device),
    erlang:send_after(1000 * Interval, self(), load_cache_classes),
    {ok, #cachestate{maxsize = MaxSize,
        threshold = Threshold,
        cacheets = ValueEts,
        checkpid = CheckPid,
        skip = 0,
        dskip = 0
    }}.

handle_call({get, Key}, _From, #cachestate{cacheets = ValueEts} = State) ->
    Reply = get(ValueEts, Key),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value, TTL}, #cachestate{
    maxsize = MaxSize,
    threshold = Threshold,
    cacheets = ValueEts,
    checkpid = CheckPid} = State) ->
    ets:insert(ValueEts, {Key, Value}),
    gen_server:call(CheckPid, {set, Key, TTL}),
    case check_memsize(ValueEts, MaxSize) of
        over ->
            shrink_table(ValueEts, CheckPid),
            delete_light_items(ValueEts, Threshold * MaxSize, CheckPid);
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({delete, Key}, #cachestate{
    cacheets = ValueEts,
    checkpid = CheckPid} = State) ->
    ets:delete(ValueEts, Key),
    gen_server:cast(CheckPid, {delete, Key}),
    {noreply, State};
handle_cast(stop, #cachestate{
    cacheets = _ValueEts,
    checkpid = CheckPid} = State) ->
    gen_server:cast(CheckPid, stop),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'load_cache_classes_fin'}, State) ->
    io:format("~s ~p ~p ~n", [?FILE, ?LINE, load_cache_classes_fin]),
    {noreply, State};

handle_info(load_cache_Product, #cachestate{skip = Skip} = State) ->
    case dgiot_hook:run_hook('parse_cache_Product', {Skip}) of
        {ok, [{next, Next_Skip}]} ->
            Interval = dgiot:get_env(load_cache_classes_interval, 10),
            erlang:send_after(1000 * Interval, self(), load_cache_Product),
            {noreply, State#cachestate{skip = Next_Skip}};
        _ ->
            dgiot_bridge_server ! {start_custom_channel},
            {noreply, State}
    end;

handle_info(load_cache_Device, #cachestate{dskip = DSkip} = State) ->
    case dgiot_hook:run_hook('parse_cache_Device', {DSkip}) of
        {ok, [{next, Next_DSkip}]} ->
            Interval = dgiot:get_env(load_cache_classes_interval, 10),
            erlang:send_after(1000 * Interval, self(), load_cache_Device),
            {noreply, State#cachestate{dskip = Next_DSkip}};
        _ ->
            {noreply, State}
    end;

handle_info(load_cache_classes, State) ->
    case dgiot_hook:run_hook({'dgiot','load_cache_classes'}, {self()}) of
        {error, not_find} ->
            Interval = dgiot:get_env(load_cache_classes_interval, 10),
            erlang:send_after(1000 * Interval, self(), load_cache_classes);
        _ ->
            pass
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #cachestate{cacheets = ValueEts}) ->
    ets:delete(ValueEts),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_memsize(ValueEts, Size) ->
    F = fun(X) ->
        case X of
            {memory, _} -> true;
            _ -> false
        end
        end,
    [{_, H} | _] = lists:filter(F, ets:info(ValueEts)),
    ValueSize = H * erlang:system_info(wordsize),
    if
        ValueSize > Size -> over;
        true ->
            ok
    end.

shrink_table(ValueEts, CheckPid) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    ExpiredKeys = gen_server:call(CheckPid, {shrink, CurrentTime}),
    lists:foreach(fun(K) ->
        ets:delete(ValueEts, K)
                  end, ExpiredKeys).

delete_light_items(ValueEts, ThresholdSize, CheckPid) ->
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            SortedKeys = gen_server:call(CheckPid, sortkey),
            delete_items(CheckPid, ValueEts, ThresholdSize, SortedKeys);
        _ ->
            ok
    end.

delete_items(_CheckPid, _ValueEts, _ThresholdSize, []) ->
    ok;
delete_items(CheckPid, ValueEts, ThresholdSize, [{HeadKey, _} | Tail]) ->
    ets:delete(ValueEts, HeadKey),
    gen_server:cast(CheckPid, {delete, HeadKey}),
    case check_memsize(ValueEts, ThresholdSize) of
        over ->
            delete_items(CheckPid, ValueEts, ThresholdSize, Tail);
        _ ->
            ok
    end.
