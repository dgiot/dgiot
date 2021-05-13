%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% @doc A simple ets-based rate limit server.
-module(esockd_limiter).

-behaviour(gen_server).

-export([ start_link/0
        , get_all/0
        , stop/0
        ]).

-export([ create/2
        , create/3
        , lookup/1
        , consume/1
        , consume/2
        , delete/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-type(bucket_name() :: term()).

-opaque(bucket_info() :: #{name      => bucket_name(),
                           capacity  => pos_integer(),
                           interval  => pos_integer(),
                           tokens    => pos_integer(),
                           lasttime  => integer()
                          }).

-export_type([bucket_info/0]).

%%-record(bucket, {name, capacity, interval, lastime}).

-define(TAB, ?MODULE).
-define(SERVER, ?MODULE).

-spec(start_link() -> {ok, pid()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(get_all() -> list(bucket_info())).
get_all() ->
    [bucket_info(Bucket) || Bucket = {{bucket, _}, _, _, _} <- ets:tab2list(?TAB)].

bucket_info({{bucket, Name}, Capacity, Interval, LastTime}) ->
    #{name     => Name,
      capacity => Capacity,
      interval => Interval,
      tokens   => tokens(Name),
      lasttime => LastTime
     }.

tokens(Name) ->
    ets:lookup_element(?TAB, {tokens, Name}, 2).

-spec(stop() -> ok).
stop() ->
    gen_server:stop(?SERVER).

-spec(create(bucket_name(), pos_integer()) -> ok).
create(Name, Capacity) when is_integer(Capacity), Capacity > 0 ->
    create(Name, Capacity , 1).

-spec(create(bucket_name(), pos_integer(), pos_integer()) -> ok).
create(Name, Capacity , Interval) when is_integer(Capacity), Capacity > 0,
                                       is_integer(Interval), Interval > 0 ->
    gen_server:call(?SERVER, {create, Name, Capacity, Interval}).

-spec(lookup(bucket_name()) -> undefined | bucket_info()).
lookup(Name) ->
    case ets:lookup(?TAB, {bucket, Name}) of
        [] -> undefined;
        [Bucket] -> bucket_info(Bucket)
    end.

-spec(consume(bucket_name()) -> {integer(), integer()}).
consume(Name) ->
    consume(Name, 1).

-spec(consume(bucket_name(), pos_integer()) -> {integer(), integer()}).
consume(Name, Tokens) when is_integer(Tokens), Tokens > 0 ->
    try ets:update_counter(?TAB, {tokens, Name}, {2, -Tokens, 0, 0}) of
        0 -> {0, pause_time(Name, erlang:system_time(millisecond))};
        I -> {I, 0}
    catch
        error:badarg -> {-1, 1000} %% pause for 1 second
    end.

%% @private
pause_time(Name, Now) ->
    case ets:lookup(?TAB, {bucket, Name}) of
        [] -> 1000; %% Pause 1 second if the bucket is deleted.
        [{_Bucket, _Capacity, Interval, LastTime}] ->
            max(1, LastTime + (Interval * 1000) - Now)
    end.

-spec(delete(bucket_name()) -> ok).
delete(Name) ->
    gen_server:cast(?SERVER, {delete, Name}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    _ = ets:new(?TAB, [public, set, named_table, {write_concurrency, true}]),
    {ok, #{countdown => #{}, timer => undefined}}.

handle_call({create, Name, Capacity, Interval}, _From, State = #{countdown := Countdown}) ->
    true = ets:insert(?TAB, {{tokens, Name}, Capacity}),
    true = ets:insert(?TAB, {{bucket, Name}, Capacity, Interval, erlang:system_time(millisecond)}),
    NCountdown = maps:put({bucket, Name}, Interval, Countdown),
    {reply, ok, ensure_countdown_timer(State#{countdown := NCountdown})};

handle_call(Req, _From, State) ->
    error_logger:error_msg("Unexpected call: ~p", [Req]),
    {reply, ignore, State}.

handle_cast({delete, Name}, State = #{countdown := Countdown}) ->
    true = ets:delete(?TAB, {bucket, Name}),
    true = ets:delete(?TAB, {tokens, Name}),
    NCountdown = maps:remove({bucket, Name}, Countdown),
    {noreply, State#{countdown := NCountdown}};

handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info({timeout, Timer, countdown}, State = #{countdown := Countdown, timer := Timer}) ->
    Countdown1 = maps:fold(
                   fun(Key = {bucket, Name}, 1, Map) ->
                           [{_Key, Capacity, Interval, _LastTime}] = ets:lookup(?TAB, Key),
                           true = ets:update_element(?TAB, {tokens, Name}, {2, Capacity}),
                           true = ets:update_element(?TAB, {bucket, Name}, {4, erlang:system_time(millisecond)}),
                           maps:put(Key, Interval, Map);
                      (Key, C, Map) when C > 1 ->
                           maps:put(Key, C-1, Map)
                   end, #{}, Countdown),
    NState = State#{countdown := Countdown1, timer := undefined},
    {noreply, ensure_countdown_timer(NState)};

handle_info(Info, State) ->
    error_logger:error_msg("Unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ensure_countdown_timer(State = #{timer := undefined}) ->
    TRef = erlang:start_timer(timer:seconds(1), self(), countdown),
    State#{timer := TRef};
ensure_countdown_timer(State = #{timer := _TRef}) -> State.

