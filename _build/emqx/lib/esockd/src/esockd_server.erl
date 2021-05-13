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

-module(esockd_server).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

%% stats API
-export([ stats_fun/2
        , init_stats/2
        , get_stats/1
        , inc_stats/3
        , dec_stats/3
        , del_stats/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {}).

-define(SERVER, ?MODULE).
-define(STATS_TAB, esockd_stats).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(stop() -> ok).
stop() -> gen_server:stop(?SERVER).

-spec(stats_fun({atom(), esockd:listen_on()}, atom()) -> fun()).
stats_fun({Protocol, ListenOn}, Metric) ->
    init_stats({Protocol, ListenOn}, Metric),
    fun({inc, Num}) -> esockd_server:inc_stats({Protocol, ListenOn}, Metric, Num);
       ({dec, Num}) -> esockd_server:dec_stats({Protocol, ListenOn}, Metric, Num)
    end.

-spec(init_stats({atom(), esockd:listen_on()}, atom()) -> ok).
init_stats({Protocol, ListenOn}, Metric) ->
    gen_server:call(?SERVER, {init, {Protocol, ListenOn}, Metric}).

-spec(get_stats({atom(), esockd:listen_on()}) -> [{atom(), non_neg_integer()}]).
get_stats({Protocol, ListenOn}) ->
    [{Metric, Val} || [Metric, Val]
                      <- ets:match(?STATS_TAB, {{{Protocol, ListenOn}, '$1'}, '$2'})].

-spec(inc_stats({atom(), esockd:listen_on()}, atom(), pos_integer()) -> any()).
inc_stats({Protocol, ListenOn}, Metric, Num) when is_integer(Num) ->
    update_counter({{Protocol, ListenOn}, Metric}, Num).

-spec(dec_stats({atom(), esockd:listen_on()}, atom(), pos_integer()) -> any()).
dec_stats({Protocol, ListenOn}, Metric, Num) when is_integer(Num) ->
    update_counter({{Protocol, ListenOn}, Metric}, -Num).

update_counter(Key, Num) ->
    ets:update_counter(?STATS_TAB, Key, {2, Num}).

-spec(del_stats({atom(), esockd:listen_on()}) -> ok).
del_stats({Protocol, ListenOn}) ->
    gen_server:cast(?SERVER, {del, {Protocol, ListenOn}}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    _ = ets:new(?STATS_TAB, [public, set, named_table,
                             {write_concurrency, true}]),
    {ok, #state{}}.

handle_call({init, {Protocol, ListenOn}, Metric}, _From, State) ->
    true = ets:insert(?STATS_TAB, {{{Protocol, ListenOn}, Metric}, 0}),
    {reply, ok, State, hibernate};

handle_call(Req, _From, State) ->
    error_logger:error_msg("[~s] Unexpected call: ~p", [?MODULE, Req]),
    {reply, ignore, State}.

handle_cast({del, {Protocol, ListenOn}}, State) ->
    ets:match_delete(?STATS_TAB, {{{Protocol, ListenOn}, '_'}, '_'}),
    {noreply, State, hibernate};

handle_cast(Msg, State) ->
    error_logger:error_msg("[~s] Unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("[~s] Unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

