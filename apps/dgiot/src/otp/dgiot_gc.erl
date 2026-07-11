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

-module(dgiot_gc).
-author("johnliu").

-behaviour(gen_server).

-export([start_link/0]).

-export([run/0]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% 5 minutes
-define(DEFAULT_INTERVAL, 300000).

%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

-spec(start_link() -> {ok, pid()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run() ->
    gen_server:call(?MODULE, run, infinity).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    Ref = erlang:send_after(?DEFAULT_INTERVAL, self(), run),
    {ok, schedule_gc(#{timer => Ref})}.

handle_call(run, _From, State) ->
    {Time, _} = timer:tc(fun run_gc/0),
    {reply, {ok, Time}, State, hibernate};

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run, State) ->
    run_gc(),
    {noreply, schedule_gc(State), hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internel function
%%------------------------------------------------------------------------------

schedule_gc(State) ->
    Interval = application:get_env(dgiot, gc_interval, ?DEFAULT_INTERVAL),
    State#{timer := erlang:send_after(Interval, self(), run)}.

run_gc() ->
    [garbage_collect(P) || P <- processes(),
                           {status, waiting} == process_info(P, status)].

