%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(ekka_node_monitor).

-behaviour(gen_server).

-include("ekka.hrl").

%% API
-export([start_link/0, stop/0]).

-export([partitions/0]).

%% Internal Exports
-export([cast/2, run_after/2]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {
          partitions :: list(node()),
          heartbeat  :: undefined | reference(),
          autoheal   :: ekka_autoheal:autoheal(),
          autoclean  :: ekka_autoclean:autoclean()
         }).

-define(SERVER, ?MODULE).
-define(LOG(Level, Format, Args),
        logger:Level("Ekka(Monitor): " ++ Format, Args)).

%% @doc Start the node monitor.
-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:stop(?SERVER).

%% @doc Get partitions.
partitions() ->
    gen_server:call(?SERVER, partitions).

%% @private
cast(Node, Msg) ->
    gen_server:cast({?SERVER, Node}, Msg).

%% @private
run_after(Delay, Msg) ->
    erlang:send_after(Delay, ?SERVER, Msg).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    rand:seed(exsplus, erlang:timestamp()),
    net_kernel:monitor_nodes(true, [{node_type, visible}, nodedown_reason]),
    {ok, _} = mnesia:subscribe(system),
    lists:foreach(fun(N) -> self() ! {nodeup, N, []} end, nodes() -- [node()]),
    State = #state{partitions = [],
                   autoheal   = ekka_autoheal:init(),
                   autoclean  = ekka_autoclean:init()
                  },
    {ok, ensure_heartbeat(State)}.

handle_call(partitions, _From, State = #state{partitions = Partitions}) ->
    {reply, Partitions, State};

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignore, State}.

handle_cast({heartbeat, _FromNode}, State) ->
    {noreply, State};

handle_cast({suspect, FromNode, TargetNode}, State) ->
    ?LOG(info, "Suspect from ~s: ~s~n", [FromNode, TargetNode]),
    spawn(fun() ->
            Status = case net_adm:ping(TargetNode) of
                         pong -> up;
                         pang -> down
                     end,
            cast(FromNode, {confirm, TargetNode, Status})
          end),
    {noreply, State};

handle_cast({confirm, TargetNode, Status}, State) ->
    ?LOG(info, "Confirm ~s ~s", [TargetNode, Status]),
    {noreply, State};

handle_cast(Msg = {report_partition, _Node}, State) ->
    {noreply, autoheal_handle_msg(Msg, State)};

handle_cast(Msg = {heal_partition, _SplitView}, State) ->
    {noreply, autoheal_handle_msg(Msg, State)};

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({nodeup, Node, _Info}, State) ->
    ekka_membership:node_up(Node),
    {noreply, State};

handle_info({nodedown, Node, _Info}, State) ->
    ekka_membership:node_down(Node),
    run_after(3000, {suspect, Node}),
    {noreply, State};

handle_info({suspect, Node}, State) ->
    case ekka_mnesia:running_nodes() -- [node(), Node] of
        [ProxyNode|_] ->
            cast(ProxyNode, {suspect, node(), Node});
        [] -> ignore
    end,
    {noreply, State};

handle_info({mnesia_system_event, {mnesia_up, Node}},
            State = #state{partitions = Partitions}) ->
    ekka_membership:mnesia_up(Node),
    case lists:member(Node, Partitions) of
        false -> ok;
        true -> ekka_membership:partition_healed(Node)
    end,
    {noreply, State#state{partitions = lists:delete(Node, Partitions)}};

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    ekka_membership:mnesia_down(Node),
    {noreply, State};

handle_info({mnesia_system_event, {inconsistent_database, Context, Node}},
            State = #state{partitions = Partitions}) ->
    ?LOG(critical, "Network partition detected from node ~s: ~p", [Node, Context]),
    ekka_membership:partition_occurred(Node),
    case ekka_autoheal:enabled() of
        {true, _} -> run_after(3000, confirm_partition);
        false -> ignore
    end,
    {noreply, State#state{partitions = lists:usort([Node | Partitions])}};

handle_info({mnesia_system_event, {mnesia_overload, Details}}, State) ->
    ?LOG(error, "Mnesia overload: ~p", [Details]),
    {noreply, State};

handle_info({mnesia_system_event, Event}, State) ->
    ?LOG(error, "Mnesia system event: ~p", [Event]),
    {noreply, State};

%% Confirm if we should report the partitions
handle_info(confirm_partition, State = #state{partitions = []}) ->
    {noreply, State};

handle_info(confirm_partition, State = #state{partitions = Partitions}) ->
    Leader = ekka_membership:leader(),
    case ekka_node:is_running(Leader, ekka) of
        true  -> cast(Leader, {report_partition, node()});
        false -> ?LOG(critical, "Leader is down, cannot autoheal the partitions: ~p", [Partitions])
    end,
    {noreply, State};

handle_info({autoheal, Msg}, State) ->
    {noreply, autoheal_handle_msg(Msg, State)};

handle_info(heartbeat, State) ->
    AliveNodes = [N || N <- ekka_mnesia:cluster_nodes(all),
                       lists:member(N, nodes())],
    lists:foreach(fun(Node) ->
                    cast(Node, {heartbeat, node()})
                  end, AliveNodes),
    {noreply, ensure_heartbeat(State#state{heartbeat = undefined})};

handle_info(Msg = {'EXIT', Pid, _Reason}, State = #state{autoheal = Autoheal}) ->
    case ekka_autoheal:proc(Autoheal) of
        Pid -> {noreply, autoheal_handle_msg(Msg, State)};
        _   -> {noreply, State}
    end;

%% Autoclean Event.
handle_info(autoclean, State = #state{autoclean = AutoClean}) ->
    {noreply, State#state{autoclean = ekka_autoclean:check(AutoClean)}};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ensure_heartbeat(State = #state{heartbeat = undefined}) ->
    Interval = rand:uniform(2000) + 2000,
    State#state{heartbeat = run_after(Interval, heartbeat)};

ensure_heartbeat(State) ->
    State.

autoheal_handle_msg(Msg, State = #state{autoheal = Autoheal}) ->
    State#state{autoheal = ekka_autoheal:handle_msg(Msg, Autoheal)}.

