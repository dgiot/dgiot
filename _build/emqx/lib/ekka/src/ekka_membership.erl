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

-module(ekka_membership).

-behaviour(gen_server).

-include("ekka.hrl").

-export([start_link/0, stop/0]).

%% Ring API
-export([ring/0, ring/1]).

%% Members API
-export([ local_member/0
        , lookup_member/1
        , members/0
        , members/1
        , is_member/1
        , oldest/1
        ]).

-export([ leader/0
        , nodelist/0
        , nodelist/1
        , coordinator/0
        , coordinator/1
        ]).

-export([is_all_alive/0]).

%% Monitor API
-export([monitor/3]).

%% Announce API
-export([announce/1]).

%% Ping/Pong API
-export([ping/2, pong/2]).

%% On Node/Mnesia Status
-export([ node_up/1
        , node_down/1
        , mnesia_up/1
        , mnesia_down/1
        ]).

%% On Cluster Status
-export([ partition_occurred/1
        , partition_healed/1
        ]).

%% gen_server Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {monitors, events}).

-type(event_type() :: partition | membership).

-define(SERVER, ?MODULE).
-define(LOG(Level, Format, Args),
        logger:Level("Ekka(Membership): " ++ Format, Args)).

-spec(start_link() -> {ok, pid()} | {error, term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(stop() -> ok).
stop() ->
    gen_server:stop(?SERVER).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(ring() -> [member()]).
ring() ->
    lists:keysort(#member.hash, members()).

-spec(ring(up | down) -> [member()]).
ring(Status) ->
    lists:keysort(#member.hash, members(Status)).

-spec(local_member() -> member()).
local_member() ->
    lookup_member(node()).

-spec(lookup_member(node()) -> member() | false).
lookup_member(Node) ->
    case ets:lookup(membership, Node) of [M] -> M; [] -> false end.

-spec(is_member(node()) -> boolean()).
is_member(Node) ->
    ets:member(membership, Node).

-spec(members() -> [member()]).
members() ->
    ets:tab2list(membership).

-spec(members(up | down) -> [member()]).
members(Status) ->
    [M || M = #member{status = St} <- members(), St =:= Status].

%% Get leader node of the members
-spec(leader() -> node()).
leader() ->
    Member = oldest(members()), Member#member.node.

%% Get coordinator node from all the alive members
-spec(coordinator() -> node()).
coordinator() ->
    Member = oldest(members(up)), Member#member.node.

%% Get Coordinator from nodes
-spec(coordinator(list(node())) -> node()).
coordinator(Nodes) ->
    Member = oldest([M || M <- [lookup_member(N) || N <- Nodes], M =/= false]),
    Member#member.node.

%% Get oldest member.
oldest(Members) ->
    hd(lists:sort(fun compare/2, Members)).

%% @private
compare(M1, M2) ->
    M1#member.guid < M2#member.guid.

-spec(nodelist() -> [node()]).
nodelist() ->
    [Node || #member{node = Node} <- members()].

-spec(nodelist(up | down) -> [node()]).
nodelist(Status) ->
    [Node || #member{node = Node} <- members(Status)].

-spec(is_all_alive() -> boolean()).
is_all_alive() ->
    length(ekka_mnesia:cluster_nodes(all) -- [node() | nodes()]) == 0.

-spec(monitor(event_type(), pid() | function(), boolean()) -> ok).
monitor(Type, PidOrFun, OnOff) ->
    call({monitor, {Type, PidOrFun, OnOff}}).

-spec(announce(join | leave | heal | {force_leave, node()}) -> ok).
announce(Action) ->
    call({announce, Action}).

-spec(ping(node(), member()) -> ok).
ping(Node, Member) ->
    case ekka_node:is_aliving(Node) of
        true  -> ping(Node, Member, 5);
        false -> ignore
    end.

ping(Node, _Member, 0) ->
    ?LOG(error, "Failed to ping ~s~n", [Node]);
ping(Node, Member, Retries) ->
    case ekka_node:is_running(Node, ekka) of
        true  -> cast(Node, {ping, Member});
        false -> timer:sleep(1000),
                 ping(Node, Member, Retries -1)
    end.

pong(Node, Member) ->
    cast(Node, {pong, Member}).

-spec(node_up(node()) -> ok).
node_up(Node) ->
    cast({node_up, Node}).

-spec(node_down(node()) -> ok).
node_down(Node) ->
    cast({node_down, Node}).

-spec(mnesia_up(node()) -> ok).
mnesia_up(Node) ->
    cast({mnesia_up, Node}).

-spec(mnesia_down(node()) -> ok).
mnesia_down(Node) ->
    cast({mnesia_down, Node}).

-spec partition_occurred(node()) -> ok.
partition_occurred(Node) ->
    cast({partition_occurred, Node}).

-spec partition_healed(node()) -> ok.
partition_healed(Node) ->
    cast({partition_healed, Node}).

%% @private
cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

%% @private
cast(Node, Msg) ->
    gen_server:cast({?SERVER, Node}, Msg).

%% @private
call(Req) ->
    gen_server:call(?SERVER, Req).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([]) ->
    _ = ets:new(membership, [ordered_set, protected, named_table, {keypos, 2}]),
    IsMnesiaRunning = case lists:member(node(), ekka_mnesia:running_nodes()) of
                          true  -> running;
                          false -> stopped
                      end,
    LocalMember = with_hash(#member{node = node(), guid = ekka_guid:gen(),
                                    status = up, mnesia = IsMnesiaRunning,
                                    ltime = erlang:timestamp()
                                   }),
    true = ets:insert(membership, LocalMember),
    lists:foreach(fun(Node) ->
                      spawn(?MODULE, ping, [Node, LocalMember])
                  end, ekka_mnesia:cluster_nodes(all) -- [node()]),
    {ok, #state{monitors = [], events = []}}.

with_hash(Member = #member{node = Node, guid = Guid}) ->
    Member#member{hash = erlang:phash2({Node, Guid}, trunc(math:pow(2, 32) - 1))}.

handle_call({monitor, {Type, PidOrFun, true}}, _From, State) ->
    reply(ok, add_monitor({Type, PidOrFun}, State));

handle_call({monitor, {Type, PidOrFun, false}}, _From, State) ->
    reply(ok, del_monitor({Type, PidOrFun}, State));

handle_call({announce, Action}, _From, State)
    when Action == join; Action == leave; Action == heal ->
    Status = case Action of
                 join  -> joining;
                 heal  -> healing;
                 leave -> leaving
             end,
    _ = [cast(N, {Status, node()}) || N <- nodelist(), N =/= node()],
    reply(ok, State);

handle_call({announce, {force_leave, Node}}, _From, State) ->
    _ = [cast(N, {leaving, Node}) || N <- nodelist(), N =/= Node],
    reply(ok, State);

handle_call(Req, _From, State) ->
    ?LOG(error, "Unexpected call: ~p", [Req]),
    {reply, ignore, State}.

handle_cast({node_up, Node}, State) ->
    ?LOG(info, "Node ~s up", [Node]),
    case ekka_mnesia:is_node_in_cluster(Node) of
        true ->
            Member = case lookup(Node) of
                       [M] -> M#member{status = up};
                       []  -> #member{node = Node, status = up}
                     end,
            insert(Member#member{mnesia = ekka_mnesia:cluster_status(Node)});
        false -> ignore
    end,
    notify({node, up, Node}, State),
    {noreply, State};

handle_cast({node_down, Node}, State) ->
    ?LOG(info, "Node ~s down", [Node]),
    case lookup(Node) of
        [#member{status = leaving}] ->
            ets:delete(membership, Node);
        [Member] ->
            insert(Member#member{status = down});
        [] -> ignore
    end,
    notify({node, down, Node}, State),
    {noreply, State};

handle_cast({joining, Node}, State) ->
    ?LOG(info, "Node ~s joining", [Node]),
    insert(case lookup(Node) of
               [Member] -> Member#member{status = joining};
               []       -> #member{node = Node, status = joining}
           end),
    notify({node, joining, Node}, State),
    {noreply, State};

handle_cast({healing, Node}, State) ->
    ?LOG(info, "Node ~s healing", [Node]),
    case lookup(Node) of
        [Member] -> insert(Member#member{status = healing});
        []       -> ignore
    end,
    notify({node, healing, Node}, State),
    {noreply, State};

handle_cast({ping, Member = #member{node = Node}}, State) ->
    pong(Node, local_member()),
    insert(Member#member{mnesia = ekka_mnesia:cluster_status(Node)}),
    {noreply, State};

handle_cast({pong, Member = #member{node = Node}}, State) ->
    insert(Member#member{mnesia = ekka_mnesia:cluster_status(Node)}),
    {noreply, State};

handle_cast({leaving, Node}, State) ->
    ?LOG(info, "Node ~s leaving", [Node]),
    case lookup(Node) of
        [#member{status = down}] ->
            ets:delete(membership, Node);
        [Member] ->
            insert(Member#member{status = leaving});
        [] -> ignore
    end,
    notify({node, leaving, Node}, State),
    {noreply, State};

handle_cast({mnesia_up, Node}, State) ->
    ?LOG(info, "Mnesia ~s up", [Node]),
    insert(case lookup(Node) of
               [Member] ->
                   Member#member{status = up, mnesia = running};
               [] ->
                   #member{node = Node, status = up, mnesia = running}
           end),
    spawn(?MODULE, pong, [Node, local_member()]),
    notify({mnesia, up, Node}, State),
    {noreply, State};

handle_cast({mnesia_down, Node}, State) ->
    ?LOG(info, "Mnesia ~s down", [Node]),
    case lookup(Node) of
        [#member{status = leaving}] ->
            ets:delete(membership, Node);
        [Member] ->
            insert(Member#member{mnesia = stopped});
        [] -> ignore
    end,
    notify({mnesia, down, Node}, State),
    {noreply, State};

handle_cast({partition_occurred, Node}, State) ->
    notify(partition, {occurred, Node}, State),
    {noreply, State};

handle_cast({partition_healed, Nodes}, State) ->
    notify(partition, {healed, Nodes}, State),
    {noreply, State};

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, DownPid, _Reason},
            State = #state{monitors = Monitors}) ->
    Left = [M || M = {{_, Pid}, _} <- Monitors, Pid =/= DownPid],
    {noreply, State#state{monitors = Left}};

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

lookup(Node) ->
    ets:lookup(membership, Node).

insert(Member) ->
    ets:insert(membership, Member#member{ltime = erlang:timestamp()}).

reply(Reply, State) ->
    {reply, Reply, State}.

notify(Event, State) ->
    notify(membership, Event, State).

notify(Type, Event, #state{monitors = Monitors}) ->
    Notify = fun(P) when is_pid(P) ->
                     P ! {Type, Event};
                (F) when is_function(F) ->
                     F({Type, Event})
             end,
    [Notify(PidOrFun) || {{T, PidOrFun}, _} <- Monitors, T == Type].

add_monitor({Type, PidOrFun}, S = #state{monitors = Monitors}) ->
    case lists:keymember({Type, PidOrFun}, 1, Monitors) of
        true  -> S;
        false ->
            MRef = case is_pid(PidOrFun) of
                       true -> erlang:monitor(process, PidOrFun);
                       _ -> undefined
                   end,
            S#state{monitors = [{{Type, PidOrFun}, MRef} | Monitors]}
    end.

del_monitor({Type, PidOrFun}, S = #state{monitors = Monitors}) ->
    case lists:keyfind({Type, PidOrFun}, 1, Monitors) of
        false -> S;
        {_, MRef} ->
            is_pid(PidOrFun) andalso erlang:demonitor(MRef, [flush]),
            S#state{monitors = lists:delete({{Type, PidOrFun}, MRef}, Monitors)}
    end.

