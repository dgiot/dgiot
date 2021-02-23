%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% --------------------------------------------------
%%
%% @author Ulf Wiger <ulf@wiger.net>
%%
%% @doc Extended process registry
%% <p>This module implements an extended process registry</p>
%% <p>For a detailed description, see gproc/doc/erlang07-wiger.pdf.</p>
%% @end
-module(gproc_dist).
-behaviour(gen_leader).

-export([start_link/0, start_link/1,
         reg/1, reg/4, unreg/1,
         reg_other/5, unreg_other/2,
	 reg_or_locate/3,
	 reg_shared/3, unreg_shared/1,
         monitor/2,
         demonitor/2,
	 set_attributes/2,
	 set_attributes_shared/2,
         mreg/2,
         munreg/2,
         set_value/2,
	 set_value_shared/2,
         give_away/2,
         update_counter/3,
	 update_counters/1,
	 update_shared_counter/2,
	 reset_counter/1]).

-export([leader_call/1,
         leader_cast/1,
         sync/0,
         get_leader/0]).

%%% internal exports
-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2, handle_info/3,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/2,  % original version
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).

-include("gproc_int.hrl").
-include("gproc.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          always_broadcast = false,
          is_leader,
          sync_clients = [],
          sync_requests = []}).

-include("gproc_trace.hrl").
%% ==========================================================
%% Start functions

start_link() ->
    start_link({[node()|nodes()], []}).

start_link(all) ->
    Workers = case application:get_env(gproc_dist_workers) of
        {ok, [_|_] = WorkersList} -> WorkersList;
        _ -> []
    end,
    start_link({[node()|nodes()], [{bcast_type, all}, {workers, Workers}]});
start_link(Nodes) when is_list(Nodes) ->
    start_link({Nodes, []});
start_link({Nodes, Opts}) ->
    SpawnOpts = gproc_lib:valid_opts(server_options, []),
    gen_leader:start_link(
      ?SERVER, Nodes, Opts, ?MODULE, [], [{spawn_opt, SpawnOpts}]).

%% ==========================================================
%% API

%% {@see gproc:reg/1}
%%
reg(Key) ->
    reg(Key, gproc:default(Key), [], reg).

%% {@see gproc:reg_or_locate/2}
%%
reg_or_locate({n,g,_} = Key, Value, Pid) when is_pid(Pid) ->
    leader_call({reg_or_locate, Key, Value, Pid});
reg_or_locate({n,g,_} = Key, Value, F) when is_function(F, 0) ->
    MyGroupLeader = group_leader(),
    leader_call({reg_or_locate, Key, Value,
		 fun() ->
			 %% leader will spawn on caller's node
			 group_leader(MyGroupLeader, self()),
			 F()
		 end});
reg_or_locate(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).


%%% @spec({Class,g, Key}, Value) -> true
%%% @doc
%%%    Class = n  - unique name
%%%          | p  - non-unique property
%%%          | c  - counter
%%%          | a  - aggregated counter
%%%          | r  - resource property
%%%          | rc - resource counter
%%% @end
reg({_,g,_} = Key, Value, Attrs, Op) ->
    %% anything global
    leader_call({reg, Key, Value, self(), Attrs, Op});
reg(_, _, _, _) ->
    ?THROW_GPROC_ERROR(badarg).

%% @spec ({Class,g,Key}, pid(), Value, Attrs, Op::reg | unreg) -> true
%% @doc
%%    Class = n  - unique name
%%          | a  - aggregated counter
%%          | r  - resource property
%%          | rc - resource counter
%%    Value = term()
%%    Attrs = [{Key, Value}]
%% @end
reg_other({T,g,_} = Key, Pid, Value, Attrs, Op) when is_pid(Pid) ->
    if T==n; T==a; T==r; T==rc ->
            leader_call({reg_other, Key, Value, Pid, Attrs, Op});
       true ->
            ?THROW_GPROC_ERROR(badarg)
    end;
reg_other(_, _, _, _, _) ->
    ?THROW_GPROC_ERROR(badarg).

unreg_other({T,g,_} = Key, Pid) when is_pid(Pid) ->
    if T==n; T==a; T==r; T==rc ->
            leader_call({unreg_other, Key, Pid});
       true ->
            ?THROW_GPROC_ERROR(badarg)
    end;
unreg_other(_, _) ->
    ?THROW_GPROC_ERROR(badarg).

reg_shared({_,g,_} = Key, Value, Attrs) ->
    leader_call({reg, Key, Value, shared, Attrs, reg});
reg_shared(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).

monitor({_,g,_} = Key, Type) when Type==info;
                                  Type==follow;
                                  Type==standby ->
    leader_call({monitor, Key, self(), Type});
monitor(_, _) ->
    ?THROW_GPROC_ERROR(badarg).

demonitor({_,g,_} = Key, Ref) ->
    leader_call({demonitor, Key, self(), Ref});
demonitor(_, _) ->
    ?THROW_GPROC_ERROR(badarg).


set_attributes({_,g,_} = Key, Attrs) ->
    leader_call({set_attributes, Key, Attrs, self()});
set_attributes(_, _) ->
    ?THROW_GPROC_ERROR(badarg).

set_attributes_shared({_,g,_} = Key, Attrs) ->
    leader_call({set_attributes, Key, Attrs, shared});
set_attributes_shared(_, _) ->
    ?THROW_GPROC_ERROR(badarg).


mreg(T, KVL) ->
    if is_list(KVL) -> leader_call({mreg, T, g, KVL, self()});
       true -> ?THROW_GPROC_ERROR(badarg)
    end.

munreg(T, Keys) ->
    if is_list(Keys) -> leader_call({munreg, T, g, Keys, self()});
       true -> ?THROW_GPROC_ERROR(badarg)
    end.

unreg({_,g,_} = Key) ->
    leader_call({unreg, Key, self()});
unreg(_) ->
    ?THROW_GPROC_ERROR(badarg).

unreg_shared({T,g,_} = Key) when T==c; T==a ->
    leader_call({unreg, Key, shared});
unreg_shared(_) ->
    ?THROW_GPROC_ERROR(badarg).


set_value({T,g,_} = Key, Value) when T==a; T==c ->
    if is_integer(Value) ->
            leader_call({set, Key, Value, self()});
       true ->
            ?THROW_GPROC_ERROR(badarg)
    end;
set_value({_,g,_} = Key, Value) ->
    leader_call({set, Key, Value, self()});
set_value(_, _) ->
    ?THROW_GPROC_ERROR(badarg).

set_value_shared({T,g,_} = Key, Value) when T==a; T==c; T==p ->
    leader_call({set, Key, Value, shared});
set_value_shared(_, _) ->
    ?THROW_GPROC_ERROR(badarg).


give_away({_,g,_} = Key, To) ->
    leader_call({give_away, Key, To, self()}).


update_counter({T,g,_} = Key, Pid, Incr) when is_integer(Incr), T==c;
					      is_integer(Incr), T==n ->
    leader_call({update_counter, Key, Incr, Pid});
update_counter(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).

update_counters(List) when is_list(List) ->
    leader_call({update_counters, List});
update_counters(_) ->
    ?THROW_GPROC_ERROR(badarg).

update_shared_counter({c,g,_} = Key, Incr) when is_integer(Incr) ->
    leader_call({update_counter, Key, Incr, shared});
update_shared_counter(_, _) ->
    ?THROW_GPROC_ERROR(badarg).


reset_counter({c,g,_} = Key) ->
    leader_call({reset_counter, Key, self()});
reset_counter(_) ->
    ?THROW_GPROC_ERROR(badarg).


%% @spec sync() -> true
%% @doc Synchronize with the gproc leader
%%
%% This function can be used to ensure that data has been replicated from the
%% leader to the current node. It does so by asking the leader to ping all
%% live participating nodes. The call will return `true' when all these nodes
%% have either responded or died. In the special case where the leader dies
%% during an ongoing sync, the call will fail with a timeout exception.
%% (Actually, it should be a `leader_died' exception; more study needed to find
%% out why gen_leader times out in this situation, rather than reporting that
%% the leader died.)
%% @end
%%
sync() ->
    %% Increase timeout since gen_leader can take some time ...
    gen_server:call(?MODULE, sync, 10000).

%% @spec get_leader() -> node()
%% @doc Returns the node of the current gproc leader.
%% @end
get_leader() ->
    GenLeader = gen_leader,
    GenLeader:call(?MODULE, get_leader).

%% ==========================================================
%% Server-side

handle_cast(_Msg, S, _) ->
    {stop, unknown_cast, S}.

handle_call(get_leader, _, S, E) ->
    {reply, gen_leader:leader_node(E), S};
handle_call(sync, From, S, E) ->
    {noreply, initiate_sync(From, S, E)};
handle_call(_, _, S, _) ->
    {reply, badarg, S}.

handle_info({'DOWN', _MRef, process, Pid, _}, S) ->
    ets:delete(?TAB, {Pid, g}),
    leader_cast({pid_is_DOWN, Pid}),
    {ok, S};
handle_info({gproc_unreg, Objs}, S) ->
    {ok, [{delete, Objs}], S};
handle_info(_, S) ->
    {ok, S}.

handle_info(Msg, S, _E) ->
    handle_info(Msg, S).


elected(S, _E) ->
    {ok, {globals,globs()}, S#state{is_leader = true}}.

elected(S, E, undefined) ->
    %% I have become leader; full synch
    {ok, {globals, globs()},
     maybe_reinitiate_sync(S#state{is_leader = true}, E)};
elected(S, E, _Node) ->
    Synch = {globals, globs()},
    if not S#state.always_broadcast ->
            %% Another node recognized us as the leader.
            %% Don't broadcast all data to everyone else
            {reply, Synch, maybe_reinitiate_sync(S, E)};
       true ->
            %% Main reason for doing this is if we are using a gen_leader
            %% that doesn't support the 'reply' return value
            {ok, Synch, maybe_reinitiate_sync(S, E)}
    end.

globs() ->
    Gs = ets:select(?TAB, [{{{{'_',g,'_'},'_'},'_','_'},[],['$_']}]),
    As = ets:select(?TAB, [{{{'$1',{'_',g,'_'}}, '$2'},[],['$_']}]),
    _ = [gproc_lib:ensure_monitor(Pid, g) || {_, Pid, _} <- Gs],
    Gs ++ As.

surrendered(#state{is_leader = true} = S, {globals, Globs}, E) ->
    %% Leader conflict!
    surrendered_1(Globs),
    {ok, maybe_reinitiate_sync(S#state{is_leader = false}, E)};
surrendered(S, {globals, Globs}, E) ->
    %% globals from this node should be more correct in our table than
    %% in the leader's
    surrendered_1(Globs),
    {ok, maybe_reinitiate_sync(S#state{is_leader = false}, E)}.


handle_DOWN(Node, S, E) ->
    S1 = check_sync_requests(Node, S, E),
    Head = {{{'_',g,'_'},'_'},'$1','_'},
    Gs = [{'==', {node,'$1'},Node}],
    Globs = ets:select(?TAB, [{Head, Gs, [{{{element,1,{element,1,'$_'}},
                                            {element,2,'$_'}}}]}]),
    case process_globals(Globs) of
        [] ->
            {ok, S1};
        Broadcast ->
            {ok, Broadcast, S1}
    end.

check_sync_requests(Node, #state{sync_requests = SReqs} = S, E) ->
    check_sync_requests(SReqs, Node, S, E).

check_sync_requests([], _, S, _) ->
    S;
check_sync_requests([{From, Ns}|Reqs], Node, S, E) ->
    case lists:member(Node, Ns) of
        true ->
            remove_node_from_sync_request(Node, Ns, From, S, E);
        false ->
            check_sync_requests(Reqs, Node, S, E)
    end.

remove_node_from_sync_request(Node, Ns, From, S, E) ->
    case Ns -- [Node] of
        [] ->
            check_sync_requests(Node, send_sync_complete(From, S, E), E);
        Ns1 ->
            Rs1 = lists:keyreplace(
                    From, 1, S#state.sync_requests, {From, Ns1}),
            %% Yes, we start over and run through the list from the top,
            %% with updated state; simpler code that way.
            check_sync_requests(Node, S#state{sync_requests = Rs1}, E)
    end.

handle_leader_call({Reg, {_C,g,_Name} = K, Value, Pid, As, Op}, _From, S, _E)
  when Reg==reg; Reg==reg_other ->
    case gproc_lib:insert_reg(K, Value, Pid, g) of
        false when Op == reg ->
            {reply, badarg, S};
        false when Op == ensure ->
            case ets:lookup(?TAB, ets_key(K, Pid)) of
                [{_, Pid, _}] ->
                    gproc_lib:do_set_value(K, Value, Pid),
                    gproc_lib:insert_attr(K, As, Pid, g),
                    Vals = mk_broadcast_insert_vals([{K, Pid, Value}]),
                    {reply, updated, [{insert, Vals}], S};
                _ ->
                    {reply, badarg, [], S}
            end;
        true ->
            _ = gproc_lib:ensure_monitor(Pid,g),
            _ = if As =/= [] ->
                        gproc_lib:insert_attr(K, As, Pid, g);
                   true -> []
                end,
	    Vals = mk_broadcast_insert_vals([{K, Pid, Value}]),
            {reply, regged_new(Op), [{insert, Vals}], S}
    end;
handle_leader_call({monitor, {T,g,_} = K, MPid, Type}, _From, S, _E) when T==n;
                                                                          T==a ->
    case ets:lookup(?TAB, {K, T}) of
        [{_, Pid, _}] ->
            Opts = get_opts(Pid, K),
            Ref = make_ref(),
            Opts1 = gproc_lib:add_monitor(Opts, MPid, Ref, Type),
            _ = gproc_lib:ensure_monitor(MPid, g),
            Obj = {{Pid,K}, Opts1},
            ets:insert(?TAB, Obj),
            {reply, Ref, [{insert, [Obj]}], S};
        LookupRes ->
            Ref = make_ref(),
            case Type of
                standby ->
                    Event = {failover, MPid},
                    Msgs = insert_reg(LookupRes, K, undefined, MPid, Event),
                    Obj = {{K,T}, MPid, undefined},
                    Rev = {{MPid,K}, []},
                    ets:insert(?TAB, [Obj, Rev]),
                    MPid ! {gproc, {failover,MPid}, Ref, K},
                    {reply, Ref, [{insert, [Obj, Rev]},
                                  {notify, Msgs}], S};
                follow ->
                    case LookupRes of
                        [{_, Waiters}] ->
                            add_follow_to_waiters(Waiters, K, MPid, Ref, S);
                        [] ->
                            add_follow_to_waiters([], K, MPid, Ref, S);
                        [{_, Pid, _}] ->
                            case ets:lookup(?TAB, {Pid,K}) of
                                [{_, Opts}] when is_list(Opts) ->
                                    Opts1 = gproc_lib:add_monitor(
                                              Opts, MPid, Ref, follow),
                                    ets:insert(?TAB, {{Pid,K}, Opts1}),
                                    {reply, Ref,
                                     [{insert, [{{Pid,K}, Opts1}]}], S}
                            end
                    end;
                _ ->
                    MPid ! {gproc, unreg, Ref, K},
                    {reply, Ref, S}
            end
    end;
handle_leader_call({demonitor, {T,g,_} = K, MPid, Ref}, _From, S, _E) ->
    case ets:lookup(?TAB, {K,T}) of
        [{_, Pid, _}] ->
            Opts = get_opts(Pid, K),
            Opts1 = gproc_lib:remove_monitors(Opts, MPid, Ref),
            Obj = {{Pid,K}, Opts1},
            ets:insert(?TAB, Obj),
            Del = case gproc_lib:does_pid_monitor(MPid, Opts1) of
                      true -> [];
                      false ->
                          ets:delete(?TAB, {MPid, K}),
                          [{delete, [{MPid, K}]}]
                  end,
            {reply, ok, Del ++ [{insert, [Obj]}], S};
        [{Key, Waiters}] ->
            case lists:filter(fun({P, R, _}) ->
                                      P =/= MPid orelse R =/= Ref
                              end, Waiters) of
                [] ->
                    ets:delete(?TAB, {MPid, K}),
                    ets:delete(?TAB, Key),
                    {reply, ok, [{delete, [{MPid, K}, Key]}], S};
                NewWaiters ->
                    ets:insert(?TAB, {Key, NewWaiters}),
                    Del = case lists:keymember(MPid, 1, NewWaiters) of
                              false ->
                                  ets:delete(?TAB, {MPid, K}),
                                  [{delete, [{MPid, K}]}];
                              true ->
                                  []
                          end,
                    {reply, ok, Del ++ [{insert, [{Key, NewWaiters}]}], S}
            end;
        _ ->
            {reply, ok, S}
    end;
handle_leader_call({set_attributes, {_,g,_} = K, Attrs, Pid}, _From, S, _E) ->
    case gproc_lib:insert_attr(K, Attrs, Pid, g) of
	false ->
	    {reply, badarg, S};
	NewAttrs when is_list(NewAttrs) ->
	    {reply, true, [{insert, [{{Pid,K}, NewAttrs}]}], S}
    end;
handle_leader_call({reg_or_locate, {n,g,_} = K, Value, P},
		   {FromPid, _}, S, _E) ->
    FromNode = node(FromPid),
    Reg = fun() ->
		  Pid = if is_function(P, 0) ->
				spawn(FromNode, P);
			   is_pid(P) ->
				P
			end,
		  case gproc_lib:insert_reg(K, Value, Pid, g) of
		      true ->
			  _ = gproc_lib:ensure_monitor(Pid,g),
			  Vals = [{{K,n},Pid,Value}],
			  {reply, {Pid, Value}, [{insert, Vals}], S};
		      false ->
			  {reply, badarg, S}
		  end
	  end,
    case ets:lookup(?TAB, {K, n}) of
	[] ->
	    Reg();
	[{_, _Waiters}] ->
	    Reg();
	[{_, OtherPid, OtherVal}] ->
	    {reply, {OtherPid, OtherVal}, S}
    end;
handle_leader_call({update_counter, {T,g,_Ctr} = Key, Incr, Pid}, _From, S, _E)
  when is_integer(Incr), T==c;
       is_integer(Incr), T==n ->
    try New = ets:update_counter(?TAB, {Key, Pid}, {3,Incr}),
	 RealPid = case Pid of
		       n -> ets:lookup_element(?TAB, {Key,Pid}, 2);
		       shared -> shared;
		       P when is_pid(P) -> P
		   end,
	 Vals = [{{Key,Pid},RealPid,New} | update_aggr_counter(Key, Incr)],
        {reply, New, [{insert, Vals}], S}
    catch
        error:_ ->
            {reply, badarg, S}
    end;
handle_leader_call({update_counters, Cs}, _From, S, _E) ->
    try  {Replies, Vals} = batch_update_counters(Cs),
	 {reply, Replies, [{insert, Vals}], S}
    catch
	error:_ ->
	    {reply, badarg, S}
    end;
handle_leader_call({reset_counter, {c,g,_Ctr} = Key, Pid}, _From, S, _E) ->
    try  Current = ets:lookup_element(?TAB, {Key, Pid}, 3),
	 Initial = case ets:lookup_element(?TAB, {Pid, Key}, 2) of
		       r -> 0;
		       Opts when is_list(Opts) ->
			   proplists:get_value(initial, Opts, 0)
		   end,
	 Incr = Initial - Current,
	 New = ets:update_counter(?TAB, {Key, Pid}, {3, Incr}),
	 Vals = [{{Key,Pid},Pid,New} | update_aggr_counter(Key, Incr)],
	 {reply, {Current, New}, [{insert, Vals}], S}
    catch
	error:_R ->
	    io:fwrite("reset_counter failed: ~p~n~p~n", [_R, erlang:get_stacktrace()]),
	    {reply, badarg, S}
    end;
handle_leader_call({Unreg, {T,g,Name} = K, Pid}, _From, S, _E)
  when Unreg==unreg;
       Unreg==unreg_other->
    Key = if T == n; T == a; T == rc -> {K,T};
             true -> {K, Pid}
          end,
    case ets:member(?TAB, Key) of
        true ->
            _ = gproc_lib:remove_reg(K, Pid, unreg),
            if T == c ->
                    case ets:lookup(?TAB, {{a,g,Name},a}) of
                        [Aggr] ->
                            %% updated by remove_reg/3
                            {reply, true, [{delete,[{K,Pid}, {Pid,K}]},
                                           {insert, [Aggr]}], S};
                        [] ->
                            {reply, true, [{delete, [{K,Pid}, {Pid,K}]}], S}
                    end;
               T == r ->
                    case ets:lookup(?TAB, {{rc,g,Name},rc}) of
                        [RC] ->
                            {reply, true, [{delete,[{K,Pid}, {Pid,K}]},
                                           {insert, [RC]}], S};
                        [] ->
                            {reply, true, [{delete, [{K,Pid}, {Pid, K}]}], S}
                    end;
               true ->
                    {reply, true, [{notify, [{K, Pid, unreg}]},
                                   {delete, [{K, Pid}, {Pid,K}]}], S}
            end;
        false ->
            {reply, badarg, S}
    end;
handle_leader_call({give_away, {T,g,_} = K, To, Pid}, _From, S, _E)
  when T == a; T == n; T == rc ->
    Key = {K, T},
    case ets:lookup(?TAB, Key) of
        [{_, Pid, Value}] ->
            Opts = get_opts(Pid, K),
            case pid_to_give_away_to(To) of
                Pid ->
                    {reply, Pid, S};
                ToPid when is_pid(ToPid) ->
                    ets:insert(?TAB, [{Key, ToPid, Value},
                                      {{ToPid,K}, Opts}]),
                    _ = gproc_lib:ensure_monitor(ToPid, g),
                    Rev = {Pid, K},
                    ets:delete(?TAB, Rev),
                    gproc_lib:notify({migrated, ToPid}, K, Opts),
                    {reply, ToPid, [{insert, [{Key, ToPid, Value}]},
                                    {notify, [{K, Pid, {migrated, ToPid}}]},
				    {delete, [{K, Pid}, Rev]}], S};
                undefined ->
                    ets:delete(?TAB, Key),
                    Rev = {Pid, K},
                    ets:delete(?TAB, Rev),
                    gproc_lib:notify(unreg, K, Opts),
                    {reply, undefined, [{notify, [{K, Pid, unreg}]},
                                        {delete, [{K, Pid}, Rev]}], S}
            end;
        _ ->
            {reply, badarg, S}
    end;
handle_leader_call({mreg, T, g, L, Pid}, _From, S, _E) ->
    if T==p; T==n; T==r ->
            try gproc_lib:insert_many(T, g, L, Pid) of
                {true,Objs} -> {reply, true, [{insert,Objs}], S};
                false       -> {reply, badarg, S}
            catch
                error:_     -> {reply, badarg, S}
            end;
       true -> {reply, badarg, S}
    end;
handle_leader_call({munreg, T, g, L, Pid}, _From, S, _E) ->
    try gproc_lib:remove_many(T, g, L, Pid) of
        [] ->
            {reply, true, S};
        Objs ->
            {reply, true, [{delete, Objs}], S}
    catch
        error:_ -> {reply, badarg, S}
    end;
handle_leader_call({set,{T,g,N} =K,V,Pid}, _From, S, _E) ->
    if T == a ->
            if is_integer(V) ->
                    case gproc_lib:do_set_value(K, V, Pid) of
                        true  -> {reply, true, [{insert,[{{K,T},Pid,V}]}], S};
                        false -> {reply, badarg, S}
                    end
            end;
       T == c ->
            try gproc_lib:do_set_counter_value(K, V, Pid),
                AKey = {{a,g,N},a},
                Aggr = ets:lookup(?TAB, AKey),  % may be []
                {reply, true, [{insert, [{{K,Pid},Pid,V} | Aggr]}], S}
            catch
                error:_ ->
                    {reply, badarg, S}
            end;
       true ->
            case gproc_lib:do_set_value(K, V, Pid) of
                true ->
                    Obj = if T==n -> {{K, T}, Pid, V};
                             true -> {{K, Pid}, Pid, V}
                          end,
                    {reply, true, [{insert,[Obj]}], S};
                false ->
                    {reply, badarg, S}
            end
    end;
handle_leader_call({await, Key, Pid}, {_,Ref} = From, S, _E) ->
    %% The pid in _From is of the gen_leader instance that forwarded the
    %% call - not of the client. This is why the Pid is explicitly passed.
    %% case gproc_lib:await(Key, {Pid,Ref}) of
    case gproc_lib:await(Key, Pid, From) of
        {reply, {Ref, {K, P, V}}} ->
            {reply, {Ref, {K, P, V}}, S};
        {reply, Reply, Insert} ->
            {reply, Reply, [{insert, Insert}], S}
    end;
handle_leader_call(_, _, S, _E) ->
    {reply, badarg, S}.

handle_leader_cast({initiate_sync, Ref}, S, E) ->
    case gen_leader:alive(E) -- [node()] of
        [] ->
            %% ???
            {noreply, send_sync_complete(Ref, S, E)};
        Alive ->
            gen_leader:broadcast({from_leader, {sync, Ref}}, Alive, E),
            {noreply, S#state{sync_requests =
                                  [{Ref, Alive}|S#state.sync_requests]}}
    end;

handle_leader_cast({sync_reply, Node, Ref}, S, E) ->
    #state{sync_requests = SReqs} = S,
    case lists:keyfind(Ref, 1, SReqs) of
        false ->
            %% This should never happen, except perhaps if the leader who
            %% received the sync request died, and the new leader gets the
            %% sync reply. In that case, we trust that the client has been
	    %% notified anyway, and ignore the message.
            {ok, S};
        {_, Ns} ->
            case lists:delete(Node, Ns) of
                [] ->
                    {ok, send_sync_complete(Ref, S, E)};
                Ns1 ->
                    SReqs1 = lists:keyreplace(Ref, 1, SReqs, {Ref, Ns1}),
                    {ok, S#state{sync_requests = SReqs1}}
            end
    end;
handle_leader_cast({add_globals, Missing}, S, _E) ->
    %% This is an audit message: a peer (non-leader) had info about granted
    %% global resources that we didn't know of when we became leader.
    %% This could happen due to a race condition when the old leader died.
    Update = insert_globals(Missing),
    {ok, [{insert, Update}], S};
handle_leader_cast({remove_globals, Globals}, S, _E) ->
    delete_globals(Globals),
    {ok, S};
handle_leader_cast({cancel_wait, Pid, {T,_,_} = Key, Ref}, S, _E) ->
    case ets:lookup(?TAB, {Key, T}) of
	[{_, Waiters}] ->
	    Ops = gproc_lib:remove_wait(Key, Pid, Ref, Waiters),
	    {ok, Ops, S};
	_ ->
	    {ok, [], S}
    end;
handle_leader_cast({cancel_wait_or_monitor, Pid, {T,_,_} = Key}, S, _E) ->
    case ets:lookup(?TAB, {Key, T}) of
	[{_, Waiters}] ->
	    Ops = gproc_lib:remove_wait(Key, Pid, all, Waiters),
	    {ok, Ops, S};
	[{_, OtherPid, _}] ->
	    Ops = gproc_lib:remove_monitors(Key, OtherPid, Pid),
	    {ok, Ops, S}
    end;
handle_leader_cast({pid_is_DOWN, Pid}, S, _E) ->
    Globals = ets:select(?TAB, [{{{Pid,'$1'}, '_'},
                                 [{'==',{element,2,'$1'},g}],[{{'$1',Pid}}]}]),
    ets:delete(?TAB, {Pid,g}),
    case process_globals(Globals) of
        [] ->
            {ok, S};
        Broadcast ->
            {ok, Broadcast, S}
    end.

mk_broadcast_insert_vals(Objs) ->
    lists:flatmap(
      fun({{C, g, Name} = K, Pid, Value}) ->
	      if C == a; C == rc ->
		      ets:lookup(?TAB, {K,C}) ++ ets:lookup(?TAB, {Pid,K});
		 C == c ->
		      [{{K,Pid},Pid,Value} | ets:lookup(?TAB,{{a,g,Name},a})]
			  ++ ets:lookup(?TAB, {Pid,K});
                 C == r ->
                      [{{K,Pid},Pid,Value} | ets:lookup(?TAB,{{rc,g,Name},rc})]
                          ++ ets:lookup(?TAB, {Pid, K});
		 C == n ->
		      [{{K,n},Pid,Value}| ets:lookup(?TAB, {Pid,K})];
		 true ->
		      [{{K,Pid},Pid,Value} | ets:lookup(?TAB, {Pid,K})]
	      end
      end, Objs).


process_globals(Globals) ->
    {Modified, Notifications} =
        lists:foldl(
          fun({{T,_,_} = Key, Pid}, A) when T==n; T==a; T==rc ->
                  case ets:lookup(?TAB, {Pid,Key}) of
                      [{_, Opts}] when is_list(Opts) ->
                          maybe_failover(Key, Pid, Opts, A);
                      _ ->
                          A
                  end;
             ({{T,_,_} = Key, Pid}, {MA,NA}) ->
                  MA1 = case T of
                            c ->
                                Incr = ets:lookup_element(?TAB, {Key,Pid}, 3),
                                update_aggr_counter(Key, -Incr) ++ MA;
                            r ->
                                decrement_resource_count(Key, []) ++ MA;
                            _ ->
                               MA
                        end,
                  N = remove_entry(Key, Pid, unreg),
                  {MA1, N ++ NA}
          end, {[],[]}, Globals),
    [{insert, Modified} || Modified =/= []] ++
        [{notify, Notifications} || Notifications =/= []] ++
	[{delete, Globals} || Globals =/= []].

maybe_failover({T,_,_} = Key, Pid, Opts, {MAcc, NAcc}) ->
    Opts = get_opts(Pid, Key),
    case filter_standbys(gproc_lib:standbys(Opts)) of
        [] ->
            Notify = remove_entry(Key, Pid, unreg),
            {MAcc, Notify ++ NAcc};
        [{ToPid,Ref,_}|_] ->
            Value = case ets:lookup(?TAB, {Key,T}) of
                        [{_, _, V}] -> V;
                        _ -> undefined
                    end,
            Notify = remove_rev_entry(Opts, Pid, Key, {failover, ToPid}),
            Opts1 = gproc_lib:remove_monitor(Opts, ToPid, Ref),
            _ = gproc_lib:ensure_monitor(ToPid, g),
            NewReg = {{Key,T}, ToPid, Value},
            NewRev = {{ToPid, Key}, Opts1},
            ets:insert(?TAB, [NewReg, NewRev]),
            {[NewReg, NewRev | MAcc], Notify ++ NAcc}
    end.

filter_standbys(SBs) ->
    filter_standbys(SBs, [node()|nodes()]).

filter_standbys([{Pid,_,_} = H|T], Nodes) ->
    case lists:member(node(Pid), Nodes) of
        true ->
            [H|T];
        false ->
            filter_standbys(T, Nodes)
    end;
filter_standbys([], _) ->
    [].


remove_entry(Key, Pid, Event) ->
    K = ets_key(Key, Pid),
    case ets:lookup(?TAB, K) of
	[{_, P, _}] when is_pid(P), P =:= Pid; is_atom(Pid) ->
	    ets:delete(?TAB, K),
	    remove_rev_entry(get_opts(Pid, Key), Pid, Key, Event);
	[{_, _OtherPid, _}] ->
	    ets:delete(?TAB, {Pid, Key}),
	    [];
        [{_, _Waiters}] ->
            ets:delete(?TAB, K),
            [];
	[] -> []
    end.

remove_rev_entry(Opts, Pid, {T,g,_} = K, Event) when T==n; T==a ->
    Key = {Pid, K},
    gproc_lib:notify(Event, K, Opts),
    ets:delete(?TAB, Key),
    [{K, Pid, Event}];
remove_rev_entry(_, Pid, K, _Event) ->
    ets:delete(?TAB, {Pid, K}),
    [].

get_opts(Pid, K) ->
    case ets:lookup(?TAB, {Pid, K}) of
        [] -> [];
        [{_, r}] -> [];
        [{_, Opts}] -> Opts
    end.

code_change(_FromVsn, S, _Extra, _E) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.

from_leader({sync, Ref}, S, _E) ->
    gen_leader:leader_cast(?MODULE, {sync_reply, node(), Ref}),
    {ok, S};
from_leader({sync_complete, Ref}, S, _E) ->
    case Ref of
        {From, _} when node(From) == node() ->
            {ok, reply_to_sync_client(Ref, S)};
        _ ->
            %% we shouldn't have to, but ensure that we don't have
            %% the sync request in our state.
            {ok, S#state{sync_requests = lists:keydelete(
                                           Ref, 1, S#state.sync_requests)}}
    end;
from_leader(Ops, S, _E) ->
    lists:foreach(
      fun({delete, Globals}) ->
              delete_globals(Globals);
         ({insert, Globals}) ->
	      _ = insert_globals(Globals);
         ({notify, Events}) ->
              do_notify(Events)
      end, Ops),
    {ok, S}.

insert_globals(Globals) ->
    lists:foldl(
      fun({{{_,_,_} = Key,_}, Pid, _} = Obj, A) ->
              ets:insert(?TAB, Obj),
	      ets:insert_new(?TAB, {{Pid,Key}, []}),
	      gproc_lib:ensure_monitor(Pid,g),
	      A;
         ({{{_,_,_},_}, _} = Obj, A) ->
              ets:insert(?TAB, Obj),
              A;
         ({{P,_K}, Opts} = Obj, A) when is_pid(P), is_list(Opts) ->
	      ets:insert(?TAB, Obj),
	      gproc_lib:ensure_monitor(P,g),
	      [Obj] ++ A;
	 (_Other, A) ->
	      A
      end, Globals, Globals).


delete_globals(Globals) ->
    lists:foreach(
      fun({{_,g,_} = K, T}) when is_atom(T); is_pid(T) ->
              remove_entry(K, T, []);
         ({{{_,g,_} = K, T}, P}) when is_pid(P), is_atom(T);
                                          is_pid(P), is_pid(T) ->
	      remove_entry(K, P, []);
         ({Pid, Key}) when is_pid(Pid); Pid==shared ->
	      ets:delete(?TAB, {Pid, Key})
      end, Globals).

do_notify([{P, Msg}|T]) when is_pid(P), node(P) =:= node() ->
    P ! Msg,
    do_notify(T);
do_notify([{P, _Msg}|T]) when is_pid(P) ->
    do_notify(T);
do_notify([{K, P, E}|T]) ->
    case ets:lookup(?TAB, {P,K}) of
        [{_, Opts}] when is_list(Opts) ->
            gproc_lib:notify(E, K, Opts);
        _ ->
            do_notify(T)
    end;
do_notify([]) ->
    ok.


ets_key({T,_,_} = K, _) when T==n; T==a; T==rc ->
    {K, T};
ets_key(K, Pid) ->
    {K, Pid}.

leader_call(Req) ->
    case gen_leader:leader_call(?MODULE, Req) of
        badarg -> ?THROW_GPROC_ERROR(badarg);
        Reply  -> Reply
    end.

%% leader_call(Req, Timeout) ->
%%     case gen_leader:leader_call(?MODULE, Req, Timeout) of
%%         badarg -> ?THROW_GPROC_ERROR(badarg);
%%         Reply  -> Reply
%%     end.

leader_cast(Msg) ->
    gen_leader:leader_cast(?MODULE, Msg).

init(Opts) ->
    S0 = #state{},
    AlwaysBcast = proplists:get_value(always_broadcast, Opts,
                                      S0#state.always_broadcast),
    {ok, #state{always_broadcast = AlwaysBcast}}.

surrendered_1(Globs) ->
    My_local_globs =
        ets:select(?TAB, [{{{{'_',g,'_'},'_'},'$1', '$2'},
                           [{'==', {node,'$1'}, node()}],
                           [{{ {element,1,'$_'}, '$1', '$2' }}]}]),
    _ = [gproc_lib:ensure_monitor(Pid, g) || {_, Pid, _} <- My_local_globs],
    ?event({'My_local_globs', My_local_globs}),
    %% remove all remote globals.
    ets:select_delete(?TAB, [{{{{'_',g,'_'},'_'}, '$1', '_'},
                              [{'=/=', {node,'$1'}, node()}],
                              [true]},
			     {{{'$1',{'_',g,'_'}}, '_'},
			      [{'=/=', {node,'$1'}, node()}],
			      [true]}]),
    %% insert new non-local globals, collect the leader's version of
    %% what my globals are
    Ldr_local_globs =
        lists:foldl(
          fun({{Key,_}=K, Pid, V}, Acc) when node(Pid) =/= node() ->
                  ets:insert(?TAB, {K, Pid, V}),
		  _ = gproc_lib:ensure_monitor(Pid, g),
		  ets:insert_new(?TAB, {{Pid,Key}, []}),
                  Acc;
	     ({{_Pid,_}=K, Opts}, Acc) -> % when node(Pid) =/= node() ->
		     ets:insert(?TAB, {K, Opts}),
		     Acc;
             ({_, Pid, _} = Obj, Acc) when node(Pid) == node() ->
                  [Obj|Acc]
          end, [], Globs),
    ?event({'Ldr_local_globs', Ldr_local_globs}),
    case [{K,P,V} || {K,P,V} <- My_local_globs,
		     is_pid(P) andalso
			 not(lists:keymember(K, 1, Ldr_local_globs))] of
        [] ->
            %% phew! We have the same picture
            ok;
        [_|_] = Missing ->
            %% This is very unlikely, I think
            ?event({'Missing', Missing}),
            leader_cast({add_globals, mk_broadcast_insert_vals(Missing)})
    end,
    case [{K,P} || {{K,_}=R,P,_} <- Ldr_local_globs,
		   is_pid(P) andalso
		       not(lists:keymember(R, 1, My_local_globs))] of
        [] ->
            ok;
        [_|_] = Remove ->
            ?event({'Remove', Remove}),
            leader_cast({remove_globals, Remove})
    end.

batch_update_counters(Cs) ->
    batch_update_counters(Cs, [], []).

batch_update_counters([{{c,g,_} = Key, Pid, Incr}|T], Returns, Updates) ->
    case update_counter_g(Key, Incr, Pid) of
	[{_,_,_} = A, {_, _, V} = C] ->
	    batch_update_counters(T, [{Key,Pid,V}|Returns], add_object(
							      A, add_object(C, Updates)));
	[{_, _, V} = C] ->
	    batch_update_counters(T, [{Key,Pid,V}|Returns], add_object(C, Updates))
    end;
batch_update_counters([], Returns, Updates) ->
    {lists:reverse(Returns), Updates}.


add_object({K,P,_} = Obj, [{K,P,_} | T]) ->
    [Obj | T];
add_object(Obj, [H|T]) ->
    [H | add_object(Obj, T)];
add_object(Obj, []) ->
    [Obj].



update_counter_g({c,g,_} = Key, Incr, Pid) when is_integer(Incr) ->
    Res = ets:update_counter(?TAB, {Key, Pid}, {3,Incr}),
    update_aggr_counter(Key, Incr, [{{Key,Pid},Pid,Res}]);
update_counter_g({c,g,_} = Key, {Incr, Threshold, SetValue}, Pid)
  when is_integer(Incr), is_integer(Threshold), is_integer(SetValue) ->
    [Prev, New] = ets:update_counter(?TAB, {Key, Pid},
				     [{3, 0}, {3, Incr, Threshold, SetValue}]),
    update_aggr_counter(Key, New - Prev, [{{Key,Pid},Pid,New}]);
update_counter_g({c,g,_} = Key, Ops, Pid) when is_list(Ops) ->
    case ets:update_counter(?TAB, {Key, Pid},
			    [{3, 0} | expand_ops(Ops)]) of
	[_] ->
	    [];
	[Prev | Rest] ->
	    [New | _] = lists:reverse(Rest),
	    update_aggr_counter(Key, New - Prev, [{Key, Pid, Rest}])
    end;
update_counter_g(_, _, _) ->
    ?THROW_GPROC_ERROR(badarg).


expand_ops([{Incr,Thr,SetV}|T])
  when is_integer(Incr), is_integer(Thr), is_integer(SetV) ->
    [{3, Incr, Thr, SetV}|expand_ops(T)];
expand_ops([Incr|T]) when is_integer(Incr) ->
    [{3, Incr}|expand_ops(T)];
expand_ops([]) ->
    [];
expand_ops(_) ->
    ?THROW_GPROC_ERROR(badarg).

update_aggr_counter({n,_,_}, _) ->
    [];
update_aggr_counter(Key, Incr) ->
    update_aggr_counter(Key, Incr, []).

update_aggr_counter({c,g,Ctr}, Incr, Acc) ->
    Key = {{a,g,Ctr},a},
    case ets:lookup(?TAB, Key) of
        [] ->
            Acc;
        [{K, Pid, Prev}] ->
            New = {K, Pid, Prev+Incr},
            ets:insert(?TAB, New),
            [New|Acc]
    end.

decrement_resource_count({r,g,Rsrc}, Acc) ->
    Key = {{rc,g,Rsrc},rc},
    case ets:member(?TAB, Key) of
        false ->
            Acc;
        true ->
            %% Call the lib function, which might trigger events
            gproc_lib:decrement_resource_count(g, Rsrc),
            ets:lookup(?TAB, Key) ++ Acc
    end.

pid_to_give_away_to(P) when is_pid(P) ->
    P;
pid_to_give_away_to({T,g,_} = Key) when T==n; T==a ->
    case ets:lookup(?TAB, {Key, T}) of
        [{_, Pid, _}] ->
            Pid;
        _ ->
            undefined
    end.

insert_reg([{_, Waiters}], K, Val, Pid, Event) ->
    gproc_lib:insert_reg(K, Val, Pid, g),
    tell_waiters(Waiters, K, Pid, Val, Event);
insert_reg([], K, Val, Pid, Event) ->
    gproc_lib:insert_reg(K, Val, Pid, g),
    tell_waiters([], K, Val, Pid, Event).

tell_waiters([{P,R}|T], K, Pid, V, Event) ->
    Msg = {gproc, R, registered, {K, Pid, V}},
    if node(P) == node() ->
            P ! Msg;
       true ->
            [{P, Msg} | tell_waiters(T, K, Pid, V, Event)]
    end;
tell_waiters([{P,R,follow}|T], K, Pid, V, Event) ->
    Msg = {gproc, Event, R, K},
    if node(P) == node() ->
            P ! Msg;
       true ->
            [{P, Msg} | tell_waiters(T, K, Pid, V, Event)]
    end;
tell_waiters([], _, _, _, _) ->
    [].

add_follow_to_waiters(Waiters, {T,_,_} = K, Pid, Ref, S) ->
    Obj = {{K,T}, [{Pid, Ref, follow}|Waiters]},
    ets:insert(?TAB, Obj),
    Rev = ensure_rev({Pid, K}),
    Msg = {gproc, unreg, Ref, K},
    if node(Pid) =:= node() ->
            Pid ! Msg,
            {reply, Ref, [{insert, [Obj, Rev]}], S};
       true ->
            {reply, Ref, [{insert, [Obj, Rev]},
                          {notify, [{Pid, Msg}]}], S}
    end.

ensure_rev(K) ->
    case ets:lookup(?TAB, K) of
        [Rev] ->
            Rev;
        [] ->
            Rev = {K, []},
            ets:insert(?TAB, Rev),
            Rev
    end.

regged_new(reg   ) -> true;
regged_new(ensure) -> new.


initiate_sync(From, #state{is_leader = true} = S, E) ->
    case gen_leader:alive(E) -- [node()] of
        [] ->
            %% I'm alone - sync is trivial
            gen_server:reply(From, true),
            S;
        Alive ->
            gen_leader:broadcast(
              {from_leader, {sync, From}}, Alive, E),
            S#state{sync_requests =
                        [{From, Alive}|S#state.sync_requests]}
    end;
initiate_sync(From, S, _E) ->
    leader_cast({initiate_sync, From}),
    S.

maybe_reinitiate_sync(#state{sync_clients = []} = S, _E) ->
    S;
maybe_reinitiate_sync(#state{sync_clients = Cs} = S, E) ->
    lists:foldl(
      fun(From, Sx) ->
              initiate_sync(From, Sx, E)
      end, S, Cs).

send_sync_complete({From, _} = Ref, S, _E) when node(From) == node() ->
    reply_to_sync_client(Ref, S);
send_sync_complete({From, _} = Ref, S, E) ->
    %% Notify the node that initiated the sync
    %% 'broadcasting' to exactly one node.
    gen_leader:broadcast(
      {from_leader, {sync_complete, Ref}}, [node(From)], E),
    S#state{sync_requests =
                lists:keydelete(Ref, 1, S#state.sync_requests)}.

reply_to_sync_client(Ref, S) ->
    gen_server:reply(Ref, true),
    S#state{sync_clients =
                S#state.sync_clients -- [Ref],
            sync_requests =
                lists:keydelete(Ref, 1, S#state.sync_requests)}.
