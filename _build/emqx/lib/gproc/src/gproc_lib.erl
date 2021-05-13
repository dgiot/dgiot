%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
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
-module(gproc_lib).

-export([await/3,
         do_set_counter_value/3,
         do_set_value/3,
         ensure_monitor/2,
         insert_many/4,
         insert_reg/4, insert_reg/5,
	 insert_attr/4,
         remove_many/4,
         remove_reg/3, remove_reg/4,
         monitors/1,
         standbys/1,
         followers/1,
         remove_monitor_pid/2,
         does_pid_monitor/2,
	 add_monitor/4,
	 remove_monitor/3,
	 remove_monitors/3,
	 remove_reverse_mapping/3, remove_reverse_mapping/4,
	 notify/2, notify/3,
	 remove_wait/4,
         update_aggr_counter/3,
         update_counter/3,
         decrement_resource_count/2,
	 valid_opts/2]).

-export([dbg/1]).

-include("gproc_int.hrl").
-include("gproc.hrl").

dbg(Mods) ->
    dbg:tracer(),
    [dbg:tpl(M,x) || M <- Mods],
    dbg:tp(ets,'_',[{[gproc,'_'], [], [{message,{exception_trace}}]}]),
    dbg:p(all,[c]).

%% We want to store names and aggregated counters with the same
%% structure as properties, but at the same time, we must ensure
%% that the key is unique. We replace the Pid in the key part
%% with an atom. To know which Pid owns the object, we lug the
%% Pid around as payload as well. This is a bit redundant, but
%% symmetric.
%%
-spec insert_reg(gproc:key(), any(), pid() | shared, gproc:scope()) -> boolean().
insert_reg(K, Value, Pid, Scope) ->
    insert_reg(K, Value, Pid, Scope, registered).

insert_reg({T,_,Name} = K, Value, Pid, Scope, Event) when T==a; T==n; T==rc ->
    Res = case ets:insert_new(?TAB, {{K,T}, Pid, Value}) of
              true ->
                  %% Use insert_new to avoid overwriting existing entry
                  _ = ets:insert_new(?TAB, {{Pid,K}, []}),
                  true;
              false ->
                  maybe_waiters(K, Pid, Value, T, Event)
          end,
    maybe_scan(T, Pid, Scope, Name, K),
    Res;
insert_reg({p,Scope,_} = K, Value, shared, Scope, _E)
  when Scope == g; Scope == l ->
    %% shared properties are unique
    Info = [{{K, shared}, shared, Value}, {{shared,K}, []}],
    ets:insert_new(?TAB, Info);
insert_reg({c,Scope,Ctr} = Key, Value, Pid, Scope, _E) when Scope==l; Scope==g ->
    %% Non-unique keys; store Pid in the key part
    K = {Key, Pid},
    Kr = {Pid, Key},
    Res = ets:insert_new(?TAB, [{K, Pid, Value}, {Kr, [{initial, Value}]}]),
    case Res of
        true ->
            update_aggr_counter(Scope, Ctr, Value);
        false ->
            ignore
    end,
    Res;
insert_reg({r,Scope,R} = Key, Value, Pid, Scope, _E) when Scope==l; Scope==g ->
    K = {Key, Pid},
    Kr = {Pid, Key},
    Res = ets:insert_new(?TAB, [{K, Pid, Value}, {Kr, [{initial, Value}]}]),
    case Res of
        true ->
            update_resource_count(Scope, R, 1);
        false ->
            ignore
    end,
    Res;
insert_reg({_,_,_} = Key, Value, Pid, _Scope, _E) when is_pid(Pid) ->
    %% Non-unique keys; store Pid in the key part
    K = {Key, Pid},
    Kr = {Pid, Key},
    ets:insert_new(?TAB, [{K, Pid, Value}, {Kr, []}]).

maybe_scan(a, Pid, Scope, Name, K) ->
    Initial = scan_existing_counters(Scope, Name),
    ets:insert(?TAB, {{K,a}, Pid, Initial});
maybe_scan(rc, Pid, Scope, Name, K) ->
    Initial = scan_existing_resources(Scope, Name),
    ets:insert(?TAB, {{K,rc}, Pid, Initial});
maybe_scan(_, _, _, _, _) ->
    true.

insert_attr({_,Scope,_} = Key, Attrs, Pid, Scope) when Scope==l;
						       Scope==g ->
    case ets:lookup(?TAB,  K = {Pid, Key}) of
	[{_, Attrs0}] when is_list(Attrs) ->
	    As = proplists:get_value(attrs, Attrs0, []),
	    As1 = lists:foldl(fun({K1,_} = Attr, Acc) ->
				     lists:keystore(K1, 1, Acc, Attr)
			     end, As, Attrs),
	    Attrs1 = lists:keystore(attrs, 1, Attrs0, {attrs, As1}),
	    ets:insert(?TAB, {K, Attrs1}),
	    Attrs1;
	_ ->
	    false
    end.

get_attr(Attr, Pid, {_,_,_} = Key, Default) ->
    case ets:lookup(?TAB, {Pid, Key}) of
        [{_, Opts}] when is_list(Opts) ->
            case lists:keyfind(attrs, 1, Opts) of
                {_, Attrs} ->
                    case lists:keyfind(Attr, 1, Attrs) of
                        {_, Val} ->
                            Val;
                        _ ->
                            Default
                    end;
                _ ->
                    Default
            end;
        _ ->
            Default
    end.

-spec insert_many(gproc:type(), gproc:scope(), [{gproc:key(),any()}], pid()) ->
          {true,list()} | false.

insert_many(T, Scope, KVL, Pid) ->
    Objs = mk_reg_objs(T, Scope, Pid, KVL),
    case ets:insert_new(?TAB, Objs) of
        true ->
            RevObjs = mk_reg_rev_objs(T, Scope, Pid, KVL),
            ets:insert(?TAB, RevObjs),
            _ = gproc_lib:ensure_monitor(Pid, Scope),
            {true, Objs};
        false ->
            Existing = [{Obj, ets:lookup(?TAB, K)} || {K,_,_} = Obj <- Objs],
            case lists:any(fun({_, [{_, _, _}]}) ->
                                   true;
                              (_) ->
                                   %% (not found), or waiters registered
                                   false
                           end, Existing) of
                true ->
                    %% conflict; return 'false', indicating failure
                    false;
                false ->
                    %% possibly waiters, but they are handled in next step
                    insert_objects(Existing),
                    _ = gproc_lib:ensure_monitor(Pid, Scope),
                    {true, Objs}
            end
    end.

-spec insert_objects([{gproc:key(), pid(), any()}]) -> ok.

insert_objects(Objs) ->
    lists:foreach(
      fun({{{Id,_} = _K, Pid, V} = Obj, Existing}) ->
              ets:insert(?TAB, [Obj, {{Pid, Id}, []}]),
              case Existing of
                  [] -> ok;
                  [{_, Waiters}] ->
                      notify_waiters(Waiters, Id, Pid, V, registered)
              end
      end, Objs).


await({T,C,_} = Key, WPid, {_Pid, Ref} = From) ->
    Rev = {{WPid,Key}, []},
    case ets:lookup(?TAB, {Key,T}) of
        [{_, P, Value}] ->
            %% for symmetry, we always reply with Ref and then send a message
            if C == g ->
                    %% in the global case, we bundle the reply, since otherwise
                    %% the messages can pass each other
                    {reply, {Ref, {Key, P, Value}}};
               true ->
                    gen_server:reply(From, Ref),
                    WPid ! {gproc, Ref, registered, {Key, P, Value}},
                    noreply
            end;
        [{K, Waiters}] ->
            NewWaiters = [{WPid,Ref} | Waiters],
            W = {K, NewWaiters},
            ets:insert(?TAB, [W, Rev]),
            _ = gproc_lib:ensure_monitor(WPid,C),
            {reply, Ref, [W,Rev]};
        [] ->
            W = {{Key,T}, [{WPid,Ref}]},
            ets:insert(?TAB, [W, Rev]),
            _ = gproc_lib:ensure_monitor(WPid,C),
            {reply, Ref, [W,Rev]}
    end.

maybe_waiters(_, _, _, _, []) ->
    false;
maybe_waiters(K, Pid, Value, T, Event) ->
    case ets:lookup(?TAB, {K,T}) of
        [{_, Waiters}] when is_list(Waiters) ->
            Followers = [F || {_,_,follow} = F <- Waiters],
            ets:insert(?TAB, [{{K,T}, Pid, Value},
                              {{Pid,K}, [{monitor, Followers}
                                         || Followers =/= []]}]),
            notify_waiters(Waiters, K, Pid, Value, Event),
            true;
        _ ->
            false
    end.

-spec notify_waiters([{pid(), reference()}], gproc:key(), pid(), any(), any()) -> ok.
notify_waiters([{P, Ref}|T], K, Pid, V, E) ->
    P ! {gproc, Ref, registered, {K, Pid, V}},
    notify_waiters(T, K, Pid, V, E);
notify_waiters([{P, Ref, follow}|T], K, Pid, V, E) ->
    %% This is really a monitor, lurking in the Waiters list
    P ! {gproc, E, Ref, K},
    notify_waiters(T, K, Pid, V, E);
notify_waiters([], _, _, _, _) ->
    ok.

remove_wait({T,_,_} = Key, Pid, Ref, Waiters) ->
    Rev = {Pid,Key},
    case remove_from_waiters(Waiters, Pid, Ref) of
	[] ->
	    ets:delete(?TAB, {Key,T}),
	    ets:delete(?TAB, Rev),
	    [{delete, [{Key,T}, Rev], []}];
	NewWaiters ->
	    ets:insert(?TAB, {Key, NewWaiters}),
	    case lists:keymember(Pid, 1, NewWaiters) of
		true ->
		    %% should be extremely unlikely
		    [{insert, [{Key, NewWaiters}]}];
		false ->
		    %% delete the reverse entry
		    ets:delete(?TAB, Rev),
		    [{insert, [{Key, NewWaiters}]},
		     {delete, [Rev], []}]
	    end
    end.

remove_from_waiters(Waiters, Pid, all) ->
    [W || W <- Waiters,
	      element(1,W) =/= Pid];
remove_from_waiters(Waiters, Pid, Ref) ->
    [W || W <- Waiters, not is_waiter(W, Pid, Ref)].

is_waiter({Pid, Ref}   , Pid, Ref) -> true;
is_waiter({Pid, Ref, _}, Pid, Ref) -> true;
is_waiter(_, _, _) ->
    false.

remove_monitors(Key, Pid, MPid) ->
    case ets:lookup(?TAB, {Pid, Key}) of
	[{_, r}] ->
	    [];
	[{K, Opts}] when is_list(Opts) ->
	    case lists:keyfind(monitors, 1, Opts) of
		false ->
		    [];
		{_, Ms} ->
		    Ms1 = [{P,R} || {P,R} <- Ms,
				    P =/= MPid],
		    NewMs = lists:keyreplace(monitors, 1, Opts, {monitors,Ms1}),
		    ets:insert(?TAB, {K, NewMs}),
		    [{insert, [{{Pid,Key}, NewMs}]}]
	    end;
	_ ->
	    []
    end.

does_pid_monitor(Pid, Opts) ->
    case lists:keyfind(monitors, 1, Opts) of
        false ->
            false;
        {_, Ms} ->
            lists:keymember(Pid, 1, Ms)
    end.

mk_reg_objs(T, Scope, Pid, L) when T==n; T==a; T==rc ->
    lists:map(fun({K,V}) ->
                      {{{T,Scope,K},T}, Pid, V};
                 (_) ->
                      erlang:error(badarg)
              end, L);
mk_reg_objs(p = T, Scope, Pid, L) ->
    lists:map(fun({K,V}) ->
                      {{{T,Scope,K},Pid}, Pid, V};
                 (_) ->
                      erlang:error(badarg)
              end, L).

mk_reg_rev_objs(T, Scope, Pid, L) ->
    [{{Pid,{T,Scope,K}}, []} || {K,_} <- L].


ensure_monitor(shared, _) ->
    ok;
ensure_monitor(Pid, _) when Pid == self() ->
    %% monitoring is ensured through a 'monitor_me' message
    ok;
ensure_monitor(Pid, Scope) when Scope==g; Scope==l ->
    case ets:insert_new(?TAB, {{Pid, Scope}}) of
        false -> ok;
        true  -> erlang:monitor(process, Pid)
    end.

remove_reg(Key, Pid, Event) ->
    Reg = remove_reg_1(Key, Pid),
    Rev = remove_reverse_mapping(Event, Pid, Key),
    [Reg, Rev].

remove_reg(Key, Pid, Event, Opts) ->
    Reg = remove_reg_1(Key, Pid),
    Rev = remove_reverse_mapping(Event, Pid, Key, Opts),
    [Reg, Rev].

remove_reverse_mapping(Event, Pid, Key) ->
    Opts = case ets:lookup(?TAB, {Pid, Key}) of
	       [] ->       [];
	       [{_, r}] -> [];
	       [{_, L}] when is_list(L) ->
		   L
	   end,
    remove_reverse_mapping(Event, Pid, Key, Opts).

remove_reverse_mapping(Event, Pid, Key, Opts) when Event==unreg;
						   element(1,Event)==migrated;
                                                   element(1,Event)==failover ->
    Rev = {Pid, Key},
    _ = notify(Event, Key, Opts),
    ets:delete(?TAB, Rev),
    Rev.

notify(Key, Opts) ->
    notify(unreg, Key, Opts).

monitors(Opts) ->
    case lists:keyfind(monitor, 1, Opts) of
	false ->
	    [];
	{_, Mons} ->
            Mons
    end.

standbys(Opts) ->
    select_monitors(monitors(Opts), standby, []).

followers(Opts) ->
    select_monitors(monitors(Opts), follow, []).

select_monitors([{_,_,Type}=H|T], Type, Acc) ->
    select_monitors(T, Type, [H|Acc]);
select_monitors([_|T], Type, Acc) ->
    select_monitors(T, Type, Acc);
select_monitors([], _, Acc) ->
    Acc.

remove_monitor_pid([{monitor, Mons}|T], Pid) ->
    [{monitors, [M || M <- Mons,
                      element(1, M) =/= Pid]}|T];
remove_monitor_pid([H|T], Pid) ->
    [H | remove_monitor_pid(T, Pid)];
remove_monitor_pid([], _) ->
    [].


notify([], _, _) ->
    ok;
notify(Event, Key, Opts) ->
    notify_(monitors(Opts), Event, Key).

%% Also handle old-style monitors
notify_([{Pid,Ref}|T], Event, Key) ->
    Pid ! {gproc, Event, Ref, Key},
    notify_(T, Event, Key);
notify_([{Pid,Ref,_}|T], Event, {_,l,_} = Key) ->
    Pid ! {gproc, Event, Ref, Key},
    notify_(T, Event, Key);
notify_([{Pid,Ref,_}|T], Event, {_,g,_} = Key) when node(Pid) == node() ->
    Pid ! {gproc, Event, Ref, Key},
    notify_(T, Event, Key);
notify_([_|T], Event, Key) ->
    notify_(T, Event, Key);
notify_([], _, _) ->
    ok.



add_monitor([{monitor, Mons}|T], Pid, Ref, Type) ->
    [{monitor, [{Pid,Ref,Type}|Mons]}|T];
add_monitor([H|T], Pid, Ref, Type) ->
    [H|add_monitor(T, Pid, Ref, Type)];
add_monitor([], Pid, Ref, Type) ->
    [{monitor, [{Pid, Ref, Type}]}].

remove_monitor([{monitor, Mons}|T], Pid, Ref) ->
    [{monitor, [Mon || Mon <- Mons, not is_mon(Mon,Pid,Ref)]} | T];
remove_monitor([H|T], Pid, Ref) ->
    [H|remove_monitor(T, Pid, Ref)];
remove_monitor([], _Pid, _Ref) ->
    [].

is_mon({Pid,Ref,_}, Pid, Ref) -> true;
is_mon({Pid,Ref},   Pid, Ref) -> true;
is_mon(_, _, _) ->
    false.

remove_many(T, Scope, L, Pid) ->
    lists:flatmap(fun(K) ->
                          Key = {T, Scope, K},
                          remove_reg(Key, Pid, unreg, unreg_opts(Key, Pid))
                  end, L).

unreg_opts(Key, Pid) ->
    case ets:lookup(?TAB, {Pid, Key}) of
	[] ->
	    [];
	[{_,r}] ->
	    [];
	[{_,Opts}] ->
	    Opts
    end.

remove_reg_1({c,_,_} = Key, Pid) ->
    remove_counter_1(Key, ets:lookup_element(?TAB, Reg = {Key,Pid}, 3), Pid),
    Reg;
remove_reg_1({r,_,_} = Key, Pid) ->
    remove_resource_1(Key, ets:lookup_element(?TAB, Reg = {Key,Pid}, 3), Pid),
    Reg;
remove_reg_1({T,_,_} = Key, _Pid) when T==a; T==n; T==rc ->
    ets:delete(?TAB, Reg = {Key,T}),
    Reg;
remove_reg_1({_,_,_} = Key, Pid) ->
    ets:delete(?TAB, Reg = {Key, Pid}),
    Reg.

remove_counter_1({c,C,N} = Key, Val, Pid) ->
    Res = ets:delete(?TAB, {Key, Pid}),
    update_aggr_counter(C, N, -Val),
    Res.

remove_resource_1({r,C,N} = Key, _, Pid) ->
    Res = ets:delete(?TAB, {Key, Pid}),
    update_resource_count(C, N, -1),
    Res.

do_set_value({T,_,_} = Key, Value, Pid) ->
    K2 = if Pid == shared -> shared;
	    T==n orelse T==a orelse T==rc -> T;
	    true -> Pid
         end,
    try ets:lookup_element(?TAB, {Key,K2}, 2) of
        Pid ->
            ets:insert(?TAB, {{Key, K2}, Pid, Value});
        _ ->
            false
    catch
        error:_ -> false
    end.

do_set_counter_value({_,C,N} = Key, Value, Pid) ->
    OldVal = ets:lookup_element(?TAB, {Key, Pid}, 3), % may fail with badarg
    Res = ets:insert(?TAB, {{Key, Pid}, Pid, Value}),
    update_aggr_counter(C, N, Value - OldVal),
    Res.

update_counter({T,l,Ctr} = Key, Incr, Pid) when is_integer(Incr), T==c;
						is_integer(Incr), T==n ->
    Res = ets:update_counter(?TAB, {Key, Pid}, {3,Incr}),
    if T==c ->
	    update_aggr_counter(l, Ctr, Incr);
       true ->
	    ok
    end,
    Res;
update_counter({T,l,Ctr} = Key, {Incr, Threshold, SetValue}, Pid)
  when is_integer(Incr), is_integer(Threshold), is_integer(SetValue), T==c;
       is_integer(Incr), is_integer(Threshold), is_integer(SetValue), T==n ->
    [Prev, New] = ets:update_counter(?TAB, {Key, Pid},
				     [{3, 0}, {3, Incr, Threshold, SetValue}]),
    if T==c ->
	    update_aggr_counter(l, Ctr, New - Prev);
       true ->
	    ok
    end,
    New;
update_counter({T,l,Ctr} = Key, Ops, Pid) when is_list(Ops), T==c;
                                               is_list(Ops), T==r;
					       is_list(Ops), T==n ->
    case ets:update_counter(?TAB, {Key, Pid},
			    [{3, 0} | expand_ops(Ops)]) of
	[_] ->
	    [];
	[Prev | Rest] ->
	    [New | _] = lists:reverse(Rest),
	    if T==c ->
		    update_aggr_counter(l, Ctr, New - Prev);
	       true ->
		    ok
	    end,
	    Rest
    end;
update_counter(_, _, _) ->
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

update_aggr_counter(C, N, Val) ->
    ?MAY_FAIL(ets:update_counter(?TAB, {{a,C,N},a}, {3, Val})).

decrement_resource_count(C, N) ->
    update_resource_count(C, N, -1).

update_resource_count(C, N, Val) ->
    try ets:update_counter(?TAB, {{rc,C,N},rc}, {3, Val}) of
        0 ->
            resource_count_zero(C, N);
        _ ->
            ok
    catch
        _:_ -> ok
    end.

resource_count_zero(C, N) ->
    case ets:lookup(?TAB, {K = {rc,C,N},rc}) of
        [{_, Pid, _}] ->
            case get_attr(on_zero, Pid, K, undefined) of
                undefined -> ok;
                Actions ->
                    perform_on_zero(Actions, C, N, Pid)
            end;
        _ -> ok
    end.

perform_on_zero(Actions, C, N, Pid) ->
    lists:foreach(
      fun(A) ->
              try perform_on_zero_(A, C, N, Pid)
              catch error:_ -> ignore
              end
      end, Actions).

perform_on_zero_({send, ToProc}, C, N, Pid) ->
    gproc:send(ToProc, {gproc, resource_on_zero, C, N, Pid}),
    ok;
perform_on_zero_({bcast, ToProc}, C, N, Pid) ->
    gproc:bcast(ToProc, {gproc, resource_on_zero, C, N, Pid}),
    ok;
perform_on_zero_(publish, C, N, Pid) ->
    gproc_ps:publish(C, gproc_resource_on_zero, {C, N, Pid}),
    ok;
perform_on_zero_({unreg_shared, T,N}, C, _, _) ->
    K = {T, C, N},
    case ets:member(?TAB, {K, shared}) of
        true ->
            Objs = remove_reg(K, shared, unreg),
            _ = if C == g -> self() ! {gproc_unreg, Objs};
                   true   -> ok
                end,
            ok;
        false ->
            ok
    end;
perform_on_zero_(_, _, _, _) ->
    ok.

scan_existing_counters(Ctxt, Name) ->
    Head = {{{c,Ctxt,Name},'_'},'_','$1'},
    Cs = ets:select(?TAB, [{Head, [], ['$1']}]),
    lists:sum(Cs).

scan_existing_resources(Ctxt, Name) ->
    Head = {{{r,Ctxt,Name},'_'},'_','_'},
    ets:select_count(?TAB, [{Head, [], [true]}]).

valid_opts(Type, Default) ->
    Opts = get_app_env(Type, Default),
    check_opts(Type, Opts).

check_opts(Type, Opts) when is_list(Opts) ->
    Check = check_option_f(Type),
    lists:map(fun(X) ->
		      case Check(X) of
			  true -> X;
			  false ->
			      erlang:error({illegal_option, X}, [Type, Opts])
		      end
	      end, Opts);
check_opts(Type, Other) ->
    erlang:error(invalid_options, [Type, Other]).

check_option_f(ets_options)    -> fun check_ets_option/1;
check_option_f(server_options) -> fun check_server_option/1.

check_ets_option({read_concurrency , B}) -> is_boolean(B);
check_ets_option({write_concurrency, B}) -> is_boolean(B);
check_ets_option(_) -> false.

check_server_option({priority, P}) ->
    %% Forbid setting priority to 'low' since that would
    %% surely cause problems. Unsure about 'max'...
    lists:member(P, [normal, high, max]);
check_server_option(_) ->
    %% assume it's a valid spawn option
    true.

get_app_env(Key, Default) ->
    case application:get_env(Key) of
	undefined       -> Default;
	{ok, undefined} -> Default;
	{ok, Value}     -> Value
    end.
