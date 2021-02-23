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
%% @author Ulf Wiger <ulf.wiger@feuerlabs.com>
%%
%% @doc
%% This module implements a notification system for gproc names
%% When a process subscribes to notifications for a given name, a message
%% will be sent each time that name is registered
-module(gproc_monitor).

-behaviour(gen_server).

%% API
-export([subscribe/1,
	 unsubscribe/1]).

%% Process start function
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec subscribe(key()) -> ok
%%
%% @doc
%% Subscribe to registration events for a certain name
%%
%% The subscribing process will receive a `{gproc_monitor, Name, Pid}' message
%% whenever a process registers under the given name, and a
%% `{gproc_monitor, Name, undefined}' message when the name is unregistered,
%% either explicitly, or because the registered process dies.
%%
%% When the subscription is first ordered, one of the above messages will be
%% sent immediately, indicating the current status of the name.
%% @end
%%--------------------------------------------------------------------
subscribe({T,S,_} = Key) when (T==n orelse T==a)
			      andalso (S==g orelse S==l) ->
    try gproc:reg({p,l,{?MODULE,Key}})
    catch
	error:badarg -> ok
    end,
    gen_server:cast(?SERVER, {subscribe, self(), Key}).


%%--------------------------------------------------------------------
%% @spec unsubscribe(key()) -> ok
%%
%% @doc
%% Unsubscribe from registration events for a certain name
%%
%% This function is the reverse of subscribe/1. It removes the subscription.
%% @end
%%--------------------------------------------------------------------
unsubscribe({T,S,_} = Key) when (T==n orelse T==a)
				andalso (S==g orelse S==l) ->
    try gproc:unreg({p, l, {?MODULE,Key}})
    catch
	error:badarg -> ok
    end,
    gen_server:cast(?SERVER, {unsubscribe, self(), Key}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Me = self(),
    _ = case ets:info(?TAB, owner) of
	    undefined ->
		ets:new(?TAB, [ordered_set, protected, named_table,
			       {heir, self(), []}]);
	    Me ->
		ok
	end,
    {ok, Pid} = proc_lib:start_link(?MODULE, init, [Me]),
    ets:give_away(?TAB, Pid, []),
    {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Parent) ->
    process_flag(priority, high),
    register(?SERVER, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    receive {'ETS-TRANSFER',?TAB,_,_} -> ok end,
    gen_server:enter_loop(?MODULE, [], #state{}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({subscribe, Pid, Key}, State) ->
    Status = gproc:where(Key),
    add_subscription(Pid, Key),
    do_monitor(Key, Status),
    Pid ! {?MODULE, Key, Status},
    monitor_pid(Pid),
    {noreply, State};
handle_cast({unsubscribe, Pid, Key}, State) ->
    del_subscription(Pid, Key),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({gproc, unreg, _Ref, Name}, State) ->
    ets:delete(?TAB, {m, Name}),
    do_monitor(Name, undefined),
    notify(Name, undefined),
    {noreply, State};
handle_info({gproc, {migrated,ToPid}, _Ref, Name}, State) ->
    ets:delete(?TAB, {m, Name}),
    do_monitor(Name, ToPid),
    notify(Name, {migrated, ToPid}),
    {noreply, State};
handle_info({gproc, {failover,ToPid}, _Ref, Name}, State) ->
    ets:delete(?TAB, {m, Name}),
    do_monitor(Name, ToPid),
    notify(Name, {failover, ToPid}),
    {noreply, State};
handle_info({gproc, _, registered, {{T,_,_} = Name, Pid, _}}, State)
  when T==n; T==a ->
    ets:delete(?TAB, {w, Name}),
    do_monitor(Name, Pid),
    notify(Name, Pid),
    {noreply, State};
handle_info({'DOWN', _, process, Pid, _}, State) ->
    pid_is_down(Pid),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_subscription(Pid, {_,_,_} = Key) when is_pid(Pid) ->
    ets:insert(?TAB, [{{s, Key, Pid}}, {{r, Pid, Key}}]).

del_subscription(Pid, Key) ->
    ets:delete(?TAB, {{s, Key, Pid}}),
    ets:delete(?TAB, {{r, Pid, Key}}),
    maybe_cancel_wait(Key).

do_monitor(Name, undefined) ->
    case ets:member(?TAB, {w, Name}) of
	false ->
	    Ref = gproc:nb_wait(Name),
	    ets:insert(?TAB, {{w, Name}, Ref});
        true ->
            ok
    end;
do_monitor(Name, Pid) when is_pid(Pid) ->
    case ets:member(?TAB, {m, Name}) of
	true ->
	    ok;
	_ ->
	    Ref = gproc:monitor(Name),
	    ets:insert(?TAB, {{m, Name}, Ref})
    end.

monitor_pid(Pid) when is_pid(Pid) ->
    case ets:member(?TAB, {p,Pid}) of
	false ->
	    Ref = erlang:monitor(process, Pid),
	    ets:insert(?TAB, {{p,Pid}, Ref});
	true ->
	    ok
    end.

pid_is_down(Pid) ->
    Keys = ets:select(?TAB, [{ {{r, Pid, '$1'}}, [], ['$1'] }]),
    ets:select_delete(?TAB, [{ {{r, Pid, '$1'}}, [], [true] }]),
    lists:foreach(fun(K) ->
			  ets:delete(?TAB, {s,K,Pid}),
			  maybe_cancel_wait(K)
		  end, Keys),
    ets:delete(?TAB, {p, Pid}).

maybe_cancel_wait(K) ->
    case ets:next(?TAB, {s,K}) of
	{s,K,P} when is_pid(P) ->
	    ok;
	_ ->
	    gproc:cancel_wait_or_monitor(K),
	    ets:delete(?TAB, {m, K}),
	    ets:delete(?TAB, {w, K})
    end.

notify(Name, Where) ->
    gproc:send({p, l, {?MODULE, Name}}, {?MODULE, Name, Where}).
