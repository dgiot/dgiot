%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% @author Ulf Wiger <ulf.wiger@ericsson.com>
%% @author Thomas Arts <thomas.arts@ituniv.se>
%% 
%% @doc Leader election behaviour.
%% <p>This application implements a leader election behaviour modeled after
%% gen_server. This behaviour intends to make it reasonably
%% straightforward to implement a fully distributed server with
%% master-slave semantics.</p>
%% <p>The gen_leader behaviour supports nearly everything that gen_server
%% does (some functions, such as multicall() and the internal timeout,
%% have been removed), and adds a few callbacks and API functions to 
%% support leader election etc.</p>
%% <p>Also included is an example program, a global dictionary, based
%% on the modules gen_leader and dict. The callback implementing the
%% global dictionary is called 'test_cb', for no particularly logical
%% reason.</p>
%% @end
%%
%% @type election() = tuple(). Opaque state of the gen_leader behaviour.
%% @type node() = atom(). A node name.
%% @type name() = atom(). A locally registered name.
%% @type serverRef() = Name | {name(),node()} | {global,Name} | pid(). 
%%   See gen_server.
%% @type callerRef() = {pid(), reference()}. See gen_server.
%%
-module(gen_leader).


-export([start/4, start/6,
	 start_link/4, start_link/6,
	 leader_call/2, leader_call/3, leader_cast/2,
	 call/2, call/3, cast/2,
	 reply/2]).

%% Query functions
-export([alive/1,
	 down/1,
	 candidates/1,
	 workers/1]).

-export([
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 format_status/2
	]).

-export([behaviour_info/1]).

%% Internal exports
-export([init_it/6, print_event/3
	 %%, safe_send/2
	]).

-import(error_logger , [format/2]).
-import(lists, [foldl/3,
		foreach/2,
		member/2,
		keydelete/3,
		keysearch/3,
		keymember/3]).


-record(election,{leader = none,
		  mode = global,
		  name,
		  leadernode = none,
		  candidate_nodes = [],	
		  worker_nodes = [],
		  alive = [],
		  iteration,
		  down = [],
		  monitored = [],
		  buffered = []
		 }).

-record(server, {parent,
		 mod,
		 state,
		 debug}).

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

%% @hidden
behaviour_info(callbacks) ->
    [{init,1},
     {elected,2},
     {surrendered,3},
     {handle_leader_call,4},
     {handle_leader_cast,3},
     {handle_local_only, 4},
     {from_leader,3},
     {handle_call,3},
     {handle_cast,2},
     {handle_DOWN,3},
     {handle_info,2},
     {terminate,2},
     {code_change,4}];
behaviour_info(_Other) ->
    undefined.

start(Name, Mod, Arg, Options) when is_atom(Name) ->
    gen:start(?MODULE, nolink, {local,Name},
	      Mod, {local_only, Arg}, Options).

%% @spec start(Name::node(), CandidateNodes::[node()],
%%             Workers::[node()], Mod::atom(), Arg, Options::list()) ->
%%    {ok,pid()}
%%
%% @doc Starts a gen_leader process without linking to the parent.
%%
start(Name, [_|_] = CandidateNodes, Workers, Mod, Arg, Options)
  when is_atom(Name) ->
    gen:start(?MODULE, nolink, {local,Name},
	      Mod, {CandidateNodes, Workers, Arg}, Options).

%% @spec start_link(Name::atom(), CandidateNodes::[atom()],
%%             Workers::[atom()], Mod::atom(), Arg, Options::list()) ->
%%  {ok, pid()}
%%
%% @doc Starts a gen_leader process.
%% <table>
%%  <tr><td>Name</td><td>The locally registered name of the process</td></tr>
%%  <tr><td>CandidateNodes</td><td>The names of nodes capable of assuming
%%     a leadership role</td></tr>
%%  <tr><td>Workers</td>
%%     <td>The names of nodes that will be part of the "cluster",
%%         but cannot ever assume a leadership role.</td></tr>
%%  <tr><td>Mod</td><td>The name of the callback module</td></tr>
%%  <tr><td>Arg</td><td>Argument passed on to <code>Mod:init/1</code></td></tr>
%%  <tr><td>Options</td><td>Same as gen_server's Options</td></tr>
%% </table>
%%
%% <p>The list of candidates needs to be known from the start. Workers 
%% can be added at runtime.</p>
%% @end
start_link(Name, [_|_] = CandidateNodes, Workers, 
	   Mod, Arg, Options) when is_atom(Name) ->
    gen:start(?MODULE, link, {local,Name}, Mod,
	      {CandidateNodes, Workers, Arg}, Options).

start_link(Name, Mod, Arg, Options) when is_atom(Name) ->
    gen:start(?MODULE, link, {local,Name}, Mod,
	      {local_only, Arg}, Options).

%% Query functions to be used from the callback module

%% @spec alive(E::election()) -> [node()]
%%
%% @doc Returns a list of live nodes (candidates and workers).
%%
alive(#election{alive = Alive}) ->
    Alive.

%% @spec down(E::election()) -> [node()]
%%
%% @doc Returns a list of candidates currently not running.
%%
down(#election{down = Down}) ->
    Down.

%% @spec candidates(E::election()) -> [node()]
%%
%% @doc Returns a list of known candidates.
%%
candidates(#election{candidate_nodes = Cands}) ->
    Cands.

%% @spec workers(E::election()) -> [node()]
%%
%% @doc Returns a list of known workers.
%%
workers(#election{worker_nodes = Workers}) ->
    Workers.

%% @spec call(Name::serverRef(), Request) -> term()
%%
%% @doc Equivalent to <code>gen_server:call/2</code>, but with a slightly
%% different exit reason if something goes wrong. This function calls 
%% the <code>gen_leader</code> process exactly as if it were a gen_server
%% (which, for practical purposes, it is.)
%% @end
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, local_call, [Name, Request]}})
    end.

%% @spec call(Name::serverRef(), Request, Timeout::integer()) ->
%%     Reply
%%
%%     Reply = term()
%%
%% @doc Equivalent to <code>gen_server:call/3</code>, but with a slightly
%% different exit reason if something goes wrong. This function calls 
%% the <code>gen_leader</code> process exactly as if it were a gen_server
%% (which, for practical purposes, it is.)
%% @end
call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, local_call, [Name, Request, Timeout]}})
    end.

%% @spec leader_call(Name::name(), Request::term())
%%    -> Reply
%%
%%    Reply = term()
%%
%% @doc Makes a call (similar to <code>gen_server:call/2</code>) to the 
%% leader. The call is forwarded via the local gen_leader instance, if 
%% that one isn't actually the leader. The client will exit if the 
%% leader dies while the request is outstanding.
%% <p>This function uses <code>gen:call/3</code>, and is subject to the
%% same default timeout as e.g. <code>gen_server:call/2</code>.</p>
%% @end
%%
leader_call(Name, Request) ->
    case catch gen:call(Name, '$leader_call', Request) of
	{ok,{leader,reply,Res}} ->
	    Res;
	{ok,{error, leader_died}} ->
	    exit({leader_died, {?MODULE, leader_call, [Name, Request]}});
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, leader_call, [Name, Request]}})
    end.

%% @spec leader_call(Name::name(), Request::term(), Timeout::integer())
%%    -> Reply
%%
%%    Reply = term()
%%
%% @doc Makes a call (similar to <code>gen_server:call/3</code>) to the 
%% leader. The call is forwarded via the local gen_leader instance, if 
%% that one isn't actually the leader. The client will exit if the 
%% leader dies while the request is outstanding.
%% @end
%%
leader_call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$leader_call', Request, Timeout) of
	{ok,{leader,reply,Res}} ->
	    Res;
	{ok,{error, leader_died}} ->
	    exit({leader_died, {?MODULE, leader_call, [Name, Request]}});
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, leader_call, [Name, Request, Timeout]}})
    end.



%% @equiv gen_server:cast/2
cast(Name, Request) ->
    catch do_cast('$gen_cast', Name, Request),
    ok.

%% @spec leader_cast(Name::name(), Msg::term()) -> ok
%% @doc Similar to <code>gen_server:cast/2</code> but will be forwarded to
%% the leader via the local gen_leader instance.
leader_cast(Name, Request) ->
    catch do_cast('$leader_cast', Name, Request),
    ok.


do_cast(Tag, Name, Request) when atom(Name) ->
    Name ! {Tag, Request};
do_cast(Tag, Pid, Request) when pid(Pid) ->
    Pid ! {Tag, Request}.


%% @spec reply(From::callerRef(), Reply::term()) -> Void
%% @equiv gen_server:reply/2
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.


%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
%%% @hidden
init_it(Starter, self, Name, Mod, {CandidateNodes, Workers, Arg}, Options) ->
    if CandidateNodes == [] ->
	    erlang:error(no_candidates);
       true ->
	    init_it(Starter, self(), Name, Mod, 
		    {CandidateNodes, Workers, Arg}, Options)
    end;
init_it(Starter,Parent,Name,Mod,{local_only, _}=Arg,Options) ->
    Debug = debug_options(Name, Options),
    reg_behaviour(),
    case catch Mod:init(Arg) of
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    Server = #server{parent = Parent,
			     mod = Mod,
			     state = State,
			     debug = Debug},
	    loop(Server, local_only, #election{name = Name, mode = local});
	Other ->
	    Error = {bad_return_value, Other},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end;
init_it(Starter,Parent,Name,Mod,{CandidateNodes,Workers,Arg},Options) ->
    Debug = debug_options(Name, Options),
    reg_behaviour(),
    AmCandidate = member(node(), CandidateNodes),
    Election = init_election(CandidateNodes, Workers, #election{name = Name}),
    case {catch Mod:init(Arg), AmCandidate} of
	{{stop, Reason},_} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{ignore,_} ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{{'EXIT', Reason},_} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	{{ok, State}, true} ->
%%%	    NewE = broadcast(capture,Workers++(CandidateNodes -- [node()]),
%%%			     Election),
	    proc_lib:init_ack(Starter, {ok, self()}), 	  
	    begin_election(#server{parent = Parent,
				   mod = Mod,
				   state = State,
				   debug = Debug}, candidate, Election);
	{{ok, State}, false} ->
%%%	    NewE = broadcast(add_worker, CandidateNodes, Election),
	    proc_lib:init_ack(Starter, {ok, self()}),
	    begin_election(#server{parent = Parent,
				   mod = Mod,
				   state = State,
				   debug = Debug}, waiting_worker, Election);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

reg_behaviour() ->
    catch gproc:reg({p,l,behaviour}, ?MODULE).

init_election(CandidateNodes, Workers, E) ->
%%%    dbg:tracer(),
%%%    dbg:tpl(?MODULE,lexcompare,[]),
%%%    dbg:p(self(),[m,c]),
    AmCandidate = member(node(), CandidateNodes),
    case AmCandidate of
	true ->
	    E#election{mode = global,
		       candidate_nodes = CandidateNodes,
		       worker_nodes = Workers,
		       iteration = {[], 
				    position(
				      node(),CandidateNodes)}};
	false ->
	    E#election{mode = global,
		       candidate_nodes = CandidateNodes,
		       worker_nodes = Workers}
    end.

begin_election(#server{mod = Mod, state = State} = Server, candidate,
	       #election{candidate_nodes = Cands,
			 worker_nodes = Workers} = E) ->
    case Cands of
	[N] when N == node() ->
	    {ok, Synch, NewState} = Mod:elected(State, E),
	    NewE = broadcast({elect,Synch}, E),
	    loop(Server#server{state = NewState}, elected, NewE);
	_ ->
	    NewE = broadcast(capture,Workers++(Cands -- [node()]), E),
	    safe_loop(Server, candidate, NewE)
    end;
begin_election(Server, waiting_worker, #election{candidate_nodes = Cands}=E) ->
    NewE = broadcast(add_worker, Cands, E),
    safe_loop(Server, waiting_worker, NewE).
    

%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------


safe_loop(#server{mod = Mod, state = State} = Server, Role,
	  #election{name = Name} = E) ->
    receive
	{system, From, Req} ->
	    #server{parent = Parent, debug = Debug} = Server,
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [safe, Server, Role, E]);
	{'EXIT', _Parent, Reason} = Msg ->
	    terminate(Reason, Msg, Server, Role, E);
	{leader,capture,Iteration,_Node,Candidate} ->
	    case Role of
		candidate ->
		    NewE =
			nodeup(node(Candidate),E),
		    case lexcompare(NewE#election.iteration,Iteration) of
			less ->
			    Candidate ! 
				{leader,accept,
				 NewE#election.iteration,self()},
			    safe_loop(Server, captured, 
				      NewE#election{leader = Candidate});
			greater ->
			    %% I'll get either an accept or DOWN
			    %% from Candidate later
			    safe_loop(Server, Role, NewE);
			equal ->
			    safe_loop(Server, Role, NewE)
		    end;
		captured ->
		    NewE = nodeup(node(Candidate), E),
		    safe_loop(Server, Role, NewE);
		waiting_worker ->
		    NewE = 
			nodeup(node(Candidate),E),
		    safe_loop(Server, Role, NewE)
	    end;
	{leader,add_worker,Worker} ->
	    NewE = nodeup(node(Worker), E),
	    safe_loop(Server, Role, NewE);
	{leader,accept,Iteration,Candidate} ->
	    case Role of
		candidate ->
		    NewE =
			nodeup(node(Candidate),E),
		    {Captured,_} = Iteration,
		    NewIteration =   % inherit all procs that have been
				     % accepted by Candidate
			foldl(fun(C,Iter) ->
				      add_captured(Iter,C)
			      end,NewE#election.iteration,
			      [node(Candidate)|Captured]),
		    check_majority(NewE#election{
				     iteration = NewIteration}, Server);
		captured ->
		    %% forward this to the leader
		    E#election.leader ! {leader,accept,Iteration,Candidate},
		    NewE = nodeup(node(Candidate), E),
		    safe_loop(Server, Role, NewE)
	    end;
	{leader,elect,Synch,Candidate} ->
	    NewE = 
		case Role of
		    waiting_worker ->
			nodeup(node(Candidate),
			       E#election{
				 leader = Candidate,
				 leadernode = node(Candidate)});
		    _ ->
			nodeup(node(Candidate),
			       E#election{
				 leader = Candidate,
				 leadernode = node(Candidate),
				 iteration = {[],
					      position(
						node(),
						E#election.candidate_nodes)}
				})
		end,
	    {ok,NewState} = Mod:surrendered(State,Synch,NewE),
	    NewRole = case Role of
			  waiting_worker ->
			      worker;
			  _ ->
			      surrendered
		      end,
	    loop(Server#server{state = NewState}, NewRole, NewE);
	{leader, local_only, Node, Candidate} ->
	    case lists:keysearch(node(Candidate), 2, E#election.monitored) of
		{value, {Ref, N}} ->
		    NewE = down(Ref, {E#election.name,N},local_only,E),
		    io:format("local_only received from ~p~n"
			      "E0 = ~p~n"
			      "E1 = ~p~n", [Node, E, NewE]),
		    safe_after_down(Server, Role, NewE);
		false ->
		    safe_loop(Server, Role, E)
	    end;
	{'DOWN',Ref,process,{Name,_}=Who,Why} ->
	    NewE = 
		down(Ref,Who,Why,E),
	    safe_after_down(Server, Role, NewE)
    end.

safe_after_down(Server, Role, E) ->
    case {Role,E#election.leader} of
	{candidate,_} ->
	    check_majority(E, Server);
	{captured,none} ->
	    check_majority(broadcast(capture,E), Server);
	{waiting_worker,_} ->
	    safe_loop(Server, Role, E)
    end.


loop(#server{parent = Parent,
	     mod = Mod,
	     state = State,
	     debug = Debug} = Server, Role,
     #election{mode = Mode, name = Name} = E) ->
    Msg = receive

	      Input ->
		    Input
	  end,
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [normal, Server, Role, E]);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Msg, Server, Role, E);
	{leader, local_only, _, _Candidate} ->
	    loop(Server, Role, E);
	LeaderMsg when element(1,LeaderMsg) == leader, Mode == local ->
	    Candidate = element(size(LeaderMsg), LeaderMsg),
	    Candidate ! {leader, local_only, node(), self()},
	    loop(Server, Role, E);
	{leader,capture,_Iteration,_Node,Candidate} ->
	    NewE = nodeup(node(Candidate),E),
	    case Role of
		R when R == surrendered; R == worker ->
		    loop(Server, Role, NewE);
		elected ->
		    {ok,Synch,NewState} = Mod:elected(State,NewE),
		    Candidate ! {leader, elect, Synch, self()},
		    loop(Server#server{state = NewState}, Role, NewE)
	    end;
	{leader,accept,_Iteration,Candidate} ->
	    NewE = nodeup(node(Candidate),E),
	    case Role of
		surrendered ->
		    loop(Server, Role, NewE);
		elected ->
		    {ok,Synch,NewState} = Mod:elected(State,NewE),
		    Candidate ! {leader, elect, Synch, self()},
		    loop(Server#server{state = NewState}, Role, NewE)
	    end;
	{leader,elect,Synch,Candidate} ->
	    NewE = 
		case Role of
		    worker ->
			nodeup(node(Candidate),
			       E#election{
				 leader = Candidate,
				 leadernode = node(Candidate)});
		    surrendered ->
			nodeup(node(Candidate),
			       E#election{
				 leader = Candidate,
				 leadernode = node(Candidate),
				 iteration = {[],
					      position(
						node(),
						E#election.candidate_nodes)}
				})
		end,
	    {ok, NewState} = Mod:surrendered(State, Synch, NewE),
	    loop(Server#server{state = NewState}, Role, NewE);
	{'DOWN',Ref,process,{Name,Node} = Who,Why} ->
	    #election{alive = PreviouslyAlive} = E,
	    NewE = 
		down(Ref,Who,Why,E),
	    case NewE#election.leader of
		none ->
		    foreach(fun({_,From}) ->
				    reply(From,{error,leader_died})
			    end, E#election.buffered),
		    NewE1 = NewE#election{buffered = []},
		    case Role of 
			surrendered ->
			    check_majority(
			      broadcast(capture,NewE1), Server);
			worker ->
			    safe_loop(Server, waiting_worker, NewE1)
		    end;
		L when L == self() ->
		    case member(Node, PreviouslyAlive) of
			true ->
			    case Mod:handle_DOWN(Node, State, E) of
				{ok, NewState} ->
				    loop(Server#server{state = NewState},
					 Role, NewE);
				{ok, Broadcast, NewState} ->
				    NewE1 = broadcast(
					      {from_leader,Broadcast}, NewE),
				    loop(Server#server{state = NewState},
					 Role, NewE1)
			    end;
			false ->
			    loop(Server, Role, NewE)
		    end;
		_ ->
		    loop(Server, Role, NewE)
	    end;
	_Msg when Debug == [] ->
	    handle_msg(Msg, Server, Role, E);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
				      E#election.name, {in, Msg}),
	    handle_msg(Msg, Server#server{debug = Debug1}, Role, E)
    end.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------

%% @hidden
system_continue(_Parent, Debug, [safe, Server, Role, E]) ->
    safe_loop(Server#server{debug = Debug}, Role, E);
system_continue(_Parent, Debug, [normal, Server, Role, E]) ->
    loop(Server#server{debug = Debug}, Role, E).

%% @hidden
system_terminate(Reason, _Parent, Debug, [_Mode, Server, Role, E]) ->
    terminate(Reason, [], Server#server{debug = Debug}, Role, E).

%% @hidden
system_code_change([Mode, Server, Role, E], _Module, OldVsn, Extra) ->
    #server{mod = Mod, state = State} = Server,
    case catch Mod:code_change(OldVsn, State, E, Extra) of
	{ok, NewState} ->
	    NewServer = Server#server{state = NewState},
	    {ok, [Mode, NewServer, Role, E]};
	{ok, NewState, NewE} ->
	    NewServer = Server#server{state = NewState},
	    {ok, [Mode, NewServer, Role, NewE]};
	Else -> Else
    end.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
%% @hidden
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got local call ~p from ~w~n",
		      [Name, Call, From]);
	{'$leader_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got global call ~p from ~w~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got local cast ~p~n",
		      [Name, Cast]);
	{'$leader_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got global cast ~p~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n", 
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


handle_msg({'$leader_call', From, Request} = Msg, 
	   #server{mod = Mod, state = State} = Server, elected = Role, E) ->
    case catch Mod:handle_leader_call(Request, From, State, E) of
	{reply, Reply, NState} ->
	    NewServer = reply(From, {leader,reply,Reply},
			      Server#server{state = NState}, Role, E),
	    loop(NewServer, Role, E);
	{reply, Reply, Broadcast, NState} ->
	    NewE = broadcast({from_leader,Broadcast}, E),
	    NewServer = reply(From, {leader,reply,Reply},
			      Server#server{state = NState}, Role,
			      NewE),
	    loop(NewServer, Role, NewE);
	{noreply, NState} = Reply ->
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, E);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Msg, 
				 Server#server{state = NState},
				 Role, E)),
	    reply(From, Reply),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Msg, Server, Role, E)
    end;
handle_msg({'$leader_call', From, Request} = Msg,
	   #server{mod = Mod, state = State} = Server, Role,
	   #election{mode = local} = E) ->
    Reply = (catch Mod:handle_leader_call(Request,From,State,E)),
    handle_call_reply(Reply, Msg, Server, Role, E);
%%%    handle_common_reply(Reply, Msg, Server, Role, E);
handle_msg({'$leader_cast', Cast} = Msg,
	   #server{mod = Mod, state = State} = Server, Role,
	   #election{mode = local} = E) ->
    Reply = (catch Mod:handle_leader_cast(Cast,State,E)),
    handle_common_reply(Reply, Msg, Server, Role, E);
handle_msg({'$leader_cast', Cast} = Msg, 
	   #server{mod = Mod, state = State} = Server, elected = Role, E) ->
    Reply = (catch Mod:handle_leader_cast(Cast, State, E)),
    handle_common_reply(Reply, Msg, Server, Role, E);
handle_msg({from_leader, Cmd} = Msg, 
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:from_leader(Cmd, State, E), 
			Msg, Server, Role, E);
handle_msg({'$leader_call', From, Request}, Server, Role,
	   #election{buffered = Buffered, leader = Leader} = E) ->
    Ref = make_ref(),
    Leader ! {'$leader_call', {self(),Ref}, Request},
    NewBuffered = [{Ref,From}|Buffered],
    loop(Server, Role, E#election{buffered = NewBuffered});
handle_msg({Ref, {leader,reply,Reply}}, Server, Role,
	   #election{buffered = Buffered} = E) ->
    {value, {_,From}} = keysearch(Ref,1,Buffered),
    NewServer = reply(From, {leader,reply,Reply}, Server, Role,
		      E#election{buffered = keydelete(Ref,1,Buffered)}),
    loop(NewServer, Role, E);
handle_msg({'$gen_call', From, Request} = Msg, 
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    Reply = (catch Mod:handle_call(Request, From, State)),
    handle_call_reply(Reply, Msg, Server, Role, E);
handle_msg({'$gen_cast',Msg} = Cast,
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:handle_cast(Msg, State), 
			Cast, Server, Role, E);
handle_msg(Msg,
	   #server{mod = Mod, state = State} = Server, Role, E) ->
    handle_common_reply(catch Mod:handle_info(Msg, State),
			Msg, Server, Role, E).


handle_call_reply(CB_reply, {_, From, _Request} = Msg, Server, Role, E) ->
    case CB_reply of
    	{reply, Reply, NState} ->
	    NewServer = reply(From, Reply, 
			      Server#server{state = NState}, Role, E),
	    loop(NewServer, Role, E);
	{noreply, NState} = Reply ->
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, E);
	{activate, Cands, Workers, Reply, NState}
	when E#election.mode == local ->
	    NewRole = case member(node(), Cands) of
			  true -> candidate;
			  false -> waiting_worker
		      end,
	    reply(From, Reply),
	    NServer = Server#server{state = NState},
	    NewE = init_election(Cands, Workers, E),
	    io:format("activating: NewE = ~p~n", [NewE]),
	    begin_election(NServer, NewRole, NewE);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Msg, Server#server{state = NState},
				 Role, E)),
	    reply(From, Reply),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Msg, Server, Role, E)
    end.


handle_common_reply(Reply, Msg, Server, Role, E) ->
    case Reply of
	{ok, NState} ->
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, E);
	{ok, Broadcast, NState} ->
	    NewE = broadcast({from_leader,Broadcast}, E),
	    NewServer = handle_debug(Server#server{state = NState},
				     Role, E, Reply),
	    loop(NewServer, Role, NewE);
	{stop, Reason, NState} ->
	    terminate(Reason, Msg, Server#server{state = NState}, Role, E);
	{'EXIT', Reason} ->
	    terminate(Reason, Msg, Server, Role, E);
	_ ->
	    terminate({bad_return_value, Reply}, Msg, Server, Role, E)
    end.


reply({To, Tag}, Reply, #server{state = State} = Server, Role, E) ->
    reply({To, Tag}, Reply),
    handle_debug(Server, Role, E, {out, Reply, To, State}).


handle_debug(#server{debug = []} = Server, _Role, _E, _Event) ->
    Server;
handle_debug(#server{debug = Debug} = Server, _Role, E, Event) ->
    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
			      E#election.name, Event),
    Server#server{debug = Debug1}.

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, Msg, #server{mod = Mod, 
			       state = State,
			       debug = Debug}, _Role,
	  #election{name = Name}) ->
    case catch Mod:terminate(Reason, State) of
	{'EXIT', R} ->
	    error_info(R, Name, Msg, State, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		_ ->
		    error_info(Reason, Name, Msg, State, Debug),
		    exit(Reason)
	    end
    end.

%% Maybe we shouldn't do this?  We have the crash report...
error_info(Reason, Name, Msg, State, Debug) ->
    format("** Generic leader ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
	   [Name, Msg, State, Reason]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Name, Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> dbg_options(Name, Options);
	_ -> dbg_options(Name, [])
    end.

dbg_options(Name, []) ->
    Opts = 
	case init:get_argument(generic_debug) of
	    error ->
		[];
	    _ ->
		[log, statistics]
	end,
    dbg_opts(Name, Opts);
dbg_options(Name, Opts) ->
    dbg_opts(Name, Opts).

dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
	{'EXIT',_} ->
	    format("~p: ignoring erroneous debug options - ~p~n",
		   [Name, Opts]),
	    [];
	Dbg ->
	    Dbg
    end.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
%% @hidden
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [_Mode, Server, _Role, E]] = StatusData,
    Header = lists:concat(["Status for generic server ", E#election.name]),
    Log = sys:get_debug(log, Debug, []),
    #server{mod = Mod, state = State} = Server,
    Specific = 
	case erlang:function_exported(Mod, format_status, 2) of
	    true ->
		case catch apply(Mod, format_status, [Opt, [PDict, State]]) of
		    {'EXIT', _} -> [{data, [{"State", State}]}];
		    Else -> Else
		end;
	    _ ->
		[{data, [{"State", State}]}]
	end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log}]} |
     Specific].




broadcast(Msg, #election{monitored = Monitored} = E) ->
    %% When broadcasting the first time, we broadcast to all candidate nodes,
    %% using broadcast/3. This function is used for subsequent broadcasts,
    %% and we make sure only to broadcast to already known nodes.
    %% It's the responsibility of new nodes to make themselves known through
    %% a wider broadcast.
    ToNodes = [N || {_,N} <- Monitored],
    broadcast(Msg, ToNodes, E).

broadcast(capture, ToNodes, #election{monitored = Monitored} = E) ->
    ToMonitor = [N || N <- ToNodes,
                      not(keymember(N,2,Monitored))],
    NewE = 
        foldl(fun(Node,Ex) ->
                      Ref = erlang:monitor(
			      process,{Ex#election.name,Node}),
                      Ex#election{monitored = [{Ref,Node}|
					      Ex#election.monitored]}
              end,E,ToMonitor),
    foreach(
      fun(Node) ->
	      {NewE#election.name,Node} !
		  {leader,capture,NewE#election.iteration,node(),self()}
      end,ToNodes),
    NewE;
broadcast({elect,Synch},ToNodes,E) ->
    foreach(
      fun(Node) ->
	      {E#election.name,Node} ! {leader,elect,Synch,self()}
      end,ToNodes),
    E;
broadcast({from_leader, Msg}, ToNodes, E) ->
    foreach(
      fun(Node) ->
	      {E#election.name,Node} ! {from_leader, Msg}
      end,ToNodes),
    E;
broadcast(add_worker, ToNodes, E) ->
    foreach(
      fun(Node) ->
	      {E#election.name,Node} ! {leader, add_worker, self()}
      end,ToNodes),
    E.



check_majority(E, Server) ->
    {Captured,_} = E#election.iteration,
    AcceptMeAsLeader = length(Captured) + 1,   % including myself
    NrCandidates = length(E#election.candidate_nodes),
    NrDown = E#election.down,
    if AcceptMeAsLeader > NrCandidates/2 ->
	    NewE = E#election{leader = self(), leadernode = node()},
	    {ok,Synch,NewState} =
		(Server#server.mod):elected(Server#server.state, NewE),
	    NewE1 = broadcast({elect,Synch}, NewE),
	    loop(Server#server{state = NewState}, elected, NewE1);
       AcceptMeAsLeader+length(NrDown) == NrCandidates -> 
	    NewE = E#election{leader = self(), leadernode = node()},
	    {ok,Synch,NewState} =
		(Server#server.mod):elected(Server#server.state, NewE),
	    NewE1 = broadcast({elect,Synch}, NewE),
	    loop(Server#server{state = NewState}, elected, NewE1);
       true ->
	    safe_loop(Server, candidate, E)
    end.


down(Ref,_Who,Why,E) ->
    case lists:keysearch(Ref,1,E#election.monitored) of
	{value, {_,Node}} ->
	    NewMonitored = if Why == local_only -> E#election.monitored;
			      true ->
				   E#election.monitored -- [{Ref,Node}]
			   end,
	    {Captured,Pos} = E#election.iteration,
	    case Node == E#election.leadernode of
		true ->
		    E#election{leader = none,
			       leadernode = none,
			       iteration = {Captured -- [Node],
					    Pos},  % TAKE CARE !
			       down = [Node|E#election.down],
			       alive = E#election.alive -- [Node],
			       monitored = NewMonitored};
		false ->
		    Down = case member(Node,E#election.candidate_nodes) of
			       true ->
				   [Node|E#election.down];
			       false ->
				   E#election.down
			   end,
		    E#election{iteration = {Captured -- [Node],
					    Pos},  % TAKE CARE !
			       down = Down,
			       alive = E#election.alive -- [Node],
			       monitored = NewMonitored}
	    end
    end.



%% position of element counted from end of the list
%%
position(X,[Head|Tail]) ->
    case X==Head of
        true ->
            length(Tail);
        false ->
            position(X,Tail)
    end.

%% This is a multi-level comment
%% This is the second line of the comment
lexcompare({C1,P1},{C2,P2}) ->
    lexcompare([{length(C1),length(C2)},{P1,P2}]).

lexcompare([]) ->
    equal;
lexcompare([{X,Y}|Rest]) ->
    if X<Y  -> less;
       X==Y -> lexcompare(Rest);
       X>Y  -> greater
    end.

add_captured({Captured,Pos}, CandidateNode) ->
    {[CandidateNode|[ Node || Node <- Captured,
			      Node =/= CandidateNode ]], Pos}.

nodeup(Node, #election{monitored = Monitored,
		       alive = Alive,
		       down = Down} = E) ->
    %% make sure process is monitored from now on
    case [ N || {_,N}<-Monitored, N==Node] of
        [] ->
            Ref = erlang:monitor(process,{E#election.name,Node}),
            E#election{down = Down -- [Node],
		       alive = [Node | Alive],
		       monitored = [{Ref,Node}|Monitored]};
        _ ->    % already monitored, thus not in down
            E#election{alive = [Node | [N || N <- Alive,
					     N =/= Node]]}
    end.

