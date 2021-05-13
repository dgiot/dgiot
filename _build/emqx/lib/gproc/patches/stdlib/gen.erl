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
%%     $Id$
%%
-module(gen).

%%%-----------------------------------------------------------------
%%% This module implements the really generic stuff of the generic
%%% standard behaviours (e.g. gen_server, gen_fsm).
%%%
%%% The standard behaviour should export init_it/6.
%%%-----------------------------------------------------------------
-export([start/5, start/6, debug_options/1,
	 call/3, call/4, reply/2]).
-export([reg_behaviour/1]).
-export([init_it/6, init_it/7]).

-define(default_timeout, 5000).

%%-----------------------------------------------------------------
%% Starts a generic process.
%% start(GenMod, LinkP, Mod, Args, Options)
%% start(GenMod, LinkP, Name, Mod, Args, Options)
%% start_link(Mod, Args, Options)
%% start_link(Name, Mod, Args, Options) where:
%%    Name = {local, atom()} | {global, atom()}
%%    Mod  = atom(), callback module implementing the 'real' fsm
%%    Args = term(), init arguments (to Mod:init/1)
%%    Options = [{debug, [Flag]}]
%%      Flag = trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%%-----------------------------------------------------------------
start(GenMod, LinkP, Name, Mod, Args, Options) ->
    case where(Name) of
	undefined ->
	    do_spawn(GenMod, LinkP, Name, Mod, Args, Options);
	Pid ->
	    {error, {already_started, Pid}}
    end.

start(GenMod, LinkP, Mod, Args, Options) ->
    do_spawn(GenMod, LinkP, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Spawn the process (and link) maybe at another node.
%% If spawn without link, set parent to our selves "self"!!!
%%-----------------------------------------------------------------
do_spawn(GenMod, link, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(gen, init_it,
			[GenMod, self(), self(), Mod, Args, Options], 
			Time,
			spawn_opts(Options));
do_spawn(GenMod, _, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(gen, init_it,
		   [GenMod, self(), self, Mod, Args, Options], 
		   Time,
		   spawn_opts(Options)).
do_spawn(GenMod, link, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(gen, init_it,
			[GenMod, self(), self(), Name, Mod, Args, Options],
			Time,
			spawn_opts(Options));
do_spawn(GenMod, _, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(gen, init_it,
		   [GenMod, self(), self, Name, Mod, Args, Options], 
		   Time,
		   spawn_opts(Options)).


reg_behaviour(B) ->
    catch begin
	      Key = {p,l,behaviour},
	      try gproc:reg(Key, B)
	      catch
		  error:badarg ->
		      gproc:set_value(Key, B)
	      end
	  end.


%%-----------------------------------------------------------------
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%%-----------------------------------------------------------------
init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
    init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    case name_register(Name) of
	true ->
	    init_it2(GenMod, Starter, Parent, name(Name), Mod, Args, Options);
	{false, Pid} ->
	    proc_lib:init_ack(Starter, {error, {already_started, Pid}})
    end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).


%%-----------------------------------------------------------------
%% Makes a synchronous call to a generic process.
%% Request is sent to the Pid, and the response must be
%% {Tag, _, Reply}.
%%-----------------------------------------------------------------

%%% New call function which uses the new monitor BIF
%%% call(ServerId, Label, Request)

call(Process, Label, Request) -> 
    call(Process, Label, Request, ?default_timeout).

%% Local or remote by pid
call(Pid, Label, Request, Timeout) 
  when is_pid(Pid), Timeout =:= infinity;
       is_pid(Pid), is_integer(Timeout), Timeout >= 0 ->
    do_call(Pid, Label, Request, Timeout);
%% Local by name
call(Name, Label, Request, Timeout) 
  when is_atom(Name), Timeout =:= infinity;
       is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    do_call(Pid, Label, Request, Timeout);
	undefined ->
	    exit(noproc)
    end;
%% Global by name
call({global, _Name}=Process, Label, Request, Timeout)
  when Timeout =:= infinity;
       is_integer(Timeout), Timeout >= 0 ->
    case where(Process) of
	Pid when is_pid(Pid) ->
	    Node = node(Pid),
	    case catch do_call(Pid, Label, Request, Timeout) of
		{'EXIT', {nodedown, Node}} ->
		    % A nodedown not yet detected by global, pretend that it
		    % was.
		    exit(noproc);
		{'EXIT', noproc} ->
		    exit(noproc);
		{'EXIT', OtherExits} ->
		    exit(OtherExits);
		Result ->
		    Result
	    end;
	undefined ->
	    exit(noproc)
    end;
%% Local by name in disguise
call({Name, Node}, Label, Request, Timeout)
  when Node =:= node(), Timeout =:= infinity;
       Node =:= node(), is_integer(Timeout), Timeout >= 0 ->
    call(Name, Label, Request, Timeout);
%% Remote by name
call({_Name, Node}=Process, Label, Request, Timeout)
  when is_atom(Node), Timeout =:= infinity;
       is_atom(Node), is_integer(Timeout), Timeout >= 0 ->
    if
 	node() =:= nonode@nohost ->
 	    exit({nodedown, Node});
 	true ->
 	    do_call(Process, Label, Request, Timeout)
    end.

do_call(Process, Label, Request, Timeout) ->
    %% We trust the arguments to be correct, i.e
    %% Process is either a local or remote pid,
    %% or a {Name, Node} tuple (of atoms) and in this 
    %% case this node (node()) _is_ distributed and Node =/= node().
    Node = case Process of
	       {_S, N} ->
		   N;
	       _ when is_pid(Process) ->
		   node(Process);
	       _ ->
		   node()
	   end,
    case catch erlang:monitor(process, Process) of
	Mref when is_reference(Mref) ->
	    receive
		{'DOWN', Mref, _, Pid1, noconnection} when is_pid(Pid1) ->
		    exit({nodedown, node(Pid1)});
		{'DOWN', Mref, _, _, noconnection} ->
		    exit({nodedown, Node});
		{'DOWN', Mref, _, _, _} ->
		    exit(noproc)
	    after 0 ->
		    Process ! {Label, {self(), Mref}, Request},
		    wait_resp_mon(Process, Mref, Timeout)
	    end;
	{'EXIT', _} ->
	    %% Old node is not supporting the monitor.
	    %% The other possible case -- this node is not distributed
	    %% -- should have been handled earlier.
	    %% Do the best possible with monitor_node/2.
	    %% This code may hang indefinitely if the Process 
	    %% does not exist. It is only used for old remote nodes.
	    monitor_node(Node, true),
	    receive
		{nodedown, Node} -> 
		    monitor_node(Node, false),
		    exit({nodedown, Node})
	    after 0 -> 
		    Mref = make_ref(),
		    Process ! {Label, {self(),Mref}, Request},
		    Res = wait_resp(Node, Mref, Timeout),
		    monitor_node(Node, false),
		    Res
	    end
    end.

wait_resp_mon(Process, Mref, Timeout) ->
    Node = case Process of
	       {_S, N} ->
		   N;
	       _ when is_pid(Process) ->
		   node(Process);
	       _ ->
		   node()
	   end,
    receive
	{Mref, Reply} ->
	    erlang:demonitor(Mref),
	    receive 
		{'DOWN', Mref, _, _, _} -> 
		    {ok, Reply}
	    after 0 -> 
		    {ok, Reply}
	    end;
	{'DOWN', Mref, _, Pid, Reason} when is_pid(Pid) ->
	    receive
		{'EXIT', Pid, noconnection} -> 
		    exit({nodedown, Node});
		{'EXIT', Pid, What} -> 
		    exit(What)
	    after 1 -> % Give 'EXIT' message time to arrive
		    case Reason of
			noconnection ->
			    exit({nodedown, Node});
			_ ->
			    exit(Reason)
		    end
	    end;
	{'DOWN', Mref, _, _, noconnection} ->
	    %% Here is a hole, when the monitor is remote by name
	    %% and the remote node goes down, we will never find 
	    %% out the Pid and cannot know which 'EXIT' message
	    %% to read out. This awkward case should have been 
	    %% handled earlier (except for against rex) 
	    %% by not using remote monitor by name.
	    case Process of
		_ when is_pid(Process) ->
		    receive
			{'EXIT', Process, noconnection} ->
			    exit({nodedown, Node});
			{'EXIT', Process, What} ->
			    exit(What)
		    after 1 -> % Give 'EXIT' message time to arrive
			    exit({nodedown, node(Process)})
		    end;
		_ ->
		    exit({nodedown, Node})
	    end;
	%% {'DOWN', Mref, _, _, noproc} ->
	%%     exit(noproc);
	{'DOWN', Mref, _Tag, _Item, Reason} ->
	    exit(Reason)
    after Timeout ->
	    erlang:demonitor(Mref),
	    receive
		{'DOWN', Mref, _, _, _Reason} -> true 
	    after 0 -> true
	    end,
	    exit(timeout)
    end.

wait_resp(Node, Tag, Timeout) ->
    receive
	{Tag, Reply} ->
	    {ok,Reply};
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    exit({nodedown, Node})
    after Timeout ->
	    monitor_node(Node, false),
	    exit(timeout)
    end.

%
% Send a reply to the client.
%
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%%-----------------------------------------------------------------
%%%  Misc. functions.
%%%-----------------------------------------------------------------
where({global, Name})    -> global:safe_whereis_name(Name);
where({local, Name})  -> whereis(Name).

name({global, Name})    -> Name;
name({local, Name})     -> Name.

name_register({local, Name}) ->
    case catch register(Name, self()) of
	true -> true;
	{'EXIT', _} ->
	    {false, where({local, Name})}
    end;
name_register({global, Name}) ->
    case global:register_name(Name, self()) of
	yes -> true;
	no -> {false, where({global, Name})}
    end.

timeout(Options) ->
    case opt(timeout, Options) of
	{ok, Time} ->
	    Time;
	_ ->
	    infinity
    end.

spawn_opts(Options) ->
    case opt(spawn_opt, Options) of
	{ok, Opts} ->
	    Opts;
	_ ->
	    []
    end.

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> sys:debug_options(Options);
	_ -> []
    end.
