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
-module(gen_event).

%%% 
%%% A general event handler.
%%% Several handlers (functions) can be added.
%%% Each handler holds a state and will be called
%%% for every event received of the handler.
%%% 

%%% Modified by Magnus.
%%%       Take care of fault situations and made notify asynchronous.
%%% Re-written by Joe with new functional interface !
%%% Modified by Martin - uses proc_lib, sys and gen!


-export([start/0, start/1, start_link/0, start_link/1, stop/1, notify/2, 
	 sync_notify/2,
	 add_handler/3, add_sup_handler/3, delete_handler/3, swap_handler/3,
	 swap_sup_handler/3, which_handlers/1, call/3, call/4]).

-export([behaviour_info/1]).

-export([init_it/6,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 print_event/3,
	 format_status/2]).

-import(error_logger, [error_msg/2]).

-define(reply(X), From ! {element(2,Tag), X}).

-record(handler, {module,
		  id = false,
		  state,
		  supervised = false}).

behaviour_info(callbacks) ->
    [{init,1},{handle_event,2},{handle_call,2},{handle_info,2},
     {terminate,2},{code_change,3}];
behaviour_info(_Other) ->
    undefined.

%% gen_event:start(Handler) -> ok | {error, What}
%%   gen_event:add_handler(Handler, Mod, Args) -> ok | Other
%%      gen_event:notify(Handler, Event) -> ok
%%      gen_event:call(Handler, Mod, Query) -> {ok, Val} | {error, Why}
%%      gen_event:call(Handler, Mod, Query, Timeout) -> {ok, Val} | {error, Why}
%%   gen_event:delete_handler(Handler, Mod, Args) -> Val
%%   gen_event:swap_handler(Handler, {OldMod, Args1}, {NewMod, Args2}) -> ok
%%   gen_event:which_handler(Handler) ->  [Mod]
%% gen_event:stop(Handler)  -> ok 


%% handlers must export
%% Mod:init(Args) -> {ok, State} | Other
%% Mod:handle_event(Event, State) -> 
%%    {ok, State'} | remove_handler | {swap_handler,Args1,State1,Mod2,Args2}
%% Mod:handle_info(Info, State) ->
%%    {ok, State'} | remove_handler | {swap_handler,Args1,State1,Mod2,Args2}
%% Mod:handle_call(Query, State) -> 
%%    {ok, Reply, State'} | {remove_handler, Reply} | 
%%    {swap_handler, Reply, Args1,State1,Mod2,Args2}
%% Mod:terminate(Args, State) -> Val


%% add_handler(H, Mod, Args) -> ok | Other
%%    Mod:init(Args) -> {ok, State} | Other

%% delete_handler(H, Mod, Args) -> Val
%%    Mod:terminate(Args, State) -> Val

%% notify(H, Event) 
%%    Mod:handle_event(Event, State) ->
%%         {ok, State1}
%%         remove_handler
%%               Mod:terminate(remove_handler, State) is called
%%               the return value is ignored
%%         {swap_handler, Args1, State1, Mod2, Args2}
%%               State2 = Mod:terminate(Args1, State1) is called
%%               the return value is chained into the new module and
%%               Mod2:init({Args2, State2}) is called
%%         Other
%%               Mod:terminate({error, Other}, State) is called
%%               The return value is ignored
%% call(H, Mod, Query) -> Val
%% call(H, Mod, Query, Timeout) -> Val
%%      Mod:handle_call(Query, State) -> as above


start() ->
    gen:start(gen_event, nolink, [], [], []).

start(Name) ->
    gen:start(gen_event, nolink, Name, [], [], []).

start_link() ->
    gen:start(gen_event, link, [], [], []).

start_link(Name) ->
    gen:start(gen_event, link, Name, [], [], []).

init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name, _, _, Options) ->
    process_flag(trap_exit, true),
    gen:reg_behaviour(?MODULE),
    Debug = gen:debug_options(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Name, [], Debug).

add_handler(M, Handler, Args)      -> rpc (M, {add_handler, Handler, Args}).
add_sup_handler(M, Handler, Args)  ->
    rpc (M, {add_sup_handler, Handler, Args, self()}).
notify(M, Event)                   -> send(M, {notify, Event}). 
sync_notify(M, Event)              -> rpc (M, {sync_notify, Event}).
call(M, Handler, Query)            -> call1(M, Handler, Query).
call(M, Handler, Query, Timeout)   -> call1(M, Handler, Query, Timeout).
delete_handler(M, Handler, Args)   -> rpc (M, {delete_handler, Handler, Args}).
swap_handler(M, {H1, A1},{H2, A2}) -> rpc (M, {swap_handler, H1, A1, H2, A2}).
swap_sup_handler(M, {H1, A1},{H2, A2}) ->
    rpc (M, {swap_sup_handler, H1, A1, H2, A2, self()}).
which_handlers(M)                  -> rpc (M, which_handlers).
stop(M)                            -> rpc (M, stop).

rpc(M, Cmd) -> 
    {ok,Reply} = gen:call(M, self(), Cmd, infinity),
    Reply.

call1(M, Handler, Query) ->
    Cmd = {call, Handler, Query},
    case catch gen:call(M, self(), Cmd) of
	{ok,Res} ->
	    Res;
	{'EXIT', Reason} ->
	    exit({Reason, {gen_event, call, [M, Handler, Query]}})
    end.

call1(M, Handler, Query, Timeout) ->
    Cmd = {call, Handler, Query},
    case catch gen:call(M, self(), Cmd, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT', Reason} ->
	    exit({Reason, {gen_event, call, [M, Handler, Query, Timeout]}})
    end.

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

loop(Parent, ServerName, MSL, Debug) ->
    receive
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, gen_event, Debug,
				  [ServerName, MSL]);
	{'EXIT', Parent, Reason} ->
	    terminate_server(Reason, Parent, MSL, ServerName);
	Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, ServerName, MSL, []);
	Msg ->
	    Debug1 = sys:handle_debug(Debug, {gen_event, print_event}, 
				      ServerName, {in, Msg}),
	    handle_msg(Msg, Parent, ServerName, MSL, Debug1)
    end.

handle_msg(Msg, Parent, ServerName, MSL, Debug) ->
    case Msg of
	{notify, Event} ->
	    MSL1 = server_notify(Event, handle_event, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {sync_notify, Event}} ->
	    MSL1 = server_notify(Event, handle_event, MSL, ServerName),
	    ?reply(ok),
	    loop(Parent, ServerName, MSL1, Debug);
	{'EXIT', From, Reason} ->
	    MSL1 = handle_exit(From, Reason, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {call, Handler, Query}} ->
	    {Reply, MSL1} = server_call(Handler, Query, MSL, ServerName),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {add_handler, Handler, Args}} ->
	    {Reply, MSL1} = server_add_handler(Handler, Args, MSL),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {add_sup_handler, Handler, Args, SupP}} ->
	    {Reply, MSL1} = server_add_sup_handler(Handler, Args, MSL, SupP),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {delete_handler, Handler, Args}} ->
	    {Reply, MSL1} = server_delete_handler(Handler, Args, MSL,
						  ServerName),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {swap_handler, Handler1, Args1, Handler2, Args2}} ->
	    {Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						Args2, MSL, ServerName),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, {swap_sup_handler, Handler1, Args1, Handler2, Args2,
		     Sup}} ->
	    {Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						Args2, MSL, Sup, ServerName),
	    ?reply(Reply),
	    loop(Parent, ServerName, MSL1, Debug);
	{From, Tag, stop} ->
	    catch terminate_server(normal, Parent, MSL, ServerName),
	    ?reply(ok);
	{From, Tag, which_handlers} ->
	    ?reply(the_handlers(MSL)),
	    loop(Parent, ServerName, MSL, Debug);
	{From, Tag, get_modules} ->
	    ?reply(get_modules(MSL)),
	    loop(Parent, ServerName, MSL, Debug);
	Other  ->
	    MSL1 = server_notify(Other, handle_info, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, Debug)
    end.

terminate_server(Reason, Parent, MSL, ServerName) ->
    stop_handlers(MSL, ServerName),
    do_unlink(Parent, MSL),
    exit(Reason).

%% unlink the supervisor process of all supervised handlers.
%% We do not want a handler supervisor to EXIT due to the
%% termination of the event manager (server).
%% Do not unlink Parent !
do_unlink(Parent, MSL) ->
    lists:foreach(fun(Handler) when Handler#handler.supervised =:= Parent ->
			  true;
		     (Handler) when is_pid(Handler#handler.supervised) ->
			  unlink(Handler#handler.supervised),
			  true;
		     (_) ->
			  true
		  end,
		  MSL).

%% First terminate the supervised (if exists) handlers and
%% then inform other handlers.
%% We do not know if any handler really is interested but it
%% may be so !
handle_exit(From, Reason, MSL, SName) ->
    MSL1 = terminate_supervised(From, Reason, MSL, SName),
    server_notify({'EXIT', From, Reason}, handle_info, MSL1, SName).

terminate_supervised(Pid, Reason, MSL, SName) ->
    F = fun(Ha) when Ha#handler.supervised =:= Pid ->
		do_terminate(Ha#handler.module,
			     Ha,
			     {stop,Reason},
			     Ha#handler.state,
			     {parent_terminated, {Pid,Reason}},
			     SName,
			     shutdown),
		false;
	   (_) ->
		true
	end,
    lists:filter(F, MSL).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [ServerName, MSL]) ->
    loop(Parent, ServerName, MSL, Debug).

system_terminate(Reason, Parent, _Debug, [ServerName, MSL]) ->
    terminate_server(Reason, Parent, MSL, ServerName).

%%-----------------------------------------------------------------
%% Module here is sent in the system msg change_code.  It specifies
%% which module should be changed.
%%-----------------------------------------------------------------
system_code_change([ServerName, MSL], Module, OldVsn, Extra) ->
    MSL1 = lists:zf(fun(H) when H#handler.module =:= Module ->
			    {ok, NewState} =
				Module:code_change(OldVsn,
						   H#handler.state, Extra),
			    {true, H#handler{state = NewState}};
		       (_) -> true
		    end,
		    MSL),
    {ok, [ServerName, MSL1]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{notify, Event} ->
	    io:format(Dev, "*DBG* ~p got event ~p~n", [Name, Event]);
	{_,_,{call, Handler, Query}} ->
	    io:format(Dev, "*DBG* ~p(~p) got call ~p~n",
		      [Name, Handler, Query]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, Dbg, Name) ->
    io:format(Dev, "*DBG* ~p : ~p~n", [Name, Dbg]).


%% server_add_handler(Handler, Args, MSL) -> {Ret, MSL'}.
%%   where MSL = [#handler]
%%   Ret goes to the top level MSL' is the new internal state of the
%%   event handler

server_add_handler({Mod,Id}, Args, MSL) ->
    Handler = #handler{module = Mod,
		       id = Id},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_handler(Mod, Args, MSL) -> 
    Handler = #handler{module = Mod},
    server_add_handler(Mod, Handler, Args, MSL).

server_add_handler(Mod, Handler, Args, MSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
	    {ok, [Handler#handler{state = State}|MSL]};
        Other ->
            {Other, MSL}
    end.

%% Set up a link to the supervising process.
%% (Ought to be unidirected links here, Erl5.0 !!)
%% NOTE: This link will not be removed then the
%% handler is removed in case another handler has
%% own link to this process.
server_add_sup_handler({Mod,Id}, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       id = Id,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_sup_handler(Mod, Args, MSL, Parent) -> 
    link(Parent),
    Handler = #handler{module = Mod,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL).

%% server_delete_handler(HandlerId, Args, MSL) -> {Ret, MSL'}

server_delete_handler(HandlerId, Args, MSL, SName) -> 
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, delete, SName, normal),
	     MSL1};
	error ->
	    {{error, module_not_found}, MSL}
    end.

%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SN)= -> MSL'
%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SN)= -> MSL'

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SName) ->
    {State2, Sup, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					      SName, Handler2, false),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{ok, MSL2} ->
	    {ok, MSL2};
	{What, MSL2} ->
	    {{error, What}, MSL2}
    end.

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SName) ->
    {State2, _, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					    SName, Handler2, Sup),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{ok, MSL2} ->
	    {ok, MSL2};
	{What, MSL2} ->
	    {{error, What}, MSL2}
    end.

s_s_h(false, Handler, Args, MSL) ->
    server_add_handler(Handler, Args, MSL);
s_s_h(Pid, Handler, Args, MSL) ->
    server_add_sup_handler(Handler, Args, MSL, Pid).

split_and_terminate(HandlerId, Args, MSL, SName, Handler2, Sup) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    OldSup = Handler#handler.supervised,
	    NewSup = if
			 not Sup -> OldSup;
			 true    -> Sup
		     end,
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, swapped, SName,
			  {swapped, Handler2, NewSup}),
	     OldSup,
	     MSL1};
	error ->
            {error, false, MSL}
    end.

%% server_notify(Event, Func, MSL, SName) -> MSL'

server_notify(Event, Func, [Handler|T], SName) -> 
    case server_update(Handler, Func, Event, SName) of
	{ok, Handler1} ->
	    [Handler1|server_notify(Event, Func, T, SName)];
	no ->
	    server_notify(Event, Func, T, SName)
    end;
server_notify(_, _, [], _) ->
    [].

%% server_update(Handler, Func, Event, ServerName) -> Handler1 | no

server_update(Handler1, Func, Event, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:Func(Event, State) of
	{ok, State1} -> 
	    {ok, Handler1#handler{state = State1}};
	{swap_handler, Args1, State1, Handler2, Args2} ->
	    do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName);
	remove_handler ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    no;
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Event, SName, crash),
	    no
    end.

do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName) ->
    %% finalise the existing handler
    State2 = do_terminate(Mod1, Handler1, Args1, State1,
			  swapped, SName,
			  {swapped, Handler2, Handler1#handler.supervised}),
    {Mod2,Handler} = new_handler(Handler2, Handler1),
    case catch Mod2:init({Args2, State2}) of
	{ok, State2a} ->
	    {ok, Handler#handler{state = State2a}};
	Other ->
	    report_terminate(Handler2, crash, {error, Other}, SName, false),
	    no
    end.

new_handler({Mod,Id}, Handler1) ->
    {Mod,#handler{module = Mod,
		  id = Id,
		  supervised = Handler1#handler.supervised}};
new_handler(Mod, Handler1) ->
    {Mod,#handler{module = Mod,
		  supervised = Handler1#handler.supervised}}.


%% split(Handler, [#handler]) ->
%%   {Mod, #handler, [#handler]} | error

split(Ha, MSL) -> split(Ha, MSL, []).

split({Mod,Id}, [Ha|T], L) when Ha#handler.module =:= Mod,
                                Ha#handler.id =:= Id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Mod, [Ha|T], L) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Ha, [H|T], L) ->
    split(Ha, T, [H|L]);
split(_, [], _) ->
    error.

%% server_call(Handler, Query, MSL, ServerName) ->
%%    {Reply, MSL1}

server_call(Handler, Query, MSL, SName) ->
    case search(Handler, MSL) of
	{ok, Ha} ->
	    case server_call_update(Ha, Query, SName) of
		{no, Reply} ->
		    {Reply, delete(Handler, MSL)};
		{{ok, Ha1}, Reply} ->
		    {Reply, replace(Handler, MSL, Ha1)}
	    end;
	false ->
	    {{error, bad_module}, MSL}
    end.

search({Mod, Id}, [Ha|_MSL]) when Ha#handler.module =:= Mod,
				  Ha#handler.id =:= Id ->
    {ok, Ha};
search(Mod, [Ha|_MSL]) when Ha#handler.module =:= Mod,
			    not Ha#handler.id ->
    {ok, Ha};
search(Handler, [_|MSL]) ->
    search(Handler, MSL);
search(_, []) ->
    false.

delete({Mod, Id}, [Ha|MSL]) when Ha#handler.module =:= Mod,
                                 Ha#handler.id =:= Id ->
    MSL;
delete(Mod, [Ha|MSL]) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    MSL;
delete(Handler, [Ha|MSL]) ->
    [Ha|delete(Handler, MSL)];
delete(_, []) ->
    [].

replace({Mod, Id}, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                         Ha#handler.id =:= Id ->
    [NewHa|MSL];
replace(Mod, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                   not Ha#handler.id ->
    [NewHa|MSL];
replace(Handler, [Ha|MSL], NewHa) ->
    [Ha|replace(Handler, MSL, NewHa)];
replace(_, [], NewHa) ->
    [NewHa].

%% server_call_update(Handler, Query, ServerName) ->
%%    {{Handler1, State1} | no, Reply}

server_call_update(Handler1, Query, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:handle_call(Query, State) of
	{ok, Reply, State1} -> 
	    {{ok, Handler1#handler{state = State1}}, Reply};
	{swap_handler, Reply, Args1, State1, Handler2, Args2} ->
	    {do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName), Reply};
	{remove_handler, Reply} -> 
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    {no, Reply};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Query, SName, crash),
	    {no, {error, Other}}
end.

do_terminate(Mod, Handler, Args, State, LastIn, SName, Reason) ->
    Res = (catch Mod:terminate(Args, State)),
    report_terminate(Handler, Reason, Args, State, LastIn, SName, Res),
    Res.

report_terminate(Handler, crash, {error, Why}, State, LastIn, SName, _) ->
    report_terminate(Handler, Why, State, LastIn, SName);
report_terminate(Handler, How, _, State, LastIn, SName, _) ->
    %% How == normal | shutdown | {swapped, NewHandler, NewSupervisor}
    report_terminate(Handler, How, State, LastIn, SName).

report_terminate(Handler, Reason, State, LastIn, SName) ->
    report_error(Handler, Reason, State, LastIn, SName),
    case Handler#handler.supervised of
	false ->
	    ok;
	Pid ->
	    Pid ! {gen_event_EXIT,handler(Handler),Reason},
	    ok
    end.

report_error(_Handler, normal, _, _, _)               -> ok;
report_error(_Handler, shutdown, _, _, _)             -> ok;
report_error(_Handler, {swapped,_,_}, _, _, _)        -> ok;
report_error(Handler, Reason, State, LastIn, SName)   ->
    Reason1 = 
	case Reason of
	    {'EXIT',{undef,[{M,F,A}|MFAs]}} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				{undef,[{M,F,A}|MFAs]};
			    false ->
				{'function not exported',[{M,F,A}|MFAs]}
			end
		end;
	    {'EXIT',Why} -> 
		Why;
	    _ ->            
		Reason
	end,
    error_msg("** gen_event handler ~p crashed.~n"
	      "** Was installed in ~p~n"
	      "** Last event was: ~p~n"
	      "** When handler state == ~p~n"
	      "** Reason == ~p~n",
	      [handler(Handler),SName,LastIn,State,Reason1]).

handler(Handler) when not Handler#handler.id ->
    Handler#handler.module;
handler(Handler) ->
    {Handler#handler.module, Handler#handler.id}.

%% stop_handlers(MSL, ServerName) -> []

stop_handlers([Handler|T], SName) ->
    Mod = Handler#handler.module,
    do_terminate(Mod, Handler, stop, Handler#handler.state,
		 stop, SName, shutdown),
    stop_handlers(T, SName);
stop_handlers([], _) ->
    [].

the_handlers(MSL) ->
    lists:map(fun(Handler) when not Handler#handler.id ->
		      Handler#handler.module;
		 (Handler) ->
		      {Handler#handler.module, Handler#handler.id}
	      end,
	      MSL).

%% Message from the release_handler.
%% The list of modules got to be a set !
get_modules(MSL) ->
    Mods = lists:map(fun(Handler) -> Handler#handler.module end,
		     MSL),
    ordsets:to_list(ordsets:from_list(Mods)).

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(_Opt, StatusData) ->
    [_PDict, SysState, Parent, _Debug, [ServerName, MSL]] = StatusData,
    Header = lists:concat(["Status for event handler ", ServerName]),
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent}]},
     {items, {"Installed handlers", MSL}}].






