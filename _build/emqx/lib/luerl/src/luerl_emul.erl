%% Copyright (c) 2013 Robert Virding
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

%% File    : luerl_emul.erl
%% Author  : Robert Virding
%% Purpose : A very basic LUA 5.2 machine emulator.

%% First version of emulator. Compiler so far only explicitly handles
%% local/global variables.
%%
%% We explicitly mirror the parser rules which generate the AST and do
%% not try to fold similar structures into common code. While this
%% means we get more code it also becomes more explicit and clear what
%% we are doing. It may also allow for specific optimisations. And
%% example is that we DON'T fold 'var' and 'funcname' even though they
%% are almost the same.
%%
%% Issues: how should we handle '...'? Now we treat it as any (local)
%% variable.

-module(luerl_emul).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

%% Basic interface.
-export([init/0,gc/1]).
-export([call/2,call/3,emul/2]).
-export([load_chunk/2,load_chunk/3,load_function/2,load_function/3]).

%% Internal functions which can be useful "outside".
-export([alloc_table/1,alloc_table/2,free_table/2,
	 functioncall/3,methodcall/4,
	 get_table_keys/2,get_table_keys/3,
	 set_table_keys/3,set_table_keys/4,
	 get_table_key/3,set_table_key/4,
	 getmetatable/2,
	 getmetamethod/3,getmetamethod/4]).

%% Currently unused internal functions, to suppress warnings.
-export([set_global_name/3,set_global_key/3,
	 get_global_name/2,get_global_key/2]).

%% For testing.
-export([pop_vals/2,push_vals/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).

%% -compile(inline).				%For when we are optimising
%% -compile({inline,[boolean_value/1,first_value/1]}).

%% -define(ITRACE_DO(Expr), ok).
-define(ITRACE_DO(Expr), (get(itrace) /= undefined) andalso Expr).

%% init() -> State.
%% Initialise the basic state.

init() ->
    %% Initialise the general stuff.
    St0 = #luerl{meta=#meta{},tag=make_ref()},
    %% Initialise the table handling.
    St1 = St0#luerl{ttab=?MAKE_TABLE(),tfree=[],tnext=0},
    %% Initialise the frame handling.
    St2 = St1#luerl{ftab=array:new(),ffree=[],fnext=0},
    %% Allocate the _G table and initialise the environment
    {_G,St3} = luerl_lib_basic:install(St2),	%Global environment
    St4 = St3#luerl{g=_G},
    %% Now we can start adding libraries. Package MUST be first!
    St5 = load_lib(<<"package">>, luerl_lib_package, St4),
    %% Add the other standard libraries.
    St6 = load_libs([
		     {<<"bit32">>,luerl_lib_bit32},
		     {<<"io">>,luerl_lib_io},
		     {<<"math">>,luerl_lib_math},
		     {<<"os">>,luerl_lib_os},
		     {<<"string">>,luerl_lib_string},
		     {<<"table">>,luerl_lib_table},
		     {<<"debug">>,luerl_lib_debug}
		    ], St5),
    %% Set _G variable to point to it and add it packages.loaded.
    St7 = set_global_key(<<"_G">>, _G, St6),
    set_table_keys([<<"package">>,<<"loaded">>,<<"_G">>], _G, St7).

load_libs(Libs, St) ->
    Fun = fun ({Key,Mod}, S) -> load_lib(Key, Mod, S) end,
    lists:foldl(Fun, St, Libs).

%% load_lib(Key, Module, State) -> State.

load_lib(Key, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    %% Add key to global and to package.loaded.
    St2 = set_global_key(Key, Tab, St1),
    set_table_keys([<<"package">>,<<"loaded">>,Key], Tab, St2).

%% set_global_name(Name, Value, State) -> State.
%% set_global_key(Key, Value, State) -> State.
%% get_global_name(Name, State) -> {[Val],State}.
%% get_global_key(Key, State) -> {[Val],State}.
%%  Access elements in the global name table, _G.

set_global_name(Name, Val, St) ->
    set_global_key(atom_to_binary(Name, latin1), Val, St).

set_global_key(Key, Val, #luerl{g=G}=St) ->
    set_table_key(G, Key, Val, St).

get_global_name(Name, St) ->
    get_global_key(atom_to_binary(Name, latin1), St).

get_global_key(Key, #luerl{g=G}=St) ->
    get_table_key(G, Key, St).

%% alloc_frame(Frame, State) -> {Fref,State}.
%%  Allocate the frame in the frame table and return its fref.

alloc_frame(Fr, #luerl{ftab=Ft0,ffree=[N|Ns]}=St) ->
    Ft1 = array:set(N, Fr, Ft0),
    {#fref{i=N},St#luerl{ftab=Ft1,ffree=Ns}};
alloc_frame(Fr, #luerl{ftab=Ft0,ffree=[],fnext=N}=St) ->
    Ft1 = array:set(N, Fr, Ft0),
    {#fref{i=N},St#luerl{ftab=Ft1,fnext=N+1}}.

%% alloc_table(State) -> {Tref,State}.
%% alloc_table(InitialTable, State) -> {Tref,State}.
%% free_table(Tref, State) -> State.
%%  The InitialTable is [{Key,Value}], there is no longer any need to
%%  have it as an orddict.

alloc_table(St) -> alloc_table([], St).

alloc_table(Itab, #luerl{ttab=Ts0,tfree=[N|Ns]}=St) ->
    T = init_table(Itab),
    %% io:fwrite("it1: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tfree=Ns}};
alloc_table(Itab, #luerl{ttab=Ts0,tfree=[],tnext=N}=St) ->
    T = init_table(Itab),
    %% io:fwrite("it2: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tnext=N+1}}.

init_table(Itab) ->
    D0 = ttdict:new(),
    A0 = array:new([{default,nil}]),		%Arrays with 'nil' as default
    Init = fun ({_,nil}, {D,A}) -> {D,A};	%Ignore nil values
	       ({K,V}, {D,A}) when is_number(K) ->
		   case ?IS_INTEGER(K, I) of
		       true when I >= 1 -> {D,array:set(I, V, A)};
		       _NegFalse -> {ttdict:store(K, V, D),A}
		   end;
	       ({K,V}, {D,A}) -> {ttdict:store(K, V, D),A}
	   end,
    {D1,A1} = lists:foldl(Init, {D0,A0}, Itab),
    #table{a=A1,d=D1,m=nil}.

free_table(#tref{i=N}, #luerl{ttab=Ts0,tfree=Ns}=St) ->
    %% io:fwrite("ft: ~p\n", [{N,?GET_TABLE(N, Ts0)}]),
    Ts1 = ?DEL_TABLE(N, Ts0),
    St#luerl{ttab=Ts1,tfree=[N|Ns]}.

%% get_table_keys(Keys, State) -> {Value,State}.
%% get_table_keys(Tab, Keys, State) -> {Value,State}.
%%  Search down tables which stops when no more tables.

get_table_keys(Keys, St) ->
    get_table_keys(St#luerl.g, Keys, St).

get_table_keys(Tab, [K|Ks], St0) ->
    {Val,St1} = luerl_emul:get_table_key(Tab, K, St0),
    get_table_keys(Val, Ks, St1);
get_table_keys(Val, [], St) -> {Val,St}.

%% set_table_keys(Keys, Val, State) -> State.
%% set_table_keys(Tab, Keys, Val, State) -> State.
%%  Setter down tables.

set_table_keys(Keys, Val, St) ->
    set_table_keys(St#luerl.g, Keys, Val, St).

set_table_keys(Tab, [K], Val, St) ->
    luerl_emul:set_table_key(Tab, K, Val, St);
set_table_keys(Tab0, [K|Ks], Val, St0) ->
    {Tab1,St1} = luerl_emul:get_table_key(Tab0, K, St0),
    set_table_keys(Tab1, Ks, Val, St1).

%% set_table_key(Tref, Key, Value, State) -> State.
%% get_table_key(Tref, Key, State) -> {Val,State}.
%%  Access tables, as opposed to the environment (which are also
%%  tables). Setting a value to 'nil' will clear it from the array but
%%  not from the table; however, we won't add a nil value.
%%  NOTE: WE ALWAYS RETURN A SINGLE VALUE!

set_table_key(#tref{}=Tref, Key, Val, St) when is_number(Key) ->
    case ?IS_INTEGER(Key, I) of
	true when I >= 1 -> set_table_int_key(Tref, Key, I, Val, St);
	_NegFalse -> set_table_key_key(Tref, Key, Val, St)
    end;
set_table_key(Tab, nil=Key, _, St) ->
    lua_error({illegal_index,Tab,Key}, St);
set_table_key(#tref{}=Tref, Key, Val, St) ->
    set_table_key_key(Tref, Key, Val, St);
set_table_key(Tab, Key, _, St) ->
    lua_error({illegal_index,Tab,Key}, St).

set_table_key_key(#tref{i=N}=Tab, Key, Val, #luerl{ttab=Ts0}=St) ->
    #table{d=Dict0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
    case ttdict:find(Key, Dict0) of
	{ok,_} ->				%Key exists
	    Dict1 = if Val =:= nil -> ttdict:erase(Key, Dict0);
		       true -> ttdict:store(Key, Val, Dict0)
		    end,
	    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
	    St#luerl{ttab=Ts1};
	error ->				%Key does not exist
	    case getmetamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value.
		    Dict1 = if Val =:= nil -> Dict0;
			       true -> ttdict:store(Key, Val, Dict0)
			    end,
		    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when element(1, Meth) =:= function ->
		    {_Ret, St1} = functioncall(Meth, [Tab,Key,Val], St),
		    St1;
		Meth -> set_table_key(Meth, Key, Val, St)
	    end
    end.

set_table_int_key(#tref{i=N}=Tab, Key, I, Val, #luerl{ttab=Ts0}=St) ->
    #table{a=Arr0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
    case array:get(I, Arr0) of
	nil ->					%Key does not exist
	    case getmetamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value, slightly faster (?)
		    Arr1 = if Val =:= nil -> Arr0;
			      true -> array:set(I, Val, Arr0)
			   end,
		    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when element(1, Meth) =:= function ->
		    {_Ret, St1} = functioncall(Meth, [Tab,Key,Val], St),
		    St1;
		Meth -> set_table_key(Meth, Key, Val, St)
	    end;
	_ ->					%Key exists
	    %% Can do this as 'nil' is default value of array.
	    Arr1 = array:set(I, Val, Arr0),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    St#luerl{ttab=Ts1}
    end.

get_table_key(#tref{}=Tref, Key, St) when is_number(Key) ->
    case ?IS_INTEGER(Key, I) of
	true when I >= 1 -> get_table_int_key(Tref, Key, I, St);
	_NegFalse -> get_table_key_key(Tref, Key, St)
    end;
get_table_key(#tref{}=Tref, Key, St) ->
    get_table_key_key(Tref, Key, St);
get_table_key(Tab, Key, St) ->			%Just find the metamethod
    case getmetamethod(Tab, <<"__index">>, St) of
	nil -> lua_error({illegal_index,Tab,Key}, St);
	Meth when element(1, Meth) =:= function ->
	    {Vs,St1} = functioncall(Meth, [Tab,Key], St),
	    {first_value(Vs),St1};
	Meth ->					%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.

get_table_key_key(#tref{i=N}=T, Key, #luerl{ttab=Ts}=St) ->
    #table{d=Dict,m=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case ttdict:find(Key, Dict) of
	{ok,Val} -> {Val,St};
	error ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St)
    end.

get_table_int_key(#tref{i=N}=T, Key, I, #luerl{ttab=Ts}=St) ->
    #table{a=A,m=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case array:get(I, A) of
	nil ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St);
	Val -> {Val,St}
    end.

get_table_metamethod(T, Meta, Key, Ts, St) ->
    case getmetamethod_tab(Meta, <<"__index">>, Ts) of
	nil -> {nil,St};
	Meth when element(1, Meth) =:= function ->
	    {Vs,St1} = functioncall(Meth, [T,Key], St),
	    {first_value(Vs),St1};
	Meth ->				%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.

%% set_local_var(Depth, Index, Var, Frames) -> Frames.
%% get_local_var(Depth, Index, Frames) -> Val.

set_local_var(1, I, V, [F|Fs]) ->
    [setelement(I, F, V)|Fs];
set_local_var(D, I, V, [F|Fs]) ->
    [F|set_local_var(D-1, I, V, Fs)].

get_local_var(1, I, [F|_]) -> element(I, F);
get_local_var(D, I, [_|Fs]) ->
    get_local_var(D-1, I, Fs).

%% set_env_var(Depth, Index, Val, Env, State) -> State.
%% get_env_var(Depth, Index, Env, State) -> Val.
%%  We must have the state as the environments are global in the
%%  state.

set_env_var(D, I, Val, Env, #luerl{ftab=Ft0}=St) ->
    Ft1 = set_env_var_1(D, I, Val, Env, Ft0),
    St#luerl{ftab=Ft1}.

set_env_var_1(1, I, V, [#fref{i=N}|_], Ft) ->
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft);
set_env_var_1(2, I, V, [_,#fref{i=N}|_], Ft) ->
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft);
set_env_var_1(D, I, V, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft).

get_env_var(D, I, Env, #luerl{ftab=Ft}) ->
    get_env_var_1(D, I, Env, Ft).

get_env_var_1(1, I, [#fref{i=N}|_], Ft) ->
    element(I, array:get(N, Ft));
get_env_var_1(2, I, [_,#fref{i=N}|_], Ft) ->
    element(I, array:get(N, Ft));
get_env_var_1(D, I, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    element(I, array:get(N, Ft)).

%% set_global_var(Var, Val, State) -> State.
%% get_global_var(Var, State) -> {Val,State}.
%%  _G a normal table with metatable so we must use the table
%%  functions.  However we can optimise a bit as we KNOW that _G is a
%%  table and the var is always a normal non-integer key.

set_global_var(Var, Val, #luerl{g=G}=St) ->
    set_table_key_key(G, Var, Val, St).

get_global_var(Var, #luerl{g=G}=St) ->
    get_table_key_key(G, Var, St).

%% load_chunk(FunctionDefCode, State) -> {Function,State}.
%% load_chunk(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a chunk from the compiler.

load_chunk(Code, St) -> load_chunk(Code, [], St).

load_chunk(#code{code=Code}, Env, St) ->
    load_function(Code, Env, St).

%% load_function(FunctionDefCode, State) -> {Function,State}.
%% load_function(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a compilefunction definition instructions returning a callable
%%  function. Currently it does nothing with the state.

load_function(F, St) -> load_function(F, [], St).

load_function([?FDEF(Lsz, Esz, Pars, Is)], Env, St) ->
    do_fdef(Lsz, Esz, Pars, Is, Env, St).

%% call(Function, State) -> {Return,State}.
%% call(Function, Args, State) -> {Return,State}.

call(Func, St) -> call(Func, [], St).

call(#function{}=Func, Args, St0) ->		%Already defined
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1};
call({function,_}=Func, Args, St0) ->		%Internal erlang function
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1}.

itrace_print(Format, Args) ->
    ?ITRACE_DO(io:fwrite(Format, Args)).

%% exp(_, _) ->
%%     error(boom).

-record(call_frame, {lvs,env}).			%Save these for the GC

%% emul(Instrs, State).
%% emul(Instrs, LocalVariables, Stack, Env, State).

emul(Is, St) ->
    emul(Is, {}, [], [], St).

emul([I|_]=Is, Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("~p\n", [{Lvs,Env}]),
		   stack_print(Stk),
		   io:fwrite("-> ~p\n", [I])
	       end),
    emul_1(Is, Lvs, Stk, Env, St);
emul([], Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("~p\n", [{Lvs,Env}]),
		   stack_print(Stk),
		   io:fwrite("-> []\n")
	       end),
    emul_1([], Lvs, Stk, Env, St).

stack_print([#call_frame{}|_]) -> io:fwrite(" ...\n");
stack_print([E|St]) ->
    io:fwrite(" ~p", [E]),
    stack_print(St);
stack_print([]) -> io:nl().

%% Expression instructions.
emul_1([?PUSH_LIT(L)|Is], Lvs, Stk, Env, St) ->
    emul(Is, Lvs, [L|Stk], Env, St);
emul_1([?PUSH_LVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [Val|Stk], Env, St);
emul_1([?PUSH_EVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, St),
    emul(Is, Lvs, [Val|Stk], Env, St);
emul_1([?PUSH_GVAR(K)|Is], Lvs, Stk, Env, St0) ->
    {Val,St1} = get_global_var(K, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);

emul_1([?PUSH_LAST_LIT(L)|Is], Lvs, Stk, Env, St) ->
    emul(Is, Lvs, [[L]|Stk], Env, St);
emul_1([?PUSH_LAST_LVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [[Val]|Stk], Env, St);
emul_1([?PUSH_LAST_EVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, St),
    emul(Is, Lvs, [[Val]|Stk], Env, St);
emul_1([?PUSH_LAST_GVAR(K)|Is], Lvs, Stk, Env, St0) ->
    {Val,St1} = get_global_var(K, St0),
    emul(Is, Lvs, [[Val]|Stk], Env, St1);

emul_1([?STORE_LVAR(D, I)|Is], Lvs0, [V|Stk], Env, St) ->
    Lvs1 = set_local_var(D, I, V, Lvs0),
    emul(Is, Lvs1, Stk, Env, St);
emul_1([?STORE_EVAR(D, I)|Is], Lvs, [V|Stk], Env, St0) ->
    St1 = set_env_var(D, I, V, Env, St0),
    emul(Is, Lvs, Stk, Env, St1);
emul_1([?STORE_GVAR(K)|Is], Lvs, [V|Stk], Env, St0) ->
    St1 = set_global_var(K, V, St0),
    emul(Is, Lvs, Stk, Env, St1);

emul_1([?GET_KEY|Is], Lvs, [Key,Tab|Stk], Env, St0) ->
    {Val,St1} = get_table_key(Tab, Key, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?GET_LIT_KEY(K)|Is], Lvs, [Tab|Stk], Env, St0) ->
    %% [?PUSH_LIT(K),?GET_KEY]
    {Val,St1} = get_table_key(Tab, K, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?SET_KEY|Is], Lvs, [Key,Tab,Val|Stk], Env, St0) ->
    St1 = set_table_key(Tab, Key, Val, St0),
    emul_1(Is, Lvs, Stk, Env, St1);
emul_1([?SET_LIT_KEY(Key)|Is], Lvs, [Tab,Val|Stk], Env, St0) ->
    %% [?PUSH_LIT(K),?SET_KEY]
    St1 = set_table_key(Tab, Key, Val, St0),
    emul_1(Is, Lvs, Stk, Env, St1);

emul_1([?SINGLE|Is], Lvs, [Val|Stk], Env, St) ->
    emul(Is, Lvs, [first_value(Val)|Stk], Env, St);
emul_1([?MULTIPLE|Is], Lvs, [Val|Stk], Env, St) ->
    emul(Is, Lvs, [multiple_value(Val)|Stk], Env, St);

emul_1([?BUILD_TAB(Fc, I)|Is], Lvs, Stk0, Env, St0) ->
    {Tab,Stk1,St1} = build_tab(Fc, I, Stk0, St0),
    emul(Is, Lvs, [Tab|Stk1], Env, St1);
emul_1([?FCALL(0)|Is], Lvs, Stk, Env, St) ->
    do_fcall_0(Is, Lvs, Stk, Env, St);
emul_1([?FCALL(1)|Is], Lvs, Stk, Env, St) ->
    do_fcall_1(Is, Lvs, Stk, Env, St);
emul_1([?FCALL(2)|Is], Lvs, Stk, Env, St) ->
    do_fcall_2(Is, Lvs, Stk, Env, St);
emul_1([?FCALL(Ac)|Is], Lvs, Stk, Env, St) ->
    do_fcall(Is, Lvs, Stk, Env, St, Ac);
emul_1([?TAIL_FCALL(Ac)|Is], Lvs, Stk, Env, St) ->
    do_tail_fcall(Is, Lvs, Stk, Env, St, Ac);
emul_1([?MCALL(K, 0)|Is], Lvs, Stk, Env, St) ->
    do_mcall_0(Is, Lvs, Stk, Env, St, K);
emul_1([?MCALL(K, 1)|Is], Lvs, Stk, Env, St) ->
    do_mcall_1(Is, Lvs, Stk, Env, St, K);
emul_1([?MCALL(K, 2)|Is], Lvs, Stk, Env, St) ->
    do_mcall_2(Is, Lvs, Stk, Env, St, K);
emul_1([?MCALL(K, Ac)|Is], Lvs, Stk, Env, St) ->
    do_mcall(Is, Lvs, Stk, Env, St, K, Ac);
emul_1([?OP(Op,1)|Is], Lvs, Stk, Env, St) ->
    do_op1(Is, Lvs, Stk, Env, St, Op);
emul_1([?OP(Op,2)|Is], Lvs, Stk, Env, St) ->
    do_op2(Is, Lvs, Stk, Env, St, Op);
emul_1([?FDEF(Lsz, Esz, Pars, Fis)|Is], Lvs, Stk, Env, St0) ->
    {Func,St1} = do_fdef(Lsz, Esz, Pars, Fis, Env, St0),
    emul(Is, Lvs, [Func|Stk], Env, St1);
%% Control instructions.
emul_1([?BLOCK(Lsz, Esz, Bis)|Is], Lvs0, Stk0, Env0, St0) ->
    {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz),
    emul(Is, Lvs1, Stk1, Env1, St1);
emul_1([?WHILE(Eis, Wis)|Is], Lvs, Stk, Env, St) ->
    do_while(Is, Lvs, Stk, Env, St, Eis, Wis);
emul_1([?REPEAT(Ris)|Is], Lvs, Stk, Env, St) ->
    do_repeat(Is, Lvs, Stk, Env, St, Ris);
emul_1([?AND_THEN(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St0) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    {Lvs1,Stk2,Env1,St1} = emul(T, Lvs, Stk1, Env, St0),
	    emul(Is, Lvs1, Stk2, Env1, St1);
	false ->
	    emul(Is, Lvs, Stk0, Env, St0)
    end;
emul_1([?OR_ELSE(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St0) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    emul(Is, Lvs, Stk0, Env, St0);
	false ->
	    {Lvs1,Stk2,Env1,St1} = emul(T, Lvs, Stk1, Env, St0),
	    emul(Is, Lvs1, Stk2, Env1, St1)
    end;
emul_1([?IF_TRUE(T)|Is], Lvs, [Val|Stk0], Env, St0) ->
    %% This is a statement and pops the boolean value.
    case boolean_value(Val) of
	true ->
	    {Lvs1,Stk1,Env1,St1} = emul(T, Lvs, Stk0, Env, St0),
	    emul(Is, Lvs1, Stk1, Env1, St1);
	false ->
	    emul(Is, Lvs, Stk0, Env, St0)
    end;
emul_1([?IF_FALSE(T)|Is], Lvs, [Val|Stk0], Env, St0) ->
    %% This is a statement and pops the boolean value.
    case boolean_value(Val) of
	true ->
	    emul(Is, Lvs, Stk0, Env, St0);
	false ->
	    {Lvs1,Stk1,Env1,St1} = emul(T, Lvs, Stk0, Env, St0),
	    emul(Is, Lvs1, Stk1, Env1, St1)
    end;
emul_1([?IF(True, False)|Is], Lvs0, Stk0, Env0, St0) ->
    {Lvs1,Stk1,Env1,St1} = do_if(Lvs0, Stk0, Env0, St0, True, False),
    emul(Is, Lvs1, Stk1, Env1, St1);
emul_1([?NFOR(V, Fis)|Is], Lvs, Stk, Env, St) ->
    do_numfor(Is, Lvs, Stk, Env, St, V, Fis);
emul_1([?GFOR(Vs, Fis)|Is], Lvs, Stk, Env, St) ->
    do_genfor(Is, Lvs, Stk, Env, St, Vs, Fis);
emul_1([?BREAK|_], Lvs, Stk, Env, St) ->
    throw({break,St#luerl.tag,Lvs,Stk,Env,St});
emul_1([?RETURN(0)|_], _, _, _, St) ->
    throw({return,St#luerl.tag,[],St});
emul_1([?RETURN(Ac)|_], _, Stk, _, St) ->
    {Ret,_} = pop_vals(Ac, Stk),
    throw({return,St#luerl.tag,Ret,St});
%% Stack instructions
emul_1([?POP|Is], Lvs, [_|Stk], Env, St) ->	%Just pop top off stack
    emul(Is, Lvs, Stk, Env, St);
emul_1([?POP2|Is], Lvs, [_,_|Stk], Env, St) ->	%Just pop top 2 off stack
    emul(Is, Lvs, Stk, Env, St);
emul_1([?SWAP|Is], Lvs, [S1,S2|Stk], Env, St) ->
    emul(Is, Lvs, [S2,S1|Stk], Env, St);
emul_1([?DUP|Is], Lvs, [V|_]=Stk, Env, St) ->
    emul_1(Is, Lvs, [V|Stk], Env, St);
emul_1([?PUSH_VALS(Vc)|Is], Lvs, [Vals|Stk0], Env, St) ->
    %% Pop list off the stack and push Vc vals from it.
    Stk1 = push_vals(Vc, Vals, Stk0),
    emul(Is, Lvs, Stk1, Env, St);
emul_1([?POP_VALS(Vc)|Is], Lvs, Stk0, Env, St) ->
    %% Pop Vc vals off the stack, put in a list and push.
    {Vals,Stk1} = pop_vals(Vc, Stk0),
    emul(Is, Lvs, [Vals|Stk1], Env, St);
emul_1([], Lvs, Stk, Env, St) ->
    {Lvs,Stk,Env,St}.

%% pop_vals(Count, Stack) -> {ValList,Stack}.
%% pop_vals(Count, Stack, ValList) -> {ValList,Stack}.
%%  Pop Count values off the stack and push onto the value list.
%%  First value is deepest. Always generates list.

pop_vals(0, Stk) -> {[],Stk};
pop_vals(C, [Vtail|Stk]) ->			%This a list tail
    pop_vals(C-1, Stk, Vtail).

pop_vals(0, Stk, Vs) -> {Vs,Stk};
pop_vals(1, [V|Stk], Vs) -> {[V|Vs],Stk};
pop_vals(2, [V2,V1|Stk], Vs) -> {[V1,V2|Vs],Stk};
pop_vals(C, [V2,V1|Stk], Vs) ->
    pop_vals(C-2, Stk, [V1,V2|Vs]).

%% push_vals(Count, ValList, Stack) -> {LastVal,Stack}.
%%  Push Count values from value list onto the stack. First value is
%%  deepest. Fill with 'nil' if not enough values.

push_vals(0, _, Stk) -> Stk;
push_vals(C, [V|Vs], Stk) ->
    push_vals(C-1, Vs, [V|Stk]);
push_vals(C, [], Stk) ->
    push_vals(C-1, [], [nil|Stk]).

%% do_block(Instrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize, BlockInstrs) -> ReturnFromEmul.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

%% do_block(Is, Lvs0, Stk0, Env, St0, 0, 0, Bis) ->
%%     %% No variables at all.
%%     {Lvs1,Stk1,_,St1} = emul(Bis, Lvs0, Stk0, Env, St0),
%%     emul(Is, Lvs1, Stk1, Env, St1);
%% do_block(Is, Lvs0, Stk0, Env, St0, 0, Esz, Bis) ->
%%     %% No local variables, only env variables.
%%     E = erlang:make_tuple(Esz, nil),
%%     {Fref,St1} = alloc_frame(E, St0),
%%     {Lvs1,Stk1,_,St2} = emul(Bis, Lvs0, Stk0, [Fref|Env], St1),
%%     emul(Is, Lvs1, Stk1, Env, St2);
%% do_block(Is, Lvs0, Stk0, Env, St0, Lsz, 0, Bis) ->
%%     %% No env variables, only local variables.
%%     L = erlang:make_tuple(Lsz, nil),
%%     {[_|Lvs1],Stk1,_,St1} = emul(Bis, [L|Lvs0], Stk0, Env, St0),
%%     emul(Is, Lvs1, Stk1, Env, St1);
%% do_block(Is, Lvs0, Stk0, Env, St0, Lsz, Esz, Bis) ->
%%     %% Both local and env variables.
%%     L = erlang:make_tuple(Lsz, nil),
%%     E = erlang:make_tuple(Esz, nil),
%%     {Fref,St1} = alloc_frame(E, St0),
%%     {[_|Lvs1],Stk1,_,St2} = emul(Bis, [L|Lvs0], Stk0, [Fref|Env], St1),
%%     emul(Is, Lvs1, Stk1, Env, St2).

%% do_block(BlockInstrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize) -> {LocalVars,Stack,Env,State}.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

do_block(Bis, Lvs, Stk, Env, St, 0, 0) ->
    %% No variables at all.
    emul(Bis, Lvs, Stk, Env, St);
do_block(Bis, Lvs0, Stk0, Env0, St0, 0, Esz) ->
    %% No local variables, only env variables.
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {Lvs1,Stk1,[_|Env1],St2} = emul(Bis, Lvs0, Stk0, [Fref|Env0], St1),
    {Lvs1,Stk1,Env1,St2};
do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, 0) ->
    %% No env variables, only local variables.
    L = erlang:make_tuple(Lsz, nil),
    {[_|Lvs1],Stk1,Env1,St1} = emul(Bis, [L|Lvs0], Stk0, Env0, St0),
    {Lvs1,Stk1,Env1,St1};
do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz) ->
    %% Both local and env variables.
    L = erlang:make_tuple(Lsz, nil),
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {[_|Lvs1],Stk1,[_|Env1],St2} = emul(Bis, [L|Lvs0], Stk0, [Fref|Env0], St1),
    {Lvs1,Stk1,Env1,St2}.

%% do_op1(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.
%% do_op2(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.

do_op1(Is, Lvs, [A|Stk], Env, St, Op) ->
    case op(Op, A) of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, {function,Meta}, []);
	{error,E} -> lua_error(E, St)
    end.

do_op2(Is, Lvs, [A2,A1|Stk], Env, St, Op) ->
    case op(Op, A1, A2) of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, {function,Meta}, []);
	{error,E} -> lua_error(E, St)
    end.

%% do_fdef(LocalSize, EnvSize, Pars, Instrs, Env, State) -> {Function,State}.

do_fdef(Lsz, Esz, Pars, Is, Env, St) ->
    {#function{lsz=Lsz,esz=Esz,pars=Pars,env=Env,b=Is},St}.

%% do_fcall_0(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall_1(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall_2(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall(Instrs, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_fcall_0(Is, Lvs, [Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, []).

do_fcall_1(Is, Lvs, [Alast,Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, Alast).

do_fcall_2(Is, Lvs, [Alast,A1,Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, [A1|Alast]).

do_fcall(Is, Lvs, Stk0, Env, St, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Func|Stk2] = Stk1,				%Get function
    functioncall(Is, Lvs, Stk2, Env, St, Func, Args).

%% functioncall(Function, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

functioncall(Func, Args, #luerl{stk=Stk}=St0) ->
    {Ret,St1} = functioncall(Func, Args, Stk, St0),
    {Ret,St1}.

%% functioncall(Instrs, LocalVars, Stk, Env, State, Func, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

functioncall(Is, Lvs, Stk0, Env, St0, Func, Args) ->
    Fr = #call_frame{lvs=Lvs,env=Env},
    Stk1 = [Fr|Stk0],
    {Ret,St1} = functioncall(Func, Args, Stk1, St0),
    emul(Is, Lvs, [Ret|Stk0], Env, St1).

%% do_tail_fcall(Instrs, Acc, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_tail_fcall(_Is, _Var, Stk, _Env, _St, 0) ->
    error({boom,[],Stk});
do_tail_fcall(_Is, _Var, Stk0, _Env, _St, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Func|Stk2] = Stk1,				%Get function
    error({boom,Func,Args,Stk2}).

%% do_mcall_0(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall_1(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall_2(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall(Instrs, LocalVars, Stack, Env, State, Method, ArgCount) ->
%%     ReturnFromEmul.

do_mcall_0(Is, Lvs, [Obj|Stk], Env, St, M) ->
    %% The object is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, []).

do_mcall_1(Is, Lvs, [Alast,Obj|Stk], Env, St, M) ->
    %% The object is on the stack and the argument is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, Alast).

do_mcall_2(Is, Lvs, [Alast,A1,Obj|Stk], Env, St, M) ->
    %% The object and 1st argument are on the stack, the 2nd is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, [A1|Alast]).

do_mcall(Is, Lvs, Stk0, Env, St, M, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Obj|Stk2] = Stk1,				%Get function
    methodcall(Is, Lvs, Stk2, Env, St, Obj, M, Args).

%% methodcall(Object, Method, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

methodcall(Obj, M, Args, St0) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    {Ret,St2} = functioncall(Val, [Obj|Args], St1#luerl.stk, St1),
	    {Ret,St2}
    end.

%% methodcall(Instrs, Var, Stk, Env, State, Object, Method, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

methodcall(Is, Lvs, Stk0, Env, St0, Obj, M, Args) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    Fr = #call_frame{lvs=Lvs,env=Env},
	    Stk1 = [Fr|Stk0],
	    {Ret,St2} = functioncall(Val, [Obj|Args], Stk1, St1),
	    emul(Is, Lvs, [Ret|Stk0], Env, St2)
    end.

%% functioncall(Function, Args, Stack, State) -> {Return,State}.
%%  Setup environment for function and do the actual call.

functioncall(#function{lsz=0,esz=0,env=Env,b=Fis}, _, Stk, St0) ->
    %% No variables at all.
    functioncall(Fis, [], Stk, Env, St0);
functioncall(#function{lsz=0,esz=Esz,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    %% No local variables, only env variables.
    E0 = erlang:make_tuple(Esz, nil),
    E1 = assign_env_pars(Pars, Args, E0),
    {Fref,St1} = alloc_frame(E1, St0),
    {Ret,St2} = functioncall(Fis, [], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall(#function{lsz=Lsz,esz=0,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    %% No env variables, only local variables.
    L0 = erlang:make_tuple(Lsz, nil),
    L1 = assign_local_pars(Pars, Args, L0),
    {Ret,St1} = functioncall(Fis, [L1], Stk, Env, St0),
    {Ret,St1};
functioncall(#function{lsz=Lsz,esz=Esz,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    L0 = erlang:make_tuple(Lsz, nil),
    E0 = erlang:make_tuple(Esz, nil),
    {L1,E1} = assign_pars(Pars, Args, L0, E0),
    {Fref,St1} = alloc_frame(E1, St0),
    {Ret,St2} = functioncall(Fis, [L1], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall({function,Func}, Args, Stk, #luerl{stk=Stk0}=St0) ->
    %% Here we must save the stack in state as function may need it.
    {Ret,St1} = Func(Args, St0#luerl{stk=Stk}),
    {Ret,St1#luerl{stk=Stk0}};			%Replace it
functioncall(Func, Args, Stk, St) ->
    case getmetamethod(Func, <<"__call">>, St) of
	nil -> lua_error({undef_function,Func}, St);
	Meta -> functioncall(Meta, [Func|Args], Stk, St)
    end.

functioncall(Fis, Lvs, Stk, Env, St0) ->
    Tag = St0#luerl.tag,
    %% Must use different St names else they become 'unsafe'.
    %% io:fwrite("fc: ~p\n", [{Lvs,Env,St0#luerl.env}]),
    try
	{_,_,_,Sta} = emul(Fis, Lvs, Stk, Env, St0),
	%%io:fwrite("fr: ~p\n", [{Tag,[]}]),
	{[],Sta}				%No return, no arguments
    catch
	throw:{return,Tag,Ret,Stb} ->
	    %%io:fwrite("fr: ~p\n", [{Tag,Ret,Stb#luerl.env}]),
	    {Ret,Stb};
	throw:{break,Tag,_,_,_,St} ->
	    lua_error({illegal_op,break}, St)
    end.

assign_local_pars([V|Vs], [A|As], Var) ->
    assign_local_pars(Vs, As, setelement(V, Var, A));
assign_local_pars([_|Vs], [], Var) ->
    assign_local_pars(Vs, [], Var);		%Var default is nil
assign_local_pars([], _, Var) -> Var;		%No vararg, drop remain args
assign_local_pars(V, As, Var) ->		%This is a vararg!
    setelement(V, Var, As).

assign_env_pars([V|Vs], [A|As], Var) ->
    assign_env_pars(Vs, As, setelement(-V, Var, A));
assign_env_pars([_|Vs], [], Var) ->
    assign_env_pars(Vs, [], Var);		%Var default is nil
assign_env_pars([], _, Var) -> Var;		%No vararg, drop remain args
assign_env_pars(V, As, Var) ->			%This is a vararg!
    setelement(-V, Var, As).

assign_pars([V|Vs], [A|As], L, E) when V > 0 ->
    assign_pars(Vs, As, setelement(V, L, A), E);
assign_pars([V|Vs], [A|As], L, E) ->		%V < 0
    assign_pars(Vs, As, L, setelement(-V, E, A));
assign_pars([_|Vs], [], L, E) ->
    assign_pars(Vs, [], L, E);		  %Var default is nil
assign_pars([], _, L, E) -> {L,E};		%No vararg, drop remain args
assign_pars(V, As, L, E) when V > 0 ->		%This is a vararg!
    {setelement(V, L, As),E};
assign_pars(V, As, L, E) ->			%This is a vararg!
    {L,setelement(-V, E, As)}.

%% do_repeat(Instrs, LocalVars, Stack, Env, State, RepeatInstrs) -> <emul>

do_repeat(Is, Lvs, Stk, Env, St, Ris) ->
    Do = fun (S) ->
		 repeat_loop(Ris, Lvs, Stk, Env, S)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

repeat_loop(Ris, Lvs0, Stk0, Env0, St0) ->
    {Lvs1,[Val|Stk1],Env1,St1} =
	emul(Ris, Lvs0, Stk0, Env0, St0),
    case boolean_value(Val) of
	true -> {Lvs1,St1};
	false -> repeat_loop(Ris, Lvs1, Stk1, Env1, St1)
    end.

%% do_while(Instrs, LocalVars, Stack, Env, State, WhileEis, WhileBis) ->
%%     <emul>

do_while(Is, Lvs, Stk, Env, St, Eis, Wis) ->
    Do = fun (S) ->
		 while_loop(Eis, Lvs, Stk, Env, S, Wis)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

while_loop(Eis, Lvs0, Stk0, Env0, St0, Wis) ->
    {Lvs1,[Val|Stk1],Env1,St1} =
	emul(Eis, Lvs0, Stk0, Env0, St0),
    case boolean_value(Val) of
	true ->
	    {Lvs2,Stk2,Env2,St2} =
		emul(Wis, Lvs1, Stk1, Env1, St1),
	    while_loop(Eis, Lvs2, Stk2, Env2, St2, Wis);
	false -> {Lvs1,St1}
    end.

loop_block(Is, Lvs0, Stk, Env, St0, Do) ->
    Tag = St0#luerl.tag,
    {Lvs2,St1} = try
		     Do(St0)
		 catch
		     throw:{break,Tag,Lvs1,_,_,St} -> {Lvs1,St}
		 end,
    %% Trim local variable stack.
    Lvs3 = lists:nthtail(length(Lvs2)-length(Lvs0), Lvs2),
    emul(Is, Lvs3, Stk, Env, St1).

%% do_if(Blocks, Else, Lvs, Stk, Env, Sy) ->
%%     do_if_blocks(Blocks, Else, Lvs, Stk, Env, St).

%% do_if_blocks([{T,B}|Ts], Else, Lvs0, Stk0, Env0, St0) ->
%%     {Lvs1,[Val|Stk1],Env1,St1} = emul(T, Lvs0, Stk0, Env0, St0),
%%     case boolean_value(Val) of
%% 	true -> emul(B, Lvs1, Stk1, Env1, St1);
%% 	false -> do_if_blocks(Ts, Lvs1, Stk1, Env1, St1)
%%     end;
%% do_if_blocks([], Else, Lvs, Stk, Env, St) ->
%%     emul(Else, Lvs, Stk, Env, St).

%% do_if(LocalVars, Stack, Env, State, TrueInstrs, FalseInstrs) ->
%%     {LocalVars,Stack,Env,State}.

do_if(Lvs, [Val|Stk], Env, St, True, False) ->
    case boolean_value(Val) of
	true -> emul(True, Lvs, Stk, Env, St);
	false -> emul(False, Lvs, Stk, Env, St)
    end.

%% do_if_block([?BLOCK(Lsz, Esz, Bis)], Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz),
%%     emul(Is, Lvs1, Stk1, Env1, St1);
%% do_if_block(Bis, Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = emul(Bis, Lvs0, Stk0, Env0, St0),
%%     emul(Is, Lvs1, Stk1, Env1, St1).

%% do_numfor(Instrs, LocalVars, Stack, Env, State, Varname, FromInstrs) ->
%%     <emul>

do_numfor(Is, Lvs, [Step,Limit,Init|Stk], Env, St, _, Fis) ->
    %% First check if we have numbers.
    case luerl_lib:tonumbers([Init,Limit,Step]) of
	[I,L,S] ->
	    Do = fun (St_) ->
			 numfor_loop(I, L, S, Fis, Lvs, Stk, Env, St_)
		 end,
	    loop_block(Is, Lvs, Stk, Env, St, Do);
	nil -> badarg_error(loop, [Init,Limit,Step], St)
    end.

numfor_loop(N, Limit, Step, Fis, Lvs0, Stk0, Env0, St0) ->
    %% Leave the counter at the top of the stack for code to get.
    itrace_print("nl: ~p\n", [{N,Stk0}]),
    if Step > 0.0, N =< Limit ->		%Keep going
	    {Lvs1,Stk1,Env1,St1} =
		emul(Fis, Lvs0, [N|Stk0], Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       Step < 0.0, N >= Limit ->		%Keep going
	    {Lvs1,Stk1,Env1,St1} =
		emul(Fis, Lvs0, [N|Stk0], Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       true -> {Lvs0,St0}				%Done!
    end.

%% do_genfor(Instrs, LocalVars, Stack, Env, State, Vars, FromInstrs) -> <emul>

do_genfor(Is, Lvs, [Val|Stk], Env, St, _, Fis) ->
    case Val of					%Export F, T, V
	[F] -> T = nil, V = nil;
	[F,T] -> V = nil;
	[F,T,V|_] -> ok;
	F -> T = nil, V = nil
    end,
    Do = fun (St_) ->
		 genfor_loop(F, T, V, Fis, Lvs, Stk, Env, St_)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

genfor_loop(Func, Tab, Val, Fis, Lvs0, Stk, Env, St0) ->
    {Vals,St1} = functioncall(Func, [Tab,Val], Stk, St0),
    case boolean_value(Vals) of
	true ->
	    {Lvs1,_,_,St2} =
		emul(Fis, Lvs0, [Vals|Stk], Env, St1),
	    genfor_loop(Func, Tab, hd(Vals), Fis, Lvs1, Stk, Env, St2);
	false -> {Lvs0,St1}
    end.

%% getmetamethod(Object1, Object2, Event, State) -> Metod | nil.
%% getmetamethod(Object, Event, State) -> Method | nil.
%% Get the metamethod for object(s).

getmetamethod(O1, O2, E, St) ->
    case getmetamethod(O1, E, St) of
	nil -> getmetamethod(O2, E, St);
	M -> M
    end.

getmetamethod(O, E, St) ->
    Meta = getmetatable(O, St),			%Can be nil
    getmetamethod_tab(Meta, E, St#luerl.ttab).

getmetatable(#tref{i=T}, #luerl{ttab=Ts}) ->
    (?GET_TABLE(T, Ts))#table.m;
getmetatable(#userdata{m=Meta}, _) -> Meta;
getmetatable(nil, #luerl{meta=Meta}) -> Meta#meta.nil;
getmetatable(B, #luerl{meta=Meta}) when is_boolean(B) ->
    Meta#meta.boolean;
getmetatable(N, #luerl{meta=Meta}) when is_number(N) ->
    Meta#meta.number;
getmetatable(S, #luerl{meta=Meta}) when is_binary(S) ->
    Meta#meta.string;
getmetatable(_, _) -> nil.			%Other types have no metatables

getmetamethod_tab(#tref{i=M}, E, Ts) ->
    #table{d=Mdict} = ?GET_TABLE(M, Ts),
    case ttdict:find(E, Mdict) of
	{ok,Mm} -> Mm;
	error -> nil
    end;
getmetamethod_tab(_, _, _) -> nil.		%Other types have no metatables

%% build_tab(FieldCount, Index, Stack, State) -> {TableRef,Stack,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the acc.

build_tab(Fc, I, [Last|Stk0], St0) ->
    Fs0 = build_tab_last(I, Last),
    {Fs1,Stk1} = build_tab_loop(Fc, Stk0, Fs0),
    %% io:fwrite("bt: ~p\n", [{Fc,I,Acc,Fs0,Fs1}]),
    {Tref,St1} = alloc_table(Fs1, St0),
    {Tref,Stk1,St1}.

build_tab_last(I, [V|Vs]) ->
    [{I,V}|build_tab_last(I+1.0, Vs)];
build_tab_last(_, []) -> [];
build_tab_last(_, Last) -> error({boom,build_tab_acc,Last}).

build_tab_loop(0, Stk, Fs) -> {Fs,Stk};
build_tab_loop(C, [V,K|Stk], Fs) ->
    build_tab_loop(C-1, Stk, [{K,V}|Fs]).

%% op(Op, Arg) -> {ok,Ret} | {meta,Func} | {error,Error}.
%% op(Op, Arg1, Arg2) -> {ok,Ret} | {meta,Func} | {error,Error}.
%%  The built-in operators. Always return a single value!

op('-', A) ->
    numeric_op('-', A, <<"__unm">>, fun (N) -> -N end);
op('not', A) -> {ok,not ?IS_TRUE(A)};
%% op('not', false) -> {[true]};
%% op('not', nil) -> {[true]};
%% op('not', _) -> {[false]};			%Everything else is false
op('#', B) when is_binary(B) -> {ok,float(byte_size(B))};
op('#', #tref{}=T) ->
    {meta,fun (_, St) -> luerl_lib_table:length(T, St) end};
op(Op, A) -> {error,{badarg,Op,[A]}}.

%% Numeric operators.
op('+', A1, A2) ->
    numeric_op('+', A1, A2, <<"__add">>, fun (N1,N2) -> N1+N2 end);
op('-', A1, A2) ->
    numeric_op('-', A1, A2, <<"__sub">>, fun (N1,N2) -> N1-N2 end);
op('*', A1, A2) ->
    numeric_op('*', A1, A2, <<"__mul">>, fun (N1,N2) -> N1*N2 end);
op('/', A1, A2) ->
    numeric_op('/', A1, A2, <<"__div">>, fun (N1,N2) -> N1/N2 end);
op('%', A1, A2) ->
    numeric_op('%', A1, A2, <<"__mod">>,
	       fun (N1,N2) -> N1 - round(N1/N2 - 0.5)*N2 end);
op('^', A1, A2) ->
    numeric_op('^', A1, A2, <<"__pow">>,
	       fun (N1,N2) -> math:pow(N1, N2) end);
%% Relational operators, getting close.
op('==', A1, A2) -> eq_op('==', A1, A2);
op('~=', A1, A2) -> neq_op('~=', A1, A2);
op('<=', A1, A2) -> le_op('<=', A1, A2);
op('>=', A1, A2) -> le_op('>=', A2, A1);
op('<', A1, A2) -> lt_op('<', A1, A2);
op('>', A1, A2) -> lt_op('>', A2, A1);
%% String operator.
op('..', A1, A2) -> concat_op(A1, A2);
%% Bad args here.
op(Op, A1, A2) -> {error,{badarg,Op,[A1,A2]}}.

%% numeric_op(Op, Arg, Event, Raw) -> {ok,Res} | {meta,Meta}.
%% numeric_op(Op, Arg, Arg, Event, Raw) -> {ok,Res} | {meta,Meta}.
%% eq_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% neq_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% lt_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% le_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% concat_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%%  Together with their metas straight out of the reference manual.

numeric_op(Op, A, E, Raw) ->
    case luerl_lib:tonumber(A) of
	nil ->					%Neither number nor string
	    {meta,fun (_, St) -> numeric_meta(Op, A, E, St) end}; 
	N -> {ok,Raw(N)}
    end.

numeric_op(Op, A1, A2, E, Raw) ->
    case luerl_lib:tonumber(A1) of
	nil -> {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
	N1 ->
	    case luerl_lib:tonumber(A2) of
		nil ->
		    {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
		N2 -> {ok,Raw(N1, N2)}
	    end
    end.

numeric_meta(Op, A, E, St0) ->
    case getmetamethod(A, E, St0) of
	nil -> badarg_error(Op, [A], St0);	%No meta method
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A], St0),
	    {first_value(Ret),St1}
    end.

numeric_meta(Op, A1, A2, E, St0) ->
    case getmetamethod(A1, A2, E, St0) of
	nil -> badarg_error(Op, [A1,A2], St0);	%No meta methods
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {first_value(Ret),St1}
    end.

eq_op(_Op, A1, A2) when A1 =:= A2 -> {ok,true};
eq_op(_Op, A1, A2) ->
    {meta,fun (_, St) -> eq_meta(A1, A2, St) end}.

neq_op(_Op, A1, A2) when A1 =:= A2 -> {ok,false};
neq_op(_Op, A1, A2) ->
    {meta,fun (_, St0) ->
		  {Ret,St1} = eq_meta(A1, A2, St0),
		  {not Ret,St1}
	  end}.

eq_meta(A1, A2, St0) ->
    %% Must have "same" metamethod here. How do we test?
    case getmetamethod(A1, <<"__eq">>, St0) of
	nil -> {false,St0};			%Tweren't no method
	Meta ->
	    case getmetamethod(A2, <<"__eq">>, St0) of
		Meta ->				%Must be the same method
		    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
		    {boolean_value(Ret),St1};
		_ -> {false,St0}
	    end
    end.

lt_op(_Op, A1, A2) when is_number(A1), is_number(A2) -> {ok,A1 < A2};
lt_op(_Op, A1, A2) when is_binary(A1), is_binary(A2) -> {ok,A1 < A2};
lt_op(Op, A1, A2) ->
    {meta,fun (_, St) -> lt_meta(Op, A1, A2, St) end}.

lt_meta(Op, A1, A2, St0) ->
    case getmetamethod(A1, A2, <<"__lt">>, St0) of
	nil -> badarg_error(Op, [A1,A2], St0);
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {boolean_value(Ret),St1}
    end.

le_op(_Op, A1, A2) when is_number(A1), is_number(A2) -> {ok,A1 =< A2};
le_op(_Op, A1, A2) when is_binary(A1), is_binary(A2) -> {ok,A1 =< A2};
le_op(Op, A1, A2) ->
    {meta,fun (_, St) -> le_meta(Op, A1, A2, St) end}.

le_meta(Op, A1, A2, St0) ->
    %% Must check for first __le then __lt metamethods.
    case getmetamethod(A1, A2, <<"__le">>, St0) of
	nil ->
	    %% Try for not (Op2 < Op1) instead.
	    case getmetamethod(A1, A2, <<"__lt">>, St0) of
		nil -> badarg_error(Op, [A1,A2], St0);
		Meta ->
		    {Ret,St1} = functioncall(Meta, [A2,A1], St0),
		    {not boolean_value(Ret),St1}
	    end;
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {boolean_value(Ret),St1}
    end.

concat_op(A1, A2) ->
    case luerl_lib:tostring(A1) of
	nil ->
	    {meta,fun (_, St) -> concat_meta(A1, A2, St) end};
	S1 ->
	    case luerl_lib:tostring(A2) of
		nil ->
		    {meta,fun (_, St) -> concat_meta(A1, A2, St) end};
		S2 ->
		    {ok,<<S1/binary,S2/binary>>}
	    end
    end.

concat_meta(A1, A2, St0) ->
    case getmetamethod(A1, A2, <<"__concat">>, St0) of
	nil -> badarg_error('..', [A1,A2], St0);
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {first_value(Ret),St1}
    end.

%% boolean_value(Rets) -> boolean().
%%  Return the "boolean" value of a value/function return list.

boolean_value([nil|_]) -> false;
boolean_value([false|_]) -> false;
boolean_value([_|_]) -> true;
boolean_value([]) -> false;
boolean_value(nil) -> false;
boolean_value(false) -> false;
boolean_value(_) -> true.

%% first_value(Rets) -> Value.

first_value([V|_]) -> V;
first_value([]) -> nil.

%%multiple_value(nil) -> [];			%Or maybe [nil]?
multiple_value(V) -> [V].

%% gc(State) -> State.
%%  The garbage collector. Its main job is to reclaim unused tables
%%  and frames. It is a mark/sweep collector which passes over all
%%  objects and marks tables and frames which it has seen. All unseen
%%  tables and frames are then freed and their indexes added to the
%%  free lists.

gc(#luerl{ttab=Tt0,tfree=Tf0,ftab=Ft0,ffree=Ff0,g=G,stk=Stk,meta=Meta}=St) ->
    %% The root set consisting of global table and stack.
    Root = [Meta#meta.nil,Meta#meta.boolean,Meta#meta.number,Meta#meta.string,
	    G|Stk],
    %% Mark all seen tables and frames, i.e. return them.
    {SeenT,SeenF} = mark(Root, [], [], [], Tt0, Ft0),
    %% io:format("gc: ~p\n", [{SeenT,SeenF}]),
    %% Free unseen tables and add freed to free list.
    {Tf1,Tt1} = filter_tables(SeenT, Tf0, Tt0),
    {Ff1,Ft1} = filter_frames(SeenF, Ff0, Ft0),
    St#luerl{ttab=Tt1,tfree=Tf1,ftab=Ft1,ffree=Ff1}.

%% mark(ToDo, MoreTodo, SeenTabs, SeenFrames, Tabs, Frames) ->
%%     {SeenTabs,SeenFrames}.
%% Scan over all live objects and mark seen tables by adding them to
%% the seen list.

mark([{in_table,_}=_T|Todo], More, St, Sf, Tt, Ft) ->
    %%io:format("gc: ~p\n", [_T]),
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#tref{i=T}|Todo], More, St0, Sf, Tt, Ft) ->
    case ordsets:is_element(T, St0) of
	true ->					%Already done
	    mark(Todo, More, St0, Sf, Tt, Ft);
	false ->				%Mark it and add to todo
	    St1 = ordsets:add_element(T, St0),
	    #table{a=Arr,d=Dict,m=Meta} = ?GET_TABLE(T, Tt),
	    %% Have to be careful where add Tab and Meta as Tab is
	    %% [{Key,Val}], Arr is array and Meta is
	    %% nil|#tref{i=M}. We want lists.
	    Aes = array:sparse_to_list(Arr),
	    Des = ttdict:to_list(Dict),
	    mark([Meta|Todo], [[{in_table,T}],Des,Aes,[{in_table,-T}]|More],
		 St1, Sf, Tt, Ft)
    end;
mark([#fref{i=F}|Todo], More, St, Sf0, Tt, Ft) ->
    case ordsets:is_element(F, Sf0) of
	true ->					%Already done
	    mark(Todo, More, St, Sf0, Tt, Ft);
	false ->				%Mark it and add to todo
	    Sf1 = ordsets:add_element(F, Sf0),
	    Ses = tuple_to_list(array:get(F, Ft)),
	    mark(Todo, [Ses|More], St, Sf1, Tt, Ft)
    end;
mark([#function{env=Env}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, [Env|More], St, Sf, Tt, Ft);
%% Catch these as they would match table key-value pair.
mark([{function,_}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#thread{}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#userdata{m=Meta}|Todo], More, St, Sf, Tt, Ft) ->
    mark([Meta|Todo], More, St, Sf, Tt, Ft);
mark([#call_frame{lvs=Lvs,env=Env}|Todo], More0, St, Sf, Tt, Ft) ->
    More1 = [ tuple_to_list(Lv) || Lv <- Lvs ] ++ [Env|More0],
    mark(Todo, More1, St, Sf, Tt, Ft);
mark([{K,V}|Todo], More, St, Sf, Tt, Ft) ->	%Table key-value pair
    %%io:format("mt: ~p\n", [{K,V}]),
    mark([K,V|Todo], More, St, Sf, Tt, Ft);
mark([_|Todo], More, St, Sf, Tt, Ft) ->		%Can ignore everything else
    mark(Todo, More, St, Sf, Tt, Ft);
mark([], [M|More], St, Sf, Tt, Ft) ->
    mark(M, More, St, Sf, Tt, Ft);
mark([], [], St, Sf, _, _) -> {St,Sf}.

%% filter_tables(Seen, Free, Tables) -> {Free,Tables}.
%% filter_frames(Seen, Free, Frames) -> {Free,Frames}.
%%  Filter tables/frames and return updated free lists and
%%  tables/frames.

filter_tables(Seen, Tf0, Tt0) ->
    Tf1 = ?FOLD_TABLES(fun (K, _, Free) ->
			       case ordsets:is_element(K, Seen) of
				   true -> Free;
				   false -> [K|Free]
			       end
		       end, Tf0, Tt0),
    Tt1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Tt0),
    {Tf1,Tt1}.

filter_frames(Seen, Ff0, Ft0) ->
    %% Unfortunately there is no array:sparse_mapfoldl.
    Ff1 = array:sparse_foldl(fun (F, _, Free) ->
				     case ordsets:is_element(F, Seen) of
					 true -> Free;
					 false -> [F|Free]
				     end
			     end, Ff0, Ft0),
    Ft1 = array:sparse_map(fun (F, Fd) ->
				   case ordsets:is_element(F, Seen) of
				       true -> Fd;
				       false -> undefined
				   end
			   end, Ft0),
    {Ff1,Ft1}.
