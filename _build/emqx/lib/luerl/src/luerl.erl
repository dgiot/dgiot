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

%% File    : luerl.erl
%% Authors : Robert Virding, Henning Diedrich
%% Purpose : Basic LUA 5.2 interface.

-module(luerl).

-include("luerl.hrl").

-export([eval/1,eval/2,evalfile/1,evalfile/2,
	 do/1,do/2,dofile/1,dofile/2,
	 load/1,load/2,loadfile/1,loadfile/2,path_loadfile/2,path_loadfile/3,
	 load_module/3,load_module1/3,
	 call/2,call/3,call_chunk/2,call_chunk/3,
	 call_function/2,call_function/3,call_function1/3,function_list/2,
	 get_table/2,get_table1/2,set_table/3,set_table1/3,set_table1/4,
	 call_method/2,call_method/3,call_method1/3,method_list/2,
	 init/0,stop/1,gc/1,
	 encode/2,encode_list/2,decode/2,decode_list/2]).

%% luerl:eval(String|Binary|Form[, State]) -> Result.
eval(Chunk) ->
    eval(Chunk, init()).

eval(Chunk, St0) ->
    try do(Chunk, St0) of
        {Ret,St1} -> {ok, decode_list(Ret, St1)}
    catch
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%% luerl:evalfile(Path[, State]) -> {ok, Result} | {error,Reason}.
evalfile(Path) ->
    evalfile(Path, init()).

evalfile(Path, St0) ->
    try dofile(Path, St0) of
        {Ret,St1} -> {ok, decode_list(Ret, St1)}
    catch
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%% luerl:do(String|Binary|Form[, State]) -> {Result, NewState}

do(SBC) -> do(SBC, init()).

do(S, St0) when is_binary(S); is_list(S) ->
    {ok,Func,St1} = load(S, St0),
    luerl_emul:call(Func, St1);
do(Func, St) ->
    luerl_emul:call(Func, St).

%% luerl:dofile(Path[, State]) -> {Result, NewState}.

dofile(Path) ->
    dofile(Path, init()).

dofile(Path, St0) ->
    {ok,Func,St1} = loadfile(Path, St0),
    luerl_emul:call(Func, St1).

%% load(String|Binary) -> {ok,Function,NewState}.

load(Str) -> load(Str, init()).

load(Bin, St) when is_binary(Bin) ->
    load(binary_to_list(Bin), St);
load(Str, St0) when is_list(Str) ->
    case luerl_comp:string(Str) of
	{ok,Chunk} ->
	    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
	    {ok,Func,St1};
	{error,_,_}=E -> E
    end.

%% loadfile(FileName) -> {ok,Function,NewState}.
%% loadfile(FileName, State) -> {ok,Function,NewState}.

loadfile(Name) -> loadfile(Name, init()).

loadfile(Name, St0) ->
    case luerl_comp:file(Name) of
	{ok,Chunk} ->
	    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
	    {ok,Func,St1};
	{error,_,_}=E -> E
    end.

%% path_loadfile(FileName, State) -> {ok,Function,FullName,State}.
%% path_loadfile(Path, FileName, State) -> {ok,Function,FullName,State}.
%%  We manually step down the path to get the correct handling of
%%  filenames by the compiler.

path_loadfile(Name, St) ->
    Path = case os:getenv("LUA_LOAD_PATH") of
	       false -> [];			%You get what you asked for
	       Env ->
		   %% Get path separator depending on os type.
		   Sep = case os:type() of
			     {win32,_} -> ";";
			     _ -> ":"		%Unix
			 end,
		   string:tokens(Env, Sep)	%Split into path list
	   end,
    path_loadfile(Path, Name, St).

path_loadfile([Dir|Dirs], Name, St0) ->
    Full = filename:join(Dir, Name),
    case loadfile(Full, St0) of
	{ok,Func,St1} ->
	    {ok,Func,Full,St1};
	{error,[{_,_,enoent}],_} ->		%Couldn't find the file
	    path_loadfile(Dirs, Name, St0);
	Error -> Error
    end;
path_loadfile([], _, _) ->
    {error,[{none,file,enoent}],[]}.

%% load_module(TablePath, ModuleName, State) -> State.
%% load_module1(LuaTablePath, ModuleName, State) -> State.
%%  Load module and add module table to the path.

load_module(Fp, Mod, St0) when is_list(Fp) ->
    {Lfp,St1} = encode_list(Fp, St0),
    load_module1(Lfp, Mod, St1);
load_module(_, _,_) -> error(badarg).

load_module1(Lfp, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    luerl_emul:set_table_keys(Lfp, Tab, St1).

%% init() -> State.
init() -> luerl_emul:init().

%% call(Chunk, Args, State) -> {Result,State}

call(C, As) -> call_chunk(C, As).

call(C, As, St) -> call_chunk(C, As, St).

call_chunk(C, As) -> call_chunk(C, As, init()).

call_chunk(C, As, St0) ->
    {Las,St1} = encode_list(As, St0),
    {Lrs,St2} = luerl_emul:call(C, Las, St1),
    Rs = decode_list(Lrs, St2),
    {Rs,St2}.

%% call_function(Table, Args) -> {Result,State}.
%% call_function(TablePath, Args, State) -> {Result,State}.
%% call_function1(LuaTablePath | Func, LuaArgs, State) -> {LuaResult,State}.

call_function(Fp, As) ->
    call_function(Fp, As, init()).

call_function(Fp, As, St0) ->
    %% Encode the input arguments.
    {Lfp,St1} = encode_list(Fp, St0),
    {Las,St2} = encode_list(As, St1),
    %% Find the function definition and call function.
    {Lrs,St3} = call_function1(Lfp, Las, St2),
    Rs = decode_list(Lrs, St3),
    {Rs,St3}.

call_function1(Lfp, Las, St0) when is_list(Lfp) ->
    {F,St1} = luerl_emul:get_table_keys(Lfp, St0),
    luerl_emul:functioncall(F, Las, St1);
call_function1(F, Las, St) ->
    luerl_emul:functioncall(F, Las, St).

%% function_list(Keys, State) -> {V,State}.
%%  Go down a list of keys and return final value.

function_list(Ks, St) -> luerl_emul:get_table_keys(Ks, St).

%% call_method(FuncPath, Args) -> {Result,State}.
%% call_method(FuncPath, Args, State) -> {Result,State}.
%% call_method1(FuncPath | FuncPath, Args, State) -> {Result,State}.

call_method(Fp, As) ->
    call_method(Fp, As, init()).

call_method(Fp, As, St0) ->
    %% Encode the input arguments.
    {Lfp,St1} = encode_list(Fp, St0),
    {Las,St2} = encode_list(As, St1),
    %% Find the object and method definition and call method.
    {O,M,St3} = method_list(Lfp, St2),
    {Lrs,St4} = luerl_emul:functioncall(M, [O|Las], St3),
    Rs = decode_list(Lrs, St4),
    {Rs,St4}.

call_method1(Fp, Las, St0) ->
    %% Find the object and method definition and call method.
    {O,M,St1} = method_list(Fp, St0),
    luerl_emul:functioncall(M, [O|Las], St1).

method_list([G|Ks], St0) ->
    {First,St1} = luerl_emul:get_global_key(G, St0),
    method_list(First, Ks, St1).

method_list(Tab, [K], St0) ->
    {Func,St1} = luerl_emul:get_table_key(Tab, K, St0),
    {Tab,Func,St1};
method_list(Tab, [K|Ks], St0) ->
    {Next,St1} = luerl_emul:get_table_key(Tab, K, St0),
    method_list(Next, Ks, St1);
method_list(_, _, _) -> error(badarg).

%% get_table(TablePath, State) -> {Result, State}.
%% Go down a list of keys and return decoded final value.

get_table(Fp, St0) when is_list(Fp) ->
    {Lfp,St1} = encode_list(Fp, St0),
    {V,St2} = luerl_emul:get_table_keys(Lfp, St1),
    Vd = decode(V, St2),
    {Vd,St2};
get_table(_,_) -> error(badarg).

%% get_table1(LuaTablePath, State) -> {LuaResult, State}.

get_table1(Fp, St) when is_list(Fp) ->
    luerl_emul:get_table_keys(Fp, St);
get_table1(_,_) -> error(badarg).

%% set_table(TablePath, Value, State) -> State.
%%  Go down a list of keys and set final key to Value.

set_table(Fp, V, St0) when is_list(Fp) ->
    {Lfp,St1} = encode_list(Fp, St0),
    {Lv, St2} = encode(V, St1),
    set_table1(Lfp, Lv, St2);
set_table(_,_,_) -> error(badarg).

%% set_table1(LuaTablePath, Value, State) -> State.
%%  Must explicitly read table key to get

set_table1(Lfp, Lv, St) ->
    luerl_emul:set_table_keys(Lfp, Lv, St).

%% set_table1(Table, Key, Value, State) -> State.
%%  Must explicitly read table key to get

set_table1(Tab, Key, Lv, St) ->
    luerl_emul:set_table_key(Tab, Key, Lv, St).

%% stop(State) -> GCedState.
stop(St) ->
    luerl_emul:gc(St).

%% gc(State) -> State.
gc(St) -> luerl_emul:gc(St).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% encode_list([Term], State) -> {[LuerlTerm],State}.
%% encode(Term, State) -> {LuerlTerm,State}.

encode_list(Ts, St) ->
    lists:mapfoldl(fun encode/2, St, Ts).

encode(nil, St) -> {nil,St};
encode(false, St) -> {false,St};
encode(true, St) -> {true,St};
encode(B, St) when is_binary(B) -> {B,St};
encode(A, St) when is_atom(A) -> {atom_to_binary(A, latin1),St};
encode(I, St) when is_integer(I) -> {float(I),St};
encode(F, St) when is_float(F) -> {F,St};
encode(F, St) when ?IS_MAP(F) -> encode(maps:to_list(F), St);
encode(L, St0) when is_list(L) ->
    {Es,{_,St1}} = lists:mapfoldl(fun ({K0,V0}, {I,S0}) ->
					  {K1,S1} = encode(K0, S0),
					  {V1,S2} = encode(V0, S1),
					  {{K1,V1},{I,S2}};
				      (V0, {I,S0}) ->
					  {V1,S1} = encode(V0, S0),
					  {{I,V1},{I+1,S1}}
			      end, {1.0,St0}, L),
    {T,St2} = luerl_emul:alloc_table(Es, St1),
    {T,St2};					%No more to do for now
encode(F, St) when is_function(F, 2) ->
    F1 = fun(Args, State) ->
		 Args1 = decode_list(Args, State),
		 {Res, State1} = F(Args1, State),
		 encode_list(Res, State1)
	 end,
    {{function, F1}, St};
encode(F, St) when is_function(F, 1) ->
    F1 = fun(Args, State) ->
		 Args1 = decode_list(Args, State),
		 Res = F(Args1),
		 encode_list(Res, State)
	 end,
    {{function, F1}, St};
encode(_, _) -> error(badarg).			%Can't encode anything else

%% decode_list([LuerlTerm], State) -> [Term].
%% decode(LuerlTerm, State) -> Term.
%%  In decode we track of which tables we have seen to detect
%%  recursive references and generate an error when that occurs.

decode_list(Lts, St) ->
    lists:map(fun (Lt) -> decode(Lt, St) end, Lts).

decode(LT, St) ->
    %% Catch errors to clean up call stack.
    try decode(LT, St, [])
    catch
	error:E ->
	    erlang:raise(error, E, [{?MODULE,decode,2}])
    end.

decode(nil, _, _) -> nil;
decode(false, _, _) -> false;
decode(true, _, _) -> true;
decode(B, _, _) when is_binary(B) -> B;
decode(N, _, _) when is_number(N) -> N;
decode(#tref{i=N}, St, In) ->
    decode_table(N, St, In);
decode({function,Fun}, _, _) -> {function,Fun};
decode(#function{}=Fun, State, _) ->
    F = fun(Args) ->
		{Args1, State1} = encode_list(Args, State),
		{Ret, State2} = luerl_emul:functioncall(Fun, Args1, State1),
		decode_list(Ret, State2)
	end,
    {function, F};
decode(_, _, _) -> error(badarg).		%Shouldn't have anything else

decode_table(N, St, In0) ->
    case lists:member(N, In0) of
	true -> error(recursive_data);		%Been here before
	false ->
	    In1 = [N|In0],			%We are in this as well
	    case ?GET_TABLE(N, St#luerl.ttab) of
		#table{a=Arr,d=Dict} ->
		    Fun = fun (K, V, Acc) ->
				  [{decode(K, St, In1),decode(V, St, In1)}|Acc]
			  end,
		    Ts = ttdict:fold(Fun, [], Dict),
		    array:sparse_foldr(Fun, Ts, Arr);
		_Undefined -> error(badarg)
	    end
    end.
