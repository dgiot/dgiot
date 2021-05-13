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

%% File    : luerl.hrl
%% Author  : Robert Virding
%% Purpose : The basic macros/records for Luerl.

%% We include the whole environment in one structure even if fields
%% come from logically different parts. This make it easier to pass
%% around but does mean that there will be more explicit fiddleling to
%% get it right. See block/2 and functioncall/4 for examples of this.

-record(luerl, {ttab,tfree,tnext,		%Table table, free, next
		ftab,ffree,fnext,		%Frame table, free, next
		g,				%Global table
		%%
		stk=[],				%Current stack
		%%
		meta=[],			%Data type metatables
		tag				%Unique tag
	       }).

-record(heap, {ttab,tfree,tnext,
	       ftab,ffree,fnext}).

%% -record(etab, {tabs=[],free=[],next=0}).	%Tables structure
%% -record(eenv, {env=[]}).			%Environment
%% -record(luerl, {tabs,env}).			%Full state

%% Metatables for atomic datatypes.

-record(meta, {nil=nil,
	       boolean=nil,
	       number=nil,
	       string=nil}).

%% Data types.

-record(tref, {i}).				%Table reference, index
-record(table, {a,d=[],m=nil}).			%Table type, array, dict, meta
-record(userdata, {d,m=nil}).			%Userdata type, data and meta
-record(thread, {}).				%Thread type
%% There are two function types, this the Lua one, and an Erlang one
%% with the same name. So no type for it.
-record(function,{lsz,				%Local var size
		  esz,				%Env var size
		  env,				%Environment
		  pars,				%Parameters
		  b}).				%Code block

-record(fref, {i}).				%Frame reference, index


-define(IS_INTEGER(N), (float(round(N)) =:= N)).
-define(IS_INTEGER(N,I), (float(I=round(N)) =:= N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

%% Different methods for storing tables in the global data #luerl{}.
%% Access through macros to allow testing with different storage
%% methods. This is inefficient with ETS tables where it would
%% probably be better to use bags and acces with match/select.

%% Set which table store to use.
-define(TS_USE_ARRAY, true).

-ifdef(TS_USE_ORDDICT).
%% Using orddict to handle tables.
-define(MAKE_TABLE(), orddict:new()).
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
-define(FILTER_TABLES(Pred, Ts), orddict:filter(Pred, Ts)).
-define(FOLD_TABLES(Fun, Acc, Ts), orddict:fold(Fun, Acc, Ts)).
-endif.

-ifdef(TS_USE_ARRAY).
%% Use arrays to handle tables.
-define(MAKE_TABLE(), array:new()).
-define(GET_TABLE(N, Ar), array:get(N, Ar)).
-define(SET_TABLE(N, T, Ar), array:set(N, T, Ar)).
-define(UPD_TABLE(N, Upd, Ar),
	array:set(N, (Upd)(array:get(N, Ar)), Ar)).
-define(DEL_TABLE(N, Ar), array:reset(N, Ar)).
-define(FILTER_TABLES(Pred, Ar),
	((fun (___Def) ->
		  ___Fil = fun (___K, ___V) ->
				   case Pred(___K, ___V) of
				       true -> ___V;
				       false -> ___Def
				   end
			   end,
		  array:sparse_map(___Fil, Ar)
	  end)(array:default(Ar)))).
-define(FOLD_TABLES(Fun, Acc, Ar), array:sparse_foldl(Fun, Acc, Ar)).
-endif.

-ifdef(TS_USE_PD).
%% Use the process dictionary to handle tables.
-define(MAKE_TABLE(), ok).
-define(GET_TABLE(N, Pd), get(N)).
-define(SET_TABLE(N, T, Pd), put(N, T)).
-define(UPD_TABLE(N, Upd, Pd), put(N, (Upd)(get(N)))).
-define(DEL_TABLE(N, Pd), erase(N)).
-define(FILTER_TABLES(Pred, Pd), Pd).		%This needs work
-define(FOLD_TABLES(Fun, Acc, Pd), Pd).		%This needs work
-endif.

-ifdef(TS_USE_ETS).
%% Use ETS to handle tables. Must get return values right!
-define(MAKE_TABLE(),ets:new(luerl_tables, [set])).
-define(GET_TABLE(N, E), ets:lookup_element(E, N, 2)).
-define(SET_TABLE(N, T, E), begin ets:insert(E, {N,T}), E end).
-define(UPD_TABLE(N, Upd, E),
	begin ets:update_element(E, N, {2,(Upd)(ets:lookup_element(E, N, 2))}),
	      E end).
-define(DEL_TABLE(N, E), begin ets:delete(E, N), E end).
-define(FILTER_TABLES(Pred, E), E).		%This needs work
-define(FOLD_TABLES(Fun, Acc, E),
	ets:foldl(fun ({___K, ___T}, ___Acc) -> Fun(___K, ___T, ___Acc) end,
		  Acc, E)).
-endif.
