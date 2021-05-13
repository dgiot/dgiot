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

%% File    : luerl_lib_basic.erl
%% Author  : Robert Virding
%% Purpose : The basic library for Luerl.

-module(luerl_lib_basic).

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

%% Export some functions which can be called from elsewhere.
-export([tostring/1,tostring/2]).

-import(luerl_lib, [lua_error/2,badarg_error/3]). %Shorten these

install(St) ->
    luerl_emul:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"_VERSION">>,<<"Lua 5.2">>},		%We are optimistic
     {<<"assert">>,{function,fun assert/2}},
     {<<"collectgarbage">>,{function,fun collectgarbage/2}},
     {<<"dofile">>,{function,fun dofile/2}},
     {<<"eprint">>,{function,fun eprint/2}},
     {<<"error">>,{function,fun error/2}},
     {<<"getmetatable">>,{function,fun getmetatable/2}},
     {<<"ipairs">>,{function,fun ipairs/2}},
     {<<"load">>,{function,fun load/2}},
     {<<"loadfile">>,{function,fun loadfile/2}},
     {<<"loadstring">>,{function,fun loadstring/2}}, %For Lua 5.1 compatibility
     {<<"next">>,{function,fun next/2}},
     {<<"pairs">>,{function,fun pairs/2}},
     {<<"pcall">>,{function,fun pcall/2}},
     {<<"print">>,{function,fun print/2}},
     {<<"rawequal">>,{function,fun rawequal/2}},
     {<<"rawget">>,{function,fun rawget/2}},
     {<<"rawlen">>,{function,fun rawlen/2}},
     {<<"rawset">>,{function,fun rawset/2}},
     {<<"select">>,{function,fun select/2}},
     {<<"setmetatable">>,{function,fun setmetatable/2}},
     {<<"tonumber">>,{function,fun tonumber/2}},
     {<<"tostring">>,{function,fun tostring/2}},
     {<<"type">>,{function,fun type/2}},
     {<<"unpack">>,{function,fun unpack/2}}	%For Lua 5.1 compatibility
    ].

assert(As, St) ->
    case luerl_lib:boolean_value(As) of
	true -> {As,St};
	false ->
	    M = case As of
		    [_,M0|_] -> M0;
		    _ -> <<"assertion failed">>
		end,
	    lua_error({assert_error,M}, St)
    end.

collectgarbage([], St) -> collectgarbage([<<"collect">>], St);
collectgarbage([<<"collect">>|_], St) ->
    %% {[],luerl_emul:gc(St)};
    {[],St};					%No-op for the moment
collectgarbage(_, St) ->			%Ignore everything else
    {[],St}.

eprint(Args, St) ->
    lists:foreach(fun (#tref{i=N}) ->
			  T = ?GET_TABLE(N, St#luerl.ttab),
			  io:format("~w ", [T]);
		      (A) -> io:format("~w ", [A])
		  end, Args),
    io:nl(),
    {[],St}.

-spec error(_, _) -> no_return().

error([{tref, _}=T|_], St0) ->
    case luerl_emul:getmetamethod(T, <<"__tostring">>, St0) of
        nil -> lua_error({error_call, T}, St0);
        Meta ->
            {[Ret|_], St1} = luerl_emul:functioncall(Meta, [T], St0),
            lua_error({error_call, Ret}, St1)
    end;
error([M|_], St) -> lua_error({error_call, M}, St);	%Never returns!
error(As, St) -> badarg_error(error, As, St).

%% ipairs(Args, State) -> {[Func,Table,FirstKey],State}.
%%  Return a function which on successive calls returns successive
%%  key-value pairs of integer keys.

ipairs([#tref{}=Tref|_], St) ->
    case luerl_emul:getmetamethod(Tref, <<"__ipairs">>, St) of
	nil -> {[{function,fun ipairs_next/2},Tref,0.0],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
ipairs(As, St) -> badarg_error(ipairs, As, St).
    
ipairs_next([A], St) -> ipairs_next([A,0.0], St);
ipairs_next([#tref{i=T},K|_], St) ->
    #table{a=Arr} = ?GET_TABLE(T, St#luerl.ttab),	%Get the table
    case ?IS_INTEGER(K, I) of
	true when I >= 0 ->
	    Next = I + 1,
	    case raw_get_index(Arr, Next) of
		nil -> {[nil],St};
		V -> {[float(Next),V],St}
	    end;
	_NegFalse -> lua_error({invalid_key,ipairs,K}, St)
    end;
ipairs_next(As, St) -> badarg_error(ipairs, As, St).

%% pairs(Args, State) -> {[Func,Table,Key],State}.
%%  Return a function to step over all the key-value pairs in a table.

pairs([#tref{}=Tref|_], St) ->
    case luerl_emul:getmetamethod(Tref, <<"__pairs">>, St) of
	nil -> {[{function,fun next/2},Tref,nil],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
pairs(As, St) -> badarg_error(pairs, As, St).

%% next(Args, State) -> {[Key,Value] | [nil], State}.
%%  Given a table and a key return the next key-value pair in the
%%  table, or nil if there is no next key. The key 'nil' gives the
%%  first key-value pair.

next([A], St) -> next([A,nil], St);
next([#tref{i=T},K|_], St) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(T, St#luerl.ttab),	%Get the table
    if K == nil ->
	    %% Find the first, start with the array.
	    %% io:format("n: ~p\n", [{Arr,Dict}]),
	    next_index(0, Arr, Dict, St);
       is_number(K) ->
	    case ?IS_INTEGER(K, I0) of
		true when I0 >= 1 ->
		    next_index(I0, Arr, Dict, St);
		_NegFalse -> next_key(K, Dict, St)	%Not integer or negative
	    end;
       true -> next_key(K, Dict, St)
    end;
next(As, St) -> badarg_error(next, As, St).

next_index(I0, Arr, Dict, St) ->
    case next_index_loop(I0+1, Arr, array:size(Arr)) of
	{I1,V} -> {[float(I1),V],St};
	none ->
	    %% Nothing in the array, take table instead.
	    first_key(Dict, St)
    end.

next_index_loop(I, Arr, S) when I < S ->
    case array:get(I, Arr) of
	nil -> next_index_loop(I+1, Arr, S);
	V -> {I,V}
    end;
next_index_loop(_, _, _) -> none.

first_key(Dict, St) ->
    case ttdict:first(Dict) of
	{ok,{K,V}} -> {[K,V],St};
	error -> {[nil],St}
    end.

next_key(K, Dict, St) ->
    case ttdict:next(K, Dict) of
	{ok,{N,V}} -> {[N,V],St};
	error -> {[nil],St}
    end.

print(Args, St0) ->
    St1 = lists:foldl(fun (A, S0) ->
			      {Str,S1} = tostring([A], S0),
			      io:format("~s ", [Str]),
			      S1
		      end, St0, Args),
    io:nl(),
    {[],St1}.

%% rawequal([Arg,Arg|_], State) -> {[Bool],State}.
%% rawlen([Object|_], State) -> {[Length],State}.
%% rawget([Table,Key|_], State) -> {[Val],State)}.
%% rawset([Table,Key,Value|_]], State) -> {[Table],State)}.

rawequal([A1,A2|_], St) -> {[A1 =:= A2],St};
rawequal(As, St) -> badarg_error(rawequal, As, St).

rawlen([A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
rawlen([#tref{}=T|_], St) ->
    {[luerl_lib_table:raw_length(T, St)],St};
rawlen(As, St) -> badarg_error(rawlen, As, St).

rawget([#tref{i=N},K|_], St) when is_number(K) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(N, St#luerl.ttab),	%Get the table.
    V = case ?IS_INTEGER(K, I) of
	    true when I >= 1 ->			%Array index
		raw_get_index(Arr, I);
	    _NegFalse ->			%Negative or false
		raw_get_key(Dict, K)
	end,
    {[V],St};
rawget([#tref{i=N},K|_], St) ->
    #table{d=Dict} = ?GET_TABLE(N, St#luerl.ttab),	%Get the table.
    V = raw_get_key(Dict, K),
    {[V],St};
rawget(As, St) -> badarg_error(rawget, As, St).

rawset([#tref{i=N}=Tref,K,V|_], #luerl{ttab=Ts0}=St) when is_number(K) ->
    #table{a=Arr0,d=Dict0}=T = ?GET_TABLE(N, Ts0),
    Ts1 = case ?IS_INTEGER(K, I) of
	      true when I >= 1 ->
		  Arr1 = raw_set_index(Arr0, I, V),
		  ?SET_TABLE(N, T#table{a=Arr1}, Ts0);
	      _NegFalse ->			%Negative or false
		  Dict1 = raw_set_key(Dict0, K, V),
		  ?SET_TABLE(N, T#table{d=Dict1}, Ts0)
	  end,
    {[Tref],St#luerl{ttab=Ts1}};
rawset([Tref,nil=K,_|_], St) ->
    lua_error({illegal_index,Tref,K}, St);
rawset([#tref{i=N}=Tref,K,V|_], #luerl{ttab=Ts0}=St) ->
    #table{d=Dict0}=T = ?GET_TABLE(N, Ts0),
    Dict1 = raw_set_key(Dict0, K, V),
    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
    {[Tref],St#luerl{ttab=Ts1}};
rawset(As, St) -> badarg_error(rawset, As, St).

%% raw_get_index(Array, Index) -> nil | Value.
%% raw_get_key(Dict, Key) -> nil | Value.

raw_get_index(Arr, I) -> array:get(I, Arr).

raw_get_key(Dict, K) ->
    case ttdict:find(K, Dict) of
	{ok,V} -> V;
	error -> nil
    end.

raw_set_index(Arr, I, V) -> array:set(I, V, Arr).

raw_set_key(Dict, K, nil) -> ttdict:erase(K, Dict);
raw_set_key(Dict, K, V) -> ttdict:store(K, V, Dict).

%% select(Args, State) -> {[Element],State}.

select([<<$#>>|As], St) -> {[float(length(As))],St};
select([A|As], St) ->
    %%io:fwrite("sel:~p\n", [[A|As]]),
    Len = length(As),
    case luerl_lib:to_int(A) of
	N when is_integer(N), N > 0 -> {select_front(N, As, Len),St};
	N when is_integer(N), N < 0 -> {select_back(-N, As, Len),St};
	_ -> badarg_error(select, [A|As], St)
    end;
select(As, St) -> badarg_error(select, As, St).

select_front(N, As, Len) when N =< Len ->
    lists:nthtail(N-1, As);
select_front(_, _, _) -> [].

select_back(N, As, Len) when N =< Len ->
    lists:nthtail(Len-N, As);
select_back(_, As, _) -> As.

tonumber([Arg], St) -> {[luerl_lib:tonumber(Arg)],St};
tonumber([Arg,B|_], St) -> {[luerl_lib:tonumber(Arg, B)],St};
tonumber(As, St) -> badarg_error(tonumber, As, St).

tostring([Arg|_], St) ->
    case luerl_emul:getmetamethod(Arg, <<"__tostring">>, St) of
	nil -> {[tostring(Arg)],St};
	M when element(1, M) =:= function ->
	    {R,St1} = luerl_emul:functioncall(M, [Arg], St),
	    {R,St1}
    end.

tostring(nil) -> <<"nil">>;
tostring(false) -> <<"false">>;
tostring(true) -> <<"true">>;
tostring(N) when is_number(N) ->
    A = abs(N),
    %% Print really big/small "integers" as floats as well.
    S = if ?IS_INTEGER(N), A < 1.0e14 ->
		integer_to_list(round(N));
	   true -> io_lib:write(N)
	end,
    iolist_to_binary(S);
tostring(S) when is_binary(S) -> S;
tostring(#tref{i=I}) -> iolist_to_binary(["table: ",io_lib:write(I)]);
tostring(#function{}) -> <<"function:">>;	%Functions defined in Lua
tostring({function,_}) -> <<"function:">>;	%Internal functions
tostring(#thread{}) -> <<"thread">>;
tostring(#userdata{}) -> <<"userdata">>;
tostring(_) -> <<"unknown">>.

type([Arg|_], St) -> {[type(Arg)],St}.		%Only one return value!

type(nil) -> <<"nil">>;
type(N) when is_number(N) -> <<"number">>;
type(S) when is_binary(S) -> <<"string">>;
type(B) when is_boolean(B) -> <<"boolean">>;
type(#tref{}) -> <<"table">>;
type(#function{}) -> <<"function">>;		%Functions defined in Lua
type({function,_}) -> <<"function">>;		%Internal functions
type(#thread{}) -> <<"thread">>;
type(#userdata{}) -> <<"userdata">>;
type(_) -> <<"unknown">>.

%% getmetatable([Value|_], State) -> {Table,State}.
%% setmetatable([Table,Table|nil|_], State) -> {Table,State}.
%%  Can only set the metatable of tables here. Return tables for all
%%  values, for tables and userdata it is the table of the object,
%%  else the metatable for the type.

getmetatable([O|_], St) ->
    case luerl_emul:getmetatable(O, St) of
	#tref{i=N}=Meta ->
	    #table{d=Dict} = ?GET_TABLE(N, St#luerl.ttab),
	    case ttdict:find(<<"__metatable">>, Dict) of
		{ok,MM} -> {[MM],St};
		error -> {[Meta],St}
	    end;
	nil -> {[nil],St}
    end.

setmetatable([#tref{}=T,#tref{}=M|_], St) ->
    do_setmetatable(T, M, St);
setmetatable([#tref{}=T,nil|_], St) ->
    do_setmetatable(T, nil, St);
setmetatable(As, St) -> badarg_error(setmetatable, As, St).

do_setmetatable(#tref{i=N}=T, M, St) ->
    case luerl_emul:getmetamethod(T, <<"__metatable">>, St) of
	nil ->
	    Ts = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=M} end, St#luerl.ttab),
	    {[T],St#luerl{ttab=Ts}};
	_ -> badarg_error(setmetatable, [T], St)
    end.

%% Do files.

dofile(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[File] ->
	    Ret = luerl_comp:file(File),	%Compile the file
	    dofile_ret(Ret, As, St);
	_ -> badarg_error(dofile, As, St)
    end.

dofile_ret({ok,Chunk}, _, St0) ->
    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
    luerl_emul:call(Func, [], St1);
dofile_ret({error,_,_}, As, St) ->
    badarg_error(dofile, As, St).

%% Load string and files.

load(As, St) ->
    case luerl_lib:conv_list(As, [string,lua_string,lua_string,lua_any]) of
	[S|_] ->
	    Ret = luerl_comp:string(S),		%Compile the string
	    load_ret(Ret, St);
	nil -> badarg_error(load, As, St)
    end.

loadfile(As, St) ->
    case luerl_lib:conv_list(As, [string,lua_string,lua_any]) of
	[F|_] ->
	    Ret = luerl_comp:file(F),		%Compile the file
	    load_ret(Ret, St);
	nil -> badarg_error(loadfile, As, St)
    end.
 
loadstring(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[S] ->
	    Ret = luerl_comp:string(S),		%Compile the string
	    load_ret(Ret, St);
	nil -> badarg_error(loadstring, As, St)
    end.

load_ret({ok,Chunk}, St0) ->
    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
    {[Func],St1};
load_ret({error,[{_,Mod,E}|_],_}, St) ->
    Msg = iolist_to_binary(Mod:format_error(E)),
    {[nil,Msg],St}.

pcall([F|As], St0) ->
    try
	{Rs,St1} = luerl_emul:functioncall(F, As, St0),
	{[true|Rs],St1}
    catch
	%% Only catch Lua errors here, signal system errors.
	error:{lua_error,{error_call, E},St2} ->
	    {[false,E],St2};
	error:{lua_error,E,St2} ->
	    %% Basic formatting for now.
	    Msg = iolist_to_binary(luerl_lib:format_error(E)),
	    {[false,Msg],St2}
    end.

%% Lua 5.1 compatibility functions.

unpack(As, St) -> luerl_lib_table:unpack(As, St).
