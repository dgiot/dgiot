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

%% File    : luerl_lib_package.erl
%% Author  : Robert Virding
%% Purpose : The package library for Luerl.

%% These functions sometimes behave strangely in the Lua 5.2
%% libraries, but we try to follow them. Most of these functions KNOW
%% that a table is a ttdict! We know that the erlang array has default
%% value 'nil'.

-module(luerl_lib_package).

-include_lib("kernel/include/file.hrl").

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

%% Export some functions which can be called from elsewhere.
-export([search_path/5]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St0) ->
    St1 = luerl_emul:set_global_key(<<"require">>,
				    {function,fun require/2}, St0),
    {S,St2} = luerl_emul:alloc_table(searchers_table(), St1),
    {L,St3} = luerl_emul:alloc_table(loaded_table(), St2),
    {P,St4} = luerl_emul:alloc_table(preload_table(), St3),
    {T,St5} = luerl_emul:alloc_table(table(S, L, P), St4),
    {T,St5}.

%% table() -> [{FuncName,Function}].
%% meta_table() -> [{TableName,Function}].
%% searchers_table()
%% preloaded_table()
%% loaded_table()

table(S, L, P) ->
    [{<<"config">>,config()},
     {<<"loaded">>,L},
     {<<"preload">>,P},
     {<<"path">>,path()},
     {<<"searchers">>,S},
     {<<"searchpath">>,{function,fun searchpath/2}}
    ].

searchers_table() ->
    [{1.0,{function,fun preload_searcher/2}},
     {2.0,{function,fun lua_searcher/2}}].

preload_table() -> [].

loaded_table() -> [].

%% meta_table() ->
%%     [{<<"__index">>,{function,fun meta_values/2}}
%%     ].

%% config()
%% path()
%% meta_values()
%%  Generate initial data for tables.

config() ->
    <<"/\n",";\n","?\n","!\n","-\n">>.		%The defaults

path() ->
    case os:getenv("LUA_PATH") of
	false -> <<"./?.lua;./?/init.lua">>;	%Default path
	Path -> list_to_binary(Path)
    end.

%% meta_values([_,<<"bert">>], St) ->
%%     {[<<"/\n",";\n","?\n","!\n","-\n">>],St};
%% meta_values(_, St) -> {[nil],St}.		%Default undefined key

%% searchpath(Name, Path [, Sep [, Rep]]) -> [File] | [nil|Files].

searchpath(As, St) ->
    case luerl_lib:conv_list(search_args(As),
			     [lua_string,lua_string,lua_string,lua_string]) of
	[N,P,S,R] ->				%Name, path, sep, rep
	    Ret = case search_path(N, P, S, R, []) of
		      {ok,File} -> [File];
		      {error,Tried} -> [nil,Tried]
		  end,
	    {Ret,St};
	_ -> badarg_error(searchpath, As, St)
    end.

search_args([N,P]) -> [N,P,<<".">>,<<"/">>];
search_args([N,P,S]) -> [N,P,S,<<"/">>];
search_args(As) -> As.

%% search_path(Name, Path, Sep, Rep, Tried) -> {ok,File} | {error,Tried}.
%%  Search for a file in a path. Callable from Erlang.

search_path(N0, P, S, R, Tried) ->
    N1 = binary:replace(N0, S, R, [global]),
    Ts = binary:split(P, <<";">>, [global]),
    search_path_loop(N1, Ts, Tried).

search_path_loop(Name, [T|Ts], Tried) ->
    File = binary:replace(T, <<"?">>, Name, [global]),
    %% Test if file can be opened for reading.
    case file:read_file_info(File) of
	{ok,#file_info{access=A}} when A =:= read; A =:= read_write ->
	    {ok,File};
	_ -> search_path_loop(Name, Ts, Tried ++ [$',File,$',$\s])
    end;
search_path_loop(_, [], Tried) ->		%Couldn't find it
    {error,iolist_to_binary(Tried)}.

-spec require([_], _) -> {_,_} | no_return().	%To keep dialyzer quiet

%% require([File|_], State) ->{Value,State}.
%%  Main require interface.

require(As, St) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] -> do_require(Mod, St);
	nil -> badarg_error(require, As, St)
    end.

do_require(Mod, St0) ->
    {Pt,St1} = luerl_emul:get_global_key(<<"package">>, St0),
    case luerl_emul:get_table_keys(Pt, [<<"loaded">>,Mod], St1) of
	{nil,St2} ->				%Not loaded
	    {Ss,St3} = luerl_emul:get_table_key(Pt, <<"searchers">>, St2),
	    {[Ldr|Extra],St4} = search_loaders(Mod, Ss, St3),
	    {Val,St5} = luerl_emul:functioncall(Ldr, [Mod|Extra], St4),
	    require_ret(Mod, Val, Pt, St5);
	{Val,St2} -> {[Val],St2}		%Already loaded
    end.

require_ret(Mod, Val, Pt, St0) ->
    Res = case luerl_lib:first_value(Val) of
	      nil -> true;			%Assign true to loaded entry
	      __tmp -> __tmp
	  end,
    St1 = luerl_emul:set_table_keys(Pt, [<<"loaded">>,Mod], Res, St0),
    {[Res],St1}.

search_loaders(Mod, #tref{i=N}, #luerl{ttab=Ts}=St) ->
    #table{a=Arr} = ?GET_TABLE(N, Ts),
    Ls = array:sparse_to_list(Arr),
    search_loaders_loop(Mod, Ls, <<>>, St).

search_loaders_loop(Mod, [nil|Ls], Estr, St) ->	%Could find some of these
    search_loaders_loop(Mod, Ls, Estr, St);
search_loaders_loop(Mod, [L|Ls], Estr, St0) ->	%Try the next loader
    %% Call the searcher function
    case luerl_emul:functioncall(L, [Mod], St0) of
	%% Searcher found a loader.
	{[F|_],_}=Ret when element(1, F) =:= function ->
	    Ret;
	%% Searcher found no loader.
	{[S|_],St1} when is_binary(S) ->
	    Estr1 = <<Estr/binary,S/binary>>,	%Append new info string
	    search_loaders_loop(Mod, Ls, Estr1, St1);
	{_,St1} ->				%Should be nil or []
	    search_loaders_loop(Mod, Ls, Estr, St1)
    end;
search_loaders_loop(Mod, [], Estr, St) ->	%No successful loader found
    lua_error({no_module,Mod,Estr}, St).

%% preload_searcher()
%% lua_searcher()
%%  Predefined search functions in package.searchers. These must be Lua
%%  callable functions as they are visible.

preload_searcher(As, St0) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] ->
	    {Pre,St1} = luerl_emul:get_table_keys([<<"package">>,<<"preload">>],
						  St0),
	    case luerl_emul:get_table_key(Pre, Mod, St1) of
		{nil,St2} -> {[],St2};
		{Val,St2} -> {[Val],St2}	%Return the chunk
	    end;
	nil -> badarg_error(preload_searcher, As, St0)
    end.

lua_searcher(As, St0) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] ->
	    {Path,St1} = luerl_emul:get_table_keys([<<"package">>,<<"path">>],
						   St0),
	    case search_path(Mod, Path, <<".">>, <<"/">>, []) of
		{ok,File} ->
		    Ret = luerl_comp:file(binary_to_list(File)),
		    lua_searcher_ret(Ret, File, St1);
		{error,Tried} ->
		    {[Tried],St1}
	    end;
	nil -> badarg_error(lua_searcher, As, St0)
    end.

lua_searcher_ret({ok,Chunk}, File, St0) ->
    %% Wrap chunk in function to be consistent.
    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
    {[Func,File],St1};
lua_searcher_ret({error,[{_,Mod,E}|_],_}, _, St) ->
    Msg = iolist_to_binary(Mod:format_error(E)),
    {[Msg],St}.
