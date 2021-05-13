%% Copyright (c) 2015 Robert Virding
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

%% File    : luerl_lib_debug.erl
%% Author  : Robert Virding
%% Purpose : The debug library for Luerl.

%% This is a very rudimentary debug module which contains those
%% functions which need no detailed information about the internals.

-module(luerl_lib_debug).

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"getmetatable">>,{function,fun getmetatable/2}},
     {<<"getuservalue">>,{function,fun getuservalue/2}},
     {<<"setmetatable">>,{function,fun setmetatable/2}},
     {<<"setuservalue">>,{function,fun setuservalue/2}}
    ].

%% getmetatable([Value|_], State) -> {[Table],State}.
%% setmetatable([Table,Table|nil|_], State) -> {[Table],State}.
%%  Can set the metatable of all types here. Return tables for all
%%  values, for tables and userdata it is the table of the object,
%%  else the metatable for the type.

getmetatable([O|_], St) ->
    {[do_getmetatable(O, St)],St};
getmetatable(As, St) -> badarg_error(getmetatable, As, St).

do_getmetatable(#tref{i=T}, #luerl{ttab=Ts}) ->
    (?GET_TABLE(T, Ts))#table.m;
do_getmetatable(#userdata{m=Meta}, _) -> Meta;
do_getmetatable(nil, #luerl{meta=Meta}) -> Meta#meta.nil;
do_getmetatable(B, #luerl{meta=Meta}) when is_boolean(B) ->
    Meta#meta.boolean;
do_getmetatable(N, #luerl{meta=Meta}) when is_number(N) ->
    Meta#meta.number;
do_getmetatable(S, #luerl{meta=Meta}) when is_binary(S) ->
     Meta#meta.string;
do_getmetatable(_, _) -> nil.	       %Other types have no metatables

setmetatable([T,M|_], St) ->
    do_setmetatable(T, M, St);
setmetatable(As, St) -> badarg_error(setmetatable, As, St).

do_setmetatable(#tref{i=N}=T, M, #luerl{ttab=Ts0}=St) ->
    Ts1 = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=M} end, Ts0),
    {[T],St#luerl{ttab=Ts1}};
do_setmetatable(#userdata{}=U, M, St) ->
    {[U#userdata{m=M}],St};
do_setmetatable(nil, M, #luerl{meta=Meta0}=St) ->
    Meta1 = Meta0#meta{nil=M},
    {[nil],St#luerl{meta=Meta1}};
do_setmetatable(B, M, #luerl{meta=Meta0}=St) when is_boolean(B) ->
    Meta1 = Meta0#meta{boolean=M},
    {[B],St#luerl{meta=Meta1}};
do_setmetatable(N, M, #luerl{meta=Meta0}=St) when is_number(N) ->
    Meta1 = Meta0#meta{number=M},
    {[N],St#luerl{meta=Meta1}};
do_setmetatable(B, M, #luerl{meta=Meta0}=St) when is_binary(B) ->
    Meta1 = Meta0#meta{string=M},
    {[B],St#luerl{meta=Meta1}};
do_setmetatable(D, _, St) ->			%Do nothing for the rest
    {[D],St}.

%% getuservalue([User|_], State) -> {[Value],State}.
%% setuservalue([User,Value|_], State) -> {[User],State}.
%%  These are basically no-ops.

getuservalue([_|_], St) -> {[nil],St};
getuservalue(As, St) -> badarg_error(getuservalue, As, St).

setuservalue([U,_|_], St) -> {[U],St};
setuservalue(As, St) -> badarg_error(setuservalue, As, St).
