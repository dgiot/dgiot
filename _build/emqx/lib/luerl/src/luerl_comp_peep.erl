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

%% File    : luerl_comp_peep.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does peep-hole optimisation in the compiler.

-module(luerl_comp_peep).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

-export([chunk/2]).

%% chunk(St0, Opts) -> {ok,St0}.
%%  A chunk is now a list of instructions to define the function.

chunk(#code{code=Is0}=Code, Opts) ->
    Is1 = instrs(Is0, nil),			%No local state
    luerl_comp:debug_print(Opts, "cp: ~p\n", [Is1]),
    {ok,Code#code{code=Is1}}.

%% Combining instructions.
instrs([?PUSH_LIT(L),?GET_KEY|Is], St) ->
    instrs([?GET_LIT_KEY(L)|Is], St);
instrs([?PUSH_LIT(L),?SET_KEY|Is], St) ->
    instrs([?SET_LIT_KEY(L)|Is], St);

%% Must check these properly, probably seldom used anyway.
%% instrs([?STORE_EVAR(D, I),?PUSH_EVAR(D, I)|Is], St) ->
%%     instrs([?DUP,?STORE_EVAR(D, I)|Is], St);
%% instrs([?STORE_LVAR(D, I),?PUSH_LVAR(D, I)|Is], St) ->
%%     instrs([?DUP,?STORE_LVAR(D, I)|Is], St);
%% instrs([?STORE_GVAR(K),?PUSH_GVAR(K)|Is], St) ->
%%     instrs([?DUP,?STORE_EVAR(D, I)|Is], St);

instrs([?PUSH_LIT(L),?MULTIPLE|Is], St) ->
    instrs([?PUSH_LAST_LIT(L)|Is], St);
instrs([?PUSH_LVAR(D, I),?MULTIPLE|Is], St) ->
    instrs([?PUSH_LAST_LVAR(D, I)|Is], St);
instrs([?PUSH_EVAR(D, I),?MULTIPLE|Is], St) ->
    instrs([?PUSH_LAST_EVAR(D, I)|Is], St);
instrs([?PUSH_GVAR(K),?MULTIPLE|Is], St) ->
    instrs([?PUSH_LAST_GVAR(K)|Is], St);

instrs([?POP,?POP|Is], St) ->
    instrs([?POP2|Is], St);

%% Doing sub instructions.
instrs([?FDEF(Lsz,Esz,Pars,Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?FDEF(Lsz,Esz,Pars,Fis1)|instrs(Is, St)];
instrs([?BLOCK(0,0,Bis)|Is], St) ->		%No need for block
    instrs(Bis ++ Is, St);
instrs([?BLOCK(Lsz,Esz,Bis0)|Is], St) ->
    Bis1 = instrs(Bis0, St),
    [?BLOCK(Lsz,Esz,Bis1)|instrs(Is, St)];
instrs([?REPEAT(Ris0)|Is], St) ->
    Ris1 = instrs(Ris0, St),
    [?REPEAT(Ris1)|instrs(Is, St)];
instrs([?WHILE(Eis0, Wis0)|Is], St) ->
    Eis1 = instrs(Eis0, St),
    Wis1 = instrs(Wis0, St),
    [?WHILE(Eis1, Wis1)|instrs(Is, St)];
instrs([?AND_THEN(Tis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    [?AND_THEN(Tis1)|instrs(Is, St)];
instrs([?OR_ELSE(Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?OR_ELSE(Fis1)|instrs(Is, St)];
instrs([?IF_TRUE(Tis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    [?IF_TRUE(Tis1)|instrs(Is, St)];
instrs([?IF_FALSE(Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?IF_FALSE(Fis1)|instrs(Is, St)];
instrs([?IF(Tis, [])|Is], St) ->
    instrs([?IF_TRUE(Tis)|Is], St);
instrs([?IF([], Fis)|Is], St) ->		%This should never happen
    instrs([?IF_FALSE(Fis)|Is], St);
instrs([?IF(Tis0, Fis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    Fis1 = instrs(Fis0, St),
    [?IF(Tis1, Fis1)|instrs(Is, St)];
instrs([?NFOR(V, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?NFOR(V, Fis1)|instrs(Is, St)];
instrs([?GFOR(Vs, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?GFOR(Vs, Fis1)|instrs(Is, St)];

%% Nothing to do.
instrs([I|Is], St) -> [I|instrs(Is, St)];
instrs([], _) -> [].
