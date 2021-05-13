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

%% File    : luerl_comp_vars.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does variable and stack analysis in the compiler

-module(luerl_comp_vars).

-include("luerl.hrl").
-include("luerl_comp.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,subtract/2,intersection/2]).

chunk(#code{code=C0}=Code, Opts) ->
    {C1,_,_,nul} = functiondef(C0, [], nul),	%No local state here!
    luerl_comp:debug_print(Opts, "cv: ~p\n", [C1]),
    {ok,Code#code{code=C1}}.

%% stmts(Stmts, VarData, State) ->
%%     {Stmts,NewVarData,State}.
%%  Main problem here is to calculate local/free/used variables in the
%%  right order. Must do everything going forwards.

stmts([S0|Ss0], Vars0, St0) ->
    {S1,New,Used,Fused,St1} = stmt(S0, nul, St0),
    Vars1 = update_vars(Vars0, New, Used, Fused),
    %% io:format("ss1: ~p\n", [Vars0]),
    %% io:format("ss1> ~p\n", [{New,Used,Fused}]),
    %% io:format("ss1> ~p\n", [Vars1]),
    {Ss1,Vars2,St2} = stmts(Ss0, Vars1, St1),
    {[S1|Ss1],Vars2,St2};
stmts([], Vars, St) -> {[],Vars,St}.

update_vars(#vars{local=Lo,free=Fr,used=Us,fused=Fu}, New, Used, Fused) ->
    Aused = union(Used, Fused),			%All used
    Free = subtract(Aused, Lo),
    #vars{local=union(New, Lo),
	  free=union(Free, Fr),
	  used=union(Used, Us),
	  fused=union(Fused, Fu)}.

%% stmt(Stmt, LocalVars, State) -> {Stmt,NewVars,UsedVars,FusedVars,State}.

stmt(#assign_stmt{}=A, Loc, St) -> assign_stmt(A, Loc, St);
stmt(#call_stmt{}=C, Loc, St) -> call_stmt(C, Loc, St);
stmt(#return_stmt{}=R, Loc, St) -> return_stmt(R, Loc, St);
stmt(#break_stmt{}=B, _, St) -> {B,[],[],[],St};
stmt(#block_stmt{}=B, Loc, St) -> block_stmt(B, Loc, St);
stmt(#while_stmt{}=W, Loc, St) -> while_stmt(W, Loc, St);
stmt(#repeat_stmt{}=R, Loc, St) -> repeat_stmt(R, Loc, St);
stmt(#if_stmt{}=If, Loc, St) -> if_stmt(If, Loc, St);
stmt(#nfor_stmt{}=For, Loc, St) -> numfor_stmt(For, Loc, St);
stmt(#gfor_stmt{}=For, Loc, St) -> genfor_stmt(For, Loc, St);
stmt(#local_assign_stmt{}=L, Loc, St) ->
    local_assign_stmt(L, Loc, St);
stmt(#local_fdef_stmt{}=L, Loc, St) ->
    local_fdef_stmt(L, Loc, St);
stmt(#expr_stmt{}=E, Loc, St) ->		%Expressions "statement"
    expr_stmt(E, Loc, St).


%% assign_stmt(Assign, LocalVars, State) ->
%%     {Assign,NewVars,UsedVars,FusedVars,State}.

assign_stmt(#assign_stmt{vs=Vs0,es=Es0}=A, Loc, St0) ->
    {Vs1,Vused,Vfused,St1} = assign_loop(Vs0, Loc, St0),
    {Es1,Eused,Efused,St2} = explist(Es0, Loc, St1),
    Used = union(Vused, Eused),
    Fused = union(Vfused, Efused),
    {A#assign_stmt{vs=Vs1,es=Es1},[],Used,Fused,St2}.

assign_loop([V0|Vs0], Loc, St0) ->
    {V1,Vused,Vfused,St1} = var(V0, Loc, St0),
    {Vs1,Vsused,Vsfused,St2} = assign_loop(Vs0, Loc, St1),
    Used = union(Vused, Vsused),
    Fused = union(Vfused, Vsfused),
    {[V1|Vs1],Used,Fused,St2};
assign_loop([], _, St) -> {[],[],[],St}.

var(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Eused,Efused,St1} = prefixexp_first(Exp0, Loc, St0),
    {Rest1,Rused,Rfused,St2} = var_rest(Rest0, Loc, St1),
    Used = union(Eused, Rused),
    Fused = union(Efused, Rfused),
    {D#dot{e=Exp1,r=Rest1},Used,Fused,St2};
var(#var{n=N}=V, _, St) ->
    {V,[N],[],St}.

var_rest(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Eused,Efused,St1} = prefixexp_element(Exp0, Loc, St0),
    {Rest1,Rused,Rfused,St2} = var_rest(Rest0, Loc, St1),
    Used = union(Eused, Rused),
    Fused = union(Efused, Rfused),
    {D#dot{e=Exp1,r=Rest1},Used,Fused,St2};
var_rest(Exp, Loc, St) -> var_last(Exp, Loc, St).

var_last(#key{k=Exp0}=K, Loc, St0) ->
    {Exp1,Used,Fused,St1} = exp(Exp0, Loc, St0),
    {K#key{k=Exp1},Used,Fused,St1}.

%% call_stmt(Call, LocalVars, State) ->
%%     {Call,NewVars,UsedVars,FusedVars,State}.

call_stmt(#call_stmt{call=Exp0}=C, Loc, St0) ->
    {Exp1,Used,Fused,St1} = exp(Exp0, Loc, St0),
    {C#call_stmt{call=Exp1},[],Used,Fused,St1}.

%% return_stmt(Return, LocalVars, State) ->
%%     {Return,NewVars,UsedVars,FusedVars,State}.

return_stmt(#return_stmt{es=Es0}=R, Loc, St0) ->
    {Es1,Used,Fused,St1} = explist(Es0, Loc, St0),
    {R#return_stmt{es=Es1},[],Used,Fused,St1}.

%% block_stmt(Block, LocalVars, State) ->
%%     {Block,NewVars,UsedVars,FusedVars,State}.

block_stmt(#block_stmt{ss=Ss0}=B, _, St0) ->
    Vars0 = #vars{local=[],free=[],used=[],fused=[]},
    {Ss1,Vars1,St1} = stmts(Ss0, Vars0, St0),
    %% Be careful what we export, adjust Used and Fused for locals.
    #vars{local=Bloc,used=Bused,fused=Bfused} = Vars1,
    Used = subtract(Bused, Bloc),
    Fused = subtract(Bfused, Bloc),
    {B#block_stmt{ss=Ss1,vars=Vars1},[],Used,Fused,St1}.

%% do_block(Block, State) -> {Block,UsedVars,FusedVars,State}.
%% do_block(Block, LocalVars, State) -> {Block,UsedVars,FusedVars,State}.
%%  Do_block never returns external new variables as it never exports
%%  variables.

do_block(B, St) -> do_block(B, [], St).

do_block(#block{ss=Ss0}=B, Loc, St0) ->
    Vars0 = #vars{local=Loc,free=[],used=[],fused=[]},
    {Ss1,Vars1,St1} = stmts(Ss0, Vars0, St0),
    %% Be careful what we export, adjust Used and Fused for locals.
    #vars{local=Bloc,used=Bused,fused=Bfused} = Vars1,
    Used = subtract(Bused, Bloc),
    Fused = subtract(Bfused, Bloc),
    {B#block{ss=Ss1,vars=Vars1},Used,Fused,St1}.

%% while_stmt(While, LocalVars, State) ->
%%     {While,NewVars,UsedVars,FusedVars,State}.
%%  While_stmt never returns external new variables.  The test
%%  expression is done in the context of the surrounding block.

while_stmt(#while_stmt{e=E0,b=B0}=W, Loc, St0) ->
    {E1,Eused,Efused,St1} = exp(E0, Loc, St0),
    {B1,Bused,Bfused,St2} = do_block(B0, St1),
    Used = union(Eused, Bused),
    Fused = union(Efused, Bfused),
    {W#while_stmt{e=E1,b=B1},[],Used,Fused,St2}.

%% repeat_stmt(Repeat, LocalVars, State) ->
%%     {Repeat,NewVars,UsedVars,FusedVars,State}.
%%  Repeat_stmt never returns external new variables. The test
%%  expression is done in the context of the repeat block and is
%%  already inside the block.

repeat_stmt(#repeat_stmt{b=B0}=R, _, St0) ->
    {B1,Used,Fused,St1} = do_block(B0, St0),
    {R#repeat_stmt{b=B1},[],Used,Fused,St1}.

%% if_stmt(If, LocalVars, State) -> {If,NewVars,FreeVars,State}.
%%  The block info includes anything from the test expressions even
%%  though we keep them separate.

if_stmt(#if_stmt{tests=Ts0,else=E0}=If, Loc, St0) ->
    {Ts1,Tused,Tfused,St1} = if_tests(Ts0, Loc, St0),
    {E1,Eused,Efused,St2} = do_block(E0, St1),
    Used = union(Tused, Eused),
    Fused = union(Tfused, Efused),
    {If#if_stmt{tests=Ts1,else=E1},[],Used,Fused,St2}.

if_tests([{E0,B0}|Ts0], Loc, St0) ->
    {E1,Eused,Efused,St1} = exp(E0, Loc, St0),
    {B1,Bused,Bfused,St2} = do_block(B0, St1),
    {Ts1,Tsused,Tsfused,St3} = if_tests(Ts0, Loc, St2),
    Used = union([Eused,Bused,Tsused]),
    Fused = union([Efused,Bfused,Tsfused]),
    {[{E1,B1}|Ts1],Used,Fused,St3};
if_tests([], _, St) -> {[],[],[],St}.

%% numfor_stmt(For, LocalVars, State) ->
%%     {For,NewVars,UsedVars,FusedVars,State}.

numfor_stmt(#nfor_stmt{v=#var{n=N},init=I0,limit=L0,step=S0,b=B0}=For,
	    Loc, St0) ->
    {[I1,L1,S1],Esused,Esfused,St1} = explist([I0,L0,S0], Loc, St0),
    {B1,Bused,Bfused,St2} = do_block(B0, [N], St1),
    %% Be careful what we export, adjust Used and Fused for N.
    Used = union(Esused, del_element(N, Bused)),
    Fused = union(Esfused, del_element(N, Bfused)),
    {For#nfor_stmt{init=I1,limit=L1,step=S1,b=B1},[],Used,Fused,St2}.

%% genfor_stmt(For, LocalVars, State) -> {For,NewVars,FreeVars,State}.

genfor_stmt(#gfor_stmt{vs=Vs,gens=Gs0,b=B0}=For, Loc, St0) ->
    {Gs1,Gused,Gfused,St1} = explist(Gs0, Loc, St0),
    Ns = lists:foldl(fun (#var{n=N}, Ns) -> add_element(N, Ns) end, [], Vs),
    {B1,Bused,Bfused,St2} = do_block(B0, Ns, St1),
    %% Be careful what we export, adjust Used and Fused for Ns.
    Used = union(Gused, subtract(Bused, Ns)),
    Fused = union(Gfused, subtract(Bfused, Ns)),
    {For#gfor_stmt{gens=Gs1,b=B1},[],Used,Fused,St2}.

%% local_assign_stmt(Local, LocalVars, State) -> {Local,NewVars,FreeVars,State}.

local_assign_stmt(#local_assign_stmt{vs=Vs,es=Es0}=L, Loc, St0) ->
    {Es1,Used,Fused,St1} = explist(Es0, Loc, St0),
    New = lists:foldl(fun (#var{n=N}, Ns) -> add_element(N, Ns) end, [], Vs),
    {L#local_assign_stmt{es=Es1},New,Used,Fused,St1}.

%% local_fdef_stmt(Local, LocalVars, State) ->
%%     {Local,NewVars,FreeVars,UsedVars,State}.
%%  We explicitly handle used variables here as we want the function
%%  name to be included in Used in recursive function calls.

local_fdef_stmt(#local_fdef_stmt{v=#var{n=N},f=F0}=L, _, St0) ->
    {F1,Used,Fused,St1} = functiondef(F0, nul, St0),
    New = [N],
    {L#local_fdef_stmt{f=F1},New,Used,Fused,St1}.

%% exp_stmt(Expr, LocalVars, State) ->
%%     {Expr,NewVars,UsedVars,FusedVars,State}.
%%  This will return a single value.

expr_stmt(#expr_stmt{exp=Exp0}=E, Loc, St0) ->
    {Exp1,Used,Fused,St1} = exp(Exp0, Loc, St0),
    {E#expr_stmt{exp=Exp1},[],Used,Fused,St1}.

%% explist(Exprs, LocalVars, State) -> {Exprs,UsedVars,FusedVars,State}.
%% exp(Expr, LocalVars, State) -> {Expr,UsedVars,FusedVars,State}.
%% prefixexp(Expr, LocalVars, State) -> {Expr,UsedVars,FusedVars,State}.
%%  An expression can never create new local variables.

explist([E0|Es0], Loc, St0) ->
    {E1,Eused,Efused,St1} = exp(E0, Loc, St0),
    {Es1,Esused,Esfused,St2} = explist(Es0, Loc, St1),
    Used = union(Eused, Esused),
    Fused = union(Efused, Esfused),
    {[E1|Es1],Used,Fused,St2};
explist([], _, St) -> {[],[],[],St}.		%No expressions at all

exp(#lit{}=L, _, St) -> {L,[],[],St};		%Nothing to do
exp(#fdef{}=F, _, St) -> functiondef(F, nul, St);
exp(#op{as=Es0}=Op, Loc, St0) ->
    {Es1,Used,Fused,St1} = explist(Es0, Loc, St0),
    {Op#op{as=Es1},Used,Fused,St1};
exp(#tc{fs=Fs0}=T, Loc, St0) ->
    {Fs1,Used,Fused,St1} = tableconstructor(Fs0, Loc, St0),
    {T#tc{fs=Fs1},Used,Fused,St1};
exp(E, Loc, St) ->
    prefixexp(E, Loc, St).

prefixexp(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Eused,Efused,St1} = prefixexp_first(Exp0, Loc, St0),
    {Rest1,Rused,Rfused,St2} = prefixexp_rest(Rest0, Loc, St1),
    Used = union(Eused, Rused),
    Fused = union(Efused, Rfused),
    {D#dot{e=Exp1,r=Rest1},Used,Fused,St2};
prefixexp(Exp, Loc, St) -> prefixexp_first(Exp, Loc, St).

prefixexp_first(#single{e=E0}=S, Loc, St0) ->
    {E1,Used,Fused,St1} = exp(E0, Loc, St0),
    {S#single{e=E1},Used,Fused,St1};
prefixexp_first(#var{n=N}=V, _, St) ->
    {V,[N],[],St}.

prefixexp_rest(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Eused,Efused,St1} = prefixexp_element(Exp0, Loc, St0),
    {Rest1,Rused,Rfused,St2} = prefixexp_rest(Rest0, Loc, St1),
    Used = union(Eused, Rused),
    Fused = union(Efused, Rfused),
    {D#dot{e=Exp1,r=Rest1},Used,Fused,St2};
prefixexp_rest(Exp, Loc, St) -> prefixexp_element(Exp, Loc, St).

prefixexp_element(#key{k=E0}=K, Loc, St0) ->
    {E1,Used,Fused,St1} = exp(E0, Loc, St0),
    {K#key{k=E1},Used,Fused,St1};
prefixexp_element(#fcall{as=As0}=F, Loc, St0) ->
    {As1,Used,Fused,St1} = explist(As0, Loc, St0),
    {F#fcall{as=As1},Used,Fused,St1};
prefixexp_element(#mcall{m=#lit{v=N},as=As0}=M, Loc, St0) ->
    {As1,Used,Fused,St1} = explist(As0, Loc, St0),
    {M#mcall{as=As1},add_element(N, Used),Fused,St1}.

%% functiondef(Func, LocalVars, State) -> {Func,UsedVars,FusedVars,State}.
%%  All the variables "used" in the function which are not local
%%  become "fused" externally.

functiondef(#fdef{ps=Ps,ss=Ss0}=F, _, St0) ->
    Loc0 = lists:foldl(fun (#var{n=N}, Vs) -> add_element(N, Vs);
			  (_, Vs) -> Vs
		      end, [], Ps),
    Vars0 = #vars{local=Loc0,free=[],used=[],fused=[]},
    {Ss1,Vars1,St1} = stmts(Ss0, Vars0, St0),
    %% Make all free variables "fused" in outside block.
    {F#fdef{ss=Ss1,vars=Vars1},[],Vars1#vars.free,St1}.

%% tableconstructor(Fields, LocalVars, State) ->
%%     {Fields,UsedVars,FusedVars,State}.

tableconstructor(Fs0, Loc, St0) ->
    Fun = fun (#efield{v=V0}=F, {Used0,Fused0,S0}) ->
		  {V1,Vused,Vfused,S1} = exp(V0, Loc, S0),
		  Used1 = union(Vused, Used0),
		  Fused1 = union(Vfused, Fused0),
		  {F#efield{v=V1},{Used1,Fused1,S1}};
	      (#kfield{k=K0,v=V0}=F, {Used0,Fused0,S0}) ->
		  {K1,Kused,Kfused,S1} = exp(K0, Loc, S0),
		  {V1,Vused,Vfused,S2} = exp(V0, Loc, S1),
		  Used1 = union([Kused,Vused,Used0]),
		  Fused1 = union([Kfused,Vfused,Fused0]),
		  {F#kfield{k=K1,v=V1},{Used1,Fused1,S2}}
	  end,
    {Fs1,{Used,Fused,St1}} = lists:mapfoldl(Fun, {[],[],St0}, Fs0),
    {Fs1,Used,Fused,St1}.
