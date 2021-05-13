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

%% File    : luerl_comp_cg.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does code generation in the compiler.

-module(luerl_comp_cg).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2,new/0]).

%% chunk(St0, Opts) -> {ok,St0}.
%%  Return a list of instructions to define the chunk function.

chunk(#code{code=C0}=Code, Opts) ->
    {Is,nul} = functiondef(C0, nul),		%No local state
    luerl_comp:debug_print(Opts, "cg: ~p\n", [Is]),
    {ok,Code#code{code=Is}}.

%% set_var(Var) -> SetIs.
%% get_var(Var) -> GetIs.
%%  These return a LIST of instructions for setting/getting variable.

set_var(#lvar{d=D,i=I}) -> [?STORE_LVAR(D, I)];
set_var(#evar{d=D,i=I}) -> [?STORE_EVAR(D, I)];
set_var(#gvar{n=N}) -> [?STORE_GVAR(N)].

get_var(#lvar{d=D,i=I}) -> [?PUSH_LVAR(D, I)];
get_var(#evar{d=D,i=I}) -> [?PUSH_EVAR(D, I)];
get_var(#gvar{n=N}) -> [?PUSH_GVAR(N)].

%% stmt(Stmts, State) -> {Istmts,State}.

stmts([S0|Ss0], St0) ->
    {S1,St1} = stmt(S0, nul, St0),
    %% io:format("ss1: ~p\n", [{Loc0,Free0,Used0}]),
    {Ss1,St2} = stmts(Ss0, St1),
    {S1 ++ Ss1,St2};
stmts([], St) -> {[],St}.

%% stmt(Stmt, LocalVars, State) -> {Istmt,State}.

stmt(#assign_stmt{}=A, _, St) -> assign_stmt(A, St);
stmt(#call_stmt{}=C, _, St) -> call_stmt(C, St);
stmt(#return_stmt{}=R, _, St) -> return_stmt(R, St);
stmt(#break_stmt{}, _, St) -> {[?BREAK],St};
stmt(#block_stmt{}=B, _, St) -> block_stmt(B, St);
stmt(#while_stmt{}=W, _, St) -> while_stmt(W, St);
stmt(#repeat_stmt{}=R, _, St) -> repeat_stmt(R, St);
stmt(#if_stmt{}=I, _, St) -> if_stmt(I, St);
stmt(#nfor_stmt{}=F, _, St) -> numfor_stmt(F, St);
stmt(#gfor_stmt{}=F, _, St) -> genfor_stmt(F, St);
stmt(#local_assign_stmt{}=L, _, St) ->
    local_assign_stmt(L, St);
stmt(#local_fdef_stmt{}=L, _, St) ->
    local_fdef_stmt(L, St);
stmt(#expr_stmt{}=E, _, St) ->
    expr_stmt(E, St).

%% assign_stmt(Assign, State) -> {AssignIs,State}.
%%  We must evaluate all expressions, even the unneeded ones.

assign_stmt(#assign_stmt{vs=Vs,es=Es}, St) ->
    assign_loop(Vs, Es, St).

%% assign_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%
%%  This could most likely be folded together with assign_local_loop/3.

assign_loop([V], [E], St0) ->			%Remove unnecessary ?PUSH_VALS
    {Ie,St1} = exp(E, single, St0),		%Last argument to one variable
    {Iv,St2} = var(V, St1),
    {Ie ++ Iv,St2};
assign_loop([V|Vs], [E], St0) ->
    {Ie,St1} = exp(E, multiple, St0),		%Last argument to rest of vars
    {Ias,St2} = assign_loop_var(Vs, 1, St1),
    {Iv,St3} = var(V, St2),
    {Ie ++ Ias ++ Iv,St3};
assign_loop([V|Vs], [E|Es], St0) ->
    {Ie,St1} = exp(E, single, St0),		%Not last argument!
    {Ias,St2} = assign_loop(Vs, Es, St1),
    {Iv,St3} = var(V, St2),
    {Ie ++ Ias ++ Iv,St3};
assign_loop([], Es, St) ->
    assign_loop_exp(Es, St).

assign_loop_var([V|Vs], Vc, St0) ->
    {Ias,St1} = assign_loop_var(Vs, Vc+1, St0),
    {Iv,St2} = var(V, St1),
    {Ias ++ Iv,St2};
assign_loop_var([], Vc, St) ->
    {[?PUSH_VALS(Vc)],St}.

assign_loop_exp([E|Es], St0) ->
    {Ie,St1} = exp(E, single, St0),		%It will be dropped anyway
    {Ias,St2} = assign_loop_exp(Es, St1),
    {Ie ++ Ias ++ [?POP],St2};			%Pop unneeded value off stack
assign_loop_exp([], St) -> {[],St}.

var(#dot{e=Exp,r=Rest}, St0) ->
    {Ie,St1} = prefixexp_first(Exp, single, St0),
    {Ir,St2} = var_rest(Rest, St1),
    {Ie ++ Ir,St2};
var(V, St) ->
    {set_var(V),St}.

var_rest(#dot{e=Exp,r=Rest}, St0) ->
    {Ie,St1} = prefixexp_element(Exp, single, St0),
    {Ir,St2} = var_rest(Rest, St1),
    {Ie ++ Ir,St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{k=#lit{v=K}}, St) ->
    {[?SET_LIT_KEY(K)],St};			%[?PUSH_LIT(K),?SET_KEY]
var_last(#key{k=Exp}, St0) ->
    {Ie,St1} = exp(Exp, single, St0),
    {Ie ++ [?SET_KEY],St1}.

%% call_stmt(Call, State) -> {CallIs,State}.
%%  Must pop function return value list from stack.

call_stmt(#call_stmt{call=Exp}, St0) ->
    {Ie,St1} = exp(Exp, multiple, St0),
    {Ie ++ [?POP],St1}.

%% return_stmt(Return, State) -> {ReturnIs,State}.
%%  Can ignore any value left on stack here.

return_stmt(#return_stmt{es=Es}, St0) ->
    {Ies,St1} = explist(Es, multiple, St0),
    {Ies ++ [?RETURN(length(Es))],St1}.

%% block_stmt(Block, State) -> {BlockIs,State}.

block_stmt(#block_stmt{ss=Ss,lsz=Lsz,esz=Esz}, St0) ->
    {Iss,St1} = stmts(Ss, St0),
    {[?BLOCK(Lsz, Esz, Iss)],St1}.

%% do_block(Block, State) -> {Block,State}.
%%  Do_block never returns external new variables. Fits into stmt().

do_block(#block{ss=Ss,lsz=Lsz,esz=Esz}, St0) ->
    {Iss,St1} = stmts(Ss, St0),
    {[?BLOCK(Lsz, Esz, Iss)],St1}.

%% while_stmt(While, State) -> {WhileIs,State}.

while_stmt(#while_stmt{e=E,b=B}, St0) ->
    {Ie,St1} = exp(E, single, St0),
    {Ib,St2} = do_block(B, St1),
    {[?WHILE(Ie, Ib)],St2}.

%% repeat_stmt(Repeat, State) -> {RepeatIs,State}.

repeat_stmt(#repeat_stmt{b=B}, St0) ->
    {Ib,St1} = do_block(B, St0),
    {[?REPEAT(Ib)],St1}.

%% if_stmt(If, State) -> {If,State}.

if_stmt(#if_stmt{tests=Ts,else=E}, St) ->
    if_tests(Ts, E, St).

if_tests([{E,B}], #block{ss=[]}, St0) ->
    {Ie,St1} = exp(E, single, St0),
    {Ib,St2} = do_block(B, St1),
    {Ie ++ [?IF_TRUE(Ib)],St2};
if_tests([{E,B}|Ts], Else, St0) ->
    {Ie,St1} = exp(E, single, St0),
    {Ib,St2} = do_block(B, St1),
    {Its,St3} = if_tests(Ts, Else, St2),
    {Ie ++ [?IF(Ib, Its)],St3};
if_tests([], Else, St0) ->
    {Ie,St1} = do_block(Else, St0),
    {Ie,St1}.

%% numfor_stmt(For, State) -> {ForIs,State}.

numfor_stmt(#nfor_stmt{v=V,init=I,limit=L,step=S,b=B}, St0) ->
    {Ies,St1} = explist([I,L,S], single, St0),
    {Ib,St2} = do_block(B, St1),
    [?BLOCK(Lsz, Esz, Is)] = Ib,
    ForBlock = [?BLOCK(Lsz, Esz, set_var(V) ++ Is)],
    {Ies ++ [?NFOR(V,ForBlock)],St2}.

%% %% An experiment to put the block *outside* the for loop.
%% numfor_stmt(#nfor_stmt{v=V,init=I,limit=L,step=S,b=B}, St0) ->
%%     {Ies,St1} = explist([I,L,S], single, St0),
%%     {Ib,St2} = do_block(B, St1),
%%     [?BLOCK(Lsz, Esz, Is)] = Ib,
%%     ForBlock = [?BLOCK(Lsz, Esz, [?NFOR(V,set_var(V) ++ Is)])],
%%     {Ies ++ ForBlock,St2}.

%% genfor_stmt(For, State) -> {ForIs,State}.

genfor_stmt(#gfor_stmt{vs=[V|Vs],gens=Gs,b=B}, St0) ->
    {Igs,St1} = explist(Gs, multiple, St0),
    {Ias,St2} = assign_local_loop_var(Vs, 1, St1),
    {Ib,St3} = do_block(B, St2),
    [?BLOCK(Lsz, Esz, Is)] = Ib,
    ForBlock = [?BLOCK(Lsz, Esz, Ias ++ set_var(V) ++ Is)],
    {Igs ++ [?POP_VALS(length(Gs))] ++ [?GFOR(Vs,ForBlock)],St3}.

%% local_assign_stmt(Local, State) -> {Ilocal,State}.
%%  We must evaluate all expressions, even the unneeded ones.

local_assign_stmt(#local_assign_stmt{vs=Vs,es=Es}, St) ->
    assign_local(Vs, Es, St).

assign_local([V|Vs], [], St0) ->
    {Ias,St1} = assign_local_loop_var(Vs, 1, St0),
    {[?PUSH_LIT([])] ++ Ias ++ set_var(V),St1};
assign_local(Vs, Es, St) ->
    assign_local_loop(Vs, Es, St).

%% assign_local_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%
%%  This could most likely be folded together with assign_loop/3.

assign_local_loop([V], [E], St0) ->		%Remove unnecessary ?PUSH_VALS
    {Ie,St1} = exp(E, single, St0),		%Last argument to one variable!
    {Ie ++ set_var(V),St1};
assign_local_loop([V|Vs], [E], St0) ->
    {Ie,St1} = exp(E, multiple, St0),		%Last argument to many vars!
    {Ias,St2} = assign_local_loop_var(Vs, 1, St1),
    {Ie ++ Ias ++ set_var(V),St2};
assign_local_loop([V|Vs], [E|Es], St0) ->
    {Ie,St1} = exp(E, single, St0),		%Not last argument!
    {Ias,St2} = assign_local_loop(Vs, Es, St1),
    {Ie ++ Ias ++ set_var(V),St2};
assign_local_loop([], Es, St) ->
    assign_local_loop_exp(Es, St).

%% assign_local_loop_var([V|Vs], Vc, St0) ->
%%     {Ias,St1} = assign_local_loop_var(Vs, Vc+1, St0),
%%     {Ias ++ [?PUSH_LIT(nil)] ++ set_var(V),St1};
%% assign_local_loop_var([], Vc, St) ->
%%     {[],St}.
assign_local_loop_var([V|Vs], Vc, St0) ->
    {Ias,St1} = assign_local_loop_var(Vs, Vc+1, St0),
    {Ias ++ set_var(V),St1};
assign_local_loop_var([], Vc, St) ->
    {[?PUSH_VALS(Vc)],St}.

assign_local_loop_exp([E|Es], St0) ->
    {Ie,St1} = exp(E, single, St0),		%It will be dropped anyway
    {Ias,St2} = assign_local_loop_exp(Es, St1),
    {Ie ++ Ias ++ [?POP],St2};			%Pop value off stack
assign_local_loop_exp([], St) -> {[],St}.

%% local_fdef_stmt(Local, State) -> {ILocal,State}.

local_fdef_stmt(#local_fdef_stmt{v=V,f=F}, St0) ->
    {If,St1} = functiondef(F, St0),
    {If ++ set_var(V),St1}.

%% expr_stmt(Expr, State) -> {ExprIs,State}.
%%  The expression pseudo statement. This will return a single value
%%  which we leave on the stack.

expr_stmt(#expr_stmt{exp=Exp}, St0) ->
    {Ie,St1} = exp(Exp, single, St0),
    {Ie,St1}.

%% explist(Exprs, Values, State) -> {Instrs,State}.
%% exp(Expr, Values, State) -> {Instrs,State}.
%%  Values determines if we are to only return the first value of a
%%  list of values. Values multiple makes us a return a list!

explist([E], S, St) -> exp(E, S, St);		%Append values to output?
explist([E|Es], S, St0) ->
    {Ie,St1} = exp(E, single, St0),
    {Ies,St2} = explist(Es, S, St1),
    {Ie ++ Ies,St2};
explist([], _, St) -> {[],St}.			%No expressions at all

exp(#lit{v=L}, S, St) ->
    Is = [?PUSH_LIT(L)],
    {multiple_values(S, Is),St};
exp(#fdef{}=F, S, St0) ->
    {If,St1} = functiondef(F, St0),
    {multiple_values(S, If), St1};
exp(#op{op='and',as=[A1,A2]}, S, St0) ->
    {Ia1,St1} = exp(A1, S, St0),
    {Ia2,St2} = exp(A2, S, St1),
    {Ia1 ++ [?AND_THEN(Ia2)],St2};		%Must handle single/multiple
exp(#op{op='or',as=[A1,A2]}, S, St0) ->
    {Ia1,St1} = exp(A1, S, St0),
    {Ia2,St2} = exp(A2, S, St1),
    {Ia1 ++ [?OR_ELSE(Ia2)],St2};		%Must handle single/multiple
exp(#op{op=Op,as=As}, S, St0) ->
    {Ias,St1} = explist(As, single, St0),
    Iop = Ias ++ [?OP(Op,length(As))],
    {multiple_values(S, Iop),St1};
exp(#tc{fs=Fs}, S, St0) ->
    {Its,Fc,I,St1} = tableconstructor(Fs, St0),
    {Its ++ multiple_values(S, [?BUILD_TAB(Fc,I)]),St1};
exp(#lvar{n= <<"...">>}=V, S, St) ->		%Can be either local or frame
    {single_value(S, get_var(V)),St};
exp(#evar{n= <<"...">>}=V, S, St) ->
    {single_value(S, get_var(V)),St};
exp(E, S, St) ->
    prefixexp(E, S, St).

%% single_value(Values, Instrs) -> Instrs.
%% multiple_values(Values, Instrs) -> Instrs.
%%  Ensure either single value or multiple value.

single_value(single, Is) -> Is ++ [?SINGLE];
single_value(multiple, Is) -> Is.

multiple_values(single, Is) -> Is;
multiple_values(multiple, Is) -> Is ++ [?MULTIPLE].

%% prefixexp(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_rest(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_first(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_element(Expr, Values, State) -> {Instrs,State}.
%%  Single determines if we are to only return the first value of a
%%  list of values. Single false makes us a return a list!

prefixexp(#dot{e=Exp,r=Rest}, S, St0) ->
    {Ie,St1} = prefixexp_first(Exp, single, St0),
    {Ir,St2} = prefixexp_rest(Rest, S, St1),
    {Ie ++ Ir,St2};
prefixexp(Exp, S, St) -> prefixexp_first(Exp, S, St).

prefixexp_first(#single{e=E}, S, St0) ->
    {Ie,St1} = exp(E, single, St0),		%Will make it single
    {multiple_values(S, Ie),St1};
prefixexp_first(Var, S, St) ->
    {multiple_values(S, get_var(Var)),St}.

prefixexp_rest(#dot{e=Exp,r=Rest}, S, St0) ->
    {Ie,St1} = prefixexp_element(Exp, single, St0),
    {Ir,St2} = prefixexp_rest(Rest, S, St1),
    {Ie ++ Ir,St2};
prefixexp_rest(Exp, S, St) -> prefixexp_element(Exp, S, St).

prefixexp_element(#key{k=#lit{v=K}}, S, St) ->
    {multiple_values(S, [?GET_LIT_KEY(K)]),St};
prefixexp_element(#key{k=E}, S, St0) ->
    {Ie,St1} = exp(E, single, St0),
    {Ie ++ multiple_values(S, [?GET_KEY]),St1};
prefixexp_element(#fcall{as=[]}, S, St) ->
    Ifs = [?FCALL(0)],
    {single_value(S, Ifs),St};			%Function call returns list
prefixexp_element(#fcall{as=As}, S, St0) ->
    {Ias,St1} = explist(As, multiple, St0),
    Ifs = Ias ++ [?FCALL(length(As))],
    {single_value(S, Ifs),St1};			%Function call returns list
prefixexp_element(#mcall{m=#lit{v=K},as=[]}, S, St) ->
    Ims = [?MCALL(K, 0)],
    {single_value(S, Ims),St};			%Method call returns list
prefixexp_element(#mcall{m=#lit{v=K},as=As}, S, St0) ->
    {Ias,St1} = explist(As, multiple, St0),
    Ims = Ias ++ [?MCALL(K, length(As))],
    {single_value(S, Ims),St1}.			%Method call returns list

%% functiondef(Func, State) -> {Func,State}.
%%  This will return a single value which we leave on the stack.

functiondef(#fdef{ps=Ps0,ss=Ss,lsz=Lsz,esz=Esz}, St0) ->
    Ps1 = func_pars(Ps0),
    {Iss,St1} = stmts(Ss, St0),
    {[?FDEF(Lsz,Esz,Ps1,Iss)],St1}.

func_pars([#evar{n= <<"...">>,i=I}]) -> -I;	%Tail is index for varargs
func_pars([#lvar{n= <<"...">>,i=I}]) -> I;
func_pars([#evar{i=I}|Ps]) -> [-I|func_pars(Ps)];
func_pars([#lvar{i=I}|Ps]) -> [I|func_pars(Ps)];
func_pars([]) -> [].				%No varargs

%% tableconstructor(Fields, State) -> {Ifields,FieldCount,Index,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the last value pushed. Make sure
%%  that the last value is a multiple.

tableconstructor(Fs, St0) ->
    {Its,Fc,I,St1} = tc_fields(Fs, 0.0, St0),
    {Its,Fc,I,St1}.

tc_fields([#efield{v=V}], I0, St0) ->
    I1 = I0 + 1.0,				%Index of next element
    {Iv,St1} = exp(V, multiple, St0),
    {Iv,0,I1,St1};
tc_fields([#efield{v=V}|Fs], I0, St0) ->
    I1 = I0 + 1.0,				%Index of next element
    {Iv,St1} = exp(V, single, St0),
    {Ifs,Fc,I2,St2} = tc_fields(Fs, I1, St1),
    {[?PUSH_LIT(I1)] ++ Iv ++ Ifs,Fc+1,I2,St2};
tc_fields([#kfield{k=K,v=V}|Fs], I0, St0) ->
    {Ik,St1} = exp(K, single, St0),
    {Iv,St2} = exp(V, single, St1),
    {Ifs,Fc,I1,St3} = tc_fields(Fs, I0, St2),
    {Ik ++ Iv ++ Ifs,Fc+1,I1,St3};
tc_fields([], _, St) -> {[?PUSH_LIT([])],0,1.0,St}.
