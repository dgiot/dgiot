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

%% File    : luerl_comp.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% This is the main loop of the Luerl compiler. While we can handle
%% errors in this loop they should never occur as Lua basically allows
%% almost everything that gets past the parser. The only exception are
%% goto's to undefined labels, but we don't handle goto's yet.
%%
%% We also have the first pass here. It normalises the code and
%% converts to an internal form.

-module(luerl_comp).

-export([file/1,file/2,string/1,string/2,forms/1,forms/2]).

-export([debug_print/3]).

-import(lists, [member/2,keysearch/3,mapfoldl/3]).

-include_lib("kernel/include/file.hrl").

-include("luerl.hrl").
-include("luerl_comp.hrl").

-record(comp, {base="",				%Base name
	       odir=".",			%Output directory
	       lfile="",			%Lua file
	       bfile="",			%Beam file
	       cfile="",			%Core file
	       opts=[],				%User options
	       mod=[],				%Module name
	       ret=file,			%What is returned [Val] | []
	       code=none,			%Code after last pass.
	       errors=[],
	       warnings=[]
	      }).

file(Name) -> file(Name, [verbose,report]).

file(Name, Opts) ->
    St0 = #comp{opts=Opts},
    St1 = filenames(Name, St0),
    compile(file_passes(), St1).

%% filenames(File, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, St) ->
    %% Test for explicit outdir.
    Odir = case keysearch(outdir, 1, St#comp.opts) of
	       {value,{outdir,D}} -> D;
	       false -> "."
	   end,
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".lua"),
    Lfile = filename:join(Dir, Base ++ ".lua"),
    Bfile = Base ++ ".beam",
    Cfile = Base ++ ".core",
    St#comp{base=Base,
	    lfile=Lfile,
	    odir=Odir,
	    bfile=filename:join(Odir, Bfile),
	    cfile=filename:join(Odir, Cfile)}.

string(Str) -> string(Str, [verbose,report]).

string(Str, Opts) when is_binary(Str) ->
    string(binary_to_list(Str), Opts);
string(Str, Opts) when is_list(Str) ->
    St0 = #comp{opts=Opts,code=Str},
    St1 = filenames("-no-file-", St0),
    compile(list_passes(), St1).

forms(Forms) -> forms(Forms, [verbose,report]).

forms(Forms, Opts) ->
    St0 = #comp{opts=Opts,code=Forms},
    St1 = filenames("-no-file-", St0),
    compile(forms_passes(), St1).

compile(Ps, St0) ->
    case do_passes(Ps, St0) of
	{ok,St1} -> do_ok_return(St1);
	{error, St1} -> do_error_return(St1)
    end.

%% file_passes() -> [Pass].
%% list_passes() -> [Pass].
%% forms_passes() -> [Pass].
%% do_passes(Passes, State) -> {ok,State} | {error,Reason}.
%%  {when_flag,Flag,Cmd}
%%  {unless_flag,Flag,Cmd}
%%  {do,Fun}
%%  {done,PrintFun,Ext}

file_passes() ->				%Reading from file
    [{do,fun do_read_file/1},
     {do,fun do_parse/1}|
     forms_passes()].

list_passes() ->				%Scanning string
    [{do,fun do_scan/1},
     {do,fun do_parse/1}|
     forms_passes()].

forms_passes() ->				%Doing the forms
    [{do,fun do_pass_1/1},
     {do,fun do_comp_vars/1},
     {when_flag,to_vars,{done,fun(St) -> {ok,St} end}},
     %% {do,fun do_comp_locf/1},
     {do,fun do_comp_env/1},
     {when_flag,to_env,{done,fun(St) -> {ok,St} end}},
     {do,fun do_code_gen/1},
     {unless_flag,no_iopt,{do,fun do_peep_op/1}}].

do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
	{ok,St1} -> do_passes(Ps, St1); 
	{error,St1} -> {error,St1}
    end;
do_passes([{when_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#comp.opts) of
	true -> do_passes([Cmd|Ps], St);
	false -> do_passes(Ps, St)
    end;
do_passes([{unless_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#comp.opts) of
	true -> do_passes(Ps, St);
	false -> do_passes([Cmd|Ps], St)
    end;
do_passes([{done,Fun}|_], St) ->
    Fun(St);
do_passes([], St) -> {ok,St}.

%% do_read_file(State) -> {ok,State} | {error,State}.
%% do_scan(State) -> {ok,State} | {error,State}.
%% do_parse(State) -> {ok,State} | {error,State}.
%% do_pass_1(State) -> {ok,State} | {error,State}.
%%  The actual compiler passes.

check_file_header(<<$#, Rest/binary>>) ->  % skip line
    skip_header_line(Rest);
check_file_header(<<239, 187, 191, Rest/binary>>) ->  % skip BOM
    Rest;
check_file_header(Binary) ->
    Binary.

skip_header_line(<<16#0D, Rest/binary>>) ->
    Rest;
skip_header_line(<<16#0A, Rest/binary>>) ->
    Rest;
skip_header_line(<<_H, Rest/binary>>) ->
    skip_header_line(Rest).

do_read_file(#comp{lfile=Name}=St) ->
    %% Read the bytes in a file skipping an initial # line or Windows BOM.
    case file:read_file(Name) of
        {ok, Binary} ->
            Data = binary_to_list(check_file_header(Binary)),
            {ok,Tokens,_EndLine} = luerl_scan:string(Data),
            {ok, St#comp{code = Tokens}};
        {error, Reason} ->
            {error,St#comp{errors=[{none,file,Reason}]}}
    end.


do_scan(#comp{code=Str}=St) ->
    case luerl_scan:string(Str) of
	{ok,Ts,_} -> {ok,St#comp{code=Ts}}; 
	{error,E,_} -> {error,St#comp{errors=[E]}}
    end.

do_parse(#comp{code=Ts}=St) ->
    case luerl_parse:chunk(Ts) of
	{ok,Chunk} -> {ok,St#comp{code=Chunk}};
	{error,E} -> {error,St#comp{errors=[E]}}
    end.

do_pass_1(#comp{code=C0,opts=Opts}=St) ->
    {ok,C1} = chunk(C0, Opts),
    {ok,St#comp{code=C1}}.

do_comp_vars(St) ->
    case luerl_comp_vars:chunk(St#comp.code, St#comp.opts) of
	{ok,C1} -> {ok,St#comp{code=C1}};
	{ok,C1,Ws} -> {ok,St#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,St#comp{errors=Es}}
    end.

%% do_comp_locf(St) ->
%%     case luerl_comp_locf:chunk(St#comp.code, St#comp.opts) of
%% 	{ok,C1} -> {ok,St#comp{code=C1}};
%% 	{ok,C1,Ws} -> {ok,St#comp{code=C1,warnings=Ws}};
%% 	{error,Es} -> {error,St#comp{errors=Es}}
%%     end.

do_comp_env(St) ->
    case luerl_comp_env:chunk(St#comp.code, St#comp.opts) of
	{ok,C1} -> {ok,St#comp{code=C1}};
	{ok,C1,Ws} -> {ok,St#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,St#comp{errors=Es}}
    end.

do_code_gen(St) ->
    case luerl_comp_cg:chunk(St#comp.code, St#comp.opts) of
	{ok,C1} -> {ok,St#comp{code=C1}};
	{ok,C1,Ws} -> {ok,St#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,St#comp{errors=Es}}
    end.

do_peep_op(St) ->
    case luerl_comp_peep:chunk(St#comp.code, St#comp.opts) of
	{ok,C1} -> {ok,St#comp{code=C1}};
	{ok,C1,Ws} -> {ok,St#comp{code=C1,warnings=Ws}};
	{error,Es} -> {error,St#comp{errors=Es}}
    end.

do_ok_return(#comp{code=C}) -> {ok,C}.

do_error_return(#comp{errors=Es,warnings=Ws}) ->
    {error,Es,Ws}.

debug_print(Opts, Format, Args) ->
    case member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% The first pass (pass_1).
%% Here we normalise the code and convert it to an internal form. 

%% chunk(Code, Options) -> {ok,Code} | {error,Reason}.

chunk(Code0, Opts) ->
    Cst = #cst{},				%Initialise common state
    {Code1,nil} = functiondef(Code0, nil),	%This is local!
    debug_print(Opts, "c: ~p\n", [Code1]),
    {ok,#code{code=Code1,cst=Cst}}.

%% stmts([{local,L,{functiondef,_,Name,_,_}=F}|Ss], St) ->
%%     %% Need to split this to handle recursive definitions.
%%     stmts([{local,L,{assign,L,[Name],[{nil,L}]}},F|Ss], St);
stmts([{';',_}|Ss], St) -> stmts(Ss, St);	%No-op so we drop it
stmts([S0|Ss0], St0) ->
    {S1,St1} = stmt(S0, St0),
    {Ss1,St2} = stmts(Ss0, St1),
    {[S1|Ss1],St2};
stmts([], St) -> {[],St}.

%% stmt(Statement, State) -> {CStat,State}.
%%  Do a statement. The ';' statement will caught and removed in stmts/2.

stmt({assign,Line,Vs,Es}, St) ->
    assign_stmt(Line, Vs, Es, St);
stmt({return,Line,Es}, St) ->
    return_stmt(Line, Es, St);
stmt({break,L}, St) ->				%Interesting
    {#break_stmt{l=L},St};
stmt({block,Line,B}, St) ->
    block_stmt(Line, B, St);
stmt({while,Line,Exp,B}, St) ->
    while_stmt(Line, Exp, B, St);
stmt({repeat,Line,B,Exp}, St) ->
    repeat_stmt(Line, B, Exp, St);
stmt({'if',Line,Tests,Else}, St) ->
    if_stmt(Line, Tests, Else, St);
stmt({for,Line,V,I,L,B}, St) ->			%Default step of 1.0
    numfor_stmt(Line, V, I, L, {'NUMBER',Line,1.0}, B, St);
stmt({for,Line,V,I,L,S,B}, St) ->
    numfor_stmt(Line, V, I, L, S, B, St);
stmt({for,Line,Ns,Gs,B}, St) ->
    genfor_stmt(Line, Ns, Gs, B, St);
stmt({functiondef,Line,Fname,Ps,B}, St) ->
    fdef_stmt(Line, Fname, Ps, B, St);
stmt({local,Line,Local}, St) ->
    local_stmt(Line, Local, St);
stmt(Exp, St) ->				%This is really just a call
    Line = element(2, Exp),
    call_stmt(Line, Exp, St).

%% assign_stmt(Line, Vars, Exps, State) -> {Assign,State}.

assign_stmt(Line, Vs, Es, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {#assign_stmt{l=Line,vs=Cvs,es=Ces},St2}.

assign_loop([V|Vs], St0) ->
    {Cv,St1} = var(V, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {[Cv|Cvs],St2};
assign_loop([], St) -> {[],St}.

%% var(VarExp, State) -> {VarExp,State}.
%%  Step down the prefixexp sequence evaluating as we go, stop at the
%%  END and return a key and a table where to put data. This is a
%%  prefixexp with different tail.

var({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var({'NAME',L,N}, St) -> {var_name(L, N),St}.

var_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string. NO!
    {#key{l=L,k=lit_name(L, N)},St};
var_last({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,k=Ce},St1}.

%% call_stmt(Line, Exp, State) -> {Call,State}.

call_stmt(Line, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#call_stmt{l=Line,call=Ce},St1}.

%% return_stmt(Line, Exps, State) -> {Return,State}.

return_stmt(Line, Es, St0) ->
    {Ces,St1} = explist(Es, St0),
    {#return_stmt{l=Line,es=Ces},St1}.

%% block_stmt(Line, Stats, State) -> {Block,Stmte}.

block_stmt(Line, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    {#block_stmt{l=Line,ss=Ss1},St1}.

block(Line, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    {#block{l=Line,ss=Ss1},St1}.

%% while_stmt(Line, Exp, Block, State) -> {While,State}.

while_stmt(Line, Exp, B, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,St2} = block(Line, B, St1),
    {#while_stmt{l=Line,e=Ce,b=Cb},St2}.

%% repeat_stmt(Line, Block, Exp, State) -> {Repeat,State}.
%%  Append the test expression into the block as a single value
%%  expression.

repeat_stmt(Line, B, Exp, St0) ->
    {Cb0,St1} = block(Line, B, St0),
    {Ce,St2} = expr_stmt(Line, {single,Line,Exp}, St1),
    Cb1 = Cb0#block{ss=Cb0#block.ss ++ [Ce]},
    {#repeat_stmt{l=Line,b=Cb1},St2}.

%% if_stmt(Line, Test, Else, State) -> {If,State}.

if_stmt(Line, Tests, Else, St0) ->
    {Cts,St1} = if_tests(Line, Tests, St0),
    {Ce,St2} = block(Line, Else, St1),
    {#if_stmt{l=Line,tests=Cts,else=Ce},St2}.

if_tests(L, Ts, St) ->
    Test = fun ({T,B}, S0) ->
		   {Ct,S1} = exp(T, S0),
		   {Cb,S2} = block(L, B, S1),
		   {{Ct,Cb},S2}
	   end,
    mapfoldl(Test, St, Ts).

%% numfor_stmt(Line, Var, Init, Limit, Step, Stmts, State) -> {NumFor,State}.

numfor_stmt(Line, {'NAME',Ln,N}, I0, L0, S0, Ss, St0) ->
    Var = var_name(Ln, N),
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {B,St2} = block(Line, Ss, St1),
    {#nfor_stmt{l=Line,v=Var,init=I1,limit=L1,step=S1,b=B},St2}.

%% genfor_stmt(Line, Vars, Generators, Stmts, State) -> {GenFor,State}.

genfor_stmt(Line, Vs0, Gs0, Ss, St0) ->
    Vs1 = [ var_name(Ln, N) || {'NAME',Ln,N} <- Vs0 ],
    {Gs1,St1} = explist(Gs0, St0),
    {B,St2} = block(Line, Ss, St1),
    {#gfor_stmt{l=Line,vs=Vs1,gens=Gs1,b=B},St2}.

%% fdef_stmt(Line, Name, Pars, Stmts, State) -> {Fdef,State}.
%%  Transform this to an assign.

fdef_stmt(Line, Fname, Ps, B, St0) ->
    {V,F,St1} = functiondef(Line, Fname, Ps, B, St0),
    {#assign_stmt{l=Line,vs=[V],es=[F]},St1}.

%% functiondef(FunctionDef, State) -> {CFunc,State}.
%% functiondef(Line, Pars, Block, State) -> {CFunc,State}.
%% functiondef(Line, Name, Pars, Block, State) -> {Var,CFunc,State}.
%%  Have to handle the case where the function is a "method". All this
%%  really means is that the function has an extra parameter 'self'
%%  prepended to the paramter list.

functiondef({functiondef,L,Ps,B}, St) ->
    functiondef(L, Ps, B, St).

functiondef(L, Ps, Stmts, St0) ->
    {Cp,Cb,St1} = function_block(Ps, Stmts, St0),
    {#fdef{l=L,ps=Cp,ss=Cb},St1}.

functiondef(L, Name0, Ps0, B, St0) ->
    %% Check if method and transform method to 'NAME'.
    case is_method(Name0) of			%Export Name1 and Ps1
	{yes,Name1} -> Ps1 = [{'NAME',L,self}|Ps0];
	no -> Name1 = Name0, Ps1 = Ps0
    end,
    {Var,St1} = funcname(Name1, St0),
    {F,St2} = functiondef(L, Ps1, B, St1),
    {Var,F,St2}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
        {yes,Rest1} -> {yes,{'.',L,N,Rest1}};
        no -> no                                %No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

%% funcname(FuncNameExp, State) -> {CFuncNameExp,State}.

funcname({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_first(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_first({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_element(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname_rest(Exp, St) ->
    funcname_last(Exp, St).

funcname_element({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,k=lit_name(L, N)},St}.

%% Method call has been transformed away
funcname_last({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,k=lit_name(L, N)},St}.

%% local_stmt(Line, Local, State) -> {Assign,State}.
%%  Create and assign local variables.

local_stmt(Line, {functiondef,Lf,Name,Ps,B}, St0) ->
    {Var,F,St1} = functiondef(Lf, Name, Ps, B, St0),
    {#local_fdef_stmt{l=Line,v=Var,f=F},St1};
local_stmt(Line, {assign,_,Ns,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cns,St2} = mapfoldl(fun (V, St) -> var(V, St) end, St1, Ns),
    {#local_assign_stmt{l=Line,vs=Cns,es=Ces},St2}.

%% expr_stmt(Line, Exp, State) -> {Call,State}.
%%  The expression pseudo statement. This will return a single value.

expr_stmt(Line, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#expr_stmt{l=Line,exp=Ce},St1}.

%% explist(Exprs, State) -> {Ins,State}.
%% exp(Expression, State) -> {Ins,State}.

explist([E|Es], St0) ->
    {Ce,St1} = exp(E, St0),
    {Ces,St2} = explist(Es, St1),
    {[Ce|Ces],St2};
explist([], St) -> {[],St}.			%No expressions at all

exp({nil,L}, St) -> {#lit{l=L,v=nil},St};
exp({false,L}, St) -> {#lit{l=L,v=false},St};
exp({true,L}, St) -> {#lit{l=L,v=true},St};
exp({'NUMBER',L,N}, St) -> {#lit{l=L,v=N},St};
exp({'STRING',L,S}, St) -> {#lit{l=L,v=S},St};
exp({'...',L}, St) ->
    {var_name(L, '...'),St};
    %% {#lit{l=L,v='...'},St};
exp({functiondef,L,Ps,B}, St0) ->
    {Cf,St1} = functiondef(L, Ps, B, St0),
    {Cf,St1};
exp({table,L,Fs}, St0) ->
    {Cfs,St1} = tableconstructor(Fs, St0),
    {#tc{l=L,fs=Cfs},St1};
exp({op,L,Op,A1,A2}, St0) ->
    {Ca1,St1} = exp(A1, St0),
    {Ca2,St2} = exp(A2, St1),
    {#op{l=L,op=Op,as=[Ca1,Ca2]},St2};
exp({op,L,Op,A}, St0) ->
    {Ca,St1} = exp(A, St0),
    {#op{l=L,op=Op,as=[Ca]},St1};
exp(E, St) ->
    prefixexp(E, St).

%% prefixexp(PrefixExp, State) -> {CPrefixExp,State}.

prefixexp({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',L,N}, St) ->
    {var_name(L, N),St};
prefixexp_first({single,L,E}, St0) ->
    {Ce,St1} = exp(E, St0),
    {#single{l=L,e=Ce},St1}.

prefixexp_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string
    {#key{l=L,k=lit_name(L, N)},St};
prefixexp_element({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,k=Ce},St1};
prefixexp_element({functioncall,L,Args}, St0) ->
    {Cas,St1} = explist(Args, St0),
    {#fcall{l=L,as=Cas},St1};
prefixexp_element({methodcall,Lm,{'NAME',Ln,N},Args}, St0) ->
    {Args1,St1} = explist(Args, St0),
    {#mcall{l=Lm,m=lit_name(Ln, N),as=Args1},St1}.

dot(L, Exp, Rest) -> #dot{l=L,e=Exp,r=Rest}.

function_block(Pars, Stmts, St0)->
    {Cps,St1} = make_local_pars(Pars, St0),
    {Cs,St2} = stmts(Stmts, St1),
    %% io:format("fb: ~p\n", [{St3#comp.fs}]),
    {Cps,Cs,St2}.

make_local_pars(Ps, St) ->
    Add = fun ({'NAME',L,N}, S) -> {var_name(L, N),S};
	      ({'...',L}, S) -> {var_name(L, '...'),S}
	  end,
    mapfoldl(Add, St, Ps).

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted. We can't unpack the last field if it is a multiple
%%  value efield as this must be done at run-time.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a MAPFOLDL!!
    Fun = fun ({exp_field,L,Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#efield{l=L,v=Ce},S1};
	      ({name_field,L,{'NAME',Ln,N},Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#kfield{l=L,k=lit_name(Ln, N),v=Ce},S1};
	      ({key_field,L,Ke,Ve}, S0) ->
		  {Ck,S1} = exp(Ke, S0),	%Key
		  {Cv,S2} = exp(Ve, S1),	%Value
		  {#kfield{l=L,k=Ck,v=Cv},S2}
	  end,
    {Cfs,St1} = mapfoldl(Fun, St0, Fs),
    {Cfs,St1}.

%% name_string(Name) -> String.
%% var_name(Line, Name) -> #var{}.
%% lit_name(Line, Name) -> #lit{}.

name_string(Name) -> atom_to_binary(Name, latin1).

lit_name(L, N) -> #lit{l=L,v=name_string(N)}.

var_name(L, N) -> #var{l=L,n=name_string(N)}.
