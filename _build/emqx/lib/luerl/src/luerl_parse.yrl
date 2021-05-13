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

%% File    : luerl_parse.yrl
%% Author  : Robert Virding
%% Purpose : Parser for LUA 5.2.

%% The Grammar rules here are taken directly from the LUA 5.2
%% manual. Unfortunately it is not an LALR(1) grammar but I have
%% included a fix by Florian Weimer <fw@deneb.enyo.de> which makes it
%% so, but it needs some after processing. Actually his fix was
%% unnecessarily complex and all that was needed was to change one
%% rule for statements.

Expect 2.					%Suppress shift/reduce warning

Nonterminals
chunk block stats stat semi retstat label_stat
while_stat repeat_stat if_stat if_elseif if_else for_stat local_decl
funcname dottedname varlist var namelist
explist exp prefixexp args
functioncall
functiondef funcbody parlist
tableconstructor fieldlist fields field fieldsep
binop unop uminus.

Terminals
NAME NUMBER STRING

'and' 'break' 'do' 'else' 'elseif' 'end' 'false' 'for' 'function' 'goto' 'if' 
'in' 'local' 'nil' 'not' 'or' 'repeat' 'return' 'then' 'true' 'until' 'while' 

'+' '-' '*' '/' '%' '^' '#' '==' '~=' '<=' '>=' '<' '>' '='
'(' ')' '{' '}' '[' ']' '::' ';' ':' ',' '.' '..' '...' .


Rootsymbol chunk.

Left 100 'or'.
Left 200 'and'.
Left 300 '<' '>' '<=' '>=' '~=' '=='.
Right 400 '..'.
Left 500 '+' '-'.
Left 600 '*' '/' '%'.
Unary 700 'not' '#' uminus.
Right 800 '^'.

chunk -> block : '$1'  .

%% block ::= {stat} [retstat]

block -> stats : '$1' .
block -> stats retstat : '$1' ++ ['$2'] .

retstat -> return semi : {return,line('$1'),[]} .
retstat -> return explist semi : {return,line('$1'),'$2'} .

semi -> ';' .					%semi is never returned
semi -> '$empty' .

stats -> '$empty' : [] .
stats -> stats stat : '$1' ++ ['$2'] .

stat -> ';' : '$1' .
stat -> varlist '=' explist : {assign,line('$2'),'$1','$3'} .
%% Following functioncall rule removed to stop reduce-reduce conflict.
%% Replaced with a prefixexp which should give the same. We hope!
%%stat -> functioncall : '$1' .
stat -> prefixexp : check_functioncall('$1') .
stat -> label_stat : '$1' .
stat -> 'break' : {break,line('$1')} .
stat -> 'goto' NAME : {goto,line('$1'),'$2'} .
stat -> 'do' block 'end' : {block,line('$1'),'$2'} .
stat -> while_stat : '$1' .
stat -> repeat_stat : '$1' .
stat -> if_stat : '$1' .
stat -> for_stat : '$1' .
stat -> function funcname funcbody : functiondef(line('$1'),'$2','$3') .
stat -> local local_decl : {local,line('$1'),'$2'} .

label_stat -> '::' NAME '::' : {label,line('$1'),'$2'} .

while_stat -> 'while' exp 'do' block 'end' : {while,line('$1'),'$2','$4'} .

repeat_stat -> 'repeat' block 'until' exp : {repeat,line('$1'),'$2','$4'} .

%% stat ::= if exp then block {elseif exp then block} [else block] end
if_stat -> 'if' exp 'then' block if_elseif if_else 'end' :
	{'if',line('$1'),[{'$2','$4'}|'$5'],'$6'} .

if_elseif -> if_elseif 'elseif' exp 'then' block : '$1' ++ [{'$3','$5'}] .
if_elseif -> '$empty' : [] .

if_else -> 'else' block : '$2' .
if_else -> '$empty' : [] .			%An empty block

%% stat ::= for Name '=' exp ',' exp [',' exp] do block end
%% stat ::= for namelist in explist do block end

for_stat -> 'for' NAME '=' explist do block end : 
	    numeric_for(line('$1'), '$2', '$4', '$6') .
for_stat -> 'for' namelist 'in' explist 'do' block 'end' :
	    generic_for(line('$1'), '$2', '$4', '$6') .

%% funcname ::= Name {'.' Name} [':' Name]

funcname -> dottedname ':' NAME :
		dot_append(line('$2'), '$1', {method,line('$2'),'$3'}) .
funcname -> dottedname : '$1' .

local_decl -> function NAME funcbody :
		  functiondef(line('$1'),'$2','$3') .
local_decl -> namelist : {assign,line(hd('$1')),'$1',[]} .
local_decl -> namelist '=' explist : {assign,line('$2'),'$1','$3'} .

dottedname -> NAME : '$1'.
dottedname -> dottedname '.' NAME : dot_append(line('$2'), '$1', '$3') . 

varlist -> var : ['$1'] .
varlist -> varlist ',' var : '$1' ++ ['$3'] .

var -> NAME : '$1' .
var -> prefixexp '[' exp ']' :
	   dot_append(line('$2'), '$1', {key_field,line('$2'),'$3'}) .
var -> prefixexp '.' NAME : dot_append(line('$2'), '$1', '$3') . 

namelist -> NAME : ['$1'] .
namelist -> namelist ',' NAME : '$1' ++ ['$3'] .

explist -> exp : ['$1'] .
explist -> explist ',' exp : '$1' ++ ['$3'] .

exp -> 'nil' : '$1' .
exp -> 'false' : '$1' .
exp -> 'true' : '$1' .
exp -> NUMBER : '$1' .
exp -> STRING : '$1' .
exp -> '...' : '$1' .
exp -> functiondef : '$1' .
exp -> prefixexp : '$1' .
exp -> tableconstructor : '$1' .
exp -> binop : '$1' . 
exp -> unop : '$1' .

prefixexp -> var : '$1' .
prefixexp -> functioncall : '$1' .
prefixexp -> '(' exp ')' : {single,line('$1'),'$2'} .

functioncall -> prefixexp args :
		    dot_append(line('$1'), '$1', {functioncall,line('$1'), '$2'}) .
functioncall -> prefixexp ':' NAME args :
		    dot_append(line('$2'), '$1', {methodcall,line('$2'),'$3','$4'}) .

args -> '(' ')' : [] .
args -> '(' explist ')' : '$2' .
args -> tableconstructor : ['$1'] .		%Syntactic sugar
args -> STRING : ['$1'] .			%Syntactic sugar

functiondef -> 'function' funcbody : functiondef(line('$1'), '$2').

funcbody -> '(' ')' block 'end' : {[],'$3'} .
funcbody -> '(' parlist ')' block 'end' : {'$2','$4'} .

parlist -> namelist : '$1' .
parlist -> namelist ',' '...' : '$1' ++ ['$3'] .
parlist -> '...' : ['$1'] .

tableconstructor -> '{' '}' : {table,line('$1'),[]} .
tableconstructor -> '{' fieldlist '}' : {table,line('$1'),'$2'} .

fieldlist -> fields : '$1' .
fieldlist -> fields fieldsep : '$1' .

fields ->  field : ['$1'] .
fields ->  fields fieldsep field : '$1' ++ ['$3'] .

field -> '[' exp ']' '=' exp : {key_field,line('$1'),'$2','$5'} .
field -> NAME '=' exp : {name_field,line('$1'),'$1','$3'} .
field -> exp : {exp_field,line('$1'),'$1'} .

fieldsep -> ',' .
fieldsep -> ';' .

%% exp ::= exp binop exp
%% exp ::= unop exp
%% We have to write them these way for the prioriies to work.

binop -> exp '+' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '-' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '*' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '/' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '%' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '^' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '==' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '~=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '<=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '>=' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '<' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '>' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp '..' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp 'and' exp : {op,line('$2'),cat('$2'),'$1','$3'}.
binop -> exp 'or' exp : {op,line('$2'),cat('$2'),'$1','$3'}.

unop -> 'not' exp : {op,line('$1'),cat('$1'),'$2'} .
unop -> '#' exp :  {op,line('$1'),cat('$1'),'$2'} .
unop -> uminus : '$1' .
     
uminus -> '-' exp : {op,line('$1'),'-','$2'} .

Erlang code.

-export([chunk/1]).

%% chunk(Tokens) -> FunctionDef | Error.
%% Return the parse as a callable nameless function definition.

chunk(Ts) ->
    case parse(Ts) of
        {error,_}=Error -> Error;
        {ok,Body} -> {ok,{functiondef,1,[{'...',1}],Body}}
    end.

cat(T) -> element(1, T).
line(T) -> element(2, T).

%% numeric_for(Line, LoopVar, [Init,Test,Upd], Block).

numeric_for(Line, Var, [Init,Limit], Block) ->
    {for,Line,Var,Init,Limit,Block};
numeric_for(Line, Var, [Init,Limit,Step], Block) ->
    {for,Line,Var,Init,Limit,Step,Block};
numeric_for(Line, _, _, _) ->			%Wrong number of expressions
    return_error(Line, "illegal for").

%% generic_for(Line, Names, ExpList, Block).

generic_for(Line, Names, Exps, Block) ->
    {for,Line,Names,Exps,Block}.

%% functiondef(Line, Name, {Parameters,Body}).
%% functiondef(Line, {Parameters,Body}).

functiondef(Line, Name, {Pars,Body}) ->
    {functiondef,Line,Name,Pars,Body}.

functiondef(Line, {Pars,Body}) ->
    {functiondef,Line,Pars,Body}.

%% dot_append(DotList, Last) -> DotList.
%%  Append Last to the end of a dotlist.

dot_append(Line, {'.',L,H,T}, Last) ->
    {'.',L,H,dot_append(Line, T, Last)};
dot_append(Line, H, Last) -> {'.',Line,H,Last}.

%% check_functioncall(PrefixExp) -> PrefixExp.
%%  Check that the PrefixExp is a proper function call/method.

check_functioncall({functioncall,_,_}=C) -> C;
check_functioncall({methodcall,_,_,_}=M) -> M;
check_functioncall({'.',L,H,T}) ->
    {'.',L,H,check_functioncall(T)};
check_functioncall(Other) ->
    return_error(line(Other),"illegal call").
