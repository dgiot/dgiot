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

%% File    : luerl_comp.hrl
%% Author  : Robert Virding
%% Purpose : Internal LUA 5.2 compiler definitions.

%% The code compile info.
-record(code, {code=none,			%Code
	       cst=none				%Shared compiler state
	      }).

%% Compiler state passed between passes.
-record(cst, {}).				%Nothing yet

%% Variable data.
-record(vars, {local=[],free=[],		%Local, free variables
	       used=[],				%Used in sub blocks
	       fused=[]				%Used in sub-functions
	      }).

%% Define internal data macros.

%% Statements.
-record(assign_stmt, {l,vs,es}).

-record(call_stmt, {l,call}).

-record(return_stmt, {l,es}).

-record(break_stmt, {l}).

-record(block_stmt, {l,ss=[],			%Block statement
		     vars=none,			%Variable info
		     lsz=none,			%Local frame size
		     lf=[],			%Local frame
		     esz=none,			%Env frame size
		     ef=[],			%Env frame
		     local=none,		%Local variables
		     locf=false}).		%Local functions

-record(while_stmt, {l,e,b=[]}).

-record(repeat_stmt, {l,b=[]}).

-record(nfor_stmt, {l,v,init,limit,step,b=[]}).

-record(gfor_stmt, {l,vs,gens,b=[]}).

-record(if_stmt, {l,tests=[],else}).

-record(local_assign_stmt, {l,vs,es}).

-record(local_fdef_stmt, {l,v,f}).

-record(expr_stmt, {l,exp}).			%Pseudo stmt for expressions

-record(block, {l,ss=[],			%Sub-blocks
		vars=none,			%Variable info
		lsz=none,			%Local frame size
		lf=[],				%Local frame
		esz=none,			%Env frame size
		ef=[],
		locf=false}).

%% Expressions.
-record(fdef, {l,ps=[],ss=[],
	       vars=none,			%Variable info
	       lsz=none,			%Local frame size
	       lf=[],
	       esz=none,			%Env frame size
	       ef=[],
	       local=none,			%Local variables
	       locf=false}).			%Local function

-record(lit, {l,v}).

-record(op, {l,op,as=[]}).

-record(single, {l,e}).

-record(dot, {l,e,r}).

-record(var, {l,n}).

-record(fcall, {l,as=[]}).

-record(mcall, {l,m,as=[]}).

-record(key, {l,k}).

-record(tc, {l,fs=[]}).

-record(efield, {l,v}).

-record(kfield, {l,k,v}).

%% Variable types.
-record(lvar, {n,d,i}).				%Local name, depth, index
-record(evar, {n,d,i}).				%Environment name, depth, index
-record(gvar, {n}).				%Global name
