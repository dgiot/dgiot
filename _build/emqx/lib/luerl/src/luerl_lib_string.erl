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

%% File    : luerl_lib_string.erl
%% Author  : Robert Virding
%% Purpose : The string library for Luerl.

-module(luerl_lib_string).

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

%% Export some test functions.
-export([test_gsub/3,test_match_pat/3,test_pat/1,
	 test_byte/3,test_do_find/4,test_sub/2,test_sub/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

%%-compile([bin_opt_info]).			%For when we are optimising

install(St0) ->
    {T,St1} = luerl_emul:alloc_table(table(), St0),
    {M,St2} = luerl_emul:alloc_table(metatable(T), St1),
    Meta0 = St2#luerl.meta,
    Meta1 = Meta0#meta{string=M},
    {T,St2#luerl{meta=Meta1}}.

%% metatable(Table) -> [{TableName,Table}].
%% table() -> [{FuncName,Function}].

metatable(T) ->					%String type metatable
    [{<<"__index">>,T}].

table() ->					%String table
    [{<<"byte">>,{function,fun byte/2}},
     {<<"char">>,{function,fun char/2}},
     {<<"dump">>,{function,fun dump/2}},
     {<<"find">>,{function,fun find/2}},
     {<<"format">>,{function, fun format/2}},
     {<<"gmatch">>,{function,fun gmatch/2}},
     {<<"gsub">>,{function,fun gsub/2}},
     {<<"len">>,{function,fun len/2}},
     {<<"lower">>,{function,fun lower/2}},
     {<<"match">>,{function,fun match/2}},
     {<<"rep">>,{function,fun rep/2}},
     {<<"reverse">>,{function,fun reverse/2}},
     {<<"sub">>,{function,fun sub/2}},
     {<<"upper">>,{function,fun upper/2}}
    ].

%% byte(String [, I [, J]] ) -> [Code]
%%  Return numerical codes of string between I and J.

byte(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,integer]) of
	[S|Is] ->
	    Bs = do_byte(S, byte_size(S), Is),
	    {Bs,St};
	_ -> badarg_error(byte, As, St)		%nil or []
    end.

test_byte(S, I, J) ->
    do_byte(S, byte_size(S), I, J).

do_byte(_, 0, _) -> [nil];
do_byte(S, Len, []) -> do_byte(S, Len, 1, 1);
do_byte(S, Len, [I]) -> do_byte(S, Len, I, I);
do_byte(S, Len, [I,J]) -> do_byte(S, Len, I, J).

do_byte(S, Len, I0, J0) ->			%The same as for sub
    I1 = do_sub_m(Len, I0),
    J1 = do_sub_m(Len, J0),
    do_byte_ij(S, Len, I1, J1).

do_byte_ij(S, Len, I, J) when I < 1 -> do_byte_ij(S, Len, 1, J);
do_byte_ij(S, Len, I, J) when J > Len -> do_byte_ij(S, Len, I, Len);
do_byte_ij(_, _, I, J) when I > J -> [nil];
do_byte_ij(S, _, I, J) ->
    [ float(N) || N <- binary_to_list(S, I, J) ].

%% char(...) -> String
%%  Return string of the numerical arguments.

char([nil], St) -> {[<<>>],St};
char(As, St) ->
    case catch list_to_binary(luerl_lib:to_ints(As)) of
	{'EXIT',_} -> badarg_error(char, As, St);
	B -> {[B],St}
    end.

%% dump(Function) -> String.
%%  Return a string with binary representation of Function.

-spec dump([_], _) -> no_return().

dump(As, St) -> badarg_error(dump, As, St).

%% find(String, Pattern [, Init [, Plain]]) -> [Indice].
%%  Return first occurrence of Pattern in String.

find(As, St0) ->
    try
	do_find(As, St0)
    catch
	throw:{error,E,St1} -> lua_error(E, St1);
	throw:{error,E} -> lua_error(E, St0)
    end.

do_find([A1,A2], St) -> do_find([A1,A2,1.0], St);
do_find([A1,A2,A3], St) -> do_find([A1,A2,A3,nil], St);
do_find(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,lua_string,integer,lua_bool]) of
	[S,P,I,Pl] -> {do_find(S, byte_size(S), P, I, Pl),St};
	_ -> throw({error,{badarg,find,As},St})	%nil, [_] or []
    end.

test_do_find(S, Pat, I, Pl) -> do_find(S, byte_size(S), Pat, I, Pl).

%% do_find(String, Length, Pattern, Start, Plain) -> [Return].
%% Adjust the starting index and find the string.

do_find(_, L, _, I, _) when I > L+1 -> [nil];
do_find(S, L, Pat, I, Pl) when I < -L -> do_find(S, L, Pat, 1, Pl);
do_find(S, L, Pat, I, Pl) when I < 0 -> do_find(S, L, Pat, L+I+1, Pl);
do_find(S, L, Pat, 0, Pl) ->  do_find(S, L, Pat, 1, Pl);
do_find(S, L, Pat, I, true) ->			%Plain text search string
    case binary:match(S, Pat, [{scope,{I-1,L-I+1}}]) of
	{Fs,Fl} -> [float(Fs+1),float(Fs+Fl)];
	nomatch -> [nil]
    end;
do_find(S, L, Pat0, I, false) ->		%Pattern search string
    case pat(binary_to_list(Pat0)) of
	{ok,{Pat1,_},_} ->
	    L1 = L - I + 1,			%Length of substring
	    S1 = binary_part(S, I-1, L1),	%Start searching from I
	    case match_loop(S1, L1, Pat1, 1) of
		[{_,P,Len}|Cas] ->		%Matches
		    P1 = P + I - 1,		%Position in original string
		    [float(P1),float(P1+Len-1)|match_caps(Cas, S, I)];
		[] -> [nil]			%No match
	    end;
	{error,E} -> throw({error,E})
    end.

%% format(Format, ...) -> [String].
%%  Format a string. All errors are badarg errors.
%%  Do all the work in luerl_string_format but generate errors here.

format([F|As], St0) ->
    try
	luerl_lib_string_format:format(F, As, St0)
    catch
	%% If we have a specific error, default is badarg.
	throw:{error,E,St1} -> lua_error(E, St1);
	throw:{error,E} -> lua_error(E, St0);
	_:_ -> badarg_error(format, [F|As], St0)
    end;
format(As, St) -> badarg_error(format, As, St).

-spec gmatch([_], _) -> no_return().		%To keep dialyzer quiet

%% gmatch(String, Pattern) -> [Function].

gmatch(As, St) -> badarg_error(gmatch, As, St).

%% gsub(String, Pattern, Repl [, N]) -> [String]

gsub(As, St0) ->
    try
	do_gsub(As, St0)
    catch
	throw:{error,E,St1} -> lua_error(E, St1);
	throw:{error,E} -> lua_error(E, St0)
    end.

do_gsub(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,lua_string,lua_any,integer]) of
	[S,P,R,N] when N > 0 ->
	    do_gsub(S, byte_size(S), P, R, N, St);
	[S,P,R] ->				%'all' bigger than any number
	    do_gsub(S, byte_size(S), P, R, all, St);
	_ -> throw({error,{badarg,gsub,As},St})
    end.

test_gsub(S, P, N) ->
    {ok,{Pat,_},_} = pat(binary_to_list(P)),
    gsub_match_loop(S, byte_size(S), Pat, 1, 1, N).

do_gsub(S, L, Pat0, R, N, St0) ->
    case pat(binary_to_list(Pat0)) of
	{ok,{Pat1,_},_} ->
	    Fs = gsub_match_loop(S, L, Pat1, 1, 1, N),
	    {Ps,St1} = gsub_repl_loop(Fs, S, 1, L, R, St0),
	    {[iolist_to_binary(Ps),float(length(Fs))],St1};
	{error,E} -> throw({error,E})
    end.

%% gsub_match_loop(S, L, Pat, I, C, N) -> [Cas].
%%  Return the list of Cas's for each match.

gsub_match_loop(_, _, _, _, C, N) when C > N -> [];
gsub_match_loop(<<>>, _, Pat, I, _, _) ->	%It can still match at end!
    case match_pat(<<>>, Pat, I) of
	{match,Cas,_,_} -> [Cas];
	nomatch -> []
    end;
gsub_match_loop(S0, L, Pat, I0, C, N) ->
    case match_pat(S0, Pat, I0) of
	{match,Cas,_,I0} ->			%Zero length match
	    S1 = binary_part(S0, 1, L-I0),
	    [Cas|gsub_match_loop(S1, L, Pat, I0+1, C+1, N)];
	{match,Cas,S1,I1} ->
	    [Cas|gsub_match_loop(S1, L, Pat, I1, C+1, N)];
	nomatch ->
	    S1 = binary_part(S0, 1, L-I0),
	    gsub_match_loop(S1, L, Pat, I0+1, C, N)
    end.

%% gsub_repl_loop([Cas], String, Index, Length, Reply, State) ->
%%     {iolist,State}.
%%  Build the return string as an iolist processing each match and
%%  filling in with the original string.

gsub_repl_loop([[{_,F,Len}|_]=Cas|Fs], S, I, L, R, St0) ->
    %% io:fwrite("grl: ~p\n", [{Cas,S,R}]),
    {Rep,St1} = gsub_repl(Cas, S, R, St0),
    %% io:fwrite("grl->~p\n", [{Rep}]),
    {Ps,St2} = gsub_repl_loop(Fs, S, F+Len, L, R, St1),
    {[binary_part(S, I-1, F-I),Rep|Ps],St2};
gsub_repl_loop([], S, I, L, _, St) ->
    {[binary_part(S, I-1, L-I+1)],St}.

gsub_repl(Cas, S, #tref{}=T, St0) ->
    case Cas of					%Export both Ca and Key
	[Ca] -> Key = match_cap(Ca, S);
	[Ca,Ca1|_] -> Key = match_cap(Ca1, S)
    end,
    {R,St1} = luerl_emul:get_table_key(T, Key, St0),
    {[gsub_repl_val(S, R, Ca)],St1};
gsub_repl(Cas0, S, Repl, St0) when element(1, Repl) =:= function ->
    case Cas0 of				%Export both Ca and Args
	[Ca] -> Args = [match_cap(Ca, S)];
	[Ca|Cas] -> Args = match_caps(Cas, S)
    end,
    {Rs,St1} = luerl_emul:functioncall(Repl, Args, St0),
    {[gsub_repl_val(S, luerl_lib:first_value(Rs), Ca)],St1};
gsub_repl(Cas, S, Repl, St) ->			%Replace string
    case luerl_lib:to_list(Repl) of
	nil -> {[],St};
	R -> {gsub_repl_str(Cas, S, R),St}
    end.

gsub_repl_str(Cas, S, [$%,$%|R]) ->
    [$%|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,$0|R]) ->
    Cstr = luerl_lib:tostring(match_cap(hd(Cas), S)), %Force to string
    [Cstr|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,C|R]) when C >= $1, C =< $9 ->
    case lists:keysearch(C-$0, 1, Cas) of
	{value,Ca} ->
	    Cstr = luerl_lib:tostring(match_cap(Ca, S)), %Force to string!
	    [Cstr|gsub_repl_str(Cas, S, R)];
	false -> throw({error,{illegal_index,capture,C-$0}})
    end;
gsub_repl_str(Cas, S, [C|R]) ->
    [C|gsub_repl_str(Cas, S, R)];
gsub_repl_str(_, _, []) -> [].

%% Return string or original match.

gsub_repl_val(S, Val, Ca) ->
    case luerl_lib:tostring(Val) of
	nil -> match_cap(Ca, S);		%Use original match
	Str -> Str
    end.

%% len(String) -> Length.

len([A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
len([A|_], St) when is_number(A) ->
    {[length(luerl_lib:number_to_list(A))],St};
len(As, St) -> badarg_error(len, As, St).

%% lower(String) -> String.

lower(As, St) ->
    case luerl_lib:conv_list(As, [list]) of
	[S] -> {[list_to_binary(string:to_lower(S))],St};
	_ -> badarg_error(lower, As, St)	%nil or []
    end.

%% match(String, Pattern [, Init]) -> [Match].

match(As, St0) ->
    try
	do_match(As, St0)
    catch
	throw:{error,E,St1} -> lua_error(E, St1);
	throw:{error,E} -> lua_error(E, St0)
    end.

do_match([A1,A2], St) -> do_match([A1,A2,1.0], St);
do_match(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,lua_string,integer]) of
	[S,P,I] -> {do_match(S, byte_size(S), P, I),St};
	_ -> throw({error,{badarg,match,As},St})
    end.

%% do_match(String, Length, Pattern, Start) -> [Return].
%% Adjust the starting index and find the match.

do_match(_, L, _, I) when I > L -> [nil];		%Shuffle values
do_match(S, L, Pat, I) when I < -L -> do_match(S, L, Pat, 1);
do_match(S, L, Pat, I) when I < 0 -> do_match(S, L, Pat, L+I+1);
do_match(S, L, Pat, 0) -> do_match(S, L, Pat, 1);
do_match(S, L, Pat0, I) ->
    case pat(binary_to_list(Pat0)) of		%"Compile" the pattern
	{ok,{Pat1,_},_} ->
	    L1 = L - I + 1,			%Length of substring
	    S1 = binary_part(S, I-1, L1),	%Start searching from I
	    case match_loop(S1, L1, Pat1, 1) of
		[{_,P,Len}] ->			%Only top level match
		    P1 = P + I - 1,		%Position in original string
		    [binary_part(S, P1-1, Len)];
		[_|Cas] ->			%Have sub matches
		    match_caps(Cas, S1);
		[] -> [nil]			%No match
	    end;
	{error,E} -> throw({error,E})
    end.

%% match_loop(String, Length, Pattern, Index) -> Cas | [].
%% Step down the string trying to find a match.

match_loop(S, L, Pat, I) when I > L ->		%It can still match at end!
    case match_pat(S, Pat, I) of
	{match,Cas,_,_} -> Cas;
	nomatch -> []				%Now we haven't found it
    end;
match_loop(S0, L, Pat, I) ->
    case match_pat(S0, Pat, I) of
	{match,Cas,_,_} -> Cas;
	nomatch ->
	    S1 = binary_part(S0, 1, L-I),
	    match_loop(S1, L, Pat, I+1)
    end.

%% match_cap(Capture, String [, Init]) -> Capture.
%% match_caps(Captures, String [, Init]) -> Captures.
%%  Get the captures. The string is the whole string not just from
%%  Init.

match_cap(Ca, S) -> match_cap(Ca, S, 1).

match_cap({_,P,Len}, _, I) when Len < 0 ->	%Capture position
    float(P+I-1);
match_cap({_,P,Len}, S, I) ->			%Capture
    binary_part(S, P+I-2, Len).			%Binaries count from 0

match_caps(Cas, S) -> match_caps(Cas, S, 1).

match_caps(Cas, S, I) -> [ match_cap(Ca, S, I) || Ca <- Cas ].

%% rep(String, N [, Separator]) -> [String].

rep([A1,A2], St) -> rep([A1,A2,<<>>], St);
rep([_,_,_|_]=As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,lua_string]) of
	[S,I,Sep] ->
	    if I > 0 ->
		    {[iolist_to_binary([S|lists:duplicate(I-1, [Sep,S])])],St};
	       true -> {[<<>>],St}
	    end;
	nil ->					%Error or bad values
	    badarg_error(rep, As, St)
    end;
rep(As, St) -> badarg_error(rep, As, St).

%% reverse([String], State) -> {[Res],St}.

reverse([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(lists:reverse(S))],St};
reverse(As, St) -> badarg_error(reverse, As, St).

%% sub([String, I [, J]], State) -> {[Res],State}.

sub(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,integer]) of
	[S,I|Js] ->
	    Len = byte_size(S),
	    Sub = do_sub(S, Len, I, Js),	%Just I, or both I and J
	    {[Sub],St};
	_ -> badarg_error(sub, As, St)		%nil, [_] or []
    end.

test_sub(S, I) -> do_sub(S, byte_size(S), I, []).
test_sub(S, I, J) -> do_sub(S, byte_size(S), I, [J]).

do_sub(S, _, 0, []) -> S;			%Special case this
do_sub(S, Len, I, []) -> do_sub_1(S, Len, I, Len);
do_sub(S, Len, I, [J]) -> do_sub_1(S, Len, I, J).

do_sub_1(S, Len, I0, J0) ->
    I1 = do_sub_m(Len, I0),
    J1 = do_sub_m(Len, J0),
    do_sub_ij(S, Len, I1, J1).

do_sub_m(Len, I) when I < 0 -> Len+I+1;		%Negative count from end
do_sub_m(_, I) -> I.

do_sub_ij(S, Len, I, J) when I < 1 -> do_sub_ij(S, Len, 1, J);
do_sub_ij(S, Len, I, J) when J > Len -> do_sub_ij(S, Len, I, Len);
do_sub_ij(_, _, I, J) when I > J -> <<>>;
do_sub_ij(S, _, I, J) ->
    binary:part(S, I-1, J-I+1).			%Zero-based, yuch!

upper([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(string:to_upper(S))],St};
upper(As, St) -> badarg_error(upper, As, St).

%% This is the pattern grammar used. It may actually be overkill to
%% first parse the pattern as the pattern is relativey simple and we
%% should be able to do it in one pass.
%%
%% pat -> seq : '$1'.
%% seq -> single seq : ['$1'|'$2'].
%% seq -> single : '$1'.
%% single -> "(" seq ")" .
%% single -> "[" class "]" : {char_class,char_class('$2')}
%% single -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% single -> char "*" .
%% single -> char "+" .
%% single -> char "-" .
%% single -> char "?" .
%% single -> char .
%% char -> "%" class .
%% char -> "." .
%% char -> char .
%%  The actual parser is a recursive descent implementation of the
%%  grammar. We leave ^ $ as normal characters and handle them
%%  specially in matching.

pat(Cs0) ->
    case catch seq(Cs0, 0, 1, []) of
	{error,E} -> {error,E};
	{P,0,Sn} -> {ok,{P,0},Sn};
	{_,_,_} -> {error,invalid_capture}
    end.

test_pat(P) -> pat(P).

seq([$^|Cs], Sd, Sn, P) -> single(Cs, Sd, Sn, ['^'|P]);
seq([_|_]=Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P);
seq([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

single([$(|Cs], Sd, Sn, P) -> single(Cs, Sd+1, Sn+1, [{'(',Sn}|P]);
single([$)|_], 0, _, _) -> throw({error,invalid_capture});
single([$)|Cs], Sd, Sn, P) -> single(Cs, Sd-1, Sn, [')'|P]);
single([$[|Cs], Sd, Sn, P) -> char_set(Cs, Sd, Sn, P);
single([$.|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, ['.'|P]);
single([$%|Cs], Sd, Sn, P) -> char_class(Cs, Sd, Sn, P);
single([$$], Sd, Sn, P) -> {lists:reverse(P, ['\$']),Sd,Sn};
single([C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [C|P]);
single([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

singlep([$*|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{kclosure,Char}|P]);
singlep([$+|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{pclosure,Char}|P]);
singlep([$-|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{mclosure,Char}|P]);
singlep([$?|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{optional,Char}|P]);
singlep(Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P).

char_set([$^|Cs], Sd, Sn, P) -> char_set(Cs, Sd, Sn, P, comp_set);
char_set(Cs, Sd, Sn, P) -> char_set(Cs, Sd, Sn, P, char_set).

char_set(Cs0, Sd, Sn, P, Tag) ->
    case char_set(Cs0) of
	{Set,[$]|Cs1]} -> singlep(Cs1, Sd, Sn, [{Tag,Set}|P]);
	{_,_} -> throw({error,invalid_char_set})
    end.

char_set([$]|Cs]) -> char_set(Cs, [$]]);	%Must special case this
char_set(Cs) -> char_set(Cs, []).

char_set([$]|_]=Cs, Set) -> {Set,Cs};		%We are at the end
char_set([$%,C|Cs], Set) ->
    char_set(Cs, [char_class(C)|Set]);
char_set([C1,$-,C2|Cs], Set) when C2 =/= $] ->
    char_set(Cs, [{C1,C2}|Set]);
char_set([C|Cs], Set) ->
    char_set(Cs, [C|Set]);
char_set([], Set) -> {Set,[]}.			%We are at the end

%% char_class([$f,$[|Cs], Sd, Sn, P) ->
%%     char_set(Cs, Sd, Sn, [frontier|P]);
char_class([$f|_], _, _, _) -> throw({error,invalid_pattern});
char_class([$b,L,R|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [{balance,L,R}|P]);
char_class([C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [char_class(C)|P]);
char_class([], _, _, _) -> throw({error,invalid_pattern}).

char_class($a) -> 'a';
char_class($A) -> 'A';
char_class($c) -> 'c';
char_class($C) -> 'C';
char_class($d) -> 'd';
char_class($D) -> 'D';
char_class($g) -> 'g';
char_class($G) -> 'G';
char_class($l) -> 'l';
char_class($L) -> 'L';
char_class($p) -> 'p';
char_class($P) -> 'P';
char_class($s) -> 's';
char_class($S) -> 'S';
char_class($u) -> 'u';
char_class($U) -> 'U';
char_class($w) -> 'w';
char_class($W) -> 'W';
char_class($x) -> 'x';
char_class($X) -> 'X';
char_class($z) -> 'z';				%Deprecated
char_class($Z) -> 'Z';
char_class(C) ->				%Only non-alphanum allowed
    case is_w_char(C) of
	true -> throw({error,{invalid_char_class,C}});
	false -> C
    end.

test_match_pat(S, P, I) ->
    {ok,{Pat,_},_} = pat(P),
    io:fwrite("tdm: ~p\n", [{Pat}]),
    match_pat(S, Pat, I).

%% match_pat(String, Pattern, Index) -> {match,[Capture],Rest,Index} | nomatch.
%%  Try and match the pattern with the string *at the current
%%  position*. No searching.

match_pat(S0, P0, I0) ->
    case match_pat(P0, S0, I0, [{0,I0}], []) of
	{match,S1,I1,_,Cas} ->{match,Cas,S1,I1};
	{nomatch,_,_,_,_,_} -> nomatch
    end.

match_pat(['\$']=Ps, Cs, I, Ca, Cas) ->		%Match only end of string
    case Cs of
	<<>> -> match_pat([], <<>>, I, Ca, Cas);
	_ -> {nomatch,Ps,Cs,I,Ca,Cas}
    end;
match_pat(['^'|Ps]=Ps0, Cs, I, Ca, Cas) ->	%Match beginning of string
    if I =:= 1 -> match_pat(Ps, Cs, 1, Ca, Cas);
       true -> {nomatch,Ps0,Cs,I,Cs,Cas}
    end;
match_pat([{'(',Sn},')'|P], Cs, I, Ca, Cas) ->
    match_pat(P, Cs, I, Ca, save_cap(Sn, I, -1, Cas));
match_pat([{'(',Sn}|P], Cs, I, Ca, Cas) ->
    match_pat(P, Cs, I, [{Sn,I}|Ca], Cas);
match_pat([')'|P], Cs, I, [{Sn,S}|Ca], Cas) ->
    match_pat(P, Cs, I, Ca, save_cap(Sn, S, I-S, Cas));
match_pat([{kclosure,P}=K|Ps], Cs, I, Ca, Cas) ->
    %%io:fwrite("dm: ~p\n", [{[P,K|Ps],Cs,I,Ca,Cas}]),
    case match_pat([P,K|Ps], Cs, I, Ca, Cas) of	%First try with it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try without it
	    match_pat(Ps, Cs, I, Ca, Cas)
    end;
match_pat([{pclosure,P}|Ps], Cs, I, Ca, Cas) ->	%The easy way
    match_pat([P,{kclosure,P}|Ps], Cs, I, Ca, Cas);
match_pat([{mclosure,P}=K|Ps], Cs, I, Ca, Cas) ->
    case match_pat(Ps, Cs, I, Ca, Cas) of	%First try without it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try with it
	    match_pat([P,K|Ps], Cs, I, Ca, Cas)
    end;
match_pat([{optional,P}|Ps], Cs, I, Ca, Cas) ->
    case match_pat([P|Ps], Cs, I, Ca, Cas) of	%First try with it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try without it
	    match_pat(Ps, Cs, I, Ca, Cas)
    end;
match_pat([{char_set,Set}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_set(Set, C) of
	true -> match_pat(Ps, Cs, I+1, Ca, Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
match_pat([{comp_set,Set}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_set(Set, C) of
	true -> {nomatch,Ps0,Cs0,I,Ca,Cas};
	false -> match_pat(Ps, Cs, I+1, Ca, Cas)
    end;
match_pat([{balance,L,R}|Ps]=Ps0, <<L,Cs1/binary>>=Cs0, I0, Ca, Cas) ->
    case balance(Cs1, I0+1, L, R, 1) of
	{ok,Cs2,I1} -> match_pat(Ps, Cs2, I1, Ca, Cas);
	error -> {nomatch,Ps0,Cs0,I0,Ca,Cas}
    end;
match_pat(['.'|Ps], <<_,Cs/binary>>, I, Ca, Cas) ->	%Matches anything
    match_pat(Ps, Cs, I+1, Ca, Cas);
match_pat([A|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) when is_atom(A) ->
    case match_class(A, C) of
	true -> match_pat(Ps, Cs, I+1, Ca, Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
match_pat([C|Ps], <<C,Cs/binary>>, I, Ca, Cas) ->
    match_pat(Ps, Cs, I+1, Ca, Cas);
match_pat([], Cs, I, [{Sn,S}|Ca], Cas) ->
    {match,Cs,I,Ca,[{Sn,S,I-S}|Cas]};
match_pat(Ps, Cs, I, Ca, Cas) ->
    {nomatch,Ps,Cs,I,Ca,Cas}.

%% save_cap(N, Position, Length, Captures) -> Captures.
%%  Add a new capture to the list in the right place, ordered.

save_cap(N, P, L, [{N1,_,_}=Ca|Cas]) when N > N1 ->
    [Ca|save_cap(N, P, L, Cas)];
save_cap(N, P, L, Cas) -> [{N,P,L}|Cas].

%% MUST first check for right char, this in case of L == R!
balance(<<R,Cs/binary>>, I, L, R, D) ->
    if D =:= 1 -> {ok,Cs,I+1};
       true -> balance(Cs, I+1, L, R, D-1)
    end;
balance(<<L,Cs/binary>>, I, L, R, D) -> balance(Cs, I+1, L, R, D+1);
balance(<<_,Cs/binary>>, I, L, R, D) -> balance(Cs, I+1, L, R, D);
balance(<<>>, _, _, _, _) -> error.

match_class('a', C) -> is_a_char(C);
match_class('A', C) -> not is_a_char(C);
match_class('c', C) -> is_c_char(C);
match_class('C', C) -> not is_c_char(C);
match_class('d', C) -> is_d_char(C);
match_class('D', C) -> not is_d_char(C);
match_class('g', C) -> is_g_char(C);
match_class('G', C) -> not is_g_char(C);
match_class('l', C) -> is_l_char(C);
match_class('L', C) -> not is_l_char(C);
match_class('p', C) -> is_p_char(C);
match_class('P', C) -> not is_p_char(C);
match_class('s', C) -> is_s_char(C);
match_class('S', C) -> not is_s_char(C);
match_class('u', C) -> is_u_char(C);
match_class('U', C) -> not is_u_char(C);
match_class('w', C) -> is_w_char(C);
match_class('W', C) -> not is_w_char(C);
match_class('x', C) -> is_x_char(C);
match_class('X', C) -> not is_x_char(C);
match_class('z', C) -> is_z_char(C);		%Deprecated
match_class('Z', C) -> not is_z_char(C).

match_char_set([{C1,C2}|_], C) when C >= C1, C=< C2 -> true;
match_char_set([A|Set], C) when is_atom(A) ->
    match_class(A, C) orelse match_char_set(Set, C);
match_char_set([C|_], C) -> true;
match_char_set([_|Set], C) -> match_char_set(Set, C);
match_char_set([], _) -> false.

%% Test for various character types.

is_a_char(C) ->					%All letters
    is_l_char(C) orelse is_u_char(C).

is_c_char(C) when C >= 0, C =< 31 -> true;	%All control characters
is_c_char(C) when C >= 128, C =< 159 -> true;
is_c_char(_) -> false.

is_d_char(C) -> (C >= $0) and (C =< $9).	%All digits

is_g_char(C) when C >= 33, C =< 126 -> true;	%All printable characters
is_g_char(C) when C >= 161, C =< 255 -> true;
is_g_char(_) -> false.

is_l_char(C) when C >= $a, C =< $z -> true;	%All lowercase letters
is_l_char(C) when C >= 224, C =< 246 -> true;
is_l_char(C) when C >= 248, C =< 255 -> true;
is_l_char(_) -> false.

is_p_char(C) when C >= 33, C =< 47 -> true;	%All punctutation characters
is_p_char(C) when C >= 58, C =< 63 -> true;
is_p_char(C) when C >= 91, C =< 96 -> true;
is_p_char(126) -> true;
is_p_char(C) when C >= 161, C =< 191 -> true;
is_p_char(215) -> true;
is_p_char(247) -> true;
is_p_char(_) -> false.

is_s_char(C) when C >= 9, C =< 13 -> true;	%Space characters
is_s_char(32) -> true;
is_s_char(160) -> true;
is_s_char(_) -> false.

is_u_char(C) when C >= $A, C =< $Z -> true;	%All uppercase letters
is_u_char(C) when C >= 192, C =< 214 -> true;
is_u_char(C) when C >= 216, C =< 223 -> true;
is_u_char(_) -> false.

is_w_char(C) ->					%All alphanumeric characters
    is_a_char(C) orelse is_d_char(C).

is_x_char(C) when C >= $a, C =< $f -> true;	%All hexadecimal characters
is_x_char(C) when C >= $A, C =< $F -> true;
is_x_char(C) -> is_d_char(C).

is_z_char(C) -> C =:= 0.			%The zero character, deprecated

%% match_class('a', C) -> (char_table(C) band ?_A) =/= 0;
%% match_class('A', C) ->  (char_table(C) band ?_A) =:= 0.

%% char_table(C) when C >= 0, C =< 31 -> ?_C;
%% char_table(C) when C >= 65, C =< 91 -> ?_U bor ?_A;
%% char_table(C) when C >= 97, C =< 123 -> ?_L;
