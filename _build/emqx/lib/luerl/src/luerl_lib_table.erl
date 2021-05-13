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

%% File    : luerl_lib_table.erl
%% Author  : Robert Virding
%% Purpose : The table library for Luerl.

%% These functions sometimes behave strangely in the Lua 5.2
%% libraries, but we try to follow them. Most of these functions KNOW
%% that a table is a ttdict! We know that the erlang array has default
%% value 'nil'.

-module(luerl_lib_table).

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

%% Export some functions which can be called from elsewhere.
-export([concat/4,concat/5,raw_length/2,length/2,unpack/2]).

%% Export some test functions.
-export([test_concat/1,
	 test_insert/2,test_insert/3,
	 test_remove/2,test_remove/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"concat">>,{function,fun concat/2}},
     {<<"insert">>,{function,fun insert/2}},
     {<<"pack">>,{function,fun pack/2}},
     {<<"remove">>,{function,fun remove/2}},
     {<<"sort">>,{function,fun sort/2}},
     {<<"unpack">>,{function,fun unpack/2}}
    ].

%% concat - concat the elements of a list into a string.

concat(As, St0) ->
    try
	do_concat(As, St0)
    catch
	throw:{error,E,St1} -> lua_error(E, St1);
	throw:{error,E} -> lua_error(E, St0)
    end.

do_concat([#tref{i=N}|As], St) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(N, St#luerl.ttab),
    case luerl_lib:conv_list(concat_args(As), [lua_string,integer,integer]) of
	[Sep,I] ->
	    {[do_concat(Arr, Dict, Sep, I, length_loop(Arr))],St};
	[Sep,I,J] ->
	    {[do_concat(Arr, Dict, Sep, I, J)],St};
	_ -> throw({error,{badarg,concat,As},St})
    end;
do_concat(As, St) -> throw({error,{badarg,concat,As},St}).

%% concat(Table, Sep, I, State) -> string().
%% concat(Table, Sep, I, J, State) -> string().
%%  Concatenate elements in a list into a string. Callable from
%%  Erlang.

concat(#tref{i=N}, Sep, I, St) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(N, St#luerl.ttab),
    J = length_loop(Arr),
    do_concat(Arr, Dict, Sep, I, J).

concat(#tref{i=N}, Sep, I, J, St) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(N, St#luerl.ttab),
    do_concat(Arr, Dict, Sep, I, J).

test_concat(As) -> concat_args(As).

concat_args([]) -> concat_args([<<>>]);
concat_args([nil|As]) -> concat_args([<<>>|As]);
concat_args([Sep]) -> [Sep,1.0];
concat_args([Sep,nil|As]) -> concat_args([Sep,1.0|As]);
concat_args([Sep,I]) -> [Sep,I];
concat_args([Sep,I,nil|_]) -> [Sep,I];
concat_args([Sep,I,J|_]) -> [Sep,I,J].

do_concat(Arr, Dict, Sep, I, J) ->
    Conc = concat_table(Arr, Dict, I, J),
    concat_join(Conc, Sep).

concat_table(Arr, Dict, I, J) ->
    concat_tab(Arr, Dict, I, J).

%% This and unpack_loop are very similar.
%% First scan over table up to 0 then the array. We have the indexes
%% and limits as integers and explicitly use '==' to compare with
%% float values in table.

concat_tab(_, _, N, J) when N > J -> [];	%Done
concat_tab(Arr, _, N, J) when N > 0 ->		%Done with table
    concat_arr(Arr, N, J);
concat_tab(Arr, Dict, N, J) ->
    case ttdict:find(N, Dict) of
	{ok,V} ->
	    case luerl_lib:to_list(V) of
		nil -> throw({error,{illegal_val,concat,V}});
		S -> [S|concat_tab(Arr, Dict, N+1, J)]
	    end;
	error -> throw({error,{illegal_val,concat,nil}})
    end.

concat_arr(_, N, J) when N > J -> [];
concat_arr(Arr, N, J) ->
    V = array:get(N, Arr),
    case luerl_lib:to_list(V) of
	nil -> throw({error,{illegal_val,concat,V}});
	S -> [S|concat_arr(Arr, N+1, J)]
    end.

concat_join([E], _) -> list_to_binary(E);
concat_join([E1|Es], Sep) ->
    iolist_to_binary([E1|[ [Sep,E] || E <- Es ]]);
concat_join([], _) -> <<>>.

%% insert(Table, [Pos,] Value) -> []
%%  Insert an element into a list shifting following elements.

insert([#tref{i=N},V], St) ->
    Ts0 = St#luerl.ttab,
    #table{a=Arr0} = T = ?GET_TABLE(N, Ts0),
    Arr1 = do_insert_last(Arr0, V),
    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
    {[],St#luerl{ttab=Ts1}};
insert([#tref{i=N},P0,V]=As, St) ->
    Ts0 = St#luerl.ttab,
    #table{a=Arr0} = T = ?GET_TABLE(N, Ts0),
    Size = length_loop(Arr0),
    case luerl_lib:to_int(P0) of
	P1 when P1 >=1, P1 =< Size+1 ->
	    Arr1 = do_insert(Arr0, P1, V),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    {[],St#luerl{ttab=Ts1}};
	_ -> badarg_error(insert, As, St)
    end;
insert(As, St) -> badarg_error(insert, As, St).

test_insert(A, V) -> do_insert_last(A, V).
test_insert(A, N, V) -> do_insert(A, N, V).

%% do_insert_last(Array, V) -> Array.
%%  Get the "length" of the first bit and put value in first slot
%%  after that.

do_insert_last(Arr, V) ->
    Len = length_loop(Arr),			%Get "length"
    array:set(Len+1, V, Arr).			%Set the value

%% do_insert(Array, P, V) -> Array.
%%  We only insert elements inside the "proper" 1..n table.

do_insert(Arr, P, V) ->				%Go to the array part
    insert_array(Arr, P, V).

insert_array(Arr0, N, Here) ->			%Put this at N shifting up
    case array:get(N, Arr0) of
	nil -> array:set(N, Here, Arr0);	%Just fill hole
	Next ->					%Take value for next slot
	    Arr1 = array:set(N, Here, Arr0),
	    insert_array(Arr1, N+1, Next)
    end.

%% remove(Table [,Pos]) -> Value.
%%  Remove an element from a list shifting following elements.

remove([#tref{i=N}], St) ->
    Ts0 = St#luerl.ttab,
    #table{a=Arr0,d=Dict0} = T = ?GET_TABLE(N, Ts0),
    {Ret,Arr1,Dict1} = do_remove_last(Arr0, Dict0),
    Ts1 = ?SET_TABLE(N, T#table{a=Arr1,d=Dict1}, Ts0),
    {Ret,St#luerl{ttab=Ts1}};
remove([#tref{i=N},P0|_]=As, St) ->
    Ts0 = St#luerl.ttab,
    #table{a=Arr0,d=Dict0} = T = ?GET_TABLE(N, Ts0),
    case luerl_lib:to_int(P0) of
	P1 when P1 =/= nil ->
	    case do_remove(Arr0, Dict0, P1) of
		{Ret,Arr1,Dict1} ->
		    Ts1 = ?SET_TABLE(N, T#table{a=Arr1,d=Dict1}, Ts0),
		    {Ret,St#luerl{ttab=Ts1}};
		badarg -> badarg_error(remove, As, St)
	    end;
	_ -> badarg_error(remove, As, St)	%nil or P < 1
    end;
remove(As, St) -> badarg_error(remove, As, St).

test_remove(Arr, Dict) -> do_remove_last(Arr, Dict).
test_remove(Arr, Dict, N) -> do_remove(Arr, Dict, N). 

%% do_remove_last(Array, Dict) -> {Return,Array,Dict}.
%%  Find the length and remove the last element. Return it even if it
%%  is nil.

do_remove_last(Arr0, Dict0) ->
    case length_loop(Arr0) of
	0 ->
	    do_remove_0(Arr0, Dict0);
	Size ->
	    Val = array:get(Size, Arr0),
	    Arr1 = array:set(Size, nil, Arr0),
	    {[Val],Arr1,Dict0}
    end.

do_remove_0(Arr, Dict0) ->
    case ttdict:find(0.0, Dict0) of
	{ok,Val} ->
	    Dict1 = ttdict:erase(0.0, Dict0),
	    {[Val],Arr,Dict1};
	error ->
	    {[nil],Arr,Dict0}
    end.

%% do_remove(Array, Dict, P) -> {Return,Array,Dict} | badarg.
%%  Don't ask, it tries to emulate the "real" Lua, where we can't
%%  remove elements elements outside of the "proper" 1..n dict.

do_remove(Arr, Dict, P) ->
    do_remove(Arr, Dict, P, length_loop(Arr)).

do_remove(Arr, Dict, 0, 0) ->
    do_remove_0(Arr, Dict);
do_remove(Arr, Dict, 1, 0) ->
    {[nil],Arr,Dict};
do_remove(Arr0, Dict, P, Size) when P >= 1, P =< Size+1 ->
    Ret = array:get(P, Arr0),
    Arr1 = remove_array_1(Arr0, P),
    {[Ret],Arr1,Dict};
do_remove(_, _, _, _) -> badarg.

remove_array_1(Arr0, N) ->
    There = array:get(N+1, Arr0),		%Next value
    Arr1 = array:set(N, There, Arr0),
    if There =:= nil -> Arr1;			%End if next a nil
       true -> remove_array_1(Arr1, N+1)
    end.

%% pack - pack arguments in to a table.

pack(As, St0) ->
    T = pack_loop(As, 0.0),			%Indexes are floats!
    {Tab,St1} = luerl_emul:alloc_table(T, St0),
    {[Tab],St1}.

pack_loop([E|Es], N) ->
    [{N+1,E}|pack_loop(Es, N+1)];
pack_loop([], N) -> [{<<"n">>,N}].

%% unpack - unpack table into return values.

unpack([#tref{i=N}=T|As], St) ->
    #table{a=Arr,d=Dict} = ?GET_TABLE(N, St#luerl.ttab),
    case luerl_lib:to_ints(unpack_args(As)) of
	[I] ->
	    Unp = do_unpack(Arr, Dict, I, length_loop(Arr)),
	    %% io:fwrite("unp: ~p\n", [{Arr,I,Start,Unp}]),
	    {Unp,St};
	[I,J] ->
	    Unp = do_unpack(Arr, Dict, I, J),
	    %% io:fwrite("unp: ~p\n", [{Arr,I,J,Start,Unp}]),
	    {Unp,St};
	nil -> badarg_error(unpack, [T|As], St)	%Not numbers
    end;
unpack([], St) -> badarg_error(unpack, [], St).

%% unpack_args(Args) -> Args.
%% Fix args for unpack getting defaults right and handling 'nil'.

unpack_args([]) -> unpack_args([1.0]);		%Just start from the beginning
unpack_args([nil|As]) -> unpack_args([1.0|As]);
unpack_args([I]) -> [I];			%Only one argument
unpack_args([I,nil|_]) -> [I];			%Goto the default end
unpack_args([I,J|_]) -> [I,J].			%Only use two arguments

%% This and concat_table are very similar.
%% First scan over table up to 0 then the array. We have the indexes
%% and limits as integers and explicitly use '==' to compare with
%% float values in table.

do_unpack(Arr, Dict, I, J) -> unpack_tab(Arr, Dict, I, J).

unpack_tab(_, _, N, J) when N > J -> [];	%Done
unpack_tab(Arr, _, N, J) when N > 0 ->		%Done with table
    unpack_arr(Arr, N, J);
unpack_tab(Arr, Dict, N, J) ->
    E = case ttdict:find(N, Dict) of
	    {ok,V} -> V;
	    error -> nil
	end,
    [E|unpack_tab(Arr, Dict, N+1, J)].

unpack_arr(_, N, J) when N > J -> [];
unpack_arr(Arr, N, J) ->
    [array:get(N, Arr)|unpack_arr(Arr, N+1, J)].

%% raw_length(Table, State) -> Length.
%% length(Table, State) -> {Length,State}.
%%  The length of a table is the number of numeric keys in sequence
%%  from 1. Except if 1 is nil followed by non-nil. Don't ask!

length(#tref{}=T, St0) ->
    Meta = luerl_emul:getmetamethod(T, <<"__len">>, St0),
    if ?IS_TRUE(Meta) ->
	    {Ret,St1} = luerl_emul:functioncall(Meta, [T], St0),
	    {luerl_lib:first_value(Ret),St1};
       true ->
	    {raw_length(T, St0),St0}
    end.

raw_length(#tref{i=N}, St) ->
    #table{a=Arr} = ?GET_TABLE(N, St#luerl.ttab),
    float(length_loop(Arr)).

length_loop(Arr) ->
    case {array:get(1, Arr),array:get(2, Arr)} of
	{nil,nil} -> 0;
	{nil,_} -> length_loop(3, Arr);
	{_,nil} -> 1;
	{_,_} -> length_loop(3, Arr)
    end.

length_loop(I, Arr) ->
    case array:get(I, Arr) of
	nil -> I-1;
	_ -> length_loop(I+1, Arr)
    end.

%% sort(Table [,SortFun])
%%  Sort the elements of the list after their values.

sort([#tref{i=N}], St0) ->
    Comp = fun (A, B, St) -> lt_comp(A, B, St) end,
    St1 = do_sort(Comp, St0, N),
    {[],St1};
sort([#tref{i=N},Func|_], St0) ->
    Comp = fun (A, B, St) ->
		   luerl_emul:functioncall(Func, [A,B], St)
	   end,
    St1 = do_sort(Comp, St0, N),
    {[],St1};
sort(As, St) -> badarg_error(sort, As, St).

do_sort(Comp, St0, N) ->
    #table{a=Arr0}=T = ?GET_TABLE(N, St0#luerl.ttab),
    case array:to_list(Arr0) of
	[] -> St0;				%Nothing to do
	[E0|Es0] ->
	    %% 1st element index 0, skip it and then prepend it again
	    {Es1,St1} = merge_sort(Comp, St0, Es0),
	    Arr2 = array:from_list([E0|Es1], nil),
	    %% io:fwrite("so: ~p\n", [{Arr0,Arr1,Arr2}]),
	    Ts0 = St1#luerl.ttab,
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr2}, Ts0),
	    St1#luerl{ttab=Ts1}
    end.

%% lt_comp(O1, O2, State) -> {[Bool],State}.
%%  Proper Lua '<' comparison.

lt_comp(O1, O2, St) when is_number(O1), is_number(O2) -> {[O1 =< O2],St};
lt_comp(O1, O2, St) when is_binary(O1), is_binary(O2) -> {[O1 =< O2],St};
lt_comp(O1, O2, St0) ->
    case luerl_emul:getmetamethod(O1, O2, <<"__lt">>, St0) of
	nil -> lua_error({illegal_comp,sort}, St0);
	Meta ->
	    {Ret,St1} = luerl_emul:functioncall(Meta, [O1,O2], St0),
	    {[luerl_lib:boolean_value(Ret)],St1}
    end.

%% sort(A,B,C) -> sort_up(A,B,C).

%% sort_up(A,B,[X,Y|L]) ->
%%     case X =< Y of
%% 	true -> merge_dn([Y,X], sort_dn(A, B, L), []);
%% 	false -> merge_dn([X,Y], sort_dn(A, B, L), [])
%%     end;
%% sort_up(A,B,[X]) -> [X];
%% sort_up(A,B,[]) -> [].

%% sort_dn(A,B,[X,Y|L]) ->
%%     case X =< Y of
%% 	true -> merge_up([X,Y], sort_up(A, B, L), []);
%% 	false ->  merge_up([Y,X], sort_up(A, B, L), [])
%%     end;
%% sort_dn(A,B,[X]) -> [X];
%% sort_dn(A,B,[]) -> [].

%% merge(A,B,C) ->
%%     merge_dn(A,B,C).

%% %% merge_up(L1, L2, Acc)
%% %%  L1, L2 increasing, Acc will be decreasing

%% merge_up([X|Xs]=Xs0, [Y|Ys]=Ys0, Acc) ->
%%     case X =< Y of
%% 	true -> merge_up(Xs, Ys0, [X|Acc]);
%% 	false -> merge_up(Xs0, Ys, [Y|Acc])
%%     end;
%% merge_up([X|Xs], [], Acc) -> merge_up(Xs, [], [X|Acc]);
%% merge_up([], [Y|Ys], Acc) -> merge_up([], Ys, [Y|Acc]);
%% merge_up([], [], Acc) -> Acc.

%% %% merge_dn(L1, L2, Acc)
%% %%  L1, L2 decreasing, Acc will be increasing

%% merge_dn([X|Xs]=Xs0, [Y|Ys]=Ys0, Acc) ->
%%     case X =< Y of
%% 	true -> merge_dn(Xs0, Ys, [Y|Acc]);
%% 	false -> merge_dn(Xs, Ys0, [X|Acc])
%%     end;
%% merge_dn([X|Xs], [], Acc) -> merge_dn(Xs, [], [X|Acc]);
%% merge_dn([], [Y|Ys], Acc) -> merge_dn([], Ys, [Y|Acc]);
%% merge_dn([], [], Acc) -> Acc.

%% merge_sort(CompFun, State, List) -> {SortedList,State}.
%%  The code here has been taken from the sort/2 code in lists.erl and
%%  converted to chain State through all calls to the comparison
%%  function.

merge_sort(_, St, []) -> {[],St};
merge_sort(_, St, [_] = L) -> {L,St};
merge_sort(Fun, St0, [X, Y|T]) ->
    {Ret,St1} = Fun(X, Y, St0),
    case luerl_lib:boolean_value(Ret) of
	true ->
	    fsplit_1(Y, X, Fun, St1, T, [], []);
	false ->
	    fsplit_2(Y, X, Fun, St1, T, [], [])
    end.

%% Ascending.
fsplit_1(Y, X, Fun, St0, [Z|L], R, Rs) ->
    {Ret1,St1} = Fun(Y, Z, St0),
    case luerl_lib:boolean_value(Ret1) of
        true ->
            fsplit_1(Z, Y, Fun, St1, L, [X|R], Rs);
        false ->
	    {Ret2,St2} = Fun(X, Z, St1),
            case luerl_lib:boolean_value(Ret2) of
                true ->
                    fsplit_1(Y, Z, Fun, St2, L, [X|R], Rs);
                false when R == [] ->
                    fsplit_1(Y, X, Fun, St2, L, [Z], Rs);
                false ->
                    fsplit_1_1(Y, X, Fun, St2, L, R, Rs, Z)
            end
    end;
fsplit_1(Y, X, Fun, St, [], R, Rs) ->
    rfmergel([[Y, X|R]|Rs], [], Fun, St, asc).

fsplit_1_1(Y, X, Fun, St0, [Z|L], R, Rs, S) ->
    {Ret1,St1} = Fun(Y, Z, St0),
    case luerl_lib:boolean_value(Ret1) of
        true ->
            fsplit_1_1(Z, Y, Fun, St1, L, [X|R], Rs, S);
        false ->
	    {Ret2,St2} = Fun(X, Z, St1),
            case luerl_lib:boolean_value(Ret2) of
                true ->
                    fsplit_1_1(Y, Z, Fun, St2, L, [X|R], Rs, S);
                false ->
		    {Ret3,St3} = Fun(S, Z, St2),
                    case luerl_lib:boolean_value(Ret3) of
                        true ->
                            fsplit_1(Z, S, Fun, St3, L, [], [[Y, X|R]|Rs]);
                        false ->
                            fsplit_1(S, Z, Fun, St3, L, [], [[Y, X|R]|Rs])
                    end
            end
    end;
fsplit_1_1(Y, X, Fun, St, [], R, Rs, S) ->
    rfmergel([[S], [Y, X|R]|Rs], [], Fun, St, asc).

%% Descending.
fsplit_2(Y, X, Fun, St0, [Z|L], R, Rs) ->
    {Ret1,St1} = Fun(Y, Z, St0),
    case luerl_lib:boolean_value(Ret1) of
        false ->
            fsplit_2(Z, Y, Fun, St1, L, [X|R], Rs);
        true ->
	    {Ret2,St2} = Fun(X, Z, St1),
            case luerl_lib:boolean_value(Ret2) of
                false ->
                    fsplit_2(Y, Z, Fun, St2, L, [X|R], Rs);
                true when R == [] ->
                    fsplit_2(Y, X, Fun, St2, L, [Z], Rs);
                true ->
                    fsplit_2_1(Y, X, Fun, St2, L, R, Rs, Z)
            end
    end;
fsplit_2(Y, X, Fun, St, [], R, Rs) ->
    fmergel([[Y, X|R]|Rs], [], Fun, St, desc).

fsplit_2_1(Y, X, Fun, St0, [Z|L], R, Rs, S) ->
    {Ret1,St1} = Fun(Y, Z, St0),
    case luerl_lib:boolean_value(Ret1) of
        false ->
            fsplit_2_1(Z, Y, Fun, St1, L, [X|R], Rs, S);
        true ->
	    {Ret2,St2} = Fun(X, Z, St1),
            case luerl_lib:boolean_value(Ret2) of
                false ->
                    fsplit_2_1(Y, Z, Fun, St2, L, [X|R], Rs, S);
                true ->
		    {Ret3,St3} = Fun(S, Z, St2),
                    case luerl_lib:boolean_value(Ret3) of
                        false ->
                            fsplit_2(Z, S, Fun, St3, L, [], [[Y, X|R]|Rs]);
                        true ->
                            fsplit_2(S, Z, Fun, St3, L, [], [[Y, X|R]|Rs])
                    end
            end
    end;
fsplit_2_1(Y, X, Fun, St, [], R, Rs, S) ->
    fmergel([[S], [Y, X|R]|Rs], [], Fun, St, desc).

fmergel([T1, [H2|T2]|L], Acc, Fun, St0, asc) ->
    {L1,St1} = fmerge2_1(T1, H2, Fun, St0, T2, []),
    fmergel(L, [L1|Acc], Fun, St1, asc);
fmergel([[H2|T2], T1|L], Acc, Fun, St0, desc) ->
    {L1,St1} = fmerge2_1(T1, H2, Fun, St0, T2, []),
    fmergel(L, [L1|Acc], Fun, St1, desc);
fmergel([L], [], _Fun, St, _O) -> {L,St};
fmergel([L], Acc, Fun, St, O) ->
    rfmergel([lists:reverse(L, [])|Acc], [], Fun, St, O);
fmergel([], Acc, Fun, St, O) ->
    rfmergel(Acc, [], Fun, St, O).

rfmergel([[H2|T2], T1|L], Acc, Fun, St0, asc) ->
    {L1,St1} = rfmerge2_1(T1, H2, Fun, St0, T2, []),
    rfmergel(L, [L1|Acc], Fun, St1, asc);
rfmergel([T1, [H2|T2]|L], Acc, Fun, St0, desc) ->
    {L1,St1} = rfmerge2_1(T1, H2, Fun, St0, T2, []),
    rfmergel(L, [L1|Acc], Fun, St1, desc);
rfmergel([L], Acc, Fun, St, O) ->
    fmergel([lists:reverse(L, [])|Acc], [], Fun, St, O);
rfmergel([], Acc, Fun, St, O) ->
    fmergel(Acc, [], Fun, St, O).

%% merge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
%%     lists:reverse(fmerge2_1(T1, H2, Fun, T2, []), []);
%% merge(Fun, T1, []) when is_function(Fun, 2) ->
%%     T1.

%% Elements from the first list are prioritized.
fmerge2_1([H1|T1], H2, Fun, St0, T2, M) ->
    {Ret,St1} = Fun(H1, H2, St0),
    case luerl_lib:boolean_value(Ret) of
        true ->
            fmerge2_1(T1, H2, Fun, St1, T2, [H1|M]);
        false ->
            fmerge2_2(H1, T1, Fun, St1, T2, [H2|M])
    end;
fmerge2_1([], H2, _Fun, St, T2, M) ->
    {lists:reverse(T2, [H2|M]),St}.

fmerge2_2(H1, T1, Fun, St0, [H2|T2], M) ->
    {Ret,St1} = Fun(H1, H2, St0),
    case luerl_lib:boolean_value(Ret) of
        true ->
            fmerge2_1(T1, H2, Fun, St1, T2, [H1|M]);
        false ->
            fmerge2_2(H1, T1, Fun, St1, T2, [H2|M])
    end;
fmerge2_2(H1, T1, _Fun, St, [], M) ->
    {lists:reverse(T1, [H1|M]),St}.

%% rmerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
%%     lists:reverse(rfmerge2_1(T1, H2, Fun, T2, []), []);
%% rmerge(Fun, T1, []) when is_function(Fun, 2) ->
%%     T1.

rfmerge2_1([H1|T1], H2, Fun, St0, T2, M) ->
    {Ret,St1} = Fun(H1, H2, St0),
    case luerl_lib:boolean_value(Ret) of
        true ->
            rfmerge2_2(H1, T1, Fun, St1, T2, [H2|M]);
        false ->
            rfmerge2_1(T1, H2, Fun, St1, T2, [H1|M])
    end;
rfmerge2_1([], H2, _Fun, St, T2, M) ->
    {lists:reverse(T2, [H2|M]),St}.

rfmerge2_2(H1, T1, Fun, St0, [H2|T2], M) ->
    {Ret,St1} = Fun(H1, H2, St0),
    case luerl_lib:boolean_value(Ret) of
        true ->
            rfmerge2_2(H1, T1, Fun, St1, T2, [H2|M]);
        false ->
            rfmerge2_1(T1, H2, Fun, St1, T2, [H1|M])
    end;
rfmerge2_2(H1, T1, _Fun, St, [], M) ->
    {lists:reverse(T1, [H1|M]),St}.
