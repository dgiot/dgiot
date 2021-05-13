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

%% File    : ttdict.erl
%% Author  : Robert Virding
%% Purpose : Key-Value dictionary as a 2-3 tree.

%% This implementation uses 2-3 trees. The description of the tree
%% restructuring which is used comes from Prof. Lyn Turbak's notes for
%% CS230 Data Structures at Wellesley College.

-module(ttdict).

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3]).
-export([update_val/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

%% Extended interface.
-export([foreach/2,mapfold/3]).

%% Special interface.
-export([first/1,last/1,next/2,prev/2]).

%% Deprecated interface.
-export([dict_to_list/1,list_to_dict/1]).
-deprecated([{dict_to_list,1},{list_to_dict,1}]).

-compile({no_auto_import,[size/1]}).		%We mean our own size/1

-ifdef(DEBUG).
-export([check_depth/1]).
-endif.

%% Data structure:
%% - {Left,Key,Val,Right}
%% - {Left,Key,Val,Middle,Key,Val,Right}
%% - empty
%%
%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.
%%
%% In all the functions we test keys from left to right in the
%% structure. This might not always be the best choice (is there any
%% best choice?) but it makes the code consistent.

-type ttdict() :: empty |
		  {empty,any(),any(),empty} |
		  {any(),any(),any(),any()} |
		  {empty,any(),any(),empty,any(),any(),empty} |
		  {any(),any(),any(),any(),any(),any(),any()}.

-export_type([ttdict/0]).

-spec new() -> Dict when
      Dict :: ttdict().

new() -> empty.					%The empty dict

-spec is_key(Key, Dict) -> boolean() when
      Key :: term(),
      Dict :: ttdict().

is_key(_, empty) -> false;
is_key(Key, {L,Xk,_,_}) when Key < Xk -> is_key(Key, L);
is_key(Key, {_,Xk,_,R}) when Key > Xk -> is_key(Key, R);
is_key(_, {_,_,_,_}) -> true;			%Key == Xk
is_key(Key, {L,Xk,_,_,_,_,_}) when Key < Xk ->
    is_key(Key, L);
is_key(Key, {_,Xk,_,M,Yk,_,R}) when Key > Xk ->
    if Key < Yk -> is_key(Key, M);
       Key > Yk -> is_key(Key, R);
       true -> true				%Key == Yk
    end;
is_key(_, {_,_,_,_,_,_,_}) -> true.		%Key == Xk

-spec to_list(Dict) -> List when
      Dict :: ttdict(),
      List :: [{Key :: term(),Value :: term()}].

to_list(D) -> to_list(D, []).

to_list(empty, Tail) -> Tail;
to_list({L,Xk,Xv,R}, Tail) ->
    to_list(L, [{Xk,Xv}|to_list(R, Tail)]);
to_list({L,Xk,Xv,M,Yk,Yv,R}, Tail) ->
    to_list(L, [{Xk,Xv}|to_list(M, [{Yk,Yv}|to_list(R, Tail)])]).

-spec from_list(List) -> Dict when
      List :: [{Key :: term(),Value :: term()}],
      Dict :: ttdict().

from_list(List) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, new(), List).

-spec size(Dict) -> non_neg_integer() when
      Dict :: ttdict().

size(empty) -> 0;
size({L,_,_,R}) ->
    size(L) + size(R) + 1;
size({L,_,_,M,_,_,R}) ->
    size(L) + size(M) + size(R) + 2.

-spec fetch(Key, Dict) -> Value when
      Key :: term(),
      Dict :: ttdict(),
      Value :: term().

fetch(K, {L,Xk,_,_}) when K < Xk -> fetch(K, L);
fetch(K, {_,Xk,_,R}) when K > Xk -> fetch(K, R);
fetch(_, {_,_,Xv,_}) -> Xv;
fetch(K, {L,Xk,_,_,_,_,_}) when K < Xk -> fetch(K, L);
fetch(K, {_,Xk,_,M,Yk,Yv,R}) when K > Xk ->
    if K < Yk -> fetch(K, M);			%Middle
       K > Yk -> fetch(K, R);			%Right
       true -> Yv
    end;
fetch(_, {_,_,Xv,_,_,_,_}) -> Xv;
fetch(_, empty) -> error(badarg).

-spec find(Key, Dict) -> {ok,Value} | error when
      Key :: term(),
      Dict :: ttdict(),
      Value :: term().

find(K, {L,Xk,_,_}) when K < Xk -> find(K, L);
find(K, {_,Xk,_,B}) when K > Xk -> find(K, B);
find(_, {_,_,Xv,_}) -> {ok,Xv};
find(K, {L,Xk,_,_,_,_,_}) when K < Xk -> find(K, L);
find(K, {_,Xk,_,M,Yk,Yv,R}) when K > Xk ->
    if K < Yk -> find(K, M);			%Middle
       K > Yk -> find(K, R);			%Right
       true -> {ok,Yv}
    end;
find(_, {_,_,Xv,_,_,_,_}) -> {ok,Xv};
find(_, empty) -> error.

-spec fetch_keys(Dict) -> Keys when
      Dict :: ttdict(),
      Keys :: [term()].

fetch_keys(D) -> fetch_keys(D, []).

fetch_keys(empty, Tail) -> Tail;
fetch_keys({L,Xk,_,R}, Tail) ->
    fetch_keys(L, [Xk|fetch_keys(R, Tail)]);
fetch_keys({L,Xk,_,M,Yk,_,R}, Tail) ->
    fetch_keys(L, [Xk|fetch_keys(M, [Yk|fetch_keys(R, Tail)])]).

-spec store(Key, Value, Dict1) -> Dict2 when
      Key :: term(),
      Value :: term(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

store(Key, Val, T) ->
    %% Store and check for a returned "Up" node.
    case store_aux(Key, Val, T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

store_aux(Key, Val, empty) -> {up,empty,Key,Val,empty};	%"Up" node
store_aux(Key, Val, {empty,K,V,empty}) ->
    %% Special case to avoid creating temporary "up" nodes.
    %% It helps a little bit, but not much.
    if Key < K ->
	    {empty,Key,Val,empty,K,V,empty};
       Key > K ->
	    {empty,K,V,empty,Key,Val,empty};
       true -> {empty,K,Val,empty}
    end;
store_aux(Key, Val, {L,K,V,R}) ->
    if Key < K ->				%Down the left
	    store_up2_l(store_aux(Key, Val, L), K, V, R);
       Key > K ->				%Down the right
	    store_up2_r(L, K, V, store_aux(Key, Val, R));
       true -> {L,K,Val,R}			%Replace current value
    end;
store_aux(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    store_up3_l(store_aux(Key, Val, L), Xk, Xv, M, Yk, Yv, R);
store_aux(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->				%Down the middle
	    store_up3_m(L, Xk, Xv, store_aux(Key, Val, M), Yk, Yv, R);
       Key > Yk ->				%Down the right
	    store_up3_r(L, Xk, Xv, M, Yk, Yv, store_aux(Key, Val, R));
       true -> {L,Xk,Xv,M,Yk,Val,R}
    end;
store_aux(_, Val, {L,Xk,_,M,Yk,Yv,R}) ->	%Key == Xk
    {L,Xk,Val,M,Yk,Yv,R}.

-spec append(Key, Value, Dict1) -> Dict2 when
      Key :: term(),
      Value :: term(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

append(Key, Val, T) ->
    %% Append and check for a returned "Up" node.
    case append_aux(Key, [Val], T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

-spec append_list(Key, Values, Dict1) -> Dict2 when
      Key :: term(),
      Values :: [Value :: term()],
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

append_list(Key, Val, T) ->
    %% Append and check for a returned "Up" node.
    case append_aux(Key, Val, T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

append_aux(Key, Val, empty) -> {up,empty,Key,Val,empty};	%"Up" node
append_aux(Key, Val, {L,K,V,R}) ->
    if Key < K ->				%Down the left
	    store_up2_l(append_aux(Key, Val, L), K, V, R);
       Key > K ->				%Down the right
	    store_up2_r(L, K, V, append_aux(Key, Val, R));
       true -> {L,Key,V ++ Val,R}		%Append to current value
    end;
append_aux(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    store_up3_l(append_aux(Key, Val, L), Xk, Xv, M, Yk, Yv, R);
append_aux(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    store_up3_m(L, Xk, Xv, append_aux(Key, Val, M), Yk, Yv, R);
       Key > Yk ->
	    store_up3_r(L, Xk, Xv, M, Yk, Yv, append_aux(Key, Val, R));
       true -> {L,Xk,Xv,M,Key,Yv ++ Val,R}
    end;
append_aux(Key, Val, {L,_,Xv,M,Yk,Yv,R}) ->
    {L,Key,Xv ++ Val,M,Yk,Yv,R}.

-spec update_val(Key, Value, Dict1) -> Dict2 when
      Key :: term(),
      Value :: term(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

update_val(Key, Val, {L,Xk,Xv,R}) ->
    if Key < Xk ->
	    {update_val(Key, Val, L),Xk,Xv,R};
       Key > Xk ->
	    {L,Xk,Xv,update_val(Key, Val, R)};
       true -> {L,Xk,Val,R}
    end;
update_val(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    {update_val(Key, Val, L),Xk,Xv,M,Yk,Yv,R};
update_val(Key, Val, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    {L,Xk,Xv,update_val(Key, Val, M),Yk,Yv,R};
       Key > Yk ->
	    {L,Xk,Xv,M,Yk,Yv,update_val(Key, Val, R)};
       true ->
	    {L,Xk,Xv,M,Yk,Val,R}
    end;
update_val(_, Val, {L,Xk,_,M,Yk,Yv,R}) ->	%Key == Xk
    {L,Xk,Val,M,Yk,Yv,R}.

-spec update(Key, Fun, Dict1) -> Dict2 when
      Key :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

update(Key, Fun, {L,Xk,Xv,R}) ->
    if Key < Xk ->
	    {update(Key, Fun, L),Xk,Xv,R};
       Key > Xk ->
	    {L,Xk,Xv,update(Key, Fun, R)};
       true -> {L,Xk,Fun(Xv),R}			%Key == Xk
    end;
update(Key, Fun, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    {update(Key, Fun, L),Xk,Xv,M,Yk,Yv,R};
update(Key, Fun, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    {L,Xk,Xv,update(Key, Fun, M),Yk,Yv,R};
       Key > Yk ->
	    {L,Xk,Xv,M,Yk,Yv,update(Key, Fun, R)};
       true ->
	    {L,Xk,Xv,M,Yk,Fun(Yv),R}
    end;
update(_, Fun, {L,Xk,Xv,M,Yk,Yv,R}) ->		%Key == Xk
    {L,Xk,Fun(Xv),M,Yk,Yv,R}.

-spec update(Key, Fun, Initial, Dict1) -> Dict2 when
      Key :: term,
      Initial :: term(),
      Fun :: fun((Value :: term()) -> Value2 :: term()),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

update(Key, Fun, I, T) ->
    case update_aux(Key, Fun, I, T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

update_aux(Key, _, I, empty) -> {up,empty,Key,I,empty};
update_aux(Key, Fun, I, {L,Xk,Xv,R}) ->
    if Key < Xk ->
	    store_up2_l(update_aux(Key, Fun, I, L), Xk, Xv, R);
       Key > Xk ->
	    store_up2_r(L, Xk, Xv, update_aux(Key, Fun, I, R));
       true -> {L,Xk,Fun(Xv),R}
    end;
update_aux(Key, Fun, I, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    store_up3_l(update_aux(Key, Fun, I, L), Xk, Xv, M, Yk, Yv, R);
update_aux(Key, Fun, I, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    store_up3_m(L, Xk, Xv, update_aux(Key, Fun, I, M), Yk, Yv, R);
       Key > Yk ->
	    store_up3_r(L, Xk, Xv, M, Yk, Yv, update_aux(Key, Fun, I, R));
       true ->
	    {L,Xk,Xv,M,Key,Fun(Yv),R}
    end;
update_aux(_, Fun, _, {L,Xk,Xv,M,Yk,Yv,R}) ->	%Key == Xk
    {L,Xk,Fun(Xv),M,Yk,Yv,R}.

-spec update_counter(Key, Increment, Dict1) -> Dict2 when
      Key :: term(),
      Increment :: number(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

update_counter(Key, I, T) ->
    case update_counter_aux(Key, I, T) of
	{up,Lu,Ku,Vu,Ru} -> {Lu,Ku,Vu,Ru};
	Node -> Node
    end.

update_counter_aux(Key, I, empty) -> {up,empty,Key,I,empty};
update_counter_aux(Key, I, {L,Xk,Xv,R}) ->
    if Key < Xk ->
	    store_up2_l(update_counter_aux(Key, I, L), Xk, Xv, R);
       Key > Xk ->
	    store_up2_r(L, Xk, Xv, update_counter_aux(Key, I, R));
       true -> {L,Xk,Xv+I,R}
    end;
update_counter_aux(Key, I, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    store_up3_l(update_counter_aux(Key, I, L), Xk, Xv, M, Yk, Yv, R);
update_counter_aux(Key, I, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    store_up3_m(L, Xk, Xv, update_counter_aux(Key, I, M), Yk, Yv, R);
       Key > Yk ->
	    store_up3_r(L, Xk, Xv, M, Yk, Yv, update_counter_aux(Key, I, R));
       true ->
	    {L,Xk,Xv,M,Yk,Yv+I,R}
    end;
update_counter_aux(_, I, {L,Xk,Xv,B,Yk,Yv,R}) ->	%Key == Xk
    {L,Xk,Xv+I,B,Yk,Yv,R}.

%% store_up2_l/r(L, K, V, R) -> {L,Xk,Xv,M,Yk,Yv,R} | {L,K,V,R}.

store_up2_l({up,Lu,Ku,Vu,Ru}, K, V, R) ->
    {Lu,Ku,Vu,Ru,K,V,R};
store_up2_l(L, K, V, R) -> {L,K,V,R}.

store_up2_r(L, K, V, {up,Lu,Ku,Vu,Ru}) ->
    {L,K,V,Lu,Ku,Vu,Ru};
store_up2_r(L, K, V, R) -> {L,K,V,R}.

%% store_up3_l/m/r(L, Xk, Xv, M, Yk, Yv, R) ->
%%     {up,L,K,V,R} | {L,Xk,Xv,M,Yk,Yv,R}.

store_up3_l({up,Lu,Ku,Vu,Ru}, Xk, Xv, M, Yk, Yv, R) ->
    {up,{Lu,Ku,Vu,Ru},Xk,Xv,{M,Yk,Yv,R}};
store_up3_l(L, Xk, Xv, M, Yk, Yv, R) ->
    {L,Xk,Xv,M,Yk,Yv,R}.

store_up3_m(L, Xk, Xv, {up,Lu,Ku,Vu,Ru}, Yk, Yv, R) ->
    {up,{L,Xk,Xv,Lu},Ku,Vu,{Ru,Yk,Yv,R}};
store_up3_m(L, Xk, Xv, M, Yk, Yv, R) ->
    {L,Xk,Xv,M,Yk,Yv,R}.

store_up3_r(L, Xk, Xv, M, Yk, Yv, {up,Lu,Ku,Vu,Ru}) ->
    {up,{L,Xk,Xv,M},Yk,Yv,{Lu,Ku,Vu,Ru}};
store_up3_r(L, Xk, Xv, M, Yk, Yv, R) ->
    {L,Xk,Xv,M,Yk,Yv,R}.

-spec erase(Key, Dict1) -> Dict2 when
      Key :: term(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().

erase(Key, T) ->
    case erase_aux(Key, T) of
	{up,T1} -> T1;				%???
	T1 -> T1
    end.

erase_aux(_, empty) -> empty;			%No element
erase_aux(Key, {empty,Xk,_,empty}=N) ->
    if Key < Xk; Key > Xk -> N;			%No element
       true -> {up,empty}
    end;
erase_aux(Key, {L,Xk,Xv,R}) ->
    if Key < Xk ->				%Down the left
	    erase_up2_l(erase_aux(Key, L), Xk, Xv, R);
       Key > Xk ->				%Down the right
	    erase_up2_r(L, Xk, Xv, erase_aux(Key, R));
       true ->
	    {{Km,Vm},R1}= erase_min(R),
	    erase_up2_r(L, Km, Vm, R1)
    end;
erase_aux(Key, {empty,Xk,Xv,empty,Yk,Yv,empty}=N) ->
    if Key < Xk -> N;				%No element
       Key > Xk ->
	    if Key < Yk -> N;			%No element
	       Key > Yk -> N;
	       true -> {empty,Xk,Xv,empty}
	    end;
       true -> {empty,Yk,Yv,empty}
    end;
erase_aux(Key, {L,Xk,Xv,M,Yk,Yv,R}) when Key < Xk ->
    erase_up3_l(erase_aux(Key, L), Xk, Xv, M, Yk, Yv, R);
erase_aux(Key, {L,Xk,Xv,M,Yk,Yv,R}) when Key > Xk ->
    if Key < Yk ->
	    erase_up3_m(L, Xk, Xv, erase_aux(Key, M), Yk, Yv, R);
       Key > Yk ->
	    erase_up3_r(L, Xk, Xv, M, Yk, Yv, erase_aux(Key, R));
       true ->
	    {{Km,Vm},R1} = erase_min(R),
	    erase_up3_r(L, Xk, Xv, M, Km, Vm, R1)
    end;
erase_aux(_, {L,_,_,M,Yk,Yv,R}) ->
    {{Km,Vm},M1} = erase_min(M),
    erase_up3_m(L, Km, Vm, M1, Yk, Yv, R).

erase_min(T) ->
    %%io:format("em: ~p\n->  ~p\n", [T,T1]),
    erase_min1(T).

erase_min1({empty,Xk,Xv,empty}) -> {{Xk,Xv},{up,empty}};
erase_min1({L,Xk,Xv,R}) ->
    {Min,L1} = erase_min1(L),
    {Min,erase_up2_l(L1, Xk, Xv, R)};
erase_min1({empty,Xk,Xv,empty,Yk,Yv,empty}) ->
    {{Xk,Xv},{empty,Yk,Yv,empty}};
erase_min1({L,Xk,Xv,M,Yk,Yv,R}) ->
    {Min,L1} = erase_min1(L),
    {Min,erase_up3_l(L1, Xk, Xv, M, Yk, Yv, R)}.

%% erase_up2_l/r(L, K, V, R) -> Node | {up,Node}.
%% We use the same naming of nodes and keys as in the text. It makes
%% checking the rules easier.

erase_up2_l({up,L}, Xk, Xv, {M,Yk,Yv,R}) ->	%1.1
    {up,{L,Xk,Xv,M,Yk,Yv,R}};
erase_up2_l({up,A}, Xk, Xv, {B,Yk,Yv,C,Zk,Zv,D}) -> %2.1
    {{A,Xk,Xv,B},Yk,Yv,{C,Zk,Zv,D}};
erase_up2_l(L, K, V, R) -> {L,K,V,R}.

erase_up2_r({L,Xk,Xv,M}, Yk, Yv, {up,R}) ->	%1.2
    {up,{L,Xk,Xv,M,Yk,Yv,R}};
erase_up2_r({A,Xk,Xv,B,Yk,Yv,C}, Zk, Zv, {up,D}) -> %2.2
    {{A,Xk,Xv,B},Yk,Yv,{C,Zk,Zv,D}};
erase_up2_r(L, K, V, R) -> {L,K,V,R}.

%% erase_up2_r(L, K, V, {up,R}) -> erase_up2_r1(L, K, V, R);
%% erase_up2_r(L, K, V, R) -> {L,K,V,R}.

%% erase_up2_r1({L,Xk,Xv,M}, Yk, Yv, R) ->		%1.2
%%     {up,{L,Xk,Xv,M,Yk,Yv,R}};
%% erase_up2_r1({A,Xk,Xv,B,Yk,Yv,C}, Zk, Zv, D) -> %2.2
%%     {{A,Xk,Xv,B},Yk,Yv,{C,Zk,Zv,D}}.

%% erase_up3_l/m/r(L, Xk, Xv, M, Yk, Yv, R) -> Node | {up,Node}.
%%  We use the same naming of nodes and keys as in the text. It makes
%%  checking the rules easier. N.B. there are alternate valid choices
%%  for the middle case!

erase_up3_l({up,A}, Xk, Xv, {B,Yk,Yv,C}, Zk, Zv, D) -> %3a.1
    {{A,Xk,Xv,B,Yk,Yv,C},Zk,Zv,D};
erase_up3_l({up,A}, Wk, Wv, {B,Xk,Xv,C,Yk,Yv,D}, Zk, Zv, E) -> %4a.1
    {{A,Wk,Wv,B},Xk,Xv,{C,Yk,Yv,D},Zk,Zv,E};
erase_up3_l(A, Xk, Xv, B, Yk, Yv, C) ->
    {A,Xk,Xv,B,Yk,Yv,C}.

erase_up3_m({A,Xk,Xv,B}, Yk, Yv, {up,C}, Zk, Zv, D) -> %3a.2
    {{A,Xk,Xv,B,Yk,Yv,C},Zk,Zv,D};
erase_up3_m(A, Xk, Xv, {up,B}, Yk, Yv, {C,Zk,Zv,D}) -> %3b.1
    {A,Xk,Xv,{B,Yk,Yv,C,Zk,Zv,D}};
erase_up3_m({A,Wk,Wv,B,Xk,Xv,C}, Yk, Yv, {up,D}, Zk, Zv, E) -> %4a.2
    {{A,Wk,Wv,B},Xk,Xv,{C,Yk,Yv,D},Zk,Zv,E};
erase_up3_m(A, Wk, Wv, {up,B}, Xk, Xv, {C,Yk,Yv,D,Zk,Zv,E}) -> %4b.1
    {A,Wk,Wv,{B,Xk,Xv,C},Yk,Yv,{D,Zk,Zv,E}};
erase_up3_m(A, Xk, Xv, B, Yk, Yv, C) ->
    {A,Xk,Xv,B,Yk,Yv,C}.

erase_up3_r(A, Xk, Xv, {B,Yk,Yv,C}, Zk, Zv, {up,D}) -> %3b.2
    {A,Xk,Xv,{B,Yk,Yv,C,Zk,Zv,D}};
erase_up3_r(A, Wk, Wv, {B,Xk,Xv,C,Yk,Yv,D}, Zk, Zv, {up,E}) -> %4b.2
    {A,Wk,Wv,{B,Xk,Xv,C},Yk,Yv,{D,Zk,Zv,E}};
erase_up3_r(A, Xk, Xv, B, Yk, Yv, C) ->
    {A,Xk,Xv,B,Yk,Yv,C}.

-spec fold(Fun, Acc0, Dict) -> Acc1 when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Key :: term(),
      Value :: term(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict :: ttdict().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {L,Xk,Xv,R}) ->
    fold(F, F(Xk, Xv, fold(F, Acc, R)), L);
fold(F, Acc, {L,Xk,Xv,M,Yk,Yv,R}) ->
    fold(F, F(Xk, Xv, fold(F, F(Yk, Yv, fold(F, Acc, R)), M)), L).

-spec map(Fun, Dict1) -> Dict2 when
      Fun :: fun((Key :: term(), Value1 :: term()) -> Value2 :: term()),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

map(_, empty) -> empty;
map(F, {A,Xk,Xv,B}) ->
    {map(F, A),Xk,F(Xk, Xv),map(F, B)};
map(F, {A,Xk,Xv,B,Yk,Yv,C}) ->
    {map(F, A),Xk,F(Xk, Xv),map(F, B),Yk,F(Yk, Yv),map(F, C)}.

-spec filter(Pred, Dict1) -> Dict2 when
      Pred :: fun((Key :: term(), Value :: term()) -> boolean()),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

filter(F, D) -> filter(F, D, new()).

filter(_, empty, New) -> New;
filter(F, {L,Xk,Xv,R}, New0) ->
    New1 = filter(F, L, New0),
    New2 = case F(Xk, Xv) of
	       true -> store(Xk, Xv, New1);
	       false -> New1
	   end,
    filter(F, R, New2);
filter(F, {L,Xk,Xv,M,Yk,Yv,R}, New0) ->
    New1 = filter(F, L, New0),
    New2 = case F(Xk, Xv) of
	       true -> store(Xk, Xv, New1);
	       false -> New1
	   end,
    New3 = filter(F, M, New2),
    New4 = case F(Yk, Yv) of
	       true -> store(Yk, Yv, New3);
	       false -> New3
	   end,
    filter(F, R, New4).

-spec merge(Fun, Dict1, Dict2) -> Dict3 when
      Fun :: fun((Key :: term(), Value1 :: term(), Value2 :: term()) -> Value :: term()),
      Dict1 :: ttdict(),
      Dict2 :: ttdict(),
      Dict3 :: ttdict().

merge(F, D1, D2) ->
    fold(fun (K, V2, D) ->
		 update(K, fun(V1) -> F(K, V1, V2) end, V2, D)
	 end, D1, D2).

%% Extended interface.

-spec foreach(Fun, Dict) -> ok when
      Fun :: fun((Key :: term(), Value :: term()) -> term()),
      Dict :: ttdict().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

foreach(_, empty) -> ok;
foreach(F, {L,Xk,Xv,R}) ->
    foreach(F, L),
    F(Xk, Xv),
    foreach(F, R);
foreach(F, {L,Xk,Xv,M,Yk,Yv,R}) ->
    foreach(F, L),
    F(Xk, Xv),
    foreach(F, M),
    F(Yk, Yv),
    foreach(F, R).

-spec mapfold(Fun, Acc0, Dict1) -> {Dict2,Acc1} when
      Fun :: fun((Key, Value1, AccIn) -> {Value2,AccOut}),
      Acc0 :: term(),
      Acc1 :: term(),
      Key :: term(),
      Value1 :: term(),
      Value2 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Dict1 :: ttdict(),
      Dict2 :: ttdict().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

mapfold(_, Acc, empty) -> {empty,Acc};
mapfold(F, Acc0, {L0,Xk,Xv0,R0}) ->
    {L1,Acc1} = mapfold(F, Acc0, L0),
    {Xv1,Acc2} = F(Xk, Xv0, Acc1),
    {R1,Acc3} = mapfold(F, Acc2, R0),
    {{L1,Xk,Xv1,R1},Acc3};
mapfold(F, Acc0, {L0,Xk,Xv0,M0,Yk,Yv0,R0}) ->
    {L1,Acc1} = mapfold(F, Acc0, L0),
    {Xv1,Acc2} = F(Xk, Xv0, Acc1),
    {M1,Acc3} = mapfold(F, Acc2, M0),
    {Yv1,Acc4} = F(Yk, Yv0, Acc3),
    {R1,Acc5} = mapfold(F, Acc4, R0),
    {{L1,Xk,Xv1,M1,Yk,Yv1,R1},Acc5}.

%% Special interface.

-spec first(Dict) -> error | {ok,{Key1,Value}} when
      Key1 :: term(),
      Value :: term(),
      Dict :: ttdict().

first(empty) -> error;
first({L,Xk,Xv,_}) ->
    case first(L) of
	error -> {ok,{Xk,Xv}};
	First -> First
    end;
first({L,Xk,Xv,_,_,_,_}) ->
    case first(L) of
	error -> {ok,{Xk,Xv}};
	First -> First
    end.

-spec last(Dict) -> error | {ok,{Key1,Value}} when
      Key1 :: term(),
      Value :: term(),
      Dict :: ttdict().

last(empty) -> error;
last({_,Xk,Xv,R}) ->
    case last(R) of
	error -> {ok,{Xk,Xv}};
	Last -> Last
    end;
last({_,_,_,_,Yk,Yv,R}) ->
    case last(R) of
	error -> {ok,{Yk,Yv}};
	Last -> Last
    end.

-spec next(Key, Dict) -> error | {ok,{Key1,Value}} when
      Key :: term(),
      Key1 :: term(),
      Value :: term(),
      Dict :: ttdict().

next(_, empty) -> error;
next(K, {L,Xk,Xv,_}) when K < Xk ->
    case next(K, L) of
	error -> {ok,{Xk,Xv}};
	Next -> Next
    end;
next(K, {_,Xk,_,R}) when K > Xk -> next(K, R);
next(_, {_,_,_,R}) -> first(R);			%when K == Xk
next(K, {L,Xk,Xv,_,_,_,_}) when K < Xk ->
    case next(K, L) of
	error -> {ok,{Xk,Xv}};
	Next -> Next
    end;
next(K, {_,Xk,_,M,Yk,Yv,R}) when K > Xk ->
    if K < Yk ->
	    case next(K, M) of
		error -> {ok,{Yk,Yv}};
		Next -> Next
	    end;
       K > Yk -> next(K, R);
       true -> first(R)				%when K == Yk
    end;
next(_, {_,_,_,M,Yk,Yv,_}) ->			%when K == Xk
    case first(M) of
	error -> {ok,{Yk,Yv}};
	First -> First
    end.

-spec prev(Key, Dict) -> error | {ok,{Key1,Value}} when
      Key :: term(),
      Key1 :: term(),
      Value :: term(),
      Dict :: ttdict().

%%  Go from right to left here as it makes it easier to understand
%%  what is going on.

prev(_, empty) -> error;
prev(K, {_,Xk,Xv,R}) when K > Xk ->
    case prev(K, R) of
	error -> {ok,{Xk,Xv}};
	Prev -> Prev
    end;
prev(K, {L,Xk,_,_}) when K < Xk -> prev(K, L);
prev(_, {L,_,_,_}) -> last(L);			%when K == Xk
prev(K, {_,_,_,_,Yk,Yv,R}) when K > Yk ->
    case prev(K, R) of
	error -> {ok,{Yk,Yv}};
	Prev -> Prev
    end;
prev(K, {L,Xk,Xv,M,Yk,_,_}) when K < Yk ->
    if K > Xk ->
	    case prev(K, M) of
		error -> {ok,{Xk,Xv}};
		Prev -> Prev
	    end;
       K < Xk -> prev(K, L);
       true -> last(L)				%when K == Xk
    end;
prev(_, {_,Xk,Xv,M,_,_,_}) ->			%when K == Yk
    case last(M) of
	error -> {ok,{Xk,Xv}};
	Prev -> Prev
    end.

%% Deprecated interface.

%% dict_to_list(Dictionary) -> [{Key,Value}].

dict_to_list(D) -> to_list(D).

%% list_to_dict([{Key,Value}]) -> Dictionary.

list_to_dict(L) -> from_list(L).

-ifdef(DEBUG).

%% Check the depth of all the leaves, should all be the same.
check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({L,_,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(two, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    check_depth(R, D+1, Dd2);
check_depth({L,_,_,M,_,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(three, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    Dd3 = check_depth(M, D+1, Dd2),
    check_depth(R, D+1, Dd3).

-endif.
