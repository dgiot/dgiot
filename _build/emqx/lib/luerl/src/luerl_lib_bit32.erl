%% Copyright (c) 2014 Łukasz Biedrycki
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

%% File    : luerl_lib_bit32.erl
%% Author  : Łukasz Biedrycki
%% Purpose : The bit32 library for Luerl.

-module(luerl_lib_bit32).

-export([install/1]).

-import(luerl_lib, [badarg_error/3]).	%Shorten this

-define(MOST_SIGNIFICANT, 16#80000000).
-define(LEAST_SIGNIFICANT, 16#00000001).
-define(DEFAULT_BAND, 4294967295).
-define(DEFAULT_BOR, 0).
-define(DEFAULT_BXOR, 0).

install(St) ->
    luerl_emul:alloc_table(table(), St).

table() ->
    [{<<"band">>,{function,fun fband/2}},
     {<<"bnot">>,{function,fun fbnot/2}},
     {<<"bor">>,{function,fun fbor/2}},
     {<<"btest">>,{function,fun fbtest/2}},
     {<<"bxor">>,{function,fun fbxor/2}},
     {<<"lshift">>,{function,fun flshift/2}},
     {<<"rshift">>,{function,fun frshift/2}},
     {<<"arshift">>,{function,fun farshift/2}},
     {<<"lrotate">>,{function,fun flrotate/2}},
     {<<"rrotate">>,{function,fun frrotate/2}},
     {<<"extract">>,{function,fun fextract/2}},
     {<<"replace">>,{function,fun freplace/2}}
     ].

fband(As, St) ->
    case luerl_lib:tointegers(As) of
    nil -> badarg_error('band', As, St);
    L when is_list(L) -> {[aband(L)], St}
    end.

aband([]) -> ?DEFAULT_BAND;
aband([X|T]) -> aband(T, checkint32(X)).

aband([], A) -> float(A);
aband([X|T], A) -> aband(T, checkint32(X) band A).

fbnot(As, St) ->
    case luerl_lib:tointegers(As) of
    [N|_] ->
        NotN = bnot checkint32(N),
        {[float(NotN)], St};
    _ -> badarg_error('bnot', As, St)
    end.

fbor(As, St) ->
    case luerl_lib:tointegers(As) of
    nil -> badarg_error('bor', As, St);
    L when is_list(L) -> {[abor(L)], St}
    end.

abor([]) -> ?DEFAULT_BOR;
abor([X|T]) -> abor(T, checkint32(X)).

abor([], A) -> float(A);
abor([X|T], A) -> abor(T, checkint32(X) bor A).

fbtest(As, St) ->
    case luerl_lib:tointegers(As) of
    nil -> badarg_error('btest', As, St);
    L when is_list(L) -> {[aband(L) /= 0], St}
    end.

fbxor(As, St) ->
    case luerl_lib:tointegers(As) of
    nil -> badarg_error('bxor', As, St);
    L when is_list(L) -> {[abxor(L)], St}
    end.

abxor([]) -> ?DEFAULT_BXOR;
abxor([X|T]) -> abxor(T, checkint32(X)).

abxor([], A) -> float(A);
abxor([X|T], A) -> abxor(T, checkint32(X) bxor A).

flshift(As, St) ->
    case luerl_lib:tointegers(As) of
    [X,Y|_] -> {[float(checkint32(X) bsl trunc(Y))], St};
    _ -> badarg_error('lshift', As, St)
    end.

frshift(As, St) ->
    case luerl_lib:tointegers(As) of
    [X,Y|_] -> {[float(checkint32(X) bsr trunc(Y))], St};
    _ -> badarg_error('rshift', As, St)
    end.

farshift(As, St) ->
    case luerl_lib:tointegers(As) of
    [X,Y|_] ->
        Disp = trunc(Y),
        case Disp > 0 of
            true -> {[float(checkint32(X) bsr trunc(Y))], St};
            false -> {[float(checkint32(X) bsl abs(trunc(Y)))], St}
        end;
    _ -> badarg_error('arshift', As, St)
    end.

flrotate(As, St) ->
    case luerl_lib:tointegers(As) of
    [X,Y|_] -> {[float(lrotate(checkint32(X), trunc(Y)))], St};
    _ -> badarg_error('lrotate', As, St)
    end.

frrotate(As, St) ->
    case luerl_lib:tointegers(As) of
    [X,Y|_] -> {[float(rrotate(checkint32(X), trunc(Y)))], St};
    _ -> badarg_error('rrotate', As, St)
    end.

fextract(As, St) ->
    case luerl_lib:tointegers(As) of
    [N,Field,Width|_] ->
        {[float(extract(N, Field, Width, As, St))], St};
    [N,Field|_] ->
        {[float(extract(N, Field, 1, As, St))], St};
    _ -> badarg_error('extract', As, St)
    end.

freplace(As, St) ->
    case luerl_lib:tointegers(As) of
    [N,V,Field,Width|_] ->
        {[float(replace(N, V, Field, Width, As, St))], St};
    [N,V,Field|_] ->
        {[float(replace(N, V, Field, 1, As, St))], St};
    _ -> badarg_error('replace', As, St)
    end.


%% Internal
lrotate(X, Y) when Y < 0 ->
    rrotate(X, abs(Y));
lrotate(X, Y) when Y == 0 ->
    X;
lrotate(X1, Y) ->
    Most = X1 band ?MOST_SIGNIFICANT,
    X2 = uint32(X1 bsl 1),
    X3 = X2 bor (Most bsr 31),
    lrotate(X3, Y - 1).


rrotate(X, Y) when Y < 0 ->
    lrotate(X, abs(Y));
rrotate(X, Y) when Y == 0 ->
    X;
rrotate(X1, Y) ->
    Least = X1 band ?LEAST_SIGNIFICANT,
    X2 = X1 bsr 1,
    X3 = X2 bor (Least bsl 31),
    rrotate(X3, Y - 1).

uint32(N) ->
    <<Res:32/integer-unsigned>> = <<N:32>>,
    Res.

checkint32(N) ->
    uint32(trunc(N)).

ge0(N, Where, As, St) ->
    case N >= 0 of
        true -> N;
        false -> badarg_error(Where, As, St)
    end.

gt0(N, Where, As, St) ->
    case N > 0 of
        true -> N;
        false -> badarg_error(Where, As, St)
    end.

le(N, V, Where, As, St) ->
    case N =< V of
        true -> N;
        false -> badarg_error(Where, As, St)
    end.

extract(N1, Field1, Width1, As, St) ->
    N2 = checkint32(N1),
    Field2 = trunc(Field1),
    Width2 = trunc(Width1),
    _ = ge0(Field2, 'extract', As, St),
    _ = gt0(Width2, 'extract', As, St),
    _ = le(Field2 + Width2, 32, 'extract', As, St),
    trunc(N2 / math:pow(2, Field2)) rem trunc(math:pow(2, Width2)).

replace(N1, V1, Field1, Width1, As, St) ->
    N2 = checkint32(N1),
    V2 = checkint32(V1),
    Field2 = trunc(Field1),
    Width2 = trunc(Width1),
    _ = ge0(Field2, 'replace', As, St),
    _ = gt0(Width2, 'replace', As, St),
    _ = le(Field2 + Width2, 32, 'extract', As, St),
    Field3 = trunc(math:pow(2, Field2)),
    Width3 = trunc(math:pow(2, Width2)),
    FW = Field3 * Width3,
    (N2 rem Field3) +
    (V2 rem Width3) * Field3 + trunc(N2 div FW) * FW.
