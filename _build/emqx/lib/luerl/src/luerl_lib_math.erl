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

%% File    : luerl_lib_math.erl
%% Author  : Robert Virding
%% Purpose : The math library for Luerl.

-module(luerl_lib_math).

-export([install/1,fmod/2,frexp/2]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

table() ->
    [{<<"abs">>,{function,fun abs/2}},
     {<<"acos">>,{function,fun acos/2}},
     {<<"asin">>,{function,fun asin/2}},
     {<<"atan">>,{function,fun atan/2}},
     {<<"atan2">>,{function,fun atan2/2}},
     {<<"ceil">>,{function,fun ceil/2}},
     {<<"cos">>,{function,fun cos/2}},
     {<<"cosh">>,{function,fun cosh/2}},
     {<<"deg">>,{function,fun deg/2}},
     {<<"exp">>,{function,fun exp/2}},
     {<<"floor">>,{function,fun floor/2}},
     {<<"fmod">>,{function,fun fmod/2}},
     {<<"frexp">>,{function,fun frexp/2}},
     {<<"huge">>,1.7976931348623157e308},	%From the specs
     {<<"ldexp">>,{function,fun ldexp/2}},
     {<<"log">>,{function,fun log/2}},
     {<<"log10">>,{function,fun log10/2}},	%For 5.1 backwards compatibility
     {<<"max">>,{function,fun max/2}},
     {<<"min">>,{function,fun min/2}},
     {<<"modf">>,{function,fun modf/2}},
     {<<"pi">>,math:pi()},
     {<<"pow">>,{function,fun pow/2}},
     {<<"rad">>,{function,fun rad/2}},
     {<<"random">>,{function,fun random/2}},
     {<<"randomseed">>,{function,fun randomseed/2}},
     {<<"sin">>,{function,fun sin/2}},
     {<<"sinh">>,{function,fun sinh/2}},
     {<<"sqrt">>,{function,fun sqrt/2}},
     {<<"tan">>,{function,fun tan/2}},
     {<<"tanh">>,{function,fun tanh/2}}
    ].

%% abs(Args, State) -> {[Ret],State}.

abs(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[abs(N)],St};
	_ -> badarg_error(abs, As, St)
    end.

acos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:acos(N)],St};
	nil -> badarg_error(acos, As, St)
    end.

asin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:asin(N)],St};
	_ -> badarg_error(asin, As, St)
    end.

atan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:atan(N)],St};
	_ -> badarg_error(atan, As, St)
    end.

atan2(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:atan2(N1, N2)],St};
	_ -> badarg_error(atan2, As, St)
    end.

ceil(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] when round(N) == N -> {[N],St};
	[N|_] -> {[float(round(N + 0.5))],St};
	_ -> badarg_error(ceil, As, St)
    end.

cos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cos(N)],St};
	_ -> badarg_error(cos, As, St)
    end.

cosh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cosh(N)],St};
	_ -> badarg_error(cosh, As, St)
    end.

deg(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[180.0*N/math:pi()],St};
	_ -> badarg_error(deg, As, St)
    end.

exp(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:exp(N)],St};
	_ -> badarg_error(exp, As, St)
    end.

floor(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] when round(N) == N -> {[N],St};
	[N|_] -> {[float(round(N - 0.5))],St};
	_ -> badarg_error(floor, As, St)
    end.

fmod(As, St) ->
    case luerl_lib:tonumbers(As) of
	[X,Y|_] ->
	    Div = float(trunc(X/Y)),
	    Rem = X - Div*Y,
	    {[Rem],St};
	_ -> badarg_error(fmod, As, St)
    end.

frexp(As, St) ->				%M,E such that X = M*2^E
    case luerl_lib:tonumbers(As) of
	[X|_] ->
	    <<_:1,E0:11,M0:52>> = <<X/float>>,	%The sneaky bit!
	    Two52 = 1 bsl 52,
	    M1 = (M0 bor Two52)/Two52,
	    if M1 >= 1.0 -> M2 = M1/2, E1 = E0 - 1022; %Export M2, E1
	       M1 < 0.5 -> M2 = M1*2.0, E1 = E0 - 1024;
	       true -> M2 = M1, E1 = E0 - 1023
	    end,
	    {[float(M2),float(E1)],St};
	_ -> badarg_error(frexp, As, St)
    end.

ldexp(As, St) ->
    case luerl_lib:conv_list(As, [lua_number,lua_integer]) of
	[M,E] ->
	    {[M*math:pow(2, E)],St};
%% 	    <<X/float>> = <<0:1,E:11,M:52>>,
%% 	    {[X],St};
	_ -> badarg_error(ldexp, As, St)
    end.

log(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N] -> {[math:log(N)],St};
	[N,10.0|_] -> {[math:log10(N)],St};	%Seeing it is builtin
	[N1,N2|_] ->
	    {[math:log(N1)/math:log(N2)],St};
	_ -> badarg_error(log, As, St)
    end.

log10(As, St) ->				%For 5.1 backwards compatibility
    case luerl_lib:tonumbers(As) of
	[0.0|_] -> {[-500.0],St};		%Bit hacky
	[N|_] -> {[math:log10(N)],St};
	_ -> badarg_error(log10, As, St)
    end.

max(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:max(Ns)],St};
	_ -> badarg_error(max, As, St)
    end.

min(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:min(Ns)],St};
	_ -> badarg_error(min, As, St)
    end.

modf(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] ->
	    I = float(trunc(N)),		%Integral part
	    {[I,N-I],St};
	_ -> badarg_error(modf, As, St)
    end.

pow(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:pow(N1, N2)],St};
	_ -> badarg_error(pow, As, St)
    end.

rad(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:pi()*N/180.0],St};
	_ -> badarg_error(sinh, As, St)
    end.

%% Use the correct random number module.

-ifdef(NEW_RAND).
-define(RAND_UNIFORM(), rand:uniform()).
-define(RAND_UNIFORM(L), rand:uniform(L)).
-define(RAND_SEED(S1,S2,S3), rand:seed(exs1024, {S1,S2,S3})).
-else.
-define(RAND_UNIFORM(), random:uniform()).
-define(RAND_UNIFORM(L), random:uniform(L)).
-define(RAND_SEED(S1,S2,S3), random:seed(S1, S2, S3)).
-endif.

random(As, St) ->
    case luerl_lib:to_ints(As) of
	[] -> {[?RAND_UNIFORM()],St};		%0-1.0
	[M] when M > 1 ->
	    R = ?RAND_UNIFORM(M),
	    {[float(R)],St};
	[M,N] when N > M ->
	    R = ?RAND_UNIFORM(N - M),
	    {[float(R + M)],St};
	_ -> badarg_error(random, As, St)
    end.

randomseed(As, St) ->
    case luerl_lib:tonumbers(As) of
	[S|_] ->
	    %% Split float-64 into three integers.
	    <<A1:24,A2:24,A3:16>> = <<S/float>>,
	    ?RAND_SEED(A1, A2, A3),
	    {[],St};
	_ -> badarg_error(randomseed, As, St)
    end.

sin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sin(N)],St};
	_ -> badarg_error(sin, As, St)
    end.

sinh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sinh(N)],St};
	_ -> badarg_error(sinh, As, St)
    end.

sqrt(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sqrt(N)],St};
	_ -> badarg_error(sqrt, As, St)
    end.

tan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tan(N)],St};
	_ -> badarg_error(tan, As, St)
    end.

tanh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tanh(N)],St};
	_ -> badarg_error(tanh, As, St)
    end.
