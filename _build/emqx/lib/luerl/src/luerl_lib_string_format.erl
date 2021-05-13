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

%% File    : luerl_lib_string_format.erl
%% Author  : Robert Virding
%% Purpose : The string formatting for Luerl.

-module(luerl_lib_string_format).

-include("luerl.hrl").

-export([format/3]).

format(F, As, St0) ->
    {Str,St1} = format_loop(luerl_lib:to_list(F), As, St0),
    {[iolist_to_binary(Str)],St1}.

format_loop(Fmt, As, St) -> format_loop(Fmt, As, St, []).

format_loop([$%|Fmt0], As0, St0, Acc) ->
    {Format,Fmt1} = collect(Fmt0),
    {Out,As1,St1} = build(Format, As0, St0),
    format_loop(Fmt1, As1, St1, [Out|Acc]);
format_loop([$\\,C|Fmt], As, St, Acc) ->
    format_loop(Fmt, As, St, [C|Acc]);
format_loop([C|Fmt], As, St, Acc) ->
    format_loop(Fmt, As, St, [C|Acc]);
format_loop([], _, St, Acc) ->			%Ignore extra arguments
    {lists:reverse(Acc),St}.

%% collect(Format) -> {{C,Flags,Field,Precision},Format}.
%%  Collect a conversion specification.

collect(Fmt0) ->
    {Fl,Fmt1} = flags(Fmt0),			%The flags characters
    {F,Fmt2} = field_width(Fmt1),		%The field width
    {P,Fmt3} = precision(Fmt2),			%The precision
    {C,Fmt4} = collect_cc(Fmt3),		%The control character
    {{C,Fl,F,P},Fmt4}.

%% Handling the flags of a format.
%% Yes, we should use a tuple or record, but this is much more fun.

-define(FL_NONE, 0).
-define(FL_H, 2#00001).
-define(FL_Z, 2#00010).
-define(FL_M, 2#00100).
-define(FL_S, 2#01000).
-define(FL_P, 2#10000).

-define(SET_BIT(FL,B), (FL bor (B))).
-define(ANY_BITS(Fl, B), ((Fl band (B)) =/= 0)).
-define(ALL_BITS(Fl, B), ((Fl band (B)) =:= (B))).
-define(NO_BITS(Fl, B), ((Fl band (B)) =:= 0)).

flags(Fmt) -> flags(Fmt, ?FL_NONE).

flags([$#|Fmt], Fl) -> flags(Fmt, ?SET_BIT(Fl, ?FL_H));
flags([$0|Fmt], Fl) -> flags(Fmt, ?SET_BIT(Fl, ?FL_Z));
flags([$-|Fmt], Fl) -> flags(Fmt, ?SET_BIT(Fl, ?FL_M));
flags([$\s|Fmt], Fl) -> flags(Fmt, ?SET_BIT(Fl, ?FL_S));
flags([$+|Fmt], Fl) -> flags(Fmt, ?SET_BIT(Fl, ?FL_P));
flags(Fmt, Fl) -> {Fl,Fmt}.

field_width(Fmt) -> field_value(Fmt).

precision([$.|Fmt]) -> field_value(Fmt);
precision(Fmt) -> {none,Fmt}.

collect_cc([C|Fmt]) -> {C,Fmt};
collect_cc([]) -> {none,[]}.

field_value([C|_]=Fmt) when C >= $0, C =< $9 -> field_value(Fmt, 0);
field_value(Fmt) -> {none,Fmt}.

field_value([C|Fmt], F) when C >= $0, C =< $9 ->
    field_value(Fmt, 10*F + (C - $0));
field_value(Fmt, F) -> {F,Fmt}.

%% build({C,Flags,Field,Precision}, Args) -> {Out,Args}.
%%  Build a string from the conversion specification.
%%  Implemented conversions are d,i o,u,x,X e,E f,F g,G c s %.
%%  No length modifiers, h L l, no conversions n p S C allowed.

build({$q,_,_,_}, [A|As], St0) ->
    %% No triming or adjusting of the $q string, we only get all of
    %% it. Use an RE to split string on quote needing characters.
    {[S0],St1} = luerl_lib_basic:tostring([A], St0),
    RE = "([\\0-\\39\\\n\\\"\\\\\\177-\\237])",	%You don't really want to know!
    Ss0 = re:split(S0, RE, [{return,binary},trim]),
    Ss1 = build_q(Ss0),
    {[$",Ss1,$"],As,St1};
build({$s,Fl,F,P}, [A|As], St0) ->
    {[S0],St1} = luerl_lib_basic:tostring([A], St0),
    S1 = trim_bin(S0, P),
    {adjust_bin(S1, Fl, F),As,St1};
build({$c,Fl,F,_}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    C = if is_number(N), N >= 0, N < 256 -> trunc(N);
	   is_number(N) -> $?
	end,
    {adjust_str([C], Fl, F),As,St};
%% Integer formats.
build({$i,Fl,F,P}, [A|As], St) ->
    I = luerl_lib:to_int(A),
    {format_decimal(Fl, F, P, I),As,St};
build({$d,Fl,F,P}, [A|As], St) ->
    I = luerl_lib:to_int(A),
    {format_decimal(Fl, F, P, I),As,St};
build({$o,Fl,F,P}, [A|As], St) ->
    I = luerl_lib:to_int(A),
    {format_octal(Fl, F, P, I),As,St};
build({$x,Fl,F,P}, [A|As], St) ->
    I = luerl_lib:to_int(A),
    {format_hex(Fl, F, P, I),As,St};
build({$X,Fl,F,P}, [A|As], St) ->
    I = luerl_lib:to_int(A),
    {format_HEX(Fl, F, P, I),As,St};
%% Float formats.
build({$e,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {e_float(Fl, F, P, N),As,St};
build({$E,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {e_float(Fl, F, P, N),As,St};
build({$f,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {f_float(Fl, F, P, N),As,St};
build({$F,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {f_float(Fl, F, P, N),As,St};
build({$g,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {g_float(Fl, F, P, N),As,St};
build({$G,Fl,F,P}, [A|As], St) ->
    N = luerl_lib:tonumber(A),
    {g_float(Fl, F, P, N),As,St};
%% Literal % format.
build({$%,?FL_NONE,none,none}, As, St) ->	%No flags, field or precision!
    {"%",As,St}.

%% format_decimal(Flags, Field, Precision, Number) -> String.
%% format_octal(Flags, Field, Precision, Number) -> String.
%% format_hex(Flags, Field, Precision, Number) -> String.
%% format_HEX(Flags, Field, Precision, Number) -> String.
%% format_integer(Flags, Field, Precision, Number, String) -> String.
%%  Print integer Number with base Base. This is a bit messy as we are
%%  following string.format handling.

format_decimal(Fl, F, P, N) ->
    Str = integer_to_list(abs(N), 10),
    format_integer(Fl, F, P, N, Str).

format_octal(Fl, F, P, N) ->
    Str = integer_to_list(abs(N), 8),
    format_integer(Fl, F, P, N, Str).

format_hex(Fl, F, P, N) ->
    Str = lists:flatten(io_lib:fwrite("~.16b", [N])),
    format_integer(Fl, F, P, N, Str).

format_HEX(Fl, F, P, N) ->
    Str = lists:flatten(io_lib:fwrite("~.16B", [N])),
    format_integer(Fl, F, P, N, Str).

format_integer(Fl, F, P, N, Str0) ->
    Sign = sign(Fl, N),
    if P =/= none ->
	    Str1 = Sign ++ lists:flatten(adjust_str(Str0, ?FL_Z, P)),
	    adjust_str(Str1, (Fl band ?FL_M), F);
       ?ANY_BITS(Fl, ?FL_M) ->
	    Str1 = Sign ++ Str0,
	    adjust_str(Str1, Fl, F);
       ?ANY_BITS(Fl, ?FL_Z), F =/= none ->
	    Str1 = adjust_str(Str0, ?FL_Z, F-length(Sign)),
	    Sign ++ Str1;
       true ->
	    Str1 = Sign ++ Str0,
	    adjust_str(Str1, Fl, F)
    end.

%% e_float(Flags, Field, Precision, Number) -> String.
%% f_float(Flags, Field, Precision, Number) -> String.
%% g_float(Flags, Field, Precision, Number) -> String.
%%  Print float Number in e/f/g format.

e_float(Fl, F, P, N) ->
    format_float(Fl, F, e_float_precision(P), "~.*e", N).

f_float(Fl, F, P, N) ->
    format_float(Fl, F, f_float_precision(P), "~.*f", N).

g_float(Fl, F, P, N) ->
    format_float(Fl, F, g_float_precision(P), "~.*g", N).

format_float(Fl, F, P, Format, N) ->
    Str0 = lists:flatten(io_lib:format(Format, [P,abs(N)])),
    Sign = sign(Fl, N),
    if ?ANY_BITS(Fl, ?FL_M) ->
	    Str1 = Sign ++ Str0,
	    adjust_str(Str1, Fl, F);
       ?ANY_BITS(Fl, ?FL_Z) ->
	    Str1 = adjust_str(Str0, ?FL_Z, F-length(Sign)),
	    Sign ++ Str1;
       true ->
	    Str1 = Sign ++ Str0,
	    adjust_str(Str1, Fl, F)
    end.

e_float_precision(none) -> 7;
e_float_precision(P) -> P+1.

f_float_precision(none) -> 6;
f_float_precision(P) -> P.

g_float_precision(none) -> 6;
g_float_precision(P) -> P.

%% sign(Flags, Number) -> SignString.

sign(_, N) when N < 0 -> "-";			%Always sign when N<0
sign(Fl, _) ->
    if ?ALL_BITS(Fl, ?FL_P) -> "+";		%+ flag has priority
       ?ALL_BITS(Fl, ?FL_S) -> " ";
       true -> ""
    end.

trim_bin(Bin, Prec) when is_integer(Prec), byte_size(Bin) > Prec ->
    binary:part(Bin, 0, Prec);
trim_bin(Bin, _) -> Bin.

%% adjust_bin(Binary, Flags, Field) -> iolist().
%% adjust_str(String, Flags, Field) -> iolist().

adjust_bin(Bin, ?FL_NONE, none) -> Bin;
adjust_bin(Bin, Fl, F) when is_integer(F), byte_size(Bin) < F ->
    Size = byte_size(Bin),
    Padding = lists:duplicate(F-Size, pad_char(Fl, F)),
    if ?ALL_BITS(Fl, ?FL_M) -> [Bin,Padding];
       true -> [Padding,Bin]
    end;
adjust_bin(Bin, _, _) -> Bin.

adjust_str(Str, ?FL_NONE, none) -> Str;
adjust_str(Str, Fl, F) when is_integer(F), length(Str) < F ->
    Size = length(Str),
    Padding = lists:duplicate(F-Size, pad_char(Fl, F)),
    if ?ALL_BITS(Fl, ?FL_M) -> [Str,Padding];
       true -> [Padding,Str]
    end;
adjust_str(Str, _, _) -> Str.

%% pad_char(Flags, Field) -> Char.

pad_char(Fl, F) ->
    if ?ALL_BITS(Fl, ?FL_M) -> $\s;		%'-' forces padding to " "
       ?ALL_BITS(Fl, ?FL_Z), F =/= none -> $0;
       true -> $\s
    end.

build_q([<<>>|Ss]) -> build_q(Ss);
build_q([<<$\n>>|Ss]) -> [$\\,$\n|build_q(Ss)];
build_q([<<$">>|Ss]) -> [$\\,$"|build_q(Ss)];
build_q([<<$\\>>|Ss]) -> [$\\,$\\|build_q(Ss)];
build_q([<<C1>>=B1|Ss0]) when C1 >=0, C1 =< 31 ->
    case Ss0 of
	[<<C2,_/binary>>|_] when C2 >= $0, C2 =< $9 ->
	    [io_lib:format("\\~.3.0w", [C1])|build_q(Ss0)];
	[<<>>|Ss1] -> build_q([B1|Ss1]);
	_ -> [io_lib:format("\\~w", [C1])|build_q(Ss0)]
    end;
build_q([<<B>>|Ss0]) when B >= 127, B =< 159 ->
    [io_lib:write(B)|build_q(Ss0)];
build_q([S|Ss]) -> [S|build_q(Ss)];
build_q([]) -> [].
