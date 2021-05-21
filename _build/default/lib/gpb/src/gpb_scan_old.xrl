%% This line tells emacs to use -*- erlang -*- mode for this file

%%% Copyright (C) 2010-2011  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA


Definitions.
D       = [0-9]
H       = [0-9a-fA-F]
WS      = [\000-\s]+

Rules.

required        : {token, {required,TokenLine}}.
optional        : {token, {optional,TokenLine}}.
repeated        : {token, {repeated,TokenLine}}.
oneof           : {token, {oneof,TokenLine}}.

double          : {token, {double,TokenLine}}.
float           : {token, {float,TokenLine}}.
int32           : {token, {int32,TokenLine}}.
int64           : {token, {int64,TokenLine}}.
uint32          : {token, {uint32,TokenLine}}.
uint64          : {token, {uint64,TokenLine}}.
sint32          : {token, {sint32,TokenLine}}.
sint64          : {token, {sint64,TokenLine}}.
fixed32         : {token, {fixed32,TokenLine}}.
fixed64         : {token, {fixed64,TokenLine}}.
sfixed32        : {token, {sfixed32,TokenLine}}.
sfixed64        : {token, {sfixed64,TokenLine}}.
bool            : {token, {bool,TokenLine}}.
string          : {token, {string,TokenLine}}.
bytes           : {token, {bytes,TokenLine}}.
map             : {token, {map,TokenLine}}.

package         : {token, {package,TokenLine}}.
message         : {token, {message,TokenLine}}.
enum            : {token, {enum,TokenLine}}.
option          : {token, {option,TokenLine}}.
import          : {token, {import,TokenLine}}.
default         : {token, {default,TokenLine}}.
packed          : {token, {packed,TokenLine}}.
deprecated      : {token, {deprecated,TokenLine}}.
extensions      : {token, {extensions,TokenLine}}.
extend          : {token, {extend,TokenLine}}.
to              : {token, {to,TokenLine}}.
max             : {token, {max,TokenLine}}.
reserved        : {token, {reserved,TokenLine}}.
group           : {token, {group,TokenLine}}.

service         : {token, {service,TokenLine}}.
rpc             : {token, {rpc,TokenLine}}.
returns         : {token, {returns,TokenLine}}.
stream          : {token, {stream,TokenLine}}.

true            : {token, {bool_lit,TokenLine,true}}.
false           : {token, {bool_lit,TokenLine,false}}.

syntax          : {token, {syntax,TokenLine}}.


"(\\x[0-9a-fA-F]|\\[0-7]|\\[abfnrtv?"'\\]|[^"\n])*" :
                  {token,{str_lit,TokenLine,string_value(TokenChars)}}.
'(\\x[0-9a-fA-F]|\\[0-7]|\\[abfnrtv?"'\\]|[^'\n])*' :
                   {token,{str_lit,TokenLine,string_value(TokenChars)}}.

(\+|-)?{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_1(TokenChars)}}.
(\+|-)?\.{D}+((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_2(TokenChars)}}.
(\+|-)?{D}+\.((E|e)(\+|\-)?{D}+)? :
                   {token,{float_lit,TokenLine,str_to_float_3(TokenChars)}}.
(\+|-)?{D}+(E|e)(\+|\-)?{D}+ :
                   {token,{float_lit,TokenLine,str_to_float_4(TokenChars)}}.

% Cannot create neither +-inf or nan in erlang...
% (\+|-)?inf :       {token,{float_lit,TokenLine,create_inf(TokenChars)}}.
% nan :              {token,{float_lit,TokenLine,create_nan()}}.

(\+|-)?0[xX]{H}+ : {token,{hex_lit,TokenLine,hexstr_to_integer(TokenChars)}}.
(\+|-)?0{D}+     : {token,{oct_lit,TokenLine,octstr_to_integer(TokenChars)}}.
(\+|-)?{D}+      : {token,{dec_lit,TokenLine,list_to_integer(TokenChars)}}.

[a-zA-Z_][A-Za-z0-9_]* : {token,{identifier,TokenLine,TokenChars}}.

\.              : {token, {'.', TokenLine}}.
;               : {token, {';', TokenLine}}.
\{              : {token, {'{', TokenLine}}.
\}              : {token, {'}', TokenLine}}.
\[              : {token, {'[', TokenLine}}.
\]              : {token, {']', TokenLine}}.
\(              : {token, {'(', TokenLine}}.
\)              : {token, {')', TokenLine}}.
=               : {token, {'=', TokenLine}}.
,               : {token, {',', TokenLine}}.
<               : {token, {'<', TokenLine}}.
>               : {token, {'>', TokenLine}}.


//.*\n          : skip_token.                    %% C++-style comment
//.*            : skip_token.                    %% C++-style comment
(/\*([^*]|(\*+[^*/]))*\*+/)|(//.*) : skip_token. %% C-style comment
{WS}            : skip_token.


Erlang code.

-include_lib("eunit/include/eunit.hrl").

%% Eliminate a dialyzer warning like below:
%% /usr/lib/erlang/lib/parsetools-2.1.1/include/leexinc.hrl:268:
%%  Function yyrev/2 will never be called
-dialyzer({nowarn_function, yyrev/2}).

string_value(S) -> % S is with start+end quote
    %% Strip quotes.
    string_val_2(lists:sublist(S, 2, length(S) - 2)).

string_val_2([$\\|Cs]) -> string_escape(Cs);
string_val_2([C|Cs])   -> [C|string_val_2(Cs)];
string_val_2([])       -> [].

-define(is_octal_char(C), $0 =< C, C =< $7).

string_escape("x"++Rest) ->
    {HexChars, Rest2} = collect(fun is_hex_char/1, 2, Rest),
    [hex_to_integer(HexChars) | string_val_2(Rest2)];
string_escape("u"++Rest) ->
    {UnicodeCodePoint, Rest2} = collect(fun is_hex_char/1, 4, Rest),
    [hex_to_integer(UnicodeCodePoint) | string_val_2(Rest2)];
string_escape("U"++Rest) ->
    {UnicodeCodePoint, Rest2} = collect(fun is_hex_char/1, 8, Rest),
    [hex_to_integer(UnicodeCodePoint) | string_val_2(Rest2)];
string_escape([Oct|Rest]) when ?is_octal_char(Oct) ->
    {OctChars, Rest2} = collect(fun is_oct_char/1, 3, [Oct|Rest]),
    [oct_to_integer(OctChars) | string_val_2(Rest2)];
string_escape([C|Rest]) ->
    [escape_char(C) | string_val_2(Rest)].

escape_char($a) -> 7;                           %\a = BEL
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char(C)  -> C.

collect(Pred, MaxLen, Str) -> splitwith_len(Pred, MaxLen, Str, []).

splitwith_len(Pred, N, [H|Tl], Acc) when N > 0 ->
    case Pred(H) of
        true  -> splitwith_len(Pred, N-1, Tl, [H|Acc]);
        false -> {lists:reverse(Acc), [H|Tl]}
    end;
splitwith_len(_Pred, _N, L, Acc) ->
    {lists:reverse(Acc), L}.

is_oct_char(C) -> $0 =< C andalso C =< $7.
is_hex_char(C) -> ($0 =< C andalso C =< $9)
                      orelse ($a =< C andalso C =< $f)
                      orelse ($A =< C andalso C =< $F).

oct_to_integer(Str) -> erlang:list_to_integer(Str, 8).
hex_to_integer(Str) -> erlang:list_to_integer(Str, 16).

octstr_to_integer(Str) -> oct_to_integer(Str).

hexstr_to_integer("0x"++H)  -> hex_to_integer(H);
hexstr_to_integer("0X"++H)  -> hex_to_integer(H);
hexstr_to_integer("+0x"++H) -> hex_to_integer(H);
hexstr_to_integer("+0X"++H) -> hex_to_integer(H);
hexstr_to_integer("-0x"++H) -> -hex_to_integer(H);
hexstr_to_integer("-0X"++H) -> -hex_to_integer(H).

str_to_float_1(S)        -> list_to_float(S).

str_to_float_2("."++_=S) -> str_to_float_1("0"++S);
str_to_float_2("+."++T)  -> str_to_float_1("0."++T);
str_to_float_2("-."++T)  -> str_to_float_1("-0."++T).

str_to_float_3(S) -> %% No decimals after `.' Possibly e+-<n> following `.'
    {UpToDot, "."++Rest} = collect(fun isnt_dot/1, length(S), S),
    str_to_float_1(UpToDot++"."++"0"++Rest).

str_to_float_4(S) -> %% Integer preceding e+-<n>
    {UpToDot, Rest} = collect(fun isnt_exp_e/1, length(S), S),
    str_to_float_1(UpToDot++"."++"0"++Rest).

isnt_dot(C) -> C /= $. .

isnt_exp_e($e) -> false;
isnt_exp_e($E) -> false;
isnt_exp_e(_)  -> true.
