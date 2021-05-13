%%% Copy of string functions from the OTP-20 string.erl module
%%% These functions are kept to keep comparisons stable and
%%% to prevent them from being unicode-aware (which could allow
%%% an attacker to slip some interesting stuff through in hostnames)
%%% even though they are being deprecated starting in OTP-21.
%%%

%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(ssl_verify_string).
-export([to_lower/1, chr/2, str/2, substr/2, substr/3, strip/3]).

-spec to_lower(StringOrChar) -> StringOrChar when
                StringOrChar :: io_lib:latin1_string() | char().
to_lower(S) when is_list(S) ->
    [to_lower_char(C) || C <- S];
to_lower(C) when is_integer(C) ->
    to_lower_char(C).

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    C + 32;
to_lower_char(C) ->
    C.

-spec chr(String, Character) -> Index when
      String :: string(),
      Character :: char(),
      Index :: non_neg_integer().
chr(S, C) when is_integer(C) -> chr(S, C, 1).
chr([C|_Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], _C, _I) -> 0.

-spec str(String, SubString) -> Index when
      String :: string(),
      SubString :: string(),
      Index :: non_neg_integer().

str(S, Sub) when is_list(Sub) -> str(S, Sub, 1).

str([C|S], [C|Sub], I) ->
    case l_prefix(Sub, S) of
        true -> I;
        false -> str(S, [C|Sub], I+1)
    end;
str([_|S], Sub, I) -> str(S, Sub, I+1);
str([], _Sub, _I) -> 0.

l_prefix([C|Pre], [C|String]) -> l_prefix(Pre, String);
l_prefix([], String) when is_list(String) -> true;
l_prefix(Pre, String) when is_list(Pre), is_list(String) -> false.

-spec substr(String, Start) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer().

substr(String, 1) when is_list(String) -> 
    String;
substr(String, S) when is_integer(S), S > 1 ->
    substr2(String, S).

-spec substr(String, Start, Length) -> SubString when
      String :: string(),
      SubString :: string(),
      Start :: pos_integer(),
      Length :: non_neg_integer().

substr(String, S, L) when is_integer(S), S >= 1, is_integer(L), L >= 0 ->
    substr1(substr2(String, S), L).

substr1([C|String], L) when L > 0 -> [C|substr1(String, L-1)];
substr1(String, _L) when is_list(String) -> [].

substr2(String, 1) when is_list(String) -> String;
substr2([_|String], S) -> substr2(String, S-1).

-spec strip(String, Direction, Character) -> Stripped when
      String :: string(),
      Stripped :: string(),
      Direction :: 'left' | 'right' | 'both',
      Character :: char().

strip(String, right, Char) -> strip_right(String, Char);
strip(String, left, Char) -> strip_left(String, Char);
strip(String, both, Char) ->
    strip_right(strip_left(String, Char), Char).

strip_left([Sc|S], Sc) ->
    strip_left(S, Sc);
strip_left([_|_]=S, Sc) when is_integer(Sc) -> S;
strip_left([], Sc) when is_integer(Sc) -> [].

strip_right([Sc|S], Sc) ->
    case strip_right(S, Sc) of
        [] -> [];
        T  -> [Sc|T]
    end;
strip_right([C|S], Sc) ->
    [C|strip_right(S, Sc)];
strip_right([], Sc) when is_integer(Sc) ->
    [].

