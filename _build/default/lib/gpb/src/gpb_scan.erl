%%% Copyright (C) 2019  Tomas Abrahamsson
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

%% @doc This is the scanner
%% @private

-module(gpb_scan).
-export([binary/1, binary/2]).
-export([format_error/1]).

-export_type([token/0]).
-export_type([pos/0]).
-export_type([error/0]).

%% -- bwd compat -- do not use these ----------------------------------
%% Deprecated!
%% Instead, to retrieve proto definitions,
%% use the option to_proto_defs
%% with gpb_compile:file or gpb_compile:string.
%% For exprotobuf, see also
%% https://github.com/bitwalker/exprotobuf/issues/114
-export([string/1]). % use opt to_proto_defs instead
%% --^^--- bwd compat -- do not use these ----------------------------

-type token() :: {token_data(), pos(), orig_text()}.
-type token_data()  :: atom() | % punctuation(-like) characters, generally
                       binary() | % words
                       {str_lit, string()} | % quoted strings
                       {float_lit, float()} |
                       {int_lit, {int_format(), integer()}}.

-type int_format() :: dec | hex | oct.

-type orig_text() :: binary() | atom().

-type pos() :: LineNumber::pos_integer().

-type error() :: {LineNumber::pos_integer(), module(), Reason::term()}.

%% @doc Tokenize input. Comments and whitespace are skipped over.
%% Input is expected to be UTF-8, or at least the strings in the input are.
%% No byte order mark is expected in the input binary.
%%
%% The format of the return values (both on success and error) mimic
%% those of a leex-generated scanner.
%%
%% Please, do not use this from outside of gpb. Instead, use the option
%% `to_proto_defs' with gpb_compile:file/1,2 or gob_string/2,3, to get
%% a parsed .proto file.
%%
%% @hidden
%% @equiv binary(Bin, 1)
-spec binary(binary()) ->
          {ok, [token()], EndLine::pos_integer()} |
          {error, error(), LineNumber::pos_integer()}.
binary(Bin) ->
    binary(Bin, 1).

-spec binary(binary(), StartLine::pos_integer()) ->
          {ok, [token()], EndLine::pos_integer()} |
          {error, error()}.
binary(Bin, Line) ->
    b(Bin, Line, []).

-define(f(Fmt, Args), io_lib:format(Fmt, Args)).

format_error({syntax_error, {error, {before, TrailingText}}}) ->
    ?f("syntax error before ~s", [TrailingText]);
format_error({syntax_error, unterminated_comment}) ->
    "unterminated comment";
format_error({syntax_error, {non_utf8_string, StrAcc}}) ->
    ?f("non UTF-8 string ~p", [StrAcc]);
format_error({syntax_error, {unterminated_string, StrAcc}}) ->
    ?f("unterminated string starting with ~s", [StrAcc]);
format_error({syntax_error, {invalid_code_point, Reason, C}}) ->
    ?f("invalid code point ~p char (~w)", [Reason, C]);
format_error({syntax_error, {unterminated_numeral, Bin}}) ->
    ?f("unterminated numeral ~s", [Bin]);
format_error({syntax_error, {invalid_number, {before, TrailingText}}}) ->
    ?f("invalid number before ~s", [TrailingText]);
format_error({syntax_error, {invalid_number, {need_space, X}}}) ->
    ?f("need space after number, found ~c", [X]);
format_error({syntax_error, {invalid_octal, Bin, {decimal_digit, D}}}) ->
    ?f("number with leading zero is octal, invalid digit ~c in ~s", [D, Bin]);
format_error({syntax_error, {invalid_number, Bin, {need_space, X}}}) ->
    ?f("need space after number, found ~c in ~s", [X, Bin]);
format_error({syntax_error, {invalid_number, multiple_decimal_points}}) ->
    ?f("invalid number, multiple decimal points", []);
format_error({syntax_error, {invalid_number, decimal_point_in_exp}}) ->
    ?f("invalid number, decimal point in exponent", []).

-define(is_white(C), (0 =< C andalso C =< $\s)).

-define(is_lower(C), ($a =< C andalso C =< $z)).
-define(is_upper(C), ($A =< C andalso C =< $Z)).
-define(is_letter(C), (?is_lower(C) orelse ?is_upper(C) orelse (C =:= $_))).

-define(is_digit(C), ($0 =< C andalso C =< $9)).
-define(is_a_to_f(C), (($a =< C andalso C =< $f)
                       orelse ($A =< C andalso C =< $F))).
-define(is_octal(C), ($0 =< C andalso C =< $7)).
-define(is_sign(C), (C =:= $+ orelse C =:= $-)).
-define(is_exp(C), (C =:= $e orelse C =:= $E)).

b(Bin, Line, Acc) ->
    case Bin of
        <<"//", Rest/binary>> -> skip_cxx_comment(Rest, Line, Acc);
        <<"/*", Rest/binary>> -> skip_c_comment(Rest, Line, Line, Acc);
        <<"\n", Rest/binary>> -> b(Rest, Line + 1, Acc);
        <<W, Rest/binary>> when ?is_white(W) -> b(Rest, Line, Acc);
        <<"\"", Rest/binary>> ->
            read_string(Rest, $\", <<>>, Line, Line, <<"\"">>, Acc);
        <<"'", Rest/binary>>  ->
            read_string(Rest, $', <<>>, Line, Line, <<"'">>, Acc);
        <<D, _/binary>> when ?is_digit(D) -> read_number(Bin, Line, Acc);
        <<$., D, _/binary>> when ?is_digit(D) -> read_number(Bin, Line, Acc);
        <<L, Rest/binary>> when ?is_letter(L) ->
            read_word(Rest, <<L>>, Line, Acc);
        <<$., Rest/binary>> -> b(Rest, Line, [{'.', Line, '.'} | Acc]);
        <<$:, Rest/binary>> -> b(Rest, Line, [{':', Line, ':'} | Acc]);
        <<$;, Rest/binary>> -> b(Rest, Line, [{';', Line, ';'} | Acc]);
        <<${, Rest/binary>> -> b(Rest, Line, [{'{', Line, '{'} | Acc]);
        <<$}, Rest/binary>> -> b(Rest, Line, [{'}', Line, '}'} | Acc]);
        <<$[, Rest/binary>> -> b(Rest, Line, [{'[', Line, '['} | Acc]);
        <<$], Rest/binary>> -> b(Rest, Line, [{']', Line, ']'} | Acc]);
        <<$(, Rest/binary>> -> b(Rest, Line, [{'(', Line, '('} | Acc]);
        <<$), Rest/binary>> -> b(Rest, Line, [{')', Line, ')'} | Acc]);
        <<$=, Rest/binary>> -> b(Rest, Line, [{'=', Line, '='} | Acc]);
        <<$,, Rest/binary>> -> b(Rest, Line, [{',', Line, ','} | Acc]);
        <<$<, Rest/binary>> -> b(Rest, Line, [{'<', Line, '<'} | Acc]);
        <<$>, Rest/binary>> -> b(Rest, Line, [{'>', Line, '>'} | Acc]);
        <<$-, Rest/binary>> -> b(Rest, Line, [{'-', Line, '-'} | Acc]);
        <<$+, Rest/binary>> -> b(Rest, Line, [{'+', Line, '+'} | Acc]);
        <<_,  _/binary>>    -> syntax_error({error, before(Bin)}, Line);
        <<>> -> ok(Line, Acc)
    end.

%% -- comments --

skip_cxx_comment(Bin, Line, Acc) ->
    case Bin of
        <<"\n", Rest/binary>> -> b(Rest, Line + 1, Acc);
        <<_, Rest/binary>> -> skip_cxx_comment(Rest, Line, Acc);
        <<>> -> ok(Line, Acc)
    end.

skip_c_comment(Bin, Line0, Line, Acc) ->
    %% Block comments cannot be nested, should we warn if we see /* ??
    case Bin of
        <<"*/", Rest/binary>> -> b(Rest, Line, Acc);
        <<"\n", Rest/binary>> -> skip_c_comment(Rest, Line0, Line + 1, Acc);
        <<_,    Rest/binary>> -> skip_c_comment(Rest, Line0, Line, Acc);
        <<>> -> syntax_error(unterminated_comment, Line0)
    end.

%% -- string --

read_string(Bin, Quote, StrAcc, Line0, Line, Orig, Acc) ->
    case Bin of
        <<Quote, Rest/binary>> ->
            case unicode:characters_to_list(StrAcc, utf8) of
                S when is_list(S) ->
                    Orig1 = <<Orig/binary, Quote>>,
                    b(Rest, Line, [{{str_lit, S}, Line, Orig1} | Acc]);
                {error, _SuccessfullyConverted, _Offending} ->
                    syntax_error({non_utf8_string, StrAcc}, Line0);
                {incomplete, _SuccessfullyConverted, _IncompleteChar} ->
                    syntax_error({non_utf8_string, StrAcc}, Line0)
            end;
        <<"\\u", Rest/binary>> -> % 4-digit (up to) char in hex
            case collect(Rest, fun is_hex_char/1, 4) of
                {B2, Rest2} ->
                    C = hex_to_integer(B2),
                    Orig1 = <<Orig/binary, "\\u", B2/binary>>,
                    add_code_point(Rest2, Quote, StrAcc, C, Line0, Line,
                                   Orig1, Acc);
                eof ->
                    syntax_error({unterminated_string, StrAcc}, Line0)
            end;
        <<"\\U", Rest/binary>> -> % 8-digit (up to) char in hex
            case collect(Rest, fun is_hex_char/1, 8) of
                {B2, Rest2} ->
                    C = hex_to_integer(B2),
                    Orig1 = <<Orig/binary, "\\U", B2/binary>>,
                    add_code_point(Rest2, Quote, StrAcc, C, Line0, Line,
                                   Orig1, Acc);
                eof ->
                    syntax_error({unterminated_string, StrAcc}, Line0)
            end;
        <<"\\x", Rest/binary>> -> % 2-digit (up to) char in hex
            case collect(Rest, fun is_hex_char/1, 2) of
                {B2, Rest2} ->
                    C = hex_to_integer(B2),
                    Orig1 = <<Orig/binary, "\\x", B2/binary>>,
                    add_code_point(Rest2, Quote, StrAcc, C, Line0, Line,
                                   Orig1, Acc);
                eof ->
                    syntax_error({unterminated_string, StrAcc}, Line0)
            end;
        <<"\\", O, _/binary>> when ?is_octal(O) -> % 3-digits (up to) in oct
            <<"\\", Rest/binary>> = Bin,
            case collect(Rest, fun is_oct_char/1, 3) of
                {B2, Rest2} ->
                    C = oct_to_integer(B2),
                    Orig1 = <<Orig/binary, "\\", B2/binary>>,
                    add_code_point(Rest2, Quote, StrAcc, C, Line0, Line,
                                   Orig1, Acc);
                eof ->
                    syntax_error({unterminated_string, StrAcc}, Line0)
            end;
        <<"\\", C, Rest/binary>> ->
            C2 = escape_char(C),
            Orig1 = <<Orig/binary, "\\", C>>,
            read_string(Rest, Quote, <<StrAcc/binary, C2>>, Line0, Line,
                        Orig1, Acc);
        <<C, Rest/binary>> ->
            Orig1 = <<Orig/binary, C>>,
            read_string(Rest, Quote, <<StrAcc/binary, C>>, Line0, Line,
                        Orig1, Acc);
        <<>> ->
            syntax_error({unterminated_string, StrAcc}, Line0)
    end.

add_code_point(Bin, Quote, StrAcc, C, Line0, Line, Orig, Acc) ->
    try <<StrAcc/binary, C/utf8>> of
        StrAcc2 ->
            read_string(Bin, Quote, StrAcc2, Line0, Line, Orig, Acc)
    catch error:badarg ->
            Reason = explain_invalid_code_point(C),
            syntax_error({invalid_code_point, Reason, C}, Line)
    end.

explain_invalid_code_point(C) ->
    if C > 16#10ffff              -> too_large;
       16#d800 =< C, C =< 16#dbff -> high_half_of_utf16_surrogate_pair;
       16#dc00 =< C, C =< 16#dfff -> low_half_of_utf16_surrogate_pair;
       true                       -> other % can't happen?
    end.

escape_char($a) -> 7;                           %\a = BEL
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char(C)  -> C.

%% -- number --

read_number(Bin, Line, Acc) -> % always positive, sign is another token
    case Bin of
        <<"0.", Rest/binary>> -> read_frac(Rest, <<"0.">>, Line, <<"0.">>, Acc);
        <<"0e", Rest/binary>> -> read_exp(Rest, <<"0.0e">>,Line, <<"0e">>, Acc);
        <<"0x", Rest/binary>> -> read_hexnum(Rest,  Line, <<"0x">>, Acc);
        <<"0X", Rest/binary>> -> read_hexnum(Rest,  Line, <<"0X">>, Acc);
        <<"0", _/binary>>     -> read_octnum(Bin, Line, <<>>, Acc);
        _                     -> read_num(Bin, <<>>, Line, <<>>, Acc)
    end.

read_hexnum(Bin, Line, Orig, Acc) ->
    case collect(Bin, fun is_hex_char/1, infinity) of
        {_, <<X, _/binary>>} when ?is_letter(X) ->
           syntax_error({invalid_number, Bin, {need_space, X}}, Line);
        {B2, Rest} ->
            N = hex_to_integer(B2),
            Orig1 = <<Orig/binary, B2/binary>>,
            b(Rest, Line, [{{int_lit, {hex, N}}, Line, Orig1} | Acc]);
        eof ->
            syntax_error({unterminated_numeral, Bin}, Line)
    end.

read_octnum(Bin, Line, Orig, Acc) ->
    case collect(Bin, fun is_oct_char/1, infinity) of
        {_, <<$8, _/binary>>} ->
           syntax_error({invalid_octal, Bin, {decimal_digit, 8}}, Line);
        {_, <<$9, _/binary>>} ->
           syntax_error({invalid_octal, Bin, {decimal_digit, 9}}, Line);
        {_, <<X, _/binary>>} when ?is_letter(X) ->
           syntax_error({invalid_number, Bin, {need_space, X}}, Line);
        {B2, Rest} ->
            Orig1 = <<Orig/binary, B2/binary>>,
            case oct_to_integer(B2) of
                0 ->
                    b(Rest, Line, [{{int_lit, {dec, 0}}, Line, Orig1} | Acc]);
                N ->
                    b(Rest, Line, [{{int_lit, {oct, N}}, Line, Orig1} | Acc])
            end;
        eof ->
            syntax_error({unterminated_numeral, Bin}, Line)
    end.

%% Num -> Digits . Digits Exp?
%%      | . Digits Exp?
%%      | Digits . Exp?
%%      | Digits Exp?
%% Exp -> (E|e)(+|-)?Digits
read_num(Bin, StrAcc, Line, Orig, Acc) ->
    case Bin of
        <<$., Rest/binary>> ->
            Orig1 = <<Orig/binary, $.>>,
            StrAcc2 = <<StrAcc/binary, $.>>,
            StrAcc3 = ensure_0_dot(StrAcc2),
            read_frac(Rest, StrAcc3, Line, Orig1, Acc);
        <<E, Rest/binary>> when ?is_exp(E) ->
            if StrAcc =:= <<>> ->
                    syntax_error({invalid_number, before(Bin)}, Line);
               StrAcc =/= <<>> ->
                    Orig1 = <<Orig/binary, $E>>,
                    StrAcc2 = ensure_digits_dot_digits(StrAcc),
                    StrAcc3 = <<StrAcc2/binary, "e">>,
                    read_exp(Rest, StrAcc3, Line, Orig1, Acc)
            end;
        <<D, Rest/binary>> when ?is_digit(D) ->
            Orig1 = <<Orig/binary, D>>,
            read_num(Rest, <<StrAcc/binary, D>>, Line, Orig1, Acc);
        <<X, _/binary>> when ?is_letter(X) ->
           syntax_error({invalid_number, {need_space, X}}, Line);
        _ ->
            if StrAcc =:= <<>> ->
                    syntax_error({invalid_number, before(Bin)}, Line);
               StrAcc =/= <<>> ->
                    N = list_to_integer(binary_to_list(StrAcc)),
                    b(Bin, Line, [{{int_lit, {dec, N}}, Line, Orig} | Acc])
            end
    end.

read_frac(Bin, StrAcc, Line, Orig, Acc) ->
    case Bin of
        <<D, Rest/binary>> when ?is_digit(D) ->
            Orig1 = <<Orig/binary, D>>,
            read_frac(Rest, <<StrAcc/binary, D>>, Line, Orig1, Acc);
        <<E, Rest/binary>> when ?is_exp(E) ->
            Orig1 = <<Orig/binary, E>>,
            StrAcc2 = ensure_digits_dot_digits(StrAcc),
            StrAcc3 = <<StrAcc2/binary, "e">>,
            read_exp(Rest, StrAcc3, Line, Orig1, Acc);
        <<X, _/binary>> when ?is_letter(X) ->
           syntax_error({invalid_number, {need_space, X}}, Line);
        <<$., _/binary>> ->
           syntax_error({invalid_number, multiple_decimal_points}, Line);
        _ ->
            StrAcc2 = ensure_digits_dot_digits(StrAcc),
            try list_to_float(binary_to_list(StrAcc2)) of
                Float ->
                    b(Bin, Line, [{{float_lit, Float}, Line, Orig} | Acc])
            catch error:badarg ->
                    syntax_error({invalid_number, before(Bin)}, Line)
            end
    end.

read_exp(Bin, StrAcc, Line, Orig, Acc) ->
    case Bin of
        <<S, Rest/binary>> when ?is_sign(S)->
            Orig1 = <<Orig/binary, S>>,
            read_exp_d(Rest, <<StrAcc/binary, S>>, Line, Orig1, Acc);
        <<D, Rest/binary>> when ?is_digit(D) ->
            Orig1 = <<Orig/binary, D>>,
            read_exp_d(Rest, <<StrAcc/binary, D>>, Line, Orig1, Acc);
        <<X, _/binary>> when ?is_letter(X) ->
           syntax_error({invalid_number, {need_space, X}}, Line);
        <<$., _/binary>> ->
           syntax_error({invalid_number, decimal_point_in_exp}, Line);
        _ ->
            syntax_error({invalid_number, before(Bin)}, Line)
    end.

read_exp_d(Bin, StrAcc, Line, Orig, Acc) ->
    case Bin of
        <<D, Rest/binary>> when ?is_digit(D) ->
            Orig1 = <<Orig/binary, D>>,
            read_exp_d(Rest, <<StrAcc/binary, D>>, Line, Orig1, Acc);
        <<X, _/binary>> when ?is_letter(X) ->
           syntax_error({invalid_number, {need_space, X}}, Line);
        <<$., _/binary>> ->
           syntax_error({invalid_number, decimal_point_in_exp}, Line);
        _ ->
            try list_to_float(binary_to_list(StrAcc)) of
                Float ->
                    b(Bin, Line, [{{float_lit, Float}, Line, Orig} | Acc])
            catch error:badarg ->
                    syntax_error({invalid_number, before(Bin)}, Line)
            end
    end.

ensure_0_dot(<<".">>) -> <<"0.">>;
ensure_0_dot(X)       -> X.

ensure_digits_dot_digits(B) -> 'd.d'(B, B).

'd.d'(<<D, Rest/binary>>, B) when ?is_digit(D) -> 'd.d'(Rest, B);
'd.d'(<<>>, B)                                 -> <<B/binary, ".0">>;
'd.d'(<<".">>, B)                              -> <<B/binary, "0">>;
'd.d'(<<".", _/binary>>, B)                    -> B.

%% -- word --

read_word(Bin, StrAcc, Line, Acc) ->
    case Bin of
        <<L, Rest/binary>> when ?is_letter(L) ->
            read_word(Rest, <<StrAcc/binary, L>>, Line, Acc);
        <<D, Rest/binary>> when ?is_digit(D) ->
            read_word(Rest, <<StrAcc/binary, D>>, Line, Acc);
        _ ->
            b(Bin, Line, [{StrAcc, Line, StrAcc} | Acc])
    end.

%% -- common helpers --

collect(Bin, Pred, MaxLen) ->
    collect2(Bin, Pred, MaxLen, <<>>).

collect2(<<C, Rest/binary>> = Bin, Pred, N, Acc) when N >= 1 ->
    case Pred(C) of
        true  -> collect2(Rest, Pred, decrement(N), <<Acc/binary, C>>);
        false -> {Acc, Bin}
    end;
collect2(Rest, _Pred, _N, Acc) ->
    if Rest =:= <<>>,
       Acc =:= <<>> ->
            eof;
       true ->
            {Acc, Rest}
    end.

decrement(infinity) -> infinity;
decrement(N) when is_integer(N) -> N - 1.

is_oct_char(C) -> ?is_octal(C).
is_hex_char(C) -> ?is_digit(C) orelse ?is_a_to_f(C).

oct_to_integer(Bin) -> erlang:list_to_integer(binary_to_list(Bin), 8).
hex_to_integer(Bin) -> erlang:list_to_integer(binary_to_list(Bin), 16).

ok(Line, Acc) ->
    {ok, lists:reverse(Acc), Line}.

syntax_error(Reason, Line) ->
    %% FIXME: error spec?
    {error, {Line, ?MODULE, {syntax_error, Reason}}, Line}.

before(Bin) -> {before, get_line(Bin, <<>>)}.

get_line(<<"\n", _/binary>>, Acc) -> Acc;
get_line(<<C, Rest/binary>>, Acc) -> get_line(Rest, <<Acc/binary, C>>);
get_line(<<>>, <<>>) -> 'end-of-input';
get_line(<<>>, Acc) -> Acc.

%% -- bwd compat -- do not use these ----------------------------------
%% Deprecated!
%% Instead, to retrieve proto definitions,
%% use the option to_proto_defs
%% with gpb_compile:file or gpb_compile:string.
%% For exprotobuf, see also
%% https://github.com/bitwalker/exprotobuf/issues/114

%% Use the option to_proto_defs with gpb_compile:file/string instead.
string(S) ->
    binary(unicode:characters_to_binary(S)).
%% --^^--- bwd compat -- do not use these ----------------------------
