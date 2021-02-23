%%% Author  : Robert Virding
%%% Purpose : Token definitions for Erlang.

Definitions.

D = [0-9]
U = [A-Z]
L = [a-z]
A = ({U}|{L}|{D}|_|@)
WS = ([\000-\s])

Rules.

{L}{A}*             : tokenize_atom(TokenChars, TokenLine).
'(\\\^.|\\.|[^'])*' : tokenize_atom(escape(unquote(TokenChars, TokenLen)), TokenLine).
"(\\\^.|\\.|[^"])*" : {token, {string, TokenLine, escape(unquote(TokenChars, TokenLen))}}.
{D}+                : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
[\#\[\]}{,+-]       : {token, {list_to_atom(TokenChars), TokenLine}}.
(<<|>>|=>)          : {token, {list_to_atom(TokenChars), TokenLine}}.
\.                  : {token, {dot, TokenLine}}.
/                   : {token, {'/', TokenLine}}.
{WS}+               : skip_token.

Erlang code.

-export([terms/1]).

terms(Tokens) ->
  terms(Tokens, []).

terms([{dot, _} = H], Buffer) ->
  [buffer_to_term([H|Buffer])];
terms([{dot, _} = H|T], Buffer) ->
  [buffer_to_term([H|Buffer])|terms(T, [])];
terms([H|T], Buffer) ->
  terms(T, [H|Buffer]).

buffer_to_term(Buffer) ->
  {ok, Term} = erl_parse:parse_term(lists:reverse(Buffer)),
  Term.

unquote(TokenChars, TokenLen) ->
  lists:sublist(TokenChars, 2, TokenLen - 2).

tokenize_atom(TokenChars, TokenLine) ->
  try list_to_existing_atom(TokenChars) of
    Atom -> {token, {atom, TokenLine, Atom}}
  catch
    error:badarg -> {error, "illegal atom " ++ TokenChars}
  end.

escape([$\\|Cs]) ->
  do_escape(Cs);
escape([C|Cs]) ->
  [C|escape(Cs)];
escape([]) -> [].

do_escape([O1,O2,O3|S]) when
    O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
  [(O1*8 + O2)*8 + O3 - 73*$0|escape(S)];
do_escape([$^,C|Cs]) ->
  [C band 31|escape(Cs)];
do_escape([C|Cs]) when C >= $\000, C =< $\s ->
  escape(Cs);
do_escape([C|Cs]) ->
  [escape_char(C)|escape(Cs)].

escape_char($n) -> $\n;       %\n = LF
escape_char($r) -> $\r;       %\r = CR
escape_char($t) -> $\t;       %\t = TAB
escape_char($v) -> $\v;       %\v = VT
escape_char($b) -> $\b;       %\b = BS
escape_char($f) -> $\f;       %\f = FF
escape_char($e) -> $\e;       %\e = ESC
escape_char($s) -> $\s;       %\s = SPC
escape_char($d) -> $\d;       %\d = DEL
escape_char(C) -> C.
