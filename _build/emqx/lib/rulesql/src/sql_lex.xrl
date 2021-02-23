%% -*- erlang -*-
Definitions.

W = \s\r\n
D = [0-9]
SD = ([-+]?{D}+)
A = [A-Za-z0-9,_{W}]
OS = (.*|[{W}]*)

Rules.

% erlang funcs
(fun[{W}]*\({A}*\){OS}*\->{OS}*end\.)               : match_fun(TokenLine, TokenChars).
(fun[{W}]+['A-Za-z0-9_]+:['A-Za-z0-9_]+\/[0-9]+\.)  : {token, {'STRING', TokenLine, TokenChars}}.

% strings
(\'([^\']*(\'\')*)*\')                              : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                        : {token, {'NAME', TokenLine, TokenChars}}.

% punctuation
(=|=~|~=|!=|<>|<|>|<=|>=)                           : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([\-\+\*\/\(\)\[\]\,\.]|(div))                      : {token, {list_to_atom(TokenChars), TokenLine}}.

% range
\[({SD}\.\.{SD})\]                                  :
    [Begin, End] = string:tokens(strip(TokenChars, TokenLen), ".."),
    IndexBegin = list_to_integer(Begin),
    IndexEnd = list_to_integer(End),
    {token, {'RANGE', TokenLine, {IndexBegin, IndexEnd}}}.

% names
[A-Za-z][A-Za-z0-9_\$@~]*                           : match_any(TokenChars, TokenLen, TokenLine, ?KEYWORDS).

% numbers
({D}+)                                              : {token, {'INTNUM', TokenLine, TokenChars}}.
(({D}+[\.]?{D}+)([eE][+-]?{D}+)?[fFdD]?)            : {token, {'APPROXNUM', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                       : skip_token.    %% white space

% comments
((\-\-).*[\n])                                      : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

-export([reserved_keywords/0]).

-include("sql_lex.hrl").

reserved_keywords() -> [T || {_, T} <- ?KEYWORDS].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P, T} | TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match, [_]} ->
            {token, {T, TokenLine}};
        nomatch ->
            match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.

match_fun(TokenLine, TokenChars) ->
    {match, [MatchedFunStr]} = re:run(TokenChars, "^fun.*end\\.", [ungreedy, dotall, {capture, all, list}]),
    {token, {'STRING', TokenLine, MatchedFunStr}, string:sub_string(TokenChars, length(MatchedFunStr) + 1)}.


strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).
