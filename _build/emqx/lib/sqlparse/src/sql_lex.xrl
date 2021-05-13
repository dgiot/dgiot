%% -----------------------------------------------------------------------------
%%
%% sql_lex.xrl: SQL - lexer definition.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Definitions.

W = \s\r\n
A = [A-Za-z0-9,_{W}]
OS = (.*|[{W}]*)

Rules.

% erlang funcs
(fun[{W}]*\({A}*\){OS}*\->{OS}*end\.)               : match_fun(TokenLine, TokenChars).
(fun[{W}]+['A-Za-z0-9_]+:['A-Za-z0-9_]+\/[0-9]+\.)  : {token, {'STRING', TokenLine, TokenChars}}.

% database link
(\"@[A-Za-z0-9_\$#\.@]+\")                          : {token, {'DBLINK', TokenLine, TokenChars}}.

% strings
(\'([^\']*(\'\')*)*\')                              : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                        : {token, {'NAME', TokenLine, TokenChars}}.

% hint
((\/\*)[^\*\/]*(\*\/))                              : {token, {'HINT', TokenLine, TokenChars}}.

% punctuation
(=~|!=|\^=|<>|<|>|<=|>=)                            : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([=\|\-\+\*\/\(\)\,\.\;]|(\|\|)|(:=)|(=>)|(div))    : {token, {list_to_atom(TokenChars), TokenLine}}.

% JSON
(\|[:{\[#]([^\|]*)+\|)                              : parse_json(TokenLine, TokenChars).

% names
[A-Za-z][A-Za-z0-9_\.\$@~]*                         : match_any(TokenChars, TokenLen, TokenLine, ?TOKENPATTERNS).

% parameters
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)                   : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
([0-9]+)                                            : {token, {'INTNUM', TokenLine, TokenChars}}.
((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))([eE][+-]?[0-9]+)?[fFdD]?)
                                                    : {token, {'APPROXNUM', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                       : skip_token.    %% white space

% comments
((\-\-).*[\n])                                      : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% sqllexer.erl: SQL - lexer.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-export([reserved_keywords/0]).

-define(NODEBUG, true).

-include("sql_lex.hrl").

reserved_keywords() -> [T || {_, T} <- ?KEYWORDS].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P, T} | TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match, [_]} ->
            if (T =:= 'FUNS') orelse
                (T =:= 'UFUN') ->
                {token, {T, TokenLine, list_to_atom(TokenChars)}};
                true -> {token, {T, TokenLine}}
            end;
        nomatch -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.

match_fun(TokenLine, TokenChars) ->
    {match, [MatchedFunStr]} = re:run(TokenChars, "^fun.*end\\.", [ungreedy, dotall, {capture, all, list}]),
    {token, {'STRING', TokenLine, MatchedFunStr}, string:sub_string(TokenChars, length(MatchedFunStr) + 1)}.

parse_json(TokenLine, TokenCharsIn) ->
    TokenChars = string:substr(TokenCharsIn, 2, length(TokenCharsIn) - 2),
    ?D("TokenChars=~p", [TokenChars]),
    parse_json(TokenLine, TokenChars, "", "", 0).

parse_json(TokenLine, [], Json, PushBack, 0) ->
    {token, {'JSON', TokenLine, lists:reverse(Json)}, PushBack};
parse_json(_TokenLine, [], Json, _PushBack, Open) ->
    {error, lists:flatten(
        io_lib:format("malformed JSON path '~s',"
        " ~p bracket(s) not closed"
            , [lists:reverse(Json), Open]))};
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when T =:= $]; T =:= $}; T =:= $) ->
    ?D("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    if Open > 0 ->
        parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open - 1);
        true ->
            parse_json(TokenLine, [], Json, [T | TokenChars], Open)
    end;
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when T =:= $[; T =:= ${; T =:= $( ->
    ?D("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open + 1);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when Open > 0 ->
    ?D("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when (Open =:= 0) andalso (
    (T =:= $:) orelse (T =:= $#)
        orelse (T =:= $_) orelse (T =:= $.)
        orelse ((T >= $A) andalso (T =< $Z))
        orelse ((T >= $a) andalso (T =< $z))
        orelse ((T >= $0) andalso (T =< $9))
) ->
    ?D("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T | Json], PushBack, Open);
parse_json(TokenLine, [T | TokenChars], Json, PushBack, Open)
    when (Open =:= 0) ->
    ?D("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    {NewTokenChars, NewJson, NewPushBack} =
        case T of
            T when [T] =:= " ";T =:= $\n;T =:= $\t;T =:= $\r -> % white space characters
                case re:run(TokenChars, "^([[:space:]]*)(.*)", [{capture, [1, 2], list}, dotall]) of
                    {match, [WhiteSpace, [F | _] = Rest]} when F =:= $[;F =:= ${;F =:= $( ->
                        {Rest, lists:reverse(WhiteSpace) ++ [T | Json], PushBack};
                    _ ->
                        {[], Json, [T | TokenChars]}
                end;
            _ -> {[], Json, [T | TokenChars]}
        end,
    ?D("NewTokenChars=~p NewJson=~p NewPushBack=~p", [NewTokenChars, NewJson, NewPushBack]),
    parse_json(TokenLine, NewTokenChars, NewJson, NewPushBack, Open).
