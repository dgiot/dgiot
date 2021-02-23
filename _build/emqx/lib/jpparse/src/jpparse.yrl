%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email chatterjee@bluewin.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 jp
 args
 leaf
.

Terminals
 STRING
 ':'
 '::'
 '['
 '{'
 ','
 ']'
 '}'
 '#'
 '('
 ')'
.

Rootsymbol jp.

Left 500 ':'.
Left 500 '::'.
Left 300 '['.
Left 300 '{'.
Left 300 '#'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jp -> '$empty'          : 'empty'.
jp -> leaf              : '$1'.
jp -> jp '#' leaf       : {'#', '$3', '$1'}.
jp -> jp ':' jp         : {':', flat('$3'), flat('$1')}.
jp -> jp '::' jp        : {'::', flat('$3'), flat('$1')}.
jp -> jp '[' args ']'   : {'[]', '$1', flat('$3')}.
jp -> jp '{' args '}'   : {'{}', '$1', flat('$3')}.
jp -> leaf '(' args ')' : {'fun', '$1', flat('$3')}.

args -> jp              : ['$1'].
args -> jp ',' args     : ['$1' | '$3'].

leaf -> STRING          : unwrap('$1').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

% parser and compiler interface
-export([parsetree/1, parsetree_with_tokens/1]).

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) when is_list(X) ->
    case catch list_to_integer(X) of
        {'EXIT', _} -> list_to_binary(X);
        Integer -> Integer
    end.

flat(['empty']) -> [];
flat(Other) -> Other.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, tuple()}.
parsetree(JPath) ->
   case parsetree_with_tokens(JPath) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {tuple(), list()}}.
parsetree_with_tokens(JPath) when is_binary(JPath) ->
    parsetree_with_tokens(binary_to_list(JPath));
parsetree_with_tokens([]) -> {parse_error, {not_a_valid_json_path, []}};
parsetree_with_tokens(JPath) when is_list(JPath) ->
    catch application:start(jpparse),
    case jsonpath_lex:string(JPath) of
        {ok, Toks, _} ->
            case jpparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error, {Line, Module, Message}} ->
                    {parse_error,
                     {Line, lists:flatten(Module:format_error(Message)), Toks}}
            end;
        LexErrorInfo -> {lex_error, jsonpath_lex:format_error(LexErrorInfo)}
    end;
parsetree_with_tokens(SomethingElse) ->
    {parse_error, {not_a_valid_json_path, SomethingElse}}.

%%-----------------------------------------------------------------------------
