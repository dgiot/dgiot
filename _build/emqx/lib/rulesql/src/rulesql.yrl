%% -*- erlang -*-
Header
"%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved."
"%% @private"
"%% @Author Shawn"
"%% @Email liuxy@emqx.io".

Nonterminals
  sql
  query_exp
  from_clause
  from_column
  from_column_commalist
  where_clause
  case_when_exp
  case_when_opt_as_exp
  case_when_then
  case_when_then_list
  event_exp
  selection
  select_field
  select_field_commalist
  do_clause
  incase_clause
  else
  scalar_exp
  scalar_opt_as_exp
  scalar_opt_as_exp_1
  scalar_opt_as_exp_2
  search_condition
  predicate
  in_predicate
  comparison_predicate
  not_in
  unary_add_or_subtract
  scalar_exp_commalist
  computed_var
  name_or_path_ref
  path_ref
  path_ref_r
  index_path_ref
  function_ref
  fun_args
  fun_arg
  literal
  index_ref
  list_ref
  list_elem
  list_elems
  range_ref
  range_literal.

Terminals
  STRING
  NAME
  COMPARISON
  RANGE
  INTNUM
  APPROXNUM
  AND
  AS
  CASE
  ELSE
  FROM
  IN
  NOT
  OR
  SELECT
  WHEN
  THEN
  FOREACH
  INCASE
  END
  DO
  WHERE
  '('
  ')'
  '['
  ']'
  '*'
  '+'
  ','
  '-'
  '.'
  '/'
  'div'.

Rootsymbol sql.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% precedence
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Left        100 OR.
Left        200 AND.
Left        300 NOT.
Nonassoc    400 COMPARISON.
Left        500 '+' '-'.
Left        600 '*' '/' 'div'.
Left        700 unary_add_or_subtract.

sql -> query_exp : '$1'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query_exp -> SELECT selection  event_exp :
    {select, [{fields, '$2'}] ++ '$3'}.

query_exp -> FOREACH selection event_exp :
    {foreach, [{fields, '$2'}] ++ '$3'}.
query_exp -> FOREACH selection do_clause event_exp :
    {foreach, [{fields, '$2'}, '$3'] ++ '$4'}.
query_exp -> FOREACH selection do_clause incase_clause event_exp :
    {foreach, [{fields, '$2'}, '$3', '$4'] ++'$5'}.
query_exp -> FOREACH selection incase_clause event_exp :
    {foreach, [{fields, '$2'}, '$3'] ++'$4'}.

do_clause -> DO selection: {'do', '$2'}.

selection -> select_field_commalist : '$1'.

select_field_commalist ->                            select_field :         '$1'.
select_field_commalist -> select_field_commalist ',' select_field : '$1' ++ '$3'.

select_field -> case_when_opt_as_exp : ['$1'].
select_field -> scalar_opt_as_exp    : ['$1'].
select_field -> '*'                  : ['*'].

case_when_opt_as_exp -> case_when_exp                         : '$1'.
case_when_opt_as_exp -> case_when_exp    name_or_path_ref     : {as, '$1', '$2'}.
case_when_opt_as_exp -> case_when_exp AS name_or_path_ref     : {as, '$1', '$3'}.

case_when_exp -> CASE                   case_when_then_list      END : {'case', <<>>, '$2', {}}.
case_when_exp -> CASE                   case_when_then_list else END : {'case', <<>>, '$2', '$3'}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list      END : {'case', '$2', '$3', {}}.
case_when_exp -> CASE scalar_opt_as_exp case_when_then_list else END : {'case', '$2', '$3', '$4'}.
case_when_exp -> '(' case_when_exp ')'                               : '$2'.

case_when_then_list -> case_when_then                     : ['$1'].
case_when_then_list -> case_when_then case_when_then_list : ['$1'|'$2'].

case_when_then -> WHEN search_condition THEN scalar_opt_as_exp : {'$2', '$4'}.

incase_clause -> INCASE search_condition : {incase, '$2'}.

where_clause -> WHERE search_condition : {where, '$2'}.

else -> ELSE scalar_opt_as_exp : '$2'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FROM and WHERE
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

event_exp -> from_clause :
['$1', {where, {}}].

event_exp -> from_clause where_clause :
['$1', '$2'].

from_clause -> FROM from_column_commalist : {from, '$2'}.

from_column -> NAME           : [unwrap_bin('$1')].

from_column_commalist ->                           from_column :        '$1'.
from_column_commalist -> from_column_commalist ',' from_column : '$1'++ '$3'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% search conditions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search_condition -> search_condition OR  search_condition : {'or',  '$1', '$3'}.
search_condition -> search_condition AND search_condition : {'and', '$1', '$3'}.
search_condition -> NOT search_condition                  : {'not', '$2'}.
search_condition -> '(' search_condition ')'              : '$2'.
search_condition -> predicate                             : '$1'.

predicate -> comparison_predicate : '$1'.
predicate -> in_predicate         : '$1'.

comparison_predicate -> scalar_opt_as_exp                            : '$1'.

in_predicate -> scalar_exp     IN '(' scalar_exp_commalist ')' :         {in, '$1', {list, '$4'}}.
in_predicate -> scalar_exp not_in '(' scalar_exp_commalist ')' : {'not', {in, '$1', {list, '$4'}}}.

not_in -> NOT IN : 'not in'.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scalar expressions
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scalar_opt_as_exp -> scalar_opt_as_exp_1                     : '$1'.
scalar_opt_as_exp -> scalar_opt_as_exp_1 AS name_or_path_ref : {as, '$1', '$3'}.
scalar_opt_as_exp -> scalar_opt_as_exp_2 : '$1'.

scalar_opt_as_exp_1 -> scalar_exp                       : '$1'.
scalar_opt_as_exp_1 -> scalar_exp COMPARISON scalar_exp : {unwrap('$2'), '$1', '$3'}.

scalar_opt_as_exp_2 -> scalar_exp    name_or_path_ref : {as, '$1', '$2'}.
scalar_opt_as_exp_2 -> scalar_exp AS name_or_path_ref : {as, '$1', '$3'}.

scalar_exp -> scalar_exp '+'    scalar_exp : {'+','$1','$3'}.
scalar_exp -> scalar_exp '-'    scalar_exp : {'-','$1','$3'}.
scalar_exp -> scalar_exp '*'    scalar_exp : {'*','$1','$3'}.
scalar_exp -> scalar_exp '/'    scalar_exp : {'/','$1','$3'}.
scalar_exp -> scalar_exp 'div'  scalar_exp : {'div','$1','$3'}.
scalar_exp -> unary_add_or_subtract scalar_exp : {'$1','$2'}.
scalar_exp -> literal                       : '$1'.
scalar_exp -> computed_var                  : '$1'.
scalar_exp -> function_ref                  : '$1'.
scalar_exp -> '(' scalar_exp ')'            : '$2'.

unary_add_or_subtract -> '+' : '+'.
unary_add_or_subtract -> '-' : '-'.

scalar_exp_commalist ->                          scalar_opt_as_exp :         ['$1'].
scalar_exp_commalist -> scalar_exp_commalist ',' scalar_opt_as_exp : '$1' ++ ['$3'].

computed_var -> name_or_path_ref :   '$1'.
computed_var -> range_ref       :   '$1'.
computed_var -> list_ref        :   '$1'.

name_or_path_ref -> NAME             : unwrap_var('$1').
name_or_path_ref -> path_ref         : '$1'.

path_ref -> path_ref '.' path_ref_r             : merge_path('$1', '$3').
path_ref -> NAME '.' path_ref_r                 : merge_path(unwrap_var('$1'), '$3').
path_ref -> NAME index_path_ref                 : merge_path(unwrap_var('$1'), '$2').

index_path_ref -> index_ref                 : '$1'.
index_path_ref -> index_ref index_path_ref  : merge_path('$1', '$2').

path_ref_r -> path_ref_r '.' path_ref_r     : merge_path('$1', '$3').
path_ref_r -> path_ref_r index_path_ref     : merge_path('$1', '$2').
path_ref_r -> NAME                          : unwrap_var('$1').
path_ref_r -> INTNUM                        : unwrap_var('$1').
path_ref_r -> STRING                        : unwrap_var('$1').

index_ref -> '[' name_or_path_ref ']'   : {'index', '$2'}.
index_ref -> '[' INTNUM ']'             : {'index', unwrap_index('+','$2')}.
index_ref -> '[' unary_add_or_subtract INTNUM ']' : {'index', unwrap_index('$2','$3')}.

list_elems -> list_elem                 : {cons, '$1', 'nil'}.
list_elems -> list_elem ',' list_elems  : {cons, '$1', '$3'}.
list_elems -> '$empty'                  : nil.

list_elem -> computed_var               : '$1'.
list_elem -> literal                    : '$1'.

list_ref -> '[' list_elems ']'           : {list, trans_list_ref('$2')}.

range_ref ->  name_or_path_ref  RANGE   : {'get_range', unwrap_range('$2'), '$1'}.
range_literal -> RANGE                  : {'range', unwrap_range('$1')}.

function_ref -> name_or_path_ref   '('                ')' : {'fun', '$1', []}.
function_ref -> name_or_path_ref   '(' fun_args       ')' : {'fun', '$1', make_list('$3')}.

fun_args -> fun_arg              : ['$1'].
fun_args -> fun_arg ',' fun_args : ['$1' | '$3'].

fun_arg -> '(' fun_arg ')'               : '$2'.
fun_arg -> literal                       : '$1'.
fun_arg -> case_when_exp                 : '$1'.
fun_arg -> computed_var                  : '$1'.
fun_arg -> fun_arg '*'   fun_arg         : {'*',  '$1','$3'}.
fun_arg -> fun_arg '+'   fun_arg         : {'+',  '$1','$3'}.
fun_arg -> fun_arg '-'   fun_arg         : {'-',  '$1','$3'}.
fun_arg -> fun_arg '/'   fun_arg         : {'/',  '$1','$3'}.
fun_arg -> fun_arg 'div' fun_arg         : {'div','$1','$3'}.
fun_arg -> fun_arg COMPARISON fun_arg    : {unwrap('$2'), '$1', '$3'}.
fun_arg -> function_ref                  : '$1'.
fun_arg -> unary_add_or_subtract fun_arg : {'$1', '$2'}.

literal -> STRING    : unwrap_const('$1').
literal -> INTNUM    : unwrap_const('$1').
literal -> APPROXNUM : unwrap_const('$1').
literal -> range_literal  :   '$1'.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

% parser and compiler interface
-export([
    is_reserved/1,
    parsetree/1,
    parsetree_with_tokens/1
]).

-include("sql_lex.hrl").

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({X, _}) when is_atom(X) -> atom_to_list(X);
unwrap({_, _, X}) -> X;
unwrap(X) -> X.

unwrap_bin({X, _}) when is_atom(X) -> atom_to_binary(X, unicode);
unwrap_bin({_, _, X}) when is_list(X) -> list_to_binary([unquote(X)]);
unwrap_bin({_, _, X}) when is_atom(X) -> atom_to_binary(X, unicode).

unwrap_var({'NAME', _, "true"}) ->
    const(true);
unwrap_var({'NAME', _, "false"}) ->
    const(false);
unwrap_var(Token) ->
    case unwrap_bin(Token) of
        <<"*">> -> '*';
        Bin -> var(Bin)
    end.

unwrap_range({'RANGE', _, {IndexBegin, IndexEnd}}) ->
    {IndexBegin, IndexEnd}.

unwrap_const({'STRING', _, X}) when is_list(X) ->
    const(list_to_binary(unquote(X)));
unwrap_const({'INTNUM', _, X}) when is_list(X) ->
    const(list_to_integer(X));
unwrap_const({'APPROXNUM', _, X}) when is_list(X) ->
    const(list_to_float(X)).

unwrap_index('+', {'INTNUM', _, X}) when X =:= "0" ->
    const(head);
unwrap_index('-', {'INTNUM', _, X}) when X =:= "0" ->
    const(tail);
unwrap_index('+', {'INTNUM', _, X}) when is_list(X) ->
    const(list_to_integer(X));
unwrap_index('-', {'INTNUM', _, X}) when is_list(X) ->
    const(-list_to_integer(X)).

var(V) -> {var, V}.

const(V) -> {const, V}.

make_list(L) when is_list(L) -> L;
make_list(L) -> [L].

unquote(Str) ->
    string:trim(string:trim(Str, both, "'"), both, "\"").

merge_path({var, V1}, {var, V2}) ->
    {path, [{key, V1}, {key, V2}]};
merge_path({var, V1}, {index, _} = I) ->
    {path, [{key, V1}, I]};
merge_path({index, _} = I, {var, V1}) ->
    {path, [I, {key, V1}]};
merge_path({index, _} = I1, {index, _} = I2) ->
    {path, [I1, I2]};
merge_path({var, V}, {path, Path}) ->
    {path, [{key, V} | Path]};
merge_path({index, _} = I, {path, Path}) ->
    {path, [I | Path]};
merge_path({path, Path}, {var, V}) ->
    {path, Path ++ [{key, V}]};
merge_path({path, Path}, {index, _} = I) ->
    {path, Path ++ [I]};
merge_path({path, Path1}, {path, Path2}) ->
    {path, Path1 ++ Path2}.

trans_list_ref(nil) ->
    [];
trans_list_ref({cons, Head, Tail}) ->
    [Head] ++ trans_list_ref(Tail).

%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Sql) ->
    ?D("Start~n Sql: ~p~n", [Sql]),
    case parsetree_with_tokens(Sql) of
        {ok, {ParseTree, _Tokens}} ->
            ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            {ok, ParseTree};
        Error -> Error
    end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Sql0) ->
    Sql = re:replace(Sql0, "(^[ \r\n]+)|([ \r\n]+$)", "",
        [global, {return, list}]),
    ?D("Start~n Sql: ~p~n", [Sql]),
    SqlClean = unicode:characters_to_list(
                    string:replace(Sql, "\n/", "", all)),
    case sql_lex:string(string:trim(SqlClean)) of
        {ok, Toks, _} ->
            case parse(Toks) of
                {ok, PTree} ->
                    ?D("~n ParseTree: ~p~n Tokens: ~p~n", [PTree, Toks]),
                    {ok, {PTree, Toks}};
                {error, {N, ?MODULE, ErrorTerms}} ->
                    {parse_error, {lists:flatten(
                        [integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error, Error} -> {parse_error, {Error, Toks}}
            end;
        {error, Error, _} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
        sql_lex:reserved_keywords()).
