%% -----------------------------------------------------------------------------
%%
%% sqlparse_format_pretty.erl: SQL - creating a pretty formatted version
%%                                   of the SQL statement.
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

-module(sqlparse_format_pretty).

-export([
    finalize/2,
    fold/5,
    init/1
]).

-define(NODEBUG, true).

-include("sqlparse_format_pretty.hrl").

-type layout_opts() :: #lopts{}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up optional parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(LOpts :: list()|map()) -> LOpts :: layout_opts().
init(LOpts)
    when is_map(LOpts) ->
    ?D("Start~n LOpts: ~p~n", [LOpts]),
    RT = init(maps:to_list(LOpts)),
    ?D("End~n LOpts: ~p~n", [RT]),
    RT;
init(LOpts)
    when is_list(LOpts) ->
    ?D("Start~n LOpts: ~p~n", [LOpts]),
    RT = process_lopts(#lopts{}, LOpts),
    ?D("End~n LOpts: ~p~n", [RT]),
    RT.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Postprocessing of the resulting SQL statement.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize(LOpts :: layout_opts(), string()|tuple()) -> binary()|tuple().
finalize(LOpts, CtxIn)
    when is_record(LOpts, lopts), is_list(CtxIn) ->
    ?D("Start~n LOpts: ~p~n CtxIn: ~p~n", [LOpts, CtxIn]),
    {ok, MP} = re:compile("[\(][\040]+[\(]"),
    CtxParentheses =
        re:replace(re:replace(CtxIn, MP, "((", [global, {return, list}]), MP,
            "((", [global, {return, list}]),
    CtxBetween = case LOpts#lopts.case_keyword of
                     lower -> re:replace(CtxParentheses,
                         "[\040]between[\040][\040]+[\(]", " between (",
                         [{return, list}]);
                     upper -> re:replace(CtxParentheses,
                         "[\040]BETWEEN[\040][\040]+[\(]", " BETWEEN (",
                         [{return, list}]);
                     _ -> re:replace(CtxParentheses,
                         "[\040]Between[\040][\040]+[\(]", " Between (",
                         [{return, list}])
                 end,
    CtxAnd = case LOpts#lopts.case_keyword of
                 lower -> re:replace(CtxBetween,
                     "[\040]and[\040][\040]+[\(]", " and (",
                     [{return, list}]);
                 upper -> re:replace(CtxBetween,
                     "[\040]AND[\040][\040]+[\(]", " AND (",
                     [{return, list}]);
                 _ -> re:replace(CtxBetween,
                     "[\040]And[\040][\040]+[\(]", " And (",
                     [{return, list}])
             end,
    CtxBL = list_to_binary(break_lines(LOpts, CtxAnd)),
    ?D("~n CtxOut: ~p~n", [CtxBL]),
    CtxBL.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Layout methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(LOpts :: layout_opts(), FunState :: tuple(),
    Ctx :: string()|tuple(), PTree :: list()|tuple(), FoldState :: tuple()) ->
    Ctx :: tuple()|none().
fold(_LOpts, _FunState, Ctx, _PTree, {"(", Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_commalist & ref_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {Rule, Step} = _FoldState)
    when Rule == column_commalist;Rule == ref_commalist ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {Stmnt, Clause, RuleG} = sqlparse_fold:get_stmnt_clause_curr(FunState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case {Stmnt, RuleG} of
                     {S, R} when S == view_def;R == table_constraint_def ->
                         " (";
                     _ -> lists:append([
                         " (",
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState)
                     ])
                 end
             ]);
             'end' -> Ctx ++
             case {Stmnt, Clause, RuleG} of
                 {S, C, R} when S == view_def;C == where_clause;
                                R == table_constraint_def ->
                     ")";
                 _ -> lists:append([
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     ")"
                 ])
             end
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_op
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Op, AnyAllSome, _SubQuery} = _PTree,
    {all_or_any_op, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_operator(LOpts, FunState, Op, false),
                 format_keyword(LOpts, AnyAllSome),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_Op, ScalarExp, _} = _PTree,
    {all_or_any_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alter_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'alter user', User, _Spec} = _PTree,
    {alter_user_def, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "alter user"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, User)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'alter user', Users, _Spec} = _PTree,
    {alter_user_def, Step} = _FoldState)
    when is_list(Users) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "alter user"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% anchor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {Anchor, '('} = _PTree, {anchor, Step} =
    _FoldState)
    when is_binary(Anchor) ->
    RT = case Step of
             start -> lists:append([Ctx, "(", binary_to_list(Anchor)]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {_Anchor, '('} = _PTree, {anchor, Step} =
    _FoldState) ->
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Value, Alias} = _PTree, {as, Step} = _FoldState)
    when is_binary(Value), is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Value),
                 " ",
                 format_identifier(LOpts, Alias)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {_Value, Alias} = _PTree, {as, Step} = _FoldState)
    when is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_identifier(LOpts, Alias)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explicit_as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Value, Alias} = _PTree, {explicit_as, Step} =
    _FoldState)
    when is_binary(Value), is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Value),
                 " AS ",
                 format_identifier(LOpts, Alias)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {_Value, Alias} = _PTree, {explicit_as, Step} =
    _FoldState)
    when is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " AS ",
                 format_identifier(LOpts, Alias)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assignment
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'=', Column, _ScalarOptAsExp} = _PTree,
    {assignment, Step, Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Column),
                 format_operator(LOpts, FunState, "=", false)
             ]);
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_table_element_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {base_table_element_commalist, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " (",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             'end' -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 ")"
             ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_and
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, ScalarExp = _PTree, {between_and, Step} =
    _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " and "),
                 format_identifier(LOpts, ScalarExp)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {between_and, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " and ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_between
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, ScalarExp = _PTree, {between_between, Step} =
    _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     format_keyword(LOpts, " between "),
                     format_identifier(LOpts, ScalarExp)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {between_between, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " between ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(LOpts, _FunState, Ctx, {between, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree, {between_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% binary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, Op = _PTree, {binary, Step} = _FoldState)
    when Op == 'and'; Op == 'or'; Op == 'div' ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_operator(LOpts, FunState, Op, false);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, Op = _PTree, {binary, Step} = _FoldState)
    when Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '||'; Op == '=';
         Op == '!='; Op == '^='; Op == '<>'; Op == '<'; Op == '>'; Op == '<=';
         Op == '>='; Op == ':=' ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_operator(LOpts, FunState, Op, false);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) andalso is_binary(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Value1),
                 format_operator(LOpts, FunState, Op, false),
                 format_identifier(LOpts, Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op) andalso is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Value1),
                 format_operator(LOpts, FunState, Op, false),
                 format_identifier(LOpts, Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, Value1, _Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Value1),
                 format_operator(LOpts, FunState, Op, false)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, Value1, _Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value1) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     format_identifier(LOpts, Value1),
                     format_operator(LOpts, FunState, Op, false)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, _Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 format_operator(LOpts, FunState, Op, false),
                 format_identifier(LOpts, Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, _Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' ->
                 lists:append([
                     Ctx,
                     format_operator(LOpts, FunState, Op, false),
                     format_identifier(LOpts, Value2)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'case'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {ScalarOptAsExpr, _CaseWhenThenList, Else} =
    _PTree, {'case', Step} = _FoldState)
    when is_list(_CaseWhenThenList) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "case"),
                 case ScalarOptAsExpr of
                     <<>> -> [];
                     B when is_binary(B) -> " " ++
                     format_identifier(LOpts, ScalarOptAsExpr);
                     _ -> " "
                 end
             ]);
             'end' -> lists:append([
                 Ctx,
                 case is_binary(Else) of
                     true -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState#fstate{indent_lvl =
                         FunState#fstate.indent_lvl + 1}),
                         format_keyword(LOpts, "else "),
                         format_identifier(LOpts, Else)
                     ]);
                     _ -> []
                 end,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "end")
             ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {check, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% close_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {close, _Cursor} = _PTree,
    {close_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_new_statement(LOpts, FunState, "close ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cols
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {cols, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " (",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {column, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {column, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Column, _DataType, _} = _PTree,
    {column_def, Step, Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, Column);
             {'end', other} -> lists:append([
                 Ctx,
                 ",",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {column_def_opt, Step, _Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 format_keyword(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {check, _Value} = _PTree,
    {column_def_opt, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " check(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {default, _Value} = _PTree,
    {column_def_opt, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " default ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {ref, _Value} = _PTree,
    {column_def_opt, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% commit_statement & rollback_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {Type, Step} = _FoldState)
    when Type == commit_statement;Type == rollback_statement ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++
             format_new_statement(LOpts, FunState, atom_to_list(PTree));
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparison_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {_Op, {prior, ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate, Step} = _FoldState) when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "prior ", binary_to_list(ScalarExp1)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {_Op, {prior, _ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "prior ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {_Op, ScalarExp1, {prior, _ScalarExp2}} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(_LOpts, _FunState, Ctx, {Op, {prior, ScalarExp}} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), "prior ", binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {Op, {prior, _ScalarExp}} = _PTree,
    {comparison_predicate, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Op), "prior "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {Op, ScalarExp} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {Op, _ScalarExp} = _PTree,
    {comparison_predicate, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ atom_to_list(Op);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% connect_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'connect by', <<>>, SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "connect by"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, SearchCondition)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'connect by', NoCycle, SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "connect by "),
                 format_keyword(LOpts, NoCycle),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, SearchCondition)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'connect by', <<>>, _SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "connect by"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'connect by', NoCycle, _SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "connect by "),
                 format_keyword(LOpts, NoCycle),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'create index', CreateIndexFoldState, _IndexName, _TableAlias, _CreateIndexSpec, _CreateIndexNorm, _CreateIndexFilter} =
        _PTree, {create_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "create "),
                 case CreateIndexFoldState of
                     {} -> [];
                     _ -> format_keyword(LOpts, CreateIndexFoldState) ++ " "
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_filter & create_index_norm
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when Rule == filter_with;Rule == norm_with ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_name
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, IndexName = _PTree, {create_index_name, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "index"),
                 case IndexName of
                     {} -> [];
                     _ -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, IndexName)
                     ])
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {create_index_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec_column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {create_index_spec_column, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {create_index_spec_column, Step, Pos} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {create_index_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "on"),
                 case is_binary(PTree) of
                     true -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, PTree)
                     ]);
                     _ -> ?CHAR_NEWLINE ++ format_column_pos(LOpts, FunState)
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'create role', Value} = _PTree,
    {create_role_def, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "create role"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {create_table_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_new_statement(LOpts, FunState, "create");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {create_table_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " table"),
                 case is_binary(PTree) of
                     true -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, PTree)
                     ]);
                     _ -> ?CHAR_NEWLINE ++ format_column_pos(LOpts, FunState)
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'create user', User, _Identified, _UserFoldStateList} = _PTree,
    {create_user_def, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "create user"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 binary_to_list(User),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cur
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {cur, Cursor} = _PTree, {cur, Step} = _FoldState)
    when is_list(Cursor) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ Cursor;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {cursor_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx, format_new_statement(LOpts, FunState, "cursor "),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {cursor_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " is ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% data_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {{Value, Prec, Scale}, _DOpts} = _PTree,
    {data_type, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec), is_binary(Scale) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 format_data_type(LOpts, Value),
                 "(",
                 binary_to_list(Prec),
                 ",",
                 binary_to_list(Scale),
                 ")"
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {{Value, Prec}, _DOpts} = _PTree,
    {data_type, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 format_data_type(LOpts, Value),
                 "(",
                 binary_to_list(Prec),
                 ")"
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {Value, _DOpts} = _PTree, {data_type, Step} =
    _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 format_data_type(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {dblink, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% default
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {default, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ atom_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, PTree, {default, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {delete, Table, _, _} = _PTree,
    {delete_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "delete from"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {delete_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "delete from"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop cluster', Name, _DropExtensions} = _PTree,
    {drop_cluster_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop cluster "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_context_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop context', Name} = _PTree,
    {drop_context_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop context "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop database'} = _PTree,
    {drop_database_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 Ctx ++ format_new_statement(LOpts, FunState, "drop database");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop database link', Name, {}} = _PTree,
    {drop_database_link_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop database link "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop database link', Name, public} = _PTree,
    {drop_database_link_public_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState,
                     "drop public database link "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_directory_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop directory', Name} = _PTree,
    {drop_directory_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop directory "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {drop_extensions = _Rule, Step} = _FoldState)
    when is_atom(PTree);is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", format_keyword(LOpts, PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_function_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop function', Name} = _PTree,
    {drop_function_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop function "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop index', IndexName, _Table, _DropExtensions} =
    _PTree, {drop_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop index"),
                 case IndexName of
                     {} -> [];
                     _ -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, IndexName)
                     ])
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'drop index', IndexName, _Table} = _PTree,
    {drop_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop index"),
                 case IndexName of
                     {} -> [];
                     _ -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, IndexName)
                     ])
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {drop_index_from, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "from"),
                 case is_binary(PTree) of
                     true -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         format_identifier(LOpts, PTree)
                     ]);
                     _ -> []
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'drop materialized view', Name, _DropExtensions} = _PTree,
    {drop_materialized_view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState,
                     "drop materialized view "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop package', Name, {}} = _PTree,
    {drop_package_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop package "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop package', Name, body} = _PTree,
    {drop_package_body_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop package body "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_procedure_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop procedure', Name} = _PTree,
    {drop_procedure_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop procedure "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop profile', Name, _DropExtensions} = _PTree,
    {drop_profile_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop profile "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop role', Role} = _PTree,
    {drop_role_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop role"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Role)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_sequence_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop sequence', Name} = _PTree,
    {drop_sequence_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop sequence "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop synonym', Name, {}, _DropExtensions} = _PTree,
    {drop_synonym_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop synonym "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'drop synonym', Name, public, _DropExtensions} = _PTree,
    {drop_synonym_public_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState,
                     "drop public synonym "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'drop table', _Tables, _Exists, _DropExtensions, DropOpt} = _PTree,
    {drop_table_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop "),
                 case DropOpt of
                     [] -> [];
                     _ -> format_identifier(LOpts, DropOpt) ++ " "
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_tablespace_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'drop tablespace', Name, _DropExtensions} = _PTree,
    {drop_tablespace_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop tablespace "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_trigger_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop trigger', Name} = _PTree,
    {drop_trigger_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop trigger "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop type', Name, _DropExtensions} = _PTree,
    {drop_type_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop type "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop type body', Name} = _PTree,
    {drop_type_body_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop type body "),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop user', User, []} = _PTree,
    {drop_user_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop user "),
                 format_identifier(LOpts, User)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'drop user', User, [Cascade]} = _PTree,
    {drop_user_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop user "),
                 format_identifier(LOpts, User),
                 " ",
                 format_keyword(LOpts, Cascade)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'drop view', Table, _DropExtensions} = _PTree,
    {drop_view_def, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "drop view "),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'drop view', _Table, _DropExtensions} = _PTree,
    {drop_view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 Ctx ++ format_new_statement(LOpts, FunState, "drop view ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% else
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {else, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " else ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% existence_test
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {existence_test, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "exists"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {exists, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case PTree of
                     {} -> format_keyword(LOpts, "table");
                     exists ->
                         format_keyword(LOpts, "table if exists")
                 end,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fetch_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {fetch_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "fetch"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, [{as, {Type, _, _}, _} | _] = _PTree,
    {fields, _Step} = _FoldState)
    when Type == intersect;Type == minus; Type == union; Type == 'union all' ->
    Ctx;

fold(_LOpts, _FunState, Ctx, [{as, {select, _}, _} | _] = _PTree,
    {fields, _Step} = _FoldState) ->
    Ctx;
fold(_LOpts, _FunState, Ctx, [{select, _} | _] = _PTree, {fields, _Step} =
    _FoldState) ->
    Ctx;

fold(_LOpts, _FunState, Ctx, [{as, {'case', _, _, _}, _} | _] = _PTree,
    {fields, _Step} = _FoldState) ->
    Ctx;
fold(_LOpts, _FunState, Ctx, [{'case', _, _, _} | _] = _PTree, {fields, _Step} =
    _FoldState) ->
    Ctx;

fold(LOpts, FunState, Ctx, _PTree, {fields, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Table, Join} = _PTree, {from, Step, Pos} =
    _FoldState)
    when is_binary(Table), is_list(Join) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, PTree, {from, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, PTree)
             ]);
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {from, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {StmntPred_1, _ClausePred_1, RulePred_1} =
        sqlparse_fold:get_stmnt_clause_pred(FunState, 1),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 case StmntPred_1 == query_exp andalso
                     (RulePred_1 == select_left orelse
                         RulePred_1 == select_right) of
                     true -> [];
                     _ -> format_column_pos(LOpts, FunState)
                 end
             ]);
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {from, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "from")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {Type, Value} = _PTree, {fun_arg, Step, _Pos} =
    _FoldState)
    when (Type == all orelse Type == distinct) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, Type),
                 " ",
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {Type, _Value} = _PTree, {fun_arg, Step, _Pos} =
    _FoldState)
    when Type == all; Type == distinct ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, Type),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, _FunState, Ctx, PTree, {fun_arg, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {fun_arg, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {fun_arg_commalist, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'fun', Name, []} = _PTree, {function_ref, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {Stmnt, _, _} = sqlparse_fold:get_stmnt_clause_curr(FunState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case Stmnt of
                     plsql_body -> format_column_pos(LOpts, FunState);
                     procedure_call ->
                         ?CHAR_NEWLINE ++ format_column_pos(LOpts, FunState);
                     _ -> []
                 end,
                 format_identifier(LOpts, Name),
                 "()"
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'fun', Name, _} = _PTree, {function_ref, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {Stmnt, _, _} = sqlparse_fold:get_stmnt_clause_curr(FunState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case Stmnt of
                     plsql_body -> format_column_pos(LOpts, FunState);
                     procedure_call ->
                         ?CHAR_NEWLINE ++ format_column_pos(LOpts, FunState);
                     _ -> []
                 end,
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {as, {{'fun', _, _}, JSON, []}, Alias} = _PTree,
    {function_ref, Step} = _FoldState)
    when is_tuple(JSON) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_identifier(LOpts, Alias)]
             );
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goto
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {goto, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "goto ", PTree]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'grant connect' & 'revoke connect'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Value1, Value2} = _PTree, {Rule, Step} = _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(Value1) andalso is_atom(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 format_keyword(LOpts, " through "),
                 format_keyword(LOpts, Value1),
                 " ",
                 format_keyword(LOpts, Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 format_keyword(LOpts, " through "),
                 format_keyword(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {{_Type, _Roles}, ProxyAuthReq} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 format_keyword(LOpts, " through ")
             ]);
             'end' -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, ProxyAuthReq)
             ])
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {_Type, _Roles} = _PTree, {Rule, Step} = _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 format_keyword(LOpts, " through ")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grant_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {grant_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "grant"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {grantee, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "to"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee_revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {grantee_revokee, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_keyword(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, PTree, {grantee_revokee, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {'identified by', Name, Password} = _PTree,
    {grantee_revokee, Step} = _FoldState)
    when is_binary(Name);is_binary(Password) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, Name),
                 format_identifier(LOpts, " identified by "),
                 binary_to_list(Password)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% group_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {group_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "group by"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% having
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {having, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "having"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {having, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "having"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hints
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {hints, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 binary_to_list(PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% identified & spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified by' andalso is_binary(Value) andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Type),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 binary_to_list(Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified extern' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState#fstate{indent_lvl =
                     FunState#fstate.indent_lvl - 1}),
                     format_keyword(LOpts, 'identified externally'),
                     case is_binary(Value) of
                         true -> lists:append([
                             format_keyword(LOpts, " as"),
                             ?CHAR_NEWLINE,
                             format_column_pos(LOpts, FunState),
                             binary_to_list(Value)
                         ]);
                         _ -> []
                     end
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified globally' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Type),
                 case is_binary(Value) of
                     true -> lists:append([
                         format_keyword(LOpts, " as"),
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState),
                         binary_to_list(Value)
                     ]);
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_in
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, _PTree, {in_in, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " in (");
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {in, ScalarExp, _} = _PTree,
    {in_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {insert, Table, _, _, _} = _PTree,
    {insert_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "insert into"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {insert_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "insert into"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% into
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {into, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "into"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {JoinType, JoinRef, _JoinOnOrUsingClause} = _PTree,
    {join, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     case JoinType of
                         join_inner -> format_keyword(LOpts, "inner join");
                         _ -> format_keyword(LOpts, JoinType)
                     end,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     format_identifier(LOpts, JoinRef)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {JoinType, JoinRef} = _PTree,
    {join, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     case JoinType of
                         cross_join -> format_keyword(LOpts, "cross join");
                         natural_join -> format_keyword(LOpts, "natural join");
                         natural_inner_join ->
                             format_keyword(LOpts, "natural inner join")
                     end,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     format_identifier(LOpts, JoinRef)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {JoinType, _JoinRef, _JoinOnOrUsingClause} =
    _PTree, {join, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 case JoinType of
                     join_inner -> format_keyword(LOpts, "inner join");
                     _ -> format_keyword(LOpts, JoinType)
                 end,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {JoinType, _JoinRef} = _PTree,
    {join, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 case JoinType of
                     cross_join -> format_keyword(LOpts, "cross join");
                     natural_join -> format_keyword(LOpts, "natural join");
                     natural_inner_join ->
                         format_keyword(LOpts, "natural inner join")
                 end,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% jpparse
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {jpparse, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == "full join";
         PTree == "left join";
         PTree == "natural full join";
         PTree == "natural join";
         PTree == "natural left join";
         PTree == "natural right join";
         PTree == "right join" ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, PTree),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == "full_outer join";
         PTree == "left_outer join";
         PTree == "natural full_outer join";
         PTree == "natural left_outer join";
         PTree == "natural right_outer join";
         PTree == "right_outer join" ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, string:replace(PTree, "_", " ")),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == intersect;
         PTree == minus;
         PTree == union;
         PTree == 'union all' ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, PTree),
                 ?CHAR_NEWLINE
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == "cascade";
         PTree == "with check option" ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_escape
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {like_escape, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " escape "),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {like_escape, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " escape ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_like
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {like_like, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " like "),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {Type, _, _} = _PTree, {like_like, Step} =
    _FoldState)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " like (");
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {like_like, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " like ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {like, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree,
    {like_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% materialized
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'materialized view log' = Value1, Value2} = _PTree,
    {materialized, Step} = _FoldState)
    when is_atom(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1}),
                 format_keyword(LOpts, Value2),
                 " ",
                 format_keyword(LOpts, Value1)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {on, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "on "),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {on, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "on ")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on_obj_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Target, Value} = _PTree, {on_obj_clause, Step} =
    _FoldState)
    when (Target == on orelse Target == 'on directory') andalso
             is_binary(Value) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Target),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Target, _Value} = _PTree, {on_obj_clause, Step} =
    _FoldState)
    when (Target == on orelse Target == 'on directory') ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Target),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {open, _Cursor} = _PTree,
    {open_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_new_statement(LOpts, FunState, "open ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {opt, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 format_keyword(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {order_by_clause, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "order by"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ordering_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {ordering_spec, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, _FunState, Ctx, {ScalarExp, <<>>} = _PTree,
    {ordering_spec, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {ScalarExp, AscDesc} = _PTree,
    {ordering_spec, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, ScalarExp);
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_keyword(LOpts, AscDesc)
             ])
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, {_ScalarExp, <<>>} = _PTree,
    {ordering_spec, _Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    ?CUSTOM_RESULT(Ctx);
fold(LOpts, _FunState, Ctx, {_ScalarExp, AscDesc} = _PTree,
    {ordering_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_keyword(LOpts, AscDesc)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param param
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx,
    {indicator, {param = Rule, Value1}, {param = Rule, Value2}} = _PTree,
    {param, Step} = _FoldState)
    when is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, binary_to_list(Value1), " indicator ", binary_to_list(
                     Value2)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(_LOpts, _FunState, Ctx,
    {{param = Rule, Value1}, {param = Rule, Value2}} =
        _PTree, {param, Step} = _FoldState)
    when is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, binary_to_list(Value1), " ", binary_to_list(Value2)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param & table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == param orelse Rule == table) andalso is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {partition_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "partition by (")
             ]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsql_body
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {plsql_body, _} = _PTree, {plsql_body, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "begin"),
                 ?CHAR_NEWLINE
             ]);
             _ -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_keyword(LOpts, "end;")
             ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% privilege
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {privilege, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 PTreeStr = atom_to_list(PTree),
                 Ctx ++
                 case lists:member(PTreeStr,
                     ?OBJECT_PRIVILEGES ++ ?SYSTEM_PRIVILEGES) of
                     true -> format_keyword(LOpts, PTreeStr);
                     _ -> format_identifier(LOpts, PTreeStr)
                 end;
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure_call
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'call procedure', _} = _PTree,
    {procedure_call, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "call")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {_Type, _QuerySpec1, _QuerySpec2} = PTree,
    {query_exp, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    {StmntPred_1, ClausePred_1, RulePred_1} =
        sqlparse_fold:get_stmnt_clause_pred(FunState, 1),
    {_, _, RulePred_2} = sqlparse_fold:get_stmnt_clause_pred(FunState, 2),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case
                     RulePred_1 == function_ref orelse
                         StmntPred_1 == query_spec andalso
                             ClausePred_1 == fields of
                     true -> ?CHAR_NEWLINE;
                     _ -> []
                 end,
                 case
                     RulePred_1 =/= RulePred_2 andalso
                         (RulePred_1 == select_left orelse
                             RulePred_1 == select_right orelse
                             RulePred_1 == union_left orelse
                             RulePred_1 == union_right) orelse
                         StmntPred_1 =/= none andalso
                             StmntPred_1 =/= query_exp of
                     true ->
                         format_column_pos(LOpts, FunState#fstate{indent_lvl =
                         FunState#fstate.indent_lvl +
                             sqlparse_fold:get_ptree_max_depth_set(PTree) - 1});
                     _ -> []
                 end,
                 case StmntPred_1 =/= none of
                     true -> "(";
                     _ -> []
                 end
             ]);
             'end' -> case StmntPred_1 =/= none of
                          true -> Ctx ++ ")";
                          _ -> Ctx
                      end
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {select, Clauses} = _PTree, {query_spec, Step} =
    _FoldState)
    when is_list(Clauses) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {StmntPred_1, ClausePred_1, RulePred_1} =
        sqlparse_fold:get_stmnt_clause_pred(FunState, 1),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 case
                     RulePred_1 == function_ref orelse
                         StmntPred_1 == query_spec andalso
                             ClausePred_1 == fields of
                     true -> ?CHAR_NEWLINE;
                     _ -> []
                 end,
                 case
                     ClausePred_1 == begin_procedure orelse
                         ClausePred_1 == query_spec orelse
                         RulePred_1 == function_ref orelse
                         StmntPred_1 == insert_statement andalso
                             ClausePred_1 == query_spec orelse
                         StmntPred_1 == plsql_body orelse
                         StmntPred_1 == query_exp orelse
                         StmntPred_1 == query_spec andalso
                             ClausePred_1 == fields orelse
                         StmntPred_1 == view_def of
                     true ->
                         format_column_pos(LOpts, FunState#fstate{indent_lvl =
                         FunState#fstate.indent_lvl - 1});
                     _ -> []
                 end,
                 case
                     ClausePred_1 == begin_procedure andalso
                         RulePred_1 =/= function_ref orelse
                         RulePred_1 == in_predicate orelse
                         StmntPred_1 == insert_statement andalso
                             ClausePred_1 == query_spec orelse
                         StmntPred_1 == none orelse
                         StmntPred_1 == plsql_body andalso
                             RulePred_1 =/= function_ref orelse
                         StmntPred_1 == view_def of
                     true -> [];
                     _ -> "("
                 end,
                 format_keyword(LOpts, "select")
             ]);
             'end' -> case
                          ClausePred_1 == begin_procedure andalso
                              RulePred_1 =/= function_ref orelse
                              RulePred_1 == in_predicate orelse
                              StmntPred_1 == insert_statement andalso
                                  ClausePred_1 == query_spec orelse
                              StmntPred_1 == none orelse
                              StmntPred_1 == plsql_body andalso
                                  RulePred_1 =/= function_ref orelse
                              StmntPred_1 == view_def of
                          true -> Ctx;
                          _ -> Ctx ++ ")"
                      end
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quota
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {limited, Number, Unit, Name} = _PTree,
    {quota, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 lists:append([
                     Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState#fstate{indent_lvl =
                     FunState#fstate.indent_lvl - 1}),
                     format_keyword(LOpts, "quota"),
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     binary_to_list(Number),
                     case Unit of
                         <<"">> -> [];
                         _ -> " " ++ format_identifier(LOpts, Unit)
                     end,
                     format_keyword(LOpts, " on "),
                     format_identifier(LOpts, Name)
                 ]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'unlimited on', Name} = _PTree,
    {quota, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 lists:append([
                     Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState#fstate{indent_lvl =
                     FunState#fstate.indent_lvl - 1}),
                     format_keyword(LOpts, "quota"),
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState),
                     format_keyword(LOpts, "unlimited on "),
                     format_identifier(LOpts, Name)
                 ]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {ref, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " references "),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {Value, _} = _PTree, {ref, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, " references "),
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {ref, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, " references ");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return & returning
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {_Selection1, _Selection2} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Rule),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "into"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revoke_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {revoke_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "revoke"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {revokee, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "from"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role & table & user
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {Rule, Step, Pos} = _FoldState)
    when (Rule == role orelse Rule == table orelse Rule == user) andalso
             is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {Type, RoleList} = _PTree, {role_list, Step} =
    _FoldState)
    when (Type == 'default role' orelse Type == 'default role all except')
             andalso is_list(RoleList) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Type),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, FunState, Ctx, {Type, RoleList} = _PTree, {role_list, Step} =
    _FoldState)
    when is_atom(Type), is_list(RoleList) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, Type),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {scalar_exp, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {scalar_exp, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_opt_as_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {scalar_opt_as_exp, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {scalar_opt_as_exp, Step, Pos} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, PTree, {scalar_opt_as_exp, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'create schema authorization', Name, _SchemaElementList} = _PTree,
    {schema, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState,
                     "create schema authorization "),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 Name
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema_element_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {schema_element_list, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% select_field
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {select_field, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_identifier(LOpts, PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {select_field, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {set, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "set")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item & user_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, {quotas, _Elements} = _PTree,
    {Rule, _Step, _Pos} = _FoldState)
    when Rule == spec_item;Rule == user_opt ->
    Ctx;
fold(LOpts, FunState, Ctx, [{Type, Value}] = _PTree, {Rule, Step, Pos} =
    _FoldState)
    when (Rule == spec_item orelse Rule == user_opt) andalso
             (Type == 'default tablespace' orelse Type == profile orelse
                 Type == 'temporary tablespace') andalso is_binary(Value) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Type),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Value)
             ]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {spec_item, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, PTree)
             ]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Value1, Value2} = _PTree,
    {spec_item, Step, _Pos} = _FoldState)
    when is_atom(Value1), is_atom(Value2) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, Value1),
                 " ",
                 format_keyword(LOpts, Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {spec_item, _Step, _Pos} = _FoldState) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql & statement_pragma
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {Type, Step, Pos} = _FoldState)
    when Type == sql; Type == statement_pragma ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {_SQL, Pos, {extra, <<>>}} = _PTree,
    {sql_list, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> lists:append([
                 Ctx,
                 ";",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1})
             ]);
             {'end', _} -> Ctx ++ ";";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {_SQL, Pos, {extra, Extra}} = _PTree,
    {sql_list, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 ";",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 binary_to_list(Extra),
                 ";",
                 case Pos of
                     other -> lists:append([
                         ?CHAR_NEWLINE,
                         format_column_pos(LOpts, FunState#fstate{indent_lvl =
                         FunState#fstate.indent_lvl - 1})
                     ]);
                     _ -> []
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'start with', SearchCondition} = _PTree,
    {start_with, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "start with"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, SearchCondition)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {start_with, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "start with"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {statement_pragma_list, Step, Pos} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> lists:append([
                 Ctx,
                 ";",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1})
             ]);
             {'end', _} -> Ctx ++ ";";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% storage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {storage = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1}),
                 format_keyword(LOpts, PTree),
                 " ",
                 format_keyword(LOpts, Rule)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {table, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_coll_expr
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {table_coll_expr, CollExpr} = _PTree,
    {table_coll_expr, Step} = _FoldState)
    when is_binary(CollExpr) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "table ("),
                 format_identifier(LOpts, CollExpr)
             ]);
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {table_coll_expr, _CollExpr} = _PTree,
    {table_coll_expr, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_keyword(LOpts, "table (");
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_constraint_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {check, [], _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_keyword(LOpts, "check (");
             {'end', other} -> lists:append([
                 Ctx,
                 "),",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             {'end', _} -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {check, ConstraintName, _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "constraint "),
                 format_identifier(LOpts, ConstraintName),
                 format_keyword(LOpts, " check (")
             ]);
             {'end', other} -> lists:append([
                 Ctx,
                 "),",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             {'end', _} -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'foreign key' = Type, [], _, _} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_keyword(LOpts, Type);
             {'end', other} -> lists:append([
                 Ctx,
                 ",",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'foreign key' = Type, ConstraintName, _, _} =
    _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "constraint "),
                 format_identifier(LOpts, ConstraintName),
                 " ",
                 format_keyword(LOpts, Type)
             ]);
             {'end', other} -> lists:append([
                 Ctx,
                 ",",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Type, [], _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ format_keyword(LOpts, Type);
             {'end', other} -> lists:append([
                 Ctx,
                 ",",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Type, ConstraintName, _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 format_keyword(LOpts, "constraint "),
                 format_identifier(LOpts, ConstraintName),
                 " ",
                 format_keyword(LOpts, Type)
             ]);
             {'end', other} -> lists:append([
                 Ctx,
                 ",",
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1})
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {as, Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, Table);
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_identifier(LOpts, Alias)
             ])
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, {as, _Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 format_identifier(LOpts, Alias)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold(LOpts, _FunState, Ctx, {Table, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_identifier(LOpts, Table);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% target
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, PTree, {target, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_identifier(LOpts, PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {target, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tbl_scope & tbl_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when (Rule == tbl_scope orelse Rule == tbl_type) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    ValueStr = binary_to_list(Value),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 case lists:member(ValueStr, ?TABLE_OPTIONS) of
                     true -> format_keyword(LOpts, ValueStr);
                     _ -> format_identifier(LOpts, ValueStr)
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_for_null
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, _FunState, Ctx, {is, ScalarExp, <<"null">>} = _PTree,
    {test_for_null, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     format_identifier(LOpts, ScalarExp),
                     format_keyword(LOpts, " is null")
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, _FunState, Ctx, _PTree, {test_for_null, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ format_keyword(LOpts, " is null");
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% then
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {then, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1}),
                 format_keyword(LOpts, "then ")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_cluster
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'truncate cluster', Name, _Cascade} = _PTree,
    {truncate_cluster, Step} = _FoldState)
    when is_binary(Name) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "truncate cluster"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'truncate table', Table, _Materialized, _Storage} =
    _PTree, {truncate_table, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "truncate table"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx,
    {'truncate table', Table, _Materialized, _Storage, _Cascade} = _PTree,
    {truncate_table, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "truncate table"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'truncate table', _Table, _Materialized, _Storage} =
    _PTree, {truncate_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "truncate table"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx,
    {'truncate table', _Table, _Materialized, _Storage, _Cascade} = _PTree,
    {truncate_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "truncate table"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {'not' = Op, Value} = _PTree, {unary, Step} =
    _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_operator(LOpts, FunState, Op, true),
                 "(",
                 format_identifier(LOpts, Value),
                 ")"
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, Value} = _PTree, {unary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_operator(LOpts, FunState, Op, true),
                 format_identifier(LOpts, Value)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {'not' = Op, _Value} = _PTree, {unary, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_operator(LOpts, FunState, Op, true),
                 "("
             ]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, {Op, _Value} = _PTree, {unary, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ format_operator(LOpts, FunState, Op, true);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {update, Table, _, _, _} = _PTree,
    {update_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "update"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {update_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "update"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, PTree, {user_list, _Step} = _FoldState)
    when is_list(PTree) ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_opts_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {user_opts_list, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {using, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_keyword(LOpts, "using (")
             ]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% values_or_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, {values, _} = _PTree,
    {values_or_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "values"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 "("
             ]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold(_LOpts, _FunState, Ctx, _PTree, {values_or_query_spec, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ ?CHAR_NEWLINE;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx,
    {'create view', Table, _ColumnCommalist, _QuerySpec} = _PTree,
    {view_def, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "create view"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "create view"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, _PTree, {view_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "as"),
                 ?CHAR_NEWLINE
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'when'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {'when', Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx,
                     ?CHAR_NEWLINE,
                     format_column_pos(LOpts, FunState#fstate{indent_lvl =
                     FunState#fstate.indent_lvl + 1}),
                     format_keyword(LOpts, "when "),
                     format_identifier(LOpts, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {'when', Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl + 1}),
                 format_keyword(LOpts, "when ")
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_not_found
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {when_not_found, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx,
                 format_new_statement(LOpts, FunState, "whenever not found"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {when_not_found, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "whenever not found"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_sql_err
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {when_sql_err, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "whenever sqlerror"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {when_sql_err, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 format_new_statement(LOpts, FunState, "whenever sqlerror"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {where_clause, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "where"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState),
                 format_identifier(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold(LOpts, FunState, Ctx, _PTree, {where_clause, Step} = _FoldState) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, "where"),
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_current_of
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {where_current_of, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " where current of ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with_grant_option & with revoke_option
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(LOpts, FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == with_grant_option orelse Rule == with_revoke_option) andalso
             is_atom(PTree) ->
    ?CUSTOM_INIT(FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 ?CHAR_NEWLINE,
                 format_column_pos(LOpts, FunState#fstate{indent_lvl =
                 FunState#fstate.indent_lvl - 1}),
                 format_keyword(LOpts, PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NO ACTION.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, Ctx, _PTree, {Rule, _Step, _Pos}) when
    Rule == case_when_then;
    Rule == join;
    Rule == user_opt ->
    Ctx;

fold(_LOpts, _FunState, Ctx, _PTree, {Rule, _Step}) when
    Rule == all_or_any_predicate;
    Rule == anchor;
    Rule == assignment_statement;
    Rule == between_predicate;
    Rule == binary;
    Rule == case_when_then;
    Rule == case_when_then_list;
    Rule == check;
    Rule == column_def_list;
    Rule == column_ref_commalist;
    Rule == comparison_predicate;
    Rule == create_opts;
    Rule == default;
    Rule == 'fun';
    Rule == function_ref_list_list;
    Rule == function_ref;
    Rule == hierarchical_query;
    Rule == in_predicate;
    Rule == join_list;
    Rule == jpparse;
    Rule == like_predicate;
    Rule == param;
    Rule == prior;
    Rule == procedure_call;
    Rule == query_exp;
    Rule == query_spec;
    Rule == query_spec_jpparse;
    Rule == quotas;
    Rule == scalar_exp_commalist;
    Rule == scalar_opt_as_exp;
    Rule == sql_list;
    Rule == sql_list_list;
    Rule == statement_pragma_list;
    Rule == statement_pragma_list_list;
    Rule == table;
    Rule == table_dblink;
    Rule == tables;
    Rule == user_opt;
    Rule == user_opts_list ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_LOpts, _FunState, _Ctx, PTree, {Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        " / pos=",
        atom_to_list(Pos),
        "]"
    ]), PTree});
fold(_LOpts, _FunState, _Ctx, PTree, {Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        "]"
    ]), PTree}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorten a specific statement line according to parameter line_break_after.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

break_line(LOpts, Line) ->
    case length(Line) =< LOpts#lopts.line_break_after of
        true -> [Line];
        _ -> LineRest = string:trim(Line, leading, " "),
            PrefixLength = length(Line) - length(LineRest),
            Prefix = string:slice(Line, 0, PrefixLength),
            case PrefixLength of
                P when P > 0, P =< LOpts#lopts.line_break_after ->
                    break_line(LOpts#lopts.line_break_after - PrefixLength - 1,
                        Prefix, LineRest);
                _ -> [Line]
            end
    end.

break_line(Length, Prefix, Line) ->
    break_line(Length, Prefix, Line, []).

break_line(_Length, _Prefix, [], Acc) ->
    Acc;
break_line(Length, Prefix, Line, Acc) ->
    case length(Line) < Length of
        true -> Acc ++ [Prefix ++ Line];
        _ -> BreakPoint = break_line_point(Length, Line),
            case BreakPoint of
                0 -> Acc ++ [Prefix ++ Line];
                1 -> Acc ++ [Prefix ++ Line];
                _ -> break_line(Length, Prefix,
                    string:trim(string:slice(Line, BreakPoint - 1), both,
                        " "),
                    Acc ++ [Prefix ++
                        string:trim(string:slice(Line, 0, BreakPoint - 1),
                            both, " ")])
            end
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the last possible position in the line to insert a suitable
% line break.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

break_line_point(Length, Line) ->
    break_line_point(Length + 1, Line, Line, 0, 1, []).

break_line_point(_Length, _Line, [], BreakPoint, _Pos, _UnBreakable) ->
    BreakPoint;
break_line_point(Length, Line, [Char | Tail], BreakPoint, Pos, UnBreakable)
    when Pos =< Length ->
    case UnBreakable of
        [] -> case Char of
                  $' -> case Pos > 1 andalso
                      string:slice(Line, Pos - 2, 1) == "'" orelse
                      string:slice(Tail, 0, 1) == "'" of
                            true ->
                                break_line_point(Length, Line, Tail, BreakPoint,
                                    Pos + 1, UnBreakable);
                            _ -> break_line_point(Length, Line, Tail, Pos,
                                Pos + 1, "'")
                        end;
                  $" ->
                      break_line_point(Length, Line, Tail, Pos, Pos + 1, "\"");
                  $| -> break_line_point(Length, Line, Tail, Pos, Pos + 1, "|");
                  $  -> break_line_point(Length, Line, Tail, Pos, Pos + 1,
                      UnBreakable);
                  $, -> break_line_point(Length, Line, Tail, Pos + 1, Pos + 1,
                      UnBreakable);
                  $/ ->
                      case string:slice(Tail, 0, 1) == "*" of
                          true ->
                              break_line_point(Length, Line, Tail, BreakPoint,
                                  Pos + 1, "/");
                          _ -> break_line_point(Length, Line, Tail, BreakPoint,
                              Pos + 1, UnBreakable)
                      end;
                  _ -> break_line_point(Length, Line, Tail, BreakPoint, Pos + 1,
                      UnBreakable)
              end;
        U when U == "'" andalso Char == $' ->
            case string:slice(Line, Pos - 2, 1) == "'" orelse
                string:slice(Tail, 0, 1) == "'" of
                true ->
                    break_line_point(Length, Line, Tail, BreakPoint, Pos + 1,
                        UnBreakable);
                _ -> break_line_point(Length, Line, Tail, Pos + 1, Pos + 1, [])
            end;
        U when U == "\"" andalso Char == $" ->
            break_line_point(Length, Line, Tail, Pos + 1, Pos + 1, []);
        U when U == "|" andalso Char == $| ->
            break_line_point(Length, Line, Tail, Pos + 1, Pos + 1, []);
        U when U == "/" andalso Char == $/
            ->
            case string:slice(Line, Pos - 2, 1) == "*" of
                true ->
                    break_line_point(Length, Line, Tail, BreakPoint, Pos + 1,
                        []);
                _ -> break_line_point(Length, Line, Tail, BreakPoint, Pos + 1,
                    UnBreakable)
            end;
        _ -> break_line_point(Length, Line, Tail, BreakPoint, Pos + 1,
            UnBreakable)
    end;
break_line_point(_Length, _Line, [Char | _Tail], BreakPoint, Pos, UnBreakable) ->
    case UnBreakable of
        [] -> case Char of
                  C when C == $ ;C == $,;C == $(;C == $) -> Pos;
                  _ -> BreakPoint
              end;
        _ -> BreakPoint
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorten the statement lines according to parameter line_break_after.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

break_lines(LOpts, Ctx) ->
    case LOpts#lopts.line_break_after of
        0 -> Ctx;
        _ -> string:join(
            break_lines(LOpts, string:split(Ctx, ?CHAR_NEWLINE, all), []),
            ?CHAR_NEWLINE)
    end.

break_lines(_LOpts, [] = _Ctx, Acc) ->
    Acc;
break_lines(LOpts, [Line | Tail] = _Ctx, Acc) ->
    break_lines(LOpts, Tail, Acc ++ break_line(LOpts, Line)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determining the current column position.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_column_pos(LOpts, FunState) ->
    format_column_pos(LOpts, FunState#fstate.indent_lvl, []).

format_column_pos(_LOpts, IndentationLevel, Acc)
    when IndentationLevel =< 0 ->
    Acc;
format_column_pos(LOpts, IndentationLevel, Acc) ->
    format_column_pos(LOpts, IndentationLevel - 1,
        Acc ++ case LOpts#lopts.indent_with of
                   tab -> ?CHAR_TAB;
                   _ ->
                       case LOpts#lopts.indent_space of
                           2 -> "  ";
                           3 -> "   ";
                           4 -> "    ";
                           5 -> "     ";
                           6 -> "      ";
                           7 -> "       ";
                           8 -> "        ";
                           _ -> " "
                       end
               end).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting data types.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_data_type(LOpts, ST)
    when is_binary(ST) ->
    format_data_type(LOpts, binary_to_list(ST));
format_data_type(LOpts, ST) ->
    STLower = string:casefold(ST),
    case lists:member(STLower, ?DATA_TYPES) of
        true -> format_keyword(LOpts, STLower);
        _ -> format_identifier(LOpts, STLower)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting identifiers.
% ------------------------------------------------------------------------------
% Allowed values: init_cap, keep_unchanged, lower,upper
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_identifier(LOpts, Identifier)
    when is_atom(Identifier) ->
    format_keyword(LOpts, Identifier);

format_identifier(LOpts, Identifier)
    when is_binary(Identifier) ->
    format_identifier(LOpts, binary_to_list(Identifier));
format_identifier(LOpts, Identifier = _ST) ->
    case lists:sublist(Identifier, 1, 1) of
        ":" -> Identifier;
        _ -> Fun_4 = string:slice(Identifier, 0, 4),
            case Fun_4 == "fun " orelse Fun_4 == "fun(" of
                true -> Identifier;
                _ -> I_1 = lists:sublist(Identifier, 1),
                    case I_1 == "'" orelse I_1 == "\"" of
                        true -> Identifier;
                        _ -> case lists:member(string:uppercase(Identifier),
                            get_funs()) of
                                 true -> format_keyword(LOpts, Identifier);
                                 _ -> format_identifier_final(LOpts, Identifier)
                             end
                    end
            end
    end.

format_identifier_final(LOpts, Identifier)
    when LOpts#lopts.case_identifier == keep_unchanged ->
    Identifier;
format_identifier_final(LOpts, Identifier) ->
    {PureIdentifier, JSON} = case string:find(Identifier, "|") of
                                 nomatch -> {Identifier, []};
                                 JSONPart -> [IdentifierPart, []] =
                                     string:split(Identifier, JSONPart),
                                     {IdentifierPart, JSONPart}
                             end,
    case LOpts#lopts.case_identifier of
        lower -> string:casefold(PureIdentifier);
        upper -> string:uppercase(PureIdentifier);
        _ -> format_init_cap(string:casefold(PureIdentifier), [], [])
    end ++ JSON.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting init_cap version.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_init_cap([], _, Acc) ->
    Acc;
format_init_cap([Head | Tail], Previous, Acc) ->
    format_init_cap(Tail, Head,
        Acc ++
        case Previous == [] orelse lists:member([Previous], [" ", "_", "."]) of
            true -> string:uppercase([Head]);
            _ -> [Head]
        end).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting keywords.
% ------------------------------------------------------------------------------
% Allowed values: init_cap, lower,upper
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_keyword(LOpts, Keyword)
    when is_atom(Keyword) ->
    format_keyword(LOpts, atom_to_list(Keyword));
format_keyword(LOpts, Keyword)
    when is_binary(Keyword) ->
    format_keyword(LOpts, binary_to_list(Keyword));
format_keyword(LOpts, Keyword) ->
    case LOpts#lopts.case_keyword of
        lower -> Keyword;
        upper -> string:uppercase(Keyword);
        _ -> format_init_cap(Keyword, [], [])
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting new statement.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_new_statement(LOpts, FunState, Keywords) ->
    format_column_pos(LOpts,
        FunState#fstate{indent_lvl = FunState#fstate.indent_lvl - 1}) ++
    case Keywords of
        [] -> [];
        _ -> format_keyword(LOpts, Keywords)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting operators.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_operator(LOpts, FunState, Op = _ST, IsUnary)
    when is_atom(Op) ->
    format_operator(LOpts, FunState, atom_to_list(Op), IsUnary);

format_operator(LOpts, FunState, Op = _ST, IsUnary) ->
    case Op == "and" orelse Op == "or" of
        true ->
            {_, _, Rule} = sqlparse_fold:get_stmnt_clause_curr(FunState),
            lists:append([
                ?CHAR_NEWLINE,
                case Rule of
                    case_when_then ->
                        format_column_pos(LOpts, FunState#fstate{indent_lvl =
                        FunState#fstate.indent_lvl + 1});
                    _ -> format_column_pos(LOpts, FunState)
                end,
                format_keyword(LOpts, Op),
                " "
            ]);
        _ -> OPStr = case Op == "not" of
                         true -> format_keyword(LOpts, Op);
                         _ -> Op
                     end,
            case IsUnary of
                true -> case LOpts#lopts.ws_operator of
                            true -> string:trim(OPStr, both, " ") ++ " ";
                            _ -> OPStr
                        end;
                _ ->
                    case LOpts#lopts.ws_operator /= true andalso (
                        OPStr == "=" orelse
                            OPStr == "!=" orelse
                            OPStr == "^=" orelse
                            OPStr == "<>" orelse
                            OPStr == "<" orelse
                            OPStr == ">" orelse
                            OPStr == "<=" orelse
                            OPStr == ">=") of
                        true -> OPStr;
                        _ -> lists:append(
                            [" ", string:trim(OPStr, both, " "), " "])
                    end
            end
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Delivers the standard functions from the lexer.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_funs() ->
    [string:trim(string:trim(F, leading, "^(?i)("), trailing,
        ")$") || {F, 'FUNS'} <- ?TOKENPATTERNS].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validation and storage of input parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_lopts(LOpts, []) ->
    LOpts;
process_lopts(LOpts, [{case_identifier = Parameter, Value} | Tail]) ->
    case Value of
        Valid when Valid == init_cap;Valid == keep_unchanged;Valid == lower;
                   Valid == upper ->
            ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{case_identifier = Value}, Tail);
process_lopts(LOpts, [{case_keyword = Parameter, Value} | Tail]) ->
    case Value of
        Valid when Valid == init_cap;Valid == lower;  Valid == upper -> ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{case_keyword = Value}, Tail);
process_lopts(LOpts, [{indent_space = Parameter, Value} | Tail]) ->
    case Value of
        Valid when is_integer(Valid), Valid >= 0, Valid =< 8 -> ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{indent_space = Value}, Tail);
process_lopts(LOpts, [{indent_with = Parameter, Value} | Tail]) ->
    case Value of
        Valid when Valid == space;Valid == tab -> ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{indent_with = Value}, Tail);
process_lopts(LOpts, [{line_break_after = Parameter, Value} | Tail]) ->
    case Value of
        Valid when is_integer(Valid), Valid >= 0 -> ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{line_break_after = Value}, Tail);
process_lopts(LOpts, [{ws_operator = Parameter, Value} | Tail]) ->
    case Value of
        Valid when Valid == false;Valid == true -> ok;
        _ -> io:format(user, "~n" ++ ?MODULE_STRING ++
            " : invalid parameter value : parameter: ~p value: ~p~n",
            [Parameter, Value]),
            throw(invalid_parameter_value)
    end,
    process_lopts(LOpts#lopts{ws_operator = Value}, Tail);
process_lopts(_LOpts, [{Parameter, Value} | _Tail]) ->
    io:format(user, "~n" ++ ?MODULE_STRING ++
        " : invalid parameter : parameter: ~p value: ~p~n",
        [Parameter, Value]),
    throw(invalid_parameter).
