%% -----------------------------------------------------------------------------
%%
%% sqlparse_format_flat.erl: SQL - creating a flat version of the SQL statement.
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

-module(sqlparse_format_flat).

-export([
    finalize/2,
    fold/5,
    init/1
]).

-define(NODEBUG, true).

-include("sqlparse_fold.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up optional parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Params :: any()) -> [].
init(_Params) ->
    ?D("Start~n Params: ~p~n", [_Params]),
    [].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Postprocessing of the resulting SQL statement.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize(Params :: any(), Ctx :: string()|tuple()) -> Ctx :: binary()|tuple().
finalize(_Params, Ctx)
    when is_list(Ctx) ->
    ?D("Start~n Params: ~p~n CtxOut: ~p~n", [_Params, Ctx]),
    list_to_binary(string:trim(Ctx)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Layout methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ( & base_table_element_commalist & cols & column_commalist & create_index_spec
% & fun_arg_commalist & ref_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(list(), FunState :: tuple(), Ctx :: string()|tuple(),
    PTree :: list()|tuple(), FoldState :: tuple()) -> Ctx :: tuple()|none().
fold([], _FunState, Ctx, _PTree, {Rule, Step} = _FoldState)
    when Rule == "(";
         Rule == base_table_element_commalist;
         Rule == cols;
         Rule == column_commalist;
         Rule == create_index_spec;
         Rule == fun_arg_commalist;
         Rule == ref_commalist ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_op
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Op, AnyAllSome, _SubQuery} = _PTree,
    {all_or_any_op, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), atom_to_list(AnyAllSome), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_Op, ScalarExp, _} = _PTree,
    {all_or_any_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alter_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'alter user', User, _Spec} = _PTree,
    {alter_user_def, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "alter user ", binary_to_list(User), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'alter user', Users, _Spec} = _PTree,
    {alter_user_def, Step} = _FoldState)
    when is_list(Users) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "alter user ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% anchor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Anchor, '('} = _PTree, {anchor, Step} =
    _FoldState)
    when is_binary(Anchor) ->
    RT = case Step of
             start -> lists:append([Ctx, "(", binary_to_list(Anchor)]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Anchor, '('} = _PTree, {anchor, Step} =
    _FoldState) ->
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Value, Alias} = _PTree, {as, Step} = _FoldState)
    when is_binary(Value), is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, binary_to_list(Value), " ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Value, Alias} = _PTree, {as, Step} = _FoldState)
    when is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, " ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'call procedure'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'call procedure', _} = _PTree,
    {procedure_call, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "call ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explicit_as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Value, Alias} = _PTree, {explicit_as, Step} =
    _FoldState)
    when is_binary(Value), is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, binary_to_list(Value), " as ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Value, Alias} = _PTree, {explicit_as, Step} =
    _FoldState)
    when is_binary(Alias) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, " as ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assignment
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'=', Column, _ScalarOptAsExp} = _PTree,
    {assignment, Step, Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([Ctx, binary_to_list(Column), "="]);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_and
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, ScalarExp = _PTree, {between_and, Step} =
    _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " and ", binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {between_and, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " and ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_between
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, ScalarExp = _PTree, {between_between, Step} =
    _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, " between ", binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {between_between, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " between ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold([], _FunState, Ctx, {between, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree, {between_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% binary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, Op = _PTree, {binary, Step} = _FoldState)
    when Op == 'and'; Op == 'or'; Op == 'div' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ lists:append([
                 " ",
                 atom_to_list(Op),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, Op = _PTree, {binary, Step} = _FoldState)
    when Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '||'; Op == '=';
         Op == '!='; Op == '^='; Op == '<>'; Op == '<'; Op == '>'; Op == '<=';
         Op == '>='; Op == ':=' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 lists:append([atom_to_list(Op)]),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) andalso is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 binary_to_list(Value1),
                 " ",
                 atom_to_list(Op),
                 " ",
                 binary_to_list(Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op) andalso is_binary(Value1), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 binary_to_list(Value1),
                 " ",
                 atom_to_list(Op),
                 " ",
                 binary_to_list(Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, Value1, _Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 binary_to_list(Value1),
                 " ",
                 atom_to_list(Op),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, Value1, _Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 binary_to_list(Value1),
                 " ",
                 atom_to_list(Op),
                 " "
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, _Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when (Op == 'and' orelse Op == 'div' orelse Op == 'or') andalso
             is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 atom_to_list(Op),
                 " ",
                 binary_to_list(Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, _Value1, Value2} = _PTree, {binary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([
                 Ctx,
                 " ",
                 atom_to_list(Op),
                 " ",
                 binary_to_list(Value2)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'case'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {ScalarOptAsExpr, _CaseWhenThenList, Else} =
    _PTree, {'case', Step} = _FoldState)
    when is_list(_CaseWhenThenList) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "case",
                 case ScalarOptAsExpr of
                     <<>> -> [];
                     B when is_binary(B) ->
                         " " ++ binary_to_list(ScalarOptAsExpr);
                     _ -> " "
                 end
             ]);
             'end' -> lists:append([
                 Ctx,
                 case is_binary(Else) of
                     true -> " else " ++ binary_to_list(Else);
                     _ -> []
                 end,
                 " end"
             ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {check, Step} = _FoldState)
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

fold([], _FunState, Ctx, {close, _Cursor} = _PTree,
    {close_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "close ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {column, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {column, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Column, _DataType, _} = _PTree,
    {column_def, Step, Pos} = _FoldState)
    when is_binary(Column) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(Column);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {column_def_opt, Step, _Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", atom_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {check, _Value} = _PTree,
    {column_def_opt, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " check (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {default, _Value} = _PTree,
    {column_def_opt, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " default ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {ref, _Value} = _PTree,
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

fold([], _FunState, Ctx, PTree, {Type, Step} = _FoldState)
    when Type == commit_statement;Type == rollback_statement ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ atom_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparison_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_Op, {prior, ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate, Step} = _FoldState) when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "prior ", binary_to_list(ScalarExp1)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Op, {prior, _ScalarExp1}, _ScalarExp2} = _PTree,
    {comparison_predicate, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "prior ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Op, ScalarExp1, {prior, _ScalarExp2}} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, {Op, {prior, ScalarExp}} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), "prior ", binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, {prior, _ScalarExp}} = _PTree,
    {comparison_predicate, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Op), "prior "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, ScalarExp} = _PTree,
    {comparison_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), binary_to_list(ScalarExp)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, _ScalarExp} = _PTree,
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

fold([], _FunState, Ctx, {'connect by', <<>>, SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [Ctx, " connect by ", binary_to_list(SearchCondition)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'connect by', NoCycle, SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [Ctx, " connect by ", binary_to_list(
                         NoCycle), " ", binary_to_list(SearchCondition)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'connect by', <<>>, _SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " connect by ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'connect by', NoCycle, _SearchCondition} = _PTree,
    {connect_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " connect by ", binary_to_list(NoCycle), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'create index', CreateIndexFoldState, _IndexName, _TableAlias, _CreateIndexSpec, _CreateIndexNorm, _CreateIndexFilter} =
        _PTree, {create_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "create ",
                 case CreateIndexFoldState of
                     {} -> [];
                     _ -> atom_to_list(CreateIndexFoldState) ++ " "
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_name
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, IndexName = _PTree, {create_index_name, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "index ",
                 case IndexName of
                     {} -> [];
                     _ -> binary_to_list(IndexName) ++ " "
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_filter & create_index_norm
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when Rule == filter_with;Rule == norm_with ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", atom_to_list(Rule), " ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec_column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {create_index_spec_column, Step, Pos} =
    _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {create_index_spec_column, Step, Pos} =
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

fold([], _FunState, Ctx, PTree, {create_index_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "on ",
                 case is_binary(PTree) of
                     true -> binary_to_list(PTree);
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'create role', Value} = _PTree,
    {create_role_def, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "create role ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {create_table_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "create ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {create_table_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "table ",
                 case is_binary(PTree) of
                     true -> binary_to_list(PTree);
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'create user', User, _Identified, _UserFoldStateList} = _PTree,
    {create_user_def, Step} = _FoldState)
    when is_binary(User) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "create user ", binary_to_list(User), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cur
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {cur, Cursor} = _PTree, {cur, Step} = _FoldState)
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

fold([], _FunState, Ctx, _PTree, {cursor_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "cursor ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {cursor_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " is ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% data_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {{Value, Prec, Scale}, _DOpts} = _PTree,
    {data_type, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec), is_binary(Scale) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", binary_to_list(Value), "(", binary_to_list(
                     Prec), ", ", binary_to_list(Scale), ")"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {{Value, Prec}, _DOpts} = _PTree,
    {data_type, Step} = _FoldState)
    when is_binary(Value), is_binary(Prec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", binary_to_list(Value), "(", binary_to_list(
                     Prec), ")"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Value, _DOpts} = _PTree, {data_type, Step} =
    _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {dblink, Step} = _FoldState)
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

fold([], _FunState, Ctx, PTree, {default, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ atom_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {default, Step} = _FoldState)
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

fold([], _FunState, Ctx, {delete, Table, _, _} = _PTree,
    {delete_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "delete from ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {delete_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "delete from ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop cluster', Name, _DropExtensions} = _PTree,
    {drop_cluster_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop cluster ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_context_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop context', Name} = _PTree,
    {drop_context_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop context ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop database'} = _PTree,
    {drop_database_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "drop database";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop database link', Name, {}} = _PTree,
    {drop_database_link_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop database link ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop database link', Name, public} = _PTree,
    {drop_database_link_public_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop public database link ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_directory_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop directory', Name} = _PTree,
    {drop_directory_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop directory ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {Type, Step} = _FoldState)
    when Type == drop_extensions, is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 " ",
                 atom_to_list(PTree)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_function_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop function', Name} = _PTree,
    {drop_function_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop function ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop index', IndexName, _Table} = _PTree,
    {drop_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop index",
                 case IndexName of
                     {} -> [];
                     _ -> " " ++ binary_to_list(IndexName)
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'drop index', IndexName, _Table, _DropExtensions} =
    _PTree, {drop_index_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop index",
                 case IndexName of
                     {} -> [];
                     _ -> " " ++ binary_to_list(IndexName)
                 end
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {drop_index_from, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " from ",
                 case is_binary(PTree) of
                     true -> binary_to_list(PTree);
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'drop materialized view', Name, _MaterializedViewExtensions} = _PTree,
    {drop_materialized_view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop materialized view ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop package', Name, {}} = _PTree,
    {drop_package_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop package ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop package', Name, body} = _PTree,
    {drop_package_body_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop package body ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_procedure_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop procedure', Name} = _PTree,
    {drop_procedure_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop procedure ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop profile', Name, _DropExtensions} = _PTree,
    {drop_profile_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop profile ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop role', Role} = _PTree,
    {drop_role_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop role ",
                 binary_to_list(Role)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_sequence_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop sequence', Name} = _PTree,
    {drop_sequence_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop sequence ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop synonym', Name, {}, _DropExtensions} = _PTree,
    {drop_synonym_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop synonym ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_public_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'drop synonym', Name, public, _DropExtensions} = _PTree,
    {drop_synonym_public_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop public synonym ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'drop table', _Tables, _Exists, _DropExtensions, DropOpt} = _PTree,
    {drop_table_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "drop ",
                 case DropOpt of
                     [] -> [];
                     _ -> DropOpt ++ " "
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_tablespace_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop tablespace', Name, _DropExtensions} = _PTree,
    {drop_tablespace_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop tablespace ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_trigger_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop trigger', Name} = _PTree,
    {drop_trigger_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop trigger ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop type', Name, _DropExtensions} = _PTree,
    {drop_type_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop type ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop type body', Name} = _PTree,
    {drop_type_body_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop type body ",
                 binary_to_list(Name)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop user', User, []} = _PTree,
    {drop_user_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "drop user ", binary_to_list(User)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'drop user', User, [Cascade]} = _PTree,
    {drop_user_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, "drop user ", binary_to_list(User), " ", atom_to_list(
                     Cascade)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'drop view', Table, _DropExtensions} = _PTree,
    {drop_view_def, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([
                 Ctx,
                 "drop view ",
                 binary_to_list(Table)
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'drop view', _Table, _DropExtensions} = _PTree,
    {drop_view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "drop view ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% else
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {else, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " else ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% existence_test
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {existence_test, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "exists ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {exists, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ case PTree of
                                 {} -> "table ";
                                 exists -> "table if exists "
                             end;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fetch_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {fetch_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "fetch ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Table, Join} = _PTree, {from, Step, Pos} =
    _FoldState)
    when is_binary(Table), is_list(Join) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(Table);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {from, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {from, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {from, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " from ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Type, Value} = _PTree, {fun_arg, Step, _Pos} =
    _FoldState)
    when (Type == all orelse Type == distinct) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Type), " ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, _Value} = _PTree, {fun_arg, Step, _Pos} =
    _FoldState)
    when Type == all; Type == distinct ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Type), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, PTree, {fun_arg, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {fun_arg, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'fun', Name, []} = _PTree, {function_ref, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(Name), "()"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'fun', Name, _} = _PTree, {function_ref, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(Name);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {as, {{'fun', _, _}, JSON, []}, Alias} = _PTree,
    {function_ref, Step} = _FoldState)
    when is_tuple(JSON) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, " ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goto
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {goto, Step} = _FoldState)
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

fold([], _FunState, Ctx, {Value1, Value2} = _PTree, {Rule, Step} =
    _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(Value1) andalso is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [Ctx, atom_to_list(Rule), " through ", atom_to_list(
                         Value1), " ", atom_to_list(Value2)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [Ctx, atom_to_list(Rule), " through ", atom_to_list(
                         PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {{_Type, _Roles}, ProxyAuthReq} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Rule), " through "]);
             'end' -> lists:append([Ctx, " ", atom_to_list(ProxyAuthReq)])
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_Type, _Roles} = _PTree, {Rule, Step} =
    _FoldState)
    when Rule == 'grant connect'; Rule == 'revoke connect' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Rule), " through "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grant_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {grant_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "grant ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {grantee, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " to ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee_revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {grantee_revokee, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ atom_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {grantee_revokee, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'identified by', Name, Password} = _PTree,
    {grantee_revokee, Step} = _FoldState)
    when is_binary(Name);is_binary(Password) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, binary_to_list(Name), " identified by ", binary_to_list(
                     Password)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% group_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {group_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " group by ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% having
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {having, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " having ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {having, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " having ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hints
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {hints, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% identified & spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified by' andalso is_binary(Value) andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Type), " ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified extern' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, atom_to_list('identified externally'),
                     case is_binary(Value) of
                         true -> " as " ++ binary_to_list(Value);
                         _ -> []
                     end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, Value} = _PTree, {Rule, Step} = _FoldState)
    when Type == 'identified globally' andalso
             (Rule == identified orelse Rule == spec) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Type),
                 case is_binary(Value) of
                     true -> " as " ++ binary_to_list(Value);
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_in
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {in_in, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " in (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {in, ScalarExp, _} = _PTree,
    {in_predicate, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {insert, Table, _, _, _} = _PTree,
    {insert_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "insert into ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {insert_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "insert into ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% into
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {into, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " into ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {JoinType, JoinRef, _JoinOnOrUsingClause} =
    _PTree, {join, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     case JoinType of
                         join_inner -> " inner join ";
                         _ -> lists:append([" ", atom_to_list(JoinType), " "])
                     end,
                     binary_to_list(JoinRef)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {JoinType, JoinRef} = _PTree,
    {join, Step, _Pos} = _FoldState)
    when is_binary(JoinRef) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([
                     Ctx,
                     case JoinType of
                         cross_join -> " cross join ";
                         natural_join -> " natural join ";
                         natural_inner_join -> " natural inner join "
                     end,
                     binary_to_list(JoinRef)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {JoinType, _JoinRef, _JoinOnOrUsingClause} =
    _PTree, {join, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ case JoinType of
                                 join_inner -> " inner join ";
                                 _ -> lists:append(
                                     [" ", atom_to_list(JoinType), " "])
                             end;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {JoinType, _JoinRef} = _PTree,
    {join, Step, _Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ case JoinType of
                                 cross_join -> " cross join ";
                                 natural_join -> " natural join ";
                                 natural_inner_join -> " natural inner join "
                             end;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% jpparse
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {jpparse, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & privilege
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == "full join";
         PTree == "left join";
         PTree == "natural full join";
         PTree == "natural join";
         PTree == "natural left join";
         PTree == "natural right join";
         PTree == "right join" ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", PTree, " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == "full_outer join";
         PTree == "left_outer join";
         PTree == "natural full_outer join";
         PTree == "natural left_outer join";
         PTree == "natural right_outer join";
         PTree == "right_outer join" ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, " ", string:replace(PTree, "_", " "), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when PTree == intersect;
         PTree == minus;
         PTree == union;
         PTree == 'union all' ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", atom_to_list(PTree), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, PTree, {keyword, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", PTree]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & privilege
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {Rule, Step, Pos} = _FoldState)
    when (Rule == keyword orelse Rule == privilege) andalso is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ atom_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & with_grant_option & with_revoke_option
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == keyword orelse Rule == with_grant_option orelse
    Rule == with_revoke_option) andalso is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", atom_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_escape
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {like_escape, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, " escape ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {like_escape, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " escape ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_like
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {like_like, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " like ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, _, _} = _PTree, {like_like, Step} =
    _FoldState)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " like (";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {like_like, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " like ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {like, ScalarExp1, _ScalarExp2, _ScalarExp3} =
    _PTree,
    {like_predicate, Step} = _FoldState)
    when is_binary(ScalarExp1) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp1);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% materialized
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'materialized view log' = Value1, Value2} =
    _PTree,
    {materialized, Step} = _FoldState)
    when is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", atom_to_list(Value2), " ", atom_to_list(Value1)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {on, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, " on ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {on, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " on ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on_obj_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Target, Value} = _PTree, {on_obj_clause, Step} =
    _FoldState)
    when (Target == on orelse Target == 'on directory') andalso
             is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", atom_to_list(Target), " ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Target, _Value} = _PTree,
    {on_obj_clause, Step} = _FoldState)
    when (Target == on orelse Target == 'on directory') ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", atom_to_list(Target), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {open, _Cursor} = _PTree,
    {open_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "open ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {opt, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(PTree), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {order_by_clause, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " order by ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ordering_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {ordering_spec, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, {ScalarExp, <<>>} = _PTree,
    {ordering_spec, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {ScalarExp, AscDesc} = _PTree,
    {ordering_spec, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(ScalarExp);
             'end' -> lists:append([Ctx, " ", binary_to_list(AscDesc)])
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_ScalarExp, <<>>} = _PTree,
    {ordering_spec, _Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    ?CUSTOM_RESULT(Ctx);
fold([], _FunState, Ctx, {_ScalarExp, AscDesc} = _PTree,
    {ordering_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, " ", binary_to_list(AscDesc)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param param
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
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

fold([], _FunState, Ctx,
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

fold([], _FunState, Ctx, PTree, {Rule, Step} = _FoldState)
    when (Rule == param orelse Rule == table) andalso is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {partition_by, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " partition by (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsql_body
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'plsql_body', _statementPragmaList} = _PTree, {plsql_body, Step} =
        _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "begin ";
             _ -> Ctx ++ " end;"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_Type, _QuerySpec1, _QuerySpec2} = _PTree,
    {query_exp, Step} = _FoldState) ->
    RT = case Step of
             start -> Ctx ++ "(";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], FunState, Ctx, {select, Clauses} = _PTree, {query_spec, Step} =
    _FoldState)
    when is_list(Clauses) ->
    ?CUSTOM_INIT(FunState, Ctx, _PTree, _FoldState),
    {StmntPred_1, ClausePred_1, _} =
        sqlparse_fold:get_stmnt_clause_pred(FunState, 1),
    RT = case Step of
             start -> lists:append([Ctx,
                 case
                     StmntPred_1 == insert_statement andalso
                         ClausePred_1 == query_spec orelse
                         StmntPred_1 == none orelse
                         StmntPred_1 == view_def of
                     true -> [];
                     _ -> "("
                 end, "select "
             ]);
             'end' -> lists:append([Ctx,
                 case
                     StmntPred_1 == insert_statement andalso
                         ClausePred_1 == query_spec orelse
                         StmntPred_1 == none orelse
                         StmntPred_1 == view_def of
                     true -> [];
                     _ -> ")"
                 end
             ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quota
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {limited, Number, Unit, Name} = _PTree,
    {quota, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 lists:append([Ctx, "quota ", binary_to_list(Number),
                     case Unit of
                         <<"">> -> [];
                         _ -> " " ++ binary_to_list(Unit)
                     end, " on ", binary_to_list(Name)]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'unlimited on', Name} = _PTree,
    {quota, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 lists:append(
                     [Ctx, "quota unlimited on ", binary_to_list(Name)]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {ref, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "references ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Value, _} = _PTree, {ref, Step} = _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "references ", binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {ref, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "references ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return & returning
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_Selection1, _Selection2} = _PTree,
    {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " ", atom_to_list(Rule), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {Rule, Step} = _FoldState)
    when Rule == return;Rule == returning ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " into ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revoke_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {revoke_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "revoke ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {revokee, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " from ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role & table & user
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {Rule, Step, Pos} = _FoldState)
    when (Rule == role orelse Rule == table orelse Rule == user) andalso
             is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {Type, RoleList} = _PTree, {role_list, Step} =
    _FoldState)
    when is_atom(Type), is_list(RoleList) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Type), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {scalar_exp, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {scalar_exp, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_opt_as_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {scalar_opt_as_exp, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {scalar_opt_as_exp, Step, Pos} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, PTree, {scalar_opt_as_exp, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'create schema authorization', Name, _SchemaElementList} = _PTree,
    {schema, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "create schema authorization ", Name]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema_element_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {schema_element_list, Step} =
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

fold([], _FunState, Ctx, PTree, {select_field, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ binary_to_list(PTree);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {select_field, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {set, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " set ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item & user_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {quotas, _Elements} = _PTree,
    {Rule, Step, _Pos} = _FoldState)
    when Rule == spec_item;Rule == user_opt ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, [{Type, Value}] = _PTree, {Rule, Step, Pos} =
    _FoldState)
    when (Rule == spec_item orelse Rule == user_opt) andalso
             (Type == 'default tablespace' orelse Type == profile orelse
                 Type == 'temporary tablespace') andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append(
                 [Ctx, atom_to_list(Type), " ", binary_to_list(Value)]);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {spec_item, Step, Pos} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ atom_to_list(PTree);
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Value1, Value2} = _PTree,
    {spec_item, Step, _Pos} = _FoldState)
    when is_atom(Value1), is_atom(Value2) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Value1), " ", atom_to_list(Value2)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {spec_item, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql & statement_pragma
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {Type, Step, Pos} = _FoldState)
    when Type == sql;Type == statement_pragma ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_SQL, Pos, {extra, <<>>}} = _PTree,
    {sql_list, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ ";";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {_SQL, Pos, {extra, Extra}} = _PTree,
    {sql_list, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, ";", binary_to_list(Extra), ";"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'start with', SearchCondition} = _PTree,
    {start_with, Step} = _FoldState)
    when is_binary(SearchCondition) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [Ctx, " start with ", binary_to_list(SearchCondition)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {start_with, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " start with ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_SQL, Pos, ';'} = _PTree,
    {statement_pragma_list, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ ";";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% storage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {storage = Rule, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, " ", atom_to_list(PTree), " ", atom_to_list(Rule)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {table, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_coll_expr
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {table_coll_expr, CollExpr} = _PTree,
    {table_coll_expr, Step} = _FoldState)
    when is_binary(CollExpr) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, "table (", binary_to_list(CollExpr)]);
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {table_coll_expr, _CollExpr} = _PTree,
    {table_coll_expr, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "table (";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_constraint_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {check, [], _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ "check (";
             {'end', other} -> Ctx ++ "),";
             {'end', _} -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {check, ConstraintName, _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} ->
                 lists:append([
                     Ctx,
                     "constraint ",
                     binary_to_list(ConstraintName),
                     " check ("
                 ]);
             {'end', other} -> Ctx ++ "),";
             {'end', _} -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'foreign key' = Type, [], _, _} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ atom_to_list(Type);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'foreign key' = Type, ConstraintName, _, _} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 "constraint ",
                 binary_to_list(ConstraintName),
                 " ",
                 atom_to_list(Type)
             ]);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, [], _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> Ctx ++ atom_to_list(Type);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Type, ConstraintName, _Value} = _PTree,
    {table_constraint_def, Step, Pos} = _FoldState)
    when is_atom(Type) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {start, _} -> lists:append([
                 Ctx,
                 "constraint ",
                 binary_to_list(ConstraintName),
                 " ",
                 atom_to_list(Type)
             ]);
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {as, Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(Table);
             'end' -> lists:append([Ctx, " ", binary_to_list(Alias)])
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {as, _Table, Alias, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> lists:append([Ctx, " ", binary_to_list(Alias)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

fold([], _FunState, Ctx, {Table, {dblink, _Value}} = _PTree,
    {table_dblink, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ binary_to_list(Table);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% target
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {target, Step, Pos} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(PTree),
                 case Pos of
                     other -> ", ";
                     _ -> []
                 end]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {target, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ", ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tbl_scope & tbl_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {_, Value} = _PTree, {Rule, Step} = _FoldState)
    when (Rule == tbl_scope orelse Rule == tbl_type) andalso is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, binary_to_list(Value), " "]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_for_null
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {is, ScalarExp, <<"null">>} = _PTree,
    {test_for_null, Step} = _FoldState)
    when is_binary(ScalarExp) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, binary_to_list(ScalarExp), " is null"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {test_for_null, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ " is null";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% then
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {then, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " then ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_cluster
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'truncate cluster', Name, _Cascade} = _PTree,
    {truncate_cluster, Step} = _FoldState)
    when is_binary(Name) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "truncate cluster ", binary_to_list(Name)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'truncate table', Table, _Materialized, _Storage} = _PTree,
    {truncate_table, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "truncate table ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx,
    {'truncate table', Table, _Materialized, _Storage, _Cascade} = _PTree,
    {truncate_table, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "truncate table ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx,
    {'truncate table', _Table, _Materialized, _Storage} = _PTree,
    {truncate_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "truncate table ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx,
    {'truncate table', _Table, _Materialized, _Storage, _Cascade} = _PTree,
    {truncate_table, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "truncate table ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {'not' = Op, Value} = _PTree, {unary, Step} =
    _FoldState)
    when is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), "(", binary_to_list(Value), ")"]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, Value} = _PTree, {unary, Step} =
    _FoldState)
    when is_atom(Op), is_binary(Value) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, atom_to_list(Op), binary_to_list(Value)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {'not' = Op, _Value} = _PTree, {unary, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, atom_to_list(Op), "("]);
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, {Op, _Value} = _PTree, {unary, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ atom_to_list(Op);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {update, Table, _, _, _} = _PTree,
    {update_statement, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "update ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {update_statement, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "update ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {user_list, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             'end' -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_opts_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {user_opts_list, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {using, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " using (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% values_or_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, {values, _} = _PTree,
    {values_or_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " values (";
             'end' -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {values_or_query_spec, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx,
    {'create view', Table, _ColumnCommalist, _QuerySpec} = _PTree,
    {view_def, Step} = _FoldState)
    when is_binary(Table) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, "create view ", binary_to_list(Table)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {view_def, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "create view ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {view_query_spec, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " as ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'when'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {'when', Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append([Ctx, " when ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {'when', Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " when ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_not_found
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {when_not_found, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, "whenever not found ", atom_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {when_not_found, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "whenever not found ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_sql_err
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {when_sql_err, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [Ctx, "whenever sqlerror ", atom_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {when_sql_err, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "whenever sqlerror ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {where_clause, Step} = _FoldState)
    when is_binary(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx, " where ", binary_to_list(PTree)]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, _PTree, {where_clause, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " where ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_current_of
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {where_current_of, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " where current of ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NO ACTION.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {Rule, _Step, _Pos}) when
    Rule == case_when_then;
    Rule == join;
    Rule == user_opt ->
    Ctx;

fold([], _FunState, Ctx, _PTree, {Rule, _Step}) when
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
    Rule == fields;
    Rule == 'fun';
    Rule == function_ref;
    Rule == function_ref_list_list;
    Rule == hierarchical_query;
    Rule == in_predicate;
    Rule == join_list;
    Rule == jpparse;
    Rule == like_predicate;
    Rule == param;
    Rule == prior;
    Rule == procedure_call;
    Rule == query_spec;
    Rule == query_spec_jpparse;
    Rule == quotas;
    Rule == scalar_exp_commalist;
    Rule == scalar_opt_as_exp;
    Rule == sql_list_list;
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

fold([], _FunState, _Ctx, PTree, {Rule, Step, Pos} = _FoldState) ->
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
fold([], _FunState, _Ctx, PTree, {Rule, Step} = _FoldState) ->
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
