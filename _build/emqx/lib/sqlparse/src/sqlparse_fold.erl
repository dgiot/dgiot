%% -----------------------------------------------------------------------------
%%
%% sqlparse_fold.erl: SQL - unparsing utilities.
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

-module(sqlparse_fold).

-export([
    bottom_up/3,
    fold/6,
    get_ptree_max_depth_set/1,
    get_stmnt_clause_curr/1,
    get_stmnt_clause_pred/2,
    top_down/3
]).

-define(NODEBUG, true).

-include("sql_lex.hrl").
-include("sqlparse_fold.hrl").

-type fold_type() :: top_down | bottom_up.

-export_type([fold_type/0]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bottom-up processing.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec bottom_up(Module :: atom(), SQLParseTree :: list()|tuple(), Params :: any()) -> any().
bottom_up(Module, SQLParseTree, Params) ->
    ?D("Start~n Module: ~p~n SQL: ~p~n Params: ~p~n",
        [Module, SQLParseTree, Params]),
    fold_state_common(Module, bottom_up, SQLParseTree, Params).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-down processing.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec top_down(Module :: atom(), SQLParseTree :: list()|tuple(), Params :: any()) -> any().
top_down(Module, SQLParseTree, Params) ->
    ?D("Start~n Module: ~p~n SQL: ~p~n Params: ~p~n",
        [Module, SQLParseTree, Params]),
    fold_state_common(Module, top_down, SQLParseTree, Params).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% common processing.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_state_common(Module, FoldType, SQLParseTree, Params) ->
    ParseTree = case SQLParseTree of
                    [PT | _] when is_tuple(PT) -> SQLParseTree;
                    PT when is_tuple(PT) -> SQLParseTree;
                    _ -> {ok, PT} = sqlparse:parsetree(SQLParseTree),
                        PT
                end,
    ParamsInitialized = Module:init(Params),
    {ok, Sql} = fold(FoldType, fun Module:fold/5, ParamsInitialized, #fstate{},
        ParseTree, []),
    Module:finalize(ParamsInitialized, Sql).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Folder starting method.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(FType :: fold_type(), Fun :: fun(), LOpts :: term(),
    FunState :: tuple(), PTree :: list()|tuple(), Ctx :: term()) ->
    Ctx :: term().
fold(FType, Fun, LOpts, FunState, PTree, CtxIn) ->
    ?D("Start~n FType: ~p~n LOpts: ~p~n FunState: ~p~n PTree: ~p~n CtxIn: ~p~n",
        [FType, LOpts, FunState, PTree, CtxIn]),
    RT = fold_i(FType, Fun, LOpts, FunState, CtxIn, PTree),
    ?D("~n CtxOut: ~p~n", [RT]),
    {ok, RT}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Folder methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ( & in_in
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, PTree})
    when Rule == "(";Rule == in_in ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_op
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {all_or_any_op =
    Rule, Op, AnyAllSome, SubQuery}) ->
    ?FOLD_INIT(FunState, Ctx, {Op, AnyAllSome, SubQuery}),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, {Op, AnyAllSome, SubQuery},
            {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, SubQuery),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, {Op, AnyAllSome, SubQuery},
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all_or_any_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {Op, ScalarExp, {AnyAllSome, [SubQuery]}} = PTree)
    when (Op == '=' orelse Op == '!=' orelse Op == '^=' orelse Op == '<>' orelse
    Op == '<' orelse Op == '>' orelse Op == '<=' orelse Op == '>=') andalso
             (AnyAllSome == all orelse AnyAllSome == any orelse
                 AnyAllSome == some) andalso is_tuple(SubQuery) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = all_or_any_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1,
        {all_or_any_op, Op, AnyAllSome, SubQuery}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alter_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'alter user', User, {spec, SpecList}} = PTree)
    when is_binary(User), is_list(SpecList) ->
    Rule = alter_user_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, spec_item,
            SpecList),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'alter user', User, {Type, _} = Spec} = PTree)
    when Type == 'grant connect';Type == 'revoke connect';Type == 'spec' ->
    Rule = alter_user_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {user_list, User}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, Spec),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% anchor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {anchor = Rule, Anchor, Bracket} =
    _PTree) ->
    ?FOLD_INIT(FunState, Ctx, _PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, {Anchor, Bracket},
        {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Anchor) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Anchor)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, {Anchor, Bracket},
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% as
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Value, Alias})
    when (Rule == as orelse Rule == explicit_as) andalso is_binary(Alias) ->
    ?FOLD_INIT(FunState, Ctx, {Value, Alias}),
    NewCtxS = Fun(LOpts, FunState, Ctx, {Value, Alias},
        {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Value) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Value)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, {Value, Alias},
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assignment
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {assignment = Rule, Pos,
    {'=', _Column, ScalarOptAsExp} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS,
        {scalar_opt_as_exp, ScalarOptAsExp}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assignment_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {':=' = Op, Variable, ScalarOptAsExp} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = assignment_statement,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Variable) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Variable)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {binary, Op}),
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2,
        {scalar_opt_as_exp, ScalarOptAsExp}),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% base_table_element_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {base_table_element_commalist = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS,
        base_table_element, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_and & between_between
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, PTree})
    when Type == between_and; Type == between_between ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% between_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {between, ScalarExp1, ScalarExp2, ScalarExp3} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = between_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp1) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp1)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtx1,
        {between_between, ScalarExp2}),
    NewCtx3 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtx2,
        {between_and, ScalarExp3}),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'case'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {'case' = Rule, ScalarOptAsExpr, CaseWhenThenList, Else})
    when is_list(CaseWhenThenList) ->
    ?FOLD_INIT(FunState, Ctx, {ScalarOptAsExpr, CaseWhenThenList, Else}),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, {ScalarOptAsExpr, CaseWhenThenList, Else},
            {Rule, get_start_end(FType, start)}),
    NewCtx1 = case ScalarOptAsExpr of
                  <<>> -> NewCtxS;
                  S when is_binary(S) -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      {scalar_opt_as_exp, ScalarOptAsExpr})
              end,
    NewCtx2 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, case_when_then),
            NewCtx1, {case_when_then_list, CaseWhenThenList}),
    NewCtx3 = case Else of
                  {} -> NewCtx2;
                  E when is_binary(E) -> NewCtx2;
                  _ ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtx2, {else, Else})
              end,
    NewCtxE =
        Fun(LOpts, FunState, NewCtx3, {ScalarOptAsExpr, CaseWhenThenList, Else},
            {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% case_when_then
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {case_when_then = Rule, Pos, {SearchCondition, ScalarOptAsExp} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {'when', SearchCondition}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, {then, ScalarOptAsExp}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% case_when_then_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {case_when_then_list = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, case_when_then,
            PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {check = Rule, _, PTree})
    when is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check & ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, PTree})
    when (Type == check orelse Type == ref) andalso is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check & default & procedure_call
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {Type = Rule, {as, {{'fun', _, _}, JSON, []}, Alias} = PTree})
    when
    (Type == check orelse Type == default orelse Type == procedure_call) andalso
        is_tuple(JSON) andalso is_binary(Alias) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {function_ref, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule,
    {{'fun', _, _}, JSON, []} = PTree})
    when
    (Type == check orelse Type == default orelse Type == procedure_call) andalso
        is_tuple(JSON) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {function_ref, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, {'fun', _, _} = PTree})
    when Type == check;Type == default; Type == procedure_call ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {function_ref, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {check = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {check = Rule, _, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% close_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {close, Cursor} = PTree) ->
    Rule = close_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, Cursor),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cols
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {cols = _Rule, [] = _PTree}) ->
    Ctx;
fold_i(FType, Fun, LOpts, FunState, Ctx, {cols = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, column, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {column = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {column_commalist = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, column, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def (<- base_table_element)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {base_table_element, Pos,
    {Column, DataType, Opts} = PTree})
    when is_binary(Column) andalso
             (is_binary(DataType) orelse is_tuple(DataType)) andalso
             is_list(Opts) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = column_def,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS,
        {data_type, DataType, Opts}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {column_def_list = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, column_def_opt,
            PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_def_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {column_def_opt = Rule, Pos,
    {Type, _} = PTree})
    when Type == check;Type == default;Type == ref ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {column_def_opt = Rule, Pos, PTree})
    when is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% column_ref_commalist & ref_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule = Rule, PTree})
    when Rule == column_ref_commalist;Rule == ref_commalist ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, column, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% commit_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == commit;Type == 'commit work' ->
    Rule = commit_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comparison_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, {prior, ScalarExp1}, ScalarExp2} =
    PTree)
    when (Op == '=' orelse Op == '!=' orelse Op == '^=' orelse Op == '<>' orelse
    Op == '<' orelse Op == '>' orelse Op == '<=' orelse Op == '>=') ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = comparison_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp1) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp1)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {Op, ScalarExp2}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, ScalarExp1, {prior, ScalarExp2}} =
    PTree)
    when (Op == '=' orelse Op == '!=' orelse Op == '^=' orelse Op == '<>' orelse
    Op == '<' orelse Op == '>' orelse Op == '<=' orelse Op == '>=') ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = comparison_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp1) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp1)
              end,
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, {Op, {prior, ScalarExp2}}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, ScalarExp} = PTree)
    when Op == '='; Op == '!='; Op == '^='; Op == '<>';    Op == '<'; Op == '>';
         Op == '<='; Op == '>=' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = comparison_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {prior = Rule, ScalarExp} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% connect_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {'connect by', _NoCycle, SearchCondition} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = connect_by,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(SearchCondition) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      SearchCondition)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create index', _CreateIndexOpts, IndexName, TableAlias, CreateIndexSpec,
        CreateIndexNorm, CreateIndexFilter} = PTree) ->
    Rule = create_index_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS,
            {create_index_name, IndexName}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, table_alias),
            NewCtx1, {create_index_table, TableAlias}),
    NewCtx3 = case CreateIndexSpec of
                  [] -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2,
                      {create_index_spec, CreateIndexSpec})
              end,
    NewCtx4 = case CreateIndexNorm of
                  {} -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3,
                      {norm_with, CreateIndexNorm})
              end,
    NewCtx5 = case CreateIndexFilter of
                  {} -> NewCtx4;
                  _ ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtx4,
                          {filter_with, CreateIndexFilter})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx5, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_name
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_index_name = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_index_spec = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS,
        create_index_spec_column, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_spec_column
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_index_spec_column =
    Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_index_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_index_table = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_opts
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_opts = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_int_rule(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'create role', Value} = PTree)
    when is_binary(Value) ->
    Rule = create_role_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create table', Table, Fields, Opts} =
        PTree) ->
    Rule = create_table_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {create_opts, Opts}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1,
            {create_table_table, Table}),
    NewCtx3 = fold_i(FType, Fun, LOpts,
        set_state_rule(FunState, base_table_element_commalist), NewCtx2,
        {base_table_element_commalist, Fields}),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_table_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {create_table_table = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create user', User, Identified, UserOptsList} = PTree)
    when is_binary(User), is_tuple(Identified), is_list(UserOptsList) ->
    Rule = create_user_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {identified, Identified}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1,
            {user_opts_list, UserOptsList}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cur
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {cur = Rule, Cursor} = PTree)
    when is_list(Cursor) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {cursor_def = Rule, {cur, _Name} =
    Cursor, QuerySpec}) ->
    FunState =
        ?FOLD_INIT_STMNT(FunStateIn, Ctx, {{cur, _Name} = Cursor, QuerySpec},
            Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, {{cur, _Name} = Cursor, QuerySpec},
        {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, Cursor),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1,
            {cursor_query_spec, QuerySpec}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, {{cur, _Name} = Cursor, QuerySpec},
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cursor_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {cursor_query_spec = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% data_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {data_type = Rule, DataType, Opts}) ->
    ?FOLD_INIT(FunState, Ctx, {DataType, Opts}),
    NewCtxS = Fun(LOpts, FunState, Ctx, {DataType, Opts},
        {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {column_def_list, Opts}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, {DataType, Opts},
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {dblink = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% default
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {default = Rule, PTree})
    when is_atom(PTree); is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {delete, Table, {where_current_of, _Cursor} = WhereCurrentOf, Returning} =
        PTree) ->
    Rule = delete_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, WhereCurrentOf),
    NewCtx3 = case Returning of
                  {returning, {}} -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, Returning)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {delete, Table, WhereClause, Returning} = PTree) ->
    Rule = delete_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = case WhereClause of
                  [] -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1, WhereClause)
              end,
    NewCtx3 = case Returning of
                  {returning, {}} -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, Returning)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop cluster', Name, {}} = PTree)
    when is_binary(Name) ->
    Rule = drop_cluster_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop cluster', _Name, {DropClusterExtensions}} = PTree) ->
    Rule = drop_cluster_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropClusterExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_cluster_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == 'including tables';
         Type == 'including tables cascade constraints' ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_context_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop context', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_context_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop database'} = PTree) ->
    Rule = drop_database_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_database_link_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop database link', Name, {}} =
    PTree)
    when is_binary(Name) ->
    Rule = drop_database_link_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop database link', Name, public} = PTree)
    when is_binary(Name) ->
    Rule = drop_database_link_public_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_directory_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop directory', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_directory_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_function_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop function', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_function_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop index', IndexName, []} =
    PTree)
    when is_binary(IndexName) ->
    Rule = drop_index_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop index', IndexName, [], {DropIndexExtensions}} = PTree)
    when is_binary(IndexName) ->
    Rule = drop_index_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropIndexExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop index', _IndexName, Table} =
    PTree) ->
    Rule = drop_index_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {drop_index_from, Table}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop index', _IndexName, Table, {DropIndexExtensions}} = PTree) ->
    Rule = drop_index_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {drop_index_from, Table}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, DropIndexExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == 'deferred invalidation';
         Type == 'force deferred invalidation';
         Type == 'force immediate invalidation';
         Type == 'force';
         Type == 'immediate invalidation';
         Type == 'online deferred invalidation';
         Type == 'online force deferred invalidation';
         Type == 'online force immediate invalidation';
         Type == 'online force';
         Type == 'online immediate invalidation';
         Type == 'online' ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_index_from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {drop_index_from = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop materialized view', Name, {}} = PTree)
    when is_binary(Name) ->
    Rule = drop_materialized_view_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop materialized view', _Name, 'preserve table' =
        MaterializedViewExtensions} = PTree) ->
    Rule = drop_materialized_view_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS,
            MaterializedViewExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_materialized_view_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == 'preserve table' ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_package_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop package', Name, {}} = PTree)
    when is_binary(Name) ->
    Rule = drop_package_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop package', Name, body} = PTree)
    when is_binary(Name) ->
    Rule = drop_package_body_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_procedure_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop procedure', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_procedure_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop profile', Name, {}} = PTree)
    when is_binary(Name) ->
    Rule = drop_profile_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop profile', _Name, DropProfileExtensions} = PTree) ->
    Rule = drop_profile_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropProfileExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_profile_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == cascade ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_role_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop role', Role} = PTree)
    when is_binary(Role) ->
    Rule = drop_role_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_sequence_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop sequence', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_sequence_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_synonym_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop synonym', Name, {}, {}} =
    PTree)
    when is_binary(Name) ->
    Rule = drop_synonym_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop synonym', Name, {}, DropSynonymExtensions} = PTree)
    when is_binary(Name) ->
    Rule = drop_synonym_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropSynonymExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop synonym', Name, public, {}} =
    PTree)
    when is_binary(Name) ->
    Rule = drop_synonym_public_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop synonym', Name, public, DropSynonymExtensions} = PTree)
    when is_binary(Name) ->
    Rule = drop_synonym_public_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropSynonymExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop table', Tables, Exists, {}, _Name} = PTree) ->
    Rule = drop_table_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {exists, Exists}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, Tables),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop table', Tables, Exists, {DropTableExtensions}, _Name} = PTree) ->
    Rule = drop_table_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {exists, Exists}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, Tables),
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2, DropTableExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_table_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == 'cascade constraints';
         Type == 'cascade constraints purge';
         Type == purge ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_tablespace_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop tablespace', Name, {}} =
    PTree)
    when is_binary(Name) ->
    Rule = drop_tablespace_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop tablespace', _Name, {DropTablespaceExtensions}} = PTree) ->
    Rule = drop_tablespace_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropTablespaceExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_tablespace_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type ==
             'drop quota including contents and datafiles cascade constraints';
    Type == 'drop quota including contents cascade constraints';
    Type == 'drop quota including contents keep datafiles cascade constraints';
    Type == 'drop quota including contents';
    Type == 'drop quota';
    Type == 'including contents';
    Type == 'including contents and datafiles cascade constraints';
    Type == 'including contents cascade constraints';
    Type == 'including contents keep datafiles cascade constraints';
    Type == 'keep quota including contents and datafiles cascade constraints';
    Type == 'keep quota including contents cascade constraints';
    Type == 'keep quota including contents keep datafiles cascade constraints';
    Type == 'keep quota including contents';
    Type == 'keep quota' ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_trigger_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop trigger', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_trigger_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop type', Name, {}} = PTree)
    when is_binary(Name) ->
    Rule = drop_type_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop type', _Name, DropTypeExtensions} = PTree) ->
    Rule = drop_type_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, DropTypeExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_extensions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == force;
         Type == validate ->
    Rule = drop_extensions,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_type_body_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop type body', Name} = PTree)
    when is_binary(Name) ->
    Rule = drop_type_body_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_user_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop user', User, _Cascade} =
    PTree)
    when is_binary(User) ->
    Rule = drop_user_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% drop_view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'drop view', Table, {}} = PTree) ->
    Rule = drop_view_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'drop view', Table, DropViewExtensions} = PTree) ->
    Rule = drop_view_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, DropViewExtensions),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% else
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {else = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS,
            {scalar_opt_as_exp, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% existence_test
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {exists, PTree})
    when PTree /= {}, is_tuple(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = existence_test,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exists
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {exists = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fetch_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {fetch, Cursor, Into} = PTree) ->
    Rule = fetch_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, Cursor),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, Into),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fields
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {fields = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, set_state_clause(FunState, Rule),
            NewCtxS, select_field, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filter_with & norm_with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, {_, _} = PTree})
    when Type == filter_with;Type == norm_with ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {from = Rule, Pos, {Table, Join} =
    PTree})
    when Table /= select, is_list(Join) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {join_list, Join}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {from = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {from = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, set_state_clause(FunState, Rule),
            NewCtxS, from, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'fun'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {'fun' = Rule, _, _} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {function_ref, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {fun_arg = Rule, Pos, PTree})
    when is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {fun_arg = Rule, Pos, {Type, Value} = PTree})
    when Type == all;Type == distinct ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(Value) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Value)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {fun_arg = Rule, Pos, {'fun', _, _} =
    PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {function_ref, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {fun_arg = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts,
        FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1}, NewCtxS,
        PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun_arg_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {fun_arg_commalist = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, fun_arg,
        PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% function_ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {function_ref = Rule, {as, {FunRef, JSON, []}, Alias} = PTree})
    when is_tuple(JSON); is_binary(Alias) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtxS,
        {function_ref, FunRef}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {jpparse, JSON}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {function_ref = Rule, {FunRef, JSON, []} = PTree})
    when is_tuple(JSON) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtxS,
        {function_ref, FunRef}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {jpparse, JSON}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {function_ref = Rule, {'fun', _Name, []} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {function_ref = Rule, {'fun', _Name, FunArgs} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtxS,
        {fun_arg_commalist, FunArgs}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goto
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {goto = Rule, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'grant connect' & 'revoke connect'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {Rule, {{Type, Roles} = RoleList, ProxyAuthReq} = PTree})
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             (Type == 'with role' orelse Type == 'with role all except') andalso
             is_list(Roles) andalso is_atom(ProxyAuthReq) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {role_list, RoleList}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, {Type, Roles} = PTree})
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             (Type == 'with role' orelse Type == 'with role all except') andalso
             is_list(Roles) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {role_list, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, {Value1, Value2} = PTree})
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(Value1) andalso is_atom(Value2) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, PTree})
    when (Rule == 'grant connect' orelse Rule == 'revoke connect') andalso
             is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grant_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {grant, Privileges, OnObjClause, Grantee, GrantOption} = PTree) ->
    Rule = grant_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, privilege,
            Privileges),
    NewCtx2 = case OnObjClause of
                  {on, <<"">>} -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1,
                      {on_obj_clause, OnObjClause})
              end,
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2, {grantee, Grantee}),
    NewCtx4 = case GrantOption of
                  '' -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3,
                      {with_grant_option, GrantOption})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {grantee = Rule, {to, GranteeRevokeeCommalist} = PTree})
    when is_list(GranteeRevokeeCommalist) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS,
        grantee_revokee, GranteeRevokeeCommalist),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grantee_revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {grantee_revokee = Rule, Pos, PTree})
    when is_atom(PTree);is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {grantee_revokee = Rule, _Pos, {'identified by', Name, Password} = PTree})
    when is_binary(Name);is_binary(Password) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'group by' (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {'group by', [] = _PTree}) ->
    ?FOLD_INIT(_FunState, Ctx, _PTree),
    ?FOLD_RESULT(Ctx);
fold_i(FType, Fun, LOpts, FunState, Ctx, {'group by', PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = group_by,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_clause(FunState, Rule), NewCtxS,
            {column_ref_commalist, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% having (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {having, {} = _PTree}) ->
    ?FOLD_INIT(_FunState, Ctx, _PTree),
    ?FOLD_RESULT(Ctx);
fold_i(FType, Fun, LOpts, FunState, Ctx, {having = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts,
                      set_state_clause(FunState, Rule), NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'hierarchical query' (list)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {'hierarchical query', {} =
    _PTree}) ->
    ?FOLD_INIT(_FunState, Ctx, _PTree),
    ?FOLD_RESULT(Ctx);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {'hierarchical query', {{'start with', _} = StartWith, ConnectBy}} =
        PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = hierarchical_query,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_clause(FunState, Rule), NewCtxS,
            StartWith),
    NewCtx2 = fold_i(FType, Fun, LOpts,
        set_state_clause(FunState, Rule), NewCtx1, ConnectBy),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {'hierarchical query', {{'connect by', _, _} = ConnectBy, StartWith}} =
        PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = hierarchical_query,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_clause(FunState, Rule), NewCtxS,
            ConnectBy),
    NewCtx2 = fold_i(FType, Fun, LOpts,
        set_state_clause(FunState, Rule), NewCtx1, StartWith),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hints
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {hints = Rule, PTree})
    when is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% identified
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {identified = Rule, PTree})
    when is_tuple(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {in, ScalarExp, {list, ScalarExpCommalist}} = PTree)
    when is_list(ScalarExpCommalist) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = in_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1,
        {in_in, {scalar_exp_commalist, ScalarExpCommalist}}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {in, ScalarExp, SubQuery} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = in_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtx1,
        {in_in, SubQuery}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {insert, Table, ColumnCommalist, ValuesOrQuerySpec, Returning} =
        PTree) ->
    Rule = insert_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = case ColumnCommalist of
                  {} -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1,
                      ColumnCommalist)
              end,
    NewCtx3 = case ValuesOrQuerySpec of
                  {} -> NewCtx2;
                  {values, _} -> fold_i(FType, Fun, LOpts,
                      set_state_clause(FunState, values),
                      NewCtx2, {values_or_query_spec, ValuesOrQuerySpec});
                  _ -> fold_i(FType, Fun, LOpts,
                      set_state_clause(FunState, query_spec),
                      NewCtx2, {values_or_query_spec, ValuesOrQuerySpec})
              end,
    NewCtx4 = case Returning of
                  {returning, {}} -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3, Returning)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% into
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {into = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, target, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {join = Rule, Pos,
    {{JoinType, QueryPartitionClause1, Natural}, JoinRef, QueryPartitionClause2, JoinOnOrUsingClause} =
        PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case QueryPartitionClause1 of
                  {} -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      QueryPartitionClause1)
              end,
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, {keyword, lists:append([
            case Natural of
                {} -> [];
                _ -> "natural "
            end,
            atom_to_list(JoinType),
            " ",
            atom_to_list(join)
        ])}),
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2, {table, JoinRef}),
    NewCtx4 = case QueryPartitionClause2 of
                  {} -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3,
                      QueryPartitionClause2)
              end,
    NewCtx5 = case JoinOnOrUsingClause of
                  {} -> NewCtx4;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx4,
                      JoinOnOrUsingClause)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx5, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {join = Rule, Pos,
    {_JoinType, JoinRef, JoinOnOrUsingClause} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(JoinRef) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, JoinRef)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, JoinOnOrUsingClause),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {join = Rule, Pos,
    {_JoinType, JoinRef} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(JoinRef) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, JoinRef)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {join_list = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, join, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% jpparse (JSON parser hooking)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {jpparse = Rule, {Op, Columns, _} = PTree})
    when Op == '{}'; Op =:= '[]' ->
    ?FOLD_INIT(FunState, Ctx, PTree),

    {ok, JPPath} = jpparse_fold:string(PTree),
    JPPathList = binary_to_list(JPPath),

    JSON = case Columns of
               _ when is_tuple(Columns) ->
                   Target = decompose_tuple(Columns),
                   lists:append([
                       string:trim(decompose_tuple(Columns), trailing, "."),
                       "|",
                       string:sub_string(JPPathList, length(Target) + 1),
                       "|"
                   ]);
               empty ->
                   lists:append([
                       "|",
                       JPPathList,
                       "|"]);
               _ ->
                   Target =
                       string:trim(binary_to_list(Columns), trailing, "."),
                   lists:append([
                       Target,
                       "|",
                       string:sub_string(JPPathList, length(Target) + 1),
                       "|"
                   ])
           end,

    NewCtxS =
        Fun(LOpts, FunState, Ctx, JSON, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, JSON,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, _Columns, _} = PTree)
    when Op == '{}'; Op =:= '[]' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = jpparse,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {Rule, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {jpparse = Rule, {Op, _, _} = PTree})
    when Op =:= ':'; Op =:= '::'; Op =:= '#' ->
    ?FOLD_INIT(FunState, Ctx, PTree),

    {ok, JPPath} = jpparse_fold:string(PTree),
    JPPathList = binary_to_list(JPPath),
    Others = decompose_tuple(PTree),

    JSON = lists:append([
        string:trim(Others, trailing, "."),
        "|",
        string:sub_string(JPPathList, length(Others) + 1),
        "|"
    ]),

    NewCtxS =
        Fun(LOpts, FunState, Ctx, JSON, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, JSON,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, _, _} = PTree)
    when Op =:= ':'; Op =:= '::'; Op =:= '#' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = jpparse,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, {Rule, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% jpparse (JSONPath anchors)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Anchor, {Op, _, _} = JSON, Bracket} =
    PTree)
    when
    (Op =:= '{}' orelse Op =:= '[]' orelse Op =:= ':' orelse Op =:= '::' orelse
        Op =:= '#') andalso (Bracket =:= [] orelse Bracket =:= '(') ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = jpparse,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {anchor, Anchor, Bracket}),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, JSON),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & privilege
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Pos, PTree})
    when (Rule == keyword orelse Rule == privilege) andalso is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keyword & with_grant_option & with_revoke_option
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, PTree})
    when (Rule == keyword orelse Rule == with_grant_option orelse
    Rule == with_revoke_option) andalso
             (is_atom(PTree) orelse is_list(PTree)) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_escape & like_like
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, PTree})
    when Type == like_escape;Type == like_like ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% like_predicate
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {like, ScalarExp1, ScalarExp2, Escape} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = like_predicate,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp1) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp1)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtx1,
        {like_like, ScalarExp2}),
    NewCtx3 = case Escape of
                  <<>> -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2,
                      {like_escape, Escape})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% materialized
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {'materialized view log', Value} =
    PTree)
    when is_atom(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = materialized,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {on = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% on_obj_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {on_obj_clause = Rule,
    {Target, Value} = PTree})
    when (Target == on orelse Target == 'on directory') ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Value) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Value)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {open, Cursor} = PTree) ->
    Rule = open_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, Cursor),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {opt = Rule, PTree})
    when is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order_by_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {'order by', [] = _PTree}) ->
    ?FOLD_INIT(_FunState, Ctx, _PTree),
    ?FOLD_RESULT(Ctx);
fold_i(FType, Fun, LOpts, FunState, Ctx, {'order by', PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = order_by_clause,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, set_state_clause(FunState, Rule),
            NewCtxS, ordering_spec, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ordering_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {ordering_spec = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {ordering_spec, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {ordering_spec = Rule,
    {ScalarExp, _AscDesc} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% param
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {indicator, {param = Rule, Value1}, {param = Rule, Value2}} = PTree)
    when is_binary(Value1), is_binary(Value2) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {{param = Rule, Value1}, {param = Rule, Value2}} = PTree)
    when is_binary(Value1), is_binary(Value2) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {param = Rule, PTree})
    when is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition_by
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {partition_by = Rule, ScalarExpCommalist} = PTree)
    when is_list(ScalarExpCommalist) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS,
        {scalar_exp_commalist, ScalarExpCommalist}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsql_body
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'plsql_body', StatementPragmaList} =
    PTree)
    when is_list(StatementPragmaList) ->
    Rule = plsql_body,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, StatementPragmaList),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedure_call
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {'call procedure', FunctionRef} =
    PTree) ->
    Rule = procedure_call,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_clause(FunState, call_procedure),
            NewCtxS, {procedure_call, FunctionRef}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {Type, {select, _} = QuerySpec1, {select, _} = QuerySpec2} = PTree)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    Rule = query_exp,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(
        FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1},
        select_left), NewCtxS, QuerySpec1),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {keyword, Type}),
    NewCtx3 =
        fold_i(FType, Fun, LOpts, set_state_rule(
            FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1},
            select_right),
            NewCtx2, QuerySpec2),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {Type, {select, _} = QuerySpec1, QuerySpec2} = PTree)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    Rule = query_exp,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(
        FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1},
        select_left), NewCtxS, QuerySpec1),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {keyword, Type}),
    NewCtx3 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, union_right),
            NewCtx2, QuerySpec2),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {Type, QuerySpec1, {select, _} = QuerySpec2} = PTree)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    Rule = query_exp,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, union_left), NewCtxS,
            QuerySpec1),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {keyword, Type}),
    NewCtx3 = fold_i(FType, Fun, LOpts, set_state_rule(
        FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1},
        select_right), NewCtx2, QuerySpec2),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {Type, QuerySpec1, QuerySpec2} =
    PTree)
    when Type == intersect;Type == minus;Type == union;Type == 'union all' ->
    Rule = query_exp,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, union_left), NewCtxS,
            QuerySpec1),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {keyword, Type}),
    NewCtx3 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, union_right),
            NewCtx2, QuerySpec2),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {select, Clauses} = PTree)
    when is_list(Clauses) ->
    Rule = query_spec,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_int_rule(FType, Fun, LOpts, FunState, NewCtxS, Clauses),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quota
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {quota = Rule, Pos, {limited, Number, Unit, Name} = PTree})
    when is_binary(Number), is_binary(Unit), is_binary(Name) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {quota = Rule, Pos, {'unlimited on', Name} = PTree})
    when is_binary(Name) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quotas
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {quotas = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, quota, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ref
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {ref = Rule, {Value, Elements} =
    PTree})
    when is_list(Elements) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Value) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Value)
              end,
    NewCtx2 =
        fold_i(FType, Fun, LOpts, FunState, NewCtx1, {ref_commalist, Elements}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {ref = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return & returning
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Selection1, Selection2})
    when Rule == return;Rule == returning ->
    ?FOLD_INIT(FunState, Ctx, {Selection1, Selection2}),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, {Selection1, Selection2},
            {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, select_field,
            Selection1),
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {Rule, Selection2}),
    NewCtxE =
        Fun(LOpts, FunState, NewCtx2, {Selection1, Selection2},
            {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, PTree})
    when Rule == return;Rule == returning ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, select_field,
            PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revoke_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {revoke, Privileges, OnObjClause, Revokee, RevokeOption} = PTree) ->
    Rule = revoke_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, privilege,
            Privileges),
    NewCtx2 = case OnObjClause of
                  {on, <<"">>} -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1,
                      {on_obj_clause, OnObjClause})
              end,
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2, {revokee, Revokee}),
    NewCtx4 = case RevokeOption of
                  '' -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3,
                      {with_revoke_option, RevokeOption})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% revokee
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {revokee = Rule, {from, GranteeRevokeeCommalist} = PTree})
    when is_list(GranteeRevokeeCommalist) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS,
        grantee_revokee, GranteeRevokeeCommalist),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role &  table & user
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Pos, PTree})
    when (Rule == role orelse Rule == table orelse Rule == user) andalso
             is_binary(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% role_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {role_list = Rule, {_Type, Roles} =
    PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, role, Roles),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rollback_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, Type = PTree)
    when Type == rollback;Type == 'rollback work' ->
    Rule = rollback_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {scalar_exp = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_exp_commalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {scalar_exp_commalist =
    Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, scalar_exp,
            PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scalar_opt_as_exp
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {scalar_opt_as_exp =
    Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case PTree of
                  P when is_binary(P) -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {scalar_opt_as_exp = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case PTree of
                  P when is_binary(P) -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create schema authorization', Name, []} = PTree)
    when is_list(Name) ->
    Rule = schema,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create schema authorization', Name, SchemaElementList} = PTree)
    when is_list(Name), is_list(SchemaElementList) ->
    Rule = schema,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS,
        {schema_element_list, SchemaElementList}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% schema_element_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {schema_element_list = Rule, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, sql, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% select_field
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {select_field = Rule, Pos, {as, {'case', _, _, _}, _} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {select_field = Rule, Pos, {'case', _, _, _} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {select_field = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      {scalar_opt_as_exp, PTree})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {set = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, assignment,
            PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item & user_opt
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Pos, [{quotas, _Elements} =
    PTree]})
    when Rule == spec_item; Rule == user_opt ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, Pos, [{Type, Value}] = PTree})
    when (Rule == spec_item orelse Rule == user_opt) andalso
             (Type == 'default tablespace' orelse Type == profile orelse
                 Type == 'temporary tablespace') andalso is_binary(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% spec_item
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {spec_item = Rule, Pos, PTree})
    when is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {spec_item = Rule, Pos, {Value1, Value2} = PTree})
    when is_atom(Value1), is_atom(Value2) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx,
    {spec_item = Rule, Pos, {Type, Roles} = PTree})
    when
    (Type == 'default role' orelse Type == 'default role all except') andalso
        is_list(Roles) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {role_list, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {spec_item = Rule, Pos, PTree})
    when is_tuple(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 =
        fold_i(FType, Fun, LOpts, FunState, NewCtxS, {identified, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql  statement_pragma
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Type = Rule, Pos, PTree})
    when (is_atom(PTree) orelse is_tuple(PTree)) andalso
             (Type == sql orelse Type == statement_pragma) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, Pos, {extra, _}} = PTree)
    when is_atom(SQL);is_tuple(SQL) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = sql_list,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, SQL),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sql_list_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, [{_, {extra, _}} | _] = PTree)
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = sql_list_list,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_sql(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start_with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {'start with', SearchCondition} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = start_with,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(SearchCondition) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      SearchCondition)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, Pos, ';'} = PTree)
    when is_atom(SQL);is_tuple(SQL) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = statement_pragma_list,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, SQL),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement_pragma_list_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, [{_, ';'} | _] = PTree)
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = statement_pragma_list_list,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_sql(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% storage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {storage = Rule, PTree})
    when is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {table = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {table = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_coll_expr
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {table_coll_expr = Rule, CollExpr} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(CollExpr) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, CollExpr)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_constraint_def (<- base_table_element)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {base_table_element, Pos,
    {check, _, _} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = table_constraint_def,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {base_table_element, Pos,
    {'foreign key', _, Columns, References} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = table_constraint_def,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtxS,
        {column_commalist, Columns}),
    NewCtx2 =
        fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtx1,
            References),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {base_table_element, Pos,
    {Type, _, Columns} = PTree})
    when Type == 'primary key';Type == 'unique' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = table_constraint_def,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = fold_i(FType, Fun, LOpts, set_state_rule(FunState, Rule), NewCtxS,
        {column_commalist, Columns}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% table_dblink
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {as, Table, _Alias, {dblink, _Value} = DBLink} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = table_dblink,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, DBLink),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Table, {dblink, _Value} = DBLink} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = table_dblink,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, DBLink),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tables
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {tables = Rule, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, table, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% target
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {target = Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, get_start_end(FType, start), Pos}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end'), Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tbl_scope
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {scope, _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = tbl_scope,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tbl_type
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {type, _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = tbl_type,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_for_null
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {is, ScalarExp, <<"null">>} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = test_for_null,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(ScalarExp) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, ScalarExp)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% then
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {then = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS,
        {scalar_opt_as_exp, PTree}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_cluster
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'truncate cluster', Name, Cascade} = PTree) ->
    Rule = truncate_cluster,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Name) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Name)
              end,
    NewCtx2 = case Cascade of
                  {} -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1, Cascade)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% truncate_table
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'truncate table', Table, Materialized, Storage} = PTree) ->
    Rule = truncate_table,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = case Materialized of
                  {} -> NewCtx1;
                  _ ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtx1, Materialized)
              end,
    NewCtx3 = case Storage of
                  {} -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, Storage)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'truncate table', Table, Materialized, Storage, Cascade} = PTree) ->
    Rule = truncate_table,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = case Materialized of
                  {} -> NewCtx1;
                  _ ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtx1, Materialized)
              end,
    NewCtx3 = case Storage of
                  {} -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, Storage)
              end,
    NewCtx4 = fold_i(FType, Fun, LOpts, FunState, NewCtx3,
        {keyword, atom_to_list(Cascade)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_statement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {update, Table, AssignmentCommalist, {where_current_of, _Cursor} =
        WhereCurrentOf, Returning} = PTree) ->
    Rule = update_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts,
        set_state_clause(FunState, assignment_commalist),
        NewCtx1, AssignmentCommalist),
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2, WhereCurrentOf),
    NewCtx4 = case Returning of
                  {returning, {}} -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3, Returning)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {update, Table, AssignmentCommalist, WhereClause, Returning} = PTree) ->
    Rule = update_statement,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts,
        set_state_clause(FunState, assignment_commalist),
        NewCtx1, AssignmentCommalist),
    NewCtx3 = case WhereClause of
                  [] -> NewCtx2;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, WhereClause)
              end,
    NewCtx4 = case Returning of
                  {returning, {}} -> NewCtx3;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx3, Returning)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {user_list = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, user, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user_opts_list
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {user_opts_list = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, user_opt,
        PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {using = Rule, Selection} = PTree)
    when is_list(Selection) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 =
        list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, select_field,
            Selection),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% values_or_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {values_or_query_spec = Rule, {values, InsertAtomCommalist} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS,
        scalar_opt_as_exp, InsertAtomCommalist),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);
fold_i(FType, Fun, LOpts, FunState, Ctx, {values_or_query_spec = Rule, PTree}
) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_def
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx,
    {'create view', Table, Columns, QuerySpec} =
        PTree) ->
    Rule = view_def,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Table) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Table)
              end,
    NewCtx2 = case Columns of
                  [] -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1,
                      {column_commalist, Columns})
              end,
    NewCtx3 = fold_i(FType, Fun, LOpts, FunState, NewCtx2,
        {view_query_spec, QuerySpec}),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% view_query_spec
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx,
    {view_query_spec = Rule, {'as', QuerySpec, WithCheckOption} = PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, QuerySpec),
    NewCtx2 = case WithCheckOption of
                  [] -> NewCtx1;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx1,
                      {keyword, WithCheckOption})
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 'when'
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {'when' = Rule, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS,
                      PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% when_not_found & when_sql_err
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunStateIn, Ctx, {Type = Rule, PTree})
    when Type == when_not_found;Type == when_sql_err ->
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_atom(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, Ctx, {where, {} = _PTree}) ->
    ?FOLD_INIT(_FunState, Ctx, _PTree),
    ?FOLD_RESULT(Ctx);
fold_i(FType, Fun, LOpts, FunState, Ctx, {where, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = where_clause,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(PTree) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts,
                      set_state_clause(FunState, Rule), NewCtxS, PTree)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% where_current_of
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {where_current_of = Rule, Cursor} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = fold_i(FType, Fun, LOpts, FunState, NewCtxS, Cursor),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, Value} = PTree)
    when Op == 'not';Op == '+';Op == '-' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = unary,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case is_binary(Value) of
                  true -> NewCtxS;
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, Value)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% binary
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, L, R} = PTree)
    when is_atom(Op), is_tuple(L), is_tuple(R), Op /= fetch, Op /=
    'connect by' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = binary,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case binary_needs_paren(L, Op) of
                  true ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtxS, {"(", L});
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, L)
              end,
    NewCtx2 = fold_i(FType, Fun, LOpts, FunState, NewCtx1, {binary, Op}),
    NewCtx3 = case binary_needs_paren(R, Op) of
                  true ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtx2, {"(", R});
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtx2, R)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, L, R} = PTree)
    when is_atom(Op), is_binary(L), is_tuple(R), Op /= 'connect by' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = binary,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case binary_needs_paren(R, Op) of
                  true ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtxS, {"(", R});
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, R)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, L, R} = PTree)
    when is_atom(Op), is_tuple(L), is_binary(R), Op /= 'connect by' ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = binary,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtx1 = case binary_needs_paren(L, Op) of
                  true ->
                      fold_i(FType, Fun, LOpts, FunState, NewCtxS, {"(", L});
                  _ -> fold_i(FType, Fun, LOpts, FunState, NewCtxS, L)
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {Op, L, R} = PTree)
    when is_atom(Op), is_binary(L), is_binary(R) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = binary,
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

fold_i(FType, Fun, LOpts, FunState, Ctx, {binary = Rule, PTree})
    when is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS =
        Fun(LOpts, FunState, Ctx, PTree, {Rule, get_start_end(FType, start)}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree,
        {Rule, get_start_end(FType, 'end')}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_FType, _Fun, _LOpts, _FunState, _Ctx, PTree) ->
    ?FOLD_INIT(_FunState, _Ctx, PTree),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] parser subtree not supported"
    ]), PTree}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get maximum depth of a set operation from the parse tree.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_ptree_max_depth_set(PTree :: tuple()) -> integer().
get_ptree_max_depth_set({Op, _L, _R} = PTree)
    when Op == intersect;Op == minus;Op == union;Op == 'union all' ->
    get_ptree_max_depth_set(PTree, 0).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get current statement, clause and rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_stmnt_clause_curr(FunState :: tuple()) -> {atom(), atom(), atom()}.
get_stmnt_clause_curr(FunState) ->
    case length(FunState#fstate.stmnts) of
        0 -> {none, none, none};
        _ -> lists:last(FunState#fstate.stmnts)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get predecessor no. m statement, clause and rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_stmnt_clause_pred(FunState :: tuple(), Pos :: integer()) -> {atom(), atom(), atom()}.
get_stmnt_clause_pred(FunState, Pos) ->
    case length(FunState#fstate.stmnts) of
        L when L < Pos + 1 -> {none, none, none};
        _ -> lists:nth(length(FunState#fstate.stmnts) - Pos,
            FunState#fstate.stmnts)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine if the binary operation needs a set of parentheses.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binary_needs_paren(LOrR, Op) when is_tuple(LOrR), is_atom(Op) ->
    binary_needs_paren(element(1, LOrR), Op);

binary_needs_paren('fun', _) -> false;
binary_needs_paren('or', 'and') -> true;
binary_needs_paren('+', '*') -> true;
binary_needs_paren('+', '/') -> true;
binary_needs_paren('+', 'div') -> true;
binary_needs_paren('-', '*') -> true;
binary_needs_paren('-', '/') -> true;
binary_needs_paren('-', 'div') -> true;
binary_needs_paren('-', '+') -> true;
binary_needs_paren('/', '*') -> true;
binary_needs_paren('div', '*') -> true;
binary_needs_paren(_, '/') -> true;
binary_needs_paren(_, 'div') -> true;
binary_needs_paren(_, '-') -> true;
binary_needs_paren(_, _) -> false.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find the innermost value.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_tuple({_, _, X})
    when is_tuple(X) ->
    decompose_tuple(X);
decompose_tuple({_, _, X})
    when is_binary(X) ->
    binary_to_list(X);
decompose_tuple({_, _, empty}) ->
    [].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get maximum depth of a set operation from the parse tree. {QueryExp, _, '('}.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ptree_max_depth_set({_Op, {select, _}, _} = _PTree, Depth) ->
    Depth + 1;
get_ptree_max_depth_set({_Op, {{select, _}, _, '('}, _} = _PTree, Depth) ->
    Depth + 1;
get_ptree_max_depth_set({_Op, {{OpI, _, _}, _, '('}, _} = _PTree, Depth)
    when OpI == intersect; OpI == minus; OpI == union; OpI == 'union all' ->
    Depth + 1;
get_ptree_max_depth_set({_Op, QuerySpec1, _} = _PTree, Depth) ->
    get_ptree_max_depth_set(QuerySpec1, Depth + 1).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine the final start / end state.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_start_end(FType, StartEnd) ->
    case FType of
        top_down -> StartEnd;
        bottom_up -> case StartEnd of
                         start -> 'end';
                         'end' -> start
                     end
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table with external rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_elem_ext_rule(FType, Fun, LOpts, FunState, Ctx, Rule, Elements) ->
    Length = length(Elements),
    case Length of
        0 -> Ctx;
        1 -> fold_i(FType, Fun, LOpts, FunState, Ctx,
            {Rule, last, lists:last(Elements)});
        _ ->
            list_elem_ext_rule(FType, Fun, LOpts, FunState, Ctx, Rule, Elements,
                Length, Length)
    end.

list_elem_ext_rule(FType, Fun, LOpts, FunState, Ctx, Rule, [Head | Tail], Counter, Length)
    when Counter == Length ->
    NewCtxS = fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, case FType of
                                                                  bottom_up ->
                                                                      last;
                                                                  _ -> other
                                                              end, Head}),
    list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, Rule, Tail,
        Counter - 1, Length);
list_elem_ext_rule(FType, Fun, LOpts, FunState, Ctx, Rule, [Head], 1, _Length) ->
    fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, case FType of
                                                        top_down -> last;
                                                        _ -> other
                                                    end, Head});
list_elem_ext_rule(FType, Fun, LOpts, FunState, Ctx, Rule, [Head | Tail], Counter, Length) ->
    NewCtxS = fold_i(FType, Fun, LOpts, FunState, Ctx, {Rule, other, Head}),
    list_elem_ext_rule(FType, Fun, LOpts, FunState, NewCtxS, Rule, Tail,
        Counter - 1, Length).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table with internal rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_elem_int_rule(FType, Fun, LOpts, FunState, Ctx, Elements) ->
    case length(Elements) of
        0 -> Ctx;
        1 -> [Head] = Elements,
            fold_i(FType, Fun, LOpts, FunState, Ctx, Head);
        _ -> list_elem_int_rule_int(FType, Fun, LOpts, FunState, Ctx, Elements)
    end.

list_elem_int_rule_int(_FType, _Fun, _LOpts, _FunState, Ctx, []) ->
    Ctx;
list_elem_int_rule_int(FType, Fun, LOpts, FunState, Ctx, [Head | Tail]) ->
    NewCtxS = fold_i(FType, Fun, LOpts, FunState, Ctx, Head),
    list_elem_int_rule_int(FType, Fun, LOpts, FunState, NewCtxS, Tail).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table with complete SQL statements (incl. extra).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_elem_sql(FType, Fun, LOpts, FunState, Ctx, Elements) ->
    Length = length(Elements),
    case Length of
        1 -> [{SQL, Extra}] = Elements,
            fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, last, Extra});
        _ -> list_elem_sql(FType, Fun, LOpts, FunState, Ctx, Elements, Length,
            Length)
    end.

list_elem_sql(FType, Fun, LOpts, FunState, Ctx, [{SQL, Extra} | Tail], Counter, Length)
    when Counter == Length ->
    NewCtxS = fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, case FType of
                                                                 bottom_up ->
                                                                     last;
                                                                 _ -> other
                                                             end, Extra}),
    list_elem_sql(FType, Fun, LOpts, FunState, NewCtxS, Tail, Counter - 1,
        Length);
list_elem_sql(FType, Fun, LOpts, FunState, Ctx, [{SQL, Extra}], 1, _Length) ->
    fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, case FType of
                                                       top_down -> last;
                                                       _ -> other
                                                   end, Extra});
list_elem_sql(FType, Fun, LOpts, FunState, Ctx, [{SQL, Extra} | Tail], Counter, Length) ->
    NewCtxS = fold_i(FType, Fun, LOpts, FunState, Ctx, {SQL, other, Extra}),
    list_elem_sql(FType, Fun, LOpts, FunState, NewCtxS, Tail, Counter - 1,
        Length).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the function state for a special clause:
% --------------------------------------------
%     assignment_commalist
%     begin_procedure
%     call_procedure
%     fields
%     from
%     group_by
%     having
%     hierarchical_query
%     order_by_clause
%     query_spec
%     values
%     where_clause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_state_clause(FunState, Clause) ->
    case FunState#fstate.stmnts of
        [] -> FunState#fstate{stmnts = [{none, Clause, none}]};
        _ -> {Stmnt, _, _} = lists:last(FunState#fstate.stmnts),
            FunState#fstate{stmnts = lists:droplast(FunState#fstate.stmnts) ++
            [{Stmnt, Clause, none}]}
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the function state for a special rule:
% ------------------------------------------
%     base_table_element_commalist
%     between_predicate
%     case_when_then
%     function_ref
%     in_predicate
%     like_predicate
%     select_left
%     select_right
%     table_alias
%     table_constraint_def
%     union_left
%     union_right
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_state_rule(FunState, Rule) ->
    case FunState#fstate.stmnts of
        [] -> FunState#fstate{stmnts = [{none, none, Rule}]};
        _ -> {Stmnt, Clause, _} = lists:last(FunState#fstate.stmnts),
            FunState#fstate{stmnts = lists:droplast(FunState#fstate.stmnts) ++
            [{Stmnt, Clause, Rule}]}
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the function state for a new statement:
% -------------------------------------------
%     alter_user_def
%     close_statement
%     commit_statement
%     create_index_def
%     create_role_def
%     create_table_def
%     create_user_def
%     cursor_def
%     delete_statement
%     drop_cluster_def,
%     drop_context_def,
%     drop_database_def,
%     drop_database_link_def,
%     drop_directory_def,
%     drop_function_def,
%     drop_index_def,
%     drop_materialized_view_def,
%     drop_package_def,
%     drop_procedure_def,
%     drop_profile_def,
%     drop_role_def,
%     drop_sequence_def,
%     drop_synonym_def,
%     drop_table_def,
%     drop_tablespace_def,
%     drop_trigger_def,
%     drop_type_def,
%     drop_type_body_def,
%     drop_user_def,
%     drop_view_def,
%     fetch_statement
%     grant_def
%     insert_statement
%     open_statement
%     procedure_call
%     query_exp (intersect, minus, union, union all)
%     query_spec (select)
%     revoke_def
%     rollback_statement
%     schema
%     truncate_cluster
%     truncate_table
%     update_statement
%     view_def
%     when_not_found
%     when_sql_err
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_state_stmnt(FunState, Stmnt) ->
    {StmntCurr, _, _} = get_stmnt_clause_curr(FunState),
    case Stmnt == query_spec andalso StmntCurr == query_exp of
        true ->
            FunState#fstate{stmnts = FunState#fstate.stmnts ++
            [{Stmnt, none, none}]};
        _ -> FunState#fstate{indent_lvl = FunState#fstate.indent_lvl +
            1, stmnts = FunState#fstate.stmnts ++ [{Stmnt, none, none}]}
    end.
