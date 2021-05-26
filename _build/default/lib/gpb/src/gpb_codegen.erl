%%% Copyright (C) 2013  Tomas Abrahamsson
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

%% @doc
%% This module is only used internally within the `gpb'.
%% You do not need to use it to compile a protobuf file. You will
%% use it indirectly though, since the protobuf compiler uses it heavily.
%% Thus, this documentation is mostly for internal use.
%%
%% This module implements a parse transform, to create syntax trees,
%% together with runtime support for subsequent transformation of
%% those syntax trees.
%%
%% Include the file `gpb_codegen.hrl' or specify
%% `-compile({parse_transform,gpb_codegen}).'
%% to activate this parse transform.
%%
%% The syntax tree operations below are provided. An `stree()' is a
%% syntax tree. Use for example `?expr(...)' or `?case_clause(...)' to
%% create syntax trees.
%%
%% <dl>
%%   <dt>`gpb_codegen:mk_fn(FnName, fun(Arg ...) -> Body end) -> stree()'</dt>
%%   <dd>Will be replaced by a parse-tree for a function `FnName',
%%       with `Arg's and `Body' as in the specified fun.
%%       The `FnName' is evaluated at run-time, not at compile-time.
%%   </dd>
%%   <dt>`gpb_codegen:mk_fn(FnName, Fun, RtTransforms) -> stree()'</dt>
%%   <dd><p>Like `gpb_codegen:mk_fn/2', but apply `RtTransforms' at run-time
%%         before returning the syntax tree.</p>
%%       <p>Inside the `Fun', a call to `call_self' is treated specially
%%         as a recursive call back to the function. NB: It is implemented
%%         as a simple term replacement, see below, so any occurrences
%%         of the atom `call_self' will be replaced regardless of whether
%%         it is in a function call or not.</p>
%%       <p>The `RtTransforms' are applied in the order specified.</p>
%%       <p>The following `RtTransforms' are available:</p>
%%       <dl>
%%         <dt>`{replace_term, Marker::atom(), Replacement::term()}'</dt>
%%         <dd>Replace any occurrences of `Marker' with the syntax tree
%%           representing `Replacement', which must be something that could
%%           have occurred as a literal term in some program text,
%%           thus it must not contain any funs, pids, ports, references or such.
%%         </dd>
%%         <dt>`{replace_tree, Marker::atom(), Replacement::stree()}'</dt>
%%         <dd>Replace any occurrences of `Marker' with the syntax tree
%%           `Replacement'.
%%         </dd>
%%         <dt>`{splice_trees, Marker::atom(), Replacements::[stree()]}'</dt>
%%         <dd>For any list that contains `Marker', insert the `Replacements'
%%           syntax trees instead of the `Marker'. Such lists are for example
%%           lists of arguments for a function, lists of elements in a tuple
%%           and lists of expressions in a function body, but not necessarily
%%           elements in literal list term, since these may be represented
%%           as cons elements in the syntax tree.
%%         </dd>
%%         <dt>`{splice_clauses, Marker::atom(), Replacements::[stree()]}'</dt>
%%         <dd>For case clauses (and function clauses), where the pattern is a
%%           single atom, `Marker', insert the case clauses in `Replacements'
%%           instead.
%%           Use the `?case_clause/1' macro to create a syntax tree
%%           for a case clause.
%%         </dd>
%%         <dt>`{repeat_clauses, Marker::atom, Rep::[[transform()]]}'</dt>
%%         <dd><p>Repeat template clauses zero or more times: as many times
%%             as `length(Rep)'. For each repetition, apply the
%%             list of transformations to the clause, be it a function clause,
%%             case clause etc.</p><p>Example: a transformation</p>
%%             <pre>
%%                gpb_codegen:format_fn(
%%                   SomeName,
%%                   fun(s)     -&gt; v;
%%                      (Other) -&gt; erlang:error({not_found, Other})
%%                   end,
%%                   [{repeat_clauses, s,
%%                     [[{replace_term, s, Sym}, {replace_term, v, Value}]
%%                      || {Sym, Value} &lt;- Mapping]}])
%%             </pre>
%%         </dd>
%%       </dl>
%%   </dd>
%%   <dt>`gpb_codegen:format_fn(FnName, Fun [, RtTransforms]) -> iolist()'</dt>
%%   <dd>like `gpb_codegen:mk_fn/2,3', but format the result into
%%       an iolist by calling `erl_prettypr:format'. The resulting
%%       iolist ends with a newline.</dd>
%%   <dt>`?expr(Expr)' or
%%       `gpb_codegen:expr(Expr)'</dt>
%%   <dd>Will be replaced by the syntax tree for a `Expr'.</dd>
%%   <dt>`?expr(Expr, RtTransforms)' or
%%       `gpb_codegen:expr(Expr, RtTransforms)'</dt>
%%   <dd>Like gpb_codegen:expr/1, but apply `RtTransforms' at run-time.</dd>
%%   <dt>`?exprs(Expr, ..., RtTransforms)' or
%%       `gpb_codegen:expr(Expr, ..., RtTransforms)'</dt>
%%   <dd>Like gpb_codegen:expr/1, but create a list of expressions.
%%       The last parameter must always be a list of run-time transforms.
%%       The macro form has support only up to some number of params.</dd>
%%   <dt>`?case_clause(Pattern [when Guard] -> Body)' or
%%       `gpb_codegen:case_clause(CaseExpression)'</dt>
%%   <dd><p>Will be replaced with the syntax tree for the case clause.
%%          Only one case clause, the first, is considered.
%%          When invoked using the `gpb_codegen:case_clause/1' function,
%%          a complete `case Expr of Clause end'  must be provided;
%%          the `Expr' is ignored.</p>
%%       <p>Examples: `?case_clause(1 -> 2)' or `?case_clause(_ -> other)' or
%%          `gpb_codegen:case_clause(case dummy of 1 -> 2 end)'.</p>
%%       <p>In the macro form, some limitations apply:</p>
%%       <ul>
%%          <li>It is only possible to specify one `Guard';
%%              it is _not_ possible to write for example:
%%                `?case_clause(L when is_list(L), length(L) > 2 -> x)'
%%              This is because the preprocessor will interpret
%%              it as two macro arguments, delimited by the comma
%%              in the middle between the two guards.
%%              This limitation does not apply when using the
%%              `gpb_codegen:case_clause/1' approach.</li>
%%          <li>It is only possible to specify one `Body' expression,
%%              because of the same preprocessor intermingling, but it
%%              is possible to work around this using `begin' ... `end'.
%%              This limitation does not apply when using the
%%              `gpb_codegen:case_clause/1' approach.</li>
%%       </ul>
%%   </dd>
%%   <dt>`?case_clause(Pattern [when Guard] -> Body, RtTransforms)' or
%%       `gpb_codegen:case_clause(CaseExpression, RtTransforms)'</dt>
%%   <dd>Like `?case_clause/1' or `gpb_codegen:case_clause/1'
%%       but apply the RtTransforms to the syntax tree.
%%   </dd>
%%   <dt>`?if_clause(Guard -> Body[, RtTransforms])' or
%%       `gpb_codegen:if_clause(IfExpression[, RtTransforms])'</dt>
%%   <dd>Like `?case_clause/1,2' but for if-clauses.</dd>
%%   <dt>`?fn_clause(fun(...) -> ... end, [, RtTransforms])' or
%%       `gpb_codegen:fn_clause(FunExpression[, RtTransforms])'</dt>
%%   <dd>Like `?case_clause/1,2' but for function clauses.</dd>
%%   <dt>`?receive_clause(Pattern -> Body, [, RtTransforms])' or
%%       `gpb_codegen:receive_clause(receive ... -> ... end[, RtTransforms])'
%%   </dt>
%%   <dd>Like `?case_clause/1,2' but for receive clauses.</dd>
%% </dl>
%%
%% Note that there is also a generation-time dependency (ie at
%% run-time for the code-generating code) to this module.
%% The generated code has no dependency to this module, though.
%% @end
%% @private

-module(gpb_codegen).
-export([parse_transform/2]).
-export([runtime_fn_transform/2, runtime_fn_transform/3]).
-export([runtime_expr_transform/1, runtime_expr_transform/2]).
-export([runtime_exprs_transform/2]).
-export([erl_prettypr_format_nl/1]).
-export([with_increased_backtrace_depth/1]).

%% Exported just to be able to give a (more informative) error than undef
-export([mk_fn/2, mk_fn/3, format_fn/2, format_fn/3]).
-export([expr/1, expr/2]).
-export([exprs/2, exprs/3, exprs/4, exprs/5, exprs/6]). %% as many as in .hrl
-export([case_clause/1, case_clause/2]).
-export([fn_clause/1, fn_clause/2]).
-export([if_clause/1, if_clause/2]).
-export([receive_clause/1, receive_clause/2]).

-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

%%@hidden
parse_transform(Forms, Opts) ->
    with_increased_backtrace_depth(
      fun() ->
              transform_forms(Forms, Opts)
      end).

%%@hidden
-spec mk_fn(atom(), fun((...) -> term())) -> no_return().
mk_fn(Name, Fun) -> error_invalid_call(mk_fn, [Name, Fun]).
%%@hidden
-spec mk_fn(atom(), fun((...) -> term()), list()) -> no_return().
mk_fn(Name, Fun, Ts) -> error_invalid_call(mk_fn, [Name, Fun, Ts]).
%%@hidden
-spec format_fn(atom(), fun((...) -> term())) -> no_return().
format_fn(Name, Fun) -> error_invalid_call(format_fn, [Name, Fun]).
%%@hidden
-spec format_fn(atom(), fun((...) -> term()), list()) -> no_return().
format_fn(Name, Fun, Ts) -> error_invalid_call(format_fn, [Name, Fun, Ts]).
%%@hidden
-spec expr(term()) -> no_return().
expr(E) -> error_invalid_call(expr, [E]).
%%@hidden
-spec expr(term(), list()) -> no_return().
expr(E, Ts) -> error_invalid_call(expr, [E, Ts]).
%%@hidden
-spec exprs(term(), list()) -> no_return().
exprs(E1, Ts) -> error_invalid_call(exprs, [E1, Ts]).
%%@hidden
-spec exprs(term(), term(), list()) -> no_return().
exprs(E1, E2, Ts) -> error_invalid_call(exprs, [E1, E2, Ts]).
%%@hidden
-spec exprs(term(), term(), term(), list()) -> no_return().
exprs(E1, E2, E3, Ts) -> error_invalid_call(exprs, [E1, E2, E3, Ts]).
%%@hidden
-spec exprs(term(), term(), term(), term(), list()) -> no_return().
exprs(E1, E2, E3, E4, Ts) -> error_invalid_call(exprs, [E1, E2, E3, E4, Ts]).
%%@hidden
-spec exprs(term(), term(), term(), term(), term(), list()) -> no_return().
exprs(E1, E2, E3, E4, E5, Ts) -> error_invalid_call(exprs,
                                                    [E1, E2, E3, E4, E5, Ts]).
%%@hidden
-spec case_clause(term()) -> no_return().
case_clause(CC) -> error_invalid_call(case_clause, [CC]).
%%@hidden
-spec case_clause(term(), list()) -> no_return().
case_clause(CC, Ts) -> error_invalid_call(case_clause, [CC, Ts]).
%%@hidden
-spec fn_clause(term()) -> no_return().
fn_clause(FC) -> error_invalid_call(fn_clause, [FC]).
%%@hidden
-spec fn_clause(term(), list()) -> no_return().
fn_clause(FC, Ts) -> error_invalid_call(fn_clause, [FC, Ts]).
%%@hidden
-spec if_clause(term()) -> no_return().
if_clause(FC) -> error_invalid_call(if_clause, [FC]).
%%@hidden
-spec if_clause(term(), list()) -> no_return().
if_clause(FC, Ts) -> error_invalid_call(if_clause, [FC, Ts]).
%%@hidden
-spec receive_clause(term()) -> no_return().
receive_clause(FC) -> error_invalid_call(receive_clause, [FC]).
%%@hidden
-spec receive_clause(term(), list()) -> no_return().
receive_clause(FC, Ts) -> error_invalid_call(receive_clause, [FC, Ts]).

error_invalid_call(Fn, Args) ->
    erlang:error({badcall, {{?MODULE, Fn, Args},
                            ["should be transformed with parse transform, "
                             "not called directly"]}}).

transform_forms(Forms, Opts) ->
    Mapper = mk_transform_fn(Forms, Opts),
    [debug_form(erl_syntax:revert(transform_form(Mapper, Form)), Opts)
     || Form <- Forms].

debug_form(NewForm, Opts) ->
    case debug_form_generation_p(Opts) of
        true ->
            try io:format("~s~n", [erl_prettypr:format(NewForm)])
            catch _:_ -> io:format("Non-pretty-printable:~n  ~p", [NewForm])
            end,
            NewForm;
        false ->
            NewForm
    end.

debug_form_generation_p(Opts) ->
    proplists:get_bool(debug_pt, proplists:unfold(Opts)).

mk_transform_fn(Forms, Opts) ->
    TOpts = maybe_opts_for_reversion_of_local_implicit_funs_bug() ++ Opts,
    fun(Node) ->
            Type = erl_syntax:type(Node),
            transform_node(Type, Node, Forms, TOpts)
    end.

transform_form(Mapper, Form) ->
    erl_syntax_lib:map(Mapper, Form).

transform_node(application, Node, AllForms, _Opts) ->
    %% General idea here: transform a "call" to
    %%
    %%    gpb_codegen:mk_fn(Name, Def, RtTransforms)
    %%
    %% into a generation-time call to:
    %%
    %%    ?MODULE:runtime_fn_transform(Name, ParseTreeForDef, RtTransforms)
    %%
    %% The Def can be either of the forms:
    %% - fun(...) -> ... end
    %% - fun somename/X  (for some arity X)
    %% - (but not module:somename/X
    %%    since we need the parse tree for the function somename/X)
    %%
    case erl_syntax_lib:analyze_application(Node) of
        {?MODULE, {mk_fn, 2}} ->
            [FnNameExpr, DefAsFun] = erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            mk_runtime_fn_transform_revert_invoker(FnNameExpr, FnClauses, []);
        {?MODULE, {mk_fn, 3}} ->
            [FnNameExpr, DefAsFun, RtTransforms] =
                erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            mk_runtime_fn_transform_revert_invoker(FnNameExpr, FnClauses,
                                                   [RtTransforms]);
        {?MODULE, {format_fn, 2}} ->
            [FnNameExpr, DefAsFun] = erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            mk_runtime_fn_transform_format_invoker(FnNameExpr, FnClauses, []);
        {?MODULE, {format_fn, 3}} ->
            [FnNameExpr, DefAsFun, RtTransforms] =
                erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            mk_runtime_fn_transform_format_invoker(FnNameExpr, FnClauses,
                                                   [RtTransforms]);
        {?MODULE, {expr, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            erl_syntax:abstract(Expr);
        {?MODULE, {expr, 2}} ->
            [Expr, RtTransforms] = erl_syntax:application_arguments(Node),
            mk_apply(?MODULE, runtime_expr_transform,
                     [erl_syntax:abstract(Expr), RtTransforms]);
        {?MODULE, {exprs, Arity}} when Arity >= 2 ->
            ExprsAndRtTransforms = erl_syntax:application_arguments(Node),
            {Exprs, RtTransforms} = split_out_last(ExprsAndRtTransforms),
            mk_apply(?MODULE, runtime_exprs_transform,
                     [erl_syntax:abstract(Exprs), RtTransforms]);
        {?MODULE, {case_clause, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            case_expr_to_parse_tree_for_clause(Expr, []);
        {?MODULE, {case_clause, 2}} ->
            [Expr, RtTransforms] = erl_syntax:application_arguments(Node),
            case_expr_to_parse_tree_for_clause(Expr, [RtTransforms]);
        {?MODULE, {fn_clause, 1}} ->
            [DefAsFun] = erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            fun_to_parse_tree_for_clause(FnClauses, []);
        {?MODULE, {fn_clause, 2}} ->
            [DefAsFun, RtTransforms] = erl_syntax:application_arguments(Node),
            FnClauses = find_fun_form_clauses(DefAsFun, AllForms),
            fun_to_parse_tree_for_clause(FnClauses, [RtTransforms]);
        {?MODULE, {if_clause, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            if_to_parse_tree_for_clause(Expr, []);
        {?MODULE, {if_clause, 2}} ->
            [Expr, RtTransforms] = erl_syntax:application_arguments(Node),
            if_to_parse_tree_for_clause(Expr, [RtTransforms]);
        {?MODULE, {receive_clause, 1}} ->
            [Expr] = erl_syntax:application_arguments(Node),
            receive_to_parse_tree_for_clause(Expr, []);
        {?MODULE, {receive_clause, 2}} ->
            [Expr, RtTransforms] = erl_syntax:application_arguments(Node),
            receive_to_parse_tree_for_clause(Expr, [RtTransforms]);
        _X ->
            Node
    end;
transform_node(implicit_fun, Node, _Forms, Opts) ->
    %% In R16B03, there's an unfortunate bug in erl_syntax for reverting
    %% exprs on the form "fun some_function/17", aka local implicit funs.
    %% I've found that a work around for the bug is to have something
    %% that's already on erl_parse format, so create local implicit funs
    %% on the erl_parse format.
    case proplists:get_bool(implicit_fun_revert_bug_r16b03, Opts) of
        true ->
            %% Create something that's already in erl_parse format
            case analyze_implicit_fun_name(Node) of
                {FnName, Arity} when is_atom(FnName), is_integer(Arity) ->
                    Pos = erl_syntax:get_pos(Node),
                    {'fun', Pos, {function, FnName, Arity}};
                _ ->
                    %% No bug for other type of implicit funs, e.g. "fun m:f/2"
                    Node
            end;
        false ->
            %% No bug workaround needed
            Node
    end;
transform_node(_Type, Node, _Forms, _Opts) ->
    Node.

split_out_last(List) ->
    [Last | RRest] = lists:reverse(List),
    {lists:reverse(RRest), Last}.

find_fun_form_clauses(DefAsFun, AllForms) ->
    case erl_syntax:type(DefAsFun) of
        fun_expr ->
            erl_syntax:fun_expr_clauses(DefAsFun);
        implicit_fun ->
            case analyze_implicit_fun_name(DefAsFun) of
                {DFnName, Arity} when is_integer(Arity) ->
                    find_function_clauses(AllForms, DFnName, Arity);
                {Module, {FnName, Arity}} ->
                    erlang:error({?MODULE,not_supported,mk_fn,remote_fn,
                                  ?ff("~p:~p/~w", [Module, FnName, Arity])})
            end
    end.

find_function_clauses([Form | Rest], FnName, Arity) ->
    case erl_syntax:type(Form) of
        function ->
            case analyze_function_name(Form) of
                {FnName, Arity} ->
                    erl_syntax:function_clauses(Form);
                _X ->
                    find_function_clauses(Rest, FnName, Arity)
            end;
        _ ->
            find_function_clauses(Rest, FnName, Arity)
    end;
find_function_clauses([], FnName, Arity) ->
    erlang:error({reference_to_undefined_function,FnName,Arity}).

mk_runtime_fn_transform_revert_invoker(FnNameExpr, FnClauses, RtTransforms) ->
    DummyFnName = erl_syntax:atom(fn_name_to_be_replaced_at_runtime),
    mk_apply(erl_syntax, revert,
             [mk_apply(?MODULE, runtime_fn_transform,
                       [FnNameExpr,
                        erl_syntax:abstract(erl_syntax:function(DummyFnName,
                                                                FnClauses))
                        | RtTransforms])]).

mk_runtime_fn_transform_format_invoker(FnNameExpr, FnClauses, RtTransforms) ->
    DummyFnName = erl_syntax:atom(fn_name_to_be_replaced_at_runtime),
    mk_apply(?MODULE, erl_prettypr_format_nl,
             [mk_apply(?MODULE, runtime_fn_transform,
                       [FnNameExpr,
                        erl_parse:abstract(erl_syntax:function(DummyFnName,
                                                               FnClauses))
                        | RtTransforms])]).

mk_apply(M, F, Args) when is_atom(M), is_atom(F) ->
    erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), Args).

case_expr_to_parse_tree_for_clause(Expr, RtTransforms) ->
    case erl_syntax:type(Expr) of
        case_expr ->
            [Clause | _] = erl_syntax:case_expr_clauses(Expr),
            AbsSyntaxTree = erl_parse:abstract(erl_syntax:revert(Clause)),
            mk_apply(?MODULE, runtime_expr_transform,
                     [AbsSyntaxTree | RtTransforms]);
        _OtherType ->
            Expr
    end.

fun_to_parse_tree_for_clause([FnClause | _], RtTransforms) ->
    AbsSyntaxTree = erl_parse:abstract(erl_syntax:revert(FnClause)),
    mk_apply(?MODULE, runtime_expr_transform, [AbsSyntaxTree | RtTransforms]).

if_to_parse_tree_for_clause(Expr, RtTransforms) ->
    case erl_syntax:type(Expr) of
        if_expr ->
            [Clause | _] = erl_syntax:if_expr_clauses(Expr),
            AbsSyntaxTree = erl_parse:abstract(erl_syntax:revert(Clause)),
            mk_apply(?MODULE, runtime_expr_transform,
                     [AbsSyntaxTree | RtTransforms]);
        _OtherType ->
            Expr
    end.

receive_to_parse_tree_for_clause(Expr, RtTransforms) ->
    case erl_syntax:type(Expr) of
        receive_expr ->
            [Clause | _] = erl_syntax:receive_expr_clauses(Expr),
            AbsSyntaxTree = erl_parse:abstract(erl_syntax:revert(Clause)),
            mk_apply(?MODULE, runtime_expr_transform,
                     [AbsSyntaxTree | RtTransforms]);
        _OtherType ->
            Expr
    end.

%% Main entry points at runtime.

%%@hidden
erl_prettypr_format_nl(Form) ->
    %% Allow very wide output. erl_prettypr tends
    %% to otherwise format the generated code very widely,
    %%           breaking lines at the end of the lines,
    %%                   giving a kind of boomerang-y shape,
    %%                                           somewhat
    %%                                             like
    %%                                               this text
    Opts = [{paper, 300}, {ribbon, 250}],
    with_increased_backtrace_depth(
      fun() -> [erl_prettypr:format(Form, Opts), "\n\n"] end).

%%@hidden
runtime_fn_transform(FnName, FnParseTree) ->
    runtime_fn_transform(FnName, FnParseTree, []).

%%@hidden
runtime_fn_transform(FnName, FnParseTree, Transforms) ->
    with_increased_backtrace_depth(
      fun() ->
              Clauses = erl_syntax:function_clauses(FnParseTree),
              apply_transforms(
                erl_syntax:function(erl_syntax:atom(FnName), Clauses),
                Transforms ++ [{replace_term, call_self, FnName}])
      end).

%%@hidden
runtime_expr_transform(ExprParseTree) ->
    runtime_expr_transform(ExprParseTree, []).

%%@hidden
runtime_expr_transform(ExprParseTree, Transforms) ->
    with_increased_backtrace_depth(
      fun() ->
              erl_syntax:copy_pos(
                ExprParseTree,
                apply_transforms(ExprParseTree, Transforms))
      end).

%%@hidden
runtime_exprs_transform(ExprParseTrees, Transforms) ->
    with_increased_backtrace_depth(
      fun() ->
              %% To be able to apply the splice_trees also on the
              %% top-level, transform this into a single expr:
              %% begin ... end. Such an expr is called a block.
              Block1 = erl_syntax:block_expr(ExprParseTrees),
              Block2 = apply_transforms(Block1, Transforms),
              erl_syntax:block_expr_body(Block2)
      end).

apply_transforms(ParseTree, Transforms) ->
    lists:foldl(fun apply_transform/2, ParseTree, Transforms).

apply_transform({replace_term, Marker, Replacement}, ParseTree) ->
    erl_syntax_lib:map(term_replacing_mapper(Marker, Replacement),
                       ParseTree);
apply_transform({replace_tree, Marker, Replacement}, ParseTree) ->
    erl_syntax_lib:map(tree_replacing_mapper(Marker, Replacement),
                       ParseTree);
apply_transform({splice_trees, Marker, Replacements}, ParseTree) ->
    splice_trees(Marker, Replacements, ParseTree);
apply_transform({splice_clauses, Marker, Replacements}, ParseTree) ->
    splice_clauses(Marker, Replacements, ParseTree);
apply_transform({repeat_clauses, Marker, Repetitions}, ParseTree) ->
    repeat_clauses(Marker, Repetitions, ParseTree).


term_replacing_mapper(Marker, Replacement) ->
    ReplacementTree = erl_parse:abstract(Replacement),
    tree_replacing_mapper(Marker, ReplacementTree).

tree_replacing_mapper(Marker, Replacement) ->
    fun(Node) ->
            case analyze_atom_as_value(Node) of
                {atom, Marker} -> Replacement;
                {atom, _Other} -> Node;
                non_atom       -> Node
            end
    end.

splice_trees(Marker, Replacements, Tree)   ->
    case erl_syntax:subtrees(Tree) of
        [] ->
            Tree;
        Gs ->
            F = fun(SubTree) -> splice_trees(Marker, Replacements, SubTree) end,
            Gs1 = [case split_list_on_marker(G, Marker) of
                       marker_not_found ->
                           [F(T) || T <- G];
                       {BeforeMarker, _MarkerTree, AfterMarker} ->
                           Before = [F(T) || T <- BeforeMarker],
                           After = [F(T) || T <- AfterMarker],
                           Before ++ Replacements ++ After
                   end
                   || G <- Gs],
            Tree1 = erl_syntax:make_tree(erl_syntax:type(Tree), Gs1),
            erl_syntax:copy_attrs(Tree, Tree1)
    end.

split_list_on_marker(Elems, Marker) -> split_aux(Elems, Marker, []).

split_aux([X | Rest], Marker, Acc) ->
    case erl_syntax:type(X) of
        binary_field ->
            %% The marker (an atom) as a binary_field, will show up
            %% as a subtree of the subtree of the binary field, but
            %% must be replaced one level above that, so catch it here.
            case analyze_binary_field_body_as_atom_as_value(X) of
                {atom, Marker} -> {lists:reverse(Acc), X, Rest};
                {atom, _Other} -> split_aux(Rest, Marker, [X | Acc]);
                non_atom       -> split_aux(Rest, Marker, [X | Acc])
            end;
        _OtherType ->
            case analyze_atom_as_value(X) of
                {atom, Marker} -> {lists:reverse(Acc), X, Rest};
                {atom, _Other} -> split_aux(Rest, Marker, [X | Acc]);
                non_atom       -> split_aux(Rest, Marker, [X | Acc])
            end
    end;
split_aux([], _Marker, _Acc) ->
    marker_not_found.

splice_clauses(CMarker, Replacements, Tree) ->
    transform_clauses(
      CMarker,
      fun(_MarkerClause, _ClauseType) -> Replacements end,
      Tree).

repeat_clauses(CMarker, Repetitions, Tree) ->
    transform_clauses(
      CMarker,
      fun(TemplateClause, _Type) ->
              [apply_transforms(TemplateClause, Transforms)
               || Transforms <- Repetitions]
      end,
      Tree).

transform_clauses(CMarker, CTransformer, Tree) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  case_expr ->
                      Arg = erl_syntax:case_expr_argument(Node),
                      Cs  = erl_syntax:case_expr_clauses(Node),
                      case split_clauses_on_marker(Cs, CMarker, 'case') of
                          marker_not_found ->
                              Node;
                          {Before, MarkerClause, After} ->
                              New = CTransformer(MarkerClause, 'case'),
                              Cs1 = Before ++ New ++ After,
                              erl_syntax:case_expr(Arg, Cs1)
                      end;
                  fun_expr ->
                      Cs = erl_syntax:fun_expr_clauses(Node),
                      case split_clauses_on_marker(Cs, CMarker, 'fun') of
                          marker_not_found ->
                              Node;
                          {Before, MarkerClause, After} ->
                              New = CTransformer(MarkerClause, 'fun'),
                              Cs1 = Before ++ New ++ After,
                              erl_syntax:fun_expr(Cs1)
                      end;
                  function ->
                      FnName = erl_syntax:function_name(Node),
                      Cs = erl_syntax:function_clauses(Node),
                      case split_clauses_on_marker(Cs, CMarker, function) of
                          marker_not_found ->
                              Node;
                          {Before, MarkerClause, After} ->
                              New = CTransformer(MarkerClause, function),
                              Cs1 = Before ++ New ++ After,
                              erl_syntax:function(FnName, Cs1)
                      end;
                  if_expr ->
                      Cs = erl_syntax:if_expr_clauses(Node),
                      case split_clauses_on_marker(Cs, CMarker, 'if') of
                          marker_not_found ->
                              Node;
                          {Before, MarkerClause, After} ->
                              New = CTransformer(MarkerClause, 'if'),
                              Cs1 = Before ++ New ++ After,
                              erl_syntax:if_expr(Cs1)
                      end;
                  receive_expr ->
                      Cs = erl_syntax:receive_expr_clauses(Node),
                      Tmo = erl_syntax:receive_expr_timeout(Node),
                      Action = erl_syntax:receive_expr_action(Node), %% after
                      case split_clauses_on_marker(Cs, CMarker, 'if') of
                          marker_not_found ->
                              Node;
                          {Before, MarkerClause, After} ->
                              New = CTransformer(MarkerClause, 'receive'),
                              Cs1 = Before ++ New ++ After,
                              erl_syntax:receive_expr(Cs1, Tmo, Action)
                      end;
                  _Other ->
                      Node
              end
      end,
      Tree).

split_clauses_on_marker(Clauses, CMarker, Type) ->
    csplit_aux(Clauses, CMarker, Type, []).

csplit_aux([C | Rest], CMarker, Type, Acc) ->
    case erl_syntax:clause_patterns(C) of
        [CPattern | _] -> %% case clause or function clause
            case analyze_atom_as_value(CPattern) of
                {atom, CMarker} -> {lists:reverse(Acc), C, Rest};
                {atom, _Other}  -> csplit_aux(Rest, CMarker, Type, [C | Acc]);
                non_atom        -> csplit_aux(Rest, CMarker, Type, [C | Acc])
            end;
        [] when Type == 'if' -> %% an if-clause
            G = erl_syntax:clause_guard(C),
            case analyze_guard_as_atom_as_value(G) of
                {atom, CMarker} -> {lists:reverse(Acc), C, Rest};
                {atom, _Other}  -> csplit_aux(Rest, CMarker, Type, [C | Acc]);
                non_atom_guard  -> csplit_aux(Rest, CMarker, Type, [C | Acc])
            end;
        _CPatterns ->
            csplit_aux(Rest, CMarker, Type, [C | Acc])
    end;
csplit_aux([], _CMarker, _Type, _Acc) ->
    marker_not_found.

analyze_guard_as_atom_as_value(G) ->
    %% The guard 'x' (the single atom x) is a disjunction of conjunctions:
    case erl_syntax:type(G) of
        disjunction ->
            [D1 | _] = erl_syntax:disjunction_body(G),
            case erl_syntax:type(D1) of
                conjunction ->
                    [C1 | _] = erl_syntax:conjunction_body(D1),
                    case analyze_atom_as_value(C1) of
                        {atom, V} -> {atom, V};
                        non_atom  -> non_atom_guard
                    end;
                _ ->
                    non_atom_guard
            end;
        _ ->
            non_atom_guard
    end.

analyze_binary_field_body_as_atom_as_value(BinField) ->
    analyze_atom_as_value(erl_syntax:binary_field_body(BinField)).

%% -> {Name,Arity} | {Module,{Name,Arity}}
analyze_implicit_fun_name(Tree) ->
    erl_syntax_lib:analyze_function_name(erl_syntax:implicit_fun_name(Tree)).

analyze_function_name(Tree) ->
    Name = erl_syntax_lib:analyze_function_name(erl_syntax:function_name(Tree)),
    Arity = erl_syntax:function_arity(Tree),
    %% Return a format like that of analyze_implicit_fun_name (no module)
    {Name, Arity}.

%% -> {atom, atom()} | non_atom
analyze_atom_as_value(Node) ->
    case erl_syntax:type(Node) of
        atom -> {atom, erl_syntax:atom_value(Node)};
        _    -> non_atom
    end.

with_increased_backtrace_depth(Fun) ->
    %% The backtrace_depth is quite often too short,
    %% when things go wrong inside the parse transform,
    %% or during the runtime application of additional transforms.
    %%
    %% The backtrace_depth controls how many levels of stack
    %% to include in the crash, too few levels means we only
    %% see the innermost function calls, not the originating
    %% top-level calls, making it difficult to debug errors.
    %%
    %% So: up it (temporarily).
    %%
    %% It is 8 in current Erlang/OTPs, but take some precautions
    %% in case it gets increased or changed in future versions.
    New = 32,
    try erlang:system_flag(backtrace_depth, New) of
        Old when Old < New ->
            try Fun()
            after erlang:system_flag(backtrace_depth, Old)
            end;
        Old when Old == New ->
            Fun();
        Old when Old > New ->
            %% Don't decrease it!
            erlang:system_flag(backtrace_depth, Old),
            Fun()
    catch error:badarg -> %% Not available
            Fun()
    end.

maybe_opts_for_reversion_of_local_implicit_funs_bug() ->
    {ok, Tokens, _End} = erl_scan:string("fun x/17."),
    {ok, [ImplicitFunExpr1]} = erl_parse:parse_exprs(Tokens),
    ImplicitFunExpr2 =
        erl_syntax:revert(
          erl_syntax:copy_pos(
            ImplicitFunExpr1,
            erl_syntax:implicit_fun(
              erl_syntax:copy_pos(ImplicitFunExpr1, erl_syntax:atom(x)),
              erl_syntax:copy_pos(ImplicitFunExpr1, erl_syntax:integer(17))))),
    case {ImplicitFunExpr1, ImplicitFunExpr2} of
        {Same, Same} ->
            %% No bug if the erl_parse format is the same
            %% as that from erl_syntax:revert, no bug-workaround options needed
            [];
        {{'fun', _, {function, x, 17}},
         {'fun', _, {function, {atom, _, x}, {integer, _, 17}}}} ->
            %% The erl_parse format and the erl_syntax format are not the same!
            %% Found the bug when reverting local implicit funs in r16b03
            [implicit_fun_revert_bug_r16b03]
    end.

