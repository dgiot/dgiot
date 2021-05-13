%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_rule_sqlparser).

-include("rule_engine.hrl").
-include("rule_events.hrl").

-export([parse_select/1]).

-export([ select_fields/1
        , select_is_foreach/1
        , select_doeach/1
        , select_incase/1
        , select_from/1
        , select_where/1
        ]).

-export([ unquote/1
        ]).

-import(proplists, [ get_value/2
                   , get_value/3
                   ]).

-record(select, {fields, from, where, is_foreach, doeach, incase}).

-opaque(select() :: #select{}).

-type(const() :: {const, number()|binary()}).

-type(variable() :: binary() | list(binary())).

-type(alias() :: binary() | list(binary())).

-type(field() :: const() | variable()
               | {as, field(), alias()}
               | {'fun', atom(), list(field())}).

-export_type([select/0]).

%% Parse one select statement.
-spec(parse_select(string() | binary())
      -> {ok, select()} | {parse_error, term()} | {lex_error, term()}).
parse_select(Sql) ->
    try case sqlparse:parsetree(Sql) of
            {ok, [{{select, Clauses}, _Extra}]} ->
                {ok, preprocess(#select{is_foreach = false,
                                        fields = get_value(fields, Clauses),
                                        doeach = [],
                                        incase = {},
                                        from = get_value(from, Clauses),
                                        where = get_value(where, Clauses)})};
            {ok, [{{foreach, Clauses}, _Extra}]} ->
                {ok, preprocess(#select{is_foreach = true,
                                        fields = get_value(fields, Clauses),
                                        doeach = get_value(do, Clauses, []),
                                        incase = get_value(incase, Clauses, {}),
                                        from = get_value(from, Clauses),
                                        where = get_value(where, Clauses)})};
            Error -> Error
        end
    catch
        _Error:Reason:StackTrace ->
            {parse_error, Reason, StackTrace}
    end.

-spec(select_fields(select()) -> list(field())).
select_fields(#select{fields = Fields}) ->
    Fields.

-spec(select_is_foreach(select()) -> boolean()).
select_is_foreach(#select{is_foreach = IsForeach}) ->
    IsForeach.

-spec(select_doeach(select()) -> list(field())).
select_doeach(#select{doeach = DoEach}) ->
    DoEach.

-spec(select_incase(select()) -> list(field())).
select_incase(#select{incase = InCase}) ->
    InCase.

-spec(select_from(select()) -> list(binary())).
select_from(#select{from = From}) ->
    From.

-spec(select_where(select()) -> tuple()).
select_where(#select{where = Where}) ->
    Where.

preprocess(#select{fields = Fields, is_foreach = IsForeach, doeach = DoEach, incase = InCase, from = Hooks, where = Conditions}) ->
    {SelectedFileds, KnownColmuns1} = preprocess_columns(Fields, fixed_columns()),
    {SelectedEach, KnownColmuns2} = preprocess_columns(DoEach, KnownColmuns1),
    #select{is_foreach = IsForeach,
            fields = SelectedFileds,
            doeach = SelectedEach,
            incase = preprocess_condition(InCase, KnownColmuns2),
            from   = [unquote(H) || H <- Hooks],
            where  = preprocess_condition(Conditions, KnownColmuns2)}.

preprocess_columns(Fields, KnownColumns) ->
    lists:foldl(
        fun(Field, {Slct0, KnwnClmn0}) ->
            case preprocess_field(Field, KnwnClmn0) of
                {Slct1, no_as_column} ->
                    {Slct0 ++ [Slct1], KnwnClmn0};
                {Slct1, AsColumn} ->
                    {Slct0 ++ [Slct1], KnwnClmn0 ++ [AsColumn]}
            end
        end, {[], KnownColumns}, Fields).

preprocess_field(<<"*">>, _Columns) ->
    {'*', no_as_column};
preprocess_field({'as', Field, Alias}, Columns) when is_binary(Alias) ->
    {{'as', transform_select_field(Field, Columns), transform_alias(Alias)}, head(transform_alias(Alias))};
preprocess_field(Field, Columns) ->
    {transform_select_field(Field, Columns), no_as_column}.

preprocess_condition({Op, L, R}, Columns) when ?is_logical(Op) orelse ?is_comp(Op) ->
    {Op, preprocess_condition(L, Columns), preprocess_condition(R, Columns)};
preprocess_condition({in, Field, {list, Vals}}, Columns) ->
    {in, transform_field(Field, Columns), {list, [transform_field(Val, Columns) || Val <- Vals]}};
preprocess_condition({'not', X}, Columns) ->
    {'not', preprocess_condition(X, Columns)};
preprocess_condition({}, _Columns) ->
    {};
preprocess_condition(Field, Columns) ->
    transform_field(Field, Columns).

transform_field({const, Val}, _Columns) ->
    {const, Val};
transform_field(<<Q, Val/binary>>, _Columns) when Q =:= $'; Q =:= $" ->
    {const, binary:part(Val, {0, byte_size(Val)-1})};
transform_field(Val, Columns) ->
    case is_number_str(Val) of
        true -> {const, Val};
        false ->
            do_transform_field(Val, Columns)
    end.

do_transform_field({Op, Arg1, Arg2}, Columns) when ?is_arith(Op) ->
    {Op, transform_field(Arg1, Columns), transform_field(Arg2, Columns)};
do_transform_field(Var, Columns) when is_binary(Var) ->
    {var, validate_var(escape(parse_nested(Var)), Columns)};
do_transform_field({'fun', Name, Args}, Columns) when is_binary(Name) ->
    Fun = list_to_existing_atom(binary_to_list(Name)),
    {'fun', Fun, [transform_field(Arg, Columns) || Arg <- Args]}.

transform_select_field({const, Val}, _Columns) ->
    {const, Val};
transform_select_field({Op, Arg1, Arg2}, Columns) when ?is_arith(Op) ->
    {Op, transform_select_field(Arg1, Columns), transform_select_field(Arg2, Columns)};
transform_select_field(Var, Columns) when is_binary(Var) ->
    {var, validate_var(escape(parse_nested(Var)), Columns)};
transform_select_field({'case', CaseOn, CaseClauses, ElseClause}, Columns) ->
    {'case', transform_caseon(CaseOn, Columns),
             transform_case_clause(CaseClauses, Columns),
             transform_caseelse(ElseClause, Columns)};
transform_select_field({'fun', Name, Args}, Columns) when is_binary(Name) ->
    Fun = list_to_existing_atom(binary_to_list(Name)),
    {'fun', Fun, [transform_select_field(Arg, Columns) || Arg <- Args]}.

transform_caseon(<<>>, _Columns) -> undefined;
transform_caseon(CaseOn, Columns) ->
    transform_select_field(CaseOn, Columns).

transform_case_clause(CaseClauses, Columns) ->
    [{preprocess_condition(Cond, Columns), transform_select_field(Return, Columns)}
     || {Cond, Return} <- CaseClauses].

transform_caseelse({}, _Columns) -> undefined;
transform_caseelse(ElseClause, Columns) ->
    transform_select_field(ElseClause, Columns).

validate_var(Var, SupportedColumns) ->
    case {Var, lists:member(Var, SupportedColumns)} of
        {_, true} -> escape(Var);
        {[TopVar | _], false} ->
            lists:member(TopVar, SupportedColumns) orelse error({unknown_column, Var}),
            escape(Var);
        {_, false} ->
            error({unknown_column, escape(Var)})
    end.

is_number_str(Str) when is_binary(Str) ->
    try _ = binary_to_integer(Str), true
    catch _:_ ->
        try _ = binary_to_float(Str), true
        catch _:_ -> false
        end
    end;
is_number_str(_NonStr) ->
    false.

fixed_columns() ->
    ?COLUMNS('message.publish') ++
    ?COLUMNS('message.acked') ++
    ?COLUMNS('message.dropped') ++
    ?COLUMNS('client.connected') ++
    ?COLUMNS('client.disconnected') ++
    ?COLUMNS('session.subscribed') ++
    ?COLUMNS('session.unsubscribed') ++
    [<<"item">>].

transform_alias(Alias) ->
    parse_nested(unquote(Alias)).

parse_nested(Attr) ->
    case string:split(Attr, <<".">>, all) of
        [Attr] -> Attr;
        Nested -> Nested
    end.

%% escape the SQL reserved key words
escape(<<"TIMESTAMP">>) -> <<"timestamp">>;
escape(Var) -> Var.

unquote(Topic) ->
    string:trim(Topic, both, "\"'").

head([H | _]) -> H;
head(Var) -> Var.
