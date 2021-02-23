%% @doc Built in operators.
-module(glc_ops).

-export([
    lt/2, lte/2,
    eq/2, neq/2,
    gt/2, gte/2,
    wc/1,
    nf/1
]).

-export([
    all/1,
    any/1,
    null/1,
    with/2
]).

-export([
    union/1
]).

-type op() ::
    {atom(), '=<', term()} |
    {atom(), '=', term()} |
    {atom(), '!=', term()} |
    {atom(), '>', term()} |
    {atom(), '>=', term()} |
    {atom(), '*'} |
    {atom(), '!'} |
    {any, [op(), ...]} |
    {all, [op(), ...]} |
    {null, true|false}.

-export_type([op/0]).

%% @doc Test that a field value is less than a term.
-spec lt(atom(), term()) -> op().
lt(Key, Term) when is_atom(Key) ->
    {Key, '<', Term};
lt(Key, Term) ->
    erlang:error(badarg, [Key, Term]).


%% @doc Test that a field value is less than or equal to a term.
-spec lte(atom(), term()) -> op().
lte(Key, Term) when is_atom(Key) ->
    {Key, '=<', Term};
lte(Key, Term) ->
    erlang:error(badarg, [Key, Term]).

%% @doc Test that a field value is equal to a term.
-spec eq(atom(), term()) -> op().
eq(Key, Term) when is_atom(Key) ->
    {Key, '=', Term};
eq(Key, Term) ->
    erlang:error(badarg, [Key, Term]).

%% @doc Test that a field value is not equal to a term.
-spec neq(atom(), term()) -> op().
neq(Key, Term) when is_atom(Key) ->
    {Key, '!=', Term};
neq(Key, Term) ->
    erlang:error(badarg, [Key, Term]).


%% @doc Test that a field value is greater than a term.
-spec gt(atom(), term()) -> op().
gt(Key, Term) when is_atom(Key) ->
    {Key, '>', Term};
gt(Key, Term) ->
    erlang:error(badarg, [Key, Term]).

%% @doc Test that a field value is greater than or equal to a term.
-spec gte(atom(), term()) -> op().
gte(Key, Term) when is_atom(Key) ->
    {Key, '>=', Term};
gte(Key, Term) ->
    erlang:error(badarg, [Key, Term]).

%% @doc Test that a field exists.
-spec wc(atom()) -> op().
wc(Key) when is_atom(Key) ->
    {Key, '*'};
wc(Key) ->
    erlang:error(badarg, [Key]).

%% @doc Test that a field is not found.
-spec nf(atom()) -> op().
nf(Key) when is_atom(Key) ->
    {Key, '!'};
nf(Key) ->
    erlang:error(badarg, [Key]).

%% @doc Filter the input using multiple filters.
%%
%% For an input to be considered valid output the all filters specified
%% in the list must hold for the input event. The list is expected to
%% be a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec all([op()]) -> op().
all([_|_]=Conds) ->
    {all, Conds};
all(Other) ->
    erlang:error(badarg, [Other]).

%% @doc Filter the input using one of multiple filters.
%%
%% For an input to be considered valid output on of the filters specified
%% in the list must hold for the input event. The list is expected to be
%% a non-empty list. If the list of filters is an empty list a `badarg'
%% error will be thrown.
-spec any([op()]) -> op().
any([_|_]=Conds) ->
    {any, Conds};
any(Other) ->
    erlang:error(badarg, [Other]).


%% @doc Always return `true' or `false'.
-spec null(boolean()) -> op().
null(Result) when is_boolean(Result) ->
    {null, Result};
null(Result) ->
    erlang:error(badarg, [Result]).

%% @doc Apply a function to each output of a query.
%%
%% Updating the output action of a query finalizes it. Attempting
%% to use a finalized query to construct a new query will result
%% in a `badarg' error.
-spec with(op(), fun((gre:event()) -> term())) -> op().
with(Query, Fun) when is_function(Fun, 1);
                      is_function(Fun, 2) ->
    {with, Query, Fun};
with(Query, Fun) ->
    erlang:error(badarg, [Query, Fun]).

%% @doc Return a union of multiple queries.
%%
%% The union of multiple queries is the equivalent of executing multiple
%% queries separately on the same input event. The advantage is that filter
%% conditions that are common to all or some of the queries only need to
%% be tested once.
%%
%% All queries are expected to be valid and have an output action other
%% than the default which is `output'. If these expectations don't hold
%% a `badarg' error will be thrown.
-spec union([op()]) -> op().
union(Queries) ->
    case [Query || Query <- Queries, glc_lib:onoutput(Query) =:= output] of
        [] -> {union, Queries};
        [_|_] -> erlang:error(badarg, [Queries])
    end.
