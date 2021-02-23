%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Query processing functions.
-module(glc_lib).

-export([
    reduce/1,
    matches/2,
    onoutput/1,
    onoutput/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-undef(LET).
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-endif.

%% @doc Return a reduced version of a query.
%% 
%% The purpose of this function is to ensure that a query filter
%% is in the simplest possible form. The query filter returned
%% from this function is functionally equivalent to the original.
reduce(Query) ->
    repeat(Query, fun(Q0) ->
        Q1 = repeat(Q0, fun flatten/1),
        Q2 = required(Q1),
        Q3 = repeat(Q2, fun flatten/1),
        Q4 = common(Q3),
        Q5 = repeat(Q4, fun flatten/1),
        Q6 = constants(Q5),
        Q6
    end).


%% @doc Test if an event matches a query.
%% This function is only intended to be used for testing purposes.
matches({any, Conds}, Event) ->
    lists:any(fun(Cond) -> matches(Cond, Event) end, Conds);
matches({all, Conds}, Event) ->
    lists:all(fun(Cond) -> matches(Cond, Event) end, Conds);
matches({null, Const}, _Event) ->
    Const;
matches({Key, '<', Term}, Event) ->
    case gre:find(Key, Event) of
        {true, Term2} -> Term2 < Term;
        false -> false
    end;
matches({Key, '=', Term}, Event) ->
    case gre:find(Key, Event) of
        {true, Term2} -> Term2 =:= Term;
        false -> false
    end;
matches({Key, '!=', Term}, Event) ->
    case gre:find(Key, Event) of
        {true, Term2} -> Term2 =/= Term;
        false -> false
    end;
matches({Key, '>', Term}, Event) ->
    case gre:find(Key, Event) of
        {true, Term2} -> Term2 > Term;
        false -> false
    end;
matches({Key, '*'}, Event) ->
    case gre:find(Key, Event) of
        {true, _} -> true;
        false -> false
    end;
matches({Key, '!'}, Event) ->
    not matches({Key, '*'}, Event).

%% @private Repeatedly apply a function to a query.
%% This is used for query transformation functions that must be applied
%% multiple times to yield the simplest possible version of a query.
repeat(Query, Fun) ->
    case Fun(Query) of
        Query -> Query;
        Query2 -> repeat(Query2, Fun)
    end.


%% @doc Return the output action of a query.
-spec onoutput(glc_ops:op()) -> output | no_return().
onoutput({_, '<', _}) ->
    output;
onoutput({_, '=', _}) ->
    output;
onoutput({_, '>', _}) ->
    output;
onoutput({_, '*'}) ->
    output;
onoutput({_, '!'}) ->
    output;
onoutput(Query) ->
    erlang:error(badarg, [Query]).

%% @doc Modify the output action of a query.
-spec onoutput(Action :: any(), Query :: glc_ops:op()) -> no_return().
onoutput(Action, Query) ->
    erlang:error(badarg, [Action, Query]).


%% @private Flatten a condition tree.
flatten({all, [Cond]}) ->
    Cond;
flatten({any, [Cond]}) ->
    Cond;
flatten({all, Conds}) ->
    flatten_all([flatten(Cond) || Cond <- Conds]);
flatten({any, [_|_]=Conds}) ->
    flatten_any([flatten(Cond) || Cond <- Conds]);
flatten({with, Cond, Action}) ->
    {with, flatten(Cond), Action};
flatten([{with, _Cond, _Action}|_] = I) ->
    [{with, flatten(Cond), Action} || {with, Cond, Action} <- I];
flatten(Other) ->
    valid(Other).


%% @private Flatten and remove duplicate members of an "all" filter.
flatten_all(Conds) ->
    {all, lists:usort(flatten_tag(all, Conds))}.

%% @private Flatten and remove duplicate members of an "any" filter.
flatten_any(Conds) ->
    {any, lists:usort(flatten_tag(any, Conds))}.

%% @private Common function for flattening "all" or "and" filters.
flatten_tag(Tag, [{Tag, Conds}|T]) ->
    Conds ++ flatten_tag(Tag, T);
flatten_tag(Tag, [H|T]) ->
    [H|flatten_tag(Tag, T)];
flatten_tag(_Tag, []) ->
    [].

%% @private Factor out required filters.
%%
%% Identical conditions may occur in all sub-filters of an "any" filter. These
%% filters can be tested once before testing the conditions that are unique to
%% each sub-filter.
%%
%% Assume that the input has been flattened first in order to eliminate all
%% occurances of an "any" filters being "sub-filters" of "any" filters.
required({any, [H|_]=Conds}) ->
    Init = ordsets:from_list(case H of {all, Init2} -> Init2; H -> [H] end),
    case required(Conds, Init) of
        nonefound ->
            Conds2 = [required(Cond) || Cond <- Conds],
            {any, Conds2};
        {found, Req} ->
            Conds2 = [required(deleteall(Cond, Req)) || Cond <- Conds],
            {all, [{all, Req}, {any, Conds2}]}
    end;
required({all, Conds}) ->
    {all, [required(Cond) || Cond <- Conds]};
required(Other) ->
    Other.

required([{all, Conds}|T], Acc) ->
    required(T, ordsets:intersection(ordsets:from_list(Conds), Acc));
required([{any, _}|_]=Cond, Acc) ->
    erlang:error(badarg, [Cond, Acc]);
required([H|T], Acc) ->
    required(T, ordsets:intersection(ordsets:from_list([H]), Acc));
required([], [_|_]=Req) ->
    {found, Req};
required([], []) ->
    nonefound.

%% @private Factor our common filters.
%%
%% Identical conditions may occur in some sub-filters of an "all" filter. These
%% filters can be tested once before testing the conditions that are unique to
%% each sub-filter. This is different from factoring out common sub-filters
%% in an "any" filter where the only those sub-filters that exist in all
%% members.
%%
%% Assume that the input has been flattened first in order to eliminate all
%% occurances of an "any" filters being "sub-filters" of "any" filters.
common({all, Conds}) ->
    case common_(Conds, []) of
        {found, Found} ->
            {all, [Found|[delete(Cond, Found) || Cond <- Conds]]};
        nonefound ->
            {all, [common(Cond) || Cond <- Conds]}
    end;
common({any, Conds}) ->
    {any, [common(Cond) || Cond <- Conds]};
common(Other) ->
    Other.
    

common_([{any, Conds}|T], Seen) ->
    Set = ordsets:from_list(Conds),
    case ordsets:intersection(Set, Seen) of
        [] -> common_(T, ordsets:union(Set, Seen));
        [H|_] -> {found, H}
    end;
common_([H|T], Seen) ->
    case ordsets:is_element(H, Seen) of
        false -> common_(T, ordsets:union(ordsets:from_list([H]), Seen));
        true -> {found, H}
    end;
common_([], _Seen) ->
    nonefound.

%% @private Delete all occurances of constants.
%%
%% An "all" or "any" filter may be reduced to a constant outcome when all
%% sub-filters has been factored out from the filter. In these cases the
%% filter can be removed from the query.
constants(Query) ->
    delete(Query, {null, true}).
    


%% @private Delete all occurances of a filter.
%%
%% Assume that the function is called because a filter is tested
%% by a parent filter. It is therefore safe to replace occurances
%% with a null filter that always returns true.
delete({all, Conds}, Filter) ->
    {all, [delete(Cond, Filter) || Cond <- Conds, Cond =/= Filter]};
delete({any, Conds}, Filter) ->
    {any, [delete(Cond, Filter) || Cond <- Conds, Cond =/= Filter]};
delete(Filter, Filter) ->
    {null, true};
delete(Cond, _Filter) ->
    Cond.

%% @private Delete all occurances of multiple filters.
deleteall(Filter, [H|T]) ->
    deleteall(delete(Filter, H), T);
deleteall(Filter, []) ->
    Filter.



%% @private Test if a term is a valid filter.
-spec is_valid(glc_ops:op()) -> boolean().
is_valid({Field, '<', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '=<', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '=', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '!=', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '>=', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '>', _Term}) when is_atom(Field) ->
    true;
is_valid({Field, '*'}) when is_atom(Field) ->
    true;
is_valid({Field, '!'}) when is_atom(Field) ->
    true;
is_valid({null, true}) ->
    true;
is_valid({null, false}) ->
    true;
is_valid(_Other) ->
    false.

%% @private Assert that a term is a valid filter.
%% If the term is a valid filter. The original term will be returned.
%% If the term is not a valid filter. A `badarg' error is thrown.
valid(Term) ->
    is_valid(Term) orelse erlang:error(badarg, [Term]),
    Term.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

all_one_test() ->
    ?assertEqual(glc:eq(a, 1),
        glc_lib:reduce(glc:all([glc:eq(a, 1)]))
    ).

all_sort_test() ->
    ?assertEqual(glc:all([glc:eq(a, 1), glc:eq(b, 2)]),
        glc_lib:reduce(glc:all([glc:eq(b, 2), glc:eq(a, 1)]))
    ).

any_one_test() ->
    ?assertEqual(glc:eq(a, 1),
        glc_lib:reduce(glc:any([glc:eq(a, 1)]))
    ).

all_two_test() ->
    ?assertEqual(glc_lib:reduce(glc:all([glc:wc(a), glc:nf(b)])),
       glc_lib:reduce(glc:any([
                    glc:all([glc:wc(a)]), 
                    glc:all([glc:wc(a), glc:nf(b)])]))
    ).

any_sort_test() ->
    ?assertEqual(glc:any([glc:eq(a, 1), glc:eq(b, 2)]),
        glc_lib:reduce(glc:any([glc:eq(b, 2), glc:eq(a, 1)]))
    ).

all_nest_test() ->
    ?assertEqual(glc:all([glc:eq(a, 1), glc:eq(b, 2)]),
        glc_lib:reduce(glc:all([glc:eq(a, 1), glc:all([glc:eq(b, 2)])]))
    ),
    ?assertEqual(glc:all([glc:eq(a, 1), glc:eq(b, 2), glc:eq(c, 3)]),
        glc_lib:reduce(glc:all([glc:eq(c, 3),
            glc:all([glc:eq(a, 1),
                glc:all([glc:eq(b, 2)])])]))
    ).

any_nest_test() ->
    ?assertEqual(glc:any([glc:eq(a, 1), glc:eq(b, 2)]),
        glc_lib:reduce(glc:any([glc:eq(a, 1), glc:any([glc:eq(b, 2)])]))
    ),
    ?assertEqual(glc:any([glc:eq(a, 1), glc:eq(b, 2), glc:eq(c, 3)]),
        glc_lib:reduce(glc:any([glc:eq(c, 3),
            glc:any([glc:eq(a, 1),
                glc:any([glc:eq(b, 2)])])]))
    ).

all_equiv_test() ->
    ?assertEqual(glc:eq(a, 1),
        glc_lib:reduce(glc:all([glc:eq(a, 1), glc:eq(a, 1)]))
    ).

any_equiv_test() ->
    ?assertEqual(glc:eq(a, 1),
        glc_lib:reduce(glc:any([glc:eq(a, 1), glc:eq(a, 1)]))
    ).

any_required_test() ->
    ?assertEqual(
        glc:all([
            glc:any([glc:nf(d), glc:eq(b, 2), glc:eq(c, 3)]),
            glc:eq(a, 1)
        ]),
        glc_lib:reduce(
            glc:any([
                glc:all([glc:eq(a, 1), glc:nf(d)]),
                glc:all([glc:eq(a, 1), glc:eq(b, 2)]),
                glc:all([glc:eq(a, 1), glc:eq(c, 3)])]))
    ).
        

all_common_test() ->
    ?assertEqual(
        glc:all([glc:eq(a, 1), glc:eq(b, 2), glc:eq(c, 3)]),
        glc_lib:reduce(
            glc:all([
                glc:any([glc:eq(a, 1), glc:eq(b, 2)]),
                glc:any([glc:eq(a, 1), glc:eq(c, 3)])]))
    ).

delete_from_all_test() ->
    ?assertEqual(
        glc:all([glc:eq(b,2)]),
        deleteall(
            glc:all([glc:eq(a, 1),glc:eq(b,2)]), [glc:eq(a, 1), glc:nf(a)])
    ).

delete_from_any_test() ->
    ?assertEqual(
        glc:any([glc:eq(b,2)]),
        deleteall(
            glc:any([glc:eq(a, 1),glc:eq(b,2)]), [glc:eq(a, 1), glc:wc(a)])
    ).

default_is_output_test_() ->
    [?_assertEqual(output, glc_lib:onoutput(glc:lt(a, 1))),
     ?_assertEqual(output, glc_lib:onoutput(glc:eq(a, 1))),
     ?_assertEqual(output, glc_lib:onoutput(glc:gt(a, 1))),
     ?_assertEqual(output, glc_lib:onoutput(glc:wc(a))),
     ?_assertEqual(output, glc_lib:onoutput(glc:nf(a)))
    ].

-ifdef(PROPER).


prop_reduce_returns() ->
    ?FORALL(Query, glc_ops:op(),
        returns(fun() -> glc_lib:reduce(Query) end)).

reduce_returns_test() ->
    ?assert(proper:quickcheck(prop_reduce_returns())).

prop_matches_returns_boolean() ->
    ?FORALL({Query, Event}, {glc_ops:op(), [{atom(), term()}]},
        is_boolean(glc_lib:matches(Query, gre:make(Event, [list])))).

matches_returns_boolean_test() ->
    ?assert(proper:quickcheck(prop_matches_returns_boolean())).

returns(Fun) ->
    try Fun(),
        true
    catch _:_ ->
        false
    end.

-endif.
-endif.
