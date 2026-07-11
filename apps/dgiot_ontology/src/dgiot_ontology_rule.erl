-module(dgiot_ontology_rule).
-export([compile/1, evaluate/3, match/2]).

compile(Rules) when is_list(Rules) ->
    [compile_rule(R) || R <- Rules].

compile_rule(#{<<"id">> := Id, <<"severity">> := Sev,
               <<"when">> := When, <<"then">> := Then}) ->
    #{
        id       => Id,
        severity => severity(Sev),
        condition => compile_condition(When),
        action   => compile_action(Then)
    }.

compile_condition(#{<<"property">> := Prop, <<"op">> := Op, <<"value">> := Val}) ->
    #{property => Prop, op => op_atom(Op), value => Val}.

compile_action(#{<<"state">> := State, <<"action">> := Action} = Then) ->
    #{state => binary_to_atom(State),
      action => binary_to_atom(Action),
      params => maps:get(<<"params">>, Then, #{})}.

evaluate(Rules, Properties, _Context) ->
    [R || R <- Rules, match(R, Properties)].

match(#{condition := #{property := Prop, op := Op, value := Threshold}}, Properties) ->
    case maps:find(Prop, Properties) of
        {ok, Actual} when is_number(Actual) -> compare(Op, Actual, Threshold);
        _ -> false
    end.

compare(less, A, B) -> A < B;
compare(greater, A, B) -> A > B;
compare(less_eq, A, B) -> A =< B;
compare(greater_eq, A, B) -> A >= B;
compare(equal, A, B) -> A == B;
compare(not_equal, A, B) -> A /= B;
compare(_, _, _) -> false.

op_atom(<<"<">>)  -> less;
op_atom(<<">">>)  -> greater;
op_atom(<<"<=">>) -> less_eq;
op_atom(<<">=">>) -> greater_eq;
op_atom(<<"==">>) -> equal;
op_atom(<<"!=">>) -> not_equal.

severity(<<"L1">>) -> l1;
severity(<<"L2">>) -> l2;
severity(_) -> l3.
