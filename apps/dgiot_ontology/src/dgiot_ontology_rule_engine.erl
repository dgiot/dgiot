-module(dgiot_ontology_rule_engine).
-export([create/3, fire/2, evaluate/2, list_rules/0, delete/1, init/0]).
-export([property/3, all_of/1, any_of/1]).

init() -> ets:new(dgiot_rules, [named_table, public, bag]), ok.

create(Id, Cond, Act) -> ets:insert(dgiot_rules, {Id, Cond, Act}), {ok, Id}.

fire(Event, _Ctx) ->
    All = ets:tab2list(dgiot_rules),
    Matched = [{Id, Act} || {Id, Cond, Act} <- All, Cond(Event)],
    {ok, [{Id, Act(Event)} || {Id, Act} <- Matched]}.

evaluate(Event, _Ctx) ->
    [Id || {Id, Cond, _} <- ets:tab2list(dgiot_rules), Cond(Event)].

list_rules() -> [{Id, Cond} || {Id, Cond, _} <- ets:tab2list(dgiot_rules)].

delete(Id) ->
    [ets:match_delete(dgiot_rules, {Id, DontCare1, DontCare2}) || DontCare1 <- [], DontCare2 <- []],
    ok.

property(Key, Op, Val) ->
    fun(Event) when is_map(Event) ->
        case maps:find(Key, Event) of {ok, V} -> compare(Op, V, Val); _ -> false end
    end.

all_of(Conds) -> fun(E) -> lists:all(fun(C) -> C(E) end, Conds) end.
any_of(Conds) -> fun(E) -> lists:any(fun(C) -> C(E) end, Conds) end.

compare(less, A, B) -> A < B;
compare(greater, A, B) -> A > B;
compare(less_eq, A, B) -> A =< B;
compare(greater_eq, A, B) -> A >= B;
compare(equal, A, B) -> A == B.
