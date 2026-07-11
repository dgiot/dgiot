%%--------------------------------------------------------------------
%% State machine model loader — stores state/transition/action definitions in ETS
%%--------------------------------------------------------------------
-module(dgiot_statem_model).

-export([init/0, load/1, load_all/1, unload/1, get/1, list/0]).

-define(TBL, dgiot_statem_models).

init() ->
    case ets:info(?TBL) of
        undefined -> ets:new(?TBL, [named_table, public, set, {keypos, 1}]);
        _ -> ok
    end.

load(#{<<"modelId">> := Id} = Raw) ->
    Model = parse(Raw),
    ets:insert(?TBL, {Id, Model}),
    {ok, Id}.

load_all(List) ->
    init(),
    [ets:insert(?TBL, {Id, parse(Raw)}) || #{<<"modelId">> := Id} = Raw <- List],
    {ok, length(List)}.

unload(Id) -> ets:delete(?TBL, Id).

get(Id) ->
    case ets:lookup(?TBL, Id) of
        [{Id, M}] -> {ok, M};
        [] -> {error, not_found}
    end.

list() -> ets:tab2list(?TBL).

%%--------------------------------------------------------------------
%% Parse raw JSON/map model into internal format
%%--------------------------------------------------------------------
parse(M) ->
    States0 = maps:get(<<"states">>, M, #{}),
    States1 = parse_states(States0),
    #{
        id       => maps:get(<<"modelId">>, M),
        class    => maps:get(<<"class">>, M, <<"default">>),
        initial  => to_atom(maps:get(<<"initial">>, M, <<"idle">>)),
        states   => States1,
        actions  => maps:get(<<"actions">>, M, #{}),
        version  => maps:get(<<"version">>, M, 1)
    }.

parse_states(States) when is_map(States) ->
    maps:fold(fun(StateName, Def, Acc) ->
        AtomName = to_atom(StateName),
        Events0 = maps:get(<<"on">>, Def, #{}),
        Events1 = maps:fold(fun(K, V, E) -> E#{to_atom(K) => to_atom(V)} end, #{}, Events0),
        Acc#{AtomName => #{
            events  => Events1,
            entry   => to_atom(maps:get(<<"entry">>, Def, undefined)),
            exit    => to_atom(maps:get(<<"exit">>, Def, undefined)),
            timeout => maps:get(<<"timeout">>, Def, undefined)
        }}
    end, #{}, States).

to_atom(undefined) -> undefined;
to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8).
