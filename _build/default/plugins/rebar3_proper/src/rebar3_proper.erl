-module(rebar3_proper).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_proper_prv:init(State),
    {ok, State1}.
