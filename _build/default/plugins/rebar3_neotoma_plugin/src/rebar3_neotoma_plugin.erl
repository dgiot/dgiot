-module(rebar3_neotoma_plugin).

-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_prv_neotoma_compile:init(State),
    {ok, State2} = rebar3_prv_neotoma_clean:init(State1),
    {ok, State2}.
