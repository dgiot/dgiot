-module(rebar3_prv_neotoma_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name,       ?PROVIDER},
                                {module,     ?MODULE},
                                {namespace,  neotoma},
                                {bare,       false},
                                {deps,       ?DEPS},
                                {example,    "rebar3 neotoma clean"},
                                {short_desc, "Clean source generated from peg files."},
                                {desc,       ""},
                                {opts,       []}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Cleaning neotoma generated files not yet implemented", []),
    {ok, State}.

format_error(Error) ->
    io_lib:format("~p", [Error]).
