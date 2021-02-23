%% @hidden
-module(prometheus_instrumenter).

-export([enabled_instrumenters/0,
         setup/1]).

-export_type([instrumenter/0]).

%%====================================================================
%% Types
%%====================================================================

-type instrumenter() :: atom().

%%====================================================================
%% Callbacks
%%====================================================================

-callback setup_instrumenter() -> ok.

%%====================================================================
%% Public API
%%====================================================================

-spec enabled_instrumenters() -> [instrumenter()].
enabled_instrumenters() ->
  case application:get_env(prometheus, instrumenters) of
    undefined -> all_known_instrumenters();
    {ok, Instrumenters} -> Instrumenters
  end.

-spec setup(Instrumenter) -> Result when
    Instrumenter :: instrumenter(),
    Result :: ok.
setup(Instrumenter) ->
  ok = Instrumenter:setup_instrumenter().

%%====================================================================
%% Private Parts
%%====================================================================

all_known_instrumenters() ->
  prometheus_misc:behaviour_modules(prometheus_instrumenter).
