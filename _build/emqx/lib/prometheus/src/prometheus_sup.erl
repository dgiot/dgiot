%%%-------------------------------------------------------------------
%% @doc prometheus top level supervisor.
%% @hidden
%%%-------------------------------------------------------------------

-module(prometheus_sup).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-behaviour(supervisor).

-include("prometheus.hrl").

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  create_tables(),
  register_collectors(),
  register_metrics(),
  setup_instrumenters(),
  {ok, {{one_for_one, 5, 1}, [{prometheus_counter,
                               {prometheus_counter, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_counter]},
                              {prometheus_gauge,
                               {prometheus_gauge, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_gauge]},
                              {prometheus_summary,
                               {prometheus_summary, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_summary]},
                              {prometheus_histogram,
                               {prometheus_histogram, start_link, []},
                               permanent,
                               5000,
                               worker,
                               [prometheus_histogram]}]}}.

%%====================================================================
%% Private Parts
%%====================================================================

create_tables() ->
  Tables = [
            {?PROMETHEUS_REGISTRY_TABLE, {bag, read_concurrency}},
            {?PROMETHEUS_COUNTER_TABLE, write_concurrency},
            {?PROMETHEUS_GAUGE_TABLE, write_concurrency},
            {?PROMETHEUS_SUMMARY_TABLE, write_concurrency},
            {?PROMETHEUS_HISTOGRAM_TABLE, write_concurrency}
           ],
  [maybe_create_table(Name, Concurrency) || {Name, Concurrency} <- Tables],
  ok.

register_collectors() ->
  Collectors = prometheus_collector:enabled_collectors(),
  prometheus_registry:register_collectors(Collectors).

register_metrics() ->
  [Metric:declare(Spec, Registry) ||
    {Registry, Metric, Spec} <- default_metrics()].

setup_instrumenters() ->
  [prometheus_instrumenter:setup(Instrumenter) ||
    Instrumenter <- prometheus_instrumenter:enabled_instrumenters()].

default_metrics() ->
  application:get_env(prometheus, default_metrics, []).

maybe_create_table(Name, {Type, Concurrency}) ->
  case ets:info(Name) of
    undefined ->
      ets:new(Name, [Type, named_table, public, {Concurrency, true}]);
    _ ->
      ok
  end;
maybe_create_table(Name, Concurrency) ->
  maybe_create_table(Name, {set, Concurrency}).
