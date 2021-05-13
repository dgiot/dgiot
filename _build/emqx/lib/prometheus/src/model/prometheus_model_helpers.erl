%% @doc
%% Helpers for working with Prometheus data model. For advanced users.
%% Probably will be used with {@link prometheus_collector}.
%% @end

-module(prometheus_model_helpers).

-export([create_mf/5,
         gauge_metrics/1,
         gauge_metric/1,
         gauge_metric/2,
         untyped_metrics/1,
         untyped_metric/1,
         untyped_metric/2,
         counter_metrics/1,
         counter_metric/1,
         counter_metric/2,
         summary_metrics/1,
         summary_metric/1,
         summary_metric/2,
         summary_metric/3,
         histogram_metrics/1,
         histogram_metric/1,
         histogram_metric/3,
         histogram_metric/4,
         label_pairs/1,
         label_pair/1]).

-ifdef(TEST).
-export([filter_undefined_metrics/1,
         ensure_mf_type/1,
         ensure_binary_or_string/1]).
-endif.

-include("prometheus_model.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type label_name() :: term().
-type label_value() :: term().
-type label() :: {label_name(), label_value()}.
-type value() :: float() | integer() | undefined | infinity.

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc
%%  Create Metric Family of `Type', `Name' and `Help'.
%%  `Collector:collect_metrics/2' callback will be called and expected to
%%  return individual metrics list.
%% @end
-spec create_mf(Name, Help, Type, Collector, CollectorData) -> MetricFamily when
    Name          :: prometheus_metric:name(),
    Help          :: prometheus_metric:help(),
    Type          :: atom(),
    Collector     :: prometheus_collector:collector(),
    CollectorData :: prometheus_collector:data(),
    MetricFamily  :: prometheus_model:'MetricFamily'().
create_mf(Name, Help, Type, Collector, CollectorData) ->
  Metrics = ensure_list(Collector:collect_metrics(Name, CollectorData)),
  #'MetricFamily'{name   = ensure_binary_or_string(Name),
                  help   = ensure_binary_or_string(Help),
                  type   = ensure_mf_type(Type),
                  metric = filter_undefined_metrics(Metrics)}.

%% @doc Equivalent to
%% {@link gauge_metric/1. `lists:map(fun gauge_metric/1, Values)'}.
%% @end
gauge_metrics(Values) -> lists:map(fun gauge_metric/1, Values).

%% @doc
%% Equivalent to
%% <a href="#gauge_metric-2"><tt>gauge_metric(Labels, Value)</tt></a>.
%% @end
-spec gauge_metric(Spec) -> prometheus_model:'Metric'() when
    Spec :: value()
          | {value()}
          | {[label()], value()}.
gauge_metric({Labels, Value}) -> gauge_metric(Labels, Value);
gauge_metric({Value})         -> gauge_metric([], Value);
gauge_metric(Value)           -> gauge_metric([], Value).

%% @doc
%% Creates gauge metric with `Labels' and `Value'.
%% @end
-spec gauge_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: [label()],
    Value  :: value().
gauge_metric(Labels, Value) ->
  #'Metric'{label = label_pairs(Labels),
            gauge = #'Gauge'{value = Value}}.

%% @doc Equivalent to
%% {@link untyped_metric/1. `lists:map(fun untyped_metric/1, Values)'}.
%% @end
untyped_metrics(Values) -> lists:map(fun untyped_metric/1, Values).

%% @doc
%% Equivalent to
%% <a href="#untyped_metric-2"><tt>untyped_metric(Labels, Value)</tt></a>.
%% @end
-spec untyped_metric(Spec) -> prometheus_model:'Metric'() when
    Spec :: value()
          | {value()}
          | {[label()], value()}.
untyped_metric({Labels, Value}) -> untyped_metric(Labels, Value);
untyped_metric({Value})         -> untyped_metric([], Value);
untyped_metric(Value)           -> untyped_metric([], Value).

%% @doc
%% Creates untyped metric with `Labels' and `Value'.
%% @end
-spec untyped_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: [label()],
    Value  :: value().
untyped_metric(Labels, Value) ->
  #'Metric'{label = label_pairs(Labels),
            untyped = #'Untyped'{value = Value}}.

%% @doc Equivalent to
%% {@link counter_metric/1. `lists:map(fun counter_metric/1, Specs)'}.
counter_metrics(Specs) -> lists:map(fun counter_metric/1, Specs).

%% @doc
%% Equivalent to
%% <a href="#counter_metric-2"><tt>counter_metric(Labels, Value)</tt></a>.
%% @end
-spec counter_metric(Spec) -> prometheus_model:'Metric'() when
    Spec :: value()
          | {value()}
          | {[label()], value()}.
counter_metric({Labels, Value}) -> counter_metric(Labels, Value);
counter_metric({Value})         -> counter_metric([], Value);
counter_metric(Value)           -> counter_metric([], Value).

%% @doc
%% Creates counter metric with `Labels' and `Value'.
%% @end
-spec counter_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: [label()],
    Value  :: value().
counter_metric(Labels, Value) ->
  #'Metric'{label   = label_pairs(Labels),
            counter = #'Counter'{value = Value}}.

%% @doc Equivalent to
%% {@link summary_metric/1. `lists:map(fun summary_metric/1, Specs)'}.
summary_metrics(Specs) -> lists:map(fun summary_metric/1, Specs).

%% @doc
%% Equivalent to
%% <a href="#summary_metric-3"><tt>summary_metric(Labels, Count, Sum)</tt></a>.
%% @end
-spec summary_metric(Spec) -> prometheus_model:'Metric'() when
    Spec   :: {Labels, Count, Sum} | {Count, Sum},
    Labels :: [label()],
    Count  :: non_neg_integer(),
    Sum    :: value().
summary_metric({Labels, Count, Sum}) -> summary_metric(Labels, Count, Sum);
summary_metric({Count, Sum})         -> summary_metric([], Count, Sum).

%% @equiv summary_metric([], Count, Sum)
summary_metric(Count, Sum) -> summary_metric([], Count, Sum).

%% @doc
%% Creates summary metric with `Labels', `Count' and `Sum'.
%% @end
-spec summary_metric(Labels, Count, Sum) -> prometheus_model:'Metric'() when
    Labels :: [label()],
    Count  :: non_neg_integer(),
    Sum    :: value().
summary_metric(Labels, Count, Sum) ->
  #'Metric'{label   = label_pairs(Labels),
            summary = #'Summary'{sample_count = Count,
                                 sample_sum   = Sum}}.

%% @doc Equivalent to
%% {@link histogram_metric/1. `lists:map(fun histogram_metric/1, Specs)'}.
%% @end
histogram_metrics(Specs) -> lists:map(fun histogram_metric/1, Specs).

%% @doc
%% Equivalent to
%% <a href="#histogram_metric-3=4">
%% <tt>histogram_metric(Labels, Buckets, Count, Sum)</tt></a>.
%% @end
-spec histogram_metric(Spec) -> prometheus_model:'Metric'() when
    Spec    :: {Labels, Buckets, Count, Sum} | {Buckets, Count, Sum},
    Labels  :: [label()],
    Buckets :: [{Bound, Count}],
    Bound   :: prometheus_buckets:bucket_bound(),
    Count   :: non_neg_integer(),
    Sum     :: value().
histogram_metric({Labels, Buckets, Count, Sum}) ->
  histogram_metric(Labels, Buckets, Count, Sum);
histogram_metric({Buckets, Count, Sum}) ->
  histogram_metric([], Buckets, Count, Sum).

%% @equiv histogram_metric([], Buckets, Count, Sum)
histogram_metric(Buckets, Count, Sum) ->
  histogram_metric([], Buckets, Count, Sum).

%% @doc
%% Creates histogram metric with `Labels', `Buckets', `Count' and `Sum'.
%% @end
-spec histogram_metric(Labels, Buckets, Count, Sum) -> Metric when
    Labels  :: [label()],
    Buckets :: [{Bound, Count}],
    Bound   :: prometheus_buckets:bucket_bound(),
    Count   :: non_neg_integer(),
    Sum     :: value(),
    Metric  :: prometheus_model:'Metric'().
histogram_metric(Labels, Buckets, Count, Sum) ->
  Label  = label_pairs(Labels),
  Bucket = histogram_buckets(Buckets),
  #'Metric'{label     = Label,
            histogram = #'Histogram'{sample_count = Count,
                                     sample_sum   = Sum,
                                     bucket       = Bucket}}.

%% @doc Equivalent to
%% {@link label_pair/1. `lists:map(fun label_pair/1, Labels)'}.
%% @end
label_pairs(Labels) -> lists:map(fun label_pair/1, Labels).

%% @doc
%% Creates `prometheus_model:'LabelPair'()' from {Name, Value} tuple.
%% @end
-spec label_pair(label()) -> prometheus_model:'LabelPair'().
label_pair({Name, Value}) ->
  #'LabelPair'{name  = ensure_binary_or_string(Name),
               value = ensure_binary_or_string(Value)}.

%%%===================================================================
%%% Private Parts
%%%===================================================================

%% @doc Equivalent to
%% {@link histogram_bucket/1. `lists:map(fun histogram_bucket/1, Specs)'}.
histogram_buckets(Specs) -> lists:map(fun histogram_bucket/1, Specs).

-spec histogram_bucket({Bound, Count}) -> Buckets when
    Bound   :: prometheus_buckets:bucket_bound(),
    Count   :: non_neg_integer(),
    Buckets :: prometheus_model:'Bucket'().
histogram_bucket({Bound, Count}) ->
  #'Bucket'{upper_bound      = Bound,
            cumulative_count = Count}.

-spec ensure_list(Val :: term()) -> list().
ensure_list(Val) when is_list(Val) ->  Val;
ensure_list(Val)                   -> [Val].

%% @private
filter_undefined_metrics(Metrics) -> lists:filter(fun not_undefined/1, Metrics).

not_undefined(undefined) -> false;
not_undefined(_)         -> true.

%% @private
-spec ensure_binary_or_string(Val :: term())     -> binary() | string().
ensure_binary_or_string(Val) when is_atom(Val)   -> atom_to_binary(Val, utf8);
ensure_binary_or_string(Val) when is_list(Val)   -> Val; %% FIXME: validate utf8
ensure_binary_or_string(Val) when is_binary(Val) -> Val;
ensure_binary_or_string(Val) ->
  io_lib:format("~p", [Val]).

%% @private
-spec ensure_mf_type(atom()) -> atom().
ensure_mf_type(gauge)     -> 'GAUGE';
ensure_mf_type(counter)   -> 'COUNTER';
ensure_mf_type(summary)   -> 'SUMMARY';
ensure_mf_type(histogram) -> 'HISTOGRAM';
ensure_mf_type(untyped)   -> 'UNTYPED';
ensure_mf_type(Type)      -> erlang:error({invalid_metric_type, Type}).
