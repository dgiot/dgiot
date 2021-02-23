%% @doc
%% A Histogram tracks the size and number of events in buckets.
%% You can use Histograms for aggregatable calculation of quantiles.
%%
%% Example use cases for Histograms:
%% <ul>
%%   <li>Response latency</li>
%%   <li>Request size</li>
%% </ul>
%%
%% Histogram expects `buckets` key in a metric spec. Buckets can be:
%%   - a list of numbers in increasing order;
%%  one of the generate specs (shortcuts for `prometheus_buckets' functions)
%%   - :default;
%%   - {:linear, start, step, count};
%%   - {:exponential, start, step, count}
%%
%% Example:
%% <pre lang="erlang">
%% -module(example_instrumenter).
%% setup() ->
%%   prometheus_histogram:declare([{name, http_request_duration_milliseconds},
%%                                 {labels, [method]},
%%                                 {buckets, [100, 300, 500, 750, 1000]},
%%                                 {help, "Http Request execution time."}]).
%%
%% instrument(Time, Method) ->
%%   %% Time must be in native units, otherwise duration_unit must be false
%%   prometheus_histogram:observe(http_request_duration_milliseconds,
%%                                [Method], Time).
%%
%% </pre>
%% @end

-module(prometheus_histogram).

%%% metric
-export([new/1,
         new/2,
         declare/1,
         declare/2,
         observe/2,
         observe/3,
         observe/4,
         dobserve/2,
         dobserve/3,
         dobserve/4,
         observe_duration/2,
         observe_duration/3,
         observe_duration/4,
         remove/1,
         remove/2,
         remove/3,
         reset/1,
         reset/2,
         reset/3,
         value/1,
         value/2,
         value/3,
         buckets/1,
         buckets/2,
         buckets/3]
       ).

%%% collector
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

%%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-ifdef(TEST).
-export([default_buckets/0,
         linear_buckets/3,
         exponential_buckets/3]).
-endif.

-import(prometheus_model_helpers, [create_mf/5,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   histogram_metric/3,
                                   histogram_metric/4]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_HISTOGRAM_TABLE).
-define(BUCKETS_POS, 2).
-define(BUCKETS_START, 3).
-define(WIDTH, 16).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a histogram using `Spec'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Soec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.<br/>
%% Raises `{invalid_value_error, Value, Message}' error if `duration_unit' is
%% unknown or doesn't match metric name.<br/>
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a histogram
%% with the same `Spec' already exists.
%%
%% Histogram-specific errors:<br/>
%% Raises `{histogram_no_buckets, Buckets}' error if `Buckets' are missing,
%% not a list, empty list or not known buckets spec.<br/>
%% Raises `{histogram_invalid_buckets, Buckets, Message}' error if `Buckets'
%% aren't in increasing order.<br/>
%% Raises `{histogram_invalid_bound, Bound}' error if `Bound' isn't a number.
%% @end
new(Spec) ->
  Spec1 = validate_histogram_spec(Spec),
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec1).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_histogram:new/2", "prometheus_histogram:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

%% @doc Creates a histogram using `Spec'.
%% If a histogram with the same `Spec' exists returns `false'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Soec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.<br/>
%% Raises `{invalid_value_error, Value, MessagE}' error if `duration_unit' is
%% unknown or doesn't match metric name.<br/>
%%
%% Histogram-specific errors:<br/>
%% Raises `{histogram_no_buckets, Buckets}' error if `Buckets' are missing,
%% not a list, empty list or not known buckets spec.<br/>
%% Raises `{histogram_invalid_buckets, Buckets, Message}' error if `Buckets'
%% aren't in increasing order.<br/>
%% Raises `{histogram_invalid_bound, Bound}' error if `Bound' isn't a number.
%% @end
declare(Spec) ->
  Spec1 = validate_histogram_spec(Spec),
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

%% @deprecated Please use {@link declare/1} with registry
%% key instead.
declare(Spec, Registry) ->
  ?DEPRECATED("prometheus_histogram:declare/2", "prometheus_histogram:declare/1"
              " with registry key"),
  declare([{registry, Registry} | Spec]).

%% @equiv observe(default, Name, [], Value)
observe(Name, Value) ->
  observe(default, Name, [], Value).

%% @equiv observe(default, Name, LabelValues, Value)
observe(Name, LabelValues, Value) ->
  observe(default, Name, LabelValues, Value).

%% @doc Observes the given `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't an integer.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if histogram with named
%% `Name' can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
observe(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  case ets:lookup(?TABLE, key(Registry, Name, LabelValues)) of
    [Metric] ->
      {BucketPosition, SumPosition} =
        calculate_histogram_update_positions(Metric, Value),
      ets:update_counter(?TABLE, key(Registry, Name, LabelValues),
                         [{BucketPosition, 1}, {SumPosition, Value}]);
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end,
  ok;
observe(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "observe accepts only integers"}).

%% @equiv dobserve(default, Name, [], Value)
dobserve(Name, Value) ->
  dobserve(default, Name, [], Value).

%% @equiv dobserve(default, Name, LabelValues, [], Value)
dobserve(Name, LabelValues, Value) ->
  dobserve(default, Name, LabelValues, Value).

%% @doc Observes the given `Value'.
%% If `Value' happened to be a float number even one time(!) you
%% shouldn't use {@link observe/4} after dobserve.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a number.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if histogram with named
%% `Name' can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
dobserve(Registry, Name, LabelValues, Value) when is_number(Value) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  CallTimeout = prometheus_metric:mf_call_timeout(MF),
  case prometheus_metric:mf_call_timeout(MF) of
    false ->
      gen_server:cast(?MODULE,
                      {observe, {Registry, Name, LabelValues, Value}});
    _ ->
      gen_server:call(?MODULE,
                      {observe, {Registry, Name, LabelValues, Value}},
                      CallTimeout)
  end,
  ok;
dobserve(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "dobserve accepts only numbers"}).

%% @equiv observe_duration(default, Name, [], Fun)
observe_duration(Name, Fun) ->
  observe_duration(default, Name, [], Fun).

%% @equiv observe_duration(default, Name, LabelValues, Fun)
observe_duration(Name, LabelValues, Fun) ->
  observe_duration(default, Name, LabelValues, Fun).

%% @doc Tracks the amount of time spent executing `Fun'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if histogram with named
%% `Name' can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% Raises `{invalid_value, Value, Message}' if `Fun'
%% isn't a function.<br/>
%% @end
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun) ->
  Start = erlang:monotonic_time(),
  try
    Fun()
  after
    observe(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
  end;
observe_duration(_Regsitry, _Name, _LabelValues, Fun) ->
  erlang:error({invalid_value, Fun, "observe_duration accepts only functions"}).

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

%% @doc Removes histogram series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if histogram with named
%% `Name' can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
remove(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case lists:flatten([ets:take(?TABLE,
                               {Registry, Name, LabelValues, Scheduler})
                      || Scheduler <- schedulers_seq()]) of
    [] -> false;
    _ -> true
  end.

%% @equiv reset(default, Name, [])
reset(Name) ->
  reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
reset(Name, LabelValues) ->
  reset(default, Name, LabelValues).

%% @doc Resets the value of the histogram identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if histogram with named
%% `Name' can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  Buckets = prometheus_metric:mf_data(MF),
  UpdateSpec = generate_update_spec(?BUCKETS_START, length(Buckets)),

  case lists:usort([ets:update_element(?TABLE,
                                       {Registry, Name, LabelValues, Scheduler},
                                       UpdateSpec)
                    || Scheduler <- schedulers_seq()]) of
    [_, _] -> true;
    [true] -> true;
    _ -> false
  end.

%% @equiv value(default, Name, [])
value(Name) ->
  value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
value(Name, LabelValues) ->
  value(default, Name, LabelValues).

%% @doc Returns the value of the histogram identified by `Registry', `Name'
%% and `LabelValues'. If there is no histogram for `LabelValues',
%% returns `undefined'.
%%
%% If duration unit set, sum will be converted to the duration unit.
%% {@link prometheus_time. Read more here.}
%%
%% Raises `{unknown_metric, Registry, Name}' error if histogram named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),

  RawValues = [ets:lookup(?TABLE, {Registry, Name, LabelValues, Scheduler})
               || Scheduler <- schedulers_seq()],
  case lists:flatten(RawValues) of
    [] -> undefined;
    Values -> {reduce_buckets_counters(Values), reduce_sum(MF, Values)}
  end.

%% @equiv buckets(default, Name, [])
buckets(Name) ->
  buckets(default, Name, []).

%% @equiv buckets(default, Name, LabelValues)
buckets(Name, LabelValues) ->
  buckets(default, Name, LabelValues).

%% @doc Returns buckets of the histogram identified by `Registry', `Name'
%% and `LabelValues'.
%% @end
buckets(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  prometheus_metric:mf_data(MF).

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(Registry) ->
  [delete_metrics(Registry, Buckets)
   || [_, _, _, _, Buckets] <- prometheus_metric:metrics(?TABLE, Registry)],
  true = prometheus_metric:deregister_mf(?TABLE, Registry),
  ok.

%% @private
collect_mf(Registry, Callback) ->
  [Callback(create_histogram(Name, Help, {Labels, Registry, DU, Buckets})) ||
    [Name, {Labels, Help}, _, DU, Buckets]
      <- prometheus_metric:metrics(?TABLE, Registry)],
  ok.

%% @private
collect_metrics(Name, {Labels, Registry, DU, MFBuckets}) ->
  BoundPlaceholders = gen_query_bound_placeholders(MFBuckets),
  SumPlaceholder = gen_query_placeholder(sum_position(MFBuckets)),
  QuerySpec =
    [{Registry, Name, '$1', '_'}, '_']
    ++ BoundPlaceholders
    ++ [SumPlaceholder],


  Fun = fun (Bucket) ->
            prometheus_time:maybe_convert_to_native(DU, Bucket)
        end,
  Buckets = lists:map(Fun, MFBuckets),

  MFValues = ets:match(?TABLE, list_to_tuple(QuerySpec)),
  [begin
     Stat = reduce_label_values(LabelValues, MFValues),
     create_histogram_metric(Labels, DU, Buckets, LabelValues, Stat)
   end ||
    LabelValues <- collect_unique_labels(MFValues)].

%%====================================================================
%% Gen_server API
%%====================================================================

%% @private
start_link() ->
  gen_server:start_link({local, prometheus_histogram},
                        prometheus_histogram, [], []).

%% @private
init(_Args) ->
  {ok, []}.

%% @private
handle_call({observe, {Registry, Name, LabelValues, Value}}, _From, State) ->
  dobserve_impl(Registry, Name, LabelValues, Value),
  {reply, ok, State}.

%% @private
handle_cast({observe, {Registry, Name, LabelValues, Value}}, State) ->
  dobserve_impl(Registry, Name, LabelValues, Value),
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Private Parts
%%====================================================================

validate_histogram_spec(Spec) ->
  Labels = prometheus_metric_spec:labels(Spec),
  validate_histogram_labels(Labels),
  RBuckets = prometheus_metric_spec:get_value(buckets, Spec, default_buckets()),
  Buckets = validate_buckets(RBuckets),
  [{data, Buckets}|Spec].

validate_histogram_labels(Labels) ->
  [raise_error_if_le_label_found(Label) || Label <- Labels].

raise_error_if_le_label_found("le") ->
  erlang:error({invalid_metric_label_name, "le",
                "histogram cannot have a label named \"le\""});
raise_error_if_le_label_found(Label) ->
  Label.

default_buckets () ->
  prometheus_buckets:default().

linear_buckets(Start, Step, Count) ->
  prometheus_buckets:linear(Start, Step, Count).

exponential_buckets(Start, Factor, Count) ->
  prometheus_buckets:exponential(Start, Factor, Count).

validate_buckets([]) ->
  erlang:error({histogram_no_buckets, []});
validate_buckets(undefined) ->
  erlang:error({histogram_no_buckets, undefined});
validate_buckets(default) ->
  default_buckets() ++ [infinity];
validate_buckets({linear, Start, Step, Count}) ->
  linear_buckets(Start, Step, Count) ++ [infinity];
validate_buckets({exponential, Start, Factor, Count}) ->
  exponential_buckets(Start, Factor, Count) ++ [infinity];
validate_buckets(RawBuckets) when is_list(RawBuckets) ->
  Buckets = lists:map(fun validate_histogram_bound/1, RawBuckets),
  case lists:sort(Buckets) of
    Buckets ->
      Buckets ++ [infinity];
    _ ->
      erlang:error({histogram_invalid_buckets, Buckets, "buckets not sorted"})
  end;
validate_buckets(Buckets) ->
  erlang:error({histogram_invalid_buckets, Buckets, "not a list"}).

validate_histogram_bound(Bound) when is_number(Bound) ->
  Bound;
validate_histogram_bound(Bound) ->
  erlang:error({histogram_invalid_bound, Bound}).

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, key(Registry, Name, LabelValues)) of
    [Metric] ->
      {BucketPosition, SumPosition} =
        calculate_histogram_update_positions(Metric, Value),
      ets:update_element(?TABLE, key(Registry, Name, LabelValues),
                         {SumPosition, sum(Metric) + Value}),
      ets:update_counter(?TABLE, key(Registry, Name, LabelValues),
                         {BucketPosition, 1});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dobserve_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, CB) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  MFBuckets = prometheus_metric:mf_data(MF),
  DU = prometheus_metric:mf_duration_unit(MF),
  Fun = fun (Bucket) ->
            prometheus_time:maybe_convert_to_native(DU, Bucket)
        end,
  BoundCounters = lists:duplicate(length(MFBuckets), 0),
  MetricSpec =
    [key(Registry, Name, LabelValues), lists:map(Fun, MFBuckets)]
    ++ BoundCounters
    ++ [0],
  ets:insert(?TABLE, list_to_tuple(MetricSpec)),
  CB(Registry, Name, LabelValues, Value).

calculate_histogram_update_positions(Metric, Value) ->
  Buckets = metric_buckets(Metric),
  BucketPosition = ?BUCKETS_POS + position(Buckets, fun(Bound) ->
                                                        Value =< Bound
                                                    end),
  SumPosition = sum_position(Metric),
  {BucketPosition, SumPosition}.

generate_update_spec(BucketsStart, BucketsCount) ->
  [{Index, 0} ||
    Index <- lists:seq(BucketsStart, ?BUCKETS_START + BucketsCount)].

gen_query_placeholder(Index) ->
  list_to_atom("$" ++ integer_to_list(Index)).

gen_query_bound_placeholders(Buckets) ->
  [gen_query_placeholder(Index) ||
    Index <- lists:seq(?BUCKETS_START, ?BUCKETS_POS + length(Buckets))].

augment_counters([Start | Counters]) ->
  augment_counters(Counters, [Start], Start).

augment_counters([], LAcc, _CAcc) ->
  LAcc;
augment_counters([Counter | Counters], LAcc, CAcc) ->
  augment_counters(Counters, LAcc ++ [CAcc + Counter], CAcc + Counter).

metric_buckets(Metric) ->
  element(?BUCKETS_POS, Metric).

reduce_buckets_counters(Metrics) ->
  ABuckets =
    [sub_tuple_to_list(Metric, ?BUCKETS_START,
                       ?BUCKETS_START + length(metric_buckets(Metric)))
     || Metric <- Metrics],
  [lists:sum(Bucket) || Bucket <- transpose(ABuckets)].

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

sum_position(Metric) when is_tuple(Metric) ->
  ?BUCKETS_START + length(metric_buckets(Metric));
sum_position(Buckets) when is_list(Buckets) ->
  ?BUCKETS_START + length(Buckets).

sum(Metric) ->
  element(sum_position(Metric), Metric).

reduce_sum(Metrics) ->
  lists:sum([element(sum_position(Metric), Metric) || Metric <- Metrics]).

reduce_sum(MF, Metrics) ->
  DU = prometheus_metric:mf_duration_unit(MF),
  prometheus_time:maybe_convert_to_du(DU, reduce_sum(Metrics)).

create_histogram_metric(Labels, DU, Buckets, LabelValues, Stat) ->
  Fun = fun(Bound) ->
            prometheus_time:maybe_convert_to_du(DU, Bound)
        end,
  BoundValues = lists:sublist(Stat, 1, length(Buckets)),
  BCounters = augment_counters(BoundValues),
  Buckets1 = lists:zipwith(fun(Bound, BCounter) ->
                               {Bound, BCounter}
                           end,
                           lists:map(Fun, Buckets), BCounters),
  histogram_metric(lists:zip(Labels, LabelValues),
                   Buckets1,
                   lists:last(BCounters),
                   prometheus_time:maybe_convert_to_du(DU, lists:last(Stat))).

delete_metrics(Registry, Buckets) ->
  BoundCounters = lists:duplicate(length(Buckets), '_'),
  MetricSpec = [{Registry, '_', '_', '_'}, '_'] ++ BoundCounters ++ ['_'],
  ets:match_delete(?TABLE, list_to_tuple(MetricSpec)).

sub_tuple_to_list(Tuple, Pos, Size) when Pos < Size ->
  [element(Pos, Tuple) | sub_tuple_to_list(Tuple, Pos + 1, Size)];
sub_tuple_to_list(_Tuple, _Pos, _Size) -> [].

position(List, Pred) ->
  position(List, Pred, 1).

position([], _Pred, _Pos) ->
  0;
position([H|L], Pred, Pos) ->
  case Pred(H) of
    true ->
      Pos;
    false ->
      position(L, Pred, Pos + 1)
  end.

schedulers_seq() ->
  lists:seq(0, ?WIDTH-1).

key(Registry, Name, LabelValues) ->
  X = erlang:system_info(scheduler_id),
  Rnd = X band (?WIDTH-1),
  {Registry, Name, LabelValues, Rnd}.

collect_unique_labels(MFValues) ->
  lists:usort([L || [L | _] <- MFValues]).

reduce_label_values(Labels, MFValues) ->
  [lists:sum(C)
   || C <- transpose([V || [L | V] <- MFValues, L == Labels])].

create_histogram(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, histogram, ?MODULE, Data).
