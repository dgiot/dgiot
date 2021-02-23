%% @doc
%% Summary metric, to track the size of events.
%%
%% Example use cases for Summaries:
%%   - Response latency;
%%   - Request size;
%%   - Response size.
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_proxy_instrumenter).
%%
%% setup() ->
%%   prometheus_summary:declare([{name, request_size_bytes},
%%                               {help, "Request size in bytes."}]),
%%   prometheus_summary:declare([{name, response_size_bytes},
%%                               {help, "Response size in bytes."}]).
%%
%% observe_request(Size) ->
%%   prometheus_summary:observe(request_size_bytes, Size).
%%
%% observe_response(Size) ->
%%   prometheus_summary:observe(response_size_bytes, Size).
%% </pre>
%% @end

-module(prometheus_summary).

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
         value/3]).

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

-import(prometheus_model_helpers, [create_mf/5,
                                   summary_metrics/1,
                                   summary_metric/1,
                                   summary_metric/2,
                                   counter_metric/1,
                                   counter_metric/2,
                                   summary_metric/3]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).
-behaviour(gen_server).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_SUMMARY_TABLE).
-define(SUM_POS, 3).
-define(COUNTER_POS, 2).
-define(WIDTH, 16).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a summary using `Spec'.
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
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a summary
%% with the same `Spec' already exists.
%% @end
new(Spec) ->
  validate_summary_spec(Spec),
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_summary:new/2", "prometheus_summary:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

%% @doc Creates a summary using `Spec'.
%% If a summary with the same `Spec' exists returns `false'.
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
%% @end
declare(Spec) ->
  Spec1 = validate_summary_spec(Spec),
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

%% @deprecated Please use {@link declare/1} with registry
%% key instead.
declare(Spec, Registry) ->
  ?DEPRECATED("prometheus_summary:declare/2", "prometheus_summary:declare/1"
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
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
observe(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, key(Registry, Name, LabelValues),
                       [{?COUNTER_POS, 1}, {?SUM_POS, Value}])
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun observe/4)
  end,
  ok;
observe(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value, "observe accepts only integers"}).

%% @equiv dobserve(default, Name, [], Value)
dobserve(Name, Value) ->
  dobserve(default, Name, [], Value).

%% @equiv dobserve(default, Name, LabelValues, Value)
dobserve(Name, LabelValues, Value) ->
  dobserve(default, Name, LabelValues, Value).

%% @doc Observes the given `Value'.
%% If `Value' happened to be a float number even one time(!) you
%% shouldn't use {@link observe/4} after dobserve.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a number.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
dobserve(Registry, Name, LabelValues, Value) when is_number(Value) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  CallTimeout = prometheus_metric:mf_call_timeout(MF),
  case CallTimeout of
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
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% Raises `{invalid_value, Value, Message}' if `Fun'
%% isn't a function.<br/>
%% @end
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun)->
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

%% @doc Removes summary series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
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

%% @doc Resets the value of the summary identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case lists:usort([ets:update_element(?TABLE,
                                       {Registry, Name, LabelValues, Scheduler},
                                       [{?COUNTER_POS, 0}, {?SUM_POS, 0}])
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

%% @doc Returns the value of the summary identified by `Registry', `Name'
%% and `LabelValues'. If there is no summary for `LabelValues',
%% returns `undefined'.
%%
%% If duration unit set, sum will be converted to the duration unit.
%% {@link prometheus_time. Read more here.}
%%
%% Raises `{unknown_metric, Registry, Name}' error if summary named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  DU = prometheus_metric:mf_duration_unit(MF),

  case ets:select(?TABLE, [{{{Registry, Name, LabelValues, '_'}, '$1', '$2'},
                            [],
                            ['$$']}]) of
    [] -> undefined;
    Values -> {Count, Sum} = reduce_values(Values),
              {Count,  prometheus_time:maybe_convert_to_du(DU, Sum)}
  end.

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_', '_'}, '_', '_'}),
  ok.

%% @private
collect_mf(Registry, Callback) ->
  [Callback(create_summary(Name, Help, {Labels, Registry, DU})) ||
    [Name, {Labels, Help}, _, DU, _] <- prometheus_metric:metrics(?TABLE,
                                                                  Registry)],
  ok.

%% @private
collect_metrics(Name, {Labels, Registry, DU}) ->
  MFValues = ets:match(?TABLE, {{Registry, Name, '$1', '_'}, '$2', '$3'}),
  [begin
     {Count, Sum} = reduce_label_values(LabelValues, MFValues),
     summary_metric(lists:zip(Labels, LabelValues), Count,
                    prometheus_time:maybe_convert_to_du(DU, Sum))
   end ||
    LabelValues <- collect_unique_labels(MFValues)].

%%====================================================================
%% Gen_server API
%%====================================================================

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

%% @private
start_link() ->
  gen_server:start_link({local, prometheus_summary},
                        prometheus_summary, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

validate_summary_spec(Spec) ->
  Labels = prometheus_metric_spec:labels(Spec),
  validate_summary_labels(Labels),
  Spec.

validate_summary_labels(Labels) ->
  [raise_error_if_quantile_label_found(Label) || Label <- Labels].

raise_error_if_quantile_label_found("quantile") ->
  erlang:error({invalid_metric_label_name, "quantile",
                "summary cannot have a label named \"quantile\""});
raise_error_if_quantile_label_found(Label) ->
  Label.

dobserve_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, key(Registry, Name, LabelValues)) of
    [Metric] ->
      ets:update_element(?TABLE, key(Registry, Name, LabelValues),
                         {?SUM_POS, sum(Metric) + Value}),
      ets:update_counter(?TABLE, key(Registry, Name, LabelValues),
                         {?COUNTER_POS, 1});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dobserve_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {key(Registry, Name, LabelValues), 1, Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

sum(Metric) ->
  element(?SUM_POS, Metric).

schedulers_seq() ->
  lists:seq(0, ?WIDTH-1).

key(Registry, Name, LabelValues) ->
  X = erlang:system_info(scheduler_id),
  Rnd = X band (?WIDTH-1),
  {Registry, Name, LabelValues, Rnd}.

collect_unique_labels(MFValues) ->
  lists:usort([L || [L, _, _] <- MFValues]).

reduce_label_values(Labels, MFValues) ->
  {lists:sum([C || [L, C, _] <- MFValues, L == Labels]),
   lists:sum([S || [L, _, S] <- MFValues, L == Labels])}.

reduce_values(Values) ->
  {lists:sum([C || [C, _] <- Values]),
   lists:sum([S || [_, S] <- Values])}.

create_summary(Name, Help, Data) ->
  prometheus_model_helpers:create_mf(Name, Help, summary, ?MODULE, Data).
