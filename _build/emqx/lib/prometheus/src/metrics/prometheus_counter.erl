%% @doc
%% Counter is a Metric that represents a single numerical value that only ever
%% goes up. That implies that it cannot be used to count items whose number can
%% also go down, e.g. the number of currently running processes. Those
%% "counters" are represented by {@link prometheus_gauge}.
%%
%% A Counter is typically used to count requests served, tasks completed, errors
%% occurred, etc.
%%
%% Examople use cases for Counters:
%% <ul>
%%   <li>Number of requests processed</li>
%%   <li>Number of items that were inserted into a queue</li>
%%   <li>Total amount of data a system has processed</li>
%% </ul>
%%
%% Use the
%% <a href="https://prometheus.io/docs/querying/functions/#rate()">rate()</a>/<a
%% href="https://prometheus.io/docs/querying/functions/#irate()">irate()</a>
%% functions in Prometheus to calculate the rate of increase of a Counter.
%% By convention, the names of Counters are suffixed by `_total'.
%%
%% To create a counter use either {@link new/1} or {@link declare/1},
%% the difference is that {@link new/1} will raise
%% {:mf_already_exists, {Registry, Name}, Message} error if counter with
%% the same `Registry', `Name' and `Labels' combination already exists.
%% Both accept `Spec' [proplist](http://erlang.org/doc/man/proplists.html)
%% with the same set of keys:
%%
%%  - `Registry' - optional, default is `default';
%%  - `Name' - required, can be an atom or a string;
%%  - `Help' - required, must be a string;
%%  - `Labels' - optional, default is `[]'.
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_service_instrumenter).
%%
%% -export([setup/0,
%%          inc/1]).
%%
%% setup() ->
%%   prometheus_counter:declare([{name, my_service_requests_total},
%%                               {help, "Requests count"},
%%                               {labels, caller}]).
%%
%% inc(Caller) ->
%%   prometheus_counter:inc(my_service_requests_total, [Caller]).
%%
%% </pre>
%% @end

-module(prometheus_counter).

%%% metric
-export([new/1,
         new/2,
         declare/1,
         declare/2,
         inc/1,
         inc/2,
         inc/3,
         inc/4,
         dinc/1,
         dinc/2,
         dinc/3,
         dinc/4,
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
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).
-behaviour(gen_server).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_COUNTER_TABLE).
-define(SUM_POS, 2).
-define(WIDTH, 16).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a counter using `Spec'.
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
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a counter
%% with the same `Spec' already exists.
%% @end
new(Spec) ->
  prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @deprecated Please use {@link new/1} with registry
%% key instead.
new(Spec, Registry) ->
  ?DEPRECATED("prometheus_counter:new/2", "prometheus_counter:new/1"
              " with registry key"),
  new([{registry, Registry} | Spec]).

%% @doc Creates a counter using `Spec', if a counter with the same `Spec' exists
%% returns `false'.
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
%% label name.
%% @end
declare(Spec) ->
  prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

%% @deprecated Please use {@link declare/1} with registry
%% key instead.
declare(Spec, Registry) ->
  ?DEPRECATED("prometheus_counter:declare/2", "prometheus_counter:declare/1"
              " with registry key"),
  declare([{registry, Registry} | Spec]).

%% @equiv inc(default, Name, [], 1)
inc(Name) ->
  inc(default, Name, [], 1).

%% @doc If the second argument is a list, equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, LabelValues, 1)</tt></a>
%% otherwise equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, [], Value)</tt></a>.
inc(Name, LabelValues) when is_list(LabelValues)->
  inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
  inc(default, Name, [], Value).

%% @equiv inc(default, Name, LabelValues, Value)
inc(Name, LabelValues, Value) ->
  inc(default, Name, LabelValues, Value).

%% @doc Increments the counter identified by `Registry', `Name'
%% and `LabelValues' by `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a positive integer.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if counter with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
inc(_Registry, _Name, _LabelValues, Value) when Value < 0 ->
  erlang:error({invalid_value, Value,
                "inc accepts only non-negative integers"});
inc(Registry, Name, LabelValues, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE,
                       key(Registry, Name, LabelValues),
                       {?SUM_POS, Value})
  catch error:badarg ->
      insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
  end,
  ok;
inc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "inc accepts only non-negative integers"}).

%% @equiv dinc(default, Name, [], 1)
dinc(Name) ->
  dinc(default, Name, [], 1).

%% @doc If the second argument is a list, equivalent to
%% <a href="#dinc-4"><tt>dinc(default, Name, LabelValues, 1)</tt></a>
%% otherwise equivalent to
%% <a href="#dinc-4"><tt>dinc(default, Name, [], Value)</tt></a>.
dinc(Name, LabelValues) when is_list(LabelValues)->
  dinc(default, Name, LabelValues, 1);
dinc(Name, Value) when is_number(Value) ->
  dinc(default, Name, [], Value).

%% @equiv dinc(default, Name, LabelValues, Value)
dinc(Name, LabelValues, Value) ->
  dinc(default, Name, LabelValues, Value).

%% @doc Increments the counter identified by `Registry', `Name'
%% and `LabelValues' by `Value'.
%% If `Value' happened to be a float number even one time(!) you
%% shouldn't use {@link inc/4} after dinc.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a number.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if counter with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
dinc(_Registry, _Name, _LabelValues, Value) when Value < 0 ->
  erlang:error({invalid_value, Value,
                "dinc accepts only non-negative numbers"});
dinc(Registry, Name, LabelValues, Value) when is_number(Value) ->
  MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  CallTimeout = prometheus_metric:mf_call_timeout(MF),
  case CallTimeout of
    false ->
      gen_server:cast(?MODULE,
                      {inc, {Registry, Name, LabelValues, Value}});
    _ -> gen_server:call(?MODULE,
                         {inc, {Registry, Name, LabelValues, Value}},
                         CallTimeout)
  end,
  ok;
dinc(_Registry, _Name, _LabelValues, Value) ->
  erlang:error({invalid_value, Value,
                "dinc accepts only non-negative numbers"}).

%% @equiv remove(default, Name, [])
remove(Name) ->
  remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
remove(Name, LabelValues) ->
  remove(default, Name, LabelValues).

%% @doc Removes counter series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter with named `Name'
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

%% @doc Resets the value of the counter identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
reset(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case lists:usort([ets:update_element(?TABLE,
                                       {Registry, Name, LabelValues, Scheduler},
                                       {?SUM_POS, 0})
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

%% @doc Returns the value of the counter identified by `Registry', `Name'
%% and `LabelValues'. If there is no counter for `LabelValues',
%% returns `undefined'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
value(Registry, Name, LabelValues) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:select(?TABLE, [{{{Registry, Name, LabelValues, '_'}, '$1'},
                            [],
                            ['$1']}]) of
    [] -> undefined;
    List -> lists:sum(List)
  end.

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(Registry) ->
  prometheus_metric:deregister_mf(?TABLE, Registry),
  true = ets:match_delete(?TABLE, {{Registry, '_', '_', '_'}, '_'}),
  ok.

%% @private
collect_mf(Registry, Callback) ->
  [Callback(create_counter(Name, Help, {Labels, Registry})) ||
    [Name, {Labels, Help}, _, _, _] <- prometheus_metric:metrics(?TABLE,
                                                                 Registry)],
  ok.

%% @private
collect_metrics(Name, {Labels, Registry}) ->
  MFValues = ets:match(?TABLE, {{Registry, Name, '$1', '_'}, '$2'}),
  [begin
     Value = reduce_label_values(LabelValues, MFValues),
     counter_metric(lists:zip(Labels, LabelValues), Value)
   end ||
    LabelValues <- collect_unique_labels(MFValues)].

%%====================================================================
%% Gen_server API
%%====================================================================

%% @private
init(_Args) ->
  {ok, []}.

%% @private
handle_call({inc, {Registry, Name, LabelValues, Value}}, _From, State) ->
  dinc_impl(Registry, Name, LabelValues, Value),
  {reply, ok, State}.

%% @private
handle_cast({inc, {Registry, Name, LabelValues, Value}}, State) ->
  dinc_impl(Registry, Name, LabelValues, Value),
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
  gen_server:start_link({local, prometheus_counter},
                        prometheus_counter, [], []).

%%====================================================================
%% Private Parts
%%====================================================================

dinc_impl(Registry, Name, LabelValues, Value) ->
  case ets:lookup(?TABLE, key(Registry, Name, LabelValues)) of
    [{_Key, OldValue}] ->
      ets:update_element(?TABLE, key(Registry, Name, LabelValues),
                         {?SUM_POS, Value + OldValue});
    [] ->
      insert_metric(Registry, Name, LabelValues, Value, fun dinc_impl/4)
  end.

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
  prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
  case ets:insert_new(?TABLE, {key(Registry, Name, LabelValues), Value}) of
    false -> %% some sneaky process already inserted
      ConflictCB(Registry, Name, LabelValues, Value);
    true ->
      ok
  end.

schedulers_seq() ->
  lists:seq(0, ?WIDTH-1).

key(Registry, Name, LabelValues) ->
  X = erlang:system_info(scheduler_id),
  Rnd = X band (?WIDTH-1),
  {Registry, Name, LabelValues, Rnd}.

collect_unique_labels(MFValues) ->
  lists:usort([L || [L, _] <- MFValues]).

reduce_label_values(Labels, MFValues) ->
  lists:sum([Y || [L, Y] <- MFValues, L == Labels]).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).
