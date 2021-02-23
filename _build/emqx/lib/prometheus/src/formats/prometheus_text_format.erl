%% @doc
%%
%% Serializes Prometheus registry using the latest
%% [text format](http://bit.ly/2cxSuJP).
%%
%% Example output:
%% <pre>
%%   # TYPE http_request_duration_milliseconds histogram
%%   # HELP http_request_duration_milliseconds Http Request execution time
%%   http_request_duration_milliseconds_bucket{method="post",le="100"} 0
%%   http_request_duration_milliseconds_bucket{method="post",le="300"} 1
%%   http_request_duration_milliseconds_bucket{method="post",le="500"} 3
%%   http_request_duration_milliseconds_bucket{method="post",le="750"} 4
%%   http_request_duration_milliseconds_bucket{method="post",le="1000"} 5
%%   http_request_duration_milliseconds_bucket{method="post",le="+Inf"} 6
%%   http_request_duration_milliseconds_count{method="post"} 6
%%   http_request_duration_milliseconds_sum{method="post"} 4350
%% </pre>
%% @end

-module(prometheus_text_format).
-export([content_type/0,
         format/0,
         format/1]).

-ifdef(TEST).
-export([escape_metric_help/1,
         escape_label_value/1,
         emit_mf_prologue/2,
         emit_mf_metrics/2
        ]).
-endif.

-include("prometheus.hrl").
-include("prometheus_model.hrl").

-behaviour(prometheus_format).

%%====================================================================
%% Macros
%%====================================================================

-define(ESCAPE_LVALUE(Value),
        sub(sub(sub(Value, "\\", "\\\\\\\\"), "\n", "\\\\n"), "\"", "\\\\\"")).

%%====================================================================
%% Format API
%%====================================================================

-spec content_type() -> binary().
%% @doc
%% Returns content type of the latest [text format](http://bit.ly/2cxSuJP).
%% @end
content_type() ->
  <<"text/plain; version=0.0.4">>.

%% @equiv format(default)
-spec format() -> binary().
%% @doc
%% Formats `default' registry using the latest text format.
%% @end
format() ->
  format(default).

-spec format(Registry :: prometheus_registry:registry()) -> binary().
%% @doc
%% Formats `Registry' using the latest text format.
%% @end
format(Registry) ->
  {ok, Fd} = ram_file:open("", [write, read, binary]),
  Callback = fun (_, Collector) ->
                 registry_collect_callback(Fd, Registry, Collector)
             end,
  prometheus_registry:collect(Registry, Callback),
  file:write(Fd, io_lib:format("\n", [])),
  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

%%====================================================================
%% Private Parts
%%====================================================================

registry_collect_callback(Fd, Registry, Collector) ->
  Callback = fun (MF) ->
                 emit_mf_prologue(Fd, MF),
                 emit_mf_metrics(Fd, MF)
             end,
  prometheus_collector:collect_mf(Registry, Collector, Callback).

%% @private
emit_mf_prologue(Fd, #'MetricFamily'{name=Name, type=Type}) ->
  Bytes = io_lib:format("# TYPE ~s ~s\n",[Name, string_type(Type)]),
  file:write(Fd, Bytes).

%% @private
emit_mf_metrics(Fd, #'MetricFamily'{name=Name, metric = Metrics}) ->
  [emit_metric(Fd, Name, Metric) || Metric <- Metrics].

emit_metric(Fd, Name, #'Metric'{label=Labels,
                                counter=#'Counter'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                gauge=#'Gauge'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                untyped=#'Untyped'{value=Value}}) ->
  emit_series(Fd, Name, Labels, Value);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                summary=#'Summary'{sample_count=Count,
                                                   sample_sum=Sum}}) ->
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum);
emit_metric(Fd, Name, #'Metric'{label=Labels,
                                histogram=#'Histogram'{sample_count=Count,
                                                       sample_sum=Sum,
                                                       bucket=Buckets}}) ->
  [emit_histogram_bucket(Fd, Name, Labels, Bucket) || Bucket <- Buckets],
  emit_series(Fd, [Name, "_count"], Labels, Count),
  emit_series(Fd, [Name, "_sum"], Labels, Sum).

emit_histogram_bucket(Fd, Name, Labels, #'Bucket'{cumulative_count=BCount,
                                                  upper_bound=BBound}) ->
  BLValue = bound_to_label_value(BBound),
  emit_series(Fd, [Name, "_bucket"],
              Labels ++ [#'LabelPair'{name="le", value=BLValue}], BCount).

string_type('COUNTER') ->
  "counter";
string_type('GAUGE') ->
  "gauge";
string_type('SUMMARY') ->
  "summary";
string_type('HISTOGRAM') ->
  "histogram";
string_type('UNTYPED') ->
  "untyped".

labels_string([])     -> "";
labels_string(Labels) ->
  Fun = fun (#'LabelPair'{name=Name, value=Value}) ->
            io_lib:format("~s=\"~s\"", [Name, escape_label_value(Value)])
        end,
  "{" ++ string:join(lists:map(Fun, Labels), ",") ++ "}".

emit_series(Fd, Name, Labels, undefined) ->
  LString = labels_string(Labels),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " NaN\n", [Name]));
emit_series(Fd, Name, Labels, Value) ->
  LString = labels_string(Labels),
  file:write(Fd, io_lib:format("~s" ++ LString ++ " ~p\n", [Name, Value])).

%% @private
escape_metric_help(Help) ->
  sub(sub(Help, "\\", "\\\\\\\\"), "\n", "\\\\n").

bound_to_label_value(Bound) when is_number(Bound) ->
  Bound;
bound_to_label_value(infinity) ->
  "+Inf".

-spec escape_label_value(binary() | iolist() | undefined) -> string().
%% @private
escape_label_value(LValue) when is_list(LValue)->
  ?ESCAPE_LVALUE(LValue);
escape_label_value(LValue) when is_binary(LValue) ->
  ?ESCAPE_LVALUE(LValue);
escape_label_value(LValue) ->
  ?ESCAPE_LVALUE(io_lib:format("~p", [LValue])).

-spec sub(iodata(), string(), string()) -> string().
sub(Str, Old, New) ->
  RegExp = "\\Q" ++ Old ++ "\\E",
  re:replace(Str, RegExp, New, [global, {return, list}]).
