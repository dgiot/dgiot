%% @doc
%% Measuring time intervals with Prometheus.erl.
%% Measuring time intervals is trivial - you just have to be sure you are using
%% monotonic time source. Basically interval is a difference between
%% start time and end time.
%% Erlang has standard `erlang:monotonic_time' function that returns
%% so called native time units. Native time units are meaningless
%% and have to be converted to seconds (or other units)
%% using `erlang:convert_time_unit'.
%% However as `erlang:convert_time_unit' documentation
%% [warns](http://erlang.org/doc/man/erlang.html#convert_time_unit-3):
%%
%% ```
%% You may lose accuracy and precision when converting between  time units.
%% In order to minimize such loss, collect all data at native time unit and
%% do the conversion on the end result.
%% '''
%%
%% and because Prometheus mandates support for floats,
%% `set_duration/observe_duration` functions always work with
%% native time units and conversion is delayed until scraping/retrieving value.
%% To implement this, metric needs to know desired time unit.
%% Users can specify time unit explicitly via `duration_unit'
%% or implicitly via metric name (preferred, since prometheus best practices
%% guide insists on `<name>_duration_<unit>' metric name format).
%%
%% Possible units:
%%  - microseconds;
%%  - milliseconds;
%%  - seconds;
%%  - minutes;
%%  - hours;
%%  - days;
%%
%% Histogram also converts buckets bounds to native units if
%% duration_unit is provided. It converts it back when scraping or
%% retrieving value.
%%
%% If values already converted to a 'real' unit, conversion can be disabled
%% by setting `duration_unit' to `false'.
%%
%% ## Examples
%%
%% Example where duration unit derived from name:
%% <pre lang="erlang">
%% prometheus_histogram:new([{name, fun_duration_seconds},
%%                           {buckets, [0.5, 1.1]}, %% in seconds
%%                           {help, ""}]),
%% prometheus_histogram:observe_duration(fun_duration_seconds,
%%                                       fun () ->
%%                                           timer:sleep(1000)
%%                                       end),
%% prometheus_histogram:value(fun_duration_seconds).
%% {[0,1,0],1.001030886}
%% </pre>
%%
%% Example where duration unit set explicitly:
%% <pre lang="erlang">
%% prometheus_histogram:new([{name, fun_duration_histogram},
%%                           {buckets, [500, 1100]}, %% in milliseconds
%%                           {help, ""},
%%                           {duration_unit, milliseconds}]),
%%
%% prometheus_histogram:observe_duration(fun_duration_histogram,
%%                                       fun () ->
%%                                           timer:sleep(1000)
%%                                       end),
%%
%% prometheus_histogram:value(fun_duration_histogram).
%% {[0,1,0],1001.885302}
%% </pre>
%%
%% Example where value is in seconds already:
%% <pre lang="erlang">
%% prometheus_histogram:new([{name, duration_seconds},
%%                           {buckets, [0.5, 1.1]}, %% in seconds
%%                           {help, ""},
%%                           {duration_unit, false}]),
%%
%% prometheus_histogram:dobserve(duration_seconds, 1.2),
%%
%% prometheus_histogram:value(duration_seconds).
%% {[0,0,1],1.2}
%% </pre>
%% @end
-module(prometheus_time).

-export([duration_unit_from_string/1,
         validate_duration_unit/1,
         maybe_convert_to_native/2,
         maybe_convert_to_du/2]).

-ifdef(TEST).
-export([from_native/2,
         to_native/2]).
-endif.

%%====================================================================
%% Macros
%%====================================================================

-define(DURATION_UNITS, [{"microseconds", microseconds},
                         {"milliseconds", milliseconds},
                         {"seconds", seconds},
                         {"minutes", minutes},
                         {"hours", hours},
                         {"days", days}]).

%%====================================================================
%% Public API
%%====================================================================

%% @private
duration_unit_from_string(Str) ->
  duration_unit_from_string(Str, ?DURATION_UNITS).

%% @private
validate_duration_unit(false) ->
  false;
validate_duration_unit(undefined) ->
  undefined;
validate_duration_unit(SDU) ->
  case lists:any(fun({_, DU}) ->
                     DU == SDU
                 end,
                 ?DURATION_UNITS) of
    true ->
      SDU;
    _ ->
      erlang:error({invalid_value, SDU, "unknown duration unit"})
  end.

%% @private
maybe_convert_to_native(_, infinity) ->
  infinity;
maybe_convert_to_native(DU, Value) ->
  case DU of
    undefined -> Value;
    _ -> to_native(Value, DU)
  end.

%% @private
maybe_convert_to_du(_, infinity) ->
  infinity;
maybe_convert_to_du(DU, Value) ->
  case DU of
    undefined -> Value;
    _ -> from_native(Value, DU)
  end.

%%====================================================================
%% Private Parts
%%====================================================================

duration_unit_from_string(Str, [{SDU, DU}|Rest]) ->
  case string:rstr(Str, SDU) of
    0 -> duration_unit_from_string(Str, Rest);
    _ -> DU
  end;
duration_unit_from_string(_, []) ->
  undefined.

%% @private
from_native(Value) ->
  erlang:convert_time_unit(trunc(Value), native, nano_seconds).

%% @private
from_native(Value, microseconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000;
from_native(Value, milliseconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000000;
from_native(Value, seconds) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 1000000000;
from_native(Value, minutes) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 60000000000;
from_native(Value, hours) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 3600000000000;
from_native(Value, days) ->
  Nanoseconds = from_native(Value),
  Nanoseconds / 86400000000000.

%% @private
to_native(Value) ->
  erlang:convert_time_unit(trunc(Value), nano_seconds, native).

%% @private
to_native(Value, microseconds) ->
  to_native(Value * 1000);
to_native(Value, milliseconds) ->
  to_native(Value * 1000000);
to_native(Value, seconds) ->
  to_native(Value * 1000000000);
to_native(Value, minutes) ->
  to_native(Value * 60000000000);
to_native(Value, hours) ->
  to_native(Value * 3600000000000);
to_native(Value, days) ->
  to_native(Value * 86400000000000).
