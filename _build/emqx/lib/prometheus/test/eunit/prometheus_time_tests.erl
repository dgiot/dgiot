-module(prometheus_time_tests).

-include_lib("eunit/include/eunit.hrl").

duration_unit_from_string_test() ->
  ?assertEqual(microseconds,
               prometheus_time:duration_unit_from_string("duration_microseconds")),
  ?assertEqual(milliseconds,
               prometheus_time:duration_unit_from_string("duration_milliseconds")),
  ?assertEqual(seconds,
               prometheus_time:duration_unit_from_string("duration_seconds")),
  ?assertEqual(minutes,
               prometheus_time:duration_unit_from_string("duration_minutes")),
  ?assertEqual(hours,
               prometheus_time:duration_unit_from_string("duration_hours")),
  ?assertEqual(days,
               prometheus_time:duration_unit_from_string("duration_days")).

validate_duration_unit_test() ->
  ?assertEqual(microseconds, prometheus_time:validate_duration_unit(microseconds)),
  ?assertEqual(milliseconds, prometheus_time:validate_duration_unit(milliseconds)),
  ?assertEqual(seconds, prometheus_time:validate_duration_unit(seconds)),
  ?assertEqual(minutes, prometheus_time:validate_duration_unit(minutes)),
  ?assertEqual(hours, prometheus_time:validate_duration_unit(hours)),
  ?assertEqual(days, prometheus_time:validate_duration_unit(days)),
  ?assertError({invalid_value, invalid, "unknown duration unit"},
               prometheus_time:validate_duration_unit(invalid)).

from_native_test() ->
  NativeInUS = erlang:convert_time_unit(1, micro_seconds, native),
  ?assertEqual(2.5, prometheus_time:from_native(2.5 * NativeInUS, microseconds)),
  ?assertEqual(2.6, prometheus_time:from_native(2.6 * 1000 * NativeInUS, milliseconds)),
  ?assertEqual(2.23, prometheus_time:from_native(2.23 * 1000000 * NativeInUS, seconds)),
  ?assertEqual(3.4, prometheus_time:from_native(3.4 * 60 * 1000000 * NativeInUS,
                                                minutes)),
  ?assertEqual(0.4, prometheus_time:from_native(0.4 * 3600 * 1000000 * NativeInUS,
                                                hours)),
  ?assertEqual(0.1, prometheus_time:from_native(0.1 * 86400 * 1000000 * NativeInUS,
                                                days)).

to_native_test() ->
  NativeInUS = erlang:convert_time_unit(1, micro_seconds, native),
  ?assertEqual(trunc(2.43 * NativeInUS), prometheus_time:to_native(2.43, microseconds)),
  ?assertEqual(trunc(2.6 * 1000 * NativeInUS),
               prometheus_time:to_native(2.6, milliseconds)),
  ?assertEqual(trunc(2.23 * 1000000 * NativeInUS),
               prometheus_time:to_native(2.23, seconds)),
  ?assertEqual(trunc(3.4 * 60 * 1000000 * NativeInUS), prometheus_time:to_native(3.4,
                                                                          minutes)),
  ?assertEqual(trunc(0.4 * 3600 * 1000000 * NativeInUS), prometheus_time:to_native(0.4,
                                                                            hours)),
  ?assertEqual(trunc(0.1 * 86400 * 1000000 * NativeInUS), prometheus_time:to_native(0.1,
                                                                             days)).
