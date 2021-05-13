

# Module prometheus_time #
* [Description](#description)

Measuring time intervals with Prometheus.erl.

<a name="description"></a>

## Description ##

Measuring time intervals is trivial - you just have to be sure you are using
monotonic time source. Basically interval is a difference between
start time and end time.
Erlang has standard `erlang:monotonic_time` function that returns
so called native time units. Native time units are meaningless
and have to be converted to seconds (or other units)
using `erlang:convert_time_unit`.
However as `erlang:convert_time_unit` documentation
[warns](http://erlang.org/doc/man/erlang.html#convert_time_unit-3):

```
  You may lose accuracy and precision when converting between  time units.
  In order to minimize such loss, collect all data at native time unit and
  do the conversion on the end result.
```

and because Prometheus mandates support for floats,
`set_duration/observe_duration` functions always work with
native time units and conversion is delayed until scraping/retrieving value.
To implement this, metric needs to know desired time unit.
Users can specify time unit explicitly via `duration_unit`
or implicitly via metric name (preferred, since prometheus best practices
guide insists on `<name>_duration_<unit>` metric name format).

Possible units:
- microseconds;
- milliseconds;
- seconds;
- minutes;
- hours;
- days;

Histogram also converts buckets bounds to native units if
duration_unit is provided. It converts it back when scraping or
retrieving value.

If values already converted to a 'real' unit, conversion can be disabled
by setting `duration_unit` to `false`.

## Examples

Example where duration unit derived from name:

```erlang

  prometheus_histogram:new([{name, fun_duration_seconds},
                            {buckets, [0.5, 1.1]}, %% in seconds
                            {help, ""}]),
  prometheus_histogram:observe_duration(fun_duration_seconds,
                                        fun () ->
                                            timer:sleep(1000)
                                        end),
  prometheus_histogram:value(fun_duration_seconds).
  {[0,1,0],1.001030886}
```

Example where duration unit set explicitly:

```erlang

  prometheus_histogram:new([{name, fun_duration_histogram},
                            {buckets, [500, 1100]}, %% in milliseconds
                            {help, ""},
                            {duration_unit, milliseconds}]),
  prometheus_histogram:observe_duration(fun_duration_histogram,
                                        fun () ->
                                            timer:sleep(1000)
                                        end),
  prometheus_histogram:value(fun_duration_histogram).
  {[0,1,0],1001.885302}
```

Example where value is in seconds already:

```erlang

  prometheus_histogram:new([{name, duration_seconds},
                            {buckets, [0.5, 1.1]}, %% in seconds
                            {help, ""},
                            {duration_unit, false}]),
  prometheus_histogram:dobserve(duration_seconds, 1.2),
  prometheus_histogram:value(duration_seconds).
  {[0,0,1],1.2}
```
