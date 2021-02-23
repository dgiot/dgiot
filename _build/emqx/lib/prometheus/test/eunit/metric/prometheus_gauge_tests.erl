-module(prometheus_gauge_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_set/1,
    fun test_inc/1,
    fun test_dinc/1,
    fun test_dec/1,
    fun test_ddec/1,
    fun test_set_to_current_time/1,
    fun test_track_inprogress/1,
    fun test_set_duration_seconds/1,
    fun test_set_duration_milliseconds/1,
    fun test_remove/1,
    fun test_undefined_value/1]}.

test_registration(_)->
  Name = pool_size,
  SpecWithRegistry = [{name, Name},
                      {help, ""},
                      {registry, qwe}],
  SpecWithoutRegistry = [{name, Name},
                         {help, ""}],
  [?_assertEqual(true,
                 prometheus_gauge:declare(SpecWithRegistry)),
   ?_assertEqual(false,
                 prometheus_gauge:declare(SpecWithoutRegistry, qwe)),
   ?_assertError({mf_already_exists, {qwe, Name}, "Consider using declare instead."},
                 prometheus_gauge:new(SpecWithoutRegistry, qwe))].

test_errors(_) ->
  prometheus_gauge:new([{name, with_label}, {labels, [label]}, {help, ""}]),

  [%% basic name/labels/help validations test
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"},
                 prometheus_gauge:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"},
                 prometheus_gauge:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"},
                 prometheus_gauge:new([{name, "qwe"}, {help, 12}])),

   %% gauge specific errors,
   ?_assertError({invalid_value, "qwe", "set accepts only numbers"},
                 prometheus_gauge:set(pool_size, "qwe")),
   ?_assertError({invalid_value, 1.5, "inc accepts only integers"},
                 prometheus_gauge:inc(pool_size, 1.5)),
   ?_assertError({invalid_value, "qwe", "dinc accepts only numbers"},
                 prometheus_gauge:dinc(pool_size, [], "qwe")),
   ?_assertError({invalid_value, 1.5, "dec accepts only integers"},
                 prometheus_gauge:dec(pool_size, 1.5)),
   ?_assertError({invalid_value, "qwe", "dec accepts only integers"},
                 prometheus_gauge:dec(pool_size, [], "qwe")),
   ?_assertError({invalid_value, 1.5, "dec accepts only integers"},
                 prometheus_gauge:dec(default, pool_size, [], 1.5)),
   ?_assertError({invalid_value, qwe, "ddec accepts only numbers"},
                 prometheus_gauge:ddec(pool_size, qwe)),
   ?_assertError({invalid_value, "qwe", "ddec accepts only numbers"},
                 prometheus_gauge:ddec(pool_size, [], "qwe")),
   ?_assertError({invalid_value, "qwe", "ddec accepts only numbers"},
                 prometheus_gauge:ddec(default, pool_size, [], "qwe")),
   ?_assertError({invalid_value, "qwe", "track_inprogress accepts only functions"},
                 prometheus_gauge:track_inprogress(pool_size, "qwe")),
   ?_assertError({invalid_value, "qwe", "set_duration accepts only functions"},
                 prometheus_gauge:set_duration(pool_size, "qwe")),

   %% mf/arity errors
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:set(unknown_metric, 2)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:set(with_label, [repo, db], 2)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:set_to_current_time(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:set_to_current_time(with_label, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:track_inprogress(unknown_metric,
                                                   fun() -> 1 end)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:track_inprogress(with_label, [repo, db],
                                                   fun() -> 1 end)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:set_duration(unknown_metric,
                                               fun() -> 1 end)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:set_duration(with_label, [repo, db],
                                               fun() -> 1 end)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:reset(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:reset(with_label, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:value(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:value(with_label, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_gauge:remove(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_gauge:remove(with_label, [repo, db]))
  ].

test_set(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:set(pool_size, [mongodb], 100),
  Value = prometheus_gauge:value(pool_size, [mongodb]),
  prometheus_gauge:set(pool_size, [mongodb], 105),
  Value1 = prometheus_gauge:value(pool_size, [mongodb]),
  prometheus_gauge:reset(pool_size, [mongodb]),
  RValue = prometheus_gauge:value(pool_size, [mongodb]),
  [?_assertEqual(100, Value),
   ?_assertEqual(105, Value1),
   ?_assertEqual(0, RValue)].

test_inc(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:new([{name, temperature}, {help, ""}]),
  prometheus_gauge:inc(pool_size, [mongodb]),
  prometheus_gauge:inc(pool_size, [mongodb], 3),
  prometheus_gauge:inc(temperature),
  prometheus_gauge:inc(temperature, 3),

  PSValue = prometheus_gauge:value(pool_size, [mongodb]),
  TValue = prometheus_gauge:value(temperature),
  [?_assertEqual(4, PSValue),
   ?_assertEqual(4, TValue)].

test_dinc(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:new([{name, temperature}, {help, ""}]),
  prometheus_gauge:dinc(pool_size, [mongodb]),
  prometheus_gauge:dinc(pool_size, [mongodb], 3.5),
  prometheus_gauge:dinc(temperature),
  prometheus_gauge:dinc(temperature, 3.5),

  %% dinc is async so lets make sure gen_server processed our increment request
  timer:sleep(10),

  PSValue = prometheus_gauge:value(pool_size, [mongodb]),
  TValue = prometheus_gauge:value(temperature),
  [?_assertEqual(4.5, PSValue),
   ?_assertEqual(4.5, TValue)].

call_cast_test() ->
  prometheus_gauge:declare([{name, cast}, {help, ""}]),
  prometheus_gauge:declare([{name, call}, {help, ""}, {call_timeout, 1000}]),
  prometheus_gauge:dinc(cast),
  prometheus_gauge:dinc(call),

  ?assertEqual(1, prometheus_gauge:value(cast)),
  ?assertEqual(1, prometheus_gauge:value(call)),

  try
    sys:suspend(prometheus_gauge),

    prometheus_gauge:dinc(cast),
    ?assertException(exit, {timeout, _}, prometheus_gauge:dinc(call)),

    ?assertEqual(1, prometheus_gauge:value(cast)),
    ?assertEqual(1, prometheus_gauge:value(call))

  after
    sys:resume(prometheus_gauge)
  end,

  %% wait for genserver
  timer:sleep(10),

  ?assertEqual(2, prometheus_gauge:value(cast)),
  ?assertEqual(2, prometheus_gauge:value(call)).

test_dec(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:new([{name, temperature}, {help, ""}]),
  prometheus_gauge:inc(pool_size, [mongodb]),
  prometheus_gauge:inc(pool_size, [mongodb], 10),
  prometheus_gauge:dec(pool_size, [mongodb]),
  prometheus_gauge:dec(pool_size, [mongodb], 6),
  prometheus_gauge:inc(temperature),
  prometheus_gauge:inc(temperature, 10),
  prometheus_gauge:dec(temperature),
  prometheus_gauge:dec(temperature, 6),

  PSValue = prometheus_gauge:value(pool_size, [mongodb]),
  TValue = prometheus_gauge:value(temperature),
  [?_assertEqual(4, PSValue),
   ?_assertEqual(4, TValue)].

test_ddec(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  prometheus_gauge:new([{name, temperature}, {help, ""}]),
  prometheus_gauge:ddec(pool_size, [mongodb]),
  prometheus_gauge:ddec(pool_size, [mongodb], 6.5),
  prometheus_gauge:ddec(temperature),
  prometheus_gauge:ddec(temperature, 6.5),
  prometheus_gauge:ddec(default, temperature, [], 6.5),

  %% ddec is async so lets make sure gen_server processed our increment request
  timer:sleep(10),

  PSValue = prometheus_gauge:value(pool_size, [mongodb]),
  TValue = prometheus_gauge:value(temperature),
  [?_assertEqual(-7.5, PSValue),
   ?_assertEqual(-14.0, TValue)].

test_set_to_current_time(_) ->
  prometheus_gauge:new([{name, cur_time}, {labels, []}, {help, ""}]),
  Timestamp = os:system_time(seconds),
  prometheus_gauge:set_to_current_time(cur_time),
  STimestamp = prometheus_gauge:value(cur_time),
  [?_assertEqual(Timestamp, STimestamp)].

test_track_inprogress(_) ->
  prometheus_gauge:new([{name, gauge}, {help, ""}]),
  Value = prometheus_gauge:track_inprogress(gauge,
                                            fun () ->
                                                prometheus_gauge:value(gauge)
                                            end),

  try prometheus_gauge:track_inprogress(gauge, fun () ->
                                                   erlang:error({qwe})
                                               end)
  catch _:_ -> ok
  end,

  [?_assertEqual(1, Value),
   ?_assertEqual(0, prometheus_gauge:value(gauge))].

test_set_duration_seconds(_) ->
  prometheus_gauge:new([{name, gauge_seconds},
                        {help, ""}]),
  ValueF = prometheus_gauge:set_duration(gauge_seconds, fun () ->
                                                            timer:sleep(1000),
                                                            1
                                                        end),
  Value = prometheus_gauge:value(gauge_seconds),

  [MF] = prometheus_collector:collect_mf_to_list(prometheus_gauge),

  #'MetricFamily'{metric=
                    [#'Metric'{gauge=
                                 #'Gauge'{value=MFValue}}]} = MF,

  try prometheus_gauge:set_duration(gauge_seconds, fun () ->
                                                       erlang:error({qwe})
                                                   end)
  catch _:_ -> ok
  end,

  ValueE = prometheus_gauge:value(gauge_seconds),

  [?_assertMatch(1, ValueF),
   ?_assertMatch(true, 0.9 < Value andalso Value < 1.2),
   ?_assertMatch(true, 0.9 < MFValue andalso MFValue < 1.2),
   ?_assertMatch(true, 0.0 < ValueE andalso ValueE < 0.1)].

test_set_duration_milliseconds(_) ->
  prometheus_gauge:new([{name, gauge},
                        {help, ""},
                        {duration_unit, milliseconds}]),
  ValueF = prometheus_gauge:set_duration(gauge, fun () ->
                                                    timer:sleep(1100),
                                                    1
                                                end),
  Value = prometheus_gauge:value(gauge),

  try prometheus_gauge:set_duration(gauge, fun () ->
                                               erlang:error({qwe})
                                           end)
  catch _:_ -> ok
  end,

  timer:sleep(10),
  ValueE = prometheus_gauge:value(gauge),

  [?_assertMatch(1, ValueF),
   ?_assertMatch(true, 900 < Value andalso Value < 1200),
   ?_assertMatch(true, 0 < ValueE andalso ValueE < 100)].

test_remove(_) ->
  prometheus_gauge:new([{name, pool_size},
                        {labels, [pool]},
                        {help, "Http request count"}]),
  prometheus_gauge:new([{name, simple_gauge}, {help, ""}]),

  prometheus_gauge:inc(pool_size, [mongodb]),
  prometheus_gauge:inc(simple_gauge),

  BRValue1 = prometheus_gauge:value(pool_size, [mongodb]),
  BRValue2 = prometheus_gauge:value(simple_gauge),

  RResult1 = prometheus_gauge:remove(pool_size, [mongodb]),
  RResult2 = prometheus_gauge:remove(simple_gauge),

  ARValue1 = prometheus_gauge:value(pool_size, [mongodb]),
  ARValue2 = prometheus_gauge:value(simple_gauge),

  RResult3 = prometheus_gauge:remove(pool_size, [mongodb]),
  RResult4 = prometheus_gauge:remove(simple_gauge),

  [?_assertEqual(1, BRValue1),
   ?_assertEqual(1, BRValue2),
   ?_assertEqual(true, RResult1),
   ?_assertEqual(true, RResult2),
   ?_assertEqual(undefined, ARValue1),
   ?_assertEqual(undefined, ARValue2),
   ?_assertEqual(false, RResult3),
   ?_assertEqual(false, RResult4)].

test_undefined_value(_) ->
  prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
  UndefinedValue = prometheus_gauge:value(pool_size, [post]),
  [?_assertEqual(undefined, UndefinedValue)].
