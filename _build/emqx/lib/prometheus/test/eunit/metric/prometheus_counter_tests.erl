-module(prometheus_counter_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_inc/1,
    fun test_dinc/1,
    fun test_remove/1,
    fun test_undefined_value/1]}.

test_registration(_)->
  Name = http_requests_total,
  SpecWithRegistry = [{name, Name},
                      {help, ""},
                      {registry, qwe}],
  SpecWithoutRegistry = [{name, Name},
                         {help, ""}],
  [?_assertEqual(true,
                 prometheus_counter:declare(SpecWithRegistry)),
   ?_assertEqual(false,
                 prometheus_counter:declare(SpecWithoutRegistry, qwe)),
   ?_assertError({mf_already_exists, {qwe, Name}, "Consider using declare instead."},
                 prometheus_counter:new(SpecWithoutRegistry, qwe))].

test_errors(_) ->
  prometheus_counter:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),

  [%% basic name/labels/help validations test
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"},
                 prometheus_counter:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"},
                 prometheus_counter:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"},
                 prometheus_counter:new([{name, "qwe"}, {help, 12}])),

   %% counter specific errors
   ?_assertError({invalid_value, -1, "inc accepts only non-negative integers"},
                 prometheus_counter:inc(http_requests_total, -1)),
   ?_assertError({invalid_value, 1.5, "inc accepts only non-negative integers"},
                 prometheus_counter:inc(http_requests_total, 1.5)),
   ?_assertError({invalid_value, "qwe", "inc accepts only non-negative integers"},
                 prometheus_counter:inc(http_requests_total, [], "qwe")),
   ?_assertError({invalid_value, -1, "dinc accepts only non-negative numbers"},
                 prometheus_counter:dinc(http_requests_total, -1)),
   ?_assertError({invalid_value, "qwe", "dinc accepts only non-negative numbers"},
                 prometheus_counter:dinc(http_requests_total, [], "qwe")),

   %% mf/arity errors
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_counter:inc(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_counter:inc(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_counter:dinc(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_counter:dinc(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_counter:reset(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_counter:reset(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_counter:value(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_counter:value(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_counter:remove(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_counter:remove(db_query_duration, [repo, db]))
  ].

test_inc(_) ->
  prometheus_counter:new([{name, http_requests_total},
                          {labels, [method]},
                          {help, "Http request count"}]),
  prometheus_counter:inc(http_requests_total, [get]),
  prometheus_counter:inc(http_requests_total, [get], 3),
  Value = prometheus_counter:value(http_requests_total, [get]),
  prometheus_counter:reset(http_requests_total, [get]),
  RValue = prometheus_counter:value(http_requests_total, [get]),
  [?_assertEqual(4, Value),
   ?_assertEqual(0, RValue)].

test_dinc(_) ->
  prometheus_counter:new([{name, http_requests_total},
                          {help, "Http request count"}]),
  prometheus_counter:dinc(http_requests_total),
  prometheus_counter:dinc(http_requests_total, 3.5),

  %% dinc is async so lets make sure gen_server processed our increment request
  timer:sleep(10),

  Value = prometheus_counter:value(http_requests_total),
  prometheus_counter:reset(http_requests_total),
  RValue = prometheus_counter:value(http_requests_total),
  [?_assertEqual(4.5, Value),
   ?_assertEqual(0, RValue)].

call_cast_test() ->
  prometheus_counter:declare([{name, cast}, {help, ""}]),
  prometheus_counter:declare([{name, call}, {help, ""}, {call_timeout, 1000}]),
  prometheus_counter:dinc(cast),
  prometheus_counter:dinc(call),

  ?assertEqual(1, prometheus_counter:value(cast)),
  ?assertEqual(1, prometheus_counter:value(call)),

  try
    sys:suspend(prometheus_counter),

    prometheus_counter:dinc(cast),
    ?assertException(exit, {timeout, _}, prometheus_counter:dinc(call)),

    ?assertEqual(1, prometheus_counter:value(cast)),
    ?assertEqual(1, prometheus_counter:value(call))

  after
    sys:resume(prometheus_counter)
  end,

  %% wait for genserver
  timer:sleep(10),

  ?assertEqual(2, prometheus_counter:value(cast)),
  ?assertEqual(2, prometheus_counter:value(call)).

test_remove(_) ->
  prometheus_counter:new([{name, http_requests_total},
                          {labels, [method]},
                          {help, "Http request count"}]),
  prometheus_counter:new([{name, simple_counter}, {help, ""}]),

  prometheus_counter:inc(http_requests_total, [get]),
  prometheus_counter:inc(simple_counter),

  BRValue1 = prometheus_counter:value(http_requests_total, [get]),
  BRValue2 = prometheus_counter:value(simple_counter),

  RResult1 = prometheus_counter:remove(http_requests_total, [get]),
  RResult2 = prometheus_counter:remove(simple_counter),

  ARValue1 = prometheus_counter:value(http_requests_total, [get]),
  ARValue2 = prometheus_counter:value(simple_counter),

  RResult3 = prometheus_counter:remove(http_requests_total, [get]),
  RResult4 = prometheus_counter:remove(simple_counter),

  [?_assertEqual(1, BRValue1),
   ?_assertEqual(1, BRValue2),
   ?_assertEqual(true, RResult1),
   ?_assertEqual(true, RResult2),
   ?_assertEqual(undefined, ARValue1),
   ?_assertEqual(undefined, ARValue2),
   ?_assertEqual(false, RResult3),
   ?_assertEqual(false, RResult4)].

test_undefined_value(_) ->
  prometheus_counter:new([{name, http_requests_total},
                          {labels, [method]},
                          {help, "Http request count"}]),
  UndefinedValue = prometheus_counter:value(http_requests_total, [post]),
  [?_assertEqual(undefined, UndefinedValue)].
