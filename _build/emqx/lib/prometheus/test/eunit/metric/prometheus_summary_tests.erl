-module(prometheus_summary_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_observe/1,
    fun test_dobserve/1,
    fun test_observe_duration_seconds/1,
    fun test_observe_duration_milliseconds/1,
    fun test_remove/1,
    fun test_undefined_value/1]}.

test_registration(_)->
  Name = orders_summary,
  SpecWithRegistry = [{name, Name},
                      {help, ""},
                      {registry, qwe}],
  SpecWithoutRegistry = [{name, Name},
                         {help, ""}],
  [?_assertEqual(true,
                 prometheus_summary:declare(SpecWithRegistry)),
   ?_assertEqual(false,
                 prometheus_summary:declare(SpecWithoutRegistry, qwe)),
   ?_assertError({mf_already_exists, {qwe, Name}, "Consider using declare instead."},
                 prometheus_summary:new(SpecWithoutRegistry, qwe))].

test_errors(_) ->
  prometheus_summary:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),
  [%% basic name/labels/help validations test
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"},
                 prometheus_summary:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"},
                 prometheus_summary:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_label_name, "quantile",
                  "summary cannot have a label named \"quantile\""},
                 prometheus_summary:new([{name, "qwe"},
                                         {labels, ["qua", "quantile"]},
                                         {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"},
                 prometheus_summary:new([{name, "qwe"}, {help, 12}])),
   %% mf/arity errors
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:observe(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:observe(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:dobserve(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:dobserve(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:observe_duration(unknown_metric,
                                                     fun() -> 1 end)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:observe_duration(db_query_duration,
                                                     [repo, db],
                                                     fun() -> 1 end)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:reset(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:reset(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:value(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:value(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_summary:remove(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_summary:remove(db_query_duration, [repo, db])),
   %% summary specific errors
   ?_assertError({invalid_value, 1.5, "observe accepts only integers"},
                 prometheus_summary:observe(orders_summary, 1.5)),
   ?_assertError({invalid_value, "qwe", "observe accepts only integers"},
                 prometheus_summary:observe(orders_summary, "qwe")),
   ?_assertError({invalid_value, "qwe", "dobserve accepts only numbers"},
                 prometheus_summary:dobserve(orders_summary, "qwe")),
   ?_assertError({invalid_value, "qwe", "observe_duration accepts only functions"},
                 prometheus_summary:observe_duration(pool_size, "qwe"))
  ].

test_observe(_) ->
  prometheus_summary:new([{name, orders_summary},
                          {labels, [department]},
                          {help, "Track orders count/total sum"}]),
  prometheus_summary:observe(orders_summary, [electronics], 10),
  prometheus_summary:observe(orders_summary, [electronics], 15),
  Value = prometheus_summary:value(orders_summary, [electronics]),
  prometheus_summary:reset(orders_summary, [electronics]),
  RValue = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual({2, 25}, Value),
   ?_assertEqual({0, 0}, RValue)].


test_dobserve(_) ->
  prometheus_summary:new([{name, orders_summary},
                          {labels, [department]},
                          {help, "Track orders count/total sum"}]),
  prometheus_summary:dobserve(orders_summary, [electronics], 1.5),
  prometheus_summary:dobserve(orders_summary, [electronics], 2.7),

  %% dobserve is async so lets make sure gen_server processed our increment request
  timer:sleep(10),

  Value = prometheus_summary:value(orders_summary, [electronics]),
  prometheus_summary:reset(orders_summary, [electronics]),
  RValue = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual({2, 4.2}, Value),
   ?_assertEqual({0, 0}, RValue)].

call_cast_test() ->
  prometheus_summary:declare([{name, cast}, {help, ""}]),
  prometheus_summary:declare([{name, call}, {help, ""}, {call_timeout, 1000}]),
  prometheus_summary:dobserve(cast, 1),
  prometheus_summary:dobserve(call, 1),

  ?assertEqual({1, 1}, prometheus_summary:value(cast)),
  ?assertEqual({1, 1}, prometheus_summary:value(call)),

  try
    sys:suspend(prometheus_summary),

    prometheus_summary:dobserve(cast, 1),
    ?assertException(exit, {timeout, _}, prometheus_summary:dobserve(call, 1)),

    ?assertEqual({1, 1}, prometheus_summary:value(cast)),
    ?assertEqual({1, 1}, prometheus_summary:value(call))

  after
    sys:resume(prometheus_summary)
  end,

  %% wait for genserver
  timer:sleep(10),

  ?assertEqual({2, 2}, prometheus_summary:value(cast)),
  ?assertEqual({2, 2}, prometheus_summary:value(call)).

test_observe_duration_seconds(_) ->
  prometheus_summary:new([{name, <<"fun_duration_seconds">>},
                          {help, ""},
                          {duration_unit, seconds}]),
  prometheus_summary:observe_duration(<<"fun_duration_seconds">>, fun () ->
                                                                      timer:sleep(1000)
                                                                  end),

  {Count, Sum} = prometheus_summary:value(<<"fun_duration_seconds">>),

  [MF] = prometheus_collector:collect_mf_to_list(prometheus_summary),

  #'MetricFamily'{metric=
                    [#'Metric'{summary=
                                 #'Summary'{sample_sum=MFSum,
                                            sample_count=MFCount}}]} = MF,

  try prometheus_summary:observe_duration(<<"fun_duration_seconds">>,
                                          fun () ->
                                              erlang:error({qwe})
                                          end)
  catch _:_ -> ok
  end,

  {CountE, SumE} = prometheus_summary:value(<<"fun_duration_seconds">>),

  [?_assertEqual(1, Count),
   ?_assertEqual(1, MFCount),
   ?_assertEqual(2, CountE),
   ?_assertMatch(true, 0.9 < Sum andalso Sum < 1.2),
   ?_assertMatch(true, 0.9 < MFSum andalso MFSum < 1.2),
   ?_assertMatch(true, 0.9 < SumE andalso SumE < 1.2)].

test_observe_duration_milliseconds(_) ->
  prometheus_summary:new([{name, fun_duration},
                          {help, ""},
                          {duration_unit, milliseconds}]),
  prometheus_summary:observe_duration(fun_duration, fun () ->
                                                        timer:sleep(1100)
                                                    end),

  {Count, Sum} = prometheus_summary:value(fun_duration),

  try prometheus_summary:observe_duration(fun_duration, fun () ->
                                                            erlang:error({qwe})
                                                        end)
  catch _:_ -> ok
  end,

  {CountE, SumE} = prometheus_summary:value(fun_duration),

  [?_assertEqual(1, Count),
   ?_assertEqual(2, CountE),
   ?_assertMatch(true, 900 < Sum andalso Sum < 1200),
   ?_assertMatch(true, 900 < SumE andalso SumE < 1200)].

test_remove(_) ->
  prometheus_summary:new([{name, summary}, {labels, [pool]}, {help, ""}]),
  prometheus_summary:new([{name, simple_summary}, {help, ""}]),

  prometheus_summary:observe(summary, [mongodb], 1),
  prometheus_summary:observe(simple_summary, 1),

  BRValue1 = prometheus_summary:value(summary, [mongodb]),
  BRValue2 = prometheus_summary:value(simple_summary),

  RResult1 = prometheus_summary:remove(summary, [mongodb]),
  RResult2 = prometheus_summary:remove(simple_summary),

  ARValue1 = prometheus_summary:value(summary, [mongodb]),
  ARValue2 = prometheus_summary:value(simple_summary),

  RResult3 = prometheus_summary:remove(summary, [mongodb]),
  RResult4 = prometheus_summary:remove(simple_summary),

  [?_assertEqual({1, 1}, BRValue1),
   ?_assertEqual({1, 1}, BRValue2),
   ?_assertEqual(true, RResult1),
   ?_assertEqual(true, RResult2),
   ?_assertEqual(undefined, ARValue1),
   ?_assertEqual(undefined, ARValue2),
   ?_assertEqual(false, RResult3),
   ?_assertEqual(false, RResult4)].

test_undefined_value(_) ->
  prometheus_summary:new([{name, orders_summary},
                          {labels, [department]},
                          {help, "Track orders count/total sum"}]),
  Value = prometheus_summary:value(orders_summary, [electronics]),
  [?_assertEqual(undefined, Value)].
