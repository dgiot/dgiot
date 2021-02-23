-module(prometheus_histogram_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_registration/1,
    fun test_errors/1,
    fun test_buckets/1,
    fun test_observe/1,
    fun test_dobserve/1,
    fun test_observe_duration_seconds/1,
    fun test_observe_duration_milliseconds/1,
    fun test_remove/1,
    fun test_undefined_value/1]}.

test_registration(_)->
  Name = request_duration,
  SpecWithRegistry = [{name, Name},
                      {buckets, [100, 300, 500, 750, 1000]},
                      {help, ""},
                      {registry, qwe}],
  SpecWithoutRegistry = [{name, Name},
                         {buckets, [100, 300, 500, 750, 1000]},
                         {help, ""}],
  [?_assertEqual(true,
                 prometheus_histogram:declare(SpecWithRegistry)),
   ?_assertEqual(false,
                 prometheus_histogram:declare(SpecWithoutRegistry, qwe)),
   ?_assertError({mf_already_exists, {qwe, Name}, "Consider using declare instead."},
                 prometheus_histogram:new(SpecWithoutRegistry, qwe))].

test_errors(_) ->
  prometheus_histogram:new([{name, request_duration},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Track requests duration"}]),
  prometheus_histogram:new([{name, db_query_duration},
                            {labels, [repo]},
                            {help, ""}]),

  [%% basic name/labels/help validations test
   ?_assertError({invalid_metric_name, 12, "metric name is not a string"},
                 prometheus_histogram:new([{name, 12}, {help, ""}])),
   ?_assertError({invalid_metric_labels, 12, "not list"},
                 prometheus_histogram:new([{name, "qwe"}, {labels, 12}, {help, ""}])),
   ?_assertError({invalid_metric_label_name, "le",
                  "histogram cannot have a label named \"le\""},
                 prometheus_histogram:new([{name, "qwe"},
                                           {labels, ["qwe", "le"]}, {help, ""}])),
   ?_assertError({invalid_metric_help, 12, "metric help is not a string"},
                 prometheus_histogram:new([{name, "qwe"}, {help, 12}])),

   %% mf/arity errors
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:observe(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:observe(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:dobserve(unknown_metric, 1)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:dobserve(db_query_duration, [repo, db], 1)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:observe_duration(unknown_metric, fun() -> 1 end)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:observe_duration(db_query_duration,
                                                       [repo, db],
                                                       fun() -> 1 end)),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:reset(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:reset(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:value(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:value(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:buckets(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:buckets(db_query_duration, [repo, db])),
   ?_assertError({unknown_metric, default, unknown_metric},
                 prometheus_histogram:remove(unknown_metric)),
   ?_assertError({invalid_metric_arity, 2, 1},
                 prometheus_histogram:remove(db_query_duration, [repo, db])),

   %% histogram specific errors
   ?_assertError({histogram_no_buckets, []},
                 prometheus_histogram:new([{name, "qwe"}, {help, ""}, {buckets, []}])),
   ?_assertError({histogram_no_buckets, undefined},
                 prometheus_histogram:new([{name, "qwe"},
                                           {help, ""},
                                           {buckets, undefined}])),
   ?_assertError({histogram_invalid_buckets, 1, "not a list"},
                 prometheus_histogram:new([{name, "qwe"}, {help, ""}, {buckets, 1}])),
   ?_assertError({histogram_invalid_bound, "qwe"},
                 prometheus_histogram:new([{name, "qwe"},
                                           {help, ""},
                                           {buckets, ["qwe"]}])),
   ?_assertError({histogram_invalid_buckets, [1, 3, 2], "buckets not sorted"},
                 prometheus_histogram:new([{name, "qwe"},
                                           {help, ""},
                                           {buckets, [1, 3, 2]}])),
   ?_assertError({invalid_value, 1.5, "observe accepts only integers"},
                 prometheus_histogram:observe(request_duration, 1.5)),
   ?_assertError({invalid_value, "qwe", "observe accepts only integers"},
                 prometheus_histogram:observe(request_duration, "qwe")),
   ?_assertError({invalid_value, "qwe", "dobserve accepts only numbers"},
                 prometheus_histogram:dobserve(request_duration, "qwe")),
   ?_assertError({invalid_value, "qwe", "observe_duration accepts only functions"},
                 prometheus_histogram:observe_duration(pool_size, "qwe"))
  ].

test_buckets(_) ->
  prometheus_histogram:new([{name, "default_buckets"}, {help, ""}]),
  DefaultBuckets = prometheus_histogram:buckets("default_buckets"),
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),


  prometheus_histogram:new([{name, "explicit_default_buckets"},
                            {help, ""},
                            {buckets, default}]),
  ExplicitDefaultBuckets = prometheus_histogram:buckets("explicit_default_buckets"),

  prometheus_histogram:new([{name, "linear_buckets"},
                            {help, ""},
                            {buckets, {linear, -15, 5, 6}}]),
  LinearBuckets = prometheus_histogram:buckets("linear_buckets"),

  prometheus_histogram:declare([{name, "exp_buckets"},
                                {help, ""},
                                {buckets, {exponential, 100, 1.2, 3}}]),
  ExpBuckets = prometheus_histogram:buckets("exp_buckets"),

  CustomBuckets = prometheus_histogram:buckets(http_request_duration_milliseconds,
                                               [method]),
  [?_assertEqual(prometheus_histogram:default_buckets() ++ [infinity],
                 DefaultBuckets),
   ?_assertEqual(prometheus_histogram:default_buckets() ++ [infinity],
                 ExplicitDefaultBuckets),
   ?_assertEqual([100, 300, 500, 750, 1000, infinity], CustomBuckets),
   ?_assertEqual([-15, -10, -5, 0, 5, 10],
                 prometheus_histogram:linear_buckets(-15, 5, 6)),
   ?_assertEqual([100, 120, 144],
                 prometheus_histogram:exponential_buckets(100, 1.2, 3)),
   ?_assertEqual([-15, -10, -5, 0, 5, 10, infinity], LinearBuckets),
   ?_assertEqual([100, 120, 144, infinity], ExpBuckets)].

test_observe(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950),
  Value = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
  prometheus_histogram:reset(http_request_duration_milliseconds, [get]),
  RValue = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
  [?_assertEqual({[3, 3, 1, 1, 1, 0], 2622}, Value),
   ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)].

test_dobserve(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 500.2),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 150.4),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 450.5),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 850.3),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 750.9),
  prometheus_histogram:dobserve(http_request_duration_milliseconds, [post], 1650.23),

  %% dobserve is async so lets make sure gen_server processed our increment request
  timer:sleep(10),

  Value = prometheus_histogram:value(http_request_duration_milliseconds, [post]),
  prometheus_histogram:reset(http_request_duration_milliseconds, [post]),
  RValue = prometheus_histogram:value(http_request_duration_milliseconds, [post]),
  [?_assertEqual({[0, 1, 1, 1, 2, 1], 4352.53}, Value),
   ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)].

call_cast_test() ->
  prometheus_histogram:declare([{name, cast},
                                {help, ""},
                                {buckets, [2]}]),
  prometheus_histogram:declare([{name, call},
                                {help, ""},
                                {buckets, [2]},
                                {call_timeout, 1000}]),
  prometheus_histogram:dobserve(cast, 1),
  prometheus_histogram:dobserve(call, 1),

  ?assertEqual({[1, 0], 1}, prometheus_histogram:value(cast)),
  ?assertEqual({[1, 0], 1}, prometheus_histogram:value(call)),

  try
    sys:suspend(prometheus_histogram),

    prometheus_histogram:dobserve(cast, 1),
    ?assertException(exit, {timeout, _}, prometheus_histogram:dobserve(call, 1)),

    ?assertEqual({[1, 0], 1}, prometheus_histogram:value(cast)),
    ?assertEqual({[1, 0], 1}, prometheus_histogram:value(call))

  after
    sys:resume(prometheus_histogram)
  end,

  %% wait for genserver
  timer:sleep(10),

  ?assertEqual({[2, 0], 2}, prometheus_histogram:value(cast)),
  ?assertEqual({[2, 0], 2}, prometheus_histogram:value(call)).

test_observe_duration_seconds(_) ->
  prometheus_histogram:new([{name, fun_duration_seconds},
                            {buckets, [0.5, 1.1]},
                            {help, ""}]),
  prometheus_histogram:observe_duration(fun_duration_seconds, fun () ->
                                                                  timer:sleep(1000)
                                                              end),
  {Buckets, Sum} = prometheus_histogram:value(fun_duration_seconds),

  try prometheus_histogram:observe_duration(fun_duration_seconds,
                                            fun () ->
                                                erlang:error({qwe})
                                            end)
  catch _:_ -> ok
  end,

  {BucketsE, SumE} = prometheus_histogram:value(fun_duration_seconds),

  [MF] = prometheus_collector:collect_mf_to_list(prometheus_histogram),

  MBuckets = [#'Bucket'{cumulative_count=1,
                        upper_bound=0.5},
              #'Bucket'{cumulative_count=2,
                        upper_bound=1.1},
              #'Bucket'{cumulative_count=2,
                        upper_bound=infinity}],

  #'MetricFamily'{metric=
                    [#'Metric'{histogram=
                                 #'Histogram'{sample_sum=MFSum,
                                              sample_count=MFCount,
                                              bucket=MBuckets}}]} = MF,

  [?_assertEqual([0, 1, 0], Buckets),
   ?_assertEqual([1, 1, 0], BucketsE),
   ?_assertEqual(true, 0.9 < Sum andalso Sum < 1.2),
   ?_assertEqual(true, 0.9 < SumE andalso SumE < 1.2),
   ?_assertEqual(2, MFCount),
   ?_assertEqual(true, 0.9 < MFSum andalso MFSum < 1.2)].

test_observe_duration_milliseconds(_) ->
  prometheus_histogram:new([{name, fun_duration_histogram},
                            {buckets, [500, 1100]},
                            {help, ""},
                            {duration_unit, milliseconds}]),
  prometheus_histogram:observe_duration(fun_duration_histogram, fun () ->
                                                                    timer:sleep(1000)
                                                                end),
  {Buckets, Sum} = prometheus_histogram:value(fun_duration_histogram),

  try prometheus_histogram:observe_duration(fun_duration_histogram,
                                            fun () ->
                                                erlang:error({qwe})
                                            end)
  catch _:_ -> ok
  end,

  {BucketsE, SumE} = prometheus_histogram:value(fun_duration_histogram),

  [?_assertEqual([0, 1, 0], Buckets),
   ?_assertEqual([1, 1, 0], BucketsE),
   ?_assertMatch(true, 900 < Sum andalso Sum < 1200),
   ?_assertMatch(true, 900 < SumE andalso SumE < 1200)].

test_remove(_) ->
  prometheus_histogram:new([{name, histogram},
                            {buckets, [5, 10]},
                            {labels, [pool]},
                            {help, ""}]),
  prometheus_histogram:new([{name, simple_histogram},
                            {buckets, [5, 10]},
                            {help, ""}]),

  prometheus_histogram:observe(histogram, [mongodb], 1),
  prometheus_histogram:observe(simple_histogram, 1),
  prometheus_histogram:observe(histogram, [mongodb], 6),
  prometheus_histogram:observe(simple_histogram, 6),

  BRValue1 = prometheus_histogram:value(histogram, [mongodb]),
  BRValue2 = prometheus_histogram:value(simple_histogram),

  RResult1 = prometheus_histogram:remove(histogram, [mongodb]),
  RResult2 = prometheus_histogram:remove(simple_histogram),

  ARValue1 = prometheus_histogram:value(histogram, [mongodb]),
  ARValue2 = prometheus_histogram:value(simple_histogram),

  RResult3 = prometheus_histogram:remove(histogram, [mongodb]),
  RResult4 = prometheus_histogram:remove(simple_histogram),

  [?_assertEqual({[1, 1, 0], 7}, BRValue1),
   ?_assertEqual({[1, 1, 0], 7}, BRValue2),
   ?_assertEqual(true, RResult1),
   ?_assertEqual(true, RResult2),
   ?_assertEqual(undefined, ARValue1),
   ?_assertEqual(undefined, ARValue2),
   ?_assertEqual(false, RResult3),
   ?_assertEqual(false, RResult4)].

test_undefined_value(_) ->
  prometheus_histogram:new([{name, duraiton_histogram}, {labels, [label]}, {help, ""}]),
  Value = prometheus_histogram:value(duraiton_histogram, [label]),
  [?_assertEqual(undefined, Value)].
