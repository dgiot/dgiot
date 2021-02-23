-module(prometheus_buckets_tests).

-include_lib("eunit/include/eunit.hrl").

linear_errors_test() ->
  ?assertError({invalid_value, 0, "Buckets count should be positive"},
               prometheus_buckets:linear(-15, 5, 0)).

linear_test() ->
  ?assertEqual([-15, -10, -5, 0, 5, 10], prometheus_buckets:linear(-15, 5, 6)),
  ?assertEqual([1, 5.5, 10, 14.5, 19, 23.5], prometheus_buckets:linear(1, 4.5, 6)).

exponential_errors_test() ->
  ?assertError({invalid_value, 0, "Buckets count should be positive"},
               prometheus_buckets:exponential(-15, 5, 0)),
  ?assertError({invalid_value, -15, "Buckets start should be positive"},
               prometheus_buckets:exponential(-15, 5, 2)),
  ?assertError({invalid_value, 0.5, "Buckets factor should be greater than 1"},
               prometheus_buckets:exponential(15, 0.5, 3)).

exponential_test() ->
  ?assertEqual([100, 120, 144], prometheus_buckets:exponential(100, 1.2, 3)),
  ?assertEqual([1.5, 2.25, 3.375], prometheus_buckets:exponential(1.5, 1.5, 3)).

default_test() ->
  ?assertEqual([0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10],
               prometheus_buckets:default()).
