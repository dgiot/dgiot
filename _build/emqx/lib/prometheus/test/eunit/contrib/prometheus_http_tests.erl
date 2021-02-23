-module(prometheus_http_tests).

-include_lib("eunit/include/eunit.hrl").

microseconds_duration_buckets_test() ->
  ?assertMatch([10, 25, 50, 100, 250, 500,
                1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
                1000000, 2500000, 5000000, 10000000],
               prometheus_http:microseconds_duration_buckets()).

status_class_test() ->
  ?assertError({invalid_value, "qwe", "status code must be a positive integer"},
               prometheus_http:status_class("qwe")),
  ?assertError({invalid_value, 1.2, "status code must be a positive integer"},
               prometheus_http:status_class(1.2)),
  ?assertError({invalid_value, -10, "status code must be a positive integer"},
               prometheus_http:status_class(-10)),
  ?assertMatch("unknown", prometheus_http:status_class(65)),
  ?assertMatch("informational", prometheus_http:status_class(155)),
  ?assertMatch("success", prometheus_http:status_class(255)),
  ?assertMatch("redirection", prometheus_http:status_class(355)),
  ?assertMatch("client-error", prometheus_http:status_class(455)),
  ?assertMatch("server-error", prometheus_http:status_class(565)),
  ?assertMatch("unknown", prometheus_http:status_class(655)).
