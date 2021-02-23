-module(prometheus_protobuf_format_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_gauge/1,
    fun test_counter/1,
    fun test_dcounter/1,
    fun test_summary/1,
    fun test_dsummary/1,
    fun test_histogram/1,
    fun test_dhistogram/1]}.

content_type_test() ->
  ?assertEqual(<<"application/vnd.google.protobuf; "
                 "proto=io.prometheus.client.MetricFamily; "
                 "encoding=delimited">>, prometheus_protobuf_format:content_type()).

test_gauge(_) ->
  prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
  prometheus_gauge:set(pool_size, 365),
  Expected = <<57, 10, 9, 112, 111, 111, 108, 95, 115, 105, 122, 101, 18, 29, 77, 111,
               110, 103, 111, 68, 66, 32, 67, 111, 110, 110, 101, 99, 116, 105, 111,
               110, 115, 32, 112, 111, 111, 108, 32, 115, 105, 122, 101, 24, 1, 34,
               11, 18, 9, 9, 0, 0, 0, 0, 0, 208, 118, 64>>,
  ?_assertEqual(Expected,
     prometheus_protobuf_format:format()).

test_counter(_) ->
  prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]),
  prometheus_counter:inc(http_requests_total),

  Expected = <<56, 10, 19, 104, 116, 116, 112, 95, 114, 101, 113, 117, 101, 115, 116,
               115, 95, 116, 111, 116, 97, 108, 18, 18, 72, 116, 116, 112, 32, 114,
               101, 113, 117, 101, 115, 116, 32, 99, 111, 117, 110, 116, 24, 0, 34,
               11, 26, 9, 9, 0, 0, 0, 0, 0, 0, 240, 63>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).

test_dcounter(_) ->
  prometheus_counter:new([{name, dtest}, {help, "qw\"\\e"}]),
  prometheus_counter:dinc(dtest, 1.5),
  prometheus_counter:dinc(dtest, 3.5),
  prometheus_counter:dinc(dtest, 1.5),

  %% dobserve is async so lets make sure gen_server processed our request
  timer:sleep(10),

  Expected = <<29, 10, 5, 100, 116, 101, 115, 116, 18, 5, 113, 119, 34, 92, 101, 24, 0,
               34, 11, 26, 9, 9, 0, 0, 0, 0, 0, 0, 26, 64>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).

test_summary(_) ->
  prometheus_summary:new([{name, orders_summary},
                          {help, "Track orders count/total sum"}]),
  prometheus_summary:observe(orders_summary,  10),
  prometheus_summary:observe(orders_summary,  15),

  Expected = <<63, 10, 14, 111, 114, 100, 101, 114, 115, 95, 115, 117, 109, 109, 97,
               114, 121, 18, 28, 84, 114, 97, 99, 107, 32, 111, 114, 100, 101, 114,
               115, 32, 99, 111, 117, 110, 116, 47, 116, 111, 116, 97, 108, 32, 115,
               117, 109, 24, 2, 34, 13, 34, 11, 8, 2, 17, 0, 0, 0, 0, 0, 0, 57, 64>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).

test_dsummary(_) ->
  prometheus_summary:new([{name, dsummary}, {help, "qwe"}]),
  prometheus_summary:dobserve(dsummary, 1.5),
  prometheus_summary:dobserve(dsummary, 2.7),

  %% dobserve is async so lets make sure gen_server processed our request
  timer:sleep(10),
  Expected = <<32, 10, 8, 100, 115, 117, 109, 109, 97, 114, 121, 18, 3, 113, 119, 101,
               24, 2, 34, 13, 34, 11, 8, 2, 17, 205, 204, 204, 204, 204, 204, 16, 64>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).

test_histogram(_) ->
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

  Expected = <<175, 1, 10, 34, 104, 116, 116, 112, 95, 114, 101, 113, 117, 101, 115,
               116, 95, 100, 117, 114, 97, 116, 105, 111, 110, 95, 109, 105, 108,
               108, 105, 115, 101, 99, 111, 110, 100, 115, 18, 27, 72, 116, 116, 112,
               32, 82, 101, 113, 117, 101, 115, 116, 32, 101, 120, 101, 99, 117, 116,
               105, 111, 110, 32, 116, 105, 109, 101, 24, 4, 34, 106, 10, 13, 10, 6,
               109, 101, 116, 104, 111, 100, 18, 3, 103, 101, 116, 58, 89, 8, 9, 17, 0,
               0, 0, 0, 0, 124, 164, 64, 26, 11, 8, 3, 17, 0, 0, 0, 0, 0, 0, 89, 64,
               26, 11, 8, 6, 17, 0, 0, 0, 0, 0, 192, 114, 64, 26, 11, 8, 7, 17, 0, 0,
               0, 0, 0, 64, 127, 64, 26, 11, 8, 8, 17, 0, 0, 0, 0, 0, 112, 135, 64, 26,
               11, 8, 9, 17, 0, 0, 0, 0, 0, 64, 143, 64, 26, 11, 8, 9, 17, 0, 0, 0, 0,
               0, 0, 240, 127>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).

test_dhistogram(_) ->
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

  %% dobserve is async so lets make sure gen_server processed our request
  timer:sleep(10),
  Expected = <<176, 1, 10, 34, 104, 116, 116, 112, 95, 114, 101, 113, 117, 101, 115,
               116, 95, 100, 117, 114, 97, 116, 105, 111, 110, 95, 109, 105, 108,
               108, 105, 115, 101, 99, 111, 110, 100, 115, 18, 27, 72, 116, 116, 112,
               32, 82, 101, 113, 117, 101, 115, 116, 32, 101, 120, 101, 99, 117, 116,
               105, 111, 110, 32, 116, 105, 109, 101, 24, 4, 34, 107, 10, 14, 10, 6,
               109, 101, 116, 104, 111, 100, 18, 4, 112, 111, 115, 116, 58, 89, 8, 6,
               17, 225, 122, 20, 174, 135, 0, 177, 64, 26, 11, 8, 0, 17, 0, 0, 0, 0, 0,
               0, 89, 64, 26, 11, 8, 1, 17, 0, 0, 0, 0, 0, 192, 114, 64, 26, 11, 8, 2,
               17, 0, 0, 0, 0, 0, 64, 127, 64, 26, 11, 8, 3, 17, 0, 0, 0, 0, 0, 112,
               135, 64, 26, 11, 8, 5, 17, 0, 0, 0, 0, 0, 64, 143, 64, 26, 11, 8, 6, 17,
               0, 0, 0, 0, 0, 0, 240, 127>>,
  ?_assertEqual(Expected,
                prometheus_protobuf_format:format()).
