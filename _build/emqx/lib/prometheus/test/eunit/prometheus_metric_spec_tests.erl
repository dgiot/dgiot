-module(prometheus_metric_spec_tests).

-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
  Spec = [{name, "qwe"}],

  ?assertMatch(undefined,
               prometheus_metric_spec:get_value(labels, Spec)),
  ?assertMatch([default],
               prometheus_metric_spec:get_value(labels, Spec, [default])),

  ?assertEqual("qwe", prometheus_metric_spec:get_value(name, Spec)).

fetch_value_test() ->
  Spec = [{name, "qwe"}],

  ?assertError({missing_metric_spec_key, labels, Spec},
               prometheus_metric_spec:fetch_value(labels, Spec)),

  ?assertEqual("qwe", prometheus_metric_spec:fetch_value(name, Spec)).

validate_metric_name_test() ->
  ?assertError({invalid_metric_name, 12, "metric name is not a string"},
               prometheus_metric_spec:validate_metric_name(12)),
  ?assertError({invalid_metric_name, <<0, 0, 123>>, "metric name is invalid string"},
               prometheus_metric_spec:validate_metric_name(<<0, 0, 123>>)),
  ?assertError({invalid_metric_name,
                "1qwe",
                "metric name doesn't match regex ^[a-zA-Z_:][a-zA-Z0-9_:]*$"},
               prometheus_metric_spec:validate_metric_name("1qwe")),

  ?assertEqual('qwe_:qwe', prometheus_metric_spec:validate_metric_name('qwe_:qwe')),
  ?assertEqual("qwe_:qwe", prometheus_metric_spec:validate_metric_name("qwe_:qwe")),
  ?assertEqual(<<"qwe_:qwe">>,
               prometheus_metric_spec:validate_metric_name(<<"qwe_:qwe">>)).

validate_metric_label_names_test() ->
  ?assertError({invalid_metric_labels, 12, "not list"},
               prometheus_metric_spec:validate_metric_label_names(12)),
  ?assertError({invalid_metric_label_name, 12, "metric label is not a string"},
               prometheus_metric_spec:validate_metric_label_names([12])),
  ?assertError({invalid_metric_label_name, [0, 0, 123], "metric label is invalid string"},
               prometheus_metric_spec:validate_metric_label_names([<<0, 0, 123>>])),
  ?assertError({invalid_metric_label_name, "__qwe", "metric label can't start with __"},
               prometheus_metric_spec:validate_metric_label_names(["__qwe"])),
  ?assertError({invalid_metric_label_name,
                "qwe:",
                "metric label doesn't match regex ^[a-zA-Z_][a-zA-Z0-9_]*$"},
               prometheus_metric_spec:validate_metric_label_names(["qwe:"])),

  ?assertEqual(["_qwe", "weq123"],
               prometheus_metric_spec:validate_metric_label_names([<<"_qwe">>,
                                                                   'weq123'])),
  ?assertEqual(["_qwe", "weq123"],
               prometheus_metric_spec:validate_metric_label_names([<<"_qwe">>,
                                                                   "weq123"])).

validate_metric_help_test() ->
  ?assertError({invalid_metric_help, 12, "metric help is not a string"},
               prometheus_metric_spec:validate_metric_help(12)),
  ?assertError({invalid_metric_help, [0, 0, 123], "metric help is invalid string"},
               prometheus_metric_spec:validate_metric_help(<<0, 0, 123>>)),

  ?assertEqual("qwe_:qwe", prometheus_metric_spec:validate_metric_help("qwe_:qwe")),
  ?assertEqual("qwe_:qwe", prometheus_metric_spec:validate_metric_help(<<"qwe_:qwe">>)).

call_timeout_test() ->
  ?assertEqual(5000, prometheus_metric_spec:call_timeout([{call_timeout, true}])),
  ?assertEqual(1000, prometheus_metric_spec:call_timeout([{call_timeout, 1000}])),
  ?assertEqual(infinity, prometheus_metric_spec:call_timeout([{call_timeout, infinity}])),
  ?assertEqual(false, prometheus_metric_spec:call_timeout([])),
  ?assertEqual(false, prometheus_metric_spec:call_timeout([{call_timeout, false}])),

  ?assertError({invalid_value, invalid,
                "call timeout must be 'false', 'true', 'infinity' "
                "or a positive integer"},
               prometheus_metric_spec:call_timeout([{call_timeout, invalid}])).

duration_unit_test() ->
  ?assertEqual(undefined, prometheus_metric_spec:duration_unit([{name, "qwe"}])),
  ?assertEqual(milliseconds,
               prometheus_metric_spec:duration_unit([{name,
                                                      "request_duration_milliseconds"}])),
  ?assertEqual(undefined,
               prometheus_metric_spec:duration_unit([{name,
                                                      "request_duration_milliseconds"},
                                                     {duration_unit, false}])),
  ?assertEqual(microseconds,
               prometheus_metric_spec:duration_unit([{name, "qwe"},
                                                     {duration_unit, microseconds}])),
  ?assertError({invalid_value, invalid, "unknown duration unit"},
               prometheus_metric_spec:duration_unit([{name, "qwe"},
                                                     {duration_unit, invalid}])),
  ?assertError({invalid_value, microseconds, "duration unit doesn't match metric name"},
               prometheus_metric_spec:duration_unit([{name,
                                                      "request_duration_milliseconds"},
                                                     {duration_unit, microseconds}])).

extract_common_params_test() ->
  ?assertError({invalid_metric_name, 12, "metric name is not a string"},
               prometheus_metric_spec:extract_common_params([{name, 12}])),
  ?assertError({invalid_metric_labels, 12, "not list"},
               prometheus_metric_spec:extract_common_params([{name, "qwe"},
                                                             {labels, 12}])),
  ?assertError({invalid_metric_help, 12, "metric help is not a string"},
               prometheus_metric_spec:extract_common_params([{name, "qwe"},
                                                             {labels, ["qwe"]},
                                                             {help, 12}])),

  ?assertEqual({default, "qwe", [], "qwe", false, undefined, undefined},
               prometheus_metric_spec:extract_common_params([{name, "qwe"},
                                                             {help, "qwe"}])),
  ?assertEqual({qwe, "qwe", ["qwe"], "qwe", false, undefined, data},
               prometheus_metric_spec:extract_common_params([{name, "qwe"},
                                                             {labels, ["qwe"]},
                                                             {help, "qwe"},
                                                             {registry, qwe},
                                                             {data, data}])).
