-define(PROMETHEUS_REGISTRY_TABLE, prometheus_registry_table).
-define(PROMETHEUS_COUNTER_TABLE, prometheus_counter_table).
-define(PROMETHEUS_GAUGE_TABLE, prometheus_gauge_table).
-define(PROMETHEUS_SUMMARY_TABLE, prometheus_summary_table).
-define(PROMETHEUS_HISTOGRAM_TABLE, prometheus_histogram_table).

-define(PROMETHEUS_COUNTER_DEFAULT, 0).

-define(PROMETHEUS_STANDARD_METRICS, [prometheus_counter,
                                      prometheus_gauge,
                                      prometheus_summary,
                                      prometheus_histogram]).

-define(DEPRECATED(Old, New),
        error_logger:warning_msg(Old " is deprecated and will soon be removed. "
                                 "Please use " New " instead.~n")).
