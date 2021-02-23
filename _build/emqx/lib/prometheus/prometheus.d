src/collectors/mnesia/prometheus_mnesia_collector.erl:: src/prometheus_collector.erl; @touch $@
src/collectors/vm/prometheus_vm_memory_collector.erl:: include/prometheus.hrl src/prometheus_collector.erl; @touch $@
src/collectors/vm/prometheus_vm_statistics_collector.erl:: include/prometheus.hrl src/prometheus_collector.erl; @touch $@
src/collectors/vm/prometheus_vm_system_info_collector.erl:: include/prometheus.hrl src/prometheus_collector.erl; @touch $@
src/contrib/prometheus_test_instrumenter.erl:: src/prometheus_instrumenter.erl; @touch $@
src/formats/prometheus_protobuf_format.erl:: include/prometheus.hrl include/prometheus_model.hrl src/prometheus_format.erl; @touch $@
src/formats/prometheus_text_format.erl:: include/prometheus.hrl include/prometheus_model.hrl src/prometheus_format.erl; @touch $@
src/metrics/prometheus_counter.erl:: include/prometheus.hrl src/prometheus_collector.erl src/prometheus_metric.erl; @touch $@
src/metrics/prometheus_gauge.erl:: include/prometheus.hrl src/prometheus_collector.erl src/prometheus_metric.erl; @touch $@
src/metrics/prometheus_histogram.erl:: include/prometheus.hrl src/prometheus_collector.erl src/prometheus_metric.erl; @touch $@
src/metrics/prometheus_summary.erl:: include/prometheus.hrl src/prometheus_collector.erl src/prometheus_metric.erl; @touch $@
src/model/prometheus_model.erl:: include/prometheus_model.hrl; @touch $@
src/model/prometheus_model_helpers.erl:: include/prometheus_model.hrl; @touch $@
src/prometheus_collector.erl:: include/prometheus.hrl; @touch $@
src/prometheus_metric.erl:: include/prometheus.hrl; @touch $@
src/prometheus_registry.erl:: include/prometheus.hrl; @touch $@
src/prometheus_sup.erl:: include/prometheus.hrl; @touch $@

COMPILE_FIRST += prometheus_collector prometheus_metric prometheus_format prometheus_instrumenter
