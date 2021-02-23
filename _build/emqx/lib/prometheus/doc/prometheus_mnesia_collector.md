

# Module prometheus_mnesia_collector #
* [Description](#description)

Collects Mnesia metrics mainly using
[
mnesia:system_info/1
](http://erlang.org/doc/man/mnesia.md#system_info-1).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##


### <a name="Exported_metrics">Exported metrics</a> ###


* `erlang_mnesia_held_locks`<br />
Type: gauge.<br />
Number of held locks.

* `erlang_mnesia_lock_queue`<br />
Type: gauge.<br />
Number of transactions waiting for a lock.

* `erlang_mnesia_transaction_participants`<br />
Type: gauge.<br />
Number of participant transactions.

* `erlang_mnesia_transaction_coordinators`<br />
Type: gauge.<br />
Number of coordinator transactions.

* `erlang_mnesia_failed_transactions`<br />
Type: counter.<br />
Number of failed (i.e. aborted) transactions.

* `erlang_mnesia_committed_transactions`<br />
Type: gauge.<br />
Number of committed transactions.

* `erlang_mnesia_logged_transactions`<br />
Type: counter.<br />
Number of transactions logged.

* `erlang_mnesia_restarted_transactions`<br />
Type: counter.<br />
Total number of transaction restarts.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`mnesia_collector_metrics` key of `prometheus` app environment.

Available options:
- `held_locks` for `erlang_mnesia_held_locks`;
- `lock_queue` for `erlang_mnesia_lock_queue`;
- `transaction_participants` for `erlang_mnesia_transaction_participants`;
- `transaction_coordinators` for `erlang_mnesia_transaction_coordinators`;
- `transaction_failures` for `erlang_mnesia_failed_transactions`;
- `transaction_commits` for `erlang_mnesia_committed_transactions`;
- `transaction_log_writes` for `erlang_mnesia_logged_transactions`;
- `transaction_restarts` for `erlang_mnesia_restarted_transactions`.

By default all metrics are enabled.
