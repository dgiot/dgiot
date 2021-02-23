# ReplayQ

A Disk Queue for Log Replay in Erlang

## Features

* Queue items are written to segment files on disk to servive restart.
* Batch poping items out of queue with size/count limit.
* An `ack/2` API is provided to record the reader position within a segment.
* Add config option `max_total_bytes` to limit the total size of replayq logs

## Usage Example

### Mem Only

```
Q0 = replayq:open(#{mem_only => true}),
Q1 = replayq:append(Q0, [Binary1, Binary2]),
{Q2, AckRef, [Binary1]} = replayq:pop(Q1, #{bytes_limt => 1}),
ok = replayq:ack(Q2, AckRef).
```

### Binary Queue Items

```
Q0 = replayq:open(#{dir => "/tmp/replayq-test", seg_bytes => 10000000}),
Q1 = replayq:append(Q0, [Binary1, Binary2]),
{Q2, AckRef, [Binary1]} = replayq:pop(Q1, #{count_limit => 1}),
ok = replayq:ack(Q2, AckRef).
```

### User Defined Queue Items

```
Q0 = replayq:open(#{dir => "/tmp/replayq-test",
                    seg_bytes => 10000000,
                    sizer => fun({K, V}) -> size(K) + size(V) end,
                    marshaller => fun({K, V}) -> term_to_binary({K, V});
                                     (Bin)    -> binary_to_term(Bin)
                                  end
                   }),
Q1 = replayq:append(Q0, [{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}]),
{Q2, AckRef, [{<<"k1">>, <<"v1">>}]} = replayq:pop(Q1, #{count_limit => 1}),
ok = replayq:ack(Q2, AckRef).
```
