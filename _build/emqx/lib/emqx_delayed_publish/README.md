
EMQ X Delayed Publish
=====================

MQTT Delayed Publish Plugin.

Specification
-------------

A MQTT client can publish a delayed message to broker. A Delayed Publish Message is identified using  a special style of Topic Name. The format is:

```
$delayed/{DelayInterval}/{TopicName}
```

- `$delayed` is a literal string that marks the Topic Name as being a delayed topic.
- `{DelayInterval}` The Delayed Interval which specifies the delay seconds of a MQTT message.
  The max interval allowed is 4294967.
- `{TopicName}` The topic name of a MQTT message


Exampels:

- `$delayed/15/x/y`: Publish the MQTT message to topic `x/y` 15 seconds later.
- `$delayed/60//a/b`: Publish the MQTT message to `/a/b` 1 minute later.
- `$delayed/3600/$SYS/topic`: Publish the MQTT message to `$SYS/topic` 1 hour later.


The MQTT broker should store a delayed message in memory or disc and then publish it when the delay expired.

Build
-----

make && make tests

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_delayed_publish
```

Example
--------

```
mosquitto_pub -t "$delayed/10/delay1" -q 1 -m 'hello'
mosquitto_pub -t "$delayed/60/topic" -q 1 -m 'hello'
```

Implementation
--------------

- Store delayed messages in local ordered_set table
- Start a publish timer at interval of ttl of the first record
- Submit the publish task to `emqx_pooler` when timer expired

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.

