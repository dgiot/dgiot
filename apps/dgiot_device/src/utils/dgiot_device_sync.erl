-module(dgiot_device_sync).
-export([sync_parse/1]).
sync_parse(X) -> dgiot_device_cache:sync_parse(X).
