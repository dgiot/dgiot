-module(dgiot_device_put).
-export([put/1]).
put(X) -> dgiot_device_cache:put(X).
