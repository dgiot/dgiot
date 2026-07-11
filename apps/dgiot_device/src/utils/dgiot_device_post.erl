-module(dgiot_device_post).
-export([post/1, post/2]).
post(X) -> dgiot_device_cache:post(X).
post(X, Y) -> dgiot_device_cache:post(X, Y).
