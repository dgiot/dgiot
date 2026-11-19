-module(dgiot_iotdb_app).
-dgiot_plugin(?MODULE).
-ifdef(DGIOT_WITH_EMQX).
-emqx_plugin(?MODULE).
-endif.
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dgiot_metrics:start(dgiot_iotdb),
    dgiot_iotdb_sup:start_link().

stop(_State) ->
    ok.
