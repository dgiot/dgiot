-module(dgiot_device_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-include("dgiot_device.hrl").

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_device_sup:start_link(),
    dgiot_protocol:start(),
    start_hook(),
    {ok, Sup}.

stop(_State) ->
    stop_hook(),
    ok.

start_hook() ->
    dgiot_hook:add(one_for_one, 'stats_Product', fun dgiot_product:stats_Product/1),
    dgiot_hook:add(one_for_one, 'parse_cache_Device', fun dgiot_device:parse_cache_Device/1).

stop_hook() ->
    dgiot_hook:remove('stats_Product'),
    dgiot_hook:remove('parse_cache_Device').

