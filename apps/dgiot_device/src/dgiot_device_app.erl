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
    dgiot_hook:add(one_for_one, 'parse_cache_Device', fun dgiot_device:parse_cache_Device/1),
    dgiot_hook:add(one_for_one, 'parse_cache_Product', fun dgiot_product:load_all_cache/1),
    dgiot_hook:add(one_for_one, {'topo', <<"counter">>}, fun dgiot_device_static:get_counter/1),
    dgiot_hook:add(one_for_one, {'topo', <<"realdata">>}, fun dgiot_device_static:get_realdata/1),
    dgiot_hook:add(one_for_one, {'topo', <<"pie">>}, fun dgiot_device_static:get_pie/1).

stop_hook() ->
    dgiot_hook:remove('parse_cache_Device'),
    dgiot_hook:remove('parse_cache_Product'),
    dgiot_hook:remove({'topo', <<"counter">>}),
    dgiot_hook:remove({'topo', <<"realdata">>}),
    dgiot_hook:remove({'topo', <<"pie">>}).
