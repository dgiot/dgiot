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
    dgiot_hook:add(one_for_one, {'topo', <<"counter">>, <<"device_counter">>}, fun dgiot_device_static:get_counter/1),
    dgiot_hook:add(one_for_one, {'topo', <<"counter">>, <<"product_counter">>}, fun dgiot_device_static:get_counter/1),
    dgiot_hook:add(one_for_one, {'topo', <<"counter">>, <<"device_offline_counter">>}, fun dgiot_device_static:get_counter/1),
    dgiot_hook:add(one_for_one, {'topo', <<"counter">>, <<"device_online_counter">>}, fun dgiot_device_static:get_counter/1),
    dgiot_hook:add(one_for_one, {'topo', <<"pie">>, <<"device_poweron_poweroff">>}, fun dgiot_device_static:get_pie/1).

stop_hook() ->
    dgiot_hook:remove('parse_cache_Device'),
    dgiot_hook:remove({'topo', <<"counter">>, <<"device_counter">>}),
    dgiot_hook:remove({'topo', <<"counter">>, <<"product_counter">>}),
    dgiot_hook:remove({'topo', <<"counter">>, <<"device_offline_counter">>}),
    dgiot_hook:remove({'topo', <<"counter">>, <<"device_online_counter">>}),
    dgiot_hook:remove({'topo', <<"counter">>, <<"device_poweron_counter">>}),
    dgiot_hook:remove({'topo', <<"counter">>, <<"device_poweroff_counter">>}),
    dgiot_hook:remove({'topo', <<"pie">>, <<"device_poweron_poweroff">>}).
