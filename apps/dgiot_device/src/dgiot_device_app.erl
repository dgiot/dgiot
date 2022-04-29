-module(dgiot_device_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-include("dgiot_device.hrl").

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_device_sup:start_link(),
    dgiot_product:start(),
    dgiot_protocol:start(),
    start_hook(),
    {ok, Sup}.

stop(_State) ->
    stop_hook(),
    ok.

start_hook() ->
    dgiot_hook:add(one_for_one, {'parse_cache_classes', <<"Product">>}, fun dgiot_product:load_cache/1),
    dgiot_hook:add(one_for_one, {'parse_cache_classes', <<"Device">>}, fun dgiot_device:load_cache/1).

stop_hook() ->
    dgiot_hook:add(one_for_one, {'parse_cache_classes', <<"Product">>}),
    dgiot_hook:add(one_for_one, {'parse_cache_classes', <<"Device">>}).