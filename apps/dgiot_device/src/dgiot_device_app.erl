-module(dgiot_device_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-include("dgiot_device.hrl").

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_device_sup:start_link(),
    dgiot_product:start(),
    dgiot_protocol:start(),
    dgiot_product_hook:start(),
    {ok, Sup}.

stop(_State) ->
    dgiot_product_hook:stop(),
    ok.
