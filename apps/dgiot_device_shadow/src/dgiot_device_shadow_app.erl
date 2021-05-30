-module(dgiot_device_shadow_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-include("dgiot_device_shadow.hrl").

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_shadow_sup:start_link(),
    dgiot_product:start(),
    dgiot_protocol:start(),
    dgiot_local_devcache:init(),
    {ok, Sup}.

stop(_State) ->
    ok.
