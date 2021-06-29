%%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Copyright (c) 2016-2017 shuwa .
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/lager:/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc
%%
%% shuwa_mqtt_app:
%%
%%
%%
%% @end
-module(shuwa_mqtt_app).

-emqx_plugin(?MODULE).

-behaviour(application).

-include("shuwa_mqtt.hrl").

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    shuwa_data:init(?SHUWA_MQTT_WORK),
    {ok, Sup} = shuwa_mqtt_sup:start_link(),
    {ok, Sup}.


stop(_State) ->
    ok.
