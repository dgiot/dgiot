%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_modbus_app).

-emqx_plugin(?MODULE).

-behaviour(application).
-include("dgiot_modbus.hrl").
-include_lib("dgiot_task/include/dgiot_task.hrl").
%% Application callbacks
-export([start/2, stop/1, start_hook/0, stop_hook/0]).

start(_StartType, _StartArgs) ->
    start_hook(),
    dgiot_modbus_sup:start_link().

stop(_State) ->
    stop_hook(),
    ok.

start_hook() ->
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"MODBUSRTU">>}, fun modbus_rtu:get_datasource/1).

stop_hook() ->
    dgiot_hook:remove({?DGIOT_DATASOURCE, <<"MODBUSRTU">>}).
