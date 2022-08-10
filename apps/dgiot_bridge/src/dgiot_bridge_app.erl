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


-module(dgiot_bridge_app).

-behaviour(application).
-include_lib("dgiot/include/logger.hrl").
-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_bridge_sup:start_link(),
    dgiot_metrics:start(dgiot_parse),
    dgiot_metrics:start(dgiot_bridge),
    start_hook(),
    {ok, Sup}.

%%--------------------------------------------------------------------
stop(_State) ->
    stop_hook(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_hook() ->
    %%    app
    dgiot_hook:add(one_for_one, {uniapp, report}, fun dgiot_bridge:uniapp_report/1).


stop_hook() ->
    %%    app
    dgiot_hook:remove({uniapp, report}).
