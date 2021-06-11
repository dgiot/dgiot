%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 dgiot Enterprise, Inc. (http://www.iotn2n.com)
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

-module(dgiot_license_app).

-behaviour(application).

-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).
-include("dgiot_license.hrl").
%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    dgiot_data:init(?DGIOT_LIC_WORK),
    dgiot_license_sup:start_link().

stop(_State) ->
    ok.

