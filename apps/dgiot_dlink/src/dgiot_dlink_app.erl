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

%% @doc
%%
%% dgiot_mqtt_app:
%%
%%
%%
%% @end
-module(dgiot_dlink_app).
-emqx_plugin(auth).
-behaviour(application).
-include_lib("dgiot_task/include/dgiot_task.hrl").

%% Application callbacks
-export([start/2,
    stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_dlink_sup:start_link(),
    start_hook(),
    {ok, Sup}.


stop(_State) ->
    stop_hook(_State),
    ok.

stop_hook(State) ->
    emqx:unhook('client.authenticate', fun dgiot_mqtt_auth:check/3),
    emqx:unhook('client.check_acl', fun dgiot_mqtt_acl:check_acl/5),
    emqx:unhook('client.disconnected', fun dgiot_mqtt_offline:on_client_disconnected/4),
    emqx:unhook('session.terminated', fun dgiot_mqtt_offline:on_session_terminated/4),
    emqx:unhook('message.publish', fun dgiot_mqtt_message:on_message_publish/2),
    dgiot_hook:remove({?DGIOT_DATASOURCE, <<"DLINK">>}),
    State.

%% todo dlink auth
start_hook() ->
    emqx:hook('client.authenticate', fun dgiot_mqtt_auth:check/3, [#{hash_type => plain}]),
    emqx:hook('client.check_acl', fun dgiot_mqtt_acl:check_acl/5, [#{}]),
    emqx:hook('client.disconnected', fun dgiot_mqtt_offline:on_client_disconnected/4, [#{}]),
    emqx:hook('session.terminated', fun dgiot_mqtt_offline:on_session_terminated/4, [#{}]),
    emqx:hook('message.publish', fun dgiot_mqtt_message:on_message_publish/2, [#{}]).

