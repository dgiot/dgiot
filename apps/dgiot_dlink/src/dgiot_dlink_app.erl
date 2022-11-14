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

%% Application callbacks
-export([start/2,
    prep_stop/1,
    stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = dgiot_dlink_sup:start_link(),
    _ = load_auth_hook(),
    _ = load_acl_hook(),
    _ = load_offline_hook(),
    _ = load_publish_hook(),
    {ok, Sup}.


stop(_State) ->
    ok.

prep_stop(State) ->
    emqx:unhook('client.authenticate', fun dgiot_mqtt_auth:check/3),
    emqx:unhook('client.check_acl', fun dgiot_mqtt_acl:check_acl/5),
    emqx:unhook('client.disconnected', fun dgiot_mqtt_offline:on_client_disconnected/4),
    emqx:unhook('session.terminated', fun dgiot_mqtt_offline:on_session_terminated/4),
    emqx:unhook('message.publish', fun dgiot_mqtt_message:on_message_publish/2),
    State.

%% todo dlink auth
load_auth_hook() ->
    emqx:hook('client.authenticate', fun dgiot_mqtt_auth:check/3, [#{hash_type => plain}]),
    ok.
load_acl_hook() ->
    emqx:hook('client.check_acl', fun dgiot_mqtt_acl:check_acl/5, [#{}]),
    ok.

load_offline_hook() ->
    emqx:hook('client.disconnected', fun dgiot_mqtt_offline:on_client_disconnected/4, [#{}]),
    emqx:hook('session.terminated', fun dgiot_mqtt_offline:on_session_terminated/4, [#{}]),
    ok.

load_publish_hook() ->
    emqx:hook('message.publish', fun dgiot_mqtt_message:on_message_publish/2, [#{}]),
    ok.

