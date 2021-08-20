%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-ifndef(DGIOT_GRPC_HRL).
-define(DGIOT_GRPC_HRL, true).

-record(state, {
    id,
    env = #{}
}).


-define(ENABLED_HOOKS,
    [ {'client.connect',      {dgiot_exhook_handler, on_client_connect,       []}}
        , {'client.connack',      {dgiot_exhook_handler, on_client_connack,       []}}
        , {'client.connected',    {dgiot_exhook_handler, on_client_connected,     []}}
        , {'client.disconnected', {dgiot_exhook_handler, on_client_disconnected,  []}}
        , {'client.authenticate', {dgiot_exhook_handler, on_client_authenticate,  []}}
        , {'client.check_acl',    {dgiot_exhook_handler, on_client_check_acl,     []}}
        , {'client.subscribe',    {dgiot_exhook_handler, on_client_subscribe,     []}}
        , {'client.unsubscribe',  {dgiot_exhook_handler, on_client_unsubscribe,   []}}
        , {'session.created',     {dgiot_exhook_handler, on_session_created,      []}}
        , {'session.subscribed',  {dgiot_exhook_handler, on_session_subscribed,   []}}
        , {'session.unsubscribed',{dgiot_exhook_handler, on_session_unsubscribed, []}}
        , {'session.resumed',     {dgiot_exhook_handler, on_session_resumed,      []}}
        , {'session.discarded',   {dgiot_exhook_handler, on_session_discarded,    []}}
        , {'session.takeovered',  {dgiot_exhook_handler, on_session_takeovered,   []}}
        , {'session.terminated',  {dgiot_exhook_handler, on_session_terminated,   []}}
        , {'message.publish',     {dgiot_exhook_handler, on_message_publish,      []}}
        , {'message.delivered',   {dgiot_exhook_handler, on_message_delivered,    []}}
        , {'message.acked',       {dgiot_exhook_handler, on_message_acked,        []}}
        , {'message.dropped',     {dgiot_exhook_handler, on_message_dropped,      []}}
    ]).

-endif.
