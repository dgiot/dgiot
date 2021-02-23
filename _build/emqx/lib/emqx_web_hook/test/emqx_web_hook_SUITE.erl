%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_web_hook_SUITE).

-compile([nowarn_export_all]).
-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(HOOK_LOOKUP(H), emqx_hooks:lookup(list_to_atom(H))).
-define(ACTION(Name), #{<<"action">> := Name}).

all() ->
    [{group, emqx_web_hook_actions},
     {group, emqx_web_hook}].

groups() ->
    [{emqx_web_hook, [sequence], [reload, change_config]},
     {emqx_web_hook_actions, [sequence], [validate_web_hook]}
    ].

init_per_suite(Config) ->
    ok = ekka_mnesia:start(),
    emqx_ct_helpers:start_apps([emqx, emqx_web_hook]),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx_web_hook, emqx]).

reload(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    lists:foreach(fun({HookName, _Action}) ->
                          Hooks  = ?HOOK_LOOKUP(HookName),
                          ?assertEqual(true, length(Hooks) > 0)
                  end, Rules).

change_config(_Config) ->
    {ok, Rules} = application:get_env(emqx_web_hook, rules),
    emqx_web_hook:unload(),
    HookRules = lists:keydelete("message.delivered", 1, Rules),
    application:set_env(emqx_web_hook, rules, HookRules),
    emqx_web_hook:load(),
    ?assertEqual([], ?HOOK_LOOKUP("message.delivered")),
    emqx_web_hook:unload(),
    application:set_env(emqx_web_hook, rules, Rules),
    emqx_web_hook:load().

validate_web_hook(_Config) ->
    http_server:start_http(),
    {ok, C} = emqtt:start_link([ {clientid, <<"simpleClient">>}
                               , {username, <<"username">>}
                               , {proto_ver, v5}
                               , {keepalive, 60}
                               ]),
    {ok, _} = emqtt:connect(C),
    emqtt:subscribe(C, <<"TopicA">>, qos2),
    emqtt:publish(C, <<"TopicA">>, <<"Payload...">>, qos2),
    emqtt:unsubscribe(C, <<"TopicA">>),
    emqtt:disconnect(C),
    ValidateData = get_http_message(),
    ?assertEqual(length(ValidateData), 11),
    [validate_hook_resp(A) || A <- ValidateData],
    http_server:stop_http().

get_http_message() ->
    get_http_message([]).

get_http_message(Acc) ->
    receive
        Info -> get_http_message([Info | Acc])
    after
        300 ->
            lists:reverse([jsx:decode(Info, [return_maps]) || [{Info, _}] <- Acc])
    end.
validate_hook_resp(Body = ?ACTION(<<"client_connect">>)) ->
    ?assertEqual(5,  maps:get(<<"proto_ver">>, Body)),
    ?assertEqual(60, maps:get(<<"keepalive">>, Body)),
    ?assertEqual(<<"127.0.0.1">>, maps:get(<<"ipaddress">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"client_connack">>)) ->
    ?assertEqual(5,  maps:get(<<"proto_ver">>, Body)),
    ?assertEqual(60, maps:get(<<"keepalive">>, Body)),
    ?assertEqual(<<"success">>, maps:get(<<"conn_ack">>, Body)),
    ?assertEqual(<<"127.0.0.1">>, maps:get(<<"ipaddress">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"client_connected">>)) ->
    _ = maps:get(<<"connected_at">>, Body),
    ?assertEqual(5,  maps:get(<<"proto_ver">>, Body)),
    ?assertEqual(60, maps:get(<<"keepalive">>, Body)),
    ?assertEqual(<<"127.0.0.1">>, maps:get(<<"ipaddress">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"client_disconnected">>)) ->
    ?assertEqual(<<"normal">>, maps:get(<<"reason">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"client_subscribe">>)) ->
    _ = maps:get(<<"opts">>, Body),
    ?assertEqual(<<"TopicA">>, maps:get(<<"topic">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"client_unsubscribe">>)) ->
    _ = maps:get(<<"opts">>, Body),
    ?assertEqual(<<"TopicA">>, maps:get(<<"topic">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"session_subscribed">>)) ->
    _ = maps:get(<<"opts">>, Body),
    ?assertEqual(<<"TopicA">>, maps:get(<<"topic">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"session_unsubscribed">>)) ->
    ?assertEqual(<<"TopicA">>, maps:get(<<"topic">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"session_terminated">>)) ->
    ?assertEqual(<<"normal">>, maps:get(<<"reason">>, Body)),
    assert_username_clientid(Body);
validate_hook_resp(Body = ?ACTION(<<"message_publish">>)) ->
    assert_messages_attrs(Body);
validate_hook_resp(Body = ?ACTION(<<"message_delivered">>)) ->
    assert_messages_attrs(Body);
validate_hook_resp(Body = ?ACTION(<<"message_acked">>)) ->
    assert_messages_attrs(Body).

assert_username_clientid(#{<<"clientid">> := ClientId, <<"username">> := Username}) ->
    ?assertEqual(<<"simpleClient">>, ClientId),
    ?assertEqual(<<"username">>, Username).

assert_messages_attrs(#{ <<"ts">> := _
                       , <<"qos">> := _
                       , <<"topic">> := _
                       , <<"retain">> := _
                       , <<"payload">> := _
                       , <<"from_username">> := _
                       , <<"from_client_id">> := _
                       }) ->
    ok.

