%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqtt_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-import(lists, [nth/2]).

-include("emqtt.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-define(TOPICS, [<<"TopicA">>, <<"TopicA/B">>, <<"Topic/C">>, <<"TopicA/C">>,
                 <<"/TopicA">>]).

-define(WILD_TOPICS, [<<"TopicA/+">>, <<"+/C">>, <<"#">>, <<"/#">>, <<"/+">>,
                      <<"+/+">>, <<"TopicA/#">>]).

all() ->
    [{group, general}, {group, mqttv3}, {group, mqttv4}, {group, mqttv5}].

groups() ->
    [{general, [non_parallel_tests],
      [t_connect,
       t_ws_connect,
       t_subscribe,
       t_publish,
       t_unsubscribe,
       t_ping,
       t_puback,
       t_pubrec,
       t_pubrel,
       t_pubcomp,
       t_subscriptions,
       t_info,
       t_stop,
       t_pause_resume,
       t_init,
       t_connected]},
    {mqttv3,[non_parallel_tests],
      [basic_test_v3]},
    {mqttv4, [non_parallel_tests],
      [basic_test_v4,
       anonymous_test,
       retry_interval_test,
       will_message_test,
       will_retain_message_test,
       offline_message_queueing_test,
       overlapping_subscriptions_test,
       redelivery_on_reconnect_test,
       dollar_topics_test]},
    {mqttv5, [non_parallel_tests],
      [basic_test_v5,
       retain_as_publish_test]}].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_auth_username]),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([]).

receive_messages(Count) ->
    receive_messages(Count, []).

receive_messages(0, Msgs) ->
    Msgs;
receive_messages(Count, Msgs) ->
    receive
        {publish, Msg} ->
            receive_messages(Count-1, [Msg|Msgs]);
        _Other ->
            receive_messages(Count, Msgs)
    after 100 ->
        Msgs
    end.

clean_retained(Topic) ->
    {ok, Clean} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(Clean),
    {ok, _} = emqtt:publish(Clean, Topic, #{}, <<"">>, [{qos, ?QOS_1}, {retain, true}]),
    ok = emqtt:disconnect(Clean).

t_props(_) ->
    ok = emqtt_props:validate(#{'Payload-Format-Indicator' => 0}).

t_connect(_) ->
    {ok, C} = emqtt:start_link(),
    {ok, _} = emqtt:connect(C),
    ok= emqtt:disconnect(C),

    {ok, C1} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C1),
    ok= emqtt:disconnect(C1),

    {ok, C2} = emqtt:start_link(#{clean_start => true}),
    {ok, _} = emqtt:connect(C2),
    ok= emqtt:disconnect(C2).

t_ws_connect(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}, {host,"127.0.0.1"}, {port, 8083}]),
    {ok, _} = emqtt:ws_connect(C),
    ok = emqtt:disconnect(C).

t_subscribe(_) ->
    Topic = nth(1, ?TOPICS),
    {ok, C} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C),

    {ok, _, [0]} = emqtt:subscribe(C, Topic),
    {ok, _, [0]} = emqtt:subscribe(C, Topic, at_most_once),
    {ok, _, [0]} = emqtt:subscribe(C, {Topic, at_most_once}),
    {ok, _, [0]} = emqtt:subscribe(C, #{}, Topic, at_most_once),

    {ok, _, [1]} = emqtt:subscribe(C, Topic, 1),
    {ok, _, [1]} = emqtt:subscribe(C, {Topic, 1}),
    {ok, _, [1]} = emqtt:subscribe(C, #{}, Topic, 1),

    {ok, _, [2]} = emqtt:subscribe(C, Topic, [{qos, ?QOS_2}]),
    {ok, _, [2]} = emqtt:subscribe(C, #{}, Topic, [{qos, ?QOS_2}, {nl, false}, {other, ignore}]),

    {ok, _, [0,1,2]} = emqtt:subscribe(C, [{Topic, at_most_once},{Topic, 1}, {Topic, [{qos, ?QOS_2}]}]),
    ok = emqtt:disconnect(C).

t_publish(_) ->
    Topic = nth(1, ?TOPICS),
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),

    ok = emqtt:publish(C, Topic, <<"t_publish">>),
    ok = emqtt:publish(C, Topic, <<"t_publish">>, 0),
    ok = emqtt:publish(C, Topic, <<"t_publish">>, at_most_once),
    {ok, _} = emqtt:publish(C, Topic, <<"t_publish">>, [{qos, 1}]),
    {ok, _} = emqtt:publish(C, Topic, #{}, <<"t_publish">>, [{qos, 2}]),

    ok = emqtt:disconnect(C).

t_unsubscribe(_) ->
    Topic1 = nth(1, ?TOPICS),
    Topic2 = nth(2, ?TOPICS),
    Topic3 = nth(3, ?TOPICS),
    Topic4 = nth(4, ?TOPICS),
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    {ok, _, [0,0,0,0]} = emqtt:subscribe(C, [{Topic1, 0}, {Topic2, 0}, {Topic3, 0}, {Topic4, 0}]),

    {ok, _, _} = emqtt:unsubscribe(C, Topic1),
    {ok, _, _} = emqtt:unsubscribe(C, [Topic2]),
    {ok, _, _} = emqtt:unsubscribe(C, #{}, Topic3),
    {ok, _, _} = emqtt:unsubscribe(C, #{}, [Topic4]),

    ok = emqtt:disconnect(C).

t_ping(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    pong = emqtt:ping(C),
    ok = emqtt:disconnect(C).

t_puback(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:puback(C, 0),
    ok = emqtt:disconnect(C).

t_pubrec(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:pubrec(C, 0),
    ok = emqtt:disconnect(C).

t_pubrel(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:pubrel(C, 0),
    ok = emqtt:disconnect(C).

t_pubcomp(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:pubcomp(C, 0),
    ok = emqtt:disconnect(C).

t_subscriptions(_) ->
    Topic = nth(1, ?TOPICS),

    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),

    [] = emqtt:subscriptions(C),
    {ok, _, [0]} = emqtt:subscribe(C, Topic, 0),

    [{Topic, #{qos := 0}}] = emqtt:subscriptions(C),

    ok = emqtt:disconnect(C).

t_info(_) ->
    {ok, C} = emqtt:start_link([{name, test_info}, {clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    [ ?assertEqual(test_info, Value) || {Key, Value} <- emqtt:info(C), Key =:= name],
    ok = emqtt:disconnect(C).

t_stop(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:stop(C).

t_pause_resume(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    ok = emqtt:pause(C),
    ok = emqtt:resume(C),
    ok = emqtt:disconnect(C).

t_init(_) ->
    {ok, C1} = emqtt:start_link([{name, test},
                                {owner, self()},
                                {host, {127,0,0,1}},
                                {port, 1883},
                                {ssl, false},
                                {ssl_opts, #{}},
                                {clientid, <<"test">>},
                                {clean_start, true},
                                {username, <<"username">>},
                                {password, <<"password">>},
                                {keepalive, 2},
                                {connect_timeout, 2},
                                {ack_timeout, 2},
                                {force_ping, true},
                                {properties, #{}},
                                {max_inflight, 1},
                                {auto_ack, true},
                                {bridge_mode, true},
                                {retry_interval, 10},
                                {other, ignore},
                                {proto_ver, v3}]),

    {ok, _} = emqtt:connect(C1),
    ok = emqtt:disconnect(C1),

    {ok, C2} = emqtt:start_link([{proto_ver, v4},
                                {msg_handler,undefined},
                                {hosts, [{{127,0,0,1}, 1883}]},
                                % {ws_path, "abcd"},
                                {max_inflight, infinity},
                                force_ping,
                                auto_ack]),
    {ok, _} = emqtt:connect(C2),
    ok = emqtt:disconnect(C2),

    {ok, C3} = emqtt:start_link([{proto_ver, v5},
                                {hosts, [{127,0,0,1}]},
                                {will_topic, nth(3, ?TOPICS)},
                                {will_payload, <<"will_retain_message_test">>},
                                {will_qos, ?QOS_1},
                                {will_retain, true},
                                {will_props, #{}}]),
    {ok, _} = emqtt:connect(C3),
    ok = emqtt:disconnect(C3).

t_initialized(_) ->
    error('TODO').

t_waiting_for_connack(_) ->
    error('TODO').

t_connected(_) ->
    {ok, C} = emqtt:start_link([{clean_start, true}]),
    {ok, _} = emqtt:connect(C),
    Clientid = gen_statem:call(C, clientid),
    [ ?assertMatch(Clientid, Value) || {Key, Value} <- emqtt:info(C), Key =:= clientid].

t_inflight_full(_) ->
    error('TODO').

t_handle_event(_) ->
    error('TODO').

t_terminate(_) ->
    error('TODO').

t_code_change(_) ->
    error('TODO').

t_reason_code_name(_) ->
    error('TODO').

basic_test(Opts) ->
    Topic = nth(1, ?TOPICS),
    ct:print("Basic test starting"),
    {ok, C} = emqtt:start_link(Opts),
    {ok, _} = emqtt:connect(C),
    {ok, _, [1]} = emqtt:subscribe(C, Topic, qos1),
    {ok, _, [2]} = emqtt:subscribe(C, Topic, qos2),
    {ok, _} = emqtt:publish(C, Topic, <<"qos 2">>, 2),
    {ok, _} = emqtt:publish(C, Topic, <<"qos 2">>, 2),
    {ok, _} = emqtt:publish(C, Topic, <<"qos 2">>, 2),
    ?assertEqual(3, length(receive_messages(3))),
    ok = emqtt:disconnect(C).

basic_test_v3(_Config) ->
    basic_test([{proto_ver, v3}]).

basic_test_v4(_Config) ->
    basic_test([{proto_ver, v4}]).

anonymous_test(_Config) ->
     application:set_env(emqx, allow_anonymous, false),

    process_flag(trap_exit, true),
    {ok, C1} = emqtt:start_link(),
    {_,{unauthorized_client,_}} = emqtt:connect(C1),
    process_flag(trap_exit, false),

    emqx_plugins:load(emqx_auth_username),
    emqx_auth_username:add_user(<<"test">>, <<"password">>),

    {ok, C2} = emqtt:start_link([{username, <<"test">>}, {password, <<"password">>}]),
    {ok, _} = emqtt:connect(C2),
    ok = emqtt:disconnect(C2),
    application:set_env(emqx, allow_anonymous, true).

retry_interval_test(_Config) ->
    {ok, Pub} = emqtt:start_link([{clean_start, true}, {retry_interval, 1}]),
    {ok, _} = emqtt:connect(Pub),

    CRef = counters:new(1, [atomics]),

    meck:new(emqtt_sock, [passthrough, no_history]),
    meck:expect(emqtt_sock, send, fun(_, _) -> counters:add(CRef, 1, 1), ok end),
    {ok, _} = emqtt:publish(Pub, nth(1, ?TOPICS), <<"qos 1">>, 1),

    timer:sleep(timer:seconds(2)),
    ?assertEqual(2, counters:get(CRef, 1)),
    
    meck:unload(emqtt_sock),
    ok = emqtt:disconnect(Pub).

will_message_test(_Config) ->
    {ok, C1} = emqtt:start_link([{clean_start, true},
                                          {will_topic, nth(3, ?TOPICS)},
                                          {will_payload, <<"client disconnected">>},
                                          {keepalive, 2}]),
    {ok, _} = emqtt:connect(C1),

    {ok, C2} = emqtt:start_link(),
    {ok, _} = emqtt:connect(C2),

    {ok, _, [2]} = emqtt:subscribe(C2, nth(3, ?TOPICS), 2),
    timer:sleep(10),
    ok = emqtt:stop(C1),
    timer:sleep(5),
    ?assertEqual(1, length(receive_messages(1))),
    ok = emqtt:disconnect(C2),
    ct:print("Will message test succeeded").

will_retain_message_test(_Config) ->
    Topic = nth(3, ?TOPICS),
    clean_retained(Topic),

    {ok, C1} = emqtt:start_link([{clean_start, true},
                                          {will_topic, Topic},
                                          {will_payload, <<"will_retain_message_test">>},
                                          {will_qos, ?QOS_1},
                                          {will_retain, true},
                                          {will_props, #{}},
                                          {keepalive, 2}]),
    {ok, _} = emqtt:connect(C1),

    {ok, C2} = emqtt:start_link(),
    {ok, _} = emqtt:connect(C2),
    {ok, _, [2]} = emqtt:subscribe(C2, Topic, 2),
    timer:sleep(5),
    [?assertMatch( #{qos := 1, retain := false, topic := Topic} ,Msg1) || Msg1 <- receive_messages(1)],
    ok = emqtt:disconnect(C2),

    {ok, C3} = emqtt:start_link(),
    {ok, _} = emqtt:connect(C3),
    {ok, _, [2]} = emqtt:subscribe(C3, Topic, 2),
    timer:sleep(5),
    [?assertMatch( #{qos := 1, retain := true, topic := Topic} ,Msg2) || Msg2 <- receive_messages(1)],
    ok = emqtt:disconnect(C3),

    ok = emqtt:stop(C1),
    clean_retained(Topic),
    ct:print("Will retain message test succeeded").

offline_message_queueing_test(_) ->
    {ok, C1} = emqtt:start_link([{clean_start, false}, {clientid, <<"c1">>}]),
    {ok, _} = emqtt:connect(C1),

    {ok, _, [2]} = emqtt:subscribe(C1, nth(6, ?WILD_TOPICS), 2),
    ok = emqtt:disconnect(C1),
    {ok, C2} = emqtt:start_link([{clean_start, true}, {clientid, <<"c2">>}]),
    {ok, _} = emqtt:connect(C2),

    ok = emqtt:publish(C2, nth(2, ?TOPICS), <<"qos 0">>, 0),
    {ok, _} = emqtt:publish(C2, nth(3, ?TOPICS), <<"qos 1">>, 1),
    {ok, _} = emqtt:publish(C2, nth(4, ?TOPICS), <<"qos 2">>, 2),
    timer:sleep(10),
    emqtt:disconnect(C2),
    {ok, C3} = emqtt:start_link([{clean_start, false}, {clientid, <<"c1">>}]),
    {ok, _} = emqtt:connect(C3),

    timer:sleep(10),
    emqtt:disconnect(C3),
    ?assertEqual(3, length(receive_messages(3))).

overlapping_subscriptions_test(_) ->
    {ok, C} = emqtt:start_link([]),
    {ok, _} = emqtt:connect(C),

    {ok, _, [2, 1]} = emqtt:subscribe(C, [{nth(7, ?WILD_TOPICS), 2},
                                                {nth(1, ?WILD_TOPICS), 1}]),
    timer:sleep(10),
    {ok, _} = emqtt:publish(C, nth(4, ?TOPICS), <<"overlapping topic filters">>, 2),
    timer:sleep(10),

    Num = length(receive_messages(2)),
    ?assert(lists:member(Num, [1, 2])),
    if
        Num == 1 ->
            ct:print("This server is publishing one message for all
                     matching overlapping subscriptions, not one for each.");
        Num == 2 ->
            ct:print("This server is publishing one message per each
                     matching overlapping subscription.");
        true -> ok
    end,
    emqtt:disconnect(C).

redelivery_on_reconnect_test(_) ->
    ct:print("Redelivery on reconnect test starting"),
    {ok, C1} = emqtt:start_link([{clean_start, false}, {clientid, <<"c">>}]),
    {ok, _} = emqtt:connect(C1),

    {ok, _, [2]} = emqtt:subscribe(C1, nth(7, ?WILD_TOPICS), 2),
    timer:sleep(10),
    ok = emqtt:pause(C1),
    {ok, _} = emqtt:publish(C1, nth(2, ?TOPICS), <<>>,
                                  [{qos, 1}, {retain, false}]),
    {ok, _} = emqtt:publish(C1, nth(4, ?TOPICS), <<>>,
                                  [{qos, 2}, {retain, false}]),
    timer:sleep(10),
    ok = emqtt:disconnect(C1),
    ?assertEqual(0, length(receive_messages(2))),
    {ok, C2} = emqtt:start_link([{clean_start, false}, {clientid, <<"c">>}]),
    {ok, _} = emqtt:connect(C2),

    timer:sleep(10),
    ok = emqtt:disconnect(C2),
    ?assertEqual(2, length(receive_messages(2))).

dollar_topics_test(_) ->
    ct:print("$ topics test starting"),
    {ok, C} = emqtt:start_link([{clean_start, true},
                                      {keepalive, 0}]),
    {ok, _} = emqtt:connect(C),

    {ok, _, [1]} = emqtt:subscribe(C, nth(6, ?WILD_TOPICS), 1),
    {ok, _} = emqtt:publish(C, << <<"$">>/binary, (nth(2, ?TOPICS))/binary>>,
                                  <<"test">>, [{qos, 1}, {retain, false}]),
    timer:sleep(10),
    ?assertEqual(0, length(receive_messages(1))),
    ok = emqtt:disconnect(C),
    ct:print("$ topics test succeeded").

basic_test_v5(_Config) ->
    basic_test([{proto_ver, v5}]).

retain_as_publish_test(_) ->
    Topic = nth(3, ?TOPICS),
    clean_retained(Topic),

    {ok, Pub} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(Pub),

    {ok, Sub1} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(Sub1),

    {ok, Sub2} = emqtt:start_link([{clean_start, true}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(Sub2),

    {ok, _} = emqtt:publish(Pub, Topic, #{}, <<"retain_as_publish_test">>, [{qos, ?QOS_1}, {retain, true}]),
    timer:sleep(10),

    {ok, _, [2]} = emqtt:subscribe(Sub1, Topic, 2),
    timer:sleep(5),
    [?assertMatch( #{qos := 1, retain := false, topic := Topic} ,Msg1) || Msg1 <- receive_messages(1)],
    ok = emqtt:disconnect(Sub1),


    {ok, _, [2]} = emqtt:subscribe(Sub2, Topic, [{qos, 2}, {rap, true}]),
    timer:sleep(5),
    [?assertMatch( #{qos := 1, retain := true, topic := Topic} ,Msg2) || Msg2 <- receive_messages(1)],
    ok = emqtt:disconnect(Sub2),

    ok = emqtt:disconnect(Pub),
    clean_retained(Topic).
