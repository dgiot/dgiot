%% @doc 刀 2 编解码单测：往返、边界、半包、畸形、UTF-8、大包。
-module(dgiot_broker_frame_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------- 变长长度 ----------------

remaining_length_roundtrip_test() ->
    [begin
         Bin = dgiot_broker_frame:encode_remaining_length(L),
         {ok, L, <<>>} = dgiot_broker_frame:decode_remaining_length(Bin)
     end || L <- [0, 1, 127, 128, 16383, 16384, 2097151, 2097152, 268435455]].

remaining_length_encoding_is_canonical_test() ->
    ?assertEqual(<<0>>, dgiot_broker_frame:encode_remaining_length(0)),
    ?assertEqual(<<127>>, dgiot_broker_frame:encode_remaining_length(127)),
    ?assertEqual(<<16#80, 1>>, dgiot_broker_frame:encode_remaining_length(128)),
    ?assertEqual(<<16#FF, 16#FF, 16#FF, 16#7F>>,
                 dgiot_broker_frame:encode_remaining_length(268435455)).

remaining_length_rejects_five_bytes_test() ->
    ?assertEqual({error, remaining_length_too_long},
                 dgiot_broker_frame:decode_remaining_length(
                   <<16#FF, 16#FF, 16#FF, 16#FF, 16#7F>>)).

remaining_length_partial_test() ->
    ?assertEqual({more, 1}, dgiot_broker_frame:decode_remaining_length(<<>>)),
    ?assertEqual({more, 1},
                 dgiot_broker_frame:decode_remaining_length(<<16#80>>)).

%% ---------------- CONNECT ----------------

connect_minimal_roundtrip_test() ->
    P = #{type => connect, clean_start => true, keepalive => 60,
          client_id => <<"edge-hub-c1">>},
    {ok, Decoded, <<>>} = dgiot_broker_frame:decode(
                            iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(connect, maps:get(type, Decoded)),
    ?assertEqual(4, maps:get(proto_level, Decoded)),
    ?assertEqual(<<"edge-hub-c1">>, maps:get(client_id, Decoded)),
    ?assertEqual(true, maps:get(clean_start, Decoded)),
    ?assertEqual(60, maps:get(keepalive, Decoded)),
    ?assertEqual(undefined, maps:get(will, Decoded)),
    ?assertEqual(undefined, maps:get(username, Decoded)).

connect_with_will_user_pass_test() ->
    P = #{type => connect, clean_start => false, keepalive => 30,
          client_id => <<"dev-1">>,
          will => #{topic => <<"dgiot/x/y/z/p/data">>,
                    payload => <<"offline">>, qos => 1, retain => true},
          username => <<"admin">>, password => <<"secret">>},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(false, maps:get(clean_start, D)),
    ?assertEqual(<<"admin">>, maps:get(username, D)),
    ?assertEqual(<<"secret">>, maps:get(password, D)),
    Will = maps:get(will, D),
    ?assertEqual(<<"dgiot/x/y/z/p/data">>, maps:get(topic, Will)),
    ?assertEqual(1, maps:get(qos, Will)),
    ?assertEqual(true, maps:get(retain, Will)).

%% ---------------- PUBLISH ----------------

publish_qos0_roundtrip_test() ->
    P = #{type => publish, qos => 0, retain => false, dup => false,
          topic => <<"dgiot/siteA/gw1/dev1/pt1/data">>,
          payload => <<"{\"v\":21.5}">>},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(publish, maps:get(type, D)),
    ?assertEqual(0, maps:get(qos, D)),
    ?assertEqual(undefined, maps:get(packet_id, D)),
    ?assertEqual(maps:get(topic, P), maps:get(topic, D)),
    ?assertEqual(maps:get(payload, P), maps:get(payload, D)).

publish_qos1_carries_packet_id_test() ->
    P = #{type => publish, qos => 1, packet_id => 4242,
          topic => <<"t">>, payload => <<"x">>},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(1, maps:get(qos, D)),
    ?assertEqual(4242, maps:get(packet_id, D)).

publish_utf8_topic_test() ->
    Topic = <<"dgiot/园区A/网关1/设备-1/测点/data"/utf8>>,
    P = #{type => publish, qos => 0, topic => Topic, payload => <<"ok">>},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(Topic, maps:get(topic, D)).

publish_large_payload_test() ->
    Payload = binary:copy(<<"A">>, 300000),
    P = #{type => publish, qos => 0, topic => <<"big">>, payload => Payload},
    Bin = iolist_to_binary(dgiot_broker_frame:encode(P)),
    {ok, D, <<>>} = dgiot_broker_frame:decode(Bin),
    ?assertEqual(byte_size(Payload), byte_size(maps:get(payload, D))).

%% ---------------- 订阅族 ----------------

subscribe_roundtrip_test() ->
    P = #{type => subscribe, packet_id => 7,
          topics => [{<<"dgiot/#">>, 1}, {<<"$dg/device/+/+/debug">>, 0}]},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(7, maps:get(packet_id, D)),
    ?assertEqual([{<<"dgiot/#">>, 1}, {<<"$dg/device/+/+/debug">>, 0}],
                 maps:get(topics, D)).

suback_unsuback_roundtrip_test() ->
    Sa = #{type => suback, packet_id => 9, return_codes => [1, 128]},
    {ok, D1, <<>>} = dgiot_broker_frame:decode(
                       iolist_to_binary(dgiot_broker_frame:encode(Sa))),
    ?assertEqual([1, 128], maps:get(return_codes, D1)),
    Ua = #{type => unsuback, packet_id => 11},
    {ok, D2, <<>>} = dgiot_broker_frame:decode(
                       iolist_to_binary(dgiot_broker_frame:encode(Ua))),
    ?assertEqual(11, maps:get(packet_id, D2)).

subscribe_without_topics_is_error_test() ->
    %% 手工构造：SUBSCRIBE 带 packet id 但无主题
    Bin = <<16#82, 2, 0, 1>>,
    ?assertMatch({error, empty_subscribe}, dgiot_broker_frame:decode(Bin)).

%% ---------------- 控制包 ----------------

control_packets_test() ->
    lists:foreach(
      fun(Type) ->
          Bin = iolist_to_binary(dgiot_broker_frame:encode(#{type => Type})),
          {ok, D, <<>>} = dgiot_broker_frame:decode(Bin),
          ?assertEqual(Type, maps:get(type, D))
      end, [pingreq, pingresp, disconnect]).

connack_roundtrip_test() ->
    P = #{type => connack, session_present => true, return_code => 0},
    {ok, D, <<>>} = dgiot_broker_frame:decode(
                      iolist_to_binary(dgiot_broker_frame:encode(P))),
    ?assertEqual(true, maps:get(session_present, D)),
    ?assertEqual(0, maps:get(return_code, D)).

%% ---------------- 流式（半包）与畸形 ----------------

partial_packet_returns_more_test() ->
    Full = iolist_to_binary(dgiot_broker_frame:encode(
                              #{type => publish, qos => 0, topic => <<"abc">>,
                                payload => <<"hello world">>})),
    %% 逐字节喂：除最后一字节外都应 {more, _}，绝不解出半个包
    Lists = [binary:part(Full, 0, N) || N <- lists:seq(1, byte_size(Full) - 1)],
    lists:foreach(fun(Prefix) ->
                          ?assertMatch({more, _},
                                       dgiot_broker_frame:decode(Prefix))
                  end, Lists),
    ?assertMatch({ok, _, <<>>}, dgiot_broker_frame:decode(Full)).

trailing_bytes_are_returned_as_rest_test() ->
    One = iolist_to_binary(dgiot_broker_frame:encode(#{type => pingreq})),
    Two = iolist_to_binary(dgiot_broker_frame:encode(#{type => pingresp})),
    {ok, P1, Rest} = dgiot_broker_frame:decode(<<One/binary, Two/binary>>),
    ?assertEqual(pingreq, maps:get(type, P1)),
    ?assertEqual(Two, Rest).

unknown_type_is_error_test() ->
    ?assertMatch({error, {unknown_packet_type, 0}},
                 dgiot_broker_frame:decode(<<0:8, 0:8>>)),
    ?assertMatch({error, {unknown_packet_type, 15}},
                 dgiot_broker_frame:decode(<<16#F0, 0>>)).

bad_publish_qos_is_error_test() ->
    %% QoS 位 = 3 是协议非法值
    ?assertMatch({error, {bad_publish_qos, 3}},
                 dgiot_broker_frame:decode(<<16#36, 2, 0, 1>>)).

bad_connect_is_error_test() ->
    ?assertMatch({error, _}, dgiot_broker_frame:decode(<<16#10, 2, 0, 0>>)).

packet_type_peek_test() ->
    ?assertEqual(connect, dgiot_broker_frame:packet_type(<<16#10, 0>>)),
    ?assertEqual(publish, dgiot_broker_frame:packet_type(<<16#30, 0>>)),
    ?assertMatch({error, too_short}, dgiot_broker_frame:packet_type(<<>>)).
