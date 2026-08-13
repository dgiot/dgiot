%% MQTT pub/sub test inside Kylin-DMZ
%% Usage: cd /tmp; erlc /mnt/d/ai/kylin/scripts/mqtt_test.erl && erl -noshell -run mqtt_test test -run init stop
-module(mqtt_test).
-export([test/0]).

test() ->
    %% 1. Publish test data in hub format
    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "erlang_test"}]),
    {ok, _} = emqtt:connect(C),
    io:format("[PUB] connected~n"),

    %% Subscribe to receive echo
    emqtt:subscribe(C, <<"dgiot/#">>, 1),
    io:format("[SUB] dgiot/#~n"),

    %% Publish test
    Topic = <<"$dg/thing/prod_oil_well_pump/DEVICE_D-001/properties/report">>,
    Payload = <<"{\"oil_pressure\":{\"value\":2.35},\"temperature\":{\"value\":48.2}}">>,
    emqtt:publish(C, Topic, Payload, [{qos, 1}]),
    io:format("[PUB] ~s -> ~s~n", [Topic, Payload]),

    timer:sleep(1000),
    emqtt:disconnect(C),
    io:format("[OK] done~n").
