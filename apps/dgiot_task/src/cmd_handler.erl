-module(cmd_handler).
-export([start/0, loop/1]).

%% Hub → Device command handler
%% Listens for command requests, forwards to devices via MQTT

start() ->
    EMQX = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/emqtt-*/ebin")]),
    JSX = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/jsx-*/ebin")]),
    code:add_path(EMQX), code:add_path(JSX),

    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "cmd_handler"}]),
    {ok, _} = emqtt:connect(C),

    %% Listen for command requests from cloud/frontend
    emqtt:subscribe(C, <<"dgiot/default/gw_131/+/+/command">>, 1),
    emqtt:subscribe(C, <<"$dg/command/+/+">>, 1),

    io:format("~n=== Command Handler Started ===~n"),
    io:format("Listening: dgiot/.../command + $dg/command~n~n"),
    loop(C).

loop(C) ->
    receive
        {publish, #{topic := Topic, payload := Payload}} ->
            case binary:split(Topic, <<"/">>, [global]) of
                [<<"$dg">>, <<"command">>, DeviceId, Cmd] ->
                    %% Forward to device
                    DevTopic = <<"$dg/things/", DeviceId/binary, "/command">>,
                    CmdPayload = jsx:encode(#{command => Cmd, ts => erlang:system_time(second)}),
                    emqtt:publish(C, DevTopic, CmdPayload, [{qos, 1}]),
                    io:format("[CMD] ~s → ~s: ~s~n", [format_time(), DeviceId, Cmd]);

                [<<"dgiot">>, _Tenant, _Gw, Ch, DevId, <<"command">>] ->
                    DevTopic = <<"dgiot/default/gw_131/", Ch/binary, "/", DevId/binary, "/command_resp">>,
                    CmdPayload = Payload,
                    emqtt:publish(C, DevTopic, CmdPayload, [{qos, 1}]),
                    io:format("[CMD] ~s → ~s~n", [format_time(), DevId]);

                _ -> ok
            end,
            loop(C);
        _ -> loop(C)
    end.

format_time() -> {H,M,S}=time(), io_lib:format("~2..0b:~2..0b:~2..0b",[H,M,S]).
