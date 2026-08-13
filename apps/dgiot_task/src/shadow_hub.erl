-module(shadow_hub).
-export([start/0, loop/1]).

%% Simple shadow handler: subscribe to MQTT shadow topics and process
%% This connects to EMQX as an MQTT client (not as an internal hook)
%% and acts as the shadow service - monitoring reported, pushing desired

start() ->
    EMQX_LIBS = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/emqtt-*/ebin")]),
    code:add_path(EMQX_LIBS),
    code:add_paths(filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/jsx-*/ebin")),

    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "shadow_hub"}]),
    {ok, _} = emqtt:connect(C),

    %% Subscribe to all shadow topics
    emqtt:subscribe(C, <<"$dg/things/+/shadow/reported">>, 1),
    emqtt:subscribe(C, <<"$dg/things/+/shadow/get">>, 1),

    io:format("~n========================================~n"),
    io:format("  Shadow Hub - Listening on EMQX~n"),
    io:format("  Topics: $dg/things/+/shadow/*~n"),
    io:format("========================================~n~n"),

    loop(C).

loop(C) ->
    receive
        {publish, #{topic := Topic, payload := Payload}} ->
            Parts = binary:split(Topic, <<"/">>, [global]),
            case Parts of
                [<<"$dg">>, <<"things">>, DeviceId, <<"shadow">>, Action] ->
                    handle_shadow(C, DeviceId, Action, Payload);
                _ -> ok
            end,
            loop(C);
        _ ->
            loop(C)
    end.

handle_shadow(C, DeviceId, <<"reported">>, Payload) ->
    Data = jsx:decode(Payload, [return_maps]),
    Reported = maps:get(<<"reported">>, Data, #{}),
    io:format("[REPORTED] ~s:~n", [DeviceId]),
    maps:fold(fun(K, V, _) ->
        io:format("  ~s = ~p~n", [K, V])
    end, ok, Reported),

    %% Compute and send delta if needed
    Desired = lookup_desired(DeviceId),
    Delta = maps:fold(fun(K, D, Acc) ->
        case maps:find(atom_to_binary(K, utf8), Reported) of
            {ok, R} when D =/= R ->
                Acc#{K => #{desired => D, reported => R}};
            _ -> Acc
        end
    end, #{}, maps:from_list(Desired)),
    case map_size(Delta) of
        0 -> io:format("  -> Synced (no delta)~n");
        _ ->
            DeltaPayload = jsx:encode(#{delta => Delta, version => 1}),
            emqtt:publish(C, <<"$dg/things/", DeviceId/binary, "/shadow/delta">>,
                         DeltaPayload, [{qos, 1}]),
            io:format("  -> Delta pushed~n")
    end;

handle_shadow(C, DeviceId, <<"get">>, _Payload) ->
    Desired = lookup_desired(DeviceId),
    DesiredPayload = jsx:encode(#{desired => maps:from_list(Desired), version => 1}),
    emqtt:publish(C, <<"$dg/things/", DeviceId/binary, "/shadow/desired">>,
                 DesiredPayload, [{qos, 1}]),
    io:format("[GET] ~s -> desired pushed~n", [DeviceId]);

handle_shadow(_C, DeviceId, Action, _P) ->
    io:format("[SHADOW] ~s / ~s (ignored)~n", [DeviceId, Action]).

%% Simulated desired state (would come from database in production)
lookup_desired(<<"DEVICE_D-001">>) ->
    [{oil_pressure, 2.5}, {temperature, 50}, {collect_interval, 10}];
lookup_desired(_) ->
    [{oil_pressure, 2.5}].
