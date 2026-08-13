-module(dgiot_hub_worker).
-export([start/0]).

%% Simple hub worker: listen to ALL edge telemetry, count per device, emit alerts
%% Hot-deployable via: erlc → copy beam → restart

start() ->
    EMQX_LIBS = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/emqtt-*/ebin")]),
    JSX_LIBS = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/jsx-*/ebin")]),
    code:add_path(EMQX_LIBS),
    code:add_path(JSX_LIBS),

    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "hub_worker"}]),
    {ok, _} = emqtt:connect(C),
    emqtt:subscribe(C, <<"dgiot/#">>, 1),

    io:format("~n=== Hub Worker Started ===~n"),
    io:format("Listening: dgiot/#~n~n"),
    loop(C, #{}).

loop(C, Counts) ->
    receive
        {publish, #{topic := Topic, payload := Payload}} ->
            case binary:split(Topic, <<"/">>, [global]) of
                [<<"dgiot">>, _Tenant, _Gw, _Ch, DevId, PointId | _] ->
                    Key = {DevId, PointId},
                    N = maps:get(Key, Counts, 0) + 1,
                    NewCounts = Counts#{Key => N},

                    %% Parse value
                    Val = case catch jsx:decode(Payload, [return_maps]) of
                        #{<<"value">> := V} when is_number(V) -> V;
                        _ -> 0
                    end,

                    %% Alert on Nth message per device
                    if N rem 50 =:= 0 ->
                        io:format("[~s] ~s/~s = ~p (#~B)~n",
                            [format_time(), DevId, PointId, Val, N]);
                       true -> ok
                    end,

                    %% Check thresholds
                    if
                        PointId =:= <<"oil_pressure">>, Val > 3.0 ->
                            Alert = jsx:encode(#{type => alert, device => DevId,
                                point => <<"oil_pressure">>, value => Val,
                                threshold => 3.0, msg => <<"oil_pressure high">>}),
                            emqtt:publish(C, <<"dgiot/default/gw_131/ch_edge_hub/alarms">>, Alert, [{qos, 1}]),
                            io:format("[ALERT] ~s oil_pressure=~p > 3.0~n", [DevId, Val]);
                        PointId =:= <<"temperature">>, Val > 55.0 ->
                            Alert = jsx:encode(#{type => alert, device => DevId,
                                point => <<"temperature">>, value => Val,
                                threshold => 55, msg => <<"temperature high">>}),
                            emqtt:publish(C, <<"dgiot/default/gw_131/ch_edge_hub/alarms">>, Alert, [{qos, 1}]),
                            io:format("[ALERT] ~s temperature=~p > 55~n", [DevId, Val]);
                        true -> ok
                    end,

                    loop(C, NewCounts);
                _ ->
                    loop(C, Counts)
            end
    end.

format_time() ->
    {H,M,S} = time(),
    io_lib:format("~2..0b:~2..0b:~2..0b", [H,M,S]).
