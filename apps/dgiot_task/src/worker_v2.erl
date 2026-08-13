-module(worker_v2).
-export([start/0, loop/2]).

%% Hub Worker v2: telemetry counting + threshold alerts + alarm dedup
%% Alerts deduplicated: same device+point alert only fires once per 60s

start() ->
    EMQX = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/emqtt-*/ebin")]),
    JSX = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/jsx-*/ebin")]),
    code:add_path(EMQX), code:add_path(JSX),

    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "worker_v2"}]),
    {ok, _} = emqtt:connect(C),
    emqtt:subscribe(C, <<"dgiot/#">>, 1),

    io:format("=== Hub Worker v2 Started ===~n"),
    io:format("Listening: dgiot/# (dedup 60s)~n~n"),
    loop(C, #{}).

loop(C, State) ->
    receive
        {publish, #{topic := Topic, payload := Payload}} ->
            case binary:split(Topic, <<"/">>, [global]) of
                [<<"dgiot">>, _T, _G, _Ch, DevId, PointId | _] ->
                    Now = erlang:system_time(second),
                    Key = {DevId, PointId},

                    %% Count
                    Counts = maps:get(counts, State, #{}),
                    N = maps:get(Key, Counts, 0) + 1,

                    %% Parse value
                    Val = case catch jsx:decode(Payload, [return_maps]) of
                        #{<<"value">> := V} when is_number(V) -> V;
                        _ -> 0.0
                    end,

                    %% Threshold check with dedup (60s window)
                    Alerts = maps:get(alerts, State, #{}),
                    {Fired, NewAlerts} = check_alert(C, DevId, PointId, Val, Alerts, Now),

                    %% Progress log every 200 msgs per device-point
                    case N rem 200 =:= 0 of
                        true -> io:format("[~s] ~s/~s: ~B msgs (last=~p)~n",
                            [fmt_time(), DevId, PointId, N, Val]);
                        false -> ok
                    end,

                    loop(C, State#{counts => Counts#{Key => N},
                                  alerts => NewAlerts});
                _ ->
                    loop(C, State)
            end;
        {stats} ->
            Counts = maps:get(counts, State, #{}),
            io:format("[STATS] ~B device-point pairs tracked~n", [map_size(Counts)]),
            loop(C, State);
        _ ->
            loop(C, State)
    after 30000 ->
        %% Periodic stats
        Counts = maps:get(counts, State, #{}),
        case map_size(Counts) > 0 of
            true ->
                Total = lists:sum(maps:values(Counts)),
                io:format("[STATS] ~B pairs, ~B total msgs~n", [map_size(Counts), Total]);
            false -> ok
        end,
        loop(C, State)
    end.

check_alert(C, DevId, PointId, Val, Alerts, Now) ->
    Threshold = case PointId of
        <<"oil_pressure">> -> 3.0;
        <<"temperature">> -> 55.0;
        <<"vibration">> -> 5.0;
        <<"motor_current">> -> 30.0;
        _ -> 999.9  %% no threshold
    end,

    Key = {DevId, PointId},
    LastAlert = maps:get(Key, Alerts, 0),

    case Val > Threshold andalso (Now - LastAlert) > 60 of
        true ->
            Alert = jsx:encode(#{type => alert, device => DevId, point => PointId,
                value => Val, threshold => Threshold,
                ts => Now}),
            emqtt:publish(C, <<"dgiot/default/gw_131/ch_edge_hub/alarms">>, Alert, [{qos, 1}]),
            io:format("[ALERT] ~s ~s=~p > ~p~n", [fmt_time(), DevId, Val, Threshold]),
            {true, Alerts#{Key => Now}};
        false ->
            {false, Alerts}
    end.

fmt_time() ->
    {H,M,S} = time(),
    io_lib:format("~2..0b:~2..0b:~2..0b", [H,M,S]).
