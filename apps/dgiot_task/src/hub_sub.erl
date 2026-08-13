-module(hub_sub).
-export([start/0]).

start() ->
    EMQX_LIB = hd([D || D <- filelib:wildcard("/data/dgiot/_build/emqx/rel/emqx/lib/emqtt-*/ebin")]),
    code:add_path(EMQX_LIB),
    {ok, C} = emqtt:start_link([{host, "127.0.0.1"}, {port, 1883}, {clientid, "hub_sub"}]),
    {ok, _} = emqtt:connect(C),
    emqtt:subscribe(C, <<"dgiot/#">>, 1),
    emqtt:subscribe(C, <<"$dg/thing/+/+/properties/report">>, 1),
    emqtt:subscribe(C, <<"$dg/things/+/shadow/reported">>, 1),
    io:format("[HUB] Listening on EMQX :1883...~n"),
    loop(C, 0, erlang:system_time(second)).

loop(C, N, Start) ->
    receive
        {publish, #{topic := T, payload := P}} ->
            case binary_to_list(P) of
                "<" ++ _ -> ok;
                PL -> io:format("[EDGE] ~s~n  ~s~n", [T, string:slice(PL, 0, 100)])
            end,
            loop(C, N+1, Start)
    after 1000 ->
        case erlang:system_time(second) - Start >= 30 of
            true  -> io:format("[HUB] Done: ~B msgs in 30s~n", [N]), emqtt:disconnect(C);
            false -> loop(C, N, Start)
        end
    end.
