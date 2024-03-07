-module(route_guide_svr).

-behavior(routeguide_route_guide_bhvr).

-compile(export_all).
-compile(nowarn_export_all).

-define(LOG(Fmt, Args), io:format(standard_error, "[Svr] " ++ Fmt, Args)).

%%--------------------------------------------------------------------
%% Callbacks

get_feature(Request, _Md) ->
    ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Request]),
    {ok, #{}, _Md}.

list_features(Stream, _Md) ->
    {eos, [Request], NStream} = grpc_stream:recv(Stream),
    ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Request]),

    grpc_stream:reply(Stream, [#{name => "City1", location => #{latitude => 1, longitude => 1}}]),
    grpc_stream:reply(Stream, [#{name => "City2", location => #{latitude => 2, longitude => 2}}]),
    grpc_stream:reply(Stream, [#{name => "City3", location => #{latitude => 3, longitude => 3}}]),
    {ok, NStream}.

record_route(Stream, _Md) ->
    LoopRecv = fun _Lp(St, Acc) ->
        case grpc_stream:recv(St) of
            {more, Reqs, NSt} ->
                ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                _Lp(NSt, Acc ++ Reqs);
            {eos, Reqs, NSt} ->
                ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                {NSt, Acc ++ Reqs}
        end
    end,
    {NStream, Points} = LoopRecv(Stream, []),
    grpc_stream:reply(NStream, #{point_count => length(Points)}),
    {ok, NStream}.

route_chat(Stream, _Md) ->
    grpc_stream:reply(Stream, [#{name => "City1", location => #{latitude => 1, longitude => 1}}]),
    grpc_stream:reply(Stream, [#{name => "City2", location => #{latitude => 2, longitude => 2}}]),
    LoopRecv = fun _Lp(St) ->
        case grpc_stream:recv(St) of
            {more, Reqs, NSt} ->
                ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                _Lp(NSt);
            {eos, Reqs, NSt} ->
                ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Reqs]),
                NSt
        end
    end,
    NStream = LoopRecv(Stream),
    grpc_stream:reply(NStream, [#{name => "City3", location => #{latitude => 3, longitude => 3}}]),
    {ok, NStream}.
