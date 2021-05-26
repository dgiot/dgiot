-module(observer_cli_escriptize).

-export([main/1]).

%% @doc escript main
-spec main(list()) -> 'ok'.

-define(BEAM_MODS, [
    recon,
    recon_alloc,
    recon_lib,
    recon_trace,
    observer_cli,
    observer_cli_ets,
    observer_cli_lib,
    observer_cli_process,
    observer_cli_application,
    observer_cli_help,
    observer_cli_mnesia,
    observer_cli_store,
    observer_cli_escriptize,
    observer_cli_inet,
    observer_cli_port,
    observer_cli_system
]).

main([TargetNode]) ->
    run(TargetNode, undefined, 1500);
main([TargetNode, Cookie, Interval]) ->
    CookieAtom = list_to_atom(Cookie),
    IntervalInt = list_to_integer(Interval),
    run(TargetNode, CookieAtom, IntervalInt);
main(_Options) ->
    io:format("Usage: observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]~n").

run(TargetNode, Cookie, Interval) ->
    {TargetNodeAtom, NameOpt} = resolve_target_name(TargetNode),
    LocalNode = random_local_node_name(),
    MyName =
        case NameOpt of
            shortnames -> list_to_atom(LocalNode);
            longnames -> list_to_atom(LocalNode ++ "@127.0.0.1")
        end,
    {ok, _} = net_kernel:start([MyName, NameOpt]),
    Start = fun() ->
        Options = [{cookie, Cookie}, {interval, Interval}],
        observer_cli:start(TargetNodeAtom, Options)
    end,
    case Start() of
        {badrpc, _} ->
            remote_load(TargetNodeAtom),
            io:format("~p~n", [Start()]);
        _ ->
            ok
    end.

remote_load(Node) ->
    [
        begin
            recon:remote_load([Node], Mod)
        end
        || Mod <- ?BEAM_MODS
    ].

random_local_node_name() ->
    {_, {H, M, S}} = calendar:local_time(),
    lists:flatten(io_lib:format("observer_cli_~2.2.0p_~2.2.0p_~2.2.0p", [H, M, S])).

resolve_target_name(TargetNode) ->
    case string:tokens(TargetNode, "@") of
        [_Name, Host] ->
            Node = list_to_atom(TargetNode),
            case string:tokens(Host, ".") of
                [Host] -> {Node, shortnames};
                [_ | _] -> {Node, longnames}
            end;
        [Name] ->
            %% only a name without host given, assume shortname
            {ok, Host} = inet:gethostname(),
            {list_to_atom(Name ++ "@" ++ Host), shortnames}
    end.
