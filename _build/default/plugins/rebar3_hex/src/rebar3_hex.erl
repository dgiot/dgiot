-module(rebar3_hex).

-export([ init/1
        , gather_opts/2
        , get_required/2
        , task_args/1
        , repo_opt/0
        , help_opt/0
        ]).

init(State) ->
    lists:foldl(fun provider_init/2, {ok, State}, [rebar3_hex_user,
                                                   rebar3_hex_cut,
                                                   rebar3_hex_key,
                                                   rebar3_hex_owner,
                                                   rebar3_hex_repo,
                                                   rebar3_hex_docs,
                                                   rebar3_hex_search,
                                                   rebar3_hex_revert,
                                                   rebar3_hex_retire,
                                                   rebar3_hex_publish]).

provider_init(Module, {ok, State}) ->
    Module:init(State).

gather_opts(Targets, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    lists:foldl(fun(T, Acc) ->
                        case proplists:get_value(T, Args, undefined) of
                            undefined ->
                                Acc;
                            V ->
                                maps:put(T, V, Acc)
                        end
                end, #{}, Targets).

get_required(Key, Args) ->
    case proplists:get_value(Key, Args) of
        undefined ->
            {error, {required, Key}};
        Value ->
            Value
    end.

task_args(State) ->
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    case proplists:get_value(task, Opts, undefined) of
        undefined ->
            {undefined, Opts};
        Task ->
            {Task, proplists:delete(task, Opts)}
    end.

repo_opt() ->
  {repo, $r, "repo", string, "Repository to use for this command."}.

help_opt() ->
  {help, $h, "help", undefined, "Help"}.
