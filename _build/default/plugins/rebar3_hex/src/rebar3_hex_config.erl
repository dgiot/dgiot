-module(rebar3_hex_config).

-export([ api_key_name/1
        , api_key_name/2
        , repos_key_name/0
        , org_key_name/2
        , parent_repos/1
        , hex_config/2
        , hex_config_write/1
        , hex_config_read/1
        , repo/1
        , update_auth_config/2
        ]).

-include("rebar3_hex.hrl").

-spec api_key_name(binary()) -> binary().
api_key_name(Key) ->
    Prefix = key_name_prefix(Key),
    key_name(Prefix, <<"-api">>).

-spec api_key_name(binary(), binary()) -> binary().
api_key_name(Key, Suffix) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-api-">>, Suffix).

-spec repos_key_name() -> binary().
repos_key_name() ->
     key_name(hostname(), <<"-repositories">>).

-spec org_key_name(binary(), binary()) -> binary().
org_key_name(Key, Org) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-repository-">>, Org).

-spec hostname() -> binary().
hostname() ->
    {ok, Name} = inet:gethostname(),
    list_to_binary(Name).

key_name(Prefix, Suffix) ->
    <<Prefix/binary, Suffix/binary>>.

key_name(Prefix, Interfix, Suffix) ->
    <<Prefix/binary, Interfix/binary, Suffix/binary>>.

key_name_prefix(undefined) -> hostname();
key_name_prefix(Key) -> Key.

update_auth_config(Config, State) ->
    rebar_hex_repos:update_auth_config(Config, State).

repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            Res = [R || R <- Repos, maps:get(name, R) =/= ?DEFAULT_HEX_REPO],
            case Res of
                [] ->
                    case rebar_hex_repos:get_repo_config(?DEFAULT_HEX_REPO, Repos) of
                        {ok, Repo} ->
                            {ok, set_http_adapter(Repo)};
                        _ ->
                            {error, no_repo_in_state}
                    end;
                [_Repo|_Rest] ->
                    {error, {required, repo}}
            end;
        RepoName ->
            repo(State, RepoName)
    end.

repo(State, RepoName) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    BinName = rebar_utils:to_binary(RepoName),
    MaybeFound1 = get_repo(BinName, Repos),
    MaybeParentRepo = <<"hexpm:">>,
    MaybeFound2 =  get_repo(<<MaybeParentRepo/binary, BinName/binary>>, Repos),
    case {MaybeFound1, MaybeFound2} of
        {{ok, Repo1}, undefined} ->
            {ok, set_http_adapter(merge_with_env(Repo1))};
        {undefined, {ok, Repo2}} ->
            {ok, set_http_adapter(merge_with_env(Repo2))};
        {undefined, undefined} ->
            {error, {not_valid_repo, RepoName}}
    end.


-define( ENV_VARS
       , [ {"HEX_API_KEY", {api_key, {string, undefined}}}
         , {"HEX_API_URL", {api_url, {string, undefined}}}
         , {"HEX_UNSAFE_REGISTRY", {repo_verify, {boolean, false}}}
         , {"HEX_NO_VERIFY_REPO_ORIGIN", {repo_verify_origin, {boolean, true}}}
         ]
       ).

merge_with_env(Repo) ->
    lists:foldl(fun({EnvName, {Key, _} = Default}, Acc) ->
                        Val = maybe_env_val(EnvName, Default),
                        maybe_put_key(Key, Val, Acc)
                end, Repo, ?ENV_VARS).

maybe_put_key(_Key, undefined, Repo) ->
    Repo;
maybe_put_key(Key, Val, Repo) ->
    case maps:get(Key, Repo, undefined) of
        Val ->
            Repo;
        _ ->
            Repo#{Key => Val}
    end.

maybe_env_val(K, {_, {Type, Default}}) ->
    case {os:getenv(K), {Type, Default}} of
        {false, {_, Default}} ->
            Default;
        {"", {_, Default}} ->
            Default;
        {Val, {boolean, _}} ->
            to_bool(string:to_lower(Val));
        {Val, {string, _}} ->
          rebar_utils:to_binary(Val)
    end.

set_http_adapter(Repo) ->
    Repo#{http_adapter => {hex_http_httpc, #{profile => rebar}}}.

to_bool("0") -> false;
to_bool("false") -> false;
to_bool(_) -> true.

parent_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Fun = fun(#{name := Name} = Repo, Acc) ->
                  [Parent|_] = rebar3_hex_io:str_split(Name, <<":">>),
                  case maps:is_key(Parent, Acc) of
                    true ->
                        Acc;
                    false ->
                        maps:put(name, Repo, Acc)
                  end
          end,
    Map = lists:foldl(Fun, #{}, Repos),
    {ok, maps:values(Map)}.

get_repo(BinaryName, Repos) ->
    try rebar_hex_repos:get_repo_config(BinaryName, Repos) of
        Name ->
            Name
    catch
        {error,{rebar_hex_repos,{repo_not_found,BinaryName}}} -> undefined
    end.

hex_config(Repo, read) ->
    hex_config_read(Repo);
hex_config(Repo, write) ->
    hex_config_write(Repo).

hex_config_write(#{api_key := Key} = HexConfig) when is_binary(Key) ->
    {ok, set_http_adapter(HexConfig)};
hex_config_write(#{write_key := undefined}) ->
    {error, no_write_key};
hex_config_write(#{api_key := undefined, write_key := WriteKey, username := Username} = HexConfig) ->
    DecryptedWriteKey = rebar3_hex_user:decrypt_write_key(Username, WriteKey),
    {ok, set_http_adapter(HexConfig#{api_key => DecryptedWriteKey})};
hex_config_write(_) ->
    {error, no_write_key}.

hex_config_read(#{read_key := ReadKey} = HexConfig) ->
    {ok, set_http_adapter(HexConfig#{api_key => ReadKey})};
hex_config_read(_Config) ->
    {error, no_read_key}.
