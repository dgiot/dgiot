-module(rebar3_hex_repo).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, repo).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex repo auth myrepo --key 1234"},
                                {short_desc, "Add, remove or list configured repositories and their auth keys"},
                                {desc, ""},
                                {opts, [{subcmd, undefined, undefined, string, "Repo task to run"},
                                        {repo, undefined, undefined, string, "Name of a repository"},
                                        {key, $k, "key", string, "Authentication key for repository"}]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(subcmd, Args, undefined) of
        "generate" ->
            case proplists:get_value(repo, Args, undefined) of
                undefined ->
                    ?PRV_ERROR(no_repo);
                Repo ->
                    generate(list_to_binary(Repo), State)
            end;
        "auth" ->
            case proplists:get_value(repo, Args, undefined) of
                undefined ->
                    ?PRV_ERROR(no_repo);
                Repo ->
                    case proplists:get_value(key, Args, undefined) of
                        undefined ->
                            ?PRV_ERROR(auth_no_key);
                        Key ->
                            auth(list_to_binary(Repo), list_to_binary(Key), State),
                            {ok, State}
                    end
            end;
        "list" ->
            list_repos(State);
        Command ->
            ?PRV_ERROR({bad_command, Command})
    end.

-spec format_error(any()) -> iolist().
format_error(no_repo) ->
    "Authenticate and generate commands require repository name as argument";
format_error(auth_no_key) ->
    "Repo authenticate command requires key";
format_error({bad_command, Command}) ->
    io_lib:format("Unknown repo command ~ts", [Command]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

auth(Repo, Key, State) ->
    Config = rebar_hex_repos:auth_config(State),
    RepoConfig = maps:get(Repo, Config, #{}),
    RepoConfig1 = RepoConfig#{auth_key => Key},
    rebar_hex_repos:update_auth_config(#{Repo => RepoConfig1}, State).

generate(RepoName, State) ->
    {ok, RepoConfig} = rebar_hex_repos:get_repo_config(RepoName, State),

    RepoName1 = case binary:split(RepoName, <<":">>) of
                   [_Parent, Org] ->
                       Org;
                   Public ->
                       Public
               end,

    Permissions = [#{<<"domain">> => <<"repository">>,
                     <<"resource">> => RepoName}],
    Name = <<RepoName1/binary, "-repository">>,

    {ok, HexConfig} = rebar3_hex_config:hex_config_write(RepoConfig),
    case hex_api_key:add(HexConfig, Name, Permissions) of
        {ok, {201, _Headers, #{<<"secret">> := Secret}}} ->
            rebar3_hex_io:say("Generated key: ~ts", [Secret]),
            {ok, State};
        {ok, {Status, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({error, Status, Message});
        {error, Reason} ->
            ?PRV_ERROR({error, Reason})
    end.

list_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Headers = ["Name", "URL", "Public Key", "Auth Key"],
    Rows = lists:map(fun(Repo) ->
                             #{name := Name,
                               api_organization := Org,
                               repo_url := Url,
                               read_key := ReadKey,
                               repo_public_key := PubKey} = Repo,

                             AuthKey = maps:get(auth_key, Repo, ReadKey),
                             [binary_to_list(Name),
                              maybe_org_url(Org, Url),
                              printable_public_key(PubKey),
                              binary_to_list(AuthKey)]
                     end, Repos),
    rebar3_hex_results:print_table([Headers] ++ Rows),
    {ok, State}.

printable_public_key(PubKey) ->
    [Pem] = public_key:pem_decode(PubKey),
    Public =  public_key:pem_entry_decode(Pem),
    Hash = crypto:hash(sha256, public_key:ssh_encode(Public, ssh2_pubkey)),
    Encoded = string:substr(base64:encode_to_string(Hash), 1, 43),
    "SHA256:" ++ Encoded.

maybe_org_url(undefined, Url) -> binary_to_list(Url);
maybe_org_url(Org, Url) -> binary_to_list(Url) ++ "/repos/" ++ binary_to_list(Org).
