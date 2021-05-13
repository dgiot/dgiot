-module(rebar3_hex_docs).

-export([init/1,
         do/1,
         publish/3,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, docs).
-define(DEPS, [{default, lock}]).

-define(DEFAULT_DOC_DIR, "doc").

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
                                {example, "rebar3 hex docs"},
                                {short_desc, "Publish documentation for the current project and version"},
                                {desc, ""},
                                {opts, [{revert, undefined, "revert", string, "Revert given version."},
                                        {dry_run, undefined, "dry-run", {boolean, false}, help(dry_run)},
                                        rebar3_hex:repo_opt()]},
                                {profiles, [docs]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
     Apps = rebar3_hex_io:select_apps(rebar_state:project_apps(State)),
     try publish_apps(Apps, State) of
         {ok, State}  ->
             {ok, State}
     catch
         throw:{error,{rebar3_hex_docs, _}} = Err ->
             Err;
         error:{badmatch, {error, Reason}} ->
            ?PRV_ERROR(Reason)
     end.

%% @doc Publish documentation directory to repository
%%
%% This following function is exported for publishing docs via the
%% main the publish command.
-spec publish(rebar_app_info:t(), rebar_state:t(), map()) ->
    {ok, rebar_state:t()}.
publish(App, State, Repo) ->
    handle_command(App, State, Repo).

-spec format_error(any()) -> iolist().
format_error(bad_command) ->
    "Invalid command and/or options provided";
format_error({publish, {unauthorized, _Res}}) ->
    "Error publishing : Not authorized";
format_error({publish, {not_found, _Res}}) ->
    "Error publishing : Package or Package Version not found";
format_error({revert, {unauthorized, _Res}}) ->
    "Error reverting docs : Not authorized";
format_error({revert, {not_found, _Res}}) ->
    "Error reverting docs : Package or Package Version not found";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Internal Functions
%% ===================================================================

help(dry_run) ->
    "Generates docs (if configured) but does not publish the docs. Useful for inspecting docs before publishing.".

publish_apps(Apps, State) ->
    lists:foldl(fun(App, {ok, StateAcc}) ->
                        case handle_command(App, StateAcc) of
                            {ok, _StateAcc} ->
                                {ok, StateAcc};
                            Err ->
                                throw(Err)
                        end
                end, {ok, State}, Apps).

handle_command(App, State) ->
    {ok, Repo} = rebar3_hex_config:repo(State),
    handle_command(App, State, Repo).

handle_command(App, State, Repo) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(revert, Args, undefined) of
        undefined ->
            do_publish(App, State, Repo);
        Vsn ->
            do_revert(App,State,Repo, Vsn)
    end.

do_publish(App, State, Repo) ->
    maybe_gen_docs(State, Repo),
    AppDir = rebar_app_info:dir(App),
    DocDir = resolve_doc_dir(App),
    assert_doc_dir(filename:join(AppDir, DocDir)),
    Files = rebar3_hex_file:expand_paths([DocDir], AppDir),
    AppDetails = rebar_app_info:app_details(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    PkgName = rebar_utils:to_list(proplists:get_value(pkg_name, AppDetails, Name)),
    OriginalVsn = rebar_app_info:original_vsn(App),
    Vsn = rebar_utils:vcs_vsn(App, OriginalVsn, State),

    Tarball = PkgName ++ "-" ++ vsn_string(Vsn) ++ "-docs.tar.gz",
    ok = erl_tar:create(Tarball, file_list(Files, DocDir), [compressed]),
    {ok, Tar} = file:read_file(Tarball),
    file:delete(Tarball),

    {ok, Config} = rebar3_hex_config:hex_config_write(Repo),

    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_bool(dry_run, Args) of
        true ->
            rebar_api:info("--dry-run enabled : will not publish docs.", []),
            {ok, State};
        false ->
            case rebar3_hex_client:publish_docs(Config, rebar_utils:to_binary(PkgName), rebar_utils:to_binary(Vsn), Tar) of
            {ok, _} ->
                rebar_api:info("Published docs for ~ts ~ts", [PkgName, Vsn]),
                {ok, State};
            Reason ->
                ?PRV_ERROR({publish, Reason})
            end
    end.

vsn_string(<<Vsn/binary>>) ->
    binary_to_list(Vsn);
vsn_string(Vsn) ->
    Vsn.

do_revert(App, State, Repo, Vsn) ->
    {ok, Config} = rebar3_hex_config:hex_config_write(Repo),

    AppDetails = rebar_app_info:app_details(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    PkgName = ec_cnv:to_list(proplists:get_value(pkg_name, AppDetails, Name)),
    case rebar3_hex_client:delete_docs(Config, rebar_utils:to_binary(PkgName), rebar_utils:to_binary(Vsn)) of
        {modified, _Body} ->
            rebar_api:info("Successfully deleted docs for ~ts ~ts", [Name, Vsn]),
            {ok, State};
        Reason ->
            ?PRV_ERROR({revert, Reason})
    end.

%% @doc Returns the directory were docs are to be found
%%
%% The priority for resolution is the following:
%%   1. `doc' entry in the application's `*.app.src'.
%%   2. `dir' entry specified in `edoc_opts'.
%%   3. `"doc"' fallback default value.
-spec resolve_doc_dir(rebar_app_info:t()) -> string().
resolve_doc_dir(AppInfo) ->
    AppOpts = rebar_app_info:opts(AppInfo),
    EdocOpts = rebar_opts:get(AppOpts, edoc_opts, []),
    AppDetails = rebar_app_info:app_details(AppInfo),
    Dir = proplists:get_value(dir, EdocOpts, ?DEFAULT_DOC_DIR),
    proplists:get_value(doc, AppDetails, Dir).

%% @doc Generates docs based on configuration
%%
%% This function will generate docs according to the following configuration:
%%
%%   - `{doc, Options}' as part of your global hex config, where `Options' is a map.
%%   - `#{doc => Options}' as part of a specific repo configuration, where `Options' is a map
%%
%%  Repo specific config will always override global hex config if the repo in question is
%%  the context in which rebar3_hex is operating in.
%%
%%  Supported options:
%%
%%  - `provider' - This value of this option should be the name of a valid doc
%%                 provider, such as `edoc'. Note that only `edoc' is supported out of
%%                 the box with rebar3. Refer to `src/rebar_prv_edoc.erl' as an example
%%                 of a docs provider in `rebar3', as well as
%%                 https://rebar3.org/docs/tutorials/building_plugins/ for documentation on
%%                 creating plugins.
%%
%%  Example global config within rebar.config :
%%
%%  `{hex, {doc, #{provider => edoc}}}.'
%%
%%  Example repo specific config:
%%  ```
%%  {hex, [
%%        {repos, [
%%                 #{name => <<"my_private_hex">>,
%%                   repo_url => <<"https://my_private_hex.foo">>,
%%                   doc => #{provider => edoc}
%%                  }
%%                ]
%%         }
%%       ]
%%   }.
%%   '''
maybe_gen_docs(State, Repo) ->
    case doc_opts(State, Repo) of
        {ok, #{provider := PrvName}} ->
            case providers:get_provider(PrvName, rebar_state:providers(State)) of
                not_found ->
                    rebar_api:error("No provider found for ~ts", [PrvName]);
                Prv ->
                    gen_docs(State, Prv)
            end;
        _ ->
            Msg = "No valid hex docs configuration found. Docs will will not be generated",
            rebar_api:error(Msg, [])
    end.

doc_opts(State, Repo) ->
    case Repo of
      #{doc := DocOpts} when is_map(DocOpts) ->
            {ok, DocOpts};
      _ ->
        Opts = rebar_state:opts(State),
        case proplists:get_value(doc, rebar_opts:get(Opts, hex, []), undefined) of
            DocOpts when is_map(DocOpts) -> {ok, DocOpts};
            _ -> undefined
        end
    end.

gen_docs(State, Prv) ->
    case providers:do(Prv, State) of
        {ok, State} ->
            {ok, State};
        Err ->
            ?PRV_ERROR({publish, Err})
    end.

-spec assert_doc_dir(string()) -> true.
assert_doc_dir(DocDir) ->
    filelib:is_file(DocDir ++ "/index.html") orelse missing_doc_abort(DocDir).

missing_doc_abort(DocDir) ->
    rebar_api:abort( "Docs were not published since they "
                     "couldn't be found in '~s'. "
                     "Please build the docs and then run "
                     "`rebar3 hex docs` to publish them."
                     , [DocDir]
                   ).


file_list(Files, DocDir) ->
    [{drop_path(ShortName, [DocDir]), FullName} || {ShortName, FullName} <- Files].

drop_path(File, Path) ->
    filename:join(filename:split(File) -- Path).
