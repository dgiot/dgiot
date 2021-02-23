%% @doc The publish provider is responsible for creating a tarball of
%% an application and uploading to the repository.
%% @end
-module(rebar3_hex_publish).

-export([init/1,
         do/1,
         format_error/1]).

-export([publish/3
        ,publish/8,
        validate_app_details/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, lock}]).

-define(VALIDATIONS, [  has_semver
                      , has_contributors
                      , has_maintainers
                      , has_description
                      , has_licenses ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex publish"},
                                 {short_desc, "Publish a new version of your package and update the package"},
                                 {desc, ""},
                                 {opts, [rebar3_hex:repo_opt(),
                                         {"yes", $y, "yes", {boolean, false}, "Publishes the package without any confirmation
                                         prompts"},
                                         {"--revert", undefined, "--revert", string, "Revert given version."}]}]),

    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar3_hex_config:repo(State) of
        {ok, Repo} ->
            handle_command(State, Repo);
        {error, Reason} ->
            ?PRV_ERROR(Reason)
        end.

handle_command(State, Repo) ->
        case maps:get(write_key, Repo, maps:get(api_key, Repo, undefined)) of
            undefined ->
                ?PRV_ERROR(no_write_key);
            _ ->
                Apps = rebar3_hex_io:select_apps(rebar_state:project_apps(State)),
                lists:foldl(fun(App, {ok, StateAcc}) ->
                                    publish(App, Repo, StateAcc)
                            end, {ok, State}, Apps)
        end.

-spec format_error(any()) -> iolist().
format_error(ErrList) when is_list(ErrList) ->
  F = fun(Err, Acc) ->
          ErrStr = format_error(Err),
          Acc ++ "     " ++ ErrStr ++ "\n"
      end,
  More = "\n     Please see https://hex.pm/docs/rebar3_publish for more info.\n",
  lists:foldl(F, "Validator Errors:\n", ErrList) ++ More;
format_error(bad_command) ->
        "bad command";
format_error({required, repo}) ->
    "publish requires a repo name argument to identify the repo to publish to";
format_error({not_valid_repo, RepoName}) ->
    io_lib:format("No configuration for repository ~ts found.", [RepoName]);
format_error({invalid_semver, AppName, Version}) ->
    Err = "~ts.app.src : non-semantic version number \"~ts\" found",
    io_lib:format(Err, [AppName, Version]);
format_error({no_description, AppName}) ->
    Err = "~ts.app.src : missing or empty description property",
    io_lib:format(Err, [AppName]);
format_error({no_license, AppName}) ->
    Err = "~ts.app.src : missing or empty licenses property",
    io_lib:format(Err, [AppName]);
format_error({has_maintainers, AppName}) ->
    Err = "~ts.app.src : deprecated field maintainers found",
    io_lib:format(Err, [AppName]);
format_error({has_contributors, AppName}) ->
    Err = "~ts.app.src : deprecated field contributors found",
    io_lib:format(Err, [AppName]);
format_error(no_write_key) ->
    "No write key found for user. Be sure to authenticate first with:"
    ++ " rebar3 hex user auth";
format_error({validation_errors, Errors, Message}) ->
    ErrorString = errors_to_string(Errors),
    io_lib:format("Failed to publish package: ~ts~n\t~ts", [Message, ErrorString]);
format_error({publish_failed, Message}) ->
    io_lib:format("Failed to publish package: ~ts", [Message]);
format_error({non_hex_deps, Excluded}) ->
    Err = "Can not publish package because the following deps are not available"
         ++ " in hex: ~s",
    io_lib:format(Err, [string:join(Excluded, ", ")]);
format_error(undefined_server_error) ->
    "Unknown server error";
format_error({status, Status}) ->
    rebar3_hex_client:pretty_print_status(Status);
format_error({status, Status, undefined_server_error}) ->
    "Unknown server error: " ++ rebar3_hex_client:pretty_print_status(Status);
format_error({status, Status, Error}) ->
  Message = maps:get(<<"message">>, Error, ""),
  Errors = maps:get(<<"errors">>, Error, ""),
  ErrorString = errors_to_string(Errors),
  Data =  [rebar3_hex_client:pretty_print_status(Status), Message, ErrorString],
  io_lib:format("Status Code: ~s~nHex Error: ~s~n\t~s", Data);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Public API
%% ===================================================================

publish(App, HexConfig, State) ->
    Name = rebar_app_info:name(App),

    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(App, Version, State),
    {application, _, AppDetails} = rebar3_hex_file:update_app_src(App, ResolvedVersion),


    Deps = rebar_state:get(State, {locks, default}, []),
    TopLevel = [{N, [{<<"app">>, A},
                     {<<"optional">>, false},
                     {<<"requirement">>, V}]} || {A,{pkg,N,V,_},0} <- Deps],
    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],

    case is_valid_app({App, Name, ResolvedVersion, AppDetails}) of
        ok ->
            publish(App, Name, ResolvedVersion, TopLevel,
                    Excluded, AppDetails, HexConfig, State);
        {error, Errors} ->
            ?PRV_ERROR(Errors)
    end.

publish(App, Name, Version, Deps, [], AppDetails, HexConfig, State) ->
    AppDir = rebar_app_info:dir(App),
    Config = rebar_config:consult(AppDir),
    ConfigDeps = proplists:get_value(deps, Config, []),
    Deps1 = update_versions(ConfigDeps, Deps),

    Description = proplists:get_value(description, AppDetails, ""),

    PackageFiles = include_files(Name, AppDir, AppDetails),

    Licenses = proplists:get_value(licenses, AppDetails, []),
    Links = proplists:get_value(links, AppDetails, []),
    BuildTools = proplists:get_value(build_tools, AppDetails, [<<"rebar3">>]),

    %% We check the app file for the 'pkg' key wich allows us to select
    %% a package name other then the app name, if it is not set we default
    %% back to the app name.
    PkgName = rebar_utils:to_binary(proplists:get_value(pkg_name, AppDetails, Name)),

    Optional = [{<<"app">>, Name},
                {<<"parameters">>, []},
                {<<"description">>, rebar_utils:to_binary(Description)},
                {<<"files">>, [binarify(File) || {File, _} <- PackageFiles]},
                {<<"licenses">>, binarify(Licenses)},
                {<<"links">>, to_map(binarify(Links))},
                {<<"build_tools">>, binarify(BuildTools)}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
    Metadata = maps:from_list([{<<"name">>, PkgName}, {<<"version">>, binarify(Version)},
                               {<<"requirements">>, maps:from_list(Deps1)} | OptionalFiltered]),

    rebar3_hex_io:say("Publishing ~ts ~ts to ~ts", [PkgName, Version, maps:get(name, HexConfig)]),
    rebar3_hex_io:say("  Description: ~ts", [Description]),
    rebar3_hex_io:say("  Dependencies:~n    ~ts", [format_deps(Deps1)]),
    rebar3_hex_io:say("  Included files:~n    ~ts", [string:join([F || {F, _} <- PackageFiles], "\n    ")]),
    rebar3_hex_io:say("  Licenses: ~ts", [format_licenses(Licenses)]),
    rebar3_hex_io:say("  Links:~n    ~ts", [format_links(Links)]),
    rebar3_hex_io:say("  Build tools: ~ts", [format_build_tools(BuildTools)]),
    maybe_say_coc(HexConfig),
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_bool("yes", Args) of
        true ->
            publish_package_and_docs(Name, Version, Metadata, PackageFiles, HexConfig, App, State);
        false ->
            case rebar3_hex_io:ask("Proceed?", boolean, "Y") of
                true ->
                    publish_package_and_docs(Name, Version, Metadata, PackageFiles, HexConfig, App, State);
                _ ->
                    rebar3_hex_io:say("Goodbye..."),
                    {ok, State}
            end
    end;

publish(_AppDir, _Name, _Version, _Deps, Excluded, _AppDetails, _, _) ->
    ?PRV_ERROR({non_hex_deps, Excluded}).

publish_package_and_docs(Name, Version, Metadata, PackageFiles, HexConfig, App, State) ->
    {ok, HexConfig1} = rebar3_hex_config:hex_config_write(HexConfig),
    case create_and_publish(Metadata, PackageFiles, HexConfig1) of
        ok ->
            rebar_api:info("Published ~s ~s", [Name, Version]),
            rebar3_hex_docs:publish(App, State, HexConfig1),
            {ok, State};
        Error={error, _} ->
            Error
    end.

%% Internal functions

%% if publishing to the public repo or to a private organization link to the code of conduct
maybe_say_coc(#{parent := <<"hexpm">>}) ->
    rebar3_hex_io:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(#{name := <<"hexpm">>}) ->
    rebar3_hex_io:say("Be aware, you are publishing to the public Hexpm repository.", []),
    rebar3_hex_io:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(_) ->
    ok.

create_and_publish(Metadata, PackageFiles, HexConfig) ->
    {ok, #{tarball := Tarball, inner_checksum := _Checksum}} = hex_tarball:create(Metadata, PackageFiles),
    case hex_api_release:publish(HexConfig, Tarball) of
        {ok, {400, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({publish_failed, Message});
        {ok, {401, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({publish_failed, Message});
        {ok, {422, _Headers, #{<<"errors">> := Errors,
                               <<"message">> := Message}}} ->
            ?PRV_ERROR({validation_errors, Errors, Message});
        {ok, {201, _Headers, _Body}} ->
            ok;
        {ok, {200, _Headers, _Body}} ->
            ok;
        {ok, {500, _Headers, _Body}} ->
            ?PRV_ERROR({error, "Internal Server Error"});
        {error, Reason} ->
            ?PRV_ERROR({error, Reason})
    end.


known_exclude_file(Path, ExcludeRe) ->
    KnownExcludes = [
                     "~$",        %% emacs temp files
                     "\\.o$",     %% c object files
                     "\\.so$",    %% compiled nif libraries
                     "\\.swp$"    %% vim swap files
                    ],
    lists:foldl(fun(_, true) -> true;
                   (RE, false) ->
                        re:run(Path, RE) =/= nomatch
                end, false, KnownExcludes ++ ExcludeRe).

exclude_file(Path, ExcludeFiles, ExcludeRe) ->
    lists:keymember(Path, 2, ExcludeFiles) orelse
        known_exclude_file(Path, ExcludeRe).

%% allows us to support lists of tuples or maps for metadata the user writes in .app.src
to_map(Map) when is_map(Map) ->
    Map;
to_map(List) when is_list(List) ->
    maps:from_list(List).

include_files(Name, AppDir, AppDetails) ->
    AppSrc = {application, to_atom(Name), AppDetails},
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    IncludeFilePaths = proplists:get_value(include_files, AppDetails, []),
    ExcludeFilePaths = proplists:get_value(exclude_files, AppDetails, []),
    ExcludeRes = proplists:get_value(exclude_regexps, AppDetails, []),

    AllFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(FilePaths, AppDir)),
    IncludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(IncludeFilePaths, AppDir)),
    ExcludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(ExcludeFilePaths, AppDir)),

    %% We filter first and then include, that way glob excludes can be
    %% overwritten be explict includes
    FilterExcluded = lists:filter(fun ({_, Path}) ->
                                      not exclude_file(Path, ExcludeFiles, ExcludeRes)
                                  end, AllFiles),
    WithIncludes = lists:ukeymerge(2, FilterExcluded, IncludeFiles),

    AppFileSrc = filename:join("src", to_list(Name)++".app.src"),
    AppSrcBinary = rebar_utils:to_binary(lists:flatten(io_lib:format("~tp.\n", [AppSrc]))),
    lists:keystore(AppFileSrc, 1, WithIncludes, {AppFileSrc, AppSrcBinary}).


is_valid_app({_App, _Name, _Version, _AppDetails} = A) ->
    F = fun(K, Acc) ->
            case validate_app(K, A) of
                ok ->
                    Acc;
                {error, Error} ->
                    Acc ++ [Error]
            end
        end,
    case lists:foldl(F, [], ?VALIDATIONS) of
        [] ->
            ok;
        Errors ->
            {error, Errors}
    end.

validate_app(has_semver, {_, Name, Ver, _}) ->
    case verl:parse(rebar_utils:to_binary(Ver)) of
        {error, invalid_version} ->
            {error, {invalid_semver, Name, Ver}};
        _ ->
         ok
    end;
validate_app(has_contributors, {_, Name, _, AppDetails}) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            rebar_log:log(warn, format_error({has_contributors, Name}), []),
            ok;
        false ->
            ok
    end;
validate_app(has_maintainers, {_, Name, _, AppDetails}) ->
    case proplists:is_defined(maintainers, AppDetails) of
        true ->
            rebar_log:log(warn, format_error({has_maintainers, Name}), []),
            ok;
        false ->
            ok
    end;
validate_app(has_description, {_, Name, _, AppDetails}) ->
    case is_empty_prop(description, AppDetails) of
        true ->
            {error, {no_description, Name}};
        false ->
            ok
    end;
validate_app(has_licenses, {_, Name, _, AppDetails}) ->
    case is_empty_prop(licenses, AppDetails)  of
        true ->
          {error, {no_license, Name}};
        _ ->
          ok
    end.

is_empty_prop(K, PropList) ->
    Prop = proplists:get_value(K, PropList),
    case Prop of
        Empty when Empty =:= [] orelse Empty =:= undefined ->
          true;
        _ ->
          false
    end.

%% TODO: Modify hex cut so we can deprecate this?
validate_app_details(AppDetails) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            {error, {rebar3_hex_publish, has_contributors}};
        false ->
            ok
    end.

format_deps(Deps) ->
    string:join([binary_to_list(<<N/binary, " ", V/binary>>) || {N, #{<<"requirement">> := V}} <- Deps], "\n    ").

format_licenses(Licenses) ->
    string:join(Licenses, ", ").

format_links(Links) ->
    string:join([lists:flatten([Name, ": ", Url]) || {Name, Url} <- Links], "\n    ").

format_build_tools(BuildTools) ->
    string:join([io_lib:format("~s", [Tool]) || Tool <- BuildTools], ", ").

update_versions(ConfigDeps, Deps) ->
    [begin
         case lists:keyfind(binary_to_atom(N, utf8), 1, ConfigDeps) of
             {_, V} when is_list(V) ->
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, {<<"requirement">>, list_to_binary(V)}))};
             _ ->
                 %% using version from lock. prepend ~> to make it looser
                 {_, Version} = lists:keyfind(<<"requirement">>, 1, M),
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, {<<"requirement">>, <<"~>", Version/binary>>}))}
         end
     end || {N, M} <- Deps].


errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string(Map) when is_map(Map) ->
    errors_to_string(maps:to_list(Map));
errors_to_string({<<"inserted_at">>, E}) ->
    lists:flatten(io_lib:format("Inserted At: ~s~n", [E]));
errors_to_string({<<"requirements">>,  Rs}) ->
    lists:flatten(["Requirements could not be computed\n",
                  [io_lib:format("~s\n~20.20c\n~s\n",[P,$-, R]) || {P, R} <- maps:to_list(Rs)]]);
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).


binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Map) when is_map(Map) ->
    maps:from_list(binarify(maps:to_list(Map)));
binarify(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true ->
            rebar_utils:to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.


%% via ec_cnv
-spec to_atom(atom() | list() | binary() | integer() | float()) ->
                     atom().
to_atom(X)
  when erlang:is_atom(X) ->
    X;
to_atom(X)
  when erlang:is_list(X) ->
    erlang:list_to_existing_atom(X);
to_atom(X) ->
    to_atom(to_list(X)).


-spec to_list(atom() | list() | binary() | integer() | float()) ->
                     list().
to_list(X)
  when erlang:is_float(X) ->
    erlang:float_to_list(X);
to_list(X)
  when erlang:is_integer(X) ->
    erlang:integer_to_list(X);
to_list(X)
  when erlang:is_binary(X) ->
    erlang:binary_to_list(X);
to_list(X)
  when erlang:is_atom(X) ->
    erlang:atom_to_list(X);
to_list(X)
  when erlang:is_list(X) ->
    X.
