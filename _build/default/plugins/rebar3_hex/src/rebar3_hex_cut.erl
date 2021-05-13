-module(rebar3_hex_cut).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, cut).
-define(DEPS, [{default, lock}]).

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
                                {example, "rebar3 hex cut"},
                                {short_desc, "Increment version number and publish package"},
                                {desc, ""},
                                {opts, [{increment, $i, "increment", string,
                                         "Type of semver increment: major, minor or patch"},
                                        rebar3_hex:repo_opt()]}
                                ]),
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
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(increment, Args, undefined) of
        undefined ->
            Apps = rebar3_hex_io:select_apps(rebar_state:project_apps(State)),
            lists:foldl(fun(App, {ok, StateAcc}) ->
                                do_(App, Repo, StateAcc)
                        end, {ok, State}, Apps);
        Type ->
            case string_to_bump(Type) of
                error ->
                    {error, {?MODULE, {bad_increment, Type}}};
                Bump ->
                    Apps = rebar3_hex_io:select_apps(rebar_state:project_apps(State)),
                    lists:foldl(fun(App, {ok, StateAcc}) ->
                                        do_(Bump, App, Repo, StateAcc)
                                end, {ok, State}, Apps)
            end
    end.

-spec format_error(any()) -> iolist().
format_error({no_write_key, RepoName}) ->
    io_lib:format("No api key with permissions to write to the repository ~ts was found.", [RepoName]);
format_error({bad_increment, Type}) ->
    io_lib:format("Increment must be major, minor or patch. ~s is not valid.", [Type]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Public API
%% ===================================================================

do_(App, HexConfig, State) ->
    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    Type = get_increment(ResolvedVersion),
    do_(Type, App, HexConfig, State).

do_(Type, App, HexConfig, State) ->
    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    {ok, Ver} = verl:parse(rebar_utils:to_binary(ResolvedVersion)),
    NewVersion = increment(Type, Ver),
    AppSrcFile = rebar_app_info:app_file_src(App),

    case Version of
        _Git when Version =:= git orelse Version =:= "git" ->
            rebar_api:info("Creating new tag v~s...", [NewVersion]),
            rebar_utils:sh(io_lib:format("git tag v~s", [NewVersion]), []),

            {application, _, AppDetails} = rebar3_hex_file:update_app_src(App, NewVersion),

            case rebar3_hex_publish:validate_app_details(AppDetails) of
                ok ->
                    Name = rebar_app_info:name(App),
                    Deps = rebar_state:get(State, {locks, default}, []),
                    {TopLevel, Excluded} = rebar3_hex_publish:gather_deps(Deps),
                    case rebar3_hex_publish:publish(App, Name, NewVersion, TopLevel,
                                                    Excluded, AppDetails, HexConfig, State) of
                        {ok, _State} ->
                            case rebar3_hex_io:ask("Push new tag to origin?", boolean, "Y") of
                                true ->
                                    rebar_api:info("Pushing new tag v~s...", [NewVersion]),
                                    rebar_utils:sh(io_lib:format("git push origin v~s", [NewVersion]), []),
                                    {ok, State};
                                false ->
                                    {ok, State}
                            end;
                        {error, _} = Error ->
                            rebar_api:info("Deleting new tag v~s...", [NewVersion]),
                            rebar_utils:sh(io_lib:format("git tag -d v~s", [NewVersion]), []),
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            Spec = rebar3_hex_file:update_app_src(App, NewVersion),
            NewAppSrcFile = io_lib:format("~tp.\n", [Spec]),
            ok = rebar_file_utils:write_file_if_contents_differ(AppSrcFile, NewAppSrcFile),
            ask_commit_and_push(NewVersion),
            case rebar3_hex_publish:publish(rebar_app_info:original_vsn(App, NewVersion), HexConfig, State) of
                {ok, _State} ->
                    {ok, State};
                {error, _} = Error ->
                    Error
            end
    end.

get_increment(Version) ->
    rebar3_hex_io:say("Select semver increment or other (Current ~s):", [Version]),
    rebar3_hex_io:say("1) patch", []),
    rebar3_hex_io:say("2) minor", []),
    rebar3_hex_io:say("3) major", []),
    rebar3_hex_io:say("4) other", []),
    case rebar3_hex_io:ask("[1-4] ", number) of
        4 ->
            rebar3_hex_io:ask("New Version ", string);
        Type ->
            int_to_bump(Type)
    end.

increment(patch, #{major := Maj, minor := Min, patch := Patch}) ->
    ver_format(#{major => Maj, minor => Min, patch => Patch + 1});
increment(minor, #{major := Maj, minor := Min}) ->
     ver_format(#{major => Maj, minor => Min + 1, patch => 0});
increment(major, #{major := Maj}) ->
    ver_format(#{major => Maj + 1, minor => 0, patch => 0});
increment(Version, _) when is_list(Version) ->
    rebar_utils:to_binary(Version).

%% TODO: Support pre and build
ver_format(#{major := Maj, minor := Min, patch := Patch}) ->
    MajBin = integer_to_binary(Maj),
    MinBin = integer_to_binary(Min),
    PatchBin =  integer_to_binary(Patch),
    DotBin = <<".">>,
    <<MajBin/binary, DotBin/binary, MinBin/binary, DotBin/binary, PatchBin/binary>>.

int_to_bump(1) -> patch;
int_to_bump(2) -> minor;
int_to_bump(3) -> major;
int_to_bump(_) -> error.

string_to_bump("patch") -> patch;
string_to_bump("minor") -> minor;
string_to_bump("major") -> major;
string_to_bump(_) -> error.

ask_commit_and_push(NewVersion) ->
    case rebar3_hex_io:ask(io_lib:format("Create 'v~s' commit?", [NewVersion]), boolean, "Y") of
        true ->
            rebar_utils:sh(io_lib:format("git commit -a -m 'v~s'", [NewVersion]), []),
            case rebar3_hex_io:ask("Push master to origin master?", boolean, "N") of
                true ->
                    rebar_utils:sh("git push origin master:master", []);
                false ->
                    ok
            end;
        false ->
            ok
    end.
