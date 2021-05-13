-module(verl).

-export([
         compare/2
         , is_match/2
         , is_match/3
         , parse/1
         , parse_requirement/1
         , compile_requirement/1]).

-type version() :: binary().
-type requirement() :: binary().
-opaque major() :: non_neg_integer().
-opaque minor() :: non_neg_integer().
-opaque patch() :: non_neg_integer().
-opaque pre() :: [binary() | non_neg_integer()].
-opaque build() :: binary() | undefined.
-opaque version_t() :: #{
          major => major()
          , minor => minor()
          , patch => patch()
          , pre   => pre()
          , build => [build()]}.
-opaque requirement_t() :: #{
          string => requirement(),
          matchspec => list(),
          compiled  => boolean()
         }.

-opaque compiled_requirement() :: #{
          compiled => true,
          matchspec => ets:comp_match_spec(),
          string => requirement()}.

-export_type([version/0, requirement/0, major/0, minor/0, patch/0, pre/0,
              build/0, version_t/0, requirement_t/0, compiled_requirement/0]).

%%% @doc
%%% Compare two version returing whether first argument is greater, equal, or
%%% less than than second argument.
%%% @end
-spec compare(version(), version()) -> gt | eq | lt | {error, invalid_version}.
compare(Version1, Version2) ->
    ver_cmp(to_matchable(Version1,true), to_matchable(Version2, true)).

%%% @doc
%%% Parses a semantic version returing a version_t() or {error, invalid_version}
%%% @end
-spec parse(version()) -> version_t() | {error, invalid_version}.
parse(Str) ->
    build_version(Str).

%%% @doc
%%% Parses a semantic version requirement, returns a requirement_t()
%%% @end
-spec parse_requirement(requirement()) ->
    {ok, requirement_t()} | {error, invalid_requirement}.
parse_requirement(Str) ->
    case verl_parser:parse_requirement(Str) of
        {ok, Spec} ->
            {ok, #{string => Str, matchspec => Spec, compiled => false}};
        {error, invalid_requirement} ->
            {error, invalid_requirement}
    end.

%%% @doc
%%% Compiles a version requirement as returned by parse_requirement for faster
%%% matches.
%%% @end
-spec compile_requirement(requirement_t()) -> compiled_requirement().
compile_requirement(Req) when is_map(Req) ->
    Ms = ets:match_spec_compile(maps:get(matchspec, Req)),
    maps:put(compiled, true, maps:put(matchspec, Ms, Req)).

%%% @doc
%%% Returns true if the dependency is in range of the requirement, otherwise
%%% false.
%%% @end
-spec is_match(version() | version_t(), requirement() | requirement_t()) -> boolean() | {error, badarg | invalid_requirement | invalid_version}.
is_match(Version, Requirement) ->
    is_match(Version, Requirement, []).

%%% @doc
%%% Exactly like is_match/2 but takes an options argument.
%%% @end
is_match(Version, Requirement, Opts) when is_binary(Version) andalso is_binary(Requirement) ->
    case build_version(Version) of
        {ok, Ver} ->
            case build_requirement(Requirement) of
                {ok, Req} ->
                    is_match(Ver, Req, Opts);
                {error, invalid_requirement} ->
                    {error, invalid_requirement}
            end;
        {error, invalid_version} ->
            {error, invalid_version}
    end;
is_match(Version, Requirement, Opts) when is_binary(Version) andalso is_map(Requirement) ->
    case build_version(Version) of
        {ok, Ver} ->
            is_match(Ver, Requirement, Opts);
        {error, invalid_version} ->
            {error, invalid_version}
    end;
is_match(Version, Requirement, Opts) when is_map(Version) andalso is_binary(Requirement) ->
    case build_requirement(Requirement) of
        {ok, Req} ->
            is_match(Version, Req, Opts);
        {error, invalid_requirement} ->
            {error, invalid_requirement}
    end;
is_match(Version, #{matchspec := Spec, compiled := false} = R, Opts) when is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, true),
    {ok, Result} = ets:test_ms(to_matchable(Version, AllowPre), Spec),
    Result /= false;
is_match(Version, #{matchspec := Spec, compiled := true} = R, Opts)  when
      is_map(Version) andalso is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, true),
    ets:match_spec_run([to_matchable(Version, AllowPre)], Spec) /= [].

to_matchable(#{major := Major, minor := Minor, patch := Patch, pre := Pre}, AllowPre) ->
    {Major, Minor, Patch, Pre, AllowPre};
to_matchable(String, AllowPre) when is_binary(String) ->
    case verl_parser:parse_version(String) of
        {ok, {Major, Minor, Patch, Pre, _Build}} ->
            {Major, Minor, Patch, Pre, AllowPre};
        {error, invalid_version} ->
            {error, invalid_version}
    end.

%% private
%%
build_version(Version) ->
    case verl_parser:parse_version(Version) of
        {ok, {Major, Minor, Patch, Pre, Build}} ->
            {ok, #{major => Major,
                   minor => Minor,
                   patch => Patch,
                   pre   => Pre,
                   build => build_string(Build)}};
        {error, invalid_version} ->
            {error, invalid_version}
    end.

build_requirement(Str) ->
    case verl_parser:parse_requirement(Str) of
        {ok, Spec} ->
            {ok, #{string => Str, matchspec => Spec, compiled => false}};
        {error, invalid_requirement} ->
            {error, invalid_requirement}
    end.

build_string(Build) ->
    case Build of
        [] -> undefined;
        _ -> binary:list_to_bin(Build)
    end.

ver_cmp({Maj1, Min1, Patch1, Pre1, _}, {Maj2, Min2, Patch2, Pre2, _}) ->
    case {Maj1, Min1, Patch1} > {Maj2, Min2, Patch2} of
        true ->
            gt;
        false ->
            case {Maj1, Min1, Patch1} < {Maj2, Min2, Patch2} of
                true ->
                    lt;
                false ->
                    test_pre(Pre1, Pre2)
            end
    end;
ver_cmp(_, _) -> {error, invalid_version}.

test_pre(Pre1, Pre2) ->
    case pre_is_eq(Pre1, Pre2) of
        true ->
            gt;
        false ->
            case pre_is_eq(Pre2, Pre1) of
                true ->
                    lt;
                false ->
                    pre_cmp(Pre1, Pre2)
            end
    end.

pre_cmp(Pre1, Pre2) ->
    case Pre1 > Pre2 of
        true ->
            gt;
        false ->
            case Pre1 < Pre2 of
                true ->
                    lt;
                false ->
                    eq
            end
    end.

pre_is_eq(Pre1, Pre2) ->
    case Pre1 == [] of
        false -> false;
        true -> Pre2 /= []
    end.
