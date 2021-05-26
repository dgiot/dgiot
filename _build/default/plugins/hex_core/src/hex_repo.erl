-module(hex_repo).
-export([
    get_names/1,
    get_versions/1,
    get_package/2,
    get_tarball/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Gets names resource from the repository.
%%
%% Examples:
%%
%% ```
%% > hex_repo:get_names(hex_core:default_config()).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>},
%%         #{name => <<"package2">>},
%%     ]}}
%% '''
%% @end
get_names(Config) when is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> hex_registry:decode_names(Data, repo_name(Config));
            false -> hex_registry:decode_names(Data, no_verify)
        end
    end,
    get_protobuf(Config, <<"names">>, Decoder).

%% @doc
%% Gets versions resource from the repository.
%%
%% Examples:
%%
%% ```
%% > hex_repo:get_versions(Config).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>, retired => [],
%%           versions => [<<"1.0.0">>]},
%%         #{name => <<"package2">>, retired => [<<"0.5.0>>"],
%%           versions => [<<"0.5.0">>, <<"1.0.0">>]},
%%     ]}}
%% '''
%% @end
get_versions(Config) when is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> hex_registry:decode_versions(Data, repo_name(Config));
            false -> hex_registry:decode_versions(Data, no_verify)
        end
    end,
    get_protobuf(Config, <<"versions">>, Decoder).

%% @doc
%% Gets package resource from the repository.
%%
%% Examples:
%%
%% ```
%% > hex_repo:get_package(hex_core:default_config(), <<"package1">>).
%% {ok, {200, ...,
%%     {
%%         #{checksum => ..., version => <<"0.5.0">>, dependencies => []},
%%         #{checksum => ..., version => <<"1.0.0">>, dependencies => [
%%             #{package => <<"package2">>, optional => true, requirement => <<"~> 0.1">>}
%%         ]},
%%     ]}}
%% '''
%% @end
get_package(Config, Name) when is_binary(Name) and is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> hex_registry:decode_package(Data, repo_name(Config), Name);
            false -> hex_registry:decode_package(Data, no_verify, no_verify)
        end
    end,
    get_protobuf(Config, <<"packages/", Name/binary>>, Decoder).

%% @doc
%% Gets tarball from the repository.
%%
%% Examples:
%%
%% ```
%% > {ok, {200, _, Tarball}} = hex_repo:get_tarball(hex_core:default_config(), <<"package1">>, <<"1.0.0">>),
%% > {ok, #{metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
%% '''
%% @end
get_tarball(Config, Name, Version) ->
    ReqHeaders = make_headers(Config),

    case get(Config, tarball_url(Config, Name, Version), ReqHeaders) of
        {ok, {200, RespHeaders, Tarball}} ->
            {ok, {200, RespHeaders, Tarball}};

        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Config, URI, Headers) ->
    hex_http:request(Config, get, URI, Headers, undefined).

get_protobuf(Config, Path, Decoder) ->
    PublicKey = maps:get(repo_public_key, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, build_url(Config, Path), ReqHeaders) of
        {ok, {200, RespHeaders, Compressed}} ->
            Signed = zlib:gunzip(Compressed),
            case decode(Signed, PublicKey, Decoder, Config) of
                {ok, Decoded} ->
                    {ok, {200, RespHeaders, Decoded}};

                {error, _} = Error ->
                    Error
            end;

        Other ->
            Other
    end.

decode(Signed, PublicKey, Decoder, Config) ->
    Verify = maps:get(repo_verify, Config, true),

    case Verify of
        true ->
            case hex_registry:decode_and_verify_signed(Signed, PublicKey) of
                {ok, Payload} ->
                    Decoder(Payload);
                Other ->
                    Other
            end;
        false ->
            #{payload := Payload} = hex_registry:decode_signed(Signed),
            Decoder(Payload)
    end.

repo_name(#{repo_organization := Name}) when is_binary(Name) -> Name;
repo_name(#{repo_name := Name}) when is_binary(Name) -> Name.

tarball_url(Config, Name, Version) ->
    Filename = tarball_filename(Name, Version),
    build_url(Config, <<"tarballs/", Filename/binary>>).

build_url(#{repo_url := URI, repo_organization := Org}, Path) when is_binary(Org) ->
    <<URI/binary, "/repos/", Org/binary, "/", Path/binary>>;
build_url(#{repo_url := URI, repo_organization := undefined}, Path) ->
    <<URI/binary, "/", Path/binary>>;
build_url(Config, Path) ->
    build_url(Config#{repo_organization => undefined}, Path).

tarball_filename(Name, Version) ->
    <<Name/binary, "-", Version/binary, ".tar">>.

make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

set_header(http_etag, ETag, Headers) when is_binary(ETag) -> maps:put(<<"if-none-match">>, ETag, Headers);
set_header(repo_key, Token, Headers) when is_binary(Token) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.
