%% @hidden

-module(hex_api).

-export([
    delete/2,
    get/2,
    post/3,
    put/3,
    encode_query_string/1,
    build_repository_path/2,
    build_organization_path/2,
    join_path_segments/1
]).
-define(ERL_CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

-export_type([body/0, response/0]).

-type response() :: {ok, {hex_http:status(), hex_http:headers(), body() | nil}} | {error, term()}.
-type body() :: [body()] | #{binary() => body() | binary()}.

get(Config, Path) ->
    request(Config, get, Path, undefined).

post(Config, Path, Body) ->
    request(Config, post, Path, encode_body(Body)).

put(Config, Path, Body) ->
    request(Config, put, Path, encode_body(Body)).

delete(Config, Path) ->
    request(Config, delete, Path, undefined).

%% @private
encode_query_string(List) ->
    Pairs = lists:map(fun ({K, V}) -> {to_list(K), to_list(V)} end, List),
    list_to_binary(compose_query(Pairs)).

%% OTP 21+
-ifdef (OTP_RELEASE).
compose_query(Pairs) ->
    uri_string:compose_query(Pairs).
-else.
compose_query(Pairs) ->
    String = join("&", lists:map(fun ({K, V}) -> K ++ "=" ++ V end, Pairs)),
    http_uri:encode(String).
-endif.

%% @private
build_repository_path(#{api_repository := Repo}, Path) when is_binary(Repo) ->
    ["repos", Repo | Path];
build_repository_path(#{api_repository := undefined}, Path) ->
    Path.

%% @private
build_organization_path(#{api_organization := Org}, Path) when is_binary(Org) ->
    ["orgs", Org | Path];
build_organization_path(#{api_organization := undefined}, Path) ->
    Path.

%% @private
join_path_segments(Segments) ->
    iolist_to_binary(recompose(Segments)).

%% OTP 21+
-ifdef (OTP_RELEASE).
recompose(Segments) ->
    Concatenated = join(<<"/">>, Segments),
    %% uri_string:recompose/1 accepts path segments as a list,
    %% both strings and binaries
    uri_string:recompose(#{path => Concatenated}).
-else.
recompose(Segments) ->
    join(<<"/">>, lists:map(fun encode_segment/1, Segments)).

encode_segment(Binary) when is_binary(Binary) ->
    encode_segment(binary_to_list(Binary));
encode_segment(String) when is_list(String) ->
    http_uri:encode(String).
-endif.

%%====================================================================
%% Internal functions
%%====================================================================

request(Config, Method, PathSegments, Body) when is_list(PathSegments) ->
    Path = join_path_segments(PathSegments),
    request(Config, Method, Path, Body);
request(Config, Method, Path, Body) when is_binary(Path) and is_map(Config) ->
    DefaultHeaders = make_headers(Config),
    ReqHeaders = maps:merge(maps:get(http_headers, Config, #{}), DefaultHeaders),
    ReqHeaders2 = put_new(<<"accept">>, ?ERL_CONTENT_TYPE, ReqHeaders),

    case hex_http:request(Config, Method, build_url(Path, Config), ReqHeaders2, Body) of
        {ok, {Status, RespHeaders, RespBody}} ->
            ContentType = maps:get(<<"content-type">>, RespHeaders, <<"">>),
            case binary:match(ContentType, ?ERL_CONTENT_TYPE) of
                {_, _} ->
                    {ok, {Status, RespHeaders, binary_to_term(RespBody)}};

                nomatch ->
                    {ok, {Status, RespHeaders, nil}}
            end;

        Other ->
            Other
    end.

build_url(Path, #{api_url := URI}) ->
    <<URI/binary, "/", Path/binary>>.

encode_body({_ContentType, _Body} = Body) ->
    Body;
encode_body(Body) ->
    {binary_to_list(?ERL_CONTENT_TYPE), term_to_binary(Body)}.

%% TODO: copy-pasted from hex_repo
make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

set_header(api_key, Token, Headers) when is_binary(Token) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.

%% https://github.com/erlang/otp/blob/OTP-20.3/lib/stdlib/src/lists.erl#L1449:L1453
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> unicode:characters_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(Str) -> unicode:characters_to_list(Str).
