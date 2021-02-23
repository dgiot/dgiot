-module(rebar3_hex_client).

-export([key_add/3
         , key_get/2
         , key_delete/2
         , key_delete_all/1
         , key_list/1
         , publish_docs/4
         , delete_docs/3
         , pretty_print_status/1
         , pretty_print_errors/1]).

-include("rebar3_hex.hrl").

key_add(HexConfig, <<KeyName/binary>>, Perms) ->
    Res = hex_api_key:add(HexConfig, KeyName, Perms),
    response(Res);
key_add(HexConfig, KeyName, Perms) ->
    key_add(HexConfig, to_binary(KeyName), Perms).

key_get(HexConfig, <<KeyName/binary>>) ->
    Res = hex_api_key:get(HexConfig, KeyName),
    response(Res);
key_get(HexConfig, KeyName) ->
    key_get(HexConfig, to_binary(KeyName)).

key_list(HexConfig) ->
    Res = hex_api_key:list(HexConfig),
    response(Res).

key_delete(HexConfig, <<KeyName/binary>>) ->
    Res = hex_api_key:delete(HexConfig, KeyName),
    response(Res);
key_delete(HexConfig, KeyName) ->
    key_delete(HexConfig, to_binary(KeyName)).

key_delete_all(HexConfig) ->
    Res = hex_api_key:delete_all(HexConfig),
    response(Res).

publish_docs(Repo, Name, Version, Tarball) ->
    {ok, Config} = rebar3_hex_config:hex_config_write(Repo),

    TarballContentType = "application/octet-stream",

    Headers = maps:get(http_headers, Config, #{}),
    Headers1 = maps:put(<<"content-length">>, integer_to_binary(byte_size(Tarball)), Headers),
    Config2 = maps:put(http_headers, Headers1, Config),

    Body = {TarballContentType, Tarball},
    Res = hex_api:post(Config2, ["packages", Name, "releases", Version, "docs"], Body),
    response(Res).

delete_docs(Config, Name, Version) ->
    Res = hex_api:delete(Config, ["packages", Name, "releases", Version, "docs"]),
    response(Res).

response({ok, {200, _Headers, Res}}) ->
    {success, Res};
response({ok, {201, _Headers, Res}}) ->
    {created, Res};
response({ok, {204, _Headers, Res}}) ->
    {success, Res};
response({ok, {401, _Headers, Res}}) ->
    {unauthorized, Res};
response({ok, {404, _Headers, Res}}) ->
    {not_found, Res};
response({ok, {422, _Headers, #{<<"message">> := <<"Validation error(s)">>} = Res}}) ->
    {validation_errors, Res};
response({ok, {422, _Headers, Res}}) ->
    {unprocessable, Res};
response({_, _} = Unknown) ->
    Unknown.

pretty_print_status(401) -> "Authentication failed (401)";
pretty_print_status(403) -> "Forbidden (403)";
pretty_print_status(404) -> "Entity not found (404)";
pretty_print_status(422) -> "Validation failed (422)";
pretty_print_status(Code) -> io_lib:format("HTTP status code: ~p", [Code]).

pretty_print_errors(Errors) ->
    L =  maps:fold(fun(K,V,Acc) ->
                           case is_map(V) of
                               true ->
                                   Acc ++ [pretty_print_errors(V)];
                               false ->
                                   Acc ++ [<<K/binary, " ", V/binary>>]
                           end
                   end,
                   [],
                   Errors),
    binary:list_to_bin(join_lists(", ", L)).


-ifdef(POST_OTP_18).
join_lists(Sep, List) -> lists:join(Sep, List).
-else.
join_lists(_Sep, []) -> [];
join_lists(Sep, List) ->
    [Last | AllButLast] = lists:reverse(List),
    lists:foldl(fun (Elem,Acc) -> [Elem,Sep|Acc] end, [Last], AllButLast).
-endif.


to_binary(Subject) ->
    rebar_utils:to_binary(Subject).
