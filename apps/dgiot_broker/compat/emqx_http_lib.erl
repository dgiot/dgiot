%% @doc 同名承接：emqx_http_lib（刀 6 批次 2）。URI 解析/解码，基于 OTP。
-module(emqx_http_lib).

-export([uri_decode/1, uri_parse/1, uri_encode/1]).

uri_decode(Bin) when is_binary(Bin) ->
    try uri_string:percent_decode(Bin)
    catch _:_ -> {error, {bad_uri, Bin}}
    end;
uri_decode(Str) when is_list(Str) -> uri_decode(list_to_binary(Str)).

uri_parse(URI) when is_binary(URI) ->
    try uri_string:parse(URI)
    catch _:_ -> {error, {bad_uri, URI}}
    end;
uri_parse(URI) when is_list(URI) -> uri_parse(list_to_binary(URI)).

uri_encode(Term) -> uri_string:quote(Term).
