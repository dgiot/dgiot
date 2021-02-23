%% Copyright (c) 2014-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cow_http_hd).

%% Parsing.
-export([parse_accept/1]).
-export([parse_accept_charset/1]).
% @todo -export([parse_accept_datetime/1]). RFC7089
-export([parse_accept_encoding/1]).
% @todo -export([parse_accept_features/1]). RFC2295
-export([parse_accept_language/1]).
-export([parse_accept_ranges/1]).
% @todo -export([parse_access_control_allow_credentials/1]). CORS
% @todo -export([parse_access_control_allow_headers/1]). CORS
% @todo -export([parse_access_control_allow_methods/1]). CORS
% @todo -export([parse_access_control_allow_origin/1]). CORS
% @todo -export([parse_access_control_expose_headers/1]). CORS
% @todo -export([parse_access_control_max_age/1]). CORS
-export([parse_access_control_request_headers/1]).
-export([parse_access_control_request_method/1]).
-export([parse_age/1]).
-export([parse_allow/1]).
% @todo -export([parse_alternates/1]). RFC2295
% @todo -export([parse_authentication_info/1]). RFC2617
-export([parse_authorization/1]).
-export([parse_cache_control/1]).
-export([parse_connection/1]).
% @todo -export([parse_content_disposition/1]). RFC6266
-export([parse_content_encoding/1]).
-export([parse_content_language/1]).
-export([parse_content_length/1]).
% @todo -export([parse_content_location/1]). RFC7231
% @todo -export([parse_content_md5/1]). RFC2616 (deprecated)
-export([parse_content_range/1]).
% @todo -export([parse_content_security_policy/1]). CSP
% @todo -export([parse_content_security_policy_report_only/1]). CSP
-export([parse_content_type/1]).
% @todo -export([parse_cookie/1]). RFC6265
-export([parse_date/1]).
% @todo -export([parse_digest/1]). RFC3230
% @todo -export([parse_dnt/1]). http://donottrack.us/
-export([parse_etag/1]).
-export([parse_expect/1]).
-export([parse_expires/1]).
% @todo -export([parse_forwarded/1]). RFC7239
% @todo -export([parse_from/1]). RFC7231
-export([parse_host/1]).
-export([parse_http2_settings/1]).
-export([parse_if_match/1]).
-export([parse_if_modified_since/1]).
-export([parse_if_none_match/1]).
-export([parse_if_range/1]).
-export([parse_if_unmodified_since/1]).
% @todo -export([parse_last_event_id/1]). eventsource
-export([parse_last_modified/1]).
% @todo -export([parse_link/1]). RFC5988
% @todo -export([parse_location/1]). RFC7231
-export([parse_max_forwards/1]).
% @todo -export([parse_memento_datetime/1]). RFC7089
% @todo -export([parse_negotiate/1]). RFC2295
-export([parse_origin/1]).
-export([parse_pragma/1]).
% @todo -export([parse_prefer/1]). RFC7240
-export([parse_proxy_authenticate/1]).
% @todo -export([parse_proxy_authentication_info/1]). RFC2617
-export([parse_proxy_authorization/1]).
% @todo -export([parse_proxy_support/1]). RFC4559
% @todo -export([parse_public_key_pins/1]). Key Pinning (upcoming)
% @todo -export([parse_public_key_pins_report_only/1]). Key Pinning (upcoming)
-export([parse_range/1]).
% @todo -export([parse_referer/1]). RFC7231
% @todo -export([parse_refresh/1]). Non-standard (examples: "5", "5; url=http://example.com/")
-export([parse_retry_after/1]).
-export([parse_sec_websocket_accept/1]).
-export([parse_sec_websocket_extensions/1]).
-export([parse_sec_websocket_key/1]).
% @todo -export([parse_sec_websocket_origin/1]). Websocket drafts 7 and 8
-export([parse_sec_websocket_protocol_req/1]).
-export([parse_sec_websocket_protocol_resp/1]).
-export([parse_sec_websocket_version_req/1]).
-export([parse_sec_websocket_version_resp/1]).
% @todo -export([parse_server/1]). RFC7231
% @todo -export([parse_set_cookie/1]). RFC6265
% @todo -export([parse_strict_transport_security/1]). RFC6797
% @todo -export([parse_tcn/1]). RFC2295
-export([parse_te/1]).
-export([parse_trailer/1]).
-export([parse_transfer_encoding/1]).
-export([parse_upgrade/1]).
% @todo -export([parse_user_agent/1]). RFC7231
% @todo -export([parse_variant_vary/1]). RFC2295
-export([parse_vary/1]).
% @todo -export([parse_via/1]). RFC7230
% @todo -export([parse_want_digest/1]). RFC3230
% @todo -export([parse_warning/1]). RFC7234
-export([parse_www_authenticate/1]).
% @todo -export([parse_x_content_duration/1]). Gecko/MDN (value: float)
% @todo -export([parse_x_dns_prefetch_control/1]). Various (value: "on"|"off")
-export([parse_x_forwarded_for/1]).
% @todo -export([parse_x_frame_options/1]). RFC7034

%% Building.
-export([access_control_allow_credentials/0]).
-export([access_control_allow_headers/1]).
-export([access_control_allow_methods/1]).
-export([access_control_allow_origin/1]).
-export([access_control_expose_headers/1]).
-export([access_control_max_age/1]).

-type etag() :: {weak | strong, binary()}.
-export_type([etag/0]).

-type media_type() :: {binary(), binary(), [{binary(), binary()}]}.
-export_type([media_type/0]).

-type qvalue() :: 0..1000.
-export_type([qvalue/0]).

-type websocket_version() :: 0..255.
-export_type([websocket_version/0]).

-include("cow_inline.hrl").
-include("cow_parse.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").

vector(Min, Max, Dom) -> ?LET(N, choose(Min, Max), vector(N, Dom)).
small_list(Dom) -> vector(0, 10, Dom).
small_non_empty_list(Dom) -> vector(1, 10, Dom).

alpha_chars() -> "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
alphanum_chars() -> "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
digit_chars() -> "0123456789".

ows() -> list(elements([$\s, $\t])).
alpha() -> elements(alpha_chars()).
alphanum() -> elements(alphanum_chars()).
digit() -> elements(digit_chars()).

tchar() ->
	frequency([
		{1, elements([$!, $#, $$, $%, $&, $', $*, $+, $-, $., $^, $_, $`, $|, $~])},
		{99, elements(alphanum_chars())}
	]).

token() ->
	?LET(T,
		non_empty(list(tchar())),
		list_to_binary(T)).

abnf_char() ->
	integer(1, 127).

vchar() ->
	integer(33, 126).

obs_text() ->
	integer(128, 255).

qdtext() ->
	frequency([
		{99, elements("\t\s!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")},
		{1, obs_text()}
	]).

quoted_pair() ->
	[$\\, frequency([
		{99, elements("\t\s!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")},
		{1, obs_text()}
	])].

quoted_string() ->
	[$", list(frequency([{100, qdtext()}, {1, quoted_pair()}])), $"].

%% Helper function for ( token / quoted-string ) values.
unquote([$", V, $"]) -> unquote(V, <<>>);
unquote(V) -> V.

unquote([], Acc) -> Acc;
unquote([[$\\, C]|Tail], Acc) -> unquote(Tail, << Acc/binary, C >>);
unquote([C|Tail], Acc) -> unquote(Tail, << Acc/binary, C >>).

parameter() ->
	?SUCHTHAT({K, _, _, _},
		{token(), oneof([token(), quoted_string()]), ows(), ows()},
		K =/= <<"q">>).

weight() ->
	frequency([
		{90, integer(0, 1000)},
		{10, undefined}
	]).

%% Helper function for weight's qvalue formatting.
qvalue_to_iodata(0) -> <<"0">>;
qvalue_to_iodata(Q) when Q < 10 -> [<<"0.00">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 100 -> [<<"0.0">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 1000 -> [<<"0.">>, integer_to_binary(Q)];
qvalue_to_iodata(1000) -> <<"1">>.
-endif.

%% Parsing.

%% @doc Parse the Accept header.

-spec parse_accept(binary()) -> [{media_type(), qvalue(), [binary() | {binary(), binary()}]}].
parse_accept(<<"*/*">>) ->
	[{{<<"*">>, <<"*">>, []}, 1000, []}];
parse_accept(Accept) ->
	media_range_list(Accept, []).

media_range_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) -> ?LOWER(media_range_type, R, Acc, <<>>);
media_range_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> media_range_list(R, Acc);
media_range_list(<<>>, Acc) -> lists:reverse(Acc).

media_range_type(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> ?LOWER(media_range_type, R, Acc, T);
media_range_type(<< $/, C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> ?LOWER(media_range_subtype, R, Acc, T, <<>>);
%% Special clause for badly behaving user agents that send * instead of */*.
media_range_type(<< $;, R/bits >>, Acc, <<"*">>) -> media_range_before_param(R, Acc, <<"*">>, <<"*">>, []).

media_range_subtype(<< C, R/bits >>, Acc, T, S) when ?IS_TOKEN(C) -> ?LOWER(media_range_subtype, R, Acc, T, S);
media_range_subtype(R, Acc, T, S) -> media_range_param_sep(R, Acc, T, S, []).

media_range_param_sep(<<>>, Acc, T, S, P) -> lists:reverse([{{T, S, lists:reverse(P)}, 1000, []}|Acc]);
media_range_param_sep(<< $,, R/bits >>, Acc, T, S, P) -> media_range_list(R, [{{T, S, lists:reverse(P)}, 1000, []}|Acc]);
media_range_param_sep(<< $;, R/bits >>, Acc, T, S, P) -> media_range_before_param(R, Acc, T, S, P);
media_range_param_sep(<< C, R/bits >>, Acc, T, S, P) when ?IS_WS(C) -> media_range_param_sep(R, Acc, T, S, P).

media_range_before_param(<< C, R/bits >>, Acc, T, S, P) when ?IS_WS(C) -> media_range_before_param(R, Acc, T, S, P);
media_range_before_param(<< $q, $=, R/bits >>, Acc, T, S, P) -> media_range_weight(R, Acc, T, S, P);
media_range_before_param(<< "charset=", $", R/bits >>, Acc, T, S, P) -> media_range_charset_quoted(R, Acc, T, S, P, <<>>);
media_range_before_param(<< "charset=", R/bits >>, Acc, T, S, P) -> media_range_charset(R, Acc, T, S, P, <<>>);
media_range_before_param(<< C, R/bits >>, Acc, T, S, P) when ?IS_TOKEN(C) -> ?LOWER(media_range_param, R, Acc, T, S, P, <<>>).

media_range_charset_quoted(<< $", R/bits >>, Acc, T, S, P, V) ->
	media_range_param_sep(R, Acc, T, S, [{<<"charset">>, V}|P]);
media_range_charset_quoted(<< $\\, C, R/bits >>, Acc, T, S, P, V) when ?IS_VCHAR_OBS(C) ->
	?LOWER(media_range_charset_quoted, R, Acc, T, S, P, V);
media_range_charset_quoted(<< C, R/bits >>, Acc, T, S, P, V) when ?IS_VCHAR_OBS(C) ->
	?LOWER(media_range_charset_quoted, R, Acc, T, S, P, V).

media_range_charset(<< C, R/bits >>, Acc, T, S, P, V) when ?IS_TOKEN(C) ->
	?LOWER(media_range_charset, R, Acc, T, S, P, V);
media_range_charset(R, Acc, T, S, P, V) ->
	media_range_param_sep(R, Acc, T, S, [{<<"charset">>, V}|P]).

media_range_param(<< $=, $", R/bits >>, Acc, T, S, P, K) -> media_range_quoted(R, Acc, T, S, P, K, <<>>);
media_range_param(<< $=, C, R/bits >>, Acc, T, S, P, K) when ?IS_TOKEN(C) -> media_range_value(R, Acc, T, S, P, K, << C >>);
media_range_param(<< C, R/bits >>, Acc, T, S, P, K) when ?IS_TOKEN(C) -> ?LOWER(media_range_param, R, Acc, T, S, P, K).

media_range_quoted(<< $", R/bits >>, Acc, T, S, P, K, V) -> media_range_param_sep(R, Acc, T, S, [{K, V}|P]);
media_range_quoted(<< $\\, C, R/bits >>, Acc, T, S, P, K, V) when ?IS_VCHAR_OBS(C) -> media_range_quoted(R, Acc, T, S, P, K, << V/binary, C >>);
media_range_quoted(<< C, R/bits >>, Acc, T, S, P, K, V) when ?IS_VCHAR_OBS(C) -> media_range_quoted(R, Acc, T, S, P, K, << V/binary, C >>).

media_range_value(<< C, R/bits >>, Acc, T, S, P, K, V) when ?IS_TOKEN(C) -> media_range_value(R, Acc, T, S, P, K, << V/binary, C >>);
media_range_value(R, Acc, T, S, P, K, V) -> media_range_param_sep(R, Acc, T, S, [{K, V}|P]).

media_range_weight(<< "1.000", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 1000, []);
media_range_weight(<< "1.00", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 1000, []);
media_range_weight(<< "1.0", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 1000, []);
media_range_weight(<< "1.", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 1000, []);
media_range_weight(<< "1", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 1000, []);
media_range_weight(<< "0.", A, B, C, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100 + (B - $0) * 10 + (C - $0), []);
media_range_weight(<< "0.", A, B, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100 + (B - $0) * 10, []);
media_range_weight(<< "0.", A, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100, []);
media_range_weight(<< "0.", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 0, []);
media_range_weight(<< "0", R/bits >>, Acc, T, S, P) -> accept_ext_sep(R, Acc, T, S, P, 0, []);
%% Special clauses for badly behaving user agents that send .123 instead of 0.123.
media_range_weight(<< ".", A, B, C, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100 + (B - $0) * 10 + (C - $0), []);
media_range_weight(<< ".", A, B, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100 + (B - $0) * 10, []);
media_range_weight(<< ".", A, R/bits >>, Acc, T, S, P) when ?IS_DIGIT(A) ->
	accept_ext_sep(R, Acc, T, S, P, (A - $0) * 100, []).

accept_ext_sep(<<>>, Acc, T, S, P, Q, E) -> lists:reverse([{{T, S, lists:reverse(P)}, Q, lists:reverse(E)}|Acc]);
accept_ext_sep(<< $,, R/bits >>, Acc, T, S, P, Q, E) -> media_range_list(R, [{{T, S, lists:reverse(P)}, Q, lists:reverse(E)}|Acc]);
accept_ext_sep(<< $;, R/bits >>, Acc, T, S, P, Q, E) -> accept_before_ext(R, Acc, T, S, P, Q, E);
accept_ext_sep(<< C, R/bits >>, Acc, T, S, P, Q, E) when ?IS_WS(C) -> accept_ext_sep(R, Acc, T, S, P, Q, E).

accept_before_ext(<< C, R/bits >>, Acc, T, S, P, Q, E) when ?IS_WS(C) -> accept_before_ext(R, Acc, T, S, P, Q, E);
accept_before_ext(<< C, R/bits >>, Acc, T, S, P, Q, E) when ?IS_TOKEN(C) -> ?LOWER(accept_ext, R, Acc, T, S, P, Q, E, <<>>).

accept_ext(<< $=, $", R/bits >>, Acc, T, S, P, Q, E, K) -> accept_quoted(R, Acc, T, S, P, Q, E, K, <<>>);
accept_ext(<< $=, C, R/bits >>, Acc, T, S, P, Q, E, K) when ?IS_TOKEN(C) -> accept_value(R, Acc, T, S, P, Q, E, K, << C >>);
accept_ext(<< C, R/bits >>, Acc, T, S, P, Q, E, K) when ?IS_TOKEN(C) -> ?LOWER(accept_ext, R, Acc, T, S, P, Q, E, K);
accept_ext(R, Acc, T, S, P, Q, E, K) -> accept_ext_sep(R, Acc, T, S, P, Q, [K|E]).

accept_quoted(<< $", R/bits >>, Acc, T, S, P, Q, E, K, V) -> accept_ext_sep(R, Acc, T, S, P, Q, [{K, V}|E]);
accept_quoted(<< $\\, C, R/bits >>, Acc, T, S, P, Q, E, K, V) when ?IS_VCHAR_OBS(C) -> accept_quoted(R, Acc, T, S, P, Q, E, K, << V/binary, C >>);
accept_quoted(<< C, R/bits >>, Acc, T, S, P, Q, E, K, V) when ?IS_VCHAR_OBS(C) -> accept_quoted(R, Acc, T, S, P, Q, E, K, << V/binary, C >>).

accept_value(<< C, R/bits >>, Acc, T, S, P, Q, E, K, V) when ?IS_TOKEN(C) -> accept_value(R, Acc, T, S, P, Q, E, K, << V/binary, C >>);
accept_value(R, Acc, T, S, P, Q, E, K, V) -> accept_ext_sep(R, Acc, T, S, P, Q, [{K, V}|E]).

-ifdef(TEST).
accept_ext() ->
	oneof([token(), parameter()]).

accept_exts() ->
	frequency([
		{90, []},
		{10, small_list(accept_ext())}
	]).

accept_param() ->
	frequency([
		{90, parameter()},
		{10, {<<"charset">>, oneof([token(), quoted_string()]), <<>>, <<>>}}
	]).

accept_params() ->
	small_list(accept_param()).

accept() ->
	?LET({T, S, P, W, E},
		{token(), token(), accept_params(), weight(), accept_exts()},
		{T, S, P, W, E, iolist_to_binary([T, $/, S,
			[[OWS1, $;, OWS2, K, $=, V] || {K, V, OWS1, OWS2} <- P],
			case W of
				undefined -> [];
				_ -> [
					[<<";q=">>, qvalue_to_iodata(W)],
					[case Ext of
						{K, V, OWS1, OWS2} -> [OWS1, $;, OWS2, K, $=, V];
						K -> [$;, K]
					end || Ext <- E]]
			end])}
	).

prop_parse_accept() ->
	?FORALL(L,
		vector(1, 50, accept()),
		begin
			<< _, Accept/binary >> = iolist_to_binary([[$,, A] || {_, _, _, _, _, A} <- L]),
			ResL = parse_accept(Accept),
			CheckedL = [begin
				ExpectedP = [case ?LOWER(K) of
					<<"charset">> -> {<<"charset">>, ?LOWER(unquote(V))};
					LowK -> {LowK, unquote(V)}
				end || {K, V, _, _} <- P],
				ExpectedE = [case Ext of
					{K, V, _, _} -> {?LOWER(K), unquote(V)};
					K -> ?LOWER(K)
				end || Ext <- E],
				ResT =:= ?LOWER(T)
					andalso ResS =:= ?LOWER(S)
					andalso ResP =:= ExpectedP
					andalso (ResW =:= W orelse (W =:= undefined andalso ResW =:= 1000))
					andalso ((W =:= undefined andalso ResE =:= []) orelse (W =/= undefined andalso ResE =:= ExpectedE))
			end || {{T, S, P, W, E, _}, {{ResT, ResS, ResP}, ResW, ResE}} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end
	).

parse_accept_test_() ->
	Tests = [
		{<<>>, []},
		{<<"   ">>, []},
		{<<"audio/*; q=0.2, audio/basic">>, [
			{{<<"audio">>, <<"*">>, []}, 200, []},
			{{<<"audio">>, <<"basic">>, []}, 1000, []}
		]},
		{<<"text/plain; q=0.5, text/html, "
		   "text/x-dvi; q=0.8, text/x-c">>, [
		   {{<<"text">>, <<"plain">>, []}, 500, []},
		   {{<<"text">>, <<"html">>, []}, 1000, []},
		   {{<<"text">>, <<"x-dvi">>, []}, 800, []},
		   {{<<"text">>, <<"x-c">>, []}, 1000, []}
		]},
		{<<"text/*, text/html, text/html;level=1, */*">>, [
			{{<<"text">>, <<"*">>, []}, 1000, []},
			{{<<"text">>, <<"html">>, []}, 1000, []},
			{{<<"text">>, <<"html">>, [{<<"level">>, <<"1">>}]}, 1000, []},
			{{<<"*">>, <<"*">>, []}, 1000, []}
		]},
		{<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
		   "text/html;level=2;q=0.4, */*;q=0.5">>, [
		   {{<<"text">>, <<"*">>, []}, 300, []},
		   {{<<"text">>, <<"html">>, []}, 700, []},
		   {{<<"text">>, <<"html">>, [{<<"level">>, <<"1">>}]}, 1000, []},
		   {{<<"text">>, <<"html">>, [{<<"level">>, <<"2">>}]}, 400, []},
		   {{<<"*">>, <<"*">>, []}, 500, []}
		]},
		{<<"text/html;level=1;quoted=\"hi hi hi\";"
		   "q=0.123;standalone;complex=gits, text/plain">>, [
			{{<<"text">>, <<"html">>,
				[{<<"level">>, <<"1">>}, {<<"quoted">>, <<"hi hi hi">>}]}, 123,
				[<<"standalone">>, {<<"complex">>, <<"gits">>}]},
			{{<<"text">>, <<"plain">>, []}, 1000, []}
		]},
		{<<"text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2">>, [
			{{<<"text">>, <<"html">>, []}, 1000, []},
			{{<<"image">>, <<"gif">>, []}, 1000, []},
			{{<<"image">>, <<"jpeg">>, []}, 1000, []},
			{{<<"*">>, <<"*">>, []}, 200, []},
			{{<<"*">>, <<"*">>, []}, 200, []}
		]},
		{<<"text/plain; charset=UTF-8">>, [
			{{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]}, 1000, []}
		]}
	],
	[{V, fun() -> R = parse_accept(V) end} || {V, R} <- Tests].

parse_accept_error_test_() ->
	Tests = [
		<<"audio/basic, */;q=0.5">>,
		<<"audio/, audio/basic">>,
		<<"aud\tio/basic">>,
		<<"audio/basic;t=\"zero \\", 0, " woo\"">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_accept(V)) end} || V <- Tests].

horse_parse_accept() ->
	horse:repeat(20000,
		parse_accept(<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
			"text/html;level=2;q=0.4, */*;q=0.5">>)
	).
-endif.

%% @doc Parse the Accept-Charset header.

-spec parse_accept_charset(binary()) -> [{binary(), qvalue()}].
parse_accept_charset(Charset) ->
	nonempty(conneg_list(Charset, [])).

conneg_list(<<>>, Acc) -> lists:reverse(Acc);
conneg_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> conneg_list(R, Acc);
conneg_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) -> ?LOWER(conneg, R, Acc, <<>>).

conneg(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> ?LOWER(conneg, R, Acc, T);
conneg(R, Acc, T) -> conneg_param_sep(R, Acc, T).

conneg_param_sep(<<>>, Acc, T) -> lists:reverse([{T, 1000}|Acc]);
conneg_param_sep(<< $,, R/bits >>, Acc, T) -> conneg_list(R, [{T, 1000}|Acc]);
conneg_param_sep(<< $;, R/bits >>, Acc, T) -> conneg_before_weight(R, Acc, T);
conneg_param_sep(<< C, R/bits >>, Acc, T) when ?IS_WS(C) -> conneg_param_sep(R, Acc, T).

conneg_before_weight(<< C, R/bits >>, Acc, T) when ?IS_WS(C) -> conneg_before_weight(R, Acc, T);
conneg_before_weight(<< $q, $=, R/bits >>, Acc, T) -> conneg_weight(R, Acc, T);
%% Special clause for broken user agents that confuse ; and , separators.
conneg_before_weight(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> ?LOWER(conneg, R, [{T, 1000}|Acc], <<>>).

conneg_weight(<< "1.000", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 1000}|Acc]);
conneg_weight(<< "1.00", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 1000}|Acc]);
conneg_weight(<< "1.0", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 1000}|Acc]);
conneg_weight(<< "1.", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 1000}|Acc]);
conneg_weight(<< "1", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 1000}|Acc]);
conneg_weight(<< "0.", A, B, C, R/bits >>, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	conneg_list_sep(R, [{T, (A - $0) * 100 + (B - $0) * 10 + (C - $0)}|Acc]);
conneg_weight(<< "0.", A, B, R/bits >>, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
	conneg_list_sep(R, [{T, (A - $0) * 100 + (B - $0) * 10}|Acc]);
conneg_weight(<< "0.", A, R/bits >>, Acc, T) when ?IS_DIGIT(A) ->
	conneg_list_sep(R, [{T, (A - $0) * 100}|Acc]);
conneg_weight(<< "0.", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 0}|Acc]);
conneg_weight(<< "0", R/bits >>, Acc, T) -> conneg_list_sep(R, [{T, 0}|Acc]).

conneg_list_sep(<<>>, Acc) -> lists:reverse(Acc);
conneg_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> conneg_list_sep(R, Acc);
conneg_list_sep(<< $,, R/bits >>, Acc) -> conneg_list(R, Acc).

-ifdef(TEST).
accept_charset() ->
	?LET({C, W},
		{token(), weight()},
		{C, W, iolist_to_binary([C, case W of
			undefined -> [];
			_ -> [<<";q=">>, qvalue_to_iodata(W)]
		end])}
	).

prop_parse_accept_charset() ->
	?FORALL(L,
		non_empty(list(accept_charset())),
		begin
			<< _, AcceptCharset/binary >> = iolist_to_binary([[$,, A] || {_, _, A} <- L]),
			ResL = parse_accept_charset(AcceptCharset),
			CheckedL = [begin
				ResC =:= ?LOWER(Ch)
					andalso (ResW =:= W orelse (W =:= undefined andalso ResW =:= 1000))
			end || {{Ch, W, _}, {ResC, ResW}} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_accept_charset_test_() ->
	Tests = [
		{<<"iso-8859-5, unicode-1-1;q=0.8">>, [
			{<<"iso-8859-5">>, 1000},
			{<<"unicode-1-1">>, 800}
		]},
		%% Some user agents send this invalid value for the Accept-Charset header
		{<<"ISO-8859-1;utf-8;q=0.7,*;q=0.7">>, [
			{<<"iso-8859-1">>, 1000},
			{<<"utf-8">>, 700},
			{<<"*">>, 700}
		]}
	],
	[{V, fun() -> R = parse_accept_charset(V) end} || {V, R} <- Tests].

parse_accept_charset_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_accept_charset(V)) end} || V <- Tests].

horse_parse_accept_charset() ->
	horse:repeat(20000,
		parse_accept_charset(<<"iso-8859-5, unicode-1-1;q=0.8">>)
	).
-endif.

%% @doc Parse the Accept-Encoding header.

-spec parse_accept_encoding(binary()) -> [{binary(), qvalue()}].
parse_accept_encoding(Encoding) ->
	conneg_list(Encoding, []).

-ifdef(TEST).
accept_encoding() ->
	?LET({E, W},
		{token(), weight()},
		{E, W, iolist_to_binary([E, case W of
			undefined -> [];
			_ -> [<<";q=">>, qvalue_to_iodata(W)]
		end])}
	).

%% @todo This property seems useless, see prop_accept_charset.
prop_parse_accept_encoding() ->
	?FORALL(L,
		non_empty(list(accept_encoding())),
		begin
			<< _, AcceptEncoding/binary >> = iolist_to_binary([[$,, A] || {_, _, A} <- L]),
			ResL = parse_accept_encoding(AcceptEncoding),
			CheckedL = [begin
				ResE =:= ?LOWER(E)
					andalso (ResW =:= W orelse (W =:= undefined andalso ResW =:= 1000))
			end || {{E, W, _}, {ResE, ResW}} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_accept_encoding_test_() ->
	Tests = [
		{<<>>, []},
		{<<"*">>, [{<<"*">>, 1000}]},
		{<<"compress, gzip">>, [
			{<<"compress">>, 1000},
			{<<"gzip">>, 1000}
		]},
		{<<"compress;q=0.5, gzip;q=1.0">>, [
			{<<"compress">>, 500},
			{<<"gzip">>, 1000}
		]},
		{<<"gzip;q=1.0, identity; q=0.5, *;q=0">>, [
			{<<"gzip">>, 1000},
			{<<"identity">>, 500},
			{<<"*">>, 0}
		]}
	],
	[{V, fun() -> R = parse_accept_encoding(V) end} || {V, R} <- Tests].

horse_parse_accept_encoding() ->
	horse:repeat(20000,
		parse_accept_encoding(<<"gzip;q=1.0, identity; q=0.5, *;q=0">>)
	).
-endif.

%% @doc Parse the Accept-Language header.

-spec parse_accept_language(binary()) -> [{binary(), qvalue()}].
parse_accept_language(LanguageRange) ->
	nonempty(language_range_list(LanguageRange, [])).

language_range_list(<<>>, Acc) -> lists:reverse(Acc);
language_range_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> language_range_list(R, Acc);
language_range_list(<< $*, R/bits >>, Acc) -> language_range_param_sep(R, Acc, <<"*">>);
language_range_list(<< C, R/bits >>, Acc) when ?IS_ALPHA(C) ->
	?LOWER(language_range, R, Acc, 1, <<>>).

language_range(<< $-, C, R/bits >>, Acc, _, T) when ?IS_ALPHANUM(C) ->
	?LOWER(language_range_sub, R, Acc, 1, << T/binary, $- >>);
language_range(<< C, R/bits >>, Acc, N, T) when ?IS_ALPHA(C), N < 8 ->
	?LOWER(language_range, R, Acc, N + 1, T);
language_range(R, Acc, _, T) -> language_range_param_sep(R, Acc, T).

language_range_sub(<< $-, R/bits >>, Acc, _, T) -> language_range_sub(R, Acc, 0, << T/binary, $- >>);
language_range_sub(<< C, R/bits >>, Acc, N, T) when ?IS_ALPHANUM(C), N < 8 ->
	?LOWER(language_range_sub, R, Acc, N + 1, T);
language_range_sub(R, Acc, _, T) -> language_range_param_sep(R, Acc, T).

language_range_param_sep(<<>>, Acc, T) -> lists:reverse([{T, 1000}|Acc]);
language_range_param_sep(<< $,, R/bits >>, Acc, T) -> language_range_list(R, [{T, 1000}|Acc]);
language_range_param_sep(<< $;, R/bits >>, Acc, T) -> language_range_before_weight(R, Acc, T);
language_range_param_sep(<< C, R/bits >>, Acc, T) when ?IS_WS(C) -> language_range_param_sep(R, Acc, T).

language_range_before_weight(<< C, R/bits >>, Acc, T) when ?IS_WS(C) -> language_range_before_weight(R, Acc, T);
language_range_before_weight(<< $q, $=, R/bits >>, Acc, T) -> language_range_weight(R, Acc, T);
%% Special clause for broken user agents that confuse ; and , separators.
language_range_before_weight(<< C, R/bits >>, Acc, T) when ?IS_ALPHA(C) ->
	?LOWER(language_range, R, [{T, 1000}|Acc], 1, <<>>).

language_range_weight(<< "1.000", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 1000}|Acc]);
language_range_weight(<< "1.00", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 1000}|Acc]);
language_range_weight(<< "1.0", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 1000}|Acc]);
language_range_weight(<< "1.", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 1000}|Acc]);
language_range_weight(<< "1", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 1000}|Acc]);
language_range_weight(<< "0.", A, B, C, R/bits >>, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	language_range_list_sep(R, [{T, (A - $0) * 100 + (B - $0) * 10 + (C - $0)}|Acc]);
language_range_weight(<< "0.", A, B, R/bits >>, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
	language_range_list_sep(R, [{T, (A - $0) * 100 + (B - $0) * 10}|Acc]);
language_range_weight(<< "0.", A, R/bits >>, Acc, T) when ?IS_DIGIT(A) ->
	language_range_list_sep(R, [{T, (A - $0) * 100}|Acc]);
language_range_weight(<< "0.", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 0}|Acc]);
language_range_weight(<< "0", R/bits >>, Acc, T) -> language_range_list_sep(R, [{T, 0}|Acc]).

language_range_list_sep(<<>>, Acc) -> lists:reverse(Acc);
language_range_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> language_range_list_sep(R, Acc);
language_range_list_sep(<< $,, R/bits >>, Acc) -> language_range_list(R, Acc).

-ifdef(TEST).
language_range_tag() ->
	vector(1, 8, alpha()).

language_range_subtag() ->
	[$-, vector(1, 8, alphanum())].

language_range() ->
	[language_range_tag(), small_list(language_range_subtag())].

accept_language() ->
	?LET({R, W},
		{language_range(), weight()},
		{iolist_to_binary(R), W, iolist_to_binary([R, case W of
			undefined -> [];
			_ -> [<<";q=">>, qvalue_to_iodata(W)]
		end])}
	).

prop_parse_accept_language() ->
	?FORALL(L,
		non_empty(list(accept_language())),
		begin
			<< _, AcceptLanguage/binary >> = iolist_to_binary([[$,, A] || {_, _, A} <- L]),
			ResL = parse_accept_language(AcceptLanguage),
			CheckedL = [begin
				ResR =:= ?LOWER(R)
					andalso (ResW =:= W orelse (W =:= undefined andalso ResW =:= 1000))
			end || {{R, W, _}, {ResR, ResW}} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_accept_language_test_() ->
	Tests = [
		{<<"da, en-gb;q=0.8, en;q=0.7">>, [
			{<<"da">>, 1000},
			{<<"en-gb">>, 800},
			{<<"en">>, 700}
		]},
		{<<"en, en-US, en-cockney, i-cherokee, x-pig-latin, es-419">>, [
			{<<"en">>, 1000},
			{<<"en-us">>, 1000},
			{<<"en-cockney">>, 1000},
			{<<"i-cherokee">>, 1000},
			{<<"x-pig-latin">>, 1000},
			{<<"es-419">>, 1000}
		]}
	],
	[{V, fun() -> R = parse_accept_language(V) end} || {V, R} <- Tests].

parse_accept_language_error_test_() ->
	Tests = [
		<<>>,
		<<"loooooong">>,
		<<"en-us-loooooong">>,
		<<"419-en-us">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_accept_language(V)) end} || V <- Tests].

horse_parse_accept_language() ->
	horse:repeat(20000,
		parse_accept_language(<<"da, en-gb;q=0.8, en;q=0.7">>)
	).
-endif.

%% @doc Parse the Accept-Ranges header.

-spec parse_accept_ranges(binary()) -> [binary()].
parse_accept_ranges(<<"none">>) -> [];
parse_accept_ranges(<<"bytes">>) -> [<<"bytes">>];
parse_accept_ranges(AcceptRanges) ->
	nonempty(token_ci_list(AcceptRanges, [])).

-ifdef(TEST).
parse_accept_ranges_test_() ->
	Tests = [
		{<<"bytes">>, [<<"bytes">>]},
		{<<"none">>, []},
		{<<"bytes, pages, kilos">>, [<<"bytes">>, <<"pages">>, <<"kilos">>]}
	],
	[{V, fun() -> R = parse_accept_ranges(V) end} || {V, R} <- Tests].

parse_accept_ranges_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_accept_ranges(V)) end} || V <- Tests].

horse_parse_accept_ranges_none() ->
	horse:repeat(200000,
		parse_accept_ranges(<<"none">>)
	).

horse_parse_accept_ranges_bytes() ->
	horse:repeat(200000,
		parse_accept_ranges(<<"bytes">>)
	).

horse_parse_accept_ranges_other() ->
	horse:repeat(200000,
		parse_accept_ranges(<<"bytes, pages, kilos">>)
	).
-endif.

%% @doc Parse the Access-Control-Request-Headers header.

-spec parse_access_control_request_headers(binary()) -> [binary()].
parse_access_control_request_headers(Headers) ->
	token_ci_list(Headers, []).

-ifdef(TEST).
headers() ->
	?LET(L,
		list({ows(), ows(), token()}),
		case L of
			[] -> {[], <<>>};
			_ ->
				<< _, Headers/binary >> = iolist_to_binary([[OWS1, $,, OWS2, M] || {OWS1, OWS2, M} <- L]),
				{[?LOWER(M) || {_, _, M} <- L], Headers}
		end).

prop_parse_access_control_request_headers() ->
	?FORALL({L, Headers},
		headers(),
		L =:= parse_access_control_request_headers(Headers)).

parse_access_control_request_headers_test_() ->
	Tests = [
		{<<>>, []},
		{<<"Content-Type">>, [<<"content-type">>]},
		{<<"accept, authorization, content-type">>, [<<"accept">>, <<"authorization">>, <<"content-type">>]},
		{<<"accept,, , authorization,content-type">>, [<<"accept">>, <<"authorization">>, <<"content-type">>]}
	],
	[{V, fun() -> R = parse_access_control_request_headers(V) end} || {V, R} <- Tests].

horse_parse_access_control_request_headers() ->
	horse:repeat(200000,
		parse_access_control_request_headers(<<"accept, authorization, content-type">>)
	).
-endif.

%% @doc Parse the Access-Control-Request-Method header.

-spec parse_access_control_request_method(binary()) -> binary().
parse_access_control_request_method(Method) ->
	true = <<>> =/= Method,
	ok = validate_token(Method),
	Method.

validate_token(<< C, R/bits >>) when ?IS_TOKEN(C) -> validate_token(R);
validate_token(<<>>) -> ok. 

-ifdef(TEST).
parse_access_control_request_method_test_() ->
	Tests = [
		<<"GET">>,
		<<"HEAD">>,
		<<"POST">>,
		<<"PUT">>,
		<<"DELETE">>,
		<<"TRACE">>,
		<<"CONNECT">>,
		<<"whatever">>
	],
	[{V, fun() -> R = parse_access_control_request_method(V) end} || {V, R} <- Tests].

parse_access_control_request_method_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_access_control_request_method(V)) end} || V <- Tests].

horse_parse_access_control_request_method() ->
	horse:repeat(200000,
		parse_access_control_request_method(<<"POST">>)
	).
-endif.

%% @doc Parse the Age header.

-spec parse_age(binary()) -> non_neg_integer().
parse_age(Age) ->
	I = binary_to_integer(Age),
	true = I >= 0,
	I.

-ifdef(TEST).
parse_age_test_() ->
	Tests = [
		{<<"0">>, 0},
		{<<"42">>, 42},
		{<<"69">>, 69},
		{<<"1337">>, 1337},
		{<<"3495">>, 3495},
		{<<"1234567890">>, 1234567890}
	],
	[{V, fun() -> R = parse_age(V) end} || {V, R} <- Tests].

parse_age_error_test_() ->
	Tests = [
		<<>>,
		<<"123, 123">>,
		<<"4.17">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_age(V)) end} || V <- Tests].
-endif.

%% @doc Parse the Allow header.

-spec parse_allow(binary()) -> [binary()].
parse_allow(Allow) ->
	token_list(Allow, []).

-ifdef(TEST).
allow() ->
	?LET(L,
		list({ows(), ows(), token()}),
		case L of
			[] -> {[], <<>>};
			_ ->
				<< _, Allow/binary >> = iolist_to_binary([[OWS1, $,, OWS2, M] || {OWS1, OWS2, M} <- L]),
				{[M || {_, _, M} <- L], Allow}
		end).

prop_parse_allow() ->
	?FORALL({L, Allow},
		allow(),
		L =:= parse_allow(Allow)).

parse_allow_test_() ->
	Tests = [
		{<<>>, []},
		{<<"GET, HEAD, PUT">>, [<<"GET">>, <<"HEAD">>, <<"PUT">>]}
	],
	[{V, fun() -> R = parse_allow(V) end} || {V, R} <- Tests].

horse_parse_allow() ->
	horse:repeat(200000,
		parse_allow(<<"GET, HEAD, PUT">>)
	).
-endif.

%% @doc Parse the Authorization header.
%%
%% We support Basic, Digest and Bearer schemes only.
%%
%% In the Digest case we do not validate that the mandatory
%% fields are present. When parsing auth-params, we do not
%% accept BWS characters around the "=".

-spec parse_authorization(binary())
	-> {basic, binary(), binary()}
	| {bearer, binary()}
	| {digest, [{binary(), binary()}]}.
parse_authorization(<<B, A, S, I, C, " ", R/bits >>)
		when ((B =:= $B) or (B =:= $b)), ((A =:= $A) or (A =:= $a)),
			((S =:= $S) or (S =:= $s)), ((I =:= $I) or (I =:= $i)),
			((C =:= $C) or (C =:= $c)) ->
	auth_basic(base64:decode(R), <<>>);
parse_authorization(<<B, E1, A, R1, E2, R2, " ", R/bits >>)
		when (R =/= <<>>), ((B =:= $B) or (B =:= $b)),
			((E1 =:= $E) or (E1 =:= $e)), ((A =:= $A) or (A =:= $a)),
			((R1 =:= $R) or (R1 =:= $r)), ((E2 =:= $E) or (E2 =:= $e)),
			((R2 =:= $R) or (R2 =:= $r)) ->
	validate_auth_bearer(R),
	{bearer, R};
parse_authorization(<<D, I, G, E, S, T, " ", R/bits >>)
		when ((D =:= $D) or (D =:= $d)), ((I =:= $I) or (I =:= $i)),
			((G =:= $G) or (G =:= $g)), ((E =:= $E) or (E =:= $e)),
			((S =:= $S) or (S =:= $s)), ((T =:= $T) or (T =:= $t)) ->
	{digest, nonempty(auth_digest_list(R, []))}.

auth_basic(<< $:, Password/bits >>, UserID) -> {basic, UserID, Password};
auth_basic(<< C, R/bits >>, UserID) -> auth_basic(R, << UserID/binary, C >>).

validate_auth_bearer(<< C, R/bits >>) when ?IS_TOKEN68(C) -> validate_auth_bearer(R);
validate_auth_bearer(<< $=, R/bits >>) -> validate_auth_bearer_eq(R);
validate_auth_bearer(<<>>) -> ok.

validate_auth_bearer_eq(<< $=, R/bits >>) -> validate_auth_bearer_eq(R);
validate_auth_bearer_eq(<<>>) -> ok.

auth_digest_list(<<>>, Acc) -> lists:reverse(Acc);
auth_digest_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> auth_digest_list(R, Acc);
auth_digest_list(<< "algorithm=", C, R/bits >>, Acc) when ?IS_TOKEN(C) -> auth_digest_token(R, Acc, <<"algorithm">>, << C >>);
auth_digest_list(<< "cnonce=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"cnonce">>, <<>>);
auth_digest_list(<< "nc=", A, B, C, D, E, F, G, H, R/bits >>, Acc)
		when ?IS_LHEX(A), ?IS_LHEX(B), ?IS_LHEX(C), ?IS_LHEX(D),
			?IS_LHEX(E), ?IS_LHEX(F), ?IS_LHEX(G), ?IS_LHEX(H) ->
	auth_digest_list_sep(R, [{<<"nc">>, << A, B, C, D, E, F, G, H >>}|Acc]);
auth_digest_list(<< "nonce=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"nonce">>, <<>>);
auth_digest_list(<< "opaque=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"opaque">>, <<>>);
auth_digest_list(<< "qop=", C, R/bits >>, Acc) when ?IS_TOKEN(C) -> auth_digest_token(R, Acc, <<"qop">>, << C >>);
auth_digest_list(<< "realm=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"realm">>, <<>>);
auth_digest_list(<< "response=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"response">>, <<>>);
auth_digest_list(<< "uri=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"uri">>, <<>>);
auth_digest_list(<< "username=\"", R/bits >>, Acc) -> auth_digest_quoted(R, Acc, <<"username">>, <<>>);
auth_digest_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) ->
	?LOWER(auth_digest_param, R, Acc, <<>>).

auth_digest_param(<< $=, $", R/bits >>, Acc, K) -> auth_digest_quoted(R, Acc, K, <<>>);
auth_digest_param(<< $=, C, R/bits >>, Acc, K) when ?IS_TOKEN(C) -> auth_digest_token(R, Acc, K, << C >>);
auth_digest_param(<< C, R/bits >>, Acc, K) when ?IS_TOKEN(C) ->
	?LOWER(auth_digest_param, R, Acc, K).

auth_digest_token(<< C, R/bits >>, Acc, K, V) when ?IS_TOKEN(C) -> auth_digest_token(R, Acc, K, << V/binary, C >>);
auth_digest_token(R, Acc, K, V) -> auth_digest_list_sep(R, [{K, V}|Acc]).

auth_digest_quoted(<< $", R/bits >>, Acc, K, V) -> auth_digest_list_sep(R, [{K, V}|Acc]);
auth_digest_quoted(<< $\\, C, R/bits >>, Acc, K, V) when ?IS_VCHAR_OBS(C) -> auth_digest_quoted(R, Acc, K, << V/binary, C >>);
auth_digest_quoted(<< C, R/bits >>, Acc, K, V) when ?IS_VCHAR_OBS(C) -> auth_digest_quoted(R, Acc, K, << V/binary, C >>).

auth_digest_list_sep(<<>>, Acc) -> lists:reverse(Acc);
auth_digest_list_sep(<< $,, R/bits >>, Acc) -> auth_digest_list(R, Acc);
auth_digest_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> auth_digest_list_sep(R, Acc).

-ifdef(TEST).
parse_authorization_test_() ->
	Tests = [
		{<<"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>, {basic, <<"Aladdin">>, <<"open sesame">>}},
		{<<"bAsIc QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>, {basic, <<"Aladdin">>, <<"open sesame">>}},
		{<<"Bearer mF_9.B5f-4.1JqM">>, {bearer, <<"mF_9.B5f-4.1JqM">>}},
		{<<"bEaRer mF_9.B5f-4.1JqM">>, {bearer, <<"mF_9.B5f-4.1JqM">>}},
		{<<"Digest username=\"Mufasa\","
				"realm=\"testrealm@host.com\","
				"nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\","
				"uri=\"/dir/index.html\","
				"qop=auth,"
				"nc=00000001,"
				"cnonce=\"0a4f113b\","
				"response=\"6629fae49393a05397450978507c4ef1\","
				"opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"">>,
			{digest, [
				{<<"username">>, <<"Mufasa">>},
				{<<"realm">>, <<"testrealm@host.com">>},
				{<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>},
				{<<"uri">>, <<"/dir/index.html">>},
				{<<"qop">>, <<"auth">>},
				{<<"nc">>, <<"00000001">>},
				{<<"cnonce">>, <<"0a4f113b">>},
				{<<"response">>, <<"6629fae49393a05397450978507c4ef1">>},
				{<<"opaque">>, <<"5ccc069c403ebaf9f0171e9517f40e41">>}]}}
	],
	[{V, fun() -> R = parse_authorization(V) end} || {V, R} <- Tests].

horse_parse_authorization_basic() ->
	horse:repeat(20000,
		parse_authorization(<<"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==">>)
	).

horse_parse_authorization_bearer() ->
	horse:repeat(20000,
		parse_authorization(<<"Bearer mF_9.B5f-4.1JqM">>)
	).

horse_parse_authorization_digest() ->
	horse:repeat(20000,
		parse_authorization(
			<<"Digest username=\"Mufasa\","
				"realm=\"testrealm@host.com\","
				"nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\","
				"uri=\"/dir/index.html\","
				"qop=auth,"
				"nc=00000001,"
				"cnonce=\"0a4f113b\","
				"response=\"6629fae49393a05397450978507c4ef1\","
				"opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"">>)
	).
-endif.

%% @doc Parse the Cache-Control header.
%%
%% In the fields list case, we do not support escaping, which shouldn't be needed anyway.

-spec parse_cache_control(binary())
	-> [binary() | {binary(), binary()} | {binary(), non_neg_integer()} | {binary(), [binary()]}].
parse_cache_control(<<"no-cache">>) ->
	[<<"no-cache">>];
parse_cache_control(<<"max-age=0">>) ->
	[{<<"max-age">>, 0}];
parse_cache_control(CacheControl) ->
	nonempty(cache_directive_list(CacheControl, [])).

cache_directive_list(<<>>, Acc) -> lists:reverse(Acc);
cache_directive_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C)-> cache_directive_list(R, Acc);
cache_directive_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) ->
	?LOWER(cache_directive, R, Acc, <<>>).

cache_directive(<< $=, $", R/bits >>, Acc, T)
		when (T =:= <<"no-cache">>) or (T =:= <<"private">>) ->
	cache_directive_fields_list(R, Acc, T, []);
cache_directive(<< $=, C, R/bits >>, Acc, T)
		when ?IS_DIGIT(C), (T =:= <<"max-age">>) or (T =:= <<"max-stale">>)
			or (T =:= <<"min-fresh">>) or (T =:= <<"s-maxage">>) ->
	cache_directive_delta(R, Acc, T, (C - $0));
cache_directive(<< $=, $", R/bits >>, Acc, T) -> cache_directive_quoted_string(R, Acc, T, <<>>);
cache_directive(<< $=, C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> cache_directive_token(R, Acc, T, << C >>);
cache_directive(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) ->
	?LOWER(cache_directive, R, Acc, T);
cache_directive(R, Acc, T) -> cache_directive_list_sep(R, [T|Acc]).

cache_directive_delta(<< C, R/bits >>, Acc, K, V) when ?IS_DIGIT(C) -> cache_directive_delta(R, Acc, K, V * 10 + (C - $0));
cache_directive_delta(R, Acc, K, V) -> cache_directive_list_sep(R, [{K, V}|Acc]).

cache_directive_fields_list(<< C, R/bits >>, Acc, K, L) when ?IS_WS_COMMA(C) -> cache_directive_fields_list(R, Acc, K, L);
cache_directive_fields_list(<< $", R/bits >>, Acc, K, L) -> cache_directive_list_sep(R, [{K, lists:reverse(L)}|Acc]);
cache_directive_fields_list(<< C, R/bits >>, Acc, K, L) when ?IS_TOKEN(C) ->
	?LOWER(cache_directive_field, R, Acc, K, L, <<>>).

cache_directive_field(<< C, R/bits >>, Acc, K, L, F) when ?IS_TOKEN(C) ->
	?LOWER(cache_directive_field, R, Acc, K, L, F);
cache_directive_field(R, Acc, K, L, F) -> cache_directive_fields_list_sep(R, Acc, K, [F|L]).

cache_directive_fields_list_sep(<< C, R/bits >>, Acc, K, L) when ?IS_WS(C) -> cache_directive_fields_list_sep(R, Acc, K, L);
cache_directive_fields_list_sep(<< $,, R/bits >>, Acc, K, L) -> cache_directive_fields_list(R, Acc, K, L);
cache_directive_fields_list_sep(<< $", R/bits >>, Acc, K, L) -> cache_directive_list_sep(R, [{K, lists:reverse(L)}|Acc]).

cache_directive_token(<< C, R/bits >>, Acc, K, V) when ?IS_TOKEN(C) -> cache_directive_token(R, Acc, K, << V/binary, C >>);
cache_directive_token(R, Acc, K, V) -> cache_directive_list_sep(R, [{K, V}|Acc]).

cache_directive_quoted_string(<< $", R/bits >>, Acc, K, V) -> cache_directive_list_sep(R, [{K, V}|Acc]);
cache_directive_quoted_string(<< $\\, C, R/bits >>, Acc, K, V) when ?IS_VCHAR_OBS(C) ->
	cache_directive_quoted_string(R, Acc, K, << V/binary, C >>);
cache_directive_quoted_string(<< C, R/bits >>, Acc, K, V) when ?IS_VCHAR_OBS(C) ->
	cache_directive_quoted_string(R, Acc, K, << V/binary, C >>).

cache_directive_list_sep(<<>>, Acc) -> lists:reverse(Acc);
cache_directive_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> cache_directive_list_sep(R, Acc);
cache_directive_list_sep(<< $,, R/bits >>, Acc) -> cache_directive_list(R, Acc).

-ifdef(TEST).
cache_directive_unreserved_token() ->
	?SUCHTHAT(T,
		token(),
		T =/= <<"max-age">> andalso T =/= <<"max-stale">> andalso T =/= <<"min-fresh">>
			andalso T =/= <<"s-maxage">> andalso T =/= <<"no-cache">> andalso T =/= <<"private">>).

cache_directive() ->
	oneof([
		token(),
		{cache_directive_unreserved_token(), token()},
		{cache_directive_unreserved_token(), quoted_string()},
		{elements([<<"max-age">>, <<"max-stale">>, <<"min-fresh">>, <<"s-maxage">>]), non_neg_integer()},
		{fields, elements([<<"no-cache">>, <<"private">>]), small_list(token())}
	]).

cache_control() ->
	?LET(L,
		non_empty(list(cache_directive())),
		begin
			<< _, CacheControl/binary >> = iolist_to_binary([[$,,
				case C of
					{fields, K, V} -> [K, $=, $", [[F, $,] || F <- V], $"];
					{K, V} when is_integer(V) -> [K, $=, integer_to_binary(V)];
					{K, V} -> [K, $=, V];
					K -> K
				end] || C <- L]),
			{L, CacheControl}
		end).

prop_parse_cache_control() ->
	?FORALL({L, CacheControl},
		cache_control(),
		begin
			ResL = parse_cache_control(CacheControl),
			CheckedL = [begin
				ExpectedCc = case Cc of
					{fields, K, V} -> {?LOWER(K), [?LOWER(F) || F <- V]};
					{K, V} -> {?LOWER(K), unquote(V)};
					K -> ?LOWER(K)
				end,
				ExpectedCc =:= ResCc
			end || {Cc, ResCc} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_cache_control_test_() ->
	Tests = [
		{<<"no-cache">>, [<<"no-cache">>]},
		{<<"no-store">>, [<<"no-store">>]},
		{<<"max-age=0">>, [{<<"max-age">>, 0}]},
		{<<"max-age=30">>, [{<<"max-age">>, 30}]},
		{<<"private, community=\"UCI\"">>, [<<"private">>, {<<"community">>, <<"UCI">>}]},
		{<<"private=\"Content-Type, Content-Encoding, Content-Language\"">>,
			[{<<"private">>, [<<"content-type">>, <<"content-encoding">>, <<"content-language">>]}]}
	],
	[{V, fun() -> R = parse_cache_control(V) end} || {V, R} <- Tests].

parse_cache_control_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_cache_control(V)) end} || V <- Tests].

horse_parse_cache_control_no_cache() ->
	horse:repeat(200000,
		parse_cache_control(<<"no-cache">>)
	).

horse_parse_cache_control_max_age_0() ->
	horse:repeat(200000,
		parse_cache_control(<<"max-age=0">>)
	).

horse_parse_cache_control_max_age_30() ->
	horse:repeat(200000,
		parse_cache_control(<<"max-age=30">>)
	).

horse_parse_cache_control_custom() ->
	horse:repeat(200000,
		parse_cache_control(<<"private, community=\"UCI\"">>)
	).

horse_parse_cache_control_fields() ->
	horse:repeat(200000,
		parse_cache_control(<<"private=\"Content-Type, Content-Encoding, Content-Language\"">>)
	).
-endif.

%% @doc Parse the Connection header.

-spec parse_connection(binary()) -> [binary()].
parse_connection(<<"close">>) ->
	[<<"close">>];
parse_connection(<<"keep-alive">>) ->
	[<<"keep-alive">>];
parse_connection(Connection) ->
	nonempty(token_ci_list(Connection, [])).

-ifdef(TEST).
prop_parse_connection() ->
	?FORALL(L,
		non_empty(list(token())),
		begin
			<< _, Connection/binary >> = iolist_to_binary([[$,, C] || C <- L]),
			ResL = parse_connection(Connection),
			CheckedL = [?LOWER(Co) =:= ResC || {Co, ResC} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_connection_test_() ->
	Tests = [
		{<<"close">>, [<<"close">>]},
		{<<"ClOsE">>, [<<"close">>]},
		{<<"Keep-Alive">>, [<<"keep-alive">>]},
		{<<"keep-alive, Upgrade">>, [<<"keep-alive">>, <<"upgrade">>]}
	],
	[{V, fun() -> R = parse_connection(V) end} || {V, R} <- Tests].

parse_connection_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_connection(V)) end} || V <- Tests].

horse_parse_connection_close() ->
	horse:repeat(200000,
		parse_connection(<<"close">>)
	).

horse_parse_connection_keepalive() ->
	horse:repeat(200000,
		parse_connection(<<"keep-alive">>)
	).

horse_parse_connection_keepalive_upgrade() ->
	horse:repeat(200000,
		parse_connection(<<"keep-alive, upgrade">>)
	).
-endif.

%% @doc Parse the Content-Encoding header.

-spec parse_content_encoding(binary()) -> [binary()].
parse_content_encoding(ContentEncoding) ->
	nonempty(token_ci_list(ContentEncoding, [])).

-ifdef(TEST).
parse_content_encoding_test_() ->
	Tests = [
		{<<"gzip">>, [<<"gzip">>]}
	],
	[{V, fun() -> R = parse_content_encoding(V) end} || {V, R} <- Tests].

parse_content_encoding_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_content_encoding(V)) end} || V <- Tests].

horse_parse_content_encoding() ->
	horse:repeat(200000,
		parse_content_encoding(<<"gzip">>)
	).
-endif.

%% @doc Parse the Content-Language header.
%%
%% We do not support irregular deprecated tags that do not match the ABNF.

-spec parse_content_language(binary()) -> [binary()].
parse_content_language(ContentLanguage) ->
	nonempty(langtag_list(ContentLanguage, [])).

langtag_list(<<>>, Acc) -> lists:reverse(Acc);
langtag_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> langtag_list(R, Acc);
langtag_list(<< A, B, C, R/bits >>, Acc) when ?IS_ALPHA(A), ?IS_ALPHA(B), ?IS_ALPHA(C) ->
	langtag_extlang(R, Acc, << ?LC(A), ?LC(B), ?LC(C) >>, 0);
langtag_list(<< A, B, R/bits >>, Acc) when ?IS_ALPHA(A), ?IS_ALPHA(B) ->
	langtag_extlang(R, Acc, << ?LC(A), ?LC(B) >>, 0);
langtag_list(<< X, R/bits >>, Acc) when X =:= $x; X =:= $X -> langtag_privateuse_sub(R, Acc, << $x >>, 0).

langtag_extlang(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T, _)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>);
langtag_extlang(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T, _)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>);
langtag_extlang(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T, _)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>);
langtag_extlang(<< $-, A, B, C, D, E, R/bits >>, Acc, T, _)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>);
langtag_extlang(<< $-, A, B, C, D, R/bits >>, Acc, T, _)
		when ?IS_ALPHA(A), ?IS_ALPHA(B), ?IS_ALPHA(C), ?IS_ALPHA(D) ->
	langtag_region(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D) >>);
langtag_extlang(<< $-, A, B, C, R/bits >>, Acc, T, N)
		when ?IS_ALPHA(A), ?IS_ALPHA(B), ?IS_ALPHA(C) ->
	case N of
		2 -> langtag_script(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C) >>);
		_ -> langtag_extlang(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C) >>, N + 1)
	end;
langtag_extlang(R, Acc, T, _) -> langtag_region(R, Acc, T).

langtag_script(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>);
langtag_script(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>);
langtag_script(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>);
langtag_script(<< $-, A, B, C, D, E, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>);
langtag_script(<< $-, A, B, C, D, R/bits >>, Acc, T)
		when ?IS_ALPHA(A), ?IS_ALPHA(B), ?IS_ALPHA(C), ?IS_ALPHA(D) ->
	langtag_region(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D) >>);
langtag_script(R, Acc, T) ->
	langtag_region(R, Acc, T).

langtag_region(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>);
langtag_region(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>);
langtag_region(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>);
langtag_region(<< $-, A, B, C, D, E, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>);
langtag_region(<< $-, A, B, C, D, R/bits >>, Acc, T)
		when ?IS_DIGIT(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D) ->
	langtag_variant(R, Acc, << T/binary, $-, A, ?LC(B), ?LC(C), ?LC(D) >>);
langtag_region(<< $-, A, B, R/bits >>, Acc, T) when ?IS_ALPHA(A), ?IS_ALPHA(B) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B) >>);
langtag_region(<< $-, A, B, C, R/bits >>, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	langtag_variant(R, Acc, << T/binary, $-, A, B, C >>);
langtag_region(R, Acc, T) ->
	langtag_variant(R, Acc, T).

langtag_variant(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>);
langtag_variant(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>);
langtag_variant(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>);
langtag_variant(<< $-, A, B, C, D, E, R/bits >>, Acc, T)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_variant(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>);
langtag_variant(<< $-, A, B, C, D, R/bits >>, Acc, T)
		when ?IS_DIGIT(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D) ->
	langtag_variant(R, Acc, << T/binary, $-, A, ?LC(B), ?LC(C), ?LC(D) >>);
langtag_variant(R, Acc, T) ->
	langtag_extension(R, Acc, T).

langtag_extension(<< $-, X, R/bits >>, Acc, T) when X =:= $x; X =:= $X -> langtag_privateuse_sub(R, Acc, << T/binary, $-, $x >>, 0);
langtag_extension(<< $-, S, R/bits >>, Acc, T) when ?IS_ALPHANUM(S) -> langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(S) >>, 0);
langtag_extension(R, Acc, T) -> langtag_list_sep(R, [T|Acc]).

langtag_extension_sub(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>, N + 1);
langtag_extension_sub(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>, N + 1);
langtag_extension_sub(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F) ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>, N + 1);
langtag_extension_sub(<< $-, A, B, C, D, E, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>, N + 1);
langtag_extension_sub(<< $-, A, B, C, D, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D)  ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D) >>, N + 1);
langtag_extension_sub(<< $-, A, B, C, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C)  ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C) >>, N + 1);
langtag_extension_sub(<< $-, A, B, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B)  ->
	langtag_extension_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B) >>, N + 1);
langtag_extension_sub(R, Acc, T, N) when N > 0 ->
	langtag_extension(R, Acc, T).

langtag_privateuse_sub(<< $-, A, B, C, D, E, F, G, H, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G), ?IS_ALPHANUM(H) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G), ?LC(H) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, C, D, E, F, G, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F), ?IS_ALPHANUM(G) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F), ?LC(G) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, C, D, E, F, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D),
			?IS_ALPHANUM(E), ?IS_ALPHANUM(F)  ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E), ?LC(F) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, C, D, E, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D), ?IS_ALPHANUM(E) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D), ?LC(E) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, C, D, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C), ?IS_ALPHANUM(D) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C), ?LC(D) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, C, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B), ?IS_ALPHANUM(C) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B), ?LC(C) >>, N + 1);
langtag_privateuse_sub(<< $-, A, B, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A), ?IS_ALPHANUM(B) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A), ?LC(B) >>, N + 1);
langtag_privateuse_sub(<< $-, A, R/bits >>, Acc, T, N)
		when ?IS_ALPHANUM(A) ->
	langtag_privateuse_sub(R, Acc, << T/binary, $-, ?LC(A) >>, N + 1);
langtag_privateuse_sub(R, Acc, T, N) when N > 0 -> langtag_list_sep(R, [T|Acc]).

langtag_list_sep(<<>>, Acc) -> lists:reverse(Acc);
langtag_list_sep(<< $,, R/bits >>, Acc) -> langtag_list(R, Acc);
langtag_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> langtag_list_sep(R, Acc).

-ifdef(TEST).
langtag_language() -> vector(2, 3, alpha()).
langtag_extlang() -> vector(0, 3, [$-, alpha(), alpha(), alpha()]).
langtag_script() -> oneof([[], [$-, alpha(), alpha(), alpha(), alpha()]]).
langtag_region() -> oneof([[], [$-, alpha(), alpha()], [$-, digit(), digit(), digit()]]).

langtag_variant() ->
	small_list(frequency([
		{4, [$-, vector(5, 8, alphanum())]},
		{1, [$-, digit(), alphanum(), alphanum(), alphanum()]}
	])).

langtag_extension() ->
	small_list([$-, ?SUCHTHAT(S, alphanum(), S =/= $x andalso S =/= $X),
		small_non_empty_list([$-, vector(2, 8, alphanum())])
	]).

langtag_privateuse() -> oneof([[], [$-, langtag_privateuse_nodash()]]).
langtag_privateuse_nodash() -> [elements([$x, $X]), small_non_empty_list([$-, vector(1, 8, alphanum())])].
private_language_tag() -> ?LET(T, langtag_privateuse_nodash(), iolist_to_binary(T)).

language_tag() ->
	?LET(IoList,
		[langtag_language(), langtag_extlang(), langtag_script(), langtag_region(),
			langtag_variant(), langtag_extension(), langtag_privateuse()],
		iolist_to_binary(IoList)).

content_language() ->
	?LET(L,
		non_empty(list(frequency([
			{90, language_tag()},
			{10, private_language_tag()}
		]))),
		begin
			<< _, ContentLanguage/binary >> = iolist_to_binary([[$,, T] || T <- L]),
			{L, ContentLanguage}
		end).

prop_parse_content_language() ->
	?FORALL({L, ContentLanguage},
		content_language(),
		begin
			ResL = parse_content_language(ContentLanguage),
			CheckedL = [?LOWER(T) =:= ResT || {T, ResT} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_content_language_test_() ->
	Tests = [
		{<<"de">>, [<<"de">>]},
		{<<"fr">>, [<<"fr">>]},
		{<<"ja">>, [<<"ja">>]},
		{<<"zh-Hant">>, [<<"zh-hant">>]},
		{<<"zh-Hans">>, [<<"zh-hans">>]},
		{<<"sr-Cyrl">>, [<<"sr-cyrl">>]},
		{<<"sr-Latn">>, [<<"sr-latn">>]},
		{<<"zh-cmn-Hans-CN">>, [<<"zh-cmn-hans-cn">>]},
		{<<"cmn-Hans-CN">>, [<<"cmn-hans-cn">>]},
		{<<"zh-yue-HK">>, [<<"zh-yue-hk">>]},
		{<<"yue-HK">>, [<<"yue-hk">>]},
		{<<"zh-Hans-CN">>, [<<"zh-hans-cn">>]},
		{<<"sr-Latn-RS">>, [<<"sr-latn-rs">>]},
		{<<"sl-rozaj">>, [<<"sl-rozaj">>]},
		{<<"sl-rozaj-biske">>, [<<"sl-rozaj-biske">>]},
		{<<"sl-nedis">>, [<<"sl-nedis">>]},
		{<<"de-CH-1901">>, [<<"de-ch-1901">>]},
		{<<"sl-IT-nedis">>, [<<"sl-it-nedis">>]},
		{<<"hy-Latn-IT-arevela">>, [<<"hy-latn-it-arevela">>]},
		{<<"de-DE">>, [<<"de-de">>]},
		{<<"en-US">>, [<<"en-us">>]},
		{<<"es-419">>, [<<"es-419">>]},
		{<<"de-CH-x-phonebk">>, [<<"de-ch-x-phonebk">>]},
		{<<"az-Arab-x-AZE-derbend">>, [<<"az-arab-x-aze-derbend">>]},
		{<<"x-whatever">>, [<<"x-whatever">>]},
		{<<"qaa-Qaaa-QM-x-southern">>, [<<"qaa-qaaa-qm-x-southern">>]},
		{<<"de-Qaaa">>, [<<"de-qaaa">>]},
		{<<"sr-Latn-QM">>, [<<"sr-latn-qm">>]},
		{<<"sr-Qaaa-RS">>, [<<"sr-qaaa-rs">>]},
		{<<"en-US-u-islamcal">>, [<<"en-us-u-islamcal">>]},
		{<<"zh-CN-a-myext-x-private">>, [<<"zh-cn-a-myext-x-private">>]},
		{<<"en-a-myext-b-another">>, [<<"en-a-myext-b-another">>]},
		{<<"mn-Cyrl-MN">>, [<<"mn-cyrl-mn">>]},
		{<<"MN-cYRL-mn">>, [<<"mn-cyrl-mn">>]},
		{<<"mN-cYrL-Mn">>, [<<"mn-cyrl-mn">>]},
		{<<"az-Arab-IR">>, [<<"az-arab-ir">>]},
		{<<"zh-gan">>, [<<"zh-gan">>]},
		{<<"zh-yue">>, [<<"zh-yue">>]},
		{<<"zh-cmn">>, [<<"zh-cmn">>]},
		{<<"de-AT">>, [<<"de-at">>]},
		{<<"de-CH-1996">>, [<<"de-ch-1996">>]},
		{<<"en-Latn-GB-boont-r-extended-sequence-x-private">>,
			[<<"en-latn-gb-boont-r-extended-sequence-x-private">>]},
		{<<"el-x-koine">>, [<<"el-x-koine">>]},
		{<<"el-x-attic">>, [<<"el-x-attic">>]},
		{<<"fr, en-US, es-419, az-Arab, x-pig-latin, man-Nkoo-GN">>,
			[<<"fr">>, <<"en-us">>, <<"es-419">>, <<"az-arab">>, <<"x-pig-latin">>, <<"man-nkoo-gn">>]},
		{<<"da">>, [<<"da">>]},
		{<<"mi, en">>, [<<"mi">>, <<"en">>]}
	],
	[{V, fun() -> R = parse_content_language(V) end} || {V, R} <- Tests].

parse_content_language_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_content_language(V)) end} || V <- Tests].

horse_parse_content_language() ->
	horse:repeat(100000,
		parse_content_language(<<"fr, en-US, es-419, az-Arab, x-pig-latin, man-Nkoo-GN">>)
	).
-endif.

%% @doc Parse the Content-Length header.

-spec parse_content_length(binary()) -> non_neg_integer().
parse_content_length(ContentLength) ->
	I = binary_to_integer(ContentLength),
	true = I >= 0,
	I.

-ifdef(TEST).
prop_parse_content_length() ->
	?FORALL(
		X,
		non_neg_integer(),
		X =:= parse_content_length(integer_to_binary(X))
	).

parse_content_length_test_() ->
	Tests = [
		{<<"0">>, 0},
		{<<"42">>, 42},
		{<<"69">>, 69},
		{<<"1337">>, 1337},
		{<<"3495">>, 3495},
		{<<"1234567890">>, 1234567890}
	],
	[{V, fun() -> R = parse_content_length(V) end} || {V, R} <- Tests].

parse_content_length_error_test_() ->
	Tests = [
		<<>>,
		<<"-1">>,
		<<"123, 123">>,
		<<"4.17">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_content_length(V)) end} || V <- Tests].

horse_parse_content_length_zero() ->
	horse:repeat(100000,
		parse_content_length(<<"0">>)
	).

horse_parse_content_length_giga() ->
	horse:repeat(100000,
		parse_content_length(<<"1234567890">>)
	).
-endif.

%% @doc Parse the Content-Range header.

-spec parse_content_range(binary())
	-> {bytes, non_neg_integer(), non_neg_integer(), non_neg_integer() | '*'}
	| {bytes, '*', non_neg_integer()} | {binary(), binary()}.
parse_content_range(<<"bytes */", C, R/bits >>) when ?IS_DIGIT(C) -> unsatisfied_range(R, C - $0);
parse_content_range(<<"bytes ", C, R/bits >>) when ?IS_DIGIT(C) -> byte_range_first(R, C - $0);
parse_content_range(<< C, R/bits >>) when ?IS_TOKEN(C) ->
	?LOWER(other_content_range_unit, R, <<>>).

byte_range_first(<< $-, C, R/bits >>, First) when ?IS_DIGIT(C) -> byte_range_last(R, First, C - $0);
byte_range_first(<< C, R/bits >>, First) when ?IS_DIGIT(C) -> byte_range_first(R, First * 10 + C - $0).

byte_range_last(<<"/*">>, First, Last) -> {bytes, First, Last, '*'};
byte_range_last(<< $/, C, R/bits >>, First, Last) when ?IS_DIGIT(C) -> byte_range_complete(R, First, Last, C - $0);
byte_range_last(<< C, R/bits >>, First, Last) when ?IS_DIGIT(C) -> byte_range_last(R, First, Last * 10 + C - $0).

byte_range_complete(<<>>, First, Last, Complete) -> {bytes, First, Last, Complete};
byte_range_complete(<< C, R/bits >>, First, Last, Complete) when ?IS_DIGIT(C) ->
	byte_range_complete(R, First, Last, Complete * 10 + C - $0).

unsatisfied_range(<<>>, Complete) -> {bytes, '*', Complete};
unsatisfied_range(<< C, R/bits >>, Complete) when ?IS_DIGIT(C) -> unsatisfied_range(R, Complete * 10 + C - $0).

other_content_range_unit(<< $\s, R/bits >>, Unit) -> other_content_range_resp(R, Unit, <<>>);
other_content_range_unit(<< C, R/bits >>, Unit) when ?IS_TOKEN(C) ->
	?LOWER(other_content_range_unit, R, Unit).

other_content_range_resp(<<>>, Unit, Resp) -> {Unit, Resp};
other_content_range_resp(<< C, R/bits >>, Unit, Resp) when ?IS_CHAR(C) -> other_content_range_resp(R, Unit, << Resp/binary, C >>).

-ifdef(TEST).
content_range() ->
	?LET(ContentRange,
		oneof([
			?SUCHTHAT({bytes, First, Last, Complete},
				{bytes, non_neg_integer(), non_neg_integer(), non_neg_integer()},
				First =< Last andalso Last < Complete),
			?SUCHTHAT({bytes, First, Last, '*'},
				{bytes, non_neg_integer(), non_neg_integer(), '*'},
				First =< Last),
			{bytes, '*', non_neg_integer()},
			{token(), ?LET(L, list(abnf_char()), list_to_binary(L))}
		]),
		{case ContentRange of
			{Unit, Resp} when is_binary(Unit) -> {?LOWER(Unit), Resp};
			_ -> ContentRange
		end, case ContentRange of
			{bytes, First, Last, '*'} ->
				<< "bytes ", (integer_to_binary(First))/binary, "-",
					(integer_to_binary(Last))/binary, "/*">>;
			{bytes, First, Last, Complete} ->
				<< "bytes ", (integer_to_binary(First))/binary, "-",
					(integer_to_binary(Last))/binary, "/", (integer_to_binary(Complete))/binary >>;
			{bytes, '*', Complete} ->
				<< "bytes */", (integer_to_binary(Complete))/binary >>;
			{Unit, Resp} ->
				<< Unit/binary, $\s, Resp/binary >>
		end}).

prop_parse_content_range() ->
	?FORALL({Res, ContentRange},
		content_range(),
		Res =:= parse_content_range(ContentRange)).

parse_content_range_test_() ->
	Tests = [
		{<<"bytes 21010-47021/47022">>, {bytes, 21010, 47021, 47022}},
		{<<"bytes 500-999/8000">>, {bytes, 500, 999, 8000}},
		{<<"bytes 7000-7999/8000">>, {bytes, 7000, 7999, 8000}},
		{<<"bytes 42-1233/1234">>, {bytes, 42, 1233, 1234}},
		{<<"bytes 42-1233/*">>, {bytes, 42, 1233, '*'}},
		{<<"bytes */1234">>, {bytes, '*', 1234}},
		{<<"bytes 0-499/1234">>, {bytes, 0, 499, 1234}},
		{<<"bytes 500-999/1234">>, {bytes, 500, 999, 1234}},
		{<<"bytes 500-1233/1234">>, {bytes, 500, 1233, 1234}},
		{<<"bytes 734-1233/1234">>, {bytes, 734, 1233, 1234}},
		{<<"bytes */47022">>, {bytes, '*', 47022}},
		{<<"exampleunit 1.2-4.3/25">>, {<<"exampleunit">>, <<"1.2-4.3/25">>}},
		{<<"exampleunit 11.2-14.3/25">>, {<<"exampleunit">>, <<"11.2-14.3/25">>}}
	],
	[{V, fun() -> R = parse_content_range(V) end} || {V, R} <- Tests].

parse_content_range_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_content_range(V)) end} || V <- Tests].

horse_parse_content_range_bytes() ->
	horse:repeat(200000,
		parse_content_range(<<"bytes 21010-47021/47022">>)
	).

horse_parse_content_range_other() ->
	horse:repeat(200000,
		parse_content_range(<<"exampleunit 11.2-14.3/25">>)
	).
-endif.

%% @doc Parse the Content-Type header.

-spec parse_content_type(binary()) -> media_type().
parse_content_type(<< C, R/bits >>) when ?IS_TOKEN(C) ->
	?LOWER(media_type, R, <<>>).

media_type(<< $/, C, R/bits >>, T) when ?IS_TOKEN(C) ->
	?LOWER(media_subtype, R, T, <<>>);
media_type(<< C, R/bits >>, T) when ?IS_TOKEN(C) ->
	?LOWER(media_type, R, T).

media_subtype(<< C, R/bits >>, T, S) when ?IS_TOKEN(C) ->
	?LOWER(media_subtype, R, T, S);
media_subtype(R, T, S) -> media_param_sep(R, T, S, []).

media_param_sep(<<>>, T, S, P) -> {T, S, lists:reverse(P)};
media_param_sep(<< $;, R/bits >>, T, S, P) -> media_before_param(R, T, S, P);
media_param_sep(<< C, R/bits >>, T, S, P) when ?IS_WS(C) -> media_param_sep(R, T, S, P).

media_before_param(<< C, R/bits >>, T, S, P) when ?IS_WS(C)-> media_before_param(R, T, S, P);
media_before_param(<< "charset=", $", R/bits >>, T, S, P) -> media_charset_quoted(R, T, S, P, <<>>);
media_before_param(<< "charset=", R/bits >>, T, S, P) -> media_charset(R, T, S, P, <<>>);
media_before_param(<< C, R/bits >>, T, S, P) when ?IS_TOKEN(C) ->
	?LOWER(media_param, R, T, S, P, <<>>).

media_charset_quoted(<< $", R/bits >>, T, S, P, V) ->
	media_param_sep(R, T, S, [{<<"charset">>, V}|P]);
media_charset_quoted(<< $\\, C, R/bits >>, T, S, P, V) when ?IS_VCHAR_OBS(C) ->
	?LOWER(media_charset_quoted, R, T, S, P, V);
media_charset_quoted(<< C, R/bits >>, T, S, P, V) when ?IS_VCHAR_OBS(C) ->
	?LOWER(media_charset_quoted, R, T, S, P, V).

media_charset(<< C, R/bits >>, T, S, P, V) when ?IS_TOKEN(C) ->
	?LOWER(media_charset, R, T, S, P, V);
media_charset(R, T, S, P, V) -> media_param_sep(R, T, S, [{<<"charset">>, V}|P]).

media_param(<< $=, $", R/bits >>, T, S, P, K) -> media_quoted(R, T, S, P, K, <<>>);
media_param(<< $=, C, R/bits >>, T, S, P, K) when ?IS_TOKEN(C) -> media_value(R, T, S, P, K, << C >>);
media_param(<< C, R/bits >>, T, S, P, K) when ?IS_TOKEN(C) ->
	?LOWER(media_param, R, T, S, P, K).

media_quoted(<< $", R/bits >>, T, S, P, K, V) -> media_param_sep(R, T, S, [{K, V}|P]);
media_quoted(<< $\\, C, R/bits >>, T, S, P, K, V) when ?IS_VCHAR_OBS(C) -> media_quoted(R, T, S, P, K, << V/binary, C >>);
media_quoted(<< C, R/bits >>, T, S, P, K, V) when ?IS_VCHAR_OBS(C) -> media_quoted(R, T, S, P, K, << V/binary, C >>).

media_value(<< C, R/bits >>, T, S, P, K, V) when ?IS_TOKEN(C) -> media_value(R, T, S, P, K, << V/binary, C >>);
media_value(R, T, S, P, K, V) -> media_param_sep(R, T, S, [{K, V}|P]).

-ifdef(TEST).
media_type_parameter() ->
	frequency([
		{90, parameter()},
		{10, {<<"charset">>, oneof([token(), quoted_string()]), <<>>, <<>>}}
	]).

media_type() ->
	?LET({T, S, P},
		{token(), token(), small_list(media_type_parameter())},
		{T, S, P, iolist_to_binary([T, $/, S, [[OWS1, $;, OWS2, K, $=, V] || {K, V, OWS1, OWS2} <- P]])}
	).

prop_parse_content_type() ->
	?FORALL({T, S, P, MediaType},
		media_type(),
		begin
			{ResT, ResS, ResP} = parse_content_type(MediaType),
			ExpectedP = [case ?LOWER(K) of
				<<"charset">> -> {<<"charset">>, ?LOWER(unquote(V))};
				LowK -> {LowK, unquote(V)}
			end || {K, V, _, _} <- P],
			ResT =:= ?LOWER(T)
				andalso ResS =:= ?LOWER(S)
				andalso ResP =:= ExpectedP
		end
	).

parse_content_type_test_() ->
	Tests = [
		{<<"text/html;charset=utf-8">>,
			{<<"text">>, <<"html">>, [{<<"charset">>, <<"utf-8">>}]}},
		{<<"text/html;charset=UTF-8">>,
			{<<"text">>, <<"html">>, [{<<"charset">>, <<"utf-8">>}]}},
		{<<"Text/HTML;Charset=\"utf-8\"">>,
			{<<"text">>, <<"html">>, [{<<"charset">>, <<"utf-8">>}]}},
		{<<"text/html; charset=\"utf-8\"">>,
			{<<"text">>, <<"html">>, [{<<"charset">>, <<"utf-8">>}]}},
		{<<"text/html; charset=ISO-8859-4">>,
			{<<"text">>, <<"html">>, [{<<"charset">>, <<"iso-8859-4">>}]}},
		{<<"text/plain; charset=iso-8859-4">>,
			{<<"text">>, <<"plain">>, [{<<"charset">>, <<"iso-8859-4">>}]}},
		{<<"multipart/form-data  \t;Boundary=\"MultipartIsUgly\"">>,
			{<<"multipart">>, <<"form-data">>, [
				{<<"boundary">>, <<"MultipartIsUgly">>}
			]}},
		{<<"foo/bar; one=FirstParam; two=SecondParam">>,
			{<<"foo">>, <<"bar">>, [
				{<<"one">>, <<"FirstParam">>},
				{<<"two">>, <<"SecondParam">>}
			]}}
	],
	[{V, fun() -> R = parse_content_type(V) end} || {V, R} <- Tests].

horse_parse_content_type() ->
	horse:repeat(200000,
		parse_content_type(<<"text/html;charset=utf-8">>)
	).
-endif.

%% @doc Parse the Date header.

-spec parse_date(binary()) -> calendar:datetime().
parse_date(Date) ->
	cow_date:parse_date(Date).

-ifdef(TEST).
parse_date_test_() ->
	Tests = [
		{<<"Tue, 15 Nov 1994 08:12:31 GMT">>, {{1994, 11, 15}, {8, 12, 31}}}
	],
	[{V, fun() -> R = parse_date(V) end} || {V, R} <- Tests].
-endif.

%% @doc Parse the ETag header.

-spec parse_etag(binary()) -> etag().
parse_etag(<< $W, $/, $", R/bits >>) ->
	etag(R, weak, <<>>);
parse_etag(<< $", R/bits >>) ->
	etag(R, strong, <<>>).

etag(<< $" >>, Strength, Tag) ->
	{Strength, Tag};
etag(<< C, R/bits >>, Strength, Tag) when ?IS_ETAGC(C) ->
	etag(R, Strength, << Tag/binary, C >>).

-ifdef(TEST).
etagc() ->
	?SUCHTHAT(C, integer(16#21, 16#ff), C =/= 16#22 andalso C =/= 16#7f).

etag() ->
	?LET({Strength, Tag},
		{elements([weak, strong]), list(etagc())},
		begin
			TagBin = list_to_binary(Tag),
			{{Strength, TagBin},
				case Strength of
					weak -> << $W, $/, $", TagBin/binary, $" >>;
					strong -> << $", TagBin/binary, $" >>
				end}
		end).

prop_parse_etag() ->
	?FORALL({Tag, TagBin},
		etag(),
		Tag =:= parse_etag(TagBin)).

parse_etag_test_() ->
	Tests = [
		{<<"\"xyzzy\"">>, {strong, <<"xyzzy">>}},
		{<<"W/\"xyzzy\"">>, {weak, <<"xyzzy">>}},
		{<<"\"\"">>, {strong, <<>>}}
	],
	[{V, fun() -> R = parse_etag(V) end} || {V, R} <- Tests].

parse_etag_error_test_() ->
	Tests = [
		<<>>,
		<<"\"">>,
		<<"W">>,
		<<"W/">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_etag(V)) end} || V <- Tests].

horse_parse_etag() ->
	horse:repeat(200000,
		parse_etag(<<"W/\"xyzzy\"">>)
	).
-endif.

%% @doc Parse the Expect header.

-spec parse_expect(binary()) -> continue.
parse_expect(<<"100-continue">>) ->
	continue;
parse_expect(<<"100-", C, O, N, T, I, M, U, E >>)
	when (C =:= $C) or (C =:= $c), (O =:= $O) or (O =:= $o),
		(N =:= $N) or (N =:= $n), (T =:= $T) or (T =:= $t),
		(I =:= $I) or (I =:= $i), (M =:= $N) or (M =:= $n),
		(U =:= $U) or (U =:= $u), (E =:= $E) or (E =:= $e) ->
	continue.

-ifdef(TEST).
expect() ->
	?LET(E,
		[$1, $0, $0, $-,
			elements([$c, $C]), elements([$o, $O]), elements([$n, $N]),
			elements([$t, $T]), elements([$i, $I]), elements([$n, $N]),
			elements([$u, $U]), elements([$e, $E])],
		list_to_binary(E)).

prop_parse_expect() ->
	?FORALL(E, expect(), continue =:= parse_expect(E)).

parse_expect_test_() ->
	Tests = [
		<<"100-continue">>,
		<<"100-CONTINUE">>,
		<<"100-Continue">>,
		<<"100-CoNtInUe">>
	],
	[{V, fun() -> continue = parse_expect(V) end} || V <- Tests].

parse_expect_error_test_() ->
	Tests = [
		<<>>,
		<<"   ">>,
		<<"200-OK">>,
		<<"Cookies">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_expect(V)) end} || V <- Tests].

horse_parse_expect() ->
	horse:repeat(200000,
		parse_expect(<<"100-continue">>)
	).
-endif.

%% @doc Parse the Expires header.
%%
%% Recipients must interpret invalid date formats as a date
%% in the past. The value "0" is commonly used.

-spec parse_expires(binary()) -> calendar:datetime().
parse_expires(<<"0">>) ->
	{{1, 1, 1}, {0, 0, 0}};
parse_expires(Expires) ->
	try
		cow_date:parse_date(Expires)
	catch _:_ ->
		{{1, 1, 1}, {0, 0, 0}}
	end.

-ifdef(TEST).
parse_expires_test_() ->
	Tests = [
		{<<"0">>, {{1, 1, 1}, {0, 0, 0}}},
		{<<"Thu, 01 Dec 1994 nope invalid">>, {{1, 1, 1}, {0, 0, 0}}},
		{<<"Thu, 01 Dec 1994 16:00:00 GMT">>, {{1994, 12, 1}, {16, 0, 0}}}
	],
	[{V, fun() -> R = parse_expires(V) end} || {V, R} <- Tests].

horse_parse_expires_0() ->
	horse:repeat(200000,
		parse_expires(<<"0">>)
	).

horse_parse_expires_invalid() ->
	horse:repeat(200000,
		parse_expires(<<"Thu, 01 Dec 1994 nope invalid">>)
	).
-endif.

%% @doc Parse the Host header.
%%
%% We only seek to have legal characters and separate the
%% host and port values. The number of segments in the host
%% or the size of each segment is not checked.
%%
%% There is no way to distinguish IPv4 addresses from regular
%% names until the last segment is reached therefore we do not
%% differentiate them.
%%
%% The following valid hosts are currently rejected: IPv6
%% addresses with a zone identifier; IPvFuture addresses;
%% and percent-encoded addresses.

-spec parse_host(binary()) -> {binary(), 0..65535 | undefined}.
parse_host(<< $[, R/bits >>) ->
	ipv6_address(R, << $[ >>);
parse_host(Host) ->
	reg_name(Host, <<>>).

ipv6_address(<< $] >>, IP) -> {<< IP/binary, $] >>, undefined};
ipv6_address(<< $], $:, Port/bits >>, IP) -> {<< IP/binary, $] >>, binary_to_integer(Port)};
ipv6_address(<< C, R/bits >>, IP) when ?IS_HEX(C) or (C =:= $:) or (C =:= $.) ->
	?LOWER(ipv6_address, R, IP).

reg_name(<<>>, Name) -> {Name, undefined};
reg_name(<< $:, Port/bits >>, Name) -> {Name, binary_to_integer(Port)};
reg_name(<< C, R/bits >>, Name) when ?IS_URI_UNRESERVED(C) or ?IS_URI_SUB_DELIMS(C) ->
	?LOWER(reg_name, R, Name).

-ifdef(TEST).
host_chars() -> "!$&'()*+,-.0123456789;=ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~".
host() -> vector(1, 255, elements(host_chars())).

host_port() ->
	?LET({Host, Port},
		{host(), oneof([undefined, integer(1, 65535)])},
		begin
			HostBin = list_to_binary(Host),
			{{?LOWER(HostBin), Port},
				case Port of
					undefined -> HostBin;
					_ -> << HostBin/binary, $:, (integer_to_binary(Port))/binary >>
				end}
		end).

prop_parse_host() ->
	?FORALL({Res, Host}, host_port(), Res =:= parse_host(Host)).

parse_host_test_() ->
	Tests = [
		{<<>>, {<<>>, undefined}},
		{<<"www.example.org:8080">>, {<<"www.example.org">>, 8080}},
		{<<"www.example.org">>, {<<"www.example.org">>, undefined}},
		{<<"192.0.2.1:8080">>, {<<"192.0.2.1">>, 8080}},
		{<<"192.0.2.1">>, {<<"192.0.2.1">>, undefined}},
		{<<"[2001:db8::1]:8080">>, {<<"[2001:db8::1]">>, 8080}},
		{<<"[2001:db8::1]">>, {<<"[2001:db8::1]">>, undefined}},
		{<<"[::ffff:192.0.2.1]:8080">>, {<<"[::ffff:192.0.2.1]">>, 8080}},
		{<<"[::ffff:192.0.2.1]">>, {<<"[::ffff:192.0.2.1]">>, undefined}}
	],
	[{V, fun() -> R = parse_host(V) end} || {V, R} <- Tests].

horse_parse_host_blue_example_org() ->
	horse:repeat(200000,
		parse_host(<<"blue.example.org:8080">>)
	).

horse_parse_host_ipv4() ->
	horse:repeat(200000,
		parse_host(<<"192.0.2.1:8080">>)
	).

horse_parse_host_ipv6() ->
	horse:repeat(200000,
		parse_host(<<"[2001:db8::1]:8080">>)
	).

horse_parse_host_ipv6_v4() ->
	horse:repeat(200000,
		parse_host(<<"[::ffff:192.0.2.1]:8080">>)
	).
-endif.

%% @doc Parse the HTTP2-Settings header.

-spec parse_http2_settings(binary()) -> map().
parse_http2_settings(HTTP2Settings) ->
	cow_http2:parse_settings_payload(base64:decode(HTTP2Settings)).

%% @doc Parse the If-Match header.

-spec parse_if_match(binary()) -> '*' | [etag()].
parse_if_match(<<"*">>) ->
	'*';
parse_if_match(IfMatch) ->
	nonempty(etag_list(IfMatch, [])).

etag_list(<<>>, Acc) -> lists:reverse(Acc);
etag_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> etag_list(R, Acc);
etag_list(<< $W, $/, $", R/bits >>, Acc) -> etag(R, Acc, weak, <<>>);
etag_list(<< $", R/bits >>, Acc) -> etag(R, Acc, strong, <<>>).

etag(<< $", R/bits >>, Acc, Strength, Tag) -> etag_list_sep(R, [{Strength, Tag}|Acc]);
etag(<< C, R/bits >>, Acc, Strength, Tag) when ?IS_ETAGC(C) -> etag(R, Acc, Strength, << Tag/binary, C >>).

etag_list_sep(<<>>, Acc) -> lists:reverse(Acc);
etag_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> etag_list_sep(R, Acc);
etag_list_sep(<< $,, R/bits >>, Acc) -> etag_list(R, Acc).

-ifdef(TEST).
prop_parse_if_match() ->
	?FORALL(L,
		non_empty(list(etag())),
		begin
			<< _, IfMatch/binary >> = iolist_to_binary([[$,, T] || {_, T} <- L]),
			ResL = parse_if_match(IfMatch),
			CheckedL = [T =:= ResT || {{T, _}, ResT} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_if_match_test_() ->
	Tests = [
		{<<"\"xyzzy\"">>, [{strong, <<"xyzzy">>}]},
		{<<"\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"">>,
			[{strong, <<"xyzzy">>}, {strong, <<"r2d2xxxx">>}, {strong, <<"c3piozzzz">>}]},
		{<<"*">>, '*'}
	],
	[{V, fun() -> R = parse_if_match(V) end} || {V, R} <- Tests].

parse_if_match_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_if_match(V)) end} || V <- Tests].

horse_parse_if_match() ->
	horse:repeat(200000,
		parse_if_match(<<"\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"">>)
	).
-endif.

%% @doc Parse the If-Modified-Since header.

-spec parse_if_modified_since(binary()) -> calendar:datetime().
parse_if_modified_since(IfModifiedSince) ->
	cow_date:parse_date(IfModifiedSince).

-ifdef(TEST).
parse_if_modified_since_test_() ->
	Tests = [
		{<<"Sat, 29 Oct 1994 19:43:31 GMT">>, {{1994, 10, 29}, {19, 43, 31}}}
	],
	[{V, fun() -> R = parse_if_modified_since(V) end} || {V, R} <- Tests].
-endif.

%% @doc Parse the If-None-Match header.

-spec parse_if_none_match(binary()) -> '*' | [etag()].
parse_if_none_match(<<"*">>) ->
	'*';
parse_if_none_match(IfNoneMatch) ->
	nonempty(etag_list(IfNoneMatch, [])).

-ifdef(TEST).
parse_if_none_match_test_() ->
	Tests = [
		{<<"\"xyzzy\"">>, [{strong, <<"xyzzy">>}]},
		{<<"W/\"xyzzy\"">>, [{weak, <<"xyzzy">>}]},
		{<<"\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"">>,
			[{strong, <<"xyzzy">>}, {strong, <<"r2d2xxxx">>}, {strong, <<"c3piozzzz">>}]},
		{<<"W/\"xyzzy\", W/\"r2d2xxxx\", W/\"c3piozzzz\"">>,
			[{weak, <<"xyzzy">>}, {weak, <<"r2d2xxxx">>}, {weak, <<"c3piozzzz">>}]},
		{<<"*">>, '*'}
	],
	[{V, fun() -> R = parse_if_none_match(V) end} || {V, R} <- Tests].

parse_if_none_match_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_if_none_match(V)) end} || V <- Tests].

horse_parse_if_none_match() ->
	horse:repeat(200000,
		parse_if_none_match(<<"W/\"xyzzy\", W/\"r2d2xxxx\", W/\"c3piozzzz\"">>)
	).
-endif.

%% @doc Parse the If-Range header.

-spec parse_if_range(binary()) -> etag() | calendar:datetime().
parse_if_range(<< $W, $/, $", R/bits >>) ->
	etag(R, weak, <<>>);
parse_if_range(<< $", R/bits >>) ->
	etag(R, strong, <<>>);
parse_if_range(IfRange) ->
	cow_date:parse_date(IfRange).

-ifdef(TEST).
parse_if_range_test_() ->
	Tests = [
		{<<"W/\"xyzzy\"">>, {weak, <<"xyzzy">>}},
		{<<"\"xyzzy\"">>, {strong, <<"xyzzy">>}},
		{<<"Sat, 29 Oct 1994 19:43:31 GMT">>, {{1994, 10, 29}, {19, 43, 31}}}
	],
	[{V, fun() -> R = parse_if_range(V) end} || {V, R} <- Tests].

parse_if_range_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_if_range(V)) end} || V <- Tests].

horse_parse_if_range_etag() ->
	horse:repeat(200000,
		parse_if_range(<<"\"xyzzy\"">>)
	).

horse_parse_if_range_date() ->
	horse:repeat(200000,
		parse_if_range(<<"Sat, 29 Oct 1994 19:43:31 GMT">>)
	).
-endif.

%% @doc Parse the If-Unmodified-Since header.

-spec parse_if_unmodified_since(binary()) -> calendar:datetime().
parse_if_unmodified_since(IfModifiedSince) ->
	cow_date:parse_date(IfModifiedSince).

-ifdef(TEST).
parse_if_unmodified_since_test_() ->
	Tests = [
		{<<"Sat, 29 Oct 1994 19:43:31 GMT">>, {{1994, 10, 29}, {19, 43, 31}}}
	],
	[{V, fun() -> R = parse_if_unmodified_since(V) end} || {V, R} <- Tests].
-endif.

%% @doc Parse the Last-Modified header.

-spec parse_last_modified(binary()) -> calendar:datetime().
parse_last_modified(LastModified) ->
	cow_date:parse_date(LastModified).

-ifdef(TEST).
parse_last_modified_test_() ->
	Tests = [
		{<<"Tue, 15 Nov 1994 12:45:26 GMT">>, {{1994, 11, 15}, {12, 45, 26}}}
	],
	[{V, fun() -> R = parse_last_modified(V) end} || {V, R} <- Tests].
-endif.

%% @doc Parse the Max-Forwards header.

-spec parse_max_forwards(binary()) -> non_neg_integer().
parse_max_forwards(MaxForwards) ->
	I = binary_to_integer(MaxForwards),
	true = I >= 0,
	I.

-ifdef(TEST).
prop_parse_max_forwards() ->
	?FORALL(
		X,
		non_neg_integer(),
		X =:= parse_max_forwards(integer_to_binary(X))
	).

parse_max_forwards_test_() ->
	Tests = [
		{<<"0">>, 0},
		{<<"42">>, 42},
		{<<"69">>, 69},
		{<<"1337">>, 1337},
		{<<"1234567890">>, 1234567890}
	],
	[{V, fun() -> R = parse_max_forwards(V) end} || {V, R} <- Tests].

parse_max_forwards_error_test_() ->
	Tests = [
		<<>>,
		<<"123, 123">>,
		<<"4.17">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_max_forwards(V)) end} || V <- Tests].
-endif.

%% @doc Parse the Origin header.

%% According to the RFC6454 we should generate
%% a fresh globally unique identifier and return that value if:
%% - URI does not use a hierarchical element as a naming authority
%%   or the URI is not an absolute URI
%% - the implementation doesn't support the protocol given by uri-scheme
%% Thus, erlang reference represents a GUID here.
%%
%% We only seek to have legal characters and separate the
%% host and port values. The number of segments in the host
%% or the size of each segment is not checked.
%%
%% There is no way to distinguish IPv4 addresses from regular
%% names until the last segment is reached therefore we do not
%% differentiate them.
%%
%% @todo The following valid hosts are currently rejected: IPv6
%% addresses with a zone identifier; IPvFuture addresses;
%% and percent-encoded addresses.

-spec parse_origin(binary()) -> [{binary(), binary(), 0..65535} | reference()].
parse_origin(Origins) ->
	nonempty(origin_scheme(Origins, [])).

origin_scheme(<<>>, Acc) -> Acc;
origin_scheme(<< "http://", R/bits >>, Acc) -> origin_host(R, Acc, <<"http">>);
origin_scheme(<< "https://", R/bits >>, Acc) -> origin_host(R, Acc, <<"https">>);
origin_scheme(<< C, R/bits >>, Acc) when ?IS_TOKEN(C)  -> origin_scheme(next_origin(R), [make_ref()|Acc]).

origin_host(<< $[, R/bits >>, Acc, Scheme) -> origin_ipv6_address(R, Acc, Scheme, << $[ >>);
origin_host(Host, Acc, Scheme) -> origin_reg_name(Host, Acc, Scheme, <<>>).

origin_ipv6_address(<< $] >>, Acc, Scheme, IP) ->
	lists:reverse([{Scheme, << IP/binary, $] >>, default_port(Scheme)}|Acc]);
origin_ipv6_address(<< $], $\s, R/bits >>, Acc, Scheme, IP) ->
	origin_scheme(R, [{Scheme, << IP/binary, $] >>, default_port(Scheme)}|Acc]);
origin_ipv6_address(<< $], $:, Port/bits >>, Acc, Scheme, IP) ->
	origin_port(Port, Acc, Scheme, << IP/binary, $] >>, <<>>);
origin_ipv6_address(<< C, R/bits >>, Acc, Scheme, IP) when ?IS_HEX(C) or (C =:= $:) or (C =:= $.) ->
	?LOWER(origin_ipv6_address, R, Acc, Scheme, IP).

origin_reg_name(<<>>, Acc, Scheme, Name) ->
	lists:reverse([{Scheme, Name, default_port(Scheme)}|Acc]);
origin_reg_name(<< $\s, R/bits >>, Acc, Scheme, Name) ->
	origin_scheme(R, [{Scheme, Name, default_port(Scheme)}|Acc]);
origin_reg_name(<< $:, Port/bits >>, Acc, Scheme, Name) ->
	origin_port(Port, Acc, Scheme, Name, <<>>);
origin_reg_name(<< C, R/bits >>, Acc, Scheme, Name) when ?IS_URI_UNRESERVED(C) or ?IS_URI_SUB_DELIMS(C) ->
	?LOWER(origin_reg_name, R, Acc, Scheme, Name).

origin_port(<<>>, Acc, Scheme, Host, Port) ->
	lists:reverse([{Scheme, Host, binary_to_integer(Port)}|Acc]);
origin_port(<< $\s, R/bits >>, Acc, Scheme, Host, Port) ->
	origin_scheme(R, [{Scheme, Host, binary_to_integer(Port)}|Acc]);
origin_port(<< C, R/bits >>, Acc, Scheme, Host, Port) when ?IS_DIGIT(C) ->
	origin_port(R, Acc, Scheme, Host, << Port/binary, C >>).

next_origin(<<>>) -> <<>>;
next_origin(<< $\s, C, R/bits >>) when ?IS_TOKEN(C) -> << C, R/bits >>;
next_origin(<< C, R/bits >>) when ?IS_TOKEN(C) or (C =:= $:) or (C =:= $/) -> next_origin(R).

default_port(<< "http" >>) -> 80;
default_port(<< "https" >>) -> 443.

-ifdef(TEST).
scheme() -> oneof([<<"http">>, <<"https">>]).

scheme_host_port() ->
	?LET({Scheme, Host, Port},
		{scheme(), host(), integer(1, 65535)},
		begin
			HostBin = list_to_binary(Host),
			{[{Scheme, ?LOWER(HostBin), Port}],
				case default_port(Scheme) of
					Port -> << Scheme/binary, "://", HostBin/binary>>;
					_ -> << Scheme/binary, "://", HostBin/binary, $:, (integer_to_binary(Port))/binary >>
				end}
		end).

prop_parse_origin() ->
	?FORALL({Res, Origin}, scheme_host_port(), Res =:= parse_origin(Origin)).

parse_origin_test_() ->
	Tests = [
		{<<"http://www.example.org:8080">>, [{<<"http">>, <<"www.example.org">>, 8080}]},
		{<<"http://www.example.org">>, [{<<"http">>, <<"www.example.org">>, 80}]},
		{<<"http://192.0.2.1:8080">>, [{<<"http">>, <<"192.0.2.1">>, 8080}]},
		{<<"http://192.0.2.1">>, [{<<"http">>, <<"192.0.2.1">>, 80}]},
		{<<"http://[2001:db8::1]:8080">>, [{<<"http">>, <<"[2001:db8::1]">>, 8080}]},
		{<<"http://[2001:db8::1]">>, [{<<"http">>, <<"[2001:db8::1]">>, 80}]},
		{<<"http://[::ffff:192.0.2.1]:8080">>, [{<<"http">>, <<"[::ffff:192.0.2.1]">>, 8080}]},
		{<<"http://[::ffff:192.0.2.1]">>, [{<<"http">>, <<"[::ffff:192.0.2.1]">>, 80}]},
		{<<"http://example.org https://blue.example.com:8080">>,
			[{<<"http">>, <<"example.org">>, 80},
			 {<<"https">>, <<"blue.example.com">>, 8080}]}
	],
	[{V, fun() -> R = parse_origin(V) end} || {V, R} <- Tests].

parse_origin_reference_test_() ->
	Tests = [
		<<"null">>,
		<<"httpx://example.org:80">>,
		<<"httpx://example.org:80 null">>,
		<<"null null">>
	],
	[{V, fun() -> [true = is_reference(Ref) || Ref <- parse_origin(V)] end} || V <- Tests].

parse_origin_error_test_() ->
	Tests = [
		<<>>,
		<<"null", $\t, "null">>,
		<<"null", $\s, $\s, "null">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_origin(V)) end} || V <- Tests].

horse_parse_origin_blue_example_org() ->
	horse:repeat(200000,
		parse_origin(<<"http://blue.example.org:8080">>)
	).

horse_parse_origin_ipv4() ->
	horse:repeat(200000,
		parse_origin(<<"http://192.0.2.1:8080">>)
	).

horse_parse_origin_ipv6() ->
	horse:repeat(200000,
		parse_origin(<<"http://[2001:db8::1]:8080">>)
	).

horse_parse_origin_ipv6_v4() ->
	horse:repeat(200000,
		parse_origin(<<"http://[::ffff:192.0.2.1]:8080">>)
	).

horse_parse_origin_null() ->
	horse:repeat(200000,
		parse_origin(<<"null">>)
	).
-endif.

%% @doc Parse the Pragma header.
%%
%% Legacy header kept for backward compatibility with HTTP/1.0 caches.
%% Only the "no-cache" directive was ever specified, and only for
%% request messages.
%%
%% We take a large shortcut in the parsing of this header, expecting
%% an exact match of "no-cache".

-spec parse_pragma(binary()) -> cache | no_cache.
parse_pragma(<<"no-cache">>) -> no_cache;
parse_pragma(_) -> cache.

%% @doc Parse the Proxy-Authenticate header.
%%
%% Alias of parse_www_authenticate/1 due to identical syntax.

-spec parse_proxy_authenticate(binary()) -> [{basic, binary()}
	| {bearer | digest | binary(), [{binary(), binary()}]}].
parse_proxy_authenticate(ProxyAuthenticate) ->
	parse_www_authenticate(ProxyAuthenticate).

%% @doc Parse the Proxy-Authorization header.
%%
%% Alias of parse_authorization/1 due to identical syntax.

-spec parse_proxy_authorization(binary())
	-> {basic, binary(), binary()}
	| {bearer, binary()}
	| {digest, [{binary(), binary()}]}.
parse_proxy_authorization(ProxyAuthorization) ->
	parse_authorization(ProxyAuthorization).

%% @doc Parse the Range header.

-spec parse_range(binary())
	-> {bytes, [{non_neg_integer(), non_neg_integer() | infinity} | neg_integer()]}
	| {binary(), binary()}.
parse_range(<<"bytes=", R/bits >>) ->
	bytes_range_set(R, []);
parse_range(<< C, R/bits >>) when ?IS_TOKEN(C) ->
	?LOWER(other_range_unit, R, <<>>).

bytes_range_set(<<>>, Acc) -> {bytes, lists:reverse(Acc)};
bytes_range_set(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> bytes_range_set(R, Acc);
bytes_range_set(<< $-, C, R/bits >>, Acc) when ?IS_DIGIT(C) -> bytes_range_suffix_spec(R, Acc, C - $0);
bytes_range_set(<< C, R/bits >>, Acc) when ?IS_DIGIT(C) -> bytes_range_spec(R, Acc, C - $0).

bytes_range_spec(<< $-, C, R/bits >>, Acc, First) when ?IS_DIGIT(C) -> bytes_range_spec_last(R, Acc, First, C - $0);
bytes_range_spec(<< $-, R/bits >>, Acc, First) -> bytes_range_set_sep(R, [{First, infinity}|Acc]);
bytes_range_spec(<< C, R/bits >>, Acc, First) when ?IS_DIGIT(C) -> bytes_range_spec(R, Acc, First * 10 + C - $0).

bytes_range_spec_last(<< C, R/bits >>, Acc, First, Last) when ?IS_DIGIT(C) -> bytes_range_spec_last(R, Acc, First, Last * 10 + C - $0);
bytes_range_spec_last(R, Acc, First, Last) -> bytes_range_set_sep(R, [{First, Last}|Acc]).

bytes_range_suffix_spec(<< C, R/bits >>, Acc, Suffix) when ?IS_DIGIT(C) -> bytes_range_suffix_spec(R, Acc, Suffix * 10 + C - $0);
bytes_range_suffix_spec(R, Acc, Suffix) -> bytes_range_set_sep(R, [-Suffix|Acc]).

bytes_range_set_sep(<<>>, Acc) -> {bytes, lists:reverse(Acc)};
bytes_range_set_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> bytes_range_set_sep(R, Acc);
bytes_range_set_sep(<< $,, R/bits >>, Acc) -> bytes_range_set(R, Acc).

other_range_unit(<< $=, C, R/bits >>, U) when ?IS_VCHAR(C) ->
	other_range_set(R, U, << C >>);
other_range_unit(<< C, R/bits >>, U) when ?IS_TOKEN(C) ->
	?LOWER(other_range_unit, R, U).

other_range_set(<<>>, U, S) ->
	{U, S};
other_range_set(<< C, R/bits >>, U, S) when ?IS_VCHAR(C) ->
	other_range_set(R, U, << S/binary, C >>).

-ifdef(TEST).
bytes_range() ->
	?LET(BytesSet,
		non_empty(list(oneof([
			?SUCHTHAT({First, Last}, {pos_integer(), pos_integer()}, First =< Last),
			{pos_integer(), infinity},
			?LET(I, pos_integer(), -I)
		]))),
		{{bytes, BytesSet}, begin
			<< _, Set/bits >> = iolist_to_binary([
				case Spec of
					{First, infinity} -> [$,, integer_to_binary(First), $-];
					{First, Last} -> [$,, integer_to_binary(First), $-, integer_to_binary(Last)];
					Suffix -> [$,, integer_to_binary(Suffix)]
				end || Spec <- BytesSet]),
			<<"bytes=", Set/binary >>
		end}).

other_range() ->
	?LET(Range = {Unit, Set},
		{token(), ?LET(L, non_empty(list(vchar())), list_to_binary(L))},
		{Range, << Unit/binary, $=, Set/binary >>}).

range() ->
	oneof([
		bytes_range(),
		other_range()
	]).

prop_parse_range() ->
	?FORALL({Range, RangeBin},
		range(),
		begin
			Range2 = case Range of
				{bytes, _} -> Range;
				{Unit, Set} -> {?LOWER(Unit), Set}
			end,
			Range2 =:= parse_range(RangeBin)
		end).

parse_range_test_() ->
	Tests = [
		{<<"bytes=0-499">>, {bytes, [{0, 499}]}},
		{<<"bytes=500-999">>, {bytes, [{500, 999}]}},
		{<<"bytes=-500">>, {bytes, [-500]}},
		{<<"bytes=9500-">>, {bytes, [{9500, infinity}]}},
		{<<"bytes=0-0,-1">>, {bytes, [{0, 0}, -1]}},
		{<<"bytes=500-600,601-999">>, {bytes, [{500, 600}, {601, 999}]}},
		{<<"bytes=500-700,601-999">>, {bytes, [{500, 700}, {601, 999}]}},
		{<<"books=I-III,V-IX">>, {<<"books">>, <<"I-III,V-IX">>}}
	],
	[{V, fun() -> R = parse_range(V) end} || {V, R} <- Tests].

parse_range_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_range(V)) end} || V <- Tests].

horse_parse_range_first_last() ->
	horse:repeat(200000,
		parse_range(<<"bytes=500-999">>)
	).

horse_parse_range_infinity() ->
	horse:repeat(200000,
		parse_range(<<"bytes=9500-">>)
	).

horse_parse_range_suffix() ->
	horse:repeat(200000,
		parse_range(<<"bytes=-500">>)
	).

horse_parse_range_two() ->
	horse:repeat(200000,
		parse_range(<<"bytes=500-700,601-999">>)
	).

horse_parse_range_other() ->
	horse:repeat(200000,
		parse_range(<<"books=I-III,V-IX">>)
	).
-endif.

%% @doc Parse the Retry-After header.

-spec parse_retry_after(binary()) -> non_neg_integer() | calendar:datetime().
parse_retry_after(RetryAfter = << D, _/bits >>) when ?IS_DIGIT(D) ->
	I = binary_to_integer(RetryAfter),
	true = I >= 0,
	I;
parse_retry_after(RetryAfter) ->
	cow_date:parse_date(RetryAfter).

-ifdef(TEST).
parse_retry_after_test_() ->
	Tests = [
		{<<"Fri, 31 Dec 1999 23:59:59 GMT">>, {{1999, 12, 31}, {23, 59, 59}}},
		{<<"120">>, 120}
	],
	[{V, fun() -> R = parse_retry_after(V) end} || {V, R} <- Tests].

parse_retry_after_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_retry_after(V)) end} || V <- Tests].

horse_parse_retry_after_date() ->
	horse:repeat(200000,
		parse_retry_after(<<"Fri, 31 Dec 1999 23:59:59 GMT">>)
	).

horse_parse_retry_after_delay_seconds() ->
	horse:repeat(200000,
		parse_retry_after(<<"120">>)
	).
-endif.

%% @doc Dummy parsing function for the Sec-WebSocket-Accept header.
%%
%% The argument is returned without any processing. This value is
%% expected to be matched directly by the client so no parsing is
%% needed.

-spec parse_sec_websocket_accept(binary()) -> binary().
parse_sec_websocket_accept(SecWebSocketAccept) ->
	SecWebSocketAccept.

%% @doc Parse the Sec-WebSocket-Extensions request header.

-spec parse_sec_websocket_extensions(binary()) -> [{binary(), [binary() | {binary(), binary()}]}].
parse_sec_websocket_extensions(SecWebSocketExtensions) ->
	nonempty(ws_extension_list(SecWebSocketExtensions, [])).

ws_extension_list(<<>>, Acc) -> lists:reverse(Acc);
ws_extension_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> ws_extension_list(R, Acc);
ws_extension_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) -> ws_extension(R, Acc, << C >>).

ws_extension(<< C, R/bits >>, Acc, E) when ?IS_TOKEN(C) -> ws_extension(R, Acc, << E/binary, C >>);
ws_extension(R, Acc, E) -> ws_extension_param_sep(R, Acc, E, []).

ws_extension_param_sep(<<>>, Acc, E, P) -> lists:reverse([{E, lists:reverse(P)}|Acc]);
ws_extension_param_sep(<< $,, R/bits >>, Acc, E, P) -> ws_extension_list(R, [{E, lists:reverse(P)}|Acc]);
ws_extension_param_sep(<< $;, R/bits >>, Acc, E, P) -> ws_extension_before_param(R, Acc, E, P);
ws_extension_param_sep(<< C, R/bits >>, Acc, E, P) when ?IS_WS(C) -> ws_extension_param_sep(R, Acc, E, P).

ws_extension_before_param(<< C, R/bits >>, Acc, E, P) when ?IS_WS(C) -> ws_extension_before_param(R, Acc, E, P);
ws_extension_before_param(<< C, R/bits >>, Acc, E, P) when ?IS_TOKEN(C) -> ws_extension_param(R, Acc, E, P, << C >>).

ws_extension_param(<< $=, $", R/bits >>, Acc, E, P, K) -> ws_extension_quoted(R, Acc, E, P, K, <<>>);
ws_extension_param(<< $=, C, R/bits >>, Acc, E, P, K) when ?IS_TOKEN(C) -> ws_extension_value(R, Acc, E, P, K, << C >>);
ws_extension_param(<< C, R/bits >>, Acc, E, P, K) when ?IS_TOKEN(C) -> ws_extension_param(R, Acc, E, P, << K/binary, C >>);
ws_extension_param(R, Acc, E, P, K) -> ws_extension_param_sep(R, Acc, E, [K|P]).

ws_extension_quoted(<< $", R/bits >>, Acc, E, P, K, V) -> ws_extension_param_sep(R, Acc, E, [{K, V}|P]);
ws_extension_quoted(<< $\\, C, R/bits >>, Acc, E, P, K, V) when ?IS_TOKEN(C) -> ws_extension_quoted(R, Acc, E, P, K, << V/binary, C >>);
ws_extension_quoted(<< C, R/bits >>, Acc, E, P, K, V) when ?IS_TOKEN(C) -> ws_extension_quoted(R, Acc, E, P, K, << V/binary, C >>).

ws_extension_value(<< C, R/bits >>, Acc, E, P, K, V) when ?IS_TOKEN(C) -> ws_extension_value(R, Acc, E, P, K, << V/binary, C >>);
ws_extension_value(R, Acc, E, P, K, V) -> ws_extension_param_sep(R, Acc, E, [{K, V}|P]).

-ifdef(TEST).
quoted_token() ->
	?LET(T,
		non_empty(list(frequency([
			{99, tchar()},
			{1, [$\\, tchar()]}
		]))),
		[$", T, $"]).

ws_extension() ->
	?LET({E, PL},
		{token(), small_list({ows(), ows(), oneof([token(), {token(), oneof([token(), quoted_token()])}])})},
		{E, PL, iolist_to_binary([E,
			[case P of
				{OWS1, OWS2, {K, V}} -> [OWS1, $;, OWS2, K, $=, V];
				{OWS1, OWS2, K} -> [OWS1, $;, OWS2, K]
			end || P <- PL]
		])}).

prop_parse_sec_websocket_extensions() ->
	?FORALL(L,
		vector(1, 50, ws_extension()),
		begin
			<< _, SecWebsocketExtensions/binary >> = iolist_to_binary([[$,, E] || {_, _, E} <- L]),
			ResL = parse_sec_websocket_extensions(SecWebsocketExtensions),
			CheckedL = [begin
				ExpectedPL = [case P of
					{_, _, {K, V}} -> {K, unquote(V)};
					{_, _, K} -> K
				end || P <- PL],
				E =:= ResE andalso ExpectedPL =:= ResPL
			end || {{E, PL, _}, {ResE, ResPL}} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_sec_websocket_extensions_test_() ->
	Tests = [
		{<<"foo">>, [{<<"foo">>, []}]},
		{<<"bar; baz=2">>, [{<<"bar">>, [{<<"baz">>, <<"2">>}]}]},
		{<<"foo, bar; baz=2">>, [{<<"foo">>, []}, {<<"bar">>, [{<<"baz">>, <<"2">>}]}]},
		{<<"deflate-stream">>, [{<<"deflate-stream">>, []}]},
		{<<"mux; max-channels=4; flow-control, deflate-stream">>,
			[{<<"mux">>, [{<<"max-channels">>, <<"4">>}, <<"flow-control">>]}, {<<"deflate-stream">>, []}]},
		{<<"private-extension">>, [{<<"private-extension">>, []}]}
	],
	[{V, fun() -> R = parse_sec_websocket_extensions(V) end} || {V, R} <- Tests].

parse_sec_websocket_extensions_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_sec_websocket_extensions(V)) end}
		|| V <- Tests].

horse_parse_sec_websocket_extensions() ->
	horse:repeat(200000,
		parse_sec_websocket_extensions(<<"mux; max-channels=4; flow-control, deflate-stream">>)
	).
-endif.

%% @doc Dummy parsing function for the Sec-WebSocket-Key header.
%%
%% The argument is returned without any processing. This value is
%% expected to be prepended to a static value, the result of which
%% hashed to form a new base64 value returned in Sec-WebSocket-Accept,
%% therefore no parsing is needed.

-spec parse_sec_websocket_key(binary()) -> binary().
parse_sec_websocket_key(SecWebSocketKey) ->
	SecWebSocketKey.

%% @doc Parse the Sec-WebSocket-Protocol request header.

-spec parse_sec_websocket_protocol_req(binary()) -> [binary()].
parse_sec_websocket_protocol_req(SecWebSocketProtocol) ->
	nonempty(token_list(SecWebSocketProtocol, [])).

-ifdef(TEST).
parse_sec_websocket_protocol_req_test_() ->
	Tests = [
		{<<"chat, superchat">>, [<<"chat">>, <<"superchat">>]},
		{<<"Chat, SuperChat">>, [<<"Chat">>, <<"SuperChat">>]}
	],
	[{V, fun() -> R = parse_sec_websocket_protocol_req(V) end} || {V, R} <- Tests].

parse_sec_websocket_protocol_req_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_sec_websocket_protocol_req(V)) end}
		|| V <- Tests].

horse_parse_sec_websocket_protocol_req() ->
	horse:repeat(200000,
		parse_sec_websocket_protocol_req(<<"chat, superchat">>)
	).
-endif.

%% @doc Parse the Sec-Websocket-Protocol response header.

-spec parse_sec_websocket_protocol_resp(binary()) -> binary().
parse_sec_websocket_protocol_resp(Protocol) ->
	true = <<>> =/= Protocol,
	ok = validate_token(Protocol),
	Protocol.

-ifdef(TEST).
prop_parse_sec_websocket_protocol_resp() ->
	?FORALL(T,
		token(),
		T =:= parse_sec_websocket_protocol_resp(T)).

parse_sec_websocket_protocol_resp_test_() ->
	Tests = [
		{<<"chat">>, <<"chat">>},
		{<<"CHAT">>, <<"CHAT">>}
	],
	[{V, fun() -> R = parse_sec_websocket_protocol_resp(V) end} || {V, R} <- Tests].

parse_sec_websocket_protocol_resp_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_sec_websocket_protocol_resp(V)) end}
		|| V <- Tests].

horse_parse_sec_websocket_protocol_resp() ->
	horse:repeat(200000,
		parse_sec_websocket_protocol_resp(<<"chat">>)
	).
-endif.

%% @doc Parse the Sec-WebSocket-Version request header.

-spec parse_sec_websocket_version_req(binary()) -> websocket_version().
parse_sec_websocket_version_req(SecWebSocketVersion) when byte_size(SecWebSocketVersion) < 4 ->
	Version = binary_to_integer(SecWebSocketVersion),
	true = Version >= 0 andalso Version =< 255,
	Version.

-ifdef(TEST).
prop_parse_sec_websocket_version_req() ->
	?FORALL(Version,
		integer(0, 255),
		Version =:= parse_sec_websocket_version_req(integer_to_binary(Version))).

parse_sec_websocket_version_req_test_() ->
	Tests = [
		{<<"13">>, 13},
		{<<"25">>, 25}
	],
	[{V, fun() -> R = parse_sec_websocket_version_req(V) end} || {V, R} <- Tests].

parse_sec_websocket_version_req_error_test_() ->
	Tests = [
		<<>>,
		<<" ">>,
		<<"7, 8, 13">>,
		<<"invalid">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_sec_websocket_version_req(V)) end}
		|| V <- Tests].

horse_parse_sec_websocket_version_req_13() ->
	horse:repeat(200000,
		parse_sec_websocket_version_req(<<"13">>)
	).

horse_parse_sec_websocket_version_req_255() ->
	horse:repeat(200000,
		parse_sec_websocket_version_req(<<"255">>)
	).
-endif.

%% @doc Parse the Sec-WebSocket-Version response header.

-spec parse_sec_websocket_version_resp(binary()) -> [websocket_version()].
parse_sec_websocket_version_resp(SecWebSocketVersion) ->
	nonempty(ws_version_list(SecWebSocketVersion, [])).

ws_version_list(<<>>, Acc) -> lists:reverse(Acc);
ws_version_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> ws_version_list(R, Acc);
ws_version_list(<< C, R/bits >>, Acc) when ?IS_DIGIT(C) -> ws_version(R, Acc, C - $0).

ws_version(<< C, R/bits >>, Acc, V) when ?IS_DIGIT(C) -> ws_version(R, Acc, V * 10 + C - $0);
ws_version(R, Acc, V) -> ws_version_list_sep(R, [V|Acc]).

ws_version_list_sep(<<>>, Acc) -> lists:reverse(Acc);
ws_version_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> ws_version_list_sep(R, Acc);
ws_version_list_sep(<< $,, R/bits >>, Acc) -> ws_version_list(R, Acc).

-ifdef(TEST).
sec_websocket_version_resp() ->
	?LET(L,
		non_empty(list({ows(), ows(), integer(0, 255)})),
		begin
			<< _, SecWebSocketVersion/binary >> = iolist_to_binary(
				[[OWS1, $,, OWS2, integer_to_binary(V)] || {OWS1, OWS2, V} <- L]),
			{[V || {_, _, V} <- L], SecWebSocketVersion}
		end).

prop_parse_sec_websocket_version_resp() ->
	?FORALL({L, SecWebSocketVersion},
		sec_websocket_version_resp(),
		L =:= parse_sec_websocket_version_resp(SecWebSocketVersion)).

parse_sec_websocket_version_resp_test_() ->
	Tests = [
		{<<"13, 8, 7">>, [13, 8, 7]}
	],
	[{V, fun() -> R = parse_sec_websocket_version_resp(V) end} || {V, R} <- Tests].

parse_sec_websocket_version_resp_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_sec_websocket_version_resp(V)) end}
		|| V <- Tests].

horse_parse_sec_websocket_version_resp() ->
	horse:repeat(200000,
		parse_sec_websocket_version_resp(<<"13, 8, 7">>)
	).
-endif.

%% @doc Parse the TE header.
%%
%% This function does not support parsing of transfer-parameter.

-spec parse_te(binary()) -> {trailers | no_trailers, [{binary(), qvalue()}]}.
parse_te(TE) ->
	te_list(TE, no_trailers, []).

te_list(<<>>, Trail, Acc) -> {Trail, lists:reverse(Acc)};
te_list(<< C, R/bits >>, Trail, Acc) when ?IS_WS_COMMA(C) -> te_list(R, Trail, Acc);
te_list(<< "trailers", R/bits >>, Trail, Acc) -> te(R, Trail, Acc, <<"trailers">>);
te_list(<< "compress", R/bits >>, Trail, Acc) -> te(R, Trail, Acc, <<"compress">>);
te_list(<< "deflate", R/bits >>, Trail, Acc) -> te(R, Trail, Acc, <<"deflate">>);
te_list(<< "gzip", R/bits >>, Trail, Acc) -> te(R, Trail, Acc, <<"gzip">>);
te_list(<< C, R/bits >>, Trail, Acc) when ?IS_TOKEN(C) ->
	?LOWER(te, R, Trail, Acc, <<>>).

te(<<>>, _, Acc, <<"trailers">>) -> {trailers, lists:reverse(Acc)};
te(<< $,, R/bits >>, _, Acc, <<"trailers">>) -> te_list(R, trailers, Acc);
te(<< $;, R/bits >>, Trail, Acc, T) when T =/= <<"trailers">> -> te_before_weight(R, Trail, Acc, T);
te(<< C, R/bits >>, _, Acc, <<"trailers">>) when ?IS_WS(C) -> te_list_sep(R, trailers, Acc);
te(<< C, R/bits >>, Trail, Acc, T) when ?IS_TOKEN(C) ->
	?LOWER(te, R, Trail, Acc, T);
te(R, Trail, Acc, T) -> te_param_sep(R, Trail, Acc, T).

te_param_sep(<<>>, Trail, Acc, T) -> {Trail, lists:reverse([{T, 1000}|Acc])};
te_param_sep(<< $,, R/bits >>, Trail, Acc, T) -> te_list(R, Trail, [{T, 1000}|Acc]);
te_param_sep(<< C, R/bits >>, Trail, Acc, T) when ?IS_WS(C) -> te_param_sep(R, Trail, Acc, T).

te_before_weight(<< C, R/bits >>, Trail, Acc, T) when ?IS_WS(C) -> te_before_weight(R, Trail, Acc, T);
te_before_weight(<< $q, $=, R/bits >>, Trail, Acc, T) -> te_weight(R, Trail, Acc, T).

te_weight(<< "1.000", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 1000}|Acc]);
te_weight(<< "1.00", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 1000}|Acc]);
te_weight(<< "1.0", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 1000}|Acc]);
te_weight(<< "1.", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 1000}|Acc]);
te_weight(<< "1", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 1000}|Acc]);
te_weight(<< "0.", A, B, C, R/bits >>, Trail, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B), ?IS_DIGIT(C) ->
	te_list_sep(R, Trail, [{T, (A - $0) * 100 + (B - $0) * 10 + (C - $0)}|Acc]);
te_weight(<< "0.", A, B, R/bits >>, Trail, Acc, T) when ?IS_DIGIT(A), ?IS_DIGIT(B) ->
	te_list_sep(R, Trail, [{T, (A - $0) * 100 + (B - $0) * 10}|Acc]);
te_weight(<< "0.", A, R/bits >>, Trail, Acc, T) when ?IS_DIGIT(A) ->
	te_list_sep(R, Trail, [{T, (A - $0) * 100}|Acc]);
te_weight(<< "0.", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 0}|Acc]);
te_weight(<< "0", R/bits >>, Trail, Acc, T) -> te_list_sep(R, Trail, [{T, 0}|Acc]).

te_list_sep(<<>>, Trail, Acc) -> {Trail, lists:reverse(Acc)};
te_list_sep(<< C, R/bits >>, Trail, Acc) when ?IS_WS(C) -> te_list_sep(R, Trail, Acc);
te_list_sep(<< $,, R/bits >>, Trail, Acc) -> te_list(R, Trail, Acc).

-ifdef(TEST).
te() ->
	?LET({Trail, L},
		{elements([trailers, no_trailers]),
			small_non_empty_list({?SUCHTHAT(T, token(), T =/= <<"trailers">>), weight()})},
		{Trail, L, begin
			L2 = case Trail of
				no_trailers -> L;
				trailers ->
					Rand = rand:uniform(length(L) + 1) - 1,
					{Before, After} = lists:split(Rand, L),
					Before ++ [{<<"trailers">>, undefined}|After]
			end,
			<< _, TE/binary >> = iolist_to_binary([case W of
				undefined -> [$,, T];
				_ -> [$,, T, <<";q=">>, qvalue_to_iodata(W)]
			end || {T, W} <- L2]),
			TE
		end}
	).

prop_parse_te() ->
	?FORALL({Trail, L, TE},
		te(),
		begin
			{ResTrail, ResL} = parse_te(TE),
			CheckedL = [begin
				ResT =:= ?LOWER(T)
					andalso (ResW =:= W orelse (W =:= undefined andalso ResW =:= 1000))
			end || {{T, W}, {ResT, ResW}} <- lists:zip(L, ResL)],
			ResTrail =:= Trail andalso [true] =:= lists:usort(CheckedL)
		end).

parse_te_test_() ->
	Tests = [
		{<<"deflate">>, {no_trailers, [{<<"deflate">>, 1000}]}},
		{<<>>, {no_trailers, []}},
		{<<"trailers, deflate;q=0.5">>, {trailers, [{<<"deflate">>, 500}]}}
	],
	[{V, fun() -> R = parse_te(V) end} || {V, R} <- Tests].

horse_parse_te() ->
	horse:repeat(200000,
		parse_te(<<"trailers, deflate;q=0.5">>)
	).
-endif.

%% @doc Parse the Trailer header.

-spec parse_trailer(binary()) -> [binary()].
parse_trailer(Trailer) ->
	nonempty(token_ci_list(Trailer, [])).

-ifdef(TEST).
parse_trailer_test_() ->
	Tests = [
		{<<"Date, Content-MD5">>, [<<"date">>, <<"content-md5">>]}
	],
	[{V, fun() -> R = parse_trailer(V) end} || {V, R} <- Tests].

parse_trailer_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_trailer(V)) end} || V <- Tests].

horse_parse_trailer() ->
	horse:repeat(200000,
		parse_trailer(<<"Date, Content-MD5">>)
	).
-endif.

%% @doc Parse the Transfer-Encoding header.
%%
%% This function does not support parsing of transfer-parameter.

-spec parse_transfer_encoding(binary()) -> [binary()].
parse_transfer_encoding(<<"chunked">>) ->
	[<<"chunked">>];
parse_transfer_encoding(TransferEncoding) ->
	nonempty(token_ci_list(TransferEncoding, [])).

-ifdef(TEST).
prop_parse_transfer_encoding() ->
	?FORALL(L,
		non_empty(list(token())),
		begin
			<< _, TransferEncoding/binary >> = iolist_to_binary([[$,, C] || C <- L]),
			ResL = parse_transfer_encoding(TransferEncoding),
			CheckedL = [?LOWER(Co) =:= ResC || {Co, ResC} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_transfer_encoding_test_() ->
	Tests = [
		{<<"a , , , ">>, [<<"a">>]},
		{<<" , , , a">>, [<<"a">>]},
		{<<"a , , b">>, [<<"a">>, <<"b">>]},
		{<<"chunked">>, [<<"chunked">>]},
		{<<"chunked, something">>, [<<"chunked">>, <<"something">>]},
		{<<"gzip, chunked">>, [<<"gzip">>, <<"chunked">>]}
	],
	[{V, fun() -> R = parse_transfer_encoding(V) end} || {V, R} <- Tests].

parse_transfer_encoding_error_test_() ->
	Tests = [
		<<>>,
		<<" ">>,
		<<" , ">>,
		<<",,,">>,
		<<"a b">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_transfer_encoding(V)) end}
		|| V <- Tests].

horse_parse_transfer_encoding_chunked() ->
	horse:repeat(200000,
		parse_transfer_encoding(<<"chunked">>)
	).

horse_parse_transfer_encoding_custom() ->
	horse:repeat(200000,
		parse_transfer_encoding(<<"chunked, something">>)
	).
-endif.

%% @doc Parse the Upgrade header.
%%
%% It is unclear from the RFC whether the values here are
%% case sensitive.
%%
%% We handle them in a case insensitive manner because they
%% are described as case insensitive in the Websocket RFC.

-spec parse_upgrade(binary()) -> [binary()].
parse_upgrade(Upgrade) ->
	nonempty(protocol_list(Upgrade, [])).

protocol_list(<<>>, Acc) -> lists:reverse(Acc);
protocol_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> protocol_list(R, Acc);
protocol_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) ->
	?LOWER(protocol_name, R, Acc, <<>>).

protocol_name(<< $/, C, R/bits >>, Acc, P) ->
	?LOWER(protocol_version, R, Acc, << P/binary, $/ >>);
protocol_name(<< C, R/bits >>, Acc, P) when ?IS_TOKEN(C) ->
	?LOWER(protocol_name, R, Acc, P);
protocol_name(R, Acc, P) -> protocol_list_sep(R, [P|Acc]).

protocol_version(<< C, R/bits >>, Acc, P) when ?IS_TOKEN(C) ->
	?LOWER(protocol_version, R, Acc, P);
protocol_version(R, Acc, P) -> protocol_list_sep(R, [P|Acc]).

protocol_list_sep(<<>>, Acc) -> lists:reverse(Acc);
protocol_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> protocol_list_sep(R, Acc);
protocol_list_sep(<< $,, R/bits >>, Acc) -> protocol_list(R, Acc).

-ifdef(TEST).
protocols() ->
	?LET(P,
		oneof([token(), [token(), $/, token()]]),
		iolist_to_binary(P)).

prop_parse_upgrade() ->
	?FORALL(L,
		non_empty(list(protocols())),
		begin
			<< _, Upgrade/binary >> = iolist_to_binary([[$,, P] || P <- L]),
			ResL = parse_upgrade(Upgrade),
			CheckedL = [?LOWER(P) =:= ResP || {P, ResP} <- lists:zip(L, ResL)],
			[true] =:= lists:usort(CheckedL)
		end).

parse_upgrade_test_() ->
	Tests = [
		{<<"HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11">>,
			[<<"http/2.0">>, <<"shttp/1.3">>, <<"irc/6.9">>, <<"rta/x11">>]},
		{<<"HTTP/2.0">>, [<<"http/2.0">>]}
	],
	[{V, fun() -> R = parse_upgrade(V) end} || {V, R} <- Tests].

parse_upgrade_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_upgrade(V)) end}
		|| V <- Tests].
-endif.

%% @doc Parse the Vary header.

-spec parse_vary(binary()) -> '*' | [binary()].
parse_vary(<<"*">>) ->
	'*';
parse_vary(Vary) ->
	nonempty(token_ci_list(Vary, [])).

-ifdef(TEST).
parse_vary_test_() ->
	Tests = [
		{<<"*">>, '*'},
		{<<"Accept-Encoding">>, [<<"accept-encoding">>]},
		{<<"accept-encoding, accept-language">>, [<<"accept-encoding">>, <<"accept-language">>]}
	],
	[{V, fun() -> R = parse_vary(V) end} || {V, R} <- Tests].

parse_vary_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_vary(V)) end} || V <- Tests].
-endif.

%% @doc Parse the WWW-Authenticate header.
%%
%% Unknown schemes are represented as the lowercase binary
%% instead of an atom. Unlike with parse_authorization/1,
%% we do not crash on unknown schemes.
%%
%% When parsing auth-params, we do not accept BWS characters around the "=".

-spec parse_www_authenticate(binary()) -> [{basic, binary()}
	| {bearer | digest | binary(), [{binary(), binary()}]}].
parse_www_authenticate(Authenticate) ->
	nonempty(www_auth_list(Authenticate, [])).

www_auth_list(<<>>, Acc) -> lists:reverse(Acc);
www_auth_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> www_auth_list(R, Acc);
www_auth_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) ->
	?LOWER(www_auth_scheme, R, Acc, <<>>).

www_auth_basic_before_realm(<< C, R/bits >>, Acc) when ?IS_WS(C) -> www_auth_basic_before_realm(R, Acc);
www_auth_basic_before_realm(<< "realm=\"", R/bits >>, Acc) -> www_auth_basic(R, Acc, <<>>).

www_auth_basic(<< $", R/bits >>, Acc, Realm) -> www_auth_list_sep(R, [{basic, Realm}|Acc]);
www_auth_basic(<< $\\, C, R/bits >>, Acc, Realm) when ?IS_VCHAR_OBS(C) -> www_auth_basic(R, Acc, << Realm/binary, C >>);
www_auth_basic(<< C, R/bits >>, Acc, Realm) when ?IS_VCHAR_OBS(C) -> www_auth_basic(R, Acc, << Realm/binary, C >>).

www_auth_scheme(<< C, R/bits >>, Acc, Scheme) when ?IS_WS(C) ->
	case Scheme of
		<<"basic">> -> www_auth_basic_before_realm(R, Acc);
		<<"bearer">> -> www_auth_params_list(R, Acc, bearer, []);
		<<"digest">> -> www_auth_params_list(R, Acc, digest, []);
		_ -> www_auth_params_list(R, Acc, Scheme, [])
	end;
www_auth_scheme(<< C, R/bits >>, Acc, Scheme) when ?IS_TOKEN(C) ->
	?LOWER(www_auth_scheme, R, Acc, Scheme).

www_auth_list_sep(<<>>, Acc) -> lists:reverse(Acc);
www_auth_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> www_auth_list_sep(R, Acc);
www_auth_list_sep(<< $,, R/bits >>, Acc) -> www_auth_list(R, Acc).

www_auth_params_list(<<>>, Acc, Scheme, Params) ->
	lists:reverse([{Scheme, lists:reverse(nonempty(Params))}|Acc]);
www_auth_params_list(<< C, R/bits >>, Acc, Scheme, Params) when ?IS_WS_COMMA(C) ->
	www_auth_params_list(R, Acc, Scheme, Params);
www_auth_params_list(<< "algorithm=", C, R/bits >>, Acc, Scheme, Params) when ?IS_TOKEN(C) ->
	www_auth_token(R, Acc, Scheme, Params, <<"algorithm">>, << C >>);
www_auth_params_list(<< "domain=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"domain">>, <<>>);
www_auth_params_list(<< "error=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"error">>, <<>>);
www_auth_params_list(<< "error_description=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"error_description">>, <<>>);
www_auth_params_list(<< "error_uri=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"error_uri">>, <<>>);
www_auth_params_list(<< "nonce=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"nonce">>, <<>>);
www_auth_params_list(<< "opaque=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"opaque">>, <<>>);
www_auth_params_list(<< "qop=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"qop">>, <<>>);
www_auth_params_list(<< "realm=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"realm">>, <<>>);
www_auth_params_list(<< "scope=\"", R/bits >>, Acc, Scheme, Params) ->
	www_auth_quoted(R, Acc, Scheme, Params, <<"scope">>, <<>>);
www_auth_params_list(<< "stale=false", R/bits >>, Acc, Scheme, Params) ->
	www_auth_params_list_sep(R, Acc, Scheme, [{<<"stale">>, <<"false">>}|Params]);
www_auth_params_list(<< "stale=true", R/bits >>, Acc, Scheme, Params) ->
	www_auth_params_list_sep(R, Acc, Scheme, [{<<"stale">>, <<"true">>}|Params]);
www_auth_params_list(<< C, R/bits >>, Acc, Scheme, Params) when ?IS_TOKEN(C) ->
	?LOWER(www_auth_param, R, Acc, Scheme, Params, <<>>).

www_auth_param(<< $=, $", R/bits >>, Acc, Scheme, Params, K) ->
	www_auth_quoted(R, Acc, Scheme, Params, K, <<>>);
www_auth_param(<< $=, C, R/bits >>, Acc, Scheme, Params, K) when ?IS_TOKEN(C) ->
	www_auth_token(R, Acc, Scheme, Params, K, << C >>);
www_auth_param(<< C, R/bits >>, Acc, Scheme, Params, K) when ?IS_TOKEN(C) ->
	?LOWER(www_auth_param, R, Acc, Scheme, Params, K);
www_auth_param(R, Acc, Scheme, Params, NewScheme) ->
	www_auth_scheme(R, [{Scheme, lists:reverse(Params)}|Acc], NewScheme).

www_auth_token(<< C, R/bits >>, Acc, Scheme, Params, K, V) when ?IS_TOKEN(C) ->
	www_auth_token(R, Acc, Scheme, Params, K, << V/binary, C >>);
www_auth_token(R, Acc, Scheme, Params, K, V) ->
	www_auth_params_list_sep(R, Acc, Scheme, [{K, V}|Params]).

www_auth_quoted(<< $", R/bits >>, Acc, Scheme, Params, K, V) ->
	www_auth_params_list_sep(R, Acc, Scheme, [{K, V}|Params]);
www_auth_quoted(<< $\\, C, R/bits >>, Acc, Scheme, Params, K, V) when ?IS_VCHAR_OBS(C) ->
	www_auth_quoted(R, Acc, Scheme, Params, K, << V/binary, C >>);
www_auth_quoted(<< C, R/bits >>, Acc, Scheme, Params, K, V) when ?IS_VCHAR_OBS(C) ->
	www_auth_quoted(R, Acc, Scheme, Params, K, << V/binary, C >>).

www_auth_params_list_sep(<<>>, Acc, Scheme, Params) ->
	lists:reverse([{Scheme, lists:reverse(Params)}|Acc]);
www_auth_params_list_sep(<< C, R/bits >>, Acc, Scheme, Params) when ?IS_WS(C) ->
	www_auth_params_list_sep(R, Acc, Scheme, Params);
www_auth_params_list_sep(<< $,, R/bits >>, Acc, Scheme, Params) ->
	www_auth_params_list_after_sep(R, Acc, Scheme, Params).

www_auth_params_list_after_sep(<<>>, Acc, Scheme, Params) ->
	lists:reverse([{Scheme, lists:reverse(Params)}|Acc]);
www_auth_params_list_after_sep(<< C, R/bits >>, Acc, Scheme, Params) when ?IS_WS_COMMA(C) ->
	www_auth_params_list_after_sep(R, Acc, Scheme, Params);
www_auth_params_list_after_sep(R, Acc, Scheme, Params) ->
	www_auth_params_list(R, Acc, Scheme, Params).

-ifdef(TEST).
parse_www_authenticate_test_() ->
	Tests = [
		{<<"Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"">>,
			[{<<"newauth">>, [
				{<<"realm">>, <<"apps">>},
				{<<"type">>, <<"1">>},
				{<<"title">>, <<"Login to \"apps\"">>}]},
			{basic, <<"simple">>}]},
		%% Same test, different order.
		{<<"Basic realm=\"simple\", Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\"">>,
			[{basic, <<"simple">>},
			{<<"newauth">>, [
				{<<"realm">>, <<"apps">>},
				{<<"type">>, <<"1">>},
				{<<"title">>, <<"Login to \"apps\"">>}]}]},
		{<<"Bearer realm=\"example\"">>,
			[{bearer, [{<<"realm">>, <<"example">>}]}]},
		{<<"Bearer realm=\"example\", error=\"invalid_token\", error_description=\"The access token expired\"">>,
			[{bearer, [
				{<<"realm">>, <<"example">>},
				{<<"error">>, <<"invalid_token">>},
				{<<"error_description">>, <<"The access token expired">>}
			]}]},
		{<<"Basic realm=\"WallyWorld\"">>,
			[{basic, <<"WallyWorld">>}]},
		{<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", "
				"nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", "
				"opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"">>,
			[{digest, [
				{<<"realm">>, <<"testrealm@host.com">>},
				{<<"qop">>, <<"auth,auth-int">>},
				{<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>},
				{<<"opaque">>, <<"5ccc069c403ebaf9f0171e9517f40e41">>}
			]}]}
	],
	[{V, fun() -> R = parse_www_authenticate(V) end} || {V, R} <- Tests].

parse_www_authenticate_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_www_authenticate(V)) end} || V <- Tests].

horse_parse_www_authenticate() ->
	horse:repeat(200000,
		parse_www_authenticate(<<"Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\", Basic realm=\"simple\"">>)
	).
-endif.

%% @doc Parse the X-Forwarded-For header.
%%
%% This header has no specification but *looks like* it is
%% a list of tokens.
%%
%% This header is deprecated in favor of the Forwarded header.

-spec parse_x_forwarded_for(binary()) -> [binary()].
parse_x_forwarded_for(XForwardedFor) ->
	nonempty(nodeid_list(XForwardedFor, [])).

-define(IS_NODEID_TOKEN(C),
	?IS_ALPHA(C) or ?IS_DIGIT(C)
		or (C =:= $:) or (C =:= $.) or (C =:= $_)
		or (C =:= $-) or (C =:= $[) or (C =:= $])).

nodeid_list(<<>>, Acc) -> lists:reverse(Acc);
nodeid_list(<<C, R/bits>>, Acc) when ?IS_WS_COMMA(C) -> nodeid_list(R, Acc);
nodeid_list(<<C, R/bits>>, Acc) when ?IS_NODEID_TOKEN(C) -> nodeid(R, Acc, <<C>>).

nodeid(<<C, R/bits>>, Acc, T) when ?IS_NODEID_TOKEN(C) -> nodeid(R, Acc, <<T/binary, C>>);
nodeid(R, Acc, T) -> nodeid_list_sep(R, [T|Acc]).

nodeid_list_sep(<<>>, Acc) -> lists:reverse(Acc);
nodeid_list_sep(<<C, R/bits>>, Acc) when ?IS_WS(C) -> nodeid_list_sep(R, Acc);
nodeid_list_sep(<<$,, R/bits>>, Acc) -> nodeid_list(R, Acc).

-ifdef(TEST).
parse_x_forwarded_for_test_() ->
	Tests = [
		{<<"client, proxy1, proxy2">>,
			[<<"client">>, <<"proxy1">>, <<"proxy2">>]},
		{<<"128.138.243.150, unknown, 192.52.106.30">>,
			[<<"128.138.243.150">>, <<"unknown">>, <<"192.52.106.30">>]},
		%% Examples from Mozilla DN.
		{<<"2001:db8:85a3:8d3:1319:8a2e:370:7348">>,
			[<<"2001:db8:85a3:8d3:1319:8a2e:370:7348">>]},
		{<<"203.0.113.195">>,
			[<<"203.0.113.195">>]},
		{<<"203.0.113.195, 70.41.3.18, 150.172.238.178">>,
			[<<"203.0.113.195">>, <<"70.41.3.18">>, <<"150.172.238.178">>]},
		%% Examples from RFC7239 modified for x-forwarded-for.
		{<<"[2001:db8:cafe::17]:4711">>,
			[<<"[2001:db8:cafe::17]:4711">>]},
		{<<"192.0.2.43, 198.51.100.17">>,
			[<<"192.0.2.43">>, <<"198.51.100.17">>]},
		{<<"_hidden">>,
			[<<"_hidden">>]},
		{<<"192.0.2.43,[2001:db8:cafe::17],unknown">>,
			[<<"192.0.2.43">>, <<"[2001:db8:cafe::17]">>, <<"unknown">>]},
		{<<"192.0.2.43, [2001:db8:cafe::17], unknown">>,
			[<<"192.0.2.43">>, <<"[2001:db8:cafe::17]">>, <<"unknown">>]},
		{<<"192.0.2.43, 2001:db8:cafe::17">>,
			[<<"192.0.2.43">>, <<"2001:db8:cafe::17">>]},
		{<<"192.0.2.43, [2001:db8:cafe::17]">>,
			[<<"192.0.2.43">>, <<"[2001:db8:cafe::17]">>]}
	],
	[{V, fun() -> R = parse_x_forwarded_for(V) end} || {V, R} <- Tests].

parse_x_forwarded_for_error_test_() ->
	Tests = [
		<<>>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_x_forwarded_for(V)) end} || V <- Tests].
-endif.

%% Building.

%% @doc Build the Access-Control-Allow-Credentials header.

-spec access_control_allow_credentials() -> iodata().
access_control_allow_credentials() -> <<"true">>.

%% @doc Build the Access-Control-Allow-Headers header.

-spec access_control_allow_headers([binary()]) -> iodata().
access_control_allow_headers(Headers) ->
	join_token_list(nonempty(Headers)).

-ifdef(TEST).
access_control_allow_headers_test_() ->
	Tests = [
		{[<<"accept">>], <<"accept">>},
		{[<<"accept">>, <<"authorization">>, <<"content-type">>], <<"accept, authorization, content-type">>}
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> R = iolist_to_binary(access_control_allow_headers(V)) end} || {V, R} <- Tests].

access_control_allow_headers_error_test_() ->
	Tests = [
		[]
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> {'EXIT', _} = (catch access_control_allow_headers(V)) end} || V <- Tests].

horse_access_control_allow_headers() ->
	horse:repeat(200000,
		access_control_allow_headers([<<"accept">>, <<"authorization">>, <<"content-type">>])
	).
-endif.

%% @doc Build the Access-Control-Allow-Methods header.

-spec access_control_allow_methods([binary()]) -> iodata().
access_control_allow_methods(Methods) ->
	join_token_list(nonempty(Methods)).

-ifdef(TEST).
access_control_allow_methods_test_() ->
	Tests = [
		{[<<"GET">>], <<"GET">>},
		{[<<"GET">>, <<"POST">>, <<"DELETE">>], <<"GET, POST, DELETE">>}
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> R = iolist_to_binary(access_control_allow_methods(V)) end} || {V, R} <- Tests].

access_control_allow_methods_error_test_() ->
	Tests = [
		[]
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> {'EXIT', _} = (catch access_control_allow_methods(V)) end} || V <- Tests].

horse_access_control_allow_methods() ->
	horse:repeat(200000,
		access_control_allow_methods([<<"GET">>, <<"POST">>, <<"DELETE">>])
	).
-endif.

%% @doc Build the Access-Control-Allow-Origin header.

-spec access_control_allow_origin({binary(), binary(), 0..65535} | reference() | '*') -> iodata().
access_control_allow_origin({Scheme, Host, Port}) ->
	case default_port(Scheme) of
		Port -> [Scheme, <<"://">>, Host];
		_ -> [Scheme, <<"://">>, Host, <<":">>, integer_to_binary(Port)]
	end;
access_control_allow_origin('*') -> <<$*>>;
access_control_allow_origin(Ref) when is_reference(Ref) -> <<"null">>.

-ifdef(TEST).
access_control_allow_origin_test_() ->
	Tests = [
		{{<<"http">>, <<"www.example.org">>, 8080}, <<"http://www.example.org:8080">>},
		{{<<"http">>, <<"www.example.org">>, 80}, <<"http://www.example.org">>},
		{{<<"http">>, <<"192.0.2.1">>, 8080}, <<"http://192.0.2.1:8080">>},
		{{<<"http">>, <<"192.0.2.1">>, 80}, <<"http://192.0.2.1">>},
		{{<<"http">>, <<"[2001:db8::1]">>, 8080}, <<"http://[2001:db8::1]:8080">>},
		{{<<"http">>, <<"[2001:db8::1]">>, 80}, <<"http://[2001:db8::1]">>},
		{{<<"http">>, <<"[::ffff:192.0.2.1]">>, 8080}, <<"http://[::ffff:192.0.2.1]:8080">>},
		{{<<"http">>, <<"[::ffff:192.0.2.1]">>, 80}, <<"http://[::ffff:192.0.2.1]">>},
		{make_ref(), <<"null">>},
		{'*', <<$*>>}
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> R = iolist_to_binary(access_control_allow_origin(V)) end} || {V, R} <- Tests].

horse_access_control_allow_origin() ->
	horse:repeat(200000,
		access_control_allow_origin({<<"http">>, <<"example.org">>, 8080})
	).
-endif.

%% @doc Build the Access-Control-Expose-Headers header.

-spec access_control_expose_headers([binary()]) -> iodata().
access_control_expose_headers(Headers) ->
	join_token_list(nonempty(Headers)).

-ifdef(TEST).
access_control_expose_headers_test_() ->
	Tests = [
		{[<<"accept">>], <<"accept">>},
		{[<<"accept">>, <<"authorization">>, <<"content-type">>], <<"accept, authorization, content-type">>}
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> R = iolist_to_binary(access_control_expose_headers(V)) end} || {V, R} <- Tests].

access_control_expose_headers_error_test_() ->
	Tests = [
		[]
	],
	[{lists:flatten(io_lib:format("~p", [V])),
		fun() -> {'EXIT', _} = (catch access_control_expose_headers(V)) end} || V <- Tests].

horse_access_control_expose_headers() ->
	horse:repeat(200000,
		access_control_expose_headers([<<"accept">>, <<"authorization">>, <<"content-type">>])
	).
-endif.

%% @doc Build the Access-Control-Max-Age header.

-spec access_control_max_age(non_neg_integer()) -> iodata().
access_control_max_age(MaxAge) -> integer_to_binary(MaxAge).

-ifdef(TEST).
access_control_max_age_test_() ->
	Tests = [
		{0, <<"0">>},
		{42, <<"42">>},
		{69, <<"69">>},
		{1337, <<"1337">>},
		{3495, <<"3495">>},
		{1234567890, <<"1234567890">>}
	],
	[{V, fun() -> R = access_control_max_age(V) end} || {V, R} <- Tests].
-endif.

%% Internal.

%% Only return if the list is not empty.
nonempty(L) when L =/= [] -> L.

%% Parse a list of case sensitive tokens.
token_list(<<>>, Acc) -> lists:reverse(Acc);
token_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> token_list(R, Acc);
token_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) -> token(R, Acc, << C >>).

token(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> token(R, Acc, << T/binary, C >>);
token(R, Acc, T) -> token_list_sep(R, [T|Acc]).

token_list_sep(<<>>, Acc) -> lists:reverse(Acc);
token_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> token_list_sep(R, Acc);
token_list_sep(<< $,, R/bits >>, Acc) -> token_list(R, Acc).

%% Parse a list of case insensitive tokens.
token_ci_list(<<>>, Acc) -> lists:reverse(Acc);
token_ci_list(<< C, R/bits >>, Acc) when ?IS_WS_COMMA(C) -> token_ci_list(R, Acc);
token_ci_list(<< C, R/bits >>, Acc) when ?IS_TOKEN(C) -> ?LOWER(token_ci, R, Acc, <<>>).

token_ci(<< C, R/bits >>, Acc, T) when ?IS_TOKEN(C) -> ?LOWER(token_ci, R, Acc, T);
token_ci(R, Acc, T) -> token_ci_list_sep(R, [T|Acc]).

token_ci_list_sep(<<>>, Acc) -> lists:reverse(Acc);
token_ci_list_sep(<< C, R/bits >>, Acc) when ?IS_WS(C) -> token_ci_list_sep(R, Acc);
token_ci_list_sep(<< $,, R/bits >>, Acc) -> token_ci_list(R, Acc).

join_token_list([]) -> [];
join_token_list([H|T]) -> join_token_list(T, [H]).

join_token_list([], Acc) -> lists:reverse(Acc);
join_token_list([H|T], Acc) -> join_token_list(T, [H,<<", ">>|Acc]).
