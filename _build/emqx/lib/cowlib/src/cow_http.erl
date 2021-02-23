%% Copyright (c) 2013-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_http).

-export([parse_request_line/1]).
-export([parse_status_line/1]).
-export([status_to_integer/1]).
-export([parse_headers/1]).

-export([parse_fullpath/1]).
-export([parse_version/1]).

-export([request/4]).
-export([response/3]).
-export([headers/1]).
-export([version/1]).

-type version() :: 'HTTP/1.0' | 'HTTP/1.1'.
-export_type([version/0]).

-type status() :: 100..999.
-export_type([status/0]).

-type headers() :: [{binary(), iodata()}].
-export_type([headers/0]).

-include("cow_inline.hrl").

%% @doc Parse the request line.

-spec parse_request_line(binary()) -> {binary(), binary(), version(), binary()}.
parse_request_line(Data) ->
	{Pos, _} = binary:match(Data, <<"\r">>),
	<<RequestLine:Pos/binary, "\r\n", Rest/bits>> = Data,
	[Method, Target, Version0] = binary:split(RequestLine, <<$\s>>, [trim_all, global]),
	Version = case Version0 of
		<<"HTTP/1.1">> -> 'HTTP/1.1';
		<<"HTTP/1.0">> -> 'HTTP/1.0'
	end,
	{Method, Target, Version, Rest}.

-ifdef(TEST).
parse_request_line_test_() ->
	Tests = [
		{<<"GET /path HTTP/1.0\r\nRest">>,
			{<<"GET">>, <<"/path">>, 'HTTP/1.0', <<"Rest">>}},
		{<<"GET /path HTTP/1.1\r\nRest">>,
			{<<"GET">>, <<"/path">>, 'HTTP/1.1', <<"Rest">>}},
		{<<"CONNECT proxy.example.org:1080 HTTP/1.1\r\nRest">>,
			{<<"CONNECT">>, <<"proxy.example.org:1080">>, 'HTTP/1.1', <<"Rest">>}}
	],
	[{V, fun() -> R = parse_request_line(V) end}
		|| {V, R} <- Tests].

parse_request_line_error_test_() ->
	Tests = [
		<<>>,
		<<"GET">>,
		<<"GET /path\r\n">>,
		<<"GET /path HTTP/1.1">>,
		<<"GET /path HTTP/1.1\r">>,
		<<"GET /path HTTP/1.1\n">>,
		<<"GET /path HTTP/0.9\r\n">>,
		<<"content-type: text/plain\r\n">>,
		<<0:80, "\r\n">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_request_line(V)) end}
		|| V <- Tests].

horse_parse_request_line_get_path() ->
	horse:repeat(200000,
		parse_request_line(<<"GET /path HTTP/1.1\r\n">>)
	).
-endif.

%% @doc Parse the status line.

-spec parse_status_line(binary()) -> {version(), status(), binary(), binary()}.
parse_status_line(<< "HTTP/1.1 200 OK\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 200, <<"OK">>, Rest};
parse_status_line(<< "HTTP/1.1 404 Not Found\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 404, <<"Not Found">>, Rest};
parse_status_line(<< "HTTP/1.1 500 Internal Server Error\r\n", Rest/bits >>) ->
	{'HTTP/1.1', 500, <<"Internal Server Error">>, Rest};
parse_status_line(<< "HTTP/1.1 ", Status/bits >>) ->
	parse_status_line(Status, 'HTTP/1.1');
parse_status_line(<< "HTTP/1.0 ", Status/bits >>) ->
	parse_status_line(Status, 'HTTP/1.0').

parse_status_line(<<H, T, U, " ", Rest/bits>>, Version) ->
	Status = status_to_integer(H, T, U),
	{Pos, _} = binary:match(Rest, <<"\r">>),
	<< StatusStr:Pos/binary, "\r\n", Rest2/bits >> = Rest,
	{Version, Status, StatusStr, Rest2}.

-spec status_to_integer(status() | binary()) -> status().
status_to_integer(Status) when is_integer(Status) ->
	Status;
status_to_integer(Status) ->
	case Status of
		<<H, T, U>> ->
			status_to_integer(H, T, U);
		<<H, T, U, " ", _/bits>> ->
			status_to_integer(H, T, U)
	end.

status_to_integer(H, T, U)
		when $0 =< H, H =< $9, $0 =< T, T =< $9, $0 =< U, U =< $9 ->
	(H - $0) * 100 + (T - $0) * 10 + (U - $0).

-ifdef(TEST).
parse_status_line_test_() ->
	Tests = [
		{<<"HTTP/1.1 200 OK\r\nRest">>,
			{'HTTP/1.1', 200, <<"OK">>, <<"Rest">>}},
		{<<"HTTP/1.0 404 Not Found\r\nRest">>,
			{'HTTP/1.0', 404, <<"Not Found">>, <<"Rest">>}},
		{<<"HTTP/1.1 500 Something very funny here\r\nRest">>,
			{'HTTP/1.1', 500, <<"Something very funny here">>, <<"Rest">>}},
		{<<"HTTP/1.1 200 \r\nRest">>,
			{'HTTP/1.1', 200, <<>>, <<"Rest">>}}
	],
	[{V, fun() -> R = parse_status_line(V) end}
		|| {V, R} <- Tests].

parse_status_line_error_test_() ->
	Tests = [
		<<>>,
		<<"HTTP/1.1">>,
		<<"HTTP/1.1 200\r\n">>,
		<<"HTTP/1.1 200 OK">>,
		<<"HTTP/1.1 200 OK\r">>,
		<<"HTTP/1.1 200 OK\n">>,
		<<"HTTP/0.9 200 OK\r\n">>,
		<<"HTTP/1.1 42 Answer\r\n">>,
		<<"HTTP/1.1 999999999 More than OK\r\n">>,
		<<"content-type: text/plain\r\n">>,
		<<0:80, "\r\n">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_status_line(V)) end}
		|| V <- Tests].

horse_parse_status_line_200() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 200 OK\r\n">>)
	).

horse_parse_status_line_404() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 404 Not Found\r\n">>)
	).

horse_parse_status_line_500() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 500 Internal Server Error\r\n">>)
	).

horse_parse_status_line_other() ->
	horse:repeat(200000,
		parse_status_line(<<"HTTP/1.1 416 Requested range not satisfiable\r\n">>)
	).
-endif.

%% @doc Parse the list of headers.

-spec parse_headers(binary()) -> {[{binary(), binary()}], binary()}.
parse_headers(Data) ->
	parse_header(Data, []).

parse_header(<< $\r, $\n, Rest/bits >>, Acc) ->
	{lists:reverse(Acc), Rest};
parse_header(Data, Acc) ->
	parse_hd_name(Data, Acc, <<>>).

parse_hd_name(<< C, Rest/bits >>, Acc, SoFar) ->
	case C of
		$: -> parse_hd_before_value(Rest, Acc, SoFar);
		$\s -> parse_hd_name_ws(Rest, Acc, SoFar);
		$\t -> parse_hd_name_ws(Rest, Acc, SoFar);
		_ -> ?LOWER(parse_hd_name, Rest, Acc, SoFar)
	end.

parse_hd_name_ws(<< C, Rest/bits >>, Acc, Name) ->
	case C of
		$: -> parse_hd_before_value(Rest, Acc, Name);
		$\s -> parse_hd_name_ws(Rest, Acc, Name);
		$\t -> parse_hd_name_ws(Rest, Acc, Name)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, Acc, Name) ->
	parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(<< $\t, Rest/bits >>, Acc, Name) ->
	parse_hd_before_value(Rest, Acc, Name);
parse_hd_before_value(Data, Acc, Name) ->
	parse_hd_value(Data, Acc, Name, <<>>).

parse_hd_value(<< $\r, Rest/bits >>, Acc, Name, SoFar) ->
	case Rest of
		<< $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
			parse_hd_value(Rest2, Acc, Name, << SoFar/binary, C >>);
		<< $\n, Rest2/bits >> ->
			Value = clean_value_ws_end(SoFar, byte_size(SoFar) - 1),
			parse_header(Rest2, [{Name, Value}|Acc])
	end;
parse_hd_value(<< C, Rest/bits >>, Acc, Name, SoFar) ->
	parse_hd_value(Rest, Acc, Name, << SoFar/binary, C >>).

%% This function has been copied from cowboy_http.
clean_value_ws_end(_, -1) ->
	<<>>;
clean_value_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s -> clean_value_ws_end(Value, N - 1);
		$\t -> clean_value_ws_end(Value, N - 1);
		_ ->
			S = N + 1,
			<< Value2:S/binary, _/bits >> = Value,
			Value2
	end.

-ifdef(TEST).
parse_headers_test_() ->
	Tests = [
		{<<"\r\nRest">>,
			{[], <<"Rest">>}},
		{<<"Server: Erlang/R17  \r\n\r\n">>,
			{[{<<"server">>, <<"Erlang/R17">>}], <<>>}},
		{<<"Server: Erlang/R17\r\n"
			"Date: Sun, 23 Feb 2014 09:30:39 GMT\r\n"
			"Multiline-Header: why hello!\r\n"
				" I didn't see you all the way over there!\r\n"
			"Content-Length: 12\r\n"
			"Content-Type: text/plain\r\n"
			"\r\nRest">>,
			{[{<<"server">>, <<"Erlang/R17">>},
				{<<"date">>, <<"Sun, 23 Feb 2014 09:30:39 GMT">>},
				{<<"multiline-header">>,
					<<"why hello! I didn't see you all the way over there!">>},
				{<<"content-length">>, <<"12">>},
				{<<"content-type">>, <<"text/plain">>}],
				<<"Rest">>}}
	],
	[{V, fun() -> R = parse_headers(V) end}
		|| {V, R} <- Tests].

parse_headers_error_test_() ->
	Tests = [
		<<>>,
		<<"\r">>,
		<<"Malformed\r\n\r\n">>,
		<<"content-type: text/plain\r\nMalformed\r\n\r\n">>,
		<<"HTTP/1.1 200 OK\r\n\r\n">>,
		<<0:80, "\r\n\r\n">>,
		<<"content-type: text/plain\r\ncontent-length: 12\r\n">>
	],
	[{V, fun() -> {'EXIT', _} = (catch parse_headers(V)) end}
		|| V <- Tests].

horse_parse_headers() ->
	horse:repeat(50000,
		parse_headers(<<"Server: Erlang/R17\r\n"
			"Date: Sun, 23 Feb 2014 09:30:39 GMT\r\n"
			"Multiline-Header: why hello!\r\n"
				" I didn't see you all the way over there!\r\n"
			"Content-Length: 12\r\n"
			"Content-Type: text/plain\r\n"
			"\r\nRest">>)
	).
-endif.

%% @doc Extract path and query string from a binary,
%% removing any fragment component.

-spec parse_fullpath(binary()) -> {binary(), binary()}.
parse_fullpath(Fullpath) ->
	parse_fullpath(Fullpath, <<>>).

parse_fullpath(<<>>, Path) -> {Path, <<>>};
parse_fullpath(<< $#, _/bits >>, Path) -> {Path, <<>>};
parse_fullpath(<< $?, Qs/bits >>, Path) -> parse_fullpath_query(Qs, Path, <<>>);
parse_fullpath(<< C, Rest/bits >>, SoFar) -> parse_fullpath(Rest, << SoFar/binary, C >>).

parse_fullpath_query(<<>>, Path, Query) -> {Path, Query};
parse_fullpath_query(<< $#, _/bits >>, Path, Query) -> {Path, Query};
parse_fullpath_query(<< C, Rest/bits >>, Path, SoFar) ->
	parse_fullpath_query(Rest, Path, << SoFar/binary, C >>).

-ifdef(TEST).
parse_fullpath_test() ->
	{<<"*">>, <<>>} = parse_fullpath(<<"*">>),
	{<<"/">>, <<>>} = parse_fullpath(<<"/">>),
	{<<"/path/to/resource">>, <<>>} = parse_fullpath(<<"/path/to/resource#fragment">>),
	{<<"/path/to/resource">>, <<>>} = parse_fullpath(<<"/path/to/resource">>),
	{<<"/">>, <<>>} = parse_fullpath(<<"/?">>),
	{<<"/">>, <<"q=cowboy">>} = parse_fullpath(<<"/?q=cowboy#fragment">>),
	{<<"/">>, <<"q=cowboy">>} = parse_fullpath(<<"/?q=cowboy">>),
	{<<"/path/to/resource">>, <<"q=cowboy">>}
		= parse_fullpath(<<"/path/to/resource?q=cowboy">>),
	ok.
-endif.

%% @doc Convert an HTTP version to atom.

-spec parse_version(binary()) -> version().
parse_version(<<"HTTP/1.1">>) -> 'HTTP/1.1';
parse_version(<<"HTTP/1.0">>) -> 'HTTP/1.0'.

-ifdef(TEST).
parse_version_test() ->
	'HTTP/1.1' = parse_version(<<"HTTP/1.1">>),
	'HTTP/1.0' = parse_version(<<"HTTP/1.0">>),
	{'EXIT', _} = (catch parse_version(<<"HTTP/1.2">>)),
	ok.
-endif.

%% @doc Return formatted request-line and headers.
%% @todo Add tests when the corresponding reverse functions are added.

-spec request(binary(), iodata(), version(), headers()) -> iodata().
request(Method, Path, Version, Headers) ->
	[Method, <<" ">>, Path, <<" ">>, version(Version), <<"\r\n">>,
		[[N, <<": ">>, V, <<"\r\n">>] || {N, V} <- Headers],
		<<"\r\n">>].

-spec response(status() | binary(), version(), headers()) -> iodata().
response(Status, Version, Headers) ->
	[version(Version), <<" ">>, status(Status), <<"\r\n">>,
		headers(Headers), <<"\r\n">>].

-spec headers(headers()) -> iodata().
headers(Headers) ->
	[[N, <<": ">>, V, <<"\r\n">>] || {N, V} <- Headers].

%% @doc Return the version as a binary.

-spec version(version()) -> binary().
version('HTTP/1.1') -> <<"HTTP/1.1">>;
version('HTTP/1.0') -> <<"HTTP/1.0">>.

-ifdef(TEST).
version_test() ->
	<<"HTTP/1.1">> = version('HTTP/1.1'),
	<<"HTTP/1.0">> = version('HTTP/1.0'),
	{'EXIT', _} = (catch version('HTTP/1.2')),
	ok.
-endif.

%% @doc Return the status code and string as binary.

-spec status(status() | binary()) -> binary().
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(103) -> <<"103 Early Hints">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(208) -> <<"208 Already Reported">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(308) -> <<"308 Permanent Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(421) -> <<"421 Misdirected Request">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(428) -> <<"428 Precondition Required">>;
status(429) -> <<"429 Too Many Requests">>;
status(431) -> <<"431 Request Header Fields Too Large">>;
status(451) -> <<"451 Unavailable For Legal Reasons">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(508) -> <<"508 Loop Detected">>;
status(510) -> <<"510 Not Extended">>;
status(511) -> <<"511 Network Authentication Required">>;
status(B) when is_binary(B) -> B.
