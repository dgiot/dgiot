%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(h2specd_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

%% ct.

all() ->
	[h2specd].

init_per_suite(Config) ->
	case os:getenv("H2SPECD") of
		false -> skip;
		_ ->
			%% We ensure that SASL is started for this test suite
			%% to have the crash reports in the CT logs.
			{ok, Apps} = application:ensure_all_started(sasl),
			[{sasl_started, Apps =/= []}|Config]
	end.

end_per_suite(Config) ->
	case config(sasl_started, Config) of
		true -> application:stop(sasl);
		false -> ok
	end.

%% Tests.

h2specd(Config) ->
	doc("h2specd test suite for the HTTP/2 protocol."),
	Self = self(),
	Pid = spawn_link(fun() -> start_port(Config, Self) end),
	receive ready -> ok after 10000 -> error(timeout) end,
	try
		run_tests(),
		timer:sleep(100),
		maybe_fail()
	after
		unlink(Pid),
		os:cmd("killall h2specd")
	end.

start_port(Config, Pid) ->
	H2specd = os:getenv("H2SPECD"),
	Port = open_port(
		{spawn, H2specd ++ " -S -p 45678"},
		[{line, 10000}, {cd, config(priv_dir, Config)}, binary, exit_status]),
	Pid ! ready,
	receive_infinity(Port, []).

receive_infinity(Port, Acc) ->
	receive
		{Port, {data, {eol, Line}}} ->
			ct:log("~ts", [Line]),
			io:format(user, "~s~n", [Line]),
			receive_infinity(Port, [Line|Acc]);
		{Port, Reason={exit_status, _}} ->
			ct:log("~ts", [[[L, $\n] || L <- lists:reverse(Acc)]]),
			exit({shutdown, Reason})
	end.

run_tests() ->
	timer:sleep(1000),
	Tests = scrape_tests(),
	ct:pal("Test ports: ~p~n", [Tests]),
	run_tests(Tests).

run_tests([]) ->
	ok;
run_tests([Port|Tail]) ->
	try
		{ok, Conn} = gun:open("127.0.0.1", Port, #{
			protocols => [http2],
			retry => 0
		}),
		MRef = monitor(process, Conn),
		{ok, http2} = gun:await_up(Conn),
		StreamRef = gun:get(Conn, "/"),
		receive
			{gun_response, Conn, StreamRef, _, _, _} ->
				timer:sleep(100);
			{'DOWN', MRef, process, Conn, _} ->
				ok
		after 100 ->
			ok
		end,
		ok = gun:close(Conn)
	after
		run_tests(Tail)
	end.

scrape_tests() ->
	{ok, Conn} = gun:open("127.0.0.1", 45678),
	{ok, http} = gun:await_up(Conn),
	StreamRef = gun:get(Conn, "/"),
	{response, nofin, 200, _} = gun:await(Conn, StreamRef),
	{ok, Body} = gun:await_body(Conn, StreamRef),
	ok = gun:close(Conn),
	{match, Matches} = re:run(Body, "<a href=\"(.*?)\"", [global, {capture, all, binary}]),
	[binary_to_integer(Port)
		|| [_, <<"http://127.0.0.1:", Port:5/binary, "/">>] <- Matches].

maybe_fail() ->
	{ok, Conn} = gun:open("127.0.0.1", 45678),
	{ok, http} = gun:await_up(Conn),
	StreamRef = gun:get(Conn, "/report", [{<<"accept">>, "text/plain"}]),
	{response, nofin, 200, _} = gun:await(Conn, StreamRef),
	{ok, Body} = gun:await_body(Conn, StreamRef),
	ok = gun:close(Conn),
	case binary:match(Body, <<"0 skipped, 0 failed">>) of
		nomatch -> exit(failed);
		_ -> ok
	end.
