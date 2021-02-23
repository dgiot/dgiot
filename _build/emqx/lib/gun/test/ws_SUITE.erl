%% Copyright (c) 2015-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(ws_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).

%% ct.

all() ->
	[{group, autobahn}].

groups() ->
	[{autobahn, [], [autobahn_fuzzingserver]}].

init_per_group(autobahn, Config) ->
	%% Some systems have it named pip2.
	Out = os:cmd("pip show autobahntestsuite ; pip2 show autobahntestsuite"),
	case string:str(Out, "autobahntestsuite") of
		0 ->
			ct:pal("Skipping the autobahn group because the "
				"Autobahn Test Suite is not installed.~nTo install it, "
				"please follow the instructions on this page:~n~n    "
				"http://autobahn.ws/testsuite/installation.html"),
			{skip, "Autobahn Test Suite not installed."};
		_ ->
			Config
	end.

end_per_group(_, _) ->
	ok.

%% Tests.

autobahn_fuzzingserver(Config) ->
	Self = self(),
	spawn_link(fun() -> start_port(Config, Self) end),
	receive autobahn_ready -> ok end,
	N = get_case_count(),
	run_cases(0, N),
	Report = config(priv_dir, Config) ++ "reports/clients/index.html",
	ct:log("<h2><a href=\"~s\">Full report</a></h2>~n", [Report]),
	ct:print("Autobahn Test Suite report: file://~s~n", [Report]),
	log_output(),
	terminate(),
	{ok, HTML} = file:read_file(Report),
	case length(binary:matches(HTML, <<"case_failed">>)) > 2 of
		true -> error(failed);
		false -> ok
	end.

start_port(Config, Pid) ->
	Port = open_port({spawn, "wstest -m fuzzingserver -s " ++ config(data_dir, Config) ++ "server.json"},
		[{line, 10000}, {cd, config(priv_dir, Config)}, binary]),
	receive_preamble(Port, Pid),
	receive_infinity(Port).

receive_preamble(Port, Pid) ->
	receive
		{Port, {data, {eol, Line = <<"Ok, will run", _/bits>>}}} ->
			Pid ! autobahn_ready,
			io:format(user, "~s~n", [Line]);
		{Port, {data, {eol, Line}}} ->
			io:format(user, "~s~n", [Line]),
			receive_preamble(Port, Pid)
	after 5000 ->
		terminate(),
		error(timeout)
	end.

receive_infinity(Port) ->
	receive
		{Port, {data, {eol, <<"Updating reports", _/bits>>}}} ->
			receive_infinity(Port);
		{Port, {data, {eol, Line}}} ->
			io:format(user, "~s~n", [Line]),
			receive_infinity(Port)
	end.

get_case_count() ->
	{Pid, MRef, StreamRef} = connect("/getCaseCount"),
	receive
		{gun_ws, Pid, StreamRef, {text, N}} ->
			close(Pid, MRef),
			binary_to_integer(N);
		Msg ->
			ct:pal("Unexpected message ~p", [Msg]),
			terminate(),
			error(failed)
	end.

run_cases(Total, Total) ->
	ok;
run_cases(N, Total) ->
	{Pid, MRef, StreamRef} = connect(["/runCase?case=", integer_to_binary(N + 1), "&agent=Gun"]),
	loop(Pid, MRef, StreamRef),
	update_reports(),
	run_cases(N + 1, Total).

loop(Pid, MRef, StreamRef) ->
	receive
		{gun_ws, Pid, StreamRef, close} ->
			gun:ws_send(Pid, close),
			loop(Pid, MRef, StreamRef);
		{gun_ws, Pid, StreamRef, {close, Code, _}} ->
			gun:ws_send(Pid, {close, Code, <<>>}),
			loop(Pid, MRef, StreamRef);
		{gun_ws, Pid, StreamRef, Frame} ->
			gun:ws_send(Pid, Frame),
			loop(Pid, MRef, StreamRef);
		{gun_down, Pid, ws, _, _, _} ->
			close(Pid, MRef);
		{'DOWN', MRef, process, Pid, normal} ->
			close(Pid, MRef);
		Msg ->
			ct:pal("Unexpected message ~p", [Msg]),
			close(Pid, MRef)
	end.

update_reports() ->
	{Pid, MRef, StreamRef} = connect("/updateReports?agent=Gun"),
	receive
		{gun_ws, Pid, StreamRef, close} ->
			close(Pid, MRef)
	after 5000 ->
		error(failed)
	end.

log_output() ->
	ok.

connect(Path) ->
	{ok, Pid} = gun:open("127.0.0.1", 33080, #{retry => 0}),
	{ok, http} = gun:await_up(Pid),
	MRef = monitor(process, Pid),
	StreamRef = gun:ws_upgrade(Pid, Path, [], #{compress => true}),
	receive
		{gun_upgrade, Pid, StreamRef, [<<"websocket">>], _} ->
			ok;
		Msg ->
			ct:pal("Unexpected message ~p", [Msg]),
			terminate(),
			error(failed)
	end,
	{Pid, MRef, StreamRef}.

close(Pid, MRef) ->
	demonitor(MRef),
	gun:close(Pid),
	gun:flush(Pid).

terminate() ->
	Res = os:cmd("killall wstest"),
	io:format(user, "~s", [Res]),
	ok.
